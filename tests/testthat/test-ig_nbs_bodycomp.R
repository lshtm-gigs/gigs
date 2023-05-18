test_percentile_tbls <- function(sex, age_lower, age_upper, acronym, tolerance) {
  get_gest_ages <- function(lower, upper) {
    ga <- gigs::ig_nbs$fmfga$male$percentiles$gest_age
    ga[which(ga >= lower & ga <= upper)]
  }
  tbl_names <- c("P03", "P10", "P50", "P90", "P97")
  digits <- if (acronym %in% "bfpfga") 1 else 0
  tbl <- lapply(X = c(0.03, 0.1, 0.5, 0.9, 0.97),
                FUN = function(x) {
                  fn <- switch(acronym,
                     "fmfga" =  ig_nbs_fmfga_percentile2value,
                     "bfpfga" =  ig_nbs_bfpfga_percentile2value,
                     "ffmfga" = ig_nbs_ffmfga_percentile2value)
                  round2(fn(p = x, gest_age = get_gest_ages(age_lower, age_upper), sex = sex),
                        digits = digits)
                }) |>
    do.call(what = cbind) |>
    as.data.frame()
  names(tbl) <- tbl_names
  tbl$gest_age <- as.integer(get_gest_ages(age_lower, age_upper))
  sex_ <- ifelse(sex == "M", yes = "male", no = "female" )
  pkg_tbl <- tbl[, c(ncol(tbl), 1:(ncol(tbl) - 1))]
  ref_tbl <- gigs::ig_nbs[[acronym]][[sex_]]$percentiles
  expect_true(all(abs(ref_tbl - pkg_tbl) <= tolerance, na.rm = T))
}

test_that(desc = "Conversion of percentiles to values works", {
  sex <- rep(c("M", "F"), 3)
  lower <- rep(266, length(sex))
  upper <- rep(294, length(sex))
  acronyms <- rep(names(gigs::ig_nbs)[5:7], times = rep(2, 3))
  tolerances <- c(26, 15, 0.5, 0.5, 7, 5)
  mapply(FUN = test_percentile_tbls, sex, lower, upper, acronyms, tolerances)

  # Test that bad input gives NA
  with(
    data.frame(sex = c("M", "F", "U", "X", "M"),
         percentiles = c(0.25, -1, 0.13, 0.84, 0.10),
         age = c(37, 38, 39, 40, 41)),
    expr = {
      sapply(X = c("fmfga", "bfpfga", "ffmfga"), FUN = function(x) {
        fn <- switch(x,
                     "fmfga" = ig_nbs_fmfga_percentile2value,
                     "bfpfga" = ig_nbs_bfpfga_percentile2value,
                     "ffmfga" = ig_nbs_ffmfga_percentile2value)
        vals <- suppressWarnings(fn(sex = sex, gest_age = age, p = percentiles))
        expect_length(object = vals, n = length(sex))
        expect_true(all(is.na(c(
          vals[1], # Because age is out of bounds
          vals[2], # Because percentile is out of bounds
          vals[4]  # Because sex is not one of "M", "F" or "U"
        ))))
      })
    })
})

test_zscore_tbls <- function(sex, age_lower, age_upper, acronym, tolerance) {
  get_gest_ages <- function(lower, upper) {
    ga <- gigs::ig_nbs$fmfga$male$percentiles$gest_age
    ga[which(ga >= lower & ga <= upper)]
  }
  tbl_names <- c("P03", "P10", "P50", "P90", "P97")
  digits <- if (acronym %in% "bfpfga") 1 else 0
  tbl <- lapply(X = qnorm(c(0.03, 0.1, 0.5, 0.9, 0.97)),
                FUN = function(x) {
                  fn <- switch(acronym,
                     "fmfga" =   ig_nbs_fmfga_zscore2value,
                     "bfpfga" =  ig_nbs_bfpfga_zscore2value,
                     "ffmfga" =  ig_nbs_ffmfga_zscore2value)
                   round2(fn(sex = sex, gest_age = get_gest_ages(age_lower, age_upper), z = x),
                         digits = digits)
                }) |>
    do.call(what = cbind) |>
    as.data.frame()
  names(tbl) <- tbl_names
  tbl$gest_age <- get_gest_ages(age_lower, age_upper)
  pkg_tbl <- tbl[, c(ncol(tbl), 1:(ncol(tbl) - 1))]
  sex_ <- ifelse(sex == "M", yes = "male", no = "female" )
  ref_tbl <- gigs::ig_nbs[[acronym]][[sex_]]$percentiles
  expect_true(all(abs(ref_tbl - pkg_tbl) <= tolerance, na.rm = T))
}

test_that("Conversion of z-scores to values works", {
  sex <- rep(c("M", "F"), 3)
  lower <- rep(266, length(sex))
  upper <- rep(294, length(sex))
  acronyms <- rep(names(gigs::ig_nbs)[5:7], times = rep(2, 3))
  tolerances <- c(26, 15, 0.5, 0.5, 7, 5)
  invisible(mapply(FUN = test_zscore_tbls, sex, lower, upper, acronyms, tolerances))
})


testthat_v2x <- function(y, gest_age, sex, acronym, z_or_p = "zscores") {
  out_z_or_p <- switch(z_or_p,
         "zscores" = switch(acronym,
                            "fmfga"  = ig_nbs_fmfga_value2zscore(fat_mass_g = y, gest_age = gest_age, sex = sex),
                            "bfpfga"  = ig_nbs_bfpfga_value2zscore(body_fat_perc = y, gest_age = gest_age, sex = sex),
                            "ffmfga" = ig_nbs_ffmfga_value2zscore(fatfree_mass_g = y, gest_age = gest_age, sex = sex)),
         "percentiles" = switch(acronym,
                            "fmfga"  = ig_nbs_fmfga_value2percentile(fat_mass_g = y, gest_age = gest_age, sex = sex),
                            "bfpfga"  = ig_nbs_bfpfga_value2percentile(body_fat_perc = y, gest_age = gest_age, sex = sex),
                            "ffmfga" = ig_nbs_ffmfga_value2percentile(fatfree_mass_g = y, gest_age = gest_age, sex = sex)))
  out_value <- switch(z_or_p,
         "zscores" = switch(acronym,
                            "fmfga"  = ig_nbs_fmfga_zscore2value(z = out_z_or_p, gest_age = gest_age, sex = sex),
                            "bfpfga"  = ig_nbs_bfpfga_zscore2value(z = out_z_or_p, gest_age = gest_age, sex = sex),
                            "ffmfga" = ig_nbs_ffmfga_zscore2value(z = out_z_or_p, gest_age = gest_age, sex = sex)),
         "percentiles" = switch(acronym,
                            "fmfga"  = ig_nbs_fmfga_percentile2value(p = out_z_or_p, gest_age = gest_age, sex = sex),
                            "bfpfga"  = ig_nbs_bfpfga_percentile2value(p = out_z_or_p, gest_age = gest_age, sex = sex),
                            "ffmfga" = ig_nbs_ffmfga_percentile2value(p = out_z_or_p, gest_age = gest_age, sex = sex)))
  if (all(is.na(out_z_or_p)) | all(is.na(out_z_or_p))) {
    stop("All values were NA.")
  }
  expect_equal(round(out_value, digits = 2), expected = round(y, digits = 2))
}


test_that("Conversion of values to z-scores works", {
  # Fat mass for gestational age
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_age = 7 * 39, sex = "M", acronym = "fmfga")
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_age = 7 * 40, sex = "F", acronym = "fmfga")

  # Fat-free mass for gestational age
  testthat_v2x(y = c(2041.9, 2193.8, 2345.6, 2497.3, 2649.1), gest_age = 7 * 42, sex = "M", acronym = "ffmfga")
  testthat_v2x(y = c(2046.7, 2191.8, 2343.5, 2497.5, 2648.1), gest_age = 7 * 41, sex = "F", acronym = "ffmfga")

  # Body fat percentage for gestational age
  testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1) / 2, gest_age = 7 * 42, sex = "M", acronym = "bfpfga")
  testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1) / 2, gest_age = 7 * 38, sex = "F", acronym = "bfpfga")
})

test_that("Conversion of values to percentiles works", {
  perc <- "percentiles"
  # Fat mass for gestational age
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_age = 7 * 41, sex = "M", acronym = "fmfga", z_or_p = perc)
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_age = 7 * 38, sex = "F", acronym = "fmfga", z_or_p = perc)

  # Fat-free mass for gestational age
  testthat_v2x(y = c(2041.9, 2193.8, 2345.6, 2497.3, 2649.1), gest_age = 7 * 39, sex = "M", acronym = "ffmfga", z_or_p = perc)
  testthat_v2x(y = c(2046.7, 2191.8, 2343.5, 2497.5, 2648.1), gest_age = 7 * 40, sex = "F", acronym = "ffmfga", z_or_p = perc)

  # Body fat percentage for gestational age
  testthat_v2x(y = c(16.4, 16.5, 17.15, 17.85, 18.0), gest_age = 7 * 42, sex = "M", acronym = "bfpfga", z_or_p = perc)
  testthat_v2x(y = c(11.6, 12.4, 10.5, 11.8, 13.2), gest_age = 7 * 38, sex = "F", acronym = "bfpfga", z_or_p = perc)
})