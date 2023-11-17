test_centile_tbls <- function(sex, age_lower, age_upper, acronym, tolerance) {
  get_gest_days <- function(lower, upper) {
    ga <- gigs::ig_nbs$fmfga$male$centiles$gest_days
    ga[which(ga >= lower & ga <= upper)]
  }
  tbl_names <- c("P03", "P10", "P50", "P90", "P97")
  digits <- if (acronym %in% "bfpfga") 1 else 0
  tbl <- lapply(X = c(0.03, 0.1, 0.5, 0.9, 0.97),
                FUN = function(x) {
                  fn <- switch(acronym,
                     "fmfga" =  ig_nbs_fmfga_centile2value,
                     "bfpfga" =  ig_nbs_bfpfga_centile2value,
                     "ffmfga" = ig_nbs_ffmfga_centile2value)
                  round2(fn(p = x, gest_days = get_gest_days(age_lower, age_upper), sex = sex),
                        digits = digits)
                }) |>
    do.call(what = cbind) |>
    as.data.frame()
  names(tbl) <- tbl_names
  tbl$gest_days <- as.integer(get_gest_days(age_lower, age_upper))
  sex_ <- ifelse(sex == "M", yes = "male", no = "female" )
  pkg_tbl <- tbl[, c(ncol(tbl), 1:(ncol(tbl) - 1))]
  ref_tbl <- gigs::ig_nbs[[acronym]][[sex_]]$centiles
  expect_true(all(abs(ref_tbl - pkg_tbl) <= tolerance, na.rm = T))
}

test_that(desc = "Conversion of centiles to values works", {
  sex <- rep(c("M", "F"), 3)
  lower <- rep(266, length(sex))
  upper <- rep(294, length(sex))
  acronyms <- rep(names(gigs::ig_nbs)[5:7], times = rep(2, 3))
  # 23/08/2023: With old linear equations used to be c(26, 15, 0.5, 0.5, 7, 5)
  tolerances <- c(6, 1, 0.11, 0.11, 2, 0)
  mapply(FUN = test_centile_tbls, sex, lower, upper, acronyms, tolerances)

  # Test that bad input gives NA
  with(
    data.frame(sex = c("M", "F", "U", "X", "M"),
         centiles = c(0.25, -1, 0.13, 0.84, 0.10),
         age = c(37, 38, 39, 40, 41)),
    expr = {
      vapply(
        X = c("fmfga", "bfpfga", "ffmfga"),
        FUN = function(x) {
          fn <- switch(x,
                       "fmfga" = ig_nbs_fmfga_centile2value,
                       "bfpfga" = ig_nbs_bfpfga_centile2value,
                       "ffmfga" = ig_nbs_ffmfga_centile2value)
          vals <- suppressWarnings(fn(sex = sex, gest_days = age, p = centiles))
          expect_length(object = vals, n = length(sex))
          expect_true(all(is.na(c(
            vals[1], # Because age is out of bounds
            vals[2], # Because centile is out of bounds
            vals[4]  # Because sex is not one of "M", "F" or "U"
          ))))
        },
        FUN.VALUE = logical(length = 1L))
    })
})

test_zscore_tbls <- function(sex, age_lower, age_upper, acronym, tolerance) {
  get_gest_days <- function(lower, upper) {
    ga <- gigs::ig_nbs$fmfga$male$centiles$gest_days
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
                   round2(fn(sex = sex, gest_days = get_gest_days(age_lower, age_upper), z = x),
                         digits = digits)
                }) |>
    do.call(what = cbind) |>
    as.data.frame()
  names(tbl) <- tbl_names
  tbl$gest_days <- get_gest_days(age_lower, age_upper)
  pkg_tbl <- tbl[, c(ncol(tbl), 1:(ncol(tbl) - 1))]
  sex_ <- ifelse(sex == "M", yes = "male", no = "female" )
  ref_tbl <- gigs::ig_nbs[[acronym]][[sex_]]$centiles
  expect_true(all(abs(ref_tbl - pkg_tbl) <= tolerance, na.rm = T))
}

test_that("Conversion of z-scores to values works", {
  sex <- rep(c("M", "F"), 3)
  lower <- rep(266, length(sex))
  upper <- rep(294, length(sex))
  acronyms <- rep(names(gigs::ig_nbs)[5:7], times = rep(2, 3))
  tolerances <- c(6, 1, 0.11, 0.11, 2, 0)
  invisible(mapply(FUN = test_zscore_tbls, sex, lower, upper, acronyms, tolerances))
})


testthat_v2x <- function(y, gest_days, sex, acronym, z_or_p = "zscore") {
  params_val2zp <- list(y = y, gest_days = gest_days, sex = sex,
                        acronym = acronym)
  fn_val2zp <- paste0("ig_nbs_value2", z_or_p)
  out_z_or_p <- do.call(fn_val2zp, params_val2zp)

  params_zp2val <- list(gest_days = gest_days, sex = sex, acronym = acronym)
  params_zp2val[[switch(z_or_p, "zscore" = "z", "centile" = "p")]] <- out_z_or_p
  fn_zp2val <- paste0("ig_nbs_", z_or_p, "2value")
  out_value <- do.call(fn_zp2val, params_zp2val)

  if (all(is.na(out_z_or_p)) | all(is.na(out_z_or_p))) {
    stop("All values were NA.")
  }
  expect_equal(round2(out_value, digits = 2), expected = round2(y, digits = 2))
}


test_that("Conversion of values to z-scores works", {
  # Fat mass for gestational age
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_days = 7 * 39, sex = "M", acronym = "fmfga")
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_days = 7 * 40, sex = "F", acronym = "fmfga")

  # Fat-free mass for gestational age
  testthat_v2x(y = c(2041.9, 2193.8, 2345.6, 2497.3, 2649.1), gest_days = 7 * 42, sex = "M", acronym = "ffmfga")
  testthat_v2x(y = c(2046.7, 2191.8, 2343.5, 2497.5, 2648.1), gest_days = 7 * 41, sex = "F", acronym = "ffmfga")

  # Body fat percentage for gestational age
  testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1) / 2, gest_days = 7 * 42, sex = "M", acronym = "bfpfga")
  testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1) / 2, gest_days = 7 * 38, sex = "F", acronym = "bfpfga")
})

test_that("Conversion of values to centiles works", {
  cent <- "centile"
  # Fat mass for gestational age
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_days = 7 * 41, sex = "M", acronym = "fmfga", z_or_p = cent)
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_days = 7 * 38, sex = "F", acronym = "fmfga", z_or_p = cent)

  # Fat-free mass for gestational age
  testthat_v2x(y = c(2041.9, 2193.8, 2345.6, 2497.3, 2649.1), gest_days = 7 * 39, sex = "M", acronym = "ffmfga", z_or_p = cent)
  testthat_v2x(y = c(2046.7, 2191.8, 2343.5, 2497.5, 2648.1), gest_days = 7 * 40, sex = "F", acronym = "ffmfga", z_or_p = cent)

  # Body fat percentage for gestational age
  testthat_v2x(y = c(16.4, 16.5, 17.15, 17.85, 18.0), gest_days = 7 * 42, sex = "M", acronym = "bfpfga", z_or_p = cent)
  testthat_v2x(y = c(11.6, 12.4, 10.5, 11.8, 13.2), gest_days = 7 * 38, sex = "F", acronym = "bfpfga", z_or_p = cent)
})