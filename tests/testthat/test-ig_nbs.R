test_zscore_tbls <- function(sex, age_lower, age_upper, acronym, tolerance) {
  get_gest_ages <- function(lower, upper) {
    if (lower < 168) lower <- 168
    if (lower > 300) upper <- 300
    ga <- gigs::ig_nbs$wfga$male$zscores$gest_age
    ga[which(ga >= lower & ga <= upper)]
  }
  roundto <- ifelse(acronym %in% c("wfga", "wlrfga"), yes = 2, no = 1)
  tbl_names <- c("SD3neg", "SD2neg", "SD1neg", "SD0", "SD1", "SD2", "SD3")
  pkg_tbl <- lapply(X = -3:3,
                    FUN = function (x) {
                      fn <- switch(acronym,
                                   "wfga" = ig_nbs_wfga_zscore2value,
                                   "lfga" = ig_nbs_lfga_zscore2value,
                                   "hcfga" = ig_nbs_hcfga_zscore2value,
                                   "wlrfga" = ig_nbs_wlrfga_zscore2value)
                      round(fn(z = x, gest_age = get_gest_ages(age_lower, age_upper), sex = sex), digits = roundto)
                }) |>
    do.call(what = cbind) |>
    as.data.frame()
  names(pkg_tbl) <- tbl_names
  pkg_tbl$gest_age <- get_gest_ages(age_lower, age_upper)
  pkg_tbl <- pkg_tbl[, c(ncol(pkg_tbl), 1:(ncol(pkg_tbl) - 1))]
  sex_ <- ifelse(sex == "M", yes = "male", no = "female" )
  ref_tbl <- gigs::ig_nbs[[acronym]][[sex_]]$zscores
  ref_tbl <- ref_tbl[ref_tbl$gest_age >= age_lower & ref_tbl$gest_age <= age_upper, ]
  rownames(pkg_tbl) <- NULL
  rownames(ref_tbl) <- NULL
  expect_equal(object = pkg_tbl, expected = ref_tbl, tolerance = tolerance)
}

test_that(desc = "Conversion of z-scores to values works", {
  sex <- rep(c("M", "F"), length(names(gigs::ig_nbs)) - 3)
  lower <- rep(168, length(sex))
  upper <- rep(300, length(sex))
  acronyms <- rep(names(gigs::ig_nbs)[1:4], times = rep(2, length(names(gigs::ig_nbs)) - 3))
  tolerance <- 0.01
  invisible(mapply(FUN = test_zscore_tbls, sex, lower, upper, acronyms, tolerance))
})

test_percentile_tbls <- function(sex, age_lower, age_upper, acronym, tolerance) {
  get_gest_ages <- function(lower, upper) {
    ga <- gigs::ig_nbs$wfga$male$zscores$gest_age
    ga[which(ga >= lower & ga <= upper)]
  }
  roundto <- ifelse(acronym == "wfga", yes = 2, no = 1)
  tbl_names <- c("P03", "P05", "P10", "P50", "P90", "P95", "P97")
  pkg_tbl <- lapply(X = c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97),
                    FUN = function (x) {
                      fn <- switch(acronym,
                                   "wfga" = ig_nbs_wfga_percentile2value,
                                   "lfga" = ig_nbs_lfga_percentile2value,
                                   "hcfga" = ig_nbs_hcfga_percentile2value,
                                   "wlrfga" = ig_nbs_wlrfga_percentile2value)
                      round(fn(p = x, gest_age = get_gest_ages(age_lower, age_upper), sex = sex), digits = roundto)
                }) |>
    do.call(what = cbind) |>
    as.data.frame()
  names(pkg_tbl) <- tbl_names
  pkg_tbl$gest_age <- get_gest_ages(age_lower, age_upper)
  pkg_tbl <- pkg_tbl[, c(ncol(pkg_tbl), 1:(ncol(pkg_tbl) - 1))]
  sex_ <- ifelse(sex == "M", yes = "male", no = "female" )
  ref_tbl <- gigs::ig_nbs[[acronym]][[sex_]]$percentiles
  ref_tbl <- ref_tbl[ref_tbl$gest_age >= age_lower & ref_tbl$gest_age <= age_upper, ]
  rownames(pkg_tbl) <- NULL
  rownames(ref_tbl) <- NULL
  list(package = pkg_tbl, reference = ref_tbl)
  expect_equal(object = pkg_tbl, expected = ref_tbl, tolerance = tolerance)
}

test_that("Conversion of percentiles to values works", {
  sex <- rep(c("M", "F"), length(names(gigs::ig_nbs)) - 3)
  lower <- rep(168, length(sex))
  upper <- rep(300, length(sex))
  acronyms <- rep(names(gigs::ig_nbs)[1:4], times = rep(2, length(names(gigs::ig_nbs)) - 3))
  tolerance <- 0.01
  mapply(FUN = test_percentile_tbls, sex, lower, upper, acronyms, tolerance)

  # Test that bad input gives NA
  with(
    data.frame(sex = c("M", "F", "U", "X", "M"),
         percentiles = c(0.25, -1, 0.13, 0.84, 0.10),
         age = c(32, 33, 34, 35, 36)),
    expr = {
      sapply(X = c("wfga", "lfga", "hcfga"), FUN = function(x) {
        fn <- switch(x,
                     "wfga" = ig_nbs_wfga_percentile2value,
                     "lfga" = ig_nbs_lfga_percentile2value,
                     "hcfga" = ig_nbs_hcfga_percentile2value)
        vals <- suppressWarnings(fn(sex = sex, gest_age = age, p = percentiles))
        expect_length(object = vals, n = length(sex))
        expect_true(all(is.na(c(
          vals[2], # Because percentile is out of bounds
          vals[4]  # Because sex is not one of "M", "F" or "U"
        ))))
      })
    })

  # Test that bad input gives NA --> VPNS
  with(
    list(sex = c("M", "F", "U", "X", "M"),
         percentiles = c(0.5, -1, 0.5, 0.5, 0.5),
         age = c(161, 168, 175, 182, 210)),
    expr = {
      sapply(X = c("wfga", "lfga", "hcfga"), FUN = function(x) {
        fn <- switch(x,
                     "wfga" = ig_nbs_wfga_percentile2value,
                     "lfga" = ig_nbs_lfga_percentile2value,
                     "hcfga" = ig_nbs_hcfga_percentile2value)
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

testthat_v2x <- function(y, gest_age, sex, acronym, z_or_p = "zscores") {
  out_z_or_p <- switch(z_or_p,
         "zscores" = switch(acronym,
                            "wfga"  = ig_nbs_wfga_value2zscore(weight_kg = y, gest_age = gest_age, sex = sex),
                            "lfga"  = ig_nbs_lfga_value2zscore(length_cm = y, gest_age = gest_age, sex = sex),
                            "hcfga" = ig_nbs_hcfga_value2zscore(headcirc_cm = y, gest_age = gest_age, sex = sex),
                            "wlrfga" = ig_nbs_wlrfga_value2zscore(wei_len_ratio = y, gest_age = gest_age, sex = sex)),
         "percentiles" = switch(acronym,
                            "wfga"  = ig_nbs_wfga_value2percentile(weight_kg = y, gest_age = gest_age, sex = sex),
                            "lfga"  = ig_nbs_lfga_value2percentile(length_cm = y, gest_age = gest_age, sex = sex),
                            "hcfga" = ig_nbs_hcfga_value2percentile(headcirc_cm = y, gest_age = gest_age, sex = sex),
                            "wlrfga" = ig_nbs_wlrfga_value2percentile(wei_len_ratio = y, gest_age = gest_age, sex = sex)))
  out_value <- switch(z_or_p,
         "zscores" = switch(acronym,
                            "wfga"  = ig_nbs_wfga_zscore2value(z = out_z_or_p, gest_age = gest_age, sex = sex),
                            "lfga"  = ig_nbs_lfga_zscore2value(z = out_z_or_p, gest_age = gest_age, sex = sex),
                            "hcfga" = ig_nbs_hcfga_zscore2value(z = out_z_or_p, gest_age = gest_age, sex = sex),
                            "wlrfga" = ig_nbs_wlrfga_zscore2value(z = out_z_or_p, gest_age = gest_age, sex = sex)),
         "percentiles" = switch(acronym,
                            "wfga"  = ig_nbs_wfga_percentile2value(p = out_z_or_p, gest_age = gest_age, sex = sex),
                            "lfga"  = ig_nbs_lfga_percentile2value(p = out_z_or_p, gest_age = gest_age, sex = sex),
                            "hcfga" = ig_nbs_hcfga_percentile2value(p = out_z_or_p, gest_age = gest_age, sex = sex),
                            "wlrfga" = ig_nbs_wlrfga_percentile2value(p = out_z_or_p, gest_age = gest_age, sex = sex)))
  expect_equal(round(y, digits = 3), round(out_value, digits = 3))
}

test_that("Conversion of values to z-scores works", {
  # Weight for gestational age
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_age = 36 * 7, sex = "M", acronym = "wfga")
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_age = 40 * 7, sex = "F", acronym = "wfga")

  # Length for gestational age
  testthat_v2x(y = c(41.9, 43.8, 45.6, 47.3, 49.1), gest_age = 7 * 34, sex = "M", acronym = "lfga")
  testthat_v2x(y = c(46.7, 41.8, 43.5, 47.5, 48.1), gest_age = 7 * (41 + 3/7), sex = "F", acronym = "lfga")

  # Head circumference for gestational age
  testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), gest_age = 7 * (42 + 2/7), sex = "M", acronym = "hcfga")
  testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), gest_age = 7 * (37 + 4/7), sex = "F", acronym = "hcfga")

  # Weight-length ratio for gestational age
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_age = 7 * (42 + 2/7), sex = "M", acronym = "wlrfga")
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_age = 7 * (37 + 4/7), sex = "F", acronym = "wlrfga")
})

test_that("Conversion of values to percentiles works", {
  # Weight for gestational age
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_age = 7 * 36, sex = "M", acronym = "wfga", z_or_p = "percentiles")
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_age = 7 * 40, sex = "F", acronym = "wfga", z_or_p = "percentiles")

  # Length circumference for gestational age
  testthat_v2x(y = c(41.9, 43.8, 45.6, 47.3, 49.1), gest_age = 7 * 36, sex = "M", acronym = "lfga", z_or_p = "percentiles")
  testthat_v2x(y = c(46.7, 41.8, 43.5, 47.5, 48.1), gest_age = 7 * 40, sex = "F", acronym = "lfga", z_or_p = "percentiles")

  # Head circumference for gestational age
  testthat_v2x(y = c(41.9, 43.8, 45.6, 47.3, 49.1), gest_age = 7 * 36, sex = "M", acronym = "hcfga", z_or_p = "percentiles")
  testthat_v2x(y = c(46.7, 41.8, 43.5, 47.5, 48.1), gest_age = 7 * 40, sex = "F", acronym = "hcfga", z_or_p = "percentiles")

  # Weight-length ratio for gestational age
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_age = 7 * 36, sex = "M", acronym = "wlrfga", z_or_p = "percentiles")
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_age = 7 * 40, sex = "F", acronym = "wlrfga", z_or_p = "percentiles")
})

test_that(desc = "Interpolation of LMS values can be performed",
          code = {
            testthat::expect_false(
              anyNA(ig_nbs_value2zscore(y = 4.5,
                                        gest_age = seq(260, 264, by = 0.5),
                                        sex = "M",
                                        acronym = "wfga")))
          })