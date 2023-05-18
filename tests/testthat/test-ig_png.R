test_zscore_tbls <- function(sex, age_lower, age_upper, acronym, tolerance) {
  get_pm_ages <- function(lower, upper) {
    pma <- gigs::ig_png$wfa$male$zscores$pma_weeks
    pma[which(pma >= lower & pma <= upper)]
  }
  roundto <- ifelse(acronym == "wfa", yes = 2, no = 1)
  tbl_names <- c("SD3neg", "SD2neg", "SD1neg", "SD0", "SD1", "SD2", "SD3")
  pkg_tbl <- lapply(X = -3:3,
                    FUN = function (x) {
                      fn <- switch(acronym,
                                   "wfa" = ig_png_wfa_zscore2value,
                                   "lfa" = ig_png_lfa_zscore2value,
                                   "hcfa" = ig_png_hcfa_zscore2value)
                      round2(fn(z = x, pma_weeks = get_pm_ages(age_lower, age_upper), sex = sex), digits = roundto)
                }) |>
    do.call(what = cbind) |>
    as.data.frame()
  names(pkg_tbl) <- tbl_names
  pkg_tbl$pma_weeks <- get_pm_ages(age_lower, age_upper)
  pkg_tbl <- pkg_tbl[, c(ncol(pkg_tbl), 1:(ncol(pkg_tbl) - 1))]
  sex_ <- ifelse(sex == "M", yes = "male", no = "female" )
  ref_tbl <- gigs::ig_png[[acronym]][[sex_]]$zscores
  ref_tbl <- ref_tbl[ref_tbl$pma_weeks >= age_lower & ref_tbl$pma_weeks <= age_upper, ]
  rownames(pkg_tbl) <- NULL
  rownames(ref_tbl) <- NULL
  expect_equal(object = pkg_tbl, expected = ref_tbl, tolerance = tolerance)
}

test_that("Conversion of z-scores to values works", {
  sex <- rep(c("M", "F"), length(names(gigs::ig_png)))
  lower <-  rep(28, 2 * length(names(gigs::ig_png)))
  upper <- rep(64, 2 * length(names(gigs::ig_png)))
  acronyms <- rep(names(gigs::ig_png), times = rep(2, length(names(gigs::ig_png))))
  tolerance <- 0.01
  invisible(mapply(FUN = test_zscore_tbls, sex, lower, upper, acronyms, tolerance))
})

test_percentile_tbls <- function(sex, age_lower, age_upper, acronym, tolerance) {
  get_pm_ages <- function(lower, upper) {
    pma <- gigs::ig_png$wfa$male$zscores$pma_weeks
    pma[which(pma >= lower & pma <= upper)]
  }
  roundto <- ifelse(acronym == "wfa", yes = 2, no = 1)
  tbl_names <- c("P03", "P05", "P10", "P50", "P90", "P95", "P97")
  pkg_tbl <- lapply(X = c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97),
                    FUN = function (x) {
                      fn <- switch(acronym,
                                   "wfa" = ig_png_wfa_percentile2value,
                                   "lfa" = ig_png_lfa_percentile2value,
                                   "hcfa" = ig_png_hcfa_percentile2value)
                      round2(fn(p = x, pma_weeks = get_pm_ages(age_lower, age_upper), sex = sex), digits = roundto)
                }) |>
    do.call(what = cbind) |>
    as.data.frame()
  names(pkg_tbl) <- tbl_names
  pkg_tbl$pma_weeks <- get_pm_ages(age_lower, age_upper)
  pkg_tbl <- pkg_tbl[, c(ncol(pkg_tbl), 1:(ncol(pkg_tbl) - 1))]
  sex_ <- ifelse(sex == "M", yes = "male", no = "female" )
  ref_tbl <- gigs::ig_png[[acronym]][[sex_]]$percentiles
  ref_tbl <- ref_tbl[ref_tbl$pma_weeks >= age_lower & ref_tbl$pma_weeks <= age_upper, ]
  rownames(pkg_tbl) <- NULL
  rownames(ref_tbl) <- NULL
  expect_equal(object = pkg_tbl, expected = ref_tbl, tolerance = tolerance)
}

test_that("Conversion of percentiles to values works", {
  sex <- rep(c("M", "F"), length(names(gigs::ig_png)))
  lower <-  rep(28, 2 * length(names(gigs::ig_png)))
  upper <- rep(64, 2 * length(names(gigs::ig_png)))
  acronyms <- rep(names(gigs::ig_png), times = rep(2, length(names(gigs::ig_png))))
  tolerance <- 0.01
  invisible(mapply(FUN = test_percentile_tbls, sex, lower, upper, acronyms, tolerance))
})

testthat_v2x <- function(y, pma_weeks, sex, acronym, z_or_p = "zscores") {
  out_z_or_p <- switch(z_or_p,
         "zscores" = switch(acronym,
                            "wfa"  = ig_png_wfa_value2zscore(weight_kg = y, pma_weeks = pma_weeks, sex = sex),
                            "lfa"  = ig_png_lfa_value2zscore(length_cm = y, pma_weeks = pma_weeks, sex = sex),
                            "hcfa" = ig_png_hcfa_value2zscore(headcirc_cm = y, pma_weeks = pma_weeks, sex = sex)),
         "percentiles" = switch(acronym,
                            "wfa"  = ig_png_wfa_value2percentile(weight_kg = y, pma_weeks = pma_weeks, sex = sex),
                            "lfa"  = ig_png_lfa_value2percentile(length_cm = y, pma_weeks = pma_weeks, sex = sex),
                            "hcfa" = ig_png_hcfa_value2percentile(headcirc_cm = y, pma_weeks = pma_weeks, sex = sex)))
  out_value <- switch(z_or_p,
         "zscores" = switch(acronym,
                            "wfa"  = ig_png_wfa_zscore2value(z = out_z_or_p, pma_weeks = pma_weeks, sex = sex),
                            "lfa"  = ig_png_lfa_zscore2value(z = out_z_or_p, pma_weeks = pma_weeks, sex = sex),
                            "hcfa" = ig_png_hcfa_zscore2value(z = out_z_or_p, pma_weeks = pma_weeks, sex = sex)),
         "percentiles" = switch(acronym,
                            "wfa"  = ig_png_wfa_percentile2value(p = out_z_or_p, pma_weeks = pma_weeks, sex = sex),
                            "lfa"  = ig_png_lfa_percentile2value(p = out_z_or_p, pma_weeks = pma_weeks, sex = sex),
                            "hcfa" = ig_png_hcfa_percentile2value(p = out_z_or_p, pma_weeks = pma_weeks, sex = sex)))
  return(all(round(y, digits = 3) == round(out_value, digits = 3), na.rm = TRUE))
}

test_that("Conversion of values to z-scores works", {
  # Weight for age
  expect_true(testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), pma_weeks = 36, sex = "M", acronym = "wfa"))
  expect_true(testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), pma_weeks = 40, sex = "F", acronym = "wfa"))
  # Length for age
  expect_true(testthat_v2x(y = c(41.9, 43.8, 45.6, 47.3, 49.1), pma_weeks = 57, sex = "M", acronym = "lfa"))
  expect_true(testthat_v2x(y = c(46.7, 41.8, 43.5, 47.5, 48.1), pma_weeks = 36, sex = "F", acronym = "lfa"))
  # Head circumference for age
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), pma_weeks = 48, sex = "M", acronym = "hcfa"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), pma_weeks = 60, sex = "F", acronym = "hcfa"))

  # NA should arise in final vector, will be reflected in this function
  expect_true(testthat_v2x(y = 26.3, pma_weeks = 50:65, sex = "F", acronym = "hcfa"))
})

test_that("Conversion of values to percentiles works", {
  # Weight for age
  expect_true(testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), pma_weeks = 36, sex = "M", acronym = "wfa", z_or_p = "percentiles"))
  expect_true(testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), pma_weeks = 40, sex = "F", acronym = "wfa", z_or_p = "percentiles"))
  # Length for age
  expect_true(testthat_v2x(y = c(41.9, 43.8, 45.6, 47.3, 49.1), pma_weeks = 57, sex = "M", acronym = "lfa", z_or_p = "percentiles"))
  expect_true(testthat_v2x(y = c(46.7, 41.8, 43.5, 47.5, 48.1), pma_weeks = 36, sex = "F", acronym = "lfa", z_or_p = "percentiles"))
  # Head circumference for age
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), pma_weeks = 48, sex = "M", acronym = "hcfa", z_or_p = "percentiles"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), pma_weeks = 60, sex = "F", acronym = "hcfa", z_or_p = "percentiles"))

  # NA should arise in final vector, will be reflected in this function
  expect_true(testthat_v2x(y = 26.3, pma_weeks = 50:65, sex = "F", acronym = "hcfa", z_or_p = "percentiles"))
})