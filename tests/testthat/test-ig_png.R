test_zscore_tbls <- function(sex, x_lower, x_upper, acronym, tolerance) {
  get_xvars <- function(lower, upper) {
    x <- gigs::ig_png[[acronym]]$male$zscores[, 1]
    x[which(x >= lower & x <= upper)]
  }
  roundto <- ifelse(acronym %in% c("wfa", "wfl"), yes = 2, no = 1)
  tbl_names <- c("SD3neg", "SD2neg", "SD1neg", "SD0", "SD1", "SD2", "SD3")
  pkg_tbl <- lapply(X = -3:3,
                    FUN = function (x) {
                      fn <- switch(acronym,
                                   "wfa" = ig_png_wfa_zscore2value,
                                   "lfa" = ig_png_lfa_zscore2value,
                                   "hcfa" = ig_png_hcfa_zscore2value,
                                   "wfl" = ig_png_wfl_zscore2value)
                      round2(fn(z = x, get_xvars(x_lower, x_upper), sex = sex), digits = roundto)
                }) |>
    do.call(what = cbind) |>
    as.data.frame()
  names(pkg_tbl) <- tbl_names
  col1_name <- ifelse(acronym == "wfl", yes = "length_cm", no = "pma_weeks")
  pkg_tbl[[col1_name]] <- get_xvars(x_lower, x_upper)
  pkg_tbl <- pkg_tbl[, c(ncol(pkg_tbl), 1:(ncol(pkg_tbl) - 1))]
  sex_ <- ifelse(sex == "M", yes = "male", no = "female" )
  ref_tbl <- gigs::ig_png[[acronym]][[sex_]]$zscores
  ref_tbl <- ref_tbl[ref_tbl[,1] >= x_lower & ref_tbl[,1] <= x_upper, ]
  rownames(pkg_tbl) <- NULL
  rownames(ref_tbl) <- NULL
  expect_equal(object = pkg_tbl, expected = ref_tbl, tolerance = tolerance)
}

test_that("Conversion of z-scores to values works", {
  png_names <- names(gigs::ig_png)
  sex <- rep(c("M", "F"), length(png_names))
  lower <-  rep.int(c(27, 35), c(6, 2))
  upper <- rep.int(c(64, 65), c(6, 2))
  acronyms <- rep(png_names, times = rep(2, length(png_names)))
  tolerance <- 10e-3
  invisible(mapply(FUN = test_zscore_tbls, sex, lower, upper, acronyms, tolerance))
})

test_percentile_tbls <- function(sex, x_lower, x_upper, acronym, tolerance) {
  get_xvars <- function(lower, upper) {
    x <- gigs::ig_png[[acronym]]$male$zscores[, 1]
    x[which(x >= lower & x <= upper)]
  }
  roundto <- ifelse(acronym %in% c("wfa", "wfl"), yes = 2, no = 1)
  tbl_names <- c("P03", "P05", "P10", "P50", "P90", "P95", "P97")
  pkg_tbl <- lapply(X = c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97),
                    FUN = function (x) {
                      fn <- switch(acronym,
                                   "wfa" = ig_png_wfa_percentile2value,
                                   "lfa" = ig_png_lfa_percentile2value,
                                   "hcfa" = ig_png_hcfa_percentile2value,
                                   "wfl" = ig_png_wfl_percentile2value)
                      round2(fn(p = x, get_xvars(x_lower, x_upper), sex = sex), digits = roundto)
                }) |>
    do.call(what = cbind) |>
    as.data.frame()
  names(pkg_tbl) <- tbl_names
  col1_name <- ifelse(acronym == "wfl", yes = "length_cm", no = "pma_weeks")
  pkg_tbl[[col1_name]] <- get_xvars(x_lower, x_upper)
  pkg_tbl <- pkg_tbl[, c(ncol(pkg_tbl), 1:(ncol(pkg_tbl) - 1))]
  sex_ <- ifelse(sex == "M", yes = "male", no = "female" )
  ref_tbl <- gigs::ig_png[[acronym]][[sex_]]$percentiles
  ref_tbl <- ref_tbl[ref_tbl[,1] >= x_lower & ref_tbl[,1] <= x_upper, ]
  rownames(pkg_tbl) <- NULL
  rownames(ref_tbl) <- NULL
  expect_equal(object = pkg_tbl, expected = ref_tbl, tolerance = tolerance)
}

test_that("Conversion of percentiles to values works", {
  png_names <- names(gigs::ig_png)
  sex <- rep(c("M", "F"), length(png_names))
  lower <-  rep.int(c(27, 35), c(6, 2))
  upper <- rep.int(c(64, 65), c(6, 2))
  acronyms <- rep(png_names, times = rep(2, length(png_names)))
  tolerance <- 10e-3
  invisible(mapply(FUN = test_percentile_tbls, sex, lower, upper, acronyms, tolerance))
})

testthat_v2x <- function(y, x, sex, acronym, z_or_p = "zscores") {
  out_z_or_p <- switch(z_or_p,
         "zscores" = switch(acronym,
                            "wfa"  = ig_png_wfa_value2zscore(weight_kg = y, pma_weeks = x, sex = sex),
                            "lfa"  = ig_png_lfa_value2zscore(length_cm = y, pma_weeks = x, sex = sex),
                            "hcfa" = ig_png_hcfa_value2zscore(headcirc_cm = y, pma_weeks = x, sex = sex),
                            "wfl" = ig_png_wfl_value2zscore(weight_kg = y, length_cm = x, sex = sex)),
         "percentiles" = switch(acronym,
                            "wfa"  = ig_png_wfa_value2percentile(weight_kg = y, pma_weeks = x, sex = sex),
                            "lfa"  = ig_png_lfa_value2percentile(length_cm = y, pma_weeks = x, sex = sex),
                            "hcfa" = ig_png_hcfa_value2percentile(headcirc_cm = y, pma_weeks = x, sex = sex),
                            "wfl" = ig_png_wfl_value2percentile(weight_kg = y, length_cm = x, sex = sex)))
  out_value <- switch(z_or_p,
         "zscores" = switch(acronym,
                            "wfa"  = ig_png_wfa_zscore2value(z = out_z_or_p, pma_weeks = x, sex = sex),
                            "lfa"  = ig_png_lfa_zscore2value(z = out_z_or_p, pma_weeks = x, sex = sex),
                            "hcfa" = ig_png_hcfa_zscore2value(z = out_z_or_p, pma_weeks = x, sex = sex),
                            "wfl" = ig_png_wfl_zscore2value(z = out_z_or_p, length_cm = x, sex = sex)),
         "percentiles" = switch(acronym,
                            "wfa"  = ig_png_wfa_percentile2value(p = out_z_or_p, pma_weeks = x, sex = sex),
                            "lfa"  = ig_png_lfa_percentile2value(p = out_z_or_p, pma_weeks = x, sex = sex),
                            "hcfa" = ig_png_hcfa_percentile2value(p = out_z_or_p, pma_weeks = x, sex = sex),
                            "wfl" = ig_png_wfl_percentile2value(p = out_z_or_p, length_cm = x, sex = sex)))
  expect_true(all(round2(y, digits = 3) == round2(out_value, digits = 3), na.rm = TRUE))
}

test_that("Conversion of values to z-scores works", {
  # Weight for age
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 36, sex = "M", acronym = "wfa")
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 40, sex = "F", acronym = "wfa")
  # Length for age
  testthat_v2x(y = c(41.9, 43.8, 45.6, 47.3, 49.1), x = 57, sex = "M", acronym = "lfa")
  testthat_v2x(y = c(46.7, 41.8, 43.5, 47.5, 48.1), x = 36, sex = "F", acronym = "lfa")
  # Head circumference for age
  testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 48, sex = "M", acronym = "hcfa")
  testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 60, sex = "F", acronym = "hcfa")
  # Weight for length
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 48, sex = "M", acronym = "wfl")
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 60, sex = "F", acronym = "wfl")

  # NA should arise in final vector, will be reflected in this function
  testthat_v2x(y = 26.3, x = 50:65, sex = "F", acronym = "hcfa")
})

test_that("Conversion of values to percentiles works", {
  # Weight for age
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 36, sex = "M", acronym = "wfa", z_or_p = "percentiles")
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 40, sex = "F", acronym = "wfa", z_or_p = "percentiles")
  # Head circumference for age
  testthat_v2x(y = c(41.9, 43.8, 45.6, 47.3, 49.1), x = 57, sex = "M", acronym = "lfa", z_or_p = "percentiles")
  testthat_v2x(y = c(46.7, 41.8, 43.5, 47.5, 48.1), x = 36, sex = "F", acronym = "lfa", z_or_p = "percentiles")
  # Length for age
  testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 48, sex = "M", acronym = "hcfa", z_or_p = "percentiles")
  testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 60, sex = "F", acronym = "hcfa", z_or_p = "percentiles")
  # Weight for length
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 48, sex = "M", acronym = "wfl", z_or_p = "percentiles")
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 60, sex = "F", acronym = "wfl", z_or_p = "percentiles")

  # NA should arise in final vector, will be reflected in this function
  testthat_v2x(y = 26.3, x = 50:65, sex = "F", acronym = "hcfa", z_or_p = "percentiles")
})

test_that(desc = "Bad input types cause errors.",
          code = {
            x <- 30:35
            x_len <- length(x)
            z <- rep_len(-3:3, x_len)
            sex <- rep_len(c("M", "F"), x_len)
            acronym <- rep_len(names(gigs::ig_png), x_len)
            # Test failures for each arg when converting zscores to values
            testthat::expect_error(
              ig_png_zscore2value(as.character(z), x, sex, acronym)
            )
            testthat::expect_error(
              ig_png_zscore2value(z, as.character(x), sex, acronym)
            )
            testthat::expect_error(ig_png_zscore2value(z, x, 1, acronym))
            testthat::expect_error(ig_png_zscore2value(z, x, sex, 1))

            # And for conversion of values to zscores
            y <- ig_png_zscore2value(z, x, sex, acronym)
            testthat::expect_error(
              ig_png_value2zscore(as.character(y), x, sex, acronym)
            )
            testthat::expect_error(
              ig_png_value2zscore(y, as.character(x), sex, acronym)
            )
            testthat::expect_error(ig_png_value2zscore(y, x, 1, acronym))
            testthat::expect_error(ig_png_value2zscore(y, x, sex, 1))
})