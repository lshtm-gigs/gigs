package_ref_zscore_tbls <- function(sex, x_lower, x_upper, acronym) {
  range_x <- seq(x_lower, x_upper, by = ifelse(acronym == "wfl" | acronym == "wfh", yes = 0.1, no = 1))
  tbl_names <- c("SD4neg", "SD3neg", "SD2neg", "SD1neg", "SD0", "SD1", "SD2", "SD3", "SD4")
  pkg_tbl <- lapply(X = -4:4,
                    FUN = function (x) {
                      fn <- switch(acronym,
                                   "wfa" =  who_gs_wfa_zscore2value,
                                   "bfa" =  who_gs_bfa_zscore2value,
                                   "lhfa" = who_gs_lhfa_zscore2value,
                                   "wfl" =  who_gs_wfl_zscore2value,
                                   "wfh" =  who_gs_wfh_zscore2value,
                                   "hcfa" = who_gs_hcfa_zscore2value,
                                   "acfa" = who_gs_acfa_zscore2value,
                                   "ssfa" = who_gs_ssfa_zscore2value,
                                   "tsfa" = who_gs_tsfa_zscore2value)
                      round(fn(z = x, x = range_x, sex = sex), digits = 3)
                }) |>
    do.call(what = cbind) |>
    as.data.frame()
  names(pkg_tbl) <- tbl_names
  pkg_tbl$age_days <- as.numeric(range_x)
  pkg_tbl <- pkg_tbl[, c(ncol(pkg_tbl), 1:(ncol(pkg_tbl) - 1))]
  if (acronym == "wfl") {
    names(pkg_tbl)[1] <- "length_cm"
  } else if (acronym == "wfh") {
    names(pkg_tbl)[1] <- "height_cm"
  }
  sex_ <- ifelse(sex == "M", yes = "male", no = "female" )
  in_or_out <- round(unname(unlist(gigs::who_gs[[acronym]][[sex_]]$zscores[, 1])), digits = 1) %in% round(range_x, 1)
  ref_tbl <- gigs::who_gs[[acronym]][[sex_]]$zscores[in_or_out, ] |> as.data.frame()
  rownames(pkg_tbl) <- NULL
  rownames(ref_tbl) <- NULL
  list(package = pkg_tbl, reference = ref_tbl)
}

test_that("Conversion of z-scores to WHO values works", {
  tolerance <- 1e-3

  # Weight for age
  lower <- 1
  upper <- 10
  wfa_m <- package_ref_zscore_tbls(sex = "M", lower, upper, acronym = "wfa")
  expect_equal(object = wfa_m$package, expected = wfa_m$reference, tolerance = tolerance)
  wfa_f <- package_ref_zscore_tbls(sex = "F", lower, upper, acronym = "wfa")
  expect_equal(object = wfa_f$package, expected = wfa_f$reference, tolerance = tolerance)

  # BMI for age
  lower <- 201
  upper <- 211
  bfa_m <- package_ref_zscore_tbls(sex = "M", lower, upper, acronym = "bfa")
  expect_equal(object = bfa_m$package, expected = bfa_m$reference, tolerance = tolerance)
  bfa_f <- package_ref_zscore_tbls(sex = "F", lower, upper, acronym = "bfa")
  expect_equal(object = bfa_f$package, expected = bfa_f$reference, tolerance = tolerance)

  # Length/height for age
  lower <- 670
  upper <- 690
  lhfa_m <- package_ref_zscore_tbls(sex = "M", lower, upper, acronym = "lhfa")
  expect_equal(object = lhfa_m$package, expected = lhfa_m$reference, tolerance = tolerance)
  lhfa_f <- package_ref_zscore_tbls(sex = "F", lower, upper, acronym = "lhfa")
  expect_equal(object = lhfa_f$package, expected = lhfa_f$reference, tolerance = tolerance)

  # Weight for length
  lower <- 45
  upper <- 110
  wfl_m <- package_ref_zscore_tbls(sex = "M", lower, upper, acronym = "wfl")
  expect_equal(object = wfl_m$package, expected = wfl_m$reference, tolerance = tolerance)
  wfl_f <- package_ref_zscore_tbls(sex = "F", lower, upper, acronym = "wfl")
  expect_equal(object = wfl_f$package, expected = wfl_f$reference, tolerance = tolerance)

  # Weight for height
  lower <- 65; upper <- 120
  wfh_m <- package_ref_zscore_tbls(sex = "M", lower, upper, acronym = "wfh")
  expect_equal(object = wfh_m$package, expected = wfh_m$reference, tolerance = tolerance)
  wfh_f <- package_ref_zscore_tbls(sex = "F", lower, upper, acronym = "wfh")
  expect_equal(object = wfh_f$package, expected = wfh_f$reference, tolerance = tolerance)

  # Head circumference for age
  lower <- 410; upper <- 530
  hcfa_m <- package_ref_zscore_tbls(sex = "M", lower, upper, acronym = "hcfa")
  expect_equal(object = hcfa_m$package, expected = hcfa_m$reference, tolerance = tolerance)
  hcfa_f <- package_ref_zscore_tbls(sex = "F", lower, upper, acronym = "hcfa")
  expect_equal(object = hcfa_m$package, expected = hcfa_m$reference, tolerance = tolerance)

  # Arm circumference for age
  lower <- 500; upper <- 620
  acfa_m <- package_ref_zscore_tbls(sex = "M", lower, upper, acronym = "acfa")
  expect_equal(object = acfa_m$package, expected = acfa_m$reference, tolerance = tolerance)
  acfa_f <- package_ref_zscore_tbls(sex = "F", lower, upper, acronym = "acfa")
  expect_equal(object = acfa_f$package, expected = acfa_f$reference, tolerance = tolerance)

  # Subscapular skinfold for age
  lower <- 700; upper <- 850
  ssfa_m <- package_ref_zscore_tbls(sex = "M", lower, upper, acronym = "ssfa")
  expect_equal(object = ssfa_m$package, expected = ssfa_m$reference, tolerance = tolerance)
  ssfa_f <- package_ref_zscore_tbls(sex = "F", lower, upper, acronym = "ssfa")
  expect_equal(object = ssfa_f$package, expected = ssfa_f$reference, tolerance = tolerance)

  # Triceps skinfold for age
  lower <- 500; upper <- 620
  tsfa_m <- package_ref_zscore_tbls(sex = "M", lower, upper, acronym = "tsfa")
  expect_equal(object = tsfa_m$package, expected = tsfa_m$reference, tolerance = tolerance)
  tsfa_f <- package_ref_zscore_tbls(sex = "F", lower, upper, acronym = "tsfa")
  expect_equal(object = tsfa_f$package, expected = tsfa_f$reference, tolerance = tolerance)
})

test_that("Inputs with only incorrect acronyms return NA values", {
  expect_equal(object = who_gs_zscore2value(z = 0, x = 50, sex = "M", acronym = "also_wrong"),
               expected = rep(NA, length(0)))
  expect_equal(object = who_gs_zscore2value(z = -3:3, x = 50, sex = rep_len(c("M", "F"), 7), acronym = "wrong"),
               expected = rep(NA, length(-3:3)))
})

package_ref_percentile_tbls <- function(sex, x_lower, x_upper, acronym) {
  range_x <- seq(x_lower, x_upper, by = ifelse(acronym == "wfl" | acronym == "wfh", yes = 0.1, no = 1))
  tbl_names <- c("P01", "P1", "P3", "P5", "P10", "P15", "P25", "P50", "P75", "P85", "P90", "P95", "P97", "P99", "P999")
  pkg_tbl <- lapply(X = c(0.001, 0.01, 0.03, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.97, 0.99, 0.999),
                    FUN = function (x) {
                      fn <- switch(acronym,
                                   "wfa" =  who_gs_wfa_percentile2value,
                                   "bfa" =  who_gs_bfa_percentile2value,
                                   "lhfa" = who_gs_lhfa_percentile2value,
                                   "wfl" =  who_gs_wfl_percentile2value,
                                   "wfh" =  who_gs_wfh_percentile2value,
                                   "hcfa" = who_gs_hcfa_percentile2value,
                                   "acfa" = who_gs_acfa_percentile2value,
                                   "ssfa" = who_gs_ssfa_percentile2value,
                                   "tsfa" = who_gs_tsfa_percentile2value)
                      round(fn(p = x, x = range_x, sex = sex), digits = 3)
                }) |>
    do.call(what = cbind) |>
    as.data.frame()
  names(pkg_tbl) <- tbl_names
  pkg_tbl$age_days <- as.numeric(range_x)
  pkg_tbl <- pkg_tbl[, c(ncol(pkg_tbl), 1:(ncol(pkg_tbl) - 1))]
  if (acronym == "wfl") {
    names(pkg_tbl)[1] <- "length_cm"
  } else if (acronym == "wfh") {
    names(pkg_tbl)[1] <- "height_cm"
  }
  sex_ <- ifelse(sex == "M", yes = "male", no = "female" )
  in_or_out <- round(unname(unlist(gigs::who_gs[[acronym]][[sex_]]$percentiles[, 1])), digits = 1) %in% round(range_x, 1)
  ref_tbl <- gigs::who_gs[[acronym]][[sex_]]$percentiles[in_or_out, ] |> as.data.frame()
  rownames(pkg_tbl) <- NULL
  rownames(ref_tbl) <- NULL
  list(package = pkg_tbl, reference = ref_tbl)
}

test_that("Conversion of percentiles to values works", {
  tolerance <- 1e-2

  # Weight for age
  lower <- 1
  upper <- 10
  wfa_m <- package_ref_percentile_tbls(sex = "M", lower, upper, acronym = "wfa")
  expect_equal(object = wfa_m$package, expected = wfa_m$reference, tolerance = tolerance)
  wfa_f <- package_ref_percentile_tbls(sex = "F", lower, upper, acronym = "wfa")
  expect_equal(object = wfa_f$package, expected = wfa_f$reference, tolerance = tolerance)

  # BMI for age
  lower <- 201; upper <- 211
  bfa_m <- package_ref_percentile_tbls(sex = "M", lower, upper, acronym = "bfa")
  expect_equal(object = bfa_m$package, expected = bfa_m$reference, tolerance = tolerance)
  bfa_f <- package_ref_percentile_tbls(sex = "F", lower, upper, acronym = "bfa")
  expect_equal(object = bfa_f$package, expected = bfa_f$reference, tolerance = tolerance)

  # Length/height for age
  lower <- 670; upper <- 690
  lhfa_m <- package_ref_percentile_tbls(sex = "M", lower, upper, acronym = "lhfa")
  expect_equal(object = lhfa_m$package, expected = lhfa_m$reference, tolerance = tolerance)
  lhfa_f <- package_ref_percentile_tbls(sex = "F", lower, upper, acronym = "lhfa")
  expect_equal(object = lhfa_f$package, expected = lhfa_f$reference, tolerance = tolerance)

  # Weight for length
  lower <- 45; upper <- 110
  wfl_m <- package_ref_percentile_tbls(sex = "M", lower, upper, acronym = "wfl")
  expect_equal(object = wfl_m$package, expected = wfl_m$reference, tolerance = tolerance)
  wfl_f <- package_ref_percentile_tbls(sex = "F", lower, upper, acronym = "wfl")
  expect_equal(object = wfl_f$package, expected = wfl_f$reference, tolerance = tolerance)

  # Weight for height
  lower <- 65; upper <- 120
  wfh_m <- package_ref_percentile_tbls(sex = "M", lower, upper, acronym = "wfh")
  expect_equal(object = wfh_m$package, expected = wfh_m$reference, tolerance = tolerance)
  wfh_f <- package_ref_percentile_tbls(sex = "F", lower, upper, acronym = "wfh")
  expect_equal(object = wfh_f$package, expected = wfh_f$reference, tolerance = tolerance)

    # Head circumference for age
  lower <- 410; upper <- 530
  hcfa_m <- package_ref_percentile_tbls(sex = "M", lower, upper, acronym = "hcfa")
  expect_equal(object = hcfa_m$package, expected = hcfa_m$reference, tolerance = tolerance)
  hcfa_f <- package_ref_percentile_tbls(sex = "F", lower, upper, acronym = "hcfa")
  expect_equal(object = hcfa_m$package, expected = hcfa_m$reference, tolerance = tolerance)

  # Arm circumference for age
  lower <- 500; upper <- 620
  acfa_m <- package_ref_percentile_tbls(sex = "M", lower, upper, acronym = "acfa")
  expect_equal(object = acfa_m$package, expected = acfa_m$reference, tolerance = tolerance)
  acfa_f <- package_ref_percentile_tbls(sex = "F", lower, upper, acronym = "acfa")
  expect_equal(object = acfa_f$package, expected = acfa_f$reference, tolerance = tolerance)

  # Subscapular skinfold for age
  lower <- 700; upper <- 850
  ssfa_m <- package_ref_percentile_tbls(sex = "M", lower, upper, acronym = "ssfa")
  expect_equal(object = ssfa_m$package, expected = ssfa_m$reference, tolerance = tolerance)
  ssfa_f <- package_ref_percentile_tbls(sex = "F", lower, upper, acronym = "ssfa")
  expect_equal(object = ssfa_f$package, expected = ssfa_f$reference, tolerance = tolerance)

  # Triceps skinfold for age
  lower <- 500; upper <- 620
  tsfa_m <- package_ref_percentile_tbls(sex = "M", lower, upper, acronym = "tsfa")
  expect_equal(object = tsfa_m$package, expected = tsfa_m$reference, tolerance = tolerance)
  tsfa_f <- package_ref_percentile_tbls(sex = "F", lower, upper, acronym = "tsfa")
  expect_equal(object = tsfa_f$package, expected = tsfa_f$reference, tolerance = tolerance)
})

testthat_v2x <- function(y, x, sex, acronym, z_or_p = "zscores") {
  out_z_or_p <- switch(z_or_p,
         "zscores" = switch(acronym,
                            "wfa" = who_gs_wfa_value2zscore(weight_kg = y, age_days = x, sex = sex),
                            "bfa" = who_gs_bfa_value2zscore(bmi = y, age_days = x, sex = sex),
                            "lhfa" = who_gs_lhfa_value2zscore(lenht_cm = y, age_days = x, sex = sex),
                            "wfl" = who_gs_wfl_value2zscore(weight_kg = y, length_cm = x, sex = sex),
                            "wfh" = who_gs_wfh_value2zscore(weight_kg = y, height_cm = x, sex = sex),
                            "hcfa" = who_gs_hcfa_value2zscore(y, x, sex),
                            "acfa" = who_gs_acfa_value2zscore(y, x, sex),
                            "ssfa" = who_gs_ssfa_value2zscore(y, x, sex),
                            "tsfa" = who_gs_tsfa_value2zscore(y, x, sex)
         ),
         "percentiles" = switch(acronym,
                                "wfa" = who_gs_wfa_value2percentile(weight_kg = y, age_days = x, sex = sex),
                                "bfa" = who_gs_bfa_value2percentile(bmi = y, age_days = x, sex = sex),
                                "lhfa" = who_gs_lhfa_value2percentile(lenht_cm = y, age_days = x, sex = sex),
                                "wfl" = who_gs_wfl_value2percentile(weight_kg = y, length_cm = x, sex = sex),
                                "wfh" = who_gs_wfh_value2percentile(weight_kg = y, height_cm = x, sex = sex),
                                "hcfa" = who_gs_hcfa_value2percentile(y, x, sex),
                                "acfa" = who_gs_acfa_value2percentile(y, x, sex),
                                "ssfa" = who_gs_ssfa_value2percentile(y, x, sex),
                                "tsfa" = who_gs_tsfa_value2percentile(y, x, sex)))
  out_value <- switch(z_or_p,
         "zscores" = switch(acronym,
                            "wfa"  = who_gs_wfa_zscore2value(z = out_z_or_p, x = x, sex = sex),
                            "bfa"  = who_gs_bfa_zscore2value(z = out_z_or_p, x = x, sex = sex),
                            "lhfa" = who_gs_lhfa_zscore2value(z = out_z_or_p, x = x, sex = sex),
                            "wfl" =  who_gs_wfl_zscore2value(z = out_z_or_p, x = x, sex = sex),
                            "wfh" =  who_gs_wfh_zscore2value(z = out_z_or_p, x = x, sex = sex),
                            "hcfa" = who_gs_hcfa_zscore2value(z = out_z_or_p, x = x, sex = sex),
                            "acfa" = who_gs_acfa_zscore2value(z = out_z_or_p, x = x, sex = sex),
                            "ssfa" = who_gs_ssfa_zscore2value(z = out_z_or_p, x = x, sex = sex),
                            "tsfa" = who_gs_tsfa_zscore2value(z = out_z_or_p, x = x, sex = sex)),
         "percentiles" = switch(acronym,
                            "wfa"  = who_gs_wfa_percentile2value(p = out_z_or_p, x = x, sex = sex),
                            "bfa"  = who_gs_bfa_percentile2value(p = out_z_or_p, x = x, sex = sex),
                            "lhfa" = who_gs_lhfa_percentile2value(p = out_z_or_p, x = x, sex = sex),
                            "wfl" =  who_gs_wfl_percentile2value(p = out_z_or_p, x = x, sex = sex),
                            "wfh" =  who_gs_wfh_percentile2value(p = out_z_or_p, x = x, sex = sex),
                            "hcfa" = who_gs_hcfa_percentile2value(p = out_z_or_p, x = x, sex = sex),
                            "acfa" = who_gs_acfa_percentile2value(p = out_z_or_p, x = x, sex = sex),
                            "ssfa" = who_gs_ssfa_percentile2value(p = out_z_or_p, x = x, sex = sex),
                            "tsfa" = who_gs_tsfa_percentile2value(p = out_z_or_p, x = x, sex = sex)))
  bool <- all(round(y, digits = 3) == round(out_value, digits = 3), na.rm = TRUE)
  return(bool)
}

test_that("Conversion of values to z-scores works", {
  # Weight for age
  expect_true(testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 36, sex = "M", acronym = "wfa"))
  expect_true(testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 40, sex = "F", acronym = "wfa"))
  # BMI for age
  expect_true(testthat_v2x(y = c(41.9, 43.8, 45.6, 47.3, 49.1) - 28, x = 57, sex = "M", acronym = "bfa"))
  expect_true(testthat_v2x(y = c(46.7, 41.8, 43.5, 47.5, 48.1) - 28, x = 36, sex = "F", acronym = "bfa"))
  # Length/height for age
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 48, sex = "M", acronym = "lhfa"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 60, sex = "F", acronym = "lhfa"))
  # Length/height for age
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 70, sex = "M", acronym = "wfl"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 75, sex = "F", acronym = "wfl"))
  # Length/height for age
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 90, sex = "M", acronym = "wfh"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 95, sex = "F", acronym = "wfh"))
  # Head circumference for age
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 90, sex = "M", acronym = "hcfa"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 95, sex = "F", acronym = "hcfa"))
  # Arm circumference for age
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 90 + 200, sex = "M", acronym = "acfa"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 95 + 200, sex = "F", acronym = "acfa"))
  # Subscapular skinfold for age
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 90 + 301, sex = "M", acronym = "ssfa"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 95 + 301, sex = "F", acronym = "ssfa"))
  # Triceps skinfold for age
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 90 + 403, sex = "M", acronym = "tsfa"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 95 + 403, sex = "F", acronym = "tsfa"))
  # NA should arise in final vector, will be reflected in this function
  expect_true(testthat_v2x(y = 26.3, x = 50:65, sex = "F", acronym = "lhfa"))
})

test_that("Conversion of values to percentiles works", {
  # Weight for age
  expect_true(testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 36, sex = "M", acronym = "wfa", z_or_p = "percentiles"))
  expect_true(testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 40, sex = "F", acronym = "wfa", z_or_p = "percentiles"))
  # BMI for age
  expect_true(testthat_v2x(y = c(41.9, 43.8, 45.6, 47.3, 49.1) - 28, x = 57, sex = "M", acronym = "bfa", z_or_p = "percentiles"))
  expect_true(testthat_v2x(y = c(46.7, 41.8, 43.5, 47.5, 48.1) - 29, x = 36, sex = "F", acronym = "bfa", z_or_p = "percentiles"))
  # Length/height for age
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 48, sex = "M", acronym = "lhfa", z_or_p = "percentiles"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 60, sex = "F", acronym = "lhfa", z_or_p = "percentiles"))
  # Weight for length
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1) - 21, x = 92.8, sex = "M", acronym = "wfl", z_or_p = "percentiles"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1) - 21, x = 92.8, sex = "F", acronym = "wfl", z_or_p = "percentiles"))
  # Weight for height
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1) - 20, x = 90, sex = "M", acronym = "wfh", z_or_p = "percentiles"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1) - 20, x = 95, sex = "F", acronym = "wfh", z_or_p = "percentiles"))
  # Head circumference for age
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 90, sex = "M", acronym = "hcfa", z_or_p = "percentiles"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 95, sex = "F", acronym = "hcfa", z_or_p = "percentiles"))
  # Arm circumference for age
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1) - 20, x = 90 + 200, sex = "M", acronym = "acfa", z_or_p = "percentiles"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1) - 20, x = 95 + 200, sex = "F", acronym = "acfa", z_or_p = "percentiles"))
  # Subscapular skinfold for age
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1) - 30, x = 90 + 301, sex = "M", acronym = "ssfa", z_or_p = "percentiles"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1) - 30, x = 95 + 301, sex = "F", acronym = "ssfa", z_or_p = "percentiles"))
  # Triceps skinfold for age
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1) - 31, x = 90 + 403, sex = "M", acronym = "tsfa", z_or_p = "percentiles"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1) - 31, x = 95 + 403, sex = "F", acronym = "tsfa", z_or_p = "percentiles"))
  # NA should arise in final vector, will be reflected in this function
  expect_true(testthat_v2x(y = 26.3, x = 50:65, sex = "F", acronym = "lhfa", z_or_p = "percentiles"))
})

