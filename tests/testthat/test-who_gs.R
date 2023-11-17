test_zscore_tbls <- function(sex, x_lower, x_upper, acronym, tolerance) {
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
                      round2(fn(z = x, range_x, sex = sex), digits = 3)
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
  in_or_out <- round2(unname(unlist(gigs::who_gs[[acronym]][[sex_]]$zscores[, 1],
                                    use.names = FALSE)), digits = 1) %in% round2(range_x, digits =  1)
  ref_tbl <- gigs::who_gs[[acronym]][[sex_]]$zscores[in_or_out, ] |> as.data.frame()
  rownames(pkg_tbl) <- NULL
  rownames(ref_tbl) <- NULL
  expect_equal(object = pkg_tbl, expected = ref_tbl, tolerance = tolerance)
}

test_that("Conversion of z-scores to WHO values works", {
  acronyms <- names(gigs::who_gs)
  sex <- c("M", "F")
  times <- rep(length(sex), length(acronyms))
  sexes <- rep(sex, length(acronyms))
  lower <-  rep(c(0, 0, 0, 45, 65, 0, 91, 91, 91), times =  times)
  upper <- rep(c(rep(1856, 3), 110, 120, rep(1856, 4)), times = times)
  acronyms <- rep(acronyms, times =  rep(length(sex), length(acronyms)))
  tolerance <- 1e-3
  invisible(mapply(FUN = test_zscore_tbls, sex, lower, upper, acronyms, tolerance))
})

test_that(desc = "Inputs with only incorrect acronyms return NA values", {
  expect_equal(object = who_gs_zscore2value(z = -3:3,
                                            x = 50,
                                            sex = rep_len(c("M", "F"), 7),
                                            acronym = "wrong"),
               expected = rep(NA, length(-3:3)))
  expect_equal(object = who_gs_zscore2value(z = 0, x = 50, sex = "M",
                                            acronym = "also_wrong"),
               expected = rep(NA, length(x = 0)))
})

test_centile_tbls <- function(sex, x_lower, x_upper, acronym, tolerance) {
  range_x <- seq(x_lower, x_upper, by = ifelse(acronym == "wfl" | acronym == "wfh", yes = 0.1, no = 1))
  tbl_names <- c("P01", "P1", "P3", "P5", "P10", "P15", "P25", "P50", "P75", "P85", "P90", "P95", "P97", "P99", "P999")
  pkg_tbl <- lapply(X = c(0.001, 0.01, 0.03, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.97, 0.99, 0.999),
                    FUN = function (x) {
                      fn <- switch(acronym,
                                   "wfa" =  who_gs_wfa_centile2value,
                                   "bfa" =  who_gs_bfa_centile2value,
                                   "lhfa" = who_gs_lhfa_centile2value,
                                   "wfl" =  who_gs_wfl_centile2value,
                                   "wfh" =  who_gs_wfh_centile2value,
                                   "hcfa" = who_gs_hcfa_centile2value,
                                   "acfa" = who_gs_acfa_centile2value,
                                   "ssfa" = who_gs_ssfa_centile2value,
                                   "tsfa" = who_gs_tsfa_centile2value)
                      round2(fn(p = x, range_x, sex = sex), digits = 3)
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
  in_or_out <- round2(unname(unlist(gigs::who_gs[[acronym]][[sex_]]$centiles[, 1],
                                   use.names = FALSE)), digits = 1) %in% round2(range_x, 1)
  ref_tbl <- gigs::who_gs[[acronym]][[sex_]]$centiles[in_or_out, ] |> as.data.frame()
  rownames(pkg_tbl) <- NULL
  rownames(ref_tbl) <- NULL
  list(package = pkg_tbl, reference = ref_tbl)

  # For now - remove P01 and P999 as these were not adjusted correctly in
  # WHO standards
  pkg_tbl <- pkg_tbl[, c(1, 3:15)]
  ref_tbl <- ref_tbl[, c(1, 3:15)]
  expect_equal(object = pkg_tbl, expected = ref_tbl, tolerance = tolerance)
}

test_that("Conversion of centiles to values works", {
  acronyms <- names(gigs::who_gs)
  sex <- c("M", "F")
  times <- rep(length(sex), length(acronyms))
  sexes <- rep(sex, length(acronyms))
  lower <-  rep(c(0, 0, 0, 45, 65, 0, 91, 91, 91), times =  times)
  upper <- rep(c(rep(1856, 3), 110, 120, rep(1856, 4)), times = times)
  acronyms <- rep(acronyms, times =  rep(length(sex), length(acronyms)))
  tolerance <- 1e-3
  invisible(mapply(FUN = test_centile_tbls,
                   sex, lower, upper, acronyms, tolerance))
})

testthat_v2x <- function(y, x, sex, acronym, z_or_p = "zscores") {
  out_z_or_p <- switch(z_or_p,
         "zscores" = switch(acronym,
                            "wfa" = who_gs_wfa_value2zscore(y, x, sex),
                            "bfa" = who_gs_bfa_value2zscore(y, x, sex),
                            "lhfa" = who_gs_lhfa_value2zscore(y, x, sex),
                            "wfl" = who_gs_wfl_value2zscore(y, x, sex),
                            "wfh" = who_gs_wfh_value2zscore(y, x, sex),
                            "hcfa" = who_gs_hcfa_value2zscore(y, x, sex),
                            "acfa" = who_gs_acfa_value2zscore(y, x, sex),
                            "ssfa" = who_gs_ssfa_value2zscore(y, x, sex),
                            "tsfa" = who_gs_tsfa_value2zscore(y, x, sex)
         ),
         "centiles" = switch(acronym,
                                "wfa" = who_gs_wfa_value2centile(y, x, sex),
                                "bfa" = who_gs_bfa_value2centile(y, x, sex),
                                "lhfa" = who_gs_lhfa_value2centile(y, x, sex),
                                "wfl" = who_gs_wfl_value2centile(y, x, sex),
                                "wfh" = who_gs_wfh_value2centile(y, x, sex),
                                "hcfa" = who_gs_hcfa_value2centile(y, x, sex),
                                "acfa" = who_gs_acfa_value2centile(y, x, sex),
                                "ssfa" = who_gs_ssfa_value2centile(y, x, sex),
                                "tsfa" = who_gs_tsfa_value2centile(y, x, sex)))
  out_value <- switch(z_or_p,
         "zscores" = switch(acronym,
                            "wfa"  = who_gs_wfa_zscore2value(z = out_z_or_p, age_days = x, sex = sex),
                            "bfa"  = who_gs_bfa_zscore2value(z = out_z_or_p, age_days = x, sex = sex),
                            "lhfa" = who_gs_lhfa_zscore2value(z = out_z_or_p, age_days = x, sex = sex),
                            "wfl" =  who_gs_wfl_zscore2value(z = out_z_or_p, length_cm = x, sex = sex),
                            "wfh" =  who_gs_wfh_zscore2value(z = out_z_or_p, height_cm = x, sex = sex),
                            "hcfa" = who_gs_hcfa_zscore2value(z = out_z_or_p, age_days = x, sex = sex),
                            "acfa" = who_gs_acfa_zscore2value(z = out_z_or_p, age_days = x, sex = sex),
                            "ssfa" = who_gs_ssfa_zscore2value(z = out_z_or_p, age_days = x, sex = sex),
                            "tsfa" = who_gs_tsfa_zscore2value(z = out_z_or_p, age_days = x, sex = sex)),
         "centiles" = switch(acronym,
                            "wfa"  = who_gs_wfa_centile2value(p = out_z_or_p, age_days = x, sex = sex),
                            "bfa"  = who_gs_bfa_centile2value(p = out_z_or_p, age_days = x, sex = sex),
                            "lhfa" = who_gs_lhfa_centile2value(p = out_z_or_p, age_days = x, sex = sex),
                            "wfl" =  who_gs_wfl_centile2value(p = out_z_or_p, length_cm = x, sex = sex),
                            "wfh" =  who_gs_wfh_centile2value(p = out_z_or_p, height_cm = x, sex = sex),
                            "hcfa" = who_gs_hcfa_centile2value(p = out_z_or_p, age_days = x, sex = sex),
                            "acfa" = who_gs_acfa_centile2value(p = out_z_or_p, age_days = x, sex = sex),
                            "ssfa" = who_gs_ssfa_centile2value(p = out_z_or_p, age_days = x, sex = sex),
                            "tsfa" = who_gs_tsfa_centile2value(p = out_z_or_p, age_days = x, sex = sex)))
  expect_true(all(round2(y, digits = 3) == round2(out_value, digits = 3), na.rm = TRUE))
}

test_that("Conversion of values to z-scores works", {
  # Weight for age
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 36, sex = "M", acronym = "wfa")
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 40, sex = "F", acronym = "wfa")
  # BMI for age
  testthat_v2x(y = c(41.9, 43.8, 45.6, 47.3, 49.1) - 28, x = 57, sex = "M", acronym = "bfa")
  testthat_v2x(y = c(46.7, 41.8, 43.5, 47.5, 48.1) - 28, x = 36, sex = "F", acronym = "bfa")
  # Length/height for age
  testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 48, sex = "M", acronym = "lhfa")
  testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 60, sex = "F", acronym = "lhfa")
  # Length/height for age
  testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 70, sex = "M", acronym = "wfl")
  testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 75, sex = "F", acronym = "wfl")
  # Length/height for age
  testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 90, sex = "M", acronym = "wfh")
  testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 95, sex = "F", acronym = "wfh")
  # Head circumference for age
  testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 90, sex = "M", acronym = "hcfa")
  testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 95, sex = "F", acronym = "hcfa")
  # Arm circumference for age
  testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 90 + 200, sex = "M", acronym = "acfa")
  testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 95 + 200, sex = "F", acronym = "acfa")
  # Subscapular skinfold for age
  testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 90 + 301, sex = "M", acronym = "ssfa")
  testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 95 + 301, sex = "F", acronym = "ssfa")
  # Triceps skinfold for age
  testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 90 + 403, sex = "M", acronym = "tsfa")
  testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 95 + 403, sex = "F", acronym = "tsfa")
  # NA should arise in final vector, will be reflected in this function
  testthat_v2x(y = 26.3, x = 50:65, sex = "F", acronym = "lhfa")
})

test_that("Conversion of values to centiles works", {
  # Weight for age
  expect_true(testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 36, sex = "M", acronym = "wfa", z_or_p = "centiles"))
  expect_true(testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 40, sex = "F", acronym = "wfa", z_or_p = "centiles"))
  # BMI for age
  expect_true(testthat_v2x(y = c(41.9, 43.8, 45.6, 47.3, 49.1) - 28, x = 57, sex = "M", acronym = "bfa", z_or_p = "centiles"))
  expect_true(testthat_v2x(y = c(46.7, 41.8, 43.5, 47.5, 48.1) - 29, x = 36, sex = "F", acronym = "bfa", z_or_p = "centiles"))
  # Length/height for age
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 48, sex = "M", acronym = "lhfa", z_or_p = "centiles"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 60, sex = "F", acronym = "lhfa", z_or_p = "centiles"))
  # Weight for length
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1) - 21, x = 92.8, sex = "M", acronym = "wfl", z_or_p = "centiles"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1) - 21, x = 92.8, sex = "F", acronym = "wfl", z_or_p = "centiles"))
  # Weight for height
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1) - 20, x = 90, sex = "M", acronym = "wfh", z_or_p = "centiles"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1) - 20, x = 95, sex = "F", acronym = "wfh", z_or_p = "centiles"))
  # Head circumference for age
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 90, sex = "M", acronym = "hcfa", z_or_p = "centiles"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 95, sex = "F", acronym = "hcfa", z_or_p = "centiles"))
  # Arm circumference for age
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1) - 20, x = 90 + 200, sex = "M", acronym = "acfa", z_or_p = "centiles"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1) - 20, x = 95 + 200, sex = "F", acronym = "acfa", z_or_p = "centiles"))
  # Subscapular skinfold for age
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1) - 30, x = 90 + 301, sex = "M", acronym = "ssfa", z_or_p = "centiles"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1) - 30, x = 95 + 301, sex = "F", acronym = "ssfa", z_or_p = "centiles"))
  # Triceps skinfold for age
  expect_true(testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1) - 31, x = 90 + 403, sex = "M", acronym = "tsfa", z_or_p = "centiles"))
  expect_true(testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1) - 31, x = 95 + 403, sex = "F", acronym = "tsfa", z_or_p = "centiles"))
  # NA should arise in final vector, will be reflected in this function
  expect_true(testthat_v2x(y = 26.3, x = 50:65, sex = "F", acronym = "lhfa", z_or_p = "centiles"))
})

test_that(desc = "Interpolation of LMS values can be performed",
          code = {
            testthat::expect_false(
              anyNA(
                who_gs_value2zscore(y = 20.3,
                                    # All xvars interpolated
                                    x = 900:920 + 0.5,
                sex = "M",
                acronym = "acfa")
              ))
            testthat::expect_false(
              anyNA(who_gs_value2zscore(y = 20.3,
                                        # Half interpolated, half not
                                        x = seq(900, 910, by = 0.5),
                                        sex = "M",
                                        acronym = "acfa")
              ))
          })

test_that(desc = "Interpolation of LMS values with multiple standards/sexes",
          code = {
            testthat::expect_false(
              anyNA(
                who_gs_value2centile(y = 20.3,
                                        x = c(900, 905.5),
                                        sex = c("M", "F"),
                                        acronym = c("acfa", "tsfa")
              )))
          })

test_that(desc = "NA values returned with out of range xvars",
          code = {
            testthat::expect_true(
              anyNA(
                who_gs_zscore2value(z = rep_len(-3:3, 200),
                                    x = seq(0, 99.5, by = 0.5),
                                    sex = rep_len(c("M", "F"), 200),
                                    acronym = rep_len(names(gigs::who_gs), 200))
              ))
          })

test_that(desc = "Bad input types cause errors.",
          code = {
            x <- seq(65, 95, by = 0.5)
            len_x <- length(x)
            z <- rep_len(-3:3, len_x)
            sex <- rep_len(c("M", "F"), len_x)
            acronym <- rep_len(names(gigs::who_gs), len_x)
            # Test failures for each arg when converting zscores to values
            testthat::expect_error(
              who_gs_zscore2value(as.character(z), x, sex, acronym)
            )
            testthat::expect_error(
              who_gs_zscore2value(z, as.character(x), sex, acronym)
            )
            testthat::expect_error(who_gs_zscore2value(z, x, 1, acronym))
            testthat::expect_error(who_gs_zscore2value(z, x, sex, 1))

            # And for conversion of values to zscores
            y <- who_gs_zscore2value(z, x, sex, acronym)
            testthat::expect_error(
              who_gs_value2zscore(as.character(y), x, sex, acronym)
            )
            testthat::expect_error(
              who_gs_value2zscore(y, as.character(x), sex, acronym)
            )
            testthat::expect_error(who_gs_value2zscore(y, x, 1, acronym))
            testthat::expect_error(who_gs_value2zscore(y, x, sex, 1))
})
