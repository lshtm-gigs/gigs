get_gest_ages <- function(lower, upper) {
    ga <- gigs::ig_nbs$wfga$male$zscores$gest_days
    ga[inrange(ga, c(lower, upper))]
}

test_zscore_tbls <- function(sex, age_lower, age_upper, acronym, tolerance) {
  gest_day_range <- get_gest_ages(age_lower, age_upper)
  roundto <- ifelse(acronym %in% c("wfga", "wlrfga"), yes = 2, no = 1)
  tbl_names <- c("SD3neg", "SD2neg", "SD1neg", "SD0", "SD1", "SD2", "SD3")
  pkg_tbl <- lapply(X = -3:3,
                    FUN = function (x) {
                      fn <- get(paste0("ig_nbs_", acronym, "_zscore2value"))
                      round(fn(z = x, gest_days = gest_day_range, sex = sex),
                            digits = roundto)
                }) |>
    do.call(what = cbind) |>
    as.data.frame()
  names(pkg_tbl) <- tbl_names
  pkg_tbl$gest_days <- gest_day_range
  pkg_tbl <- pkg_tbl[, c(ncol(pkg_tbl), 1:(ncol(pkg_tbl) - 1))]
  sex_ <- ifelse(sex == "M", yes = "male", no = "female" )
  ref_tbl <- gigs::ig_nbs[[acronym]][[sex_]]$zscores
  ref_tbl <- ref_tbl[inrange(ref_tbl$gest_days, c(age_lower, age_upper)), ]
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

test_centile_tbls <- function(sex, age_lower, age_upper, acronym, tolerance) {
  roundto <- ifelse(acronym == "wfga", yes = 2, no = 1)
  tbl_names <- c("P03", "P05", "P10", "P50", "P90", "P95", "P97")
  gest_day_range <- get_gest_ages(age_lower, age_upper)
  pkg_tbl <- lapply(X = c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97),
                    FUN = function (x) {
                      fn <- get(paste0("ig_nbs_", acronym, "_centile2value"))
                      round(fn(p = x, gest_days = gest_day_range, sex = sex),
                             digits = roundto)
                }) |>
    do.call(what = cbind) |>
    as.data.frame()
  names(pkg_tbl) <- tbl_names
  pkg_tbl$gest_days <- gest_day_range
  pkg_tbl <- pkg_tbl[, c(ncol(pkg_tbl), 1:(ncol(pkg_tbl) - 1))]
  sex_ <- ifelse(sex == "M", yes = "male", no = "female" )
  ref_tbl <- gigs::ig_nbs[[acronym]][[sex_]]$centiles
  ref_tbl <- ref_tbl[ref_tbl$gest_days >= age_lower & ref_tbl$gest_days <= age_upper, ]
  rownames(pkg_tbl) <- NULL
  rownames(ref_tbl) <- NULL
  list(package = pkg_tbl, reference = ref_tbl)
  expect_equal(object = pkg_tbl, expected = ref_tbl, tolerance = tolerance)
}

test_that("Conversion of centiles to values works", {
  sex <- rep(c("M", "F"), length(names(gigs::ig_nbs)) - 3)
  lower <- rep(168, length(sex))
  upper <- rep(300, length(sex))
  acronyms <- rep(names(gigs::ig_nbs)[1:4], times = rep(2, length(names(gigs::ig_nbs)) - 3))
  tolerance <- 0.01
  mapply(FUN = test_centile_tbls, sex, lower, upper, acronyms, tolerance)
})

testthat_v2x <- function(y, gest_days, sex, acronym, z_or_p = "zscore") {
  fn_stem <- paste0("ig_nbs_", acronym)
  fn_val2zp <- get(paste0(fn_stem, "_value2", z_or_p))
  out_z_or_p <- fn_val2zp(y, gest_days, sex)

  fn_zp2val <- get(paste0(fn_stem, "_", z_or_p, "2value"))
  out_value <- fn_zp2val(out_z_or_p, gest_days, sex)

  if (all(is.na(out_z_or_p)) | all(is.na(out_z_or_p))) {
    stop("All values were NA.")
  }
  expect_equal(round2(out_value, digits = 2), expected = round2(y, digits = 2))
}

test_that("Conversion of values to z-scores works", {
  # Weight for gestational age
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_days = 36 * 7, sex = "M", acronym = "wfga")
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_days = 40 * 7, sex = "F", acronym = "wfga")

  # Length for gestational age
  testthat_v2x(y = c(41.9, 43.8, 45.6, 47.3, 49.1), gest_days = 7 * 34, sex = "M", acronym = "lfga")
  testthat_v2x(y = c(46.7, 41.8, 43.5, 47.5, 48.1), gest_days = 7 * (41 + 3/7), sex = "F", acronym = "lfga")

  # Head circumference for gestational age
  testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), gest_days = 7 * (42 + 2/7), sex = "M", acronym = "hcfga")
  testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), gest_days = 7 * (37 + 4/7), sex = "F", acronym = "hcfga")

  # Weight-length ratio for gestational age
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_days = 7 * (42 + 2/7), sex = "M", acronym = "wlrfga")
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_days = 7 * (37 + 4/7), sex = "F", acronym = "wlrfga")
})

test_that("Conversion of values to centiles works", {
  # Weight for gestational age
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_days = 7 * 36, sex = "M", acronym = "wfga", z_or_p = "centile")
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_days = 7 * 40, sex = "F", acronym = "wfga", z_or_p = "centile")

  # Length circumference for gestational age
  testthat_v2x(y = c(41.9, 43.8, 45.6, 47.3, 49.1), gest_days = 7 * 36, sex = "M", acronym = "lfga", z_or_p = "centile")
  testthat_v2x(y = c(46.7, 41.8, 43.5, 47.5, 48.1), gest_days = 7 * 40, sex = "F", acronym = "lfga", z_or_p = "centile")

  # Head circumference for gestational age
  testthat_v2x(y = c(41.9, 43.8, 45.6, 47.3, 49.1), gest_days = 7 * 36, sex = "M", acronym = "hcfga", z_or_p = "centile")
  testthat_v2x(y = c(46.7, 41.8, 43.5, 47.5, 48.1), gest_days = 7 * 40, sex = "F", acronym = "hcfga", z_or_p = "centile")

  # Weight-length ratio for gestational age
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_days = 7 * 36, sex = "M", acronym = "wlrfga", z_or_p = "centile")
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), gest_days = 7 * 40, sex = "F", acronym = "wlrfga", z_or_p = "centile")
})

test_that(desc = "Bad input gives NA", code = {
  with(
    data.frame(sex = c("M", "F", "U", "X", "M"),
         centiles = c(0.25, -1, 0.13, 0.84, 0.10),
         age = c(32, 33, 34, 35, 36)),
    expr = {
      vapply(
        X = c("wfga", "lfga", "hcfga", "wlrfga"),
        FUN = function(x) {
          fn <- get(paste0("ig_nbs_", x, "_centile2value"))
          vals <- suppressWarnings(fn(sex = sex, gest_days = age, p = centiles))
          expect_length(object = vals, n = length(sex))
          expect_true(all(is.na(c(
            vals[2], # Because centile is out of bounds
            vals[4]  # Because sex is not one of "M", "F" or "U"
          ))))
        },
      FUN.VALUE = logical(length = 1L))
    })

  # Test that bad input gives NA in VPNS
  with(
    list(sex = c("M", "F", "U", "X", "M"),
         zscores = c(0.5, -1, 0.5, 0.5, 0.5),
         age = c(161, 168, 175, 182, 210)),
    expr = {
      vapply(
        X = c("wfga", "lfga", "hcfga"),
        FUN = function(x) {
          fn <- get(paste0("ig_nbs_", x, "_zscore2value"))
          vals <- suppressWarnings(fn(sex = sex, gest_days = age, z = zscores))
          expect_length(object = vals, n = length(sex))
          expect_true(all(is.na(c(
            vals[1], # Because age is out of bounds
            vals[4]  # Because sex is not one of "M", "F" or "U"
          ))))
        },
        FUN.VALUE = logical(length = 1L))
    })
})

test_that(desc = "Interpolation of MSNT values can be performed",
          code = {
            testthat::expect_false(
              anyNA(ig_nbs_value2zscore(y = 48.5,
                                        # All gest_day interpolated
                                        gest_days = 260:268 + 0.5,
                                        sex = "M",
                                        acronym = "lfga")))
          })

test_that(desc = "Coefficient interpolation possible on ints and doubles",
          code = {
            testthat::expect_false(
              anyNA(ig_nbs_value2zscore(y = 3.4,
                                        # Half interpolated, half not
                                        gest_days = seq(260, 264, by = 0.5),
                                        sex = "M",
                                        acronym = "wfga")
              ))
          })

test_that(desc = "Interpolation of MSNT values with multiple standards/sexes",
          code = {
            testthat::expect_false(
              anyNA(ig_nbs_value2zscore(y = 3,
                                        gest_days = seq(250, 245.5,
                                                        length.out = 10),
                                        sex = rep(c("M", "F"), 5),
                                        acronym = rep(c("wfga", "lfga"), 5))))
          })

test_that(desc = "NA values returned with out of range gestage",
          code = {
            ga_range <- 130:310
            out_len <- length(ga_range)
            full_nbs_range <- 168:300
            testthat::expect_equal(
              sum(
                !is.na(
                  ig_nbs_centile2value(
                    p = 0.5,
                    gest_days = ga_range,
                    sex = rep_len(c("M", "F"), out_len),
                    acronym = rep_len(names(gigs::ig_nbs)[1:4], out_len))
                )
              ),
              length(full_nbs_range)
            )
            bodycomp_range <- 266:294
            testthat::expect_equal(
              sum(
                !is.na(
                  ig_nbs_centile2value(
                    p = 0.5,
                    gest_days = ga_range,
                    sex = rep_len(c("M", "F"), out_len),
                    acronym = rep_len(names(gigs::ig_nbs[5:7]), out_len))
                )
              ),
              length(bodycomp_range)
            )
          })

test_that(desc = "Coefficient retrieval works when not all acronyms are valid",
          code = {
            testthat::expect_true(
              anyNA(
                ig_nbs_value2centile(
                  y = rnorm(n = 100, mean =  2.3, sd = 0.25),
                  gest_days = rep_len(232:300, length.out = 100),
                  sex = rep_len(c("M", "F"), length.out = 100),
                  acronym = rep_len(c(names(gigs::ig_nbs_coeffs)[1:2], "xfga"),
                                    length.out = 100)
                )))
            testthat::expect_true(
              anyNA(
                ig_nbs_centile2value(
                  p = rnorm(n = 100, mean = 0.5, sd = 0.05),
                  gest_days = rep_len(232:300, length.out = 100),
                  sex = rep_len(c("M", "F"), length.out = 100),
                  acronym = rep_len(c(names(gigs::ig_nbs_coeffs)[1:2], "xfga"),
                                    length.out = 100)
                )))
          }
)

test_that(desc = "Bad input types cause errors.",
          code = {
            x <- 250:260
            x_len <- length(x)
            z <- rep_len(-3:3, x_len)
            sex <- rep_len(c("M", "F"), x_len)
            acronym <- rep_len(names(gigs::ig_nbs[1:4]), x_len)
            # Test failures for each arg when converting zscores to values
            testthat::expect_error(
              ig_nbs_zscore2value(as.character(z), x, sex, acronym)
            )
            testthat::expect_error(
              ig_nbs_zscore2value(z, as.character(x), sex, acronym)
            )
            testthat::expect_error(ig_nbs_zscore2value(z, x, 1, acronym))
            testthat::expect_error(ig_nbs_zscore2value(z, x, sex, 1))

            # And for conversion of values to zscores
            y <- ig_nbs_zscore2value(z, x, sex, acronym)
            testthat::expect_error(
              ig_nbs_value2zscore(as.character(y), x, sex, acronym)
            )
            testthat::expect_error(
              ig_nbs_value2zscore(y, as.character(x), sex, acronym)
            )
            testthat::expect_error(ig_nbs_value2zscore(y, x, 1, acronym))
            testthat::expect_error(ig_nbs_value2zscore(y, x, sex, 1))
})