png_xrange <- function(x_lower, x_upper, acronym) {
    x <- gigs::ig_png[[acronym]]$male$zscores[, 1]
    x[inrange(x, c(x_lower, x_upper))]
}

test_zscore_tbls <- function(sex, x_lower, x_upper, acronym, tolerance) {
  range_x <- png_xrange(x_lower, x_upper, acronym)
  roundto <- if (acronym %in% c("wfa", "wfl")) 2 else 1
  sex_ <- if (sex == "M") "male" else "female"
  ref_tbl <- gigs::ig_png[[acronym]][[sex_]]$zscores
  pkg_tbl <- lapply(X = -3:3,
                    FUN = \(z) {
                      fn <- get(paste0("ig_png_", acronym, "_zscore2value"))
                      round(fn(z, range_x, sex), digits = roundto)
                    }) |>
    do.call(what = cbind) |>
    as.data.frame() |>
    setNames(names(ref_tbl)[-1])
  col1_name <- names(ref_tbl)[1]
  pkg_tbl[[col1_name]] <- range_x
  pkg_tbl <- pkg_tbl[, c(ncol(pkg_tbl), 1:(ncol(pkg_tbl) - 1))]
  ref_tbl <- ref_tbl[inrange(ref_tbl[,1], range_x), ]
  expect_equal(object = pkg_tbl, expected = ref_tbl, tolerance = tolerance)
}

test_centile_tbls <- function(sex, x_lower, x_upper, acronym, tolerance) {
  range_x <- png_xrange(x_lower, x_upper, acronym)
  roundto <- if (acronym %in% c("wfa", "wfl")) 2 else 1
  sex_ <- if (sex == "M") "male" else "female"
  ref_tbl <- gigs::ig_png[[acronym]][[sex_]]$centiles

  tbl_names <- c("P03", "P05", "P10", "P50", "P90", "P95", "P97")
  pkg_tbl <- lapply(X = c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97),
                    FUN = \(p) {
                      fn <- get(paste0("ig_png_", acronym, "_centile2value"))
                      round(fn(p, range_x, sex), digits = roundto)
                }) |>
    do.call(what = cbind) |>
    as.data.frame() |>
    setNames(tbl_names)
  col1_name <- if (acronym != "wfl") "pma_weeks" else "length_cm"
  pkg_tbl[[col1_name]] <- range_x
  pkg_tbl <- pkg_tbl[, c(ncol(pkg_tbl), 1:(ncol(pkg_tbl) - 1))]
  ref_tbl <- ref_tbl[inrange(ref_tbl[,1], range_x), ]
  expect_equal(object = pkg_tbl, expected = ref_tbl, tolerance = tolerance)
}

png_names <- names(gigs::ig_png)
sex <- rep(c("M", "F"), length(png_names))
lower <-  rep.int(c(27, 35), c(6, 2))
upper <- rep.int(c(64, 65), c(6, 2))
acronyms <- rep(png_names, times = rep(2, length(png_names)))
tolerance <- 0.01

test_that("Conversion of z-scores to values works", {
  mapply(FUN = test_zscore_tbls, sex, lower, upper, acronyms, tolerance)
})

test_that("Conversion of centiles to values works", {
  mapply(FUN = test_centile_tbls, sex, lower, upper, acronyms, tolerance)
})

testthat_v2x <- function(y, x, sex, acronym, z_or_p = "zscore") {
  fn_stem <- paste0("ig_png_", acronym)
  fn_val2zp <- get(paste0(fn_stem, "_value2", z_or_p))
  out_z_or_p <- fn_val2zp(y, x, sex)

  fn_zp2val <- get(paste0(fn_stem, "_", z_or_p, "2value"))
  out_value <- fn_zp2val(out_z_or_p, x, sex)

  if (all(is.na(out_z_or_p)) | all(is.na(out_z_or_p))) {
    stop("All values were NA.")
  }
  expect_equal(y, out_value)
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
})

test_that("Conversion of values to centiles works", {
  cent <- "centile"
  # Weight for age
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 36, sex = "M", acronym = "wfa", z_or_p = cent)
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 40, sex = "F", acronym = "wfa", z_or_p = cent)
  # Head circumference for age
  testthat_v2x(y = c(41.9, 43.8, 45.6, 47.3, 49.1), x = 57, sex = "M", acronym = "lfa", z_or_p = cent)
  testthat_v2x(y = c(46.7, 41.8, 43.5, 47.5, 48.1), x = 36, sex = "F", acronym = "lfa", z_or_p = cent)
  # Length for age
  testthat_v2x(y = c(32.6, 33.0, 34.3, 35.7, 36.1), x = 48, sex = "M", acronym = "hcfa", z_or_p = cent)
  testthat_v2x(y = c(29.1, 31.0, 26.3, 29.7, 33.1), x = 60, sex = "F", acronym = "hcfa", z_or_p = cent)
  # Weight for length
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 48, sex = "M", acronym = "wfl", z_or_p = cent)
  testthat_v2x(y = c(2.65, 3.00, 2.86, 3.10, 3.32), x = 60, sex = "F", acronym = "wfl", z_or_p = cent)
})

test_that(
  desc = "Bad input types cause errors.",
  code = {
    x <- 30:35
    x_len <- length(x)
    z <- rep_len(-3:3, x_len)
    sex <- rep_len(c("M", "F"), x_len)
    acronym <- rep_len(names(gigs::ig_png), x_len)

    error_msg <- function(name, wanted, got) {
      paste0("Assertion on '", name, "' failed: Must be of type '", wanted,
             "', not '", got, "'.")
    }

    # Test failures for each arg when converting zscores to values
    testthat::expect_error(
      ig_png_zscore2value(as.character(z), x, sex, acronym),
      regexp = error_msg(name = "z", wanted = "numeric", got = "character")
    )
    testthat::expect_error(
      ig_png_zscore2value(z, as.character(x), sex, acronym),
      regexp = error_msg(name = "x", wanted = "numeric", got = "character")
    )
    testthat::expect_error(
      ig_png_zscore2value(z, x, 1, acronym),
      regexp = error_msg(name = "sex", wanted = "character", got = "double")
    )
    testthat::expect_error(
      ig_png_zscore2value(z, x, sex, 1),
      regexp = error_msg(name = "acronym", wanted = "character", got = "double")
    )

    # And for conversion of values to zscores
    y <- ig_png_zscore2value(z, x, sex, acronym)
    testthat::expect_error(
      ig_png_value2zscore(as.character(y), x, sex, acronym),
      regexp = error_msg(name = "y", wanted = "numeric", got = "character")
    )
    testthat::expect_error(
      ig_png_value2zscore(y, as.character(x), sex, acronym),
      regexp = error_msg(name = "x", wanted = "numeric", got = "character")
    )
    testthat::expect_error(
      ig_png_value2zscore(y, x, 1, acronym),
      regexp = error_msg(name = "sex", wanted = "character", got = "double")
    )
    testthat::expect_error(
      ig_png_value2zscore(y, x, sex, 1),
      regexp = error_msg(name = "acronym", wanted = "character", got = "double")
    )

    error_msg_bad_value <- function(name) {
      paste0("No value in `", name, "` was valid.")
    }
    # All bad sex values cause function to error
    expect_error(
      object = ig_png_zscore2value(z = 0,
                                   x = 50,
                                   sex = "wrong_sex",
                                   acronym = "wfa"),
      regexp = error_msg_bad_value(name = "sex")
    )

    # All bad acronyms cause function to error
    expect_error(
      object = ig_png_zscore2value(z = 0,
                                   x = 50,
                                   sex = "M",
                                   acronym = "wrong_acronym"),
      regexp = error_msg_bad_value(name = "acronym")
    )
})