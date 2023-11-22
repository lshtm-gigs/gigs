get_gest_days <- function(lower, upper) {
  ga <- gigs::ig_nbs$fmfga$male$centiles$gest_days
  ga[inrange(ga, c(lower, upper))]
}

test_centile_tbls <- function(sex, age_lower, age_upper, acronym, tolerance) {
  gest_day_range <- get_gest_days(age_lower, age_upper)
  roundto <- if (acronym %in% "bfpfga") 1 else 0
  sex_ <- if (sex == "M") "male" else "female"
  ref_tbl <- gigs::ig_nbs[[acronym]][[sex_]]$centiles
  pkg_tbl <- lapply(X = c(0.03, 0.1, 0.5, 0.9, 0.97),
                    FUN = \(p) {
                      fn <- get(paste0("ig_nbs_", acronym, "_centile2value"))
                      round(fn(p, gest_day_range, sex), digits = roundto)
                    }) |>
    do.call(what = cbind) |>
    as.data.frame() |>
    setNames(names(ref_tbl)[-1])
  col1_name <- names(ref_tbl)[1]
  pkg_tbl[[col1_name]] <- gest_day_range
  pkg_tbl <- pkg_tbl[, c(ncol(pkg_tbl), 1:(ncol(pkg_tbl) - 1))]
  ref_tbl <- ref_tbl[inrange(ref_tbl$gest_days, c(age_lower, age_upper)), ]
  expect_equal(object = pkg_tbl, expected = ref_tbl, tolerance = tolerance)
}

test_zscore_tbls <- function(sex, age_lower, age_upper, acronym, tolerance) {
  gest_day_range <- get_gest_days(age_lower, age_upper)
  roundto <- if (acronym %in% "bfpfga") 1 else 0
  sex_ <- if (sex == "M") "male" else "female"
  ref_tbl <- gigs::ig_nbs[[acronym]][[sex_]]$centiles
  pkg_tbl <- lapply(X = qnorm(c(0.03, 0.1, 0.5, 0.9, 0.97)),
                    FUN = \(z) {
                      fn <- get(paste0("ig_nbs_", acronym, "_zscore2value"))
                      round(fn(z, gest_day_range, sex), digits = roundto)
                    }) |>
    do.call(what = cbind) |>
    as.data.frame() |>
    setNames(names(ref_tbl)[-1])
  col1_name <- names(ref_tbl)[1]
  pkg_tbl[[col1_name]] <- gest_day_range
  pkg_tbl <- pkg_tbl[, c(ncol(pkg_tbl), 1:(ncol(pkg_tbl) - 1))]
  ref_tbl <- ref_tbl[inrange(ref_tbl$gest_days, c(age_lower, age_upper)), ]
  expect_equal(object = pkg_tbl, expected = ref_tbl, tolerance = tolerance)
}

sex <- rep(c("M", "F"), 3)
lower <- rep(266, length(sex))
upper <- rep(294, length(sex))
acronyms <- rep(names(gigs::ig_nbs)[5:7], times = rep(2, 3))
# 23/08/2023: With old linear equations used to be c(26, 15, 0.5, 0.5, 7, 5)
tolerances <- c(6, 1, 0.11, 0.11, 2, 0)

test_that(desc = "Conversion of centiles to values works", {
  mapply(FUN = test_centile_tbls, sex, lower, upper, acronyms, tolerances)
})

test_that("Conversion of z-scores to values works", {
  mapply(FUN = test_zscore_tbls, sex, lower, upper, acronyms, tolerances)
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
  expect_equal(round(out_value, digits = 2), expected = round(y, digits = 2))
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

test_that(desc = "Bad inputs return as NAs", code = {
  # Test that bad input gives NA
  with(
    data.frame(sex = c("M", "F", "U", "X", "M"),
         centiles = c(0.25, -1, 0.13, 0.84, 0.10),
         age = c(37, 38, 39, 40, 41)),
    expr = {
      vapply(
        X = c("fmfga", "bfpfga", "ffmfga"),
        FUN = function(x) {
          fn <- get(paste0("ig_nbs_", x, "_centile2value"))
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