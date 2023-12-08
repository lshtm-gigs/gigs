fet_xrange <- function(x_lower, x_upper, acronym) {
  x <- gigs::ig_fet[[acronym]]$zscores[, 1]
  x[inrange(x, c(x_lower, x_upper))]
}

test_zscore_tbls <- function(x_lower, x_upper, acronym, tolerance) {
  range_x <- fet_xrange(x_lower, x_upper, acronym)
  # roundto <- if (acronym == "efwfga") 0 else 1
  ref_tbl <- gigs::ig_fet[[acronym]]$zscores
  pkg_tbl <- lapply(X = -3:3,
                    FUN = \(z) {
                      fn <- get(paste0("ig_fet_", acronym, "_zscore2value"))
                      fn(z, range_x)
                      # round(fn(z, range_x), digits = roundto)
                    }) |>
    do.call(what = cbind) |>
    as.data.frame() |>
    setNames(names(ref_tbl)[-1])
  col1_name <- names(ref_tbl)[1]
  pkg_tbl[[col1_name]] <- range_x
  pkg_tbl <- pkg_tbl[, c(ncol(pkg_tbl), 1:(ncol(pkg_tbl) - 1))]
  ref_tbl <- ref_tbl[inrange(ref_tbl[,1], range_x), ]
  expect_equal(object = pkg_tbl, expected = ref_tbl, tolerance = tolerance,
               ignore_attr = TRUE)
}

test_centile_tbls <- function(x_lower, x_upper, acronym, tolerance) {
  range_x <- fet_xrange(x_lower, x_upper, acronym)
  roundto <- if (acronym == "efwfga") 0 else 1
  ref_tbl <- gigs::ig_fet[[acronym]]$centiles
  pkg_tbl <- lapply(X = c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97),
                    FUN = function (p) {
                      fn <- get(paste0("ig_fet_", acronym, "_centile2value"))
                      fn(p, range_x)
                }) |>
    do.call(what = cbind) |>
    as.data.frame() |>
    setNames(names(ref_tbl)[-1])
  col1_name <- names(ref_tbl)[1]
  pkg_tbl[[col1_name]] <- range_x
  pkg_tbl <- pkg_tbl[, c(ncol(pkg_tbl), 1:(ncol(pkg_tbl) - 1))]
  ref_tbl <- ref_tbl[inrange(ref_tbl[,1], range_x), ]
  expect_equal(object = pkg_tbl, expected = ref_tbl, tolerance = tolerance,
               ignore_attr = TRUE)
}

igfet_names <- names(gigs::ig_fet)
lower <- c(rep(98, length(igfet_names) - 1), 154)
upper <- rep(280, length(igfet_names))
acronyms <- rep(igfet_names, times = rep(2, length(igfet_names)))
tolerance <- 0.01

test_that(desc = "Conversion of z-scores to values works", {
  mapply(FUN = test_zscore_tbls, lower, upper, acronyms, tolerance)
})

test_that("Conversion of centiles to values works", {
  mapply(FUN = test_centile_tbls, lower, upper, acronyms, tolerance)
})

testthat_v2x <- function(y, gest_days, acronym, z_or_p = "zscore") {
  fn_stem <- paste0("ig_fet_", acronym)
  fn_val2zp <- get(paste0(fn_stem, "_value2", z_or_p))
  out_z_or_p <- fn_val2zp(y, gest_days)

  fn_zp2val <- get(paste0(fn_stem, "_", z_or_p, "2value"))
  out_value <- fn_zp2val(out_z_or_p, gest_days)

  if (all(is.na(out_z_or_p)) | all(is.na(out_z_or_p))) {
    stop("All values were NA.")
  }
  expect_equal(round(out_value, digits = 2), expected = round(y, digits = 2))
}

test_that(desc = "Conversion of values to z-scores works", {
  # Head circumference for gestational age
  testthat_v2x(y = 320:350, gest_days = 36 * 7, acronym = "hcfga")

  # Biparietal diameter for gestational age
  testthat_v2x(y = 90:105, gest_days = 7 * 34, acronym = "bpdfga")

  # Abdominal circumference for gestational age
  testthat_v2x(y = 225:235, gest_days = 7 * (28 + 2/7), acronym = "acfga")

  # Femur length for gestational age
  testthat_v2x(y = seq(50, 55, 0.5), gest_days = 7 * (28 + 2/7), acronym = "flfga")

  # Occipitofrontal diameter for gestational age
  testthat_v2x(y = seq(90, 100, 0.5), gest_days = 7 * (28 + 2/7), acronym = "ofdfga")

  # Estimated fetal weight for gestational age
  testthat_v2x(y = seq(967, 2076, 200), gest_days = 7 * 30, acronym = "efwfga")
})

test_that(desc = "Conversion of values to centiles works", {
  # Head circumference for gestational age
  testthat_v2x(y = 320:350, gest_days = 36 * 7, acronym = "hcfga", z_or_p = "centile")

  # Biparietal diameter for gestational age
  testthat_v2x(y = 90:105, gest_days = 7 * 34, acronym = "bpdfga", z_or_p = "centile")

  # Abdominal circumference for gestational age
  testthat_v2x(y = 225:235, gest_days = 7 * (28 + 2/7), acronym = "acfga", z_or_p = "centile")

  # Femur length for gestational age
  testthat_v2x(y = seq(50, 55, 0.5), gest_days = 7 * (28 + 2/7), acronym = "flfga", z_or_p = "centile")

  # Occipitofrontal diameter for gestational age
  testthat_v2x(y = seq(90, 100, 0.5), gest_days = 7 * (28 + 2/7), acronym = "ofdfga", z_or_p = "centile")

  # Estimated fetal weight for gestational age
  testthat_v2x(y = seq(967, 2076, 200), gest_days = 7 * 30, acronym = "efwfga", z_or_p = "centile")
})


error_msg <- function(name, wanted, got) {
  paste0("Assertion on '", name, "' failed: Must be of type '", wanted,
         "', not '", got, "'.")
}

test_that(
  desc = "Bad input types cause errors.",
  code = {
    x <- seq(154, 280, by = 0.5)
    len_x <- length(x)
    z <- pnorm(rep_len(-3:3, len_x))
    acronym <- rep_len(names(gigs::ig_fet), len_x)

    # Test failures for each arg when converting centiles to values
    testthat::expect_error(
      ig_fet_zscore2value(as.character(z), x, acronym),
      regexp = error_msg(name = "z", wanted = "numeric", got = "character")
    )
    testthat::expect_error(
      ig_fet_zscore2value(z, as.character(x), acronym),
      regexp = error_msg(name = "x", wanted = "numeric", got = "character")
    )
    testthat::expect_error(
      ig_fet_zscore2value(z, x, 1),
      regexp = error_msg(name = "acronym", wanted = "character", got = "double")
    )

    # And for conversion of values to centiles
    y <- ig_fet_zscore2value(z, x, acronym)
    testthat::expect_error(
      ig_fet_value2zscore(as.character(y), x, acronym),
      regexp = error_msg(name = "y", wanted = "numeric", got = "character")
    )
    testthat::expect_error(
      ig_fet_value2zscore(y, as.character(x), acronym),
      regexp = error_msg(name = "x", wanted = "numeric", got = "character")
    )
    testthat::expect_error(
      ig_fet_value2zscore(y, x, 1),
      regexp = error_msg(name = "acronym", wanted = "character", got = "double")
    )

    # All bad acronyms cause function to error
    expect_error(
      object = ig_fet_zscore2value(z = 0.6, gest_days = 50,
                                   acronym = "wrong_acronym"),
      regexp = paste0("No value in `acronym` was valid.")
    )
})

# Fetal weight estimation ------------------------------------------------------

test_that(
  desc = "Fetal weight estimation function works",
  code = {
    # Test that output matches example in Ultrasound Obstet. Gynecol.
    headcirc <- 29L
    abdocirc <- 26L
    efw <- ig_fet_estimate_fetal_weight(headcirc_cm = headcirc,
                                        abdocirc_cm = abdocirc)
    expect_equal(log(efw), expected = 7.312292, tolerance = 10e-8)
    expect_equal(round(efw), expected = 1499, tolerance = 0)

    # Test that function fails if inputs of wrong type
    expect_error(
      ig_fet_estimate_fetal_weight(headcirc_cm = as.character(headcirc),
                                   abdocirc_cm = abdocirc),
      regexp = error_msg("headcirc_cm", "numeric", "character")
    )
    expect_error(
      ig_fet_estimate_fetal_weight(headcirc_cm = headcirc,
                                   abdocirc_cm = as.complex(abdocirc)),
      regexp = error_msg("abdocirc_cm", "numeric", "complex")
    )
  }
)