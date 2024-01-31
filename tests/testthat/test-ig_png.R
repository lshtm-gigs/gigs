# Correctness checks -----------------------------------------------------------

png_roundto <- function(acronym) {
  switch(acronym, wfa = 2, wfl = 2, 1)
}

png_tolerance <- function(acronym) {
  switch(acronym, wfa = 0.01, wfl = 0.01, 0.1)
}

#' @srrstats {G5.4, G5.4c} Tests ensure that `gigs` functions can be used to
#'   replicate published growth charts, within a tolerance.
test_that(desc = "Conversion of z-scores/centiles to values works", {
  for (acronym in names(gigs::ig_png)) {
    for (chr_sex in c("male", "female")) {
      for (chr_z_or_p in c("zscore", "centile")) {
        ref_tbl <- gigs::ig_png[[acronym]][[chr_sex]][[paste0(chr_z_or_p, "s")]]
        if (is.null(ref_tbl)) {
          ref_tbl <- gigs::ig_png[[acronym]][[chr_sex]][[1]]
        }
        dbl_z_or_p <- switch(chr_z_or_p,
                             centile = c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97),
                             zscore = -3:3)
        xvar <- ref_tbl[[1]]
        sexvar <- if (chr_sex != "female") "M" else "F"
        fn <- get(paste0("ig_png_", acronym, "_", chr_z_or_p, "2value"))
        pkg_tbl <- lapply(X = dbl_z_or_p,
                          FUN = \(zp) {
                            round(fn(zp, xvar, sexvar),
                                  digits = png_roundto(acronym))
                          }) |>
          do.call(what = cbind) |>
          as.data.frame() |>
          setNames(names(ref_tbl)[-1])
        ref_tbl <- ref_tbl[-1]
        differences <- round(abs(pkg_tbl - ref_tbl), png_roundto(acronym))
        tolerance <- png_tolerance(acronym)
        maxdiff <- max(differences, na.rm = TRUE)
        expect_true(max(differences, na.rm = TRUE) <= tolerance)
      }
    }
  }
})

#' @srrstats {G5.6, G5.6a} Checks that conversion functionality works when
#'   converting values to z-scores/centiles AND vice versa.
test_that(desc = "Conversion of values to z-scores works", {
  for (acronym in names(gigs::ig_png)) {
    for (chr_z_or_p in c("zscore", "centile")) {
      xvar <- gigs::ig_png[[acronym]][[1]][[1]][[1]]
      set.seed(seed = 1000)
      dbl_z_or_p <- rnorm(n = length(xvar))
      sexvar <- sample(c("M", "F"), size = length(xvar), replace = TRUE)
      if (chr_z_or_p == "centile") dbl_z_or_p <- pnorm(dbl_z_or_p)

      fn_stem <- paste0("ig_png_", acronym)
      fn_zp2val <- get(paste0(fn_stem, "_", chr_z_or_p, "2value"))
      y_gigs <- fn_zp2val(dbl_z_or_p, xvar, sexvar)

      fn_val2zp <- get(paste0(fn_stem, "_value2", chr_z_or_p))
      gigs_z_or_p <- fn_val2zp(y_gigs, xvar, sexvar)

      expect_equal(gigs_z_or_p, expected = dbl_z_or_p, tolerance = 10e-10)
    }
  }
})

# test_that(
#   desc = "Bad input types cause errors.",
#   code = {
#     x <- 30:35
#     x_len <- length(x)
#     z <- rep_len(-3:3, x_len)
#     sex <- rep_len(c("M", "F"), x_len)
#     acronym <- rep_len(names(gigs::ig_png), x_len)
#
#     error_msg <- function(name, wanted, got) {
#       paste0("Assertion on '", name, "' failed: Must be of type '", wanted,
#              "', not '", got, "'.")
#     }
#
#     # Test failures for each arg when converting zscores to values
#     expect_error(
#       ig_png_zscore2value(as.character(z), x, sex, acronym),
#       regexp = error_msg(name = "z", wanted = "numeric", got = "character")
#     )
#     expect_error(
#       ig_png_zscore2value(z, as.character(x), sex, acronym),
#       regexp = error_msg(name = "x", wanted = "numeric", got = "character")
#     )
#     expect_error(
#       ig_png_zscore2value(z, x, 1, acronym),
#       regexp = error_msg(name = "sex", wanted = "character", got = "double")
#     )
#     expect_error(
#       ig_png_zscore2value(z, x, sex, 1),
#       regexp = error_msg(name = "acronym", wanted = "character", got = "double")
#     )
#
#     # And for conversion of values to zscores
#     y <- ig_png_zscore2value(z, x, sex, acronym)
#     expect_error(
#       ig_png_value2zscore(as.character(y), x, sex, acronym),
#       regexp = error_msg(name = "y", wanted = "numeric", got = "character")
#     )
#     expect_error(
#       ig_png_value2zscore(y, as.character(x), sex, acronym),
#       regexp = error_msg(name = "x", wanted = "numeric", got = "character")
#     )
#     expect_error(
#       ig_png_value2zscore(y, x, 1, acronym),
#       regexp = error_msg(name = "sex", wanted = "character", got = "double")
#     )
#     expect_error(
#       ig_png_value2zscore(y, x, sex, 1),
#       regexp = error_msg(name = "acronym", wanted = "character", got = "double")
#     )
#
#     error_msg_bad_value <- function(name) {
#       paste0("No value in `", name, "` was valid.")
#     }
#     # All bad sex values cause function to error
#     expect_error(
#       object = ig_png_zscore2value(z = 0,
#                                    x = 50,
#                                    sex = "wrong_sex",
#                                    acronym = "wfa"),
#       regexp = error_msg_bad_value(name = "sex")
#     )
#
#     # All bad acronyms cause function to error
#     expect_error(
#       object = ig_png_zscore2value(z = 0,
#                                    x = 50,
#                                    sex = "M",
#                                    acronym = "wrong_acronym"),
#       regexp = error_msg_bad_value(name = "acronym")
#     )
# })