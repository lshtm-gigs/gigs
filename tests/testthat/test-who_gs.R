# Correctness tests ------------------------------------------------------------

who_roundto <- function() 3

who_tolerance <- function() 10e-4

#' @srrstats {G5.4, G5.4c} Tests ensure that `gigs` functions can be used to
#'   replicate published growth charts, within a tolerance.
test_that(desc = "Conversion of z-scores/centiles to values works", {
  for (acronym in names(gigs::who_gs)) {
    for (chr_sex in c("male", "female")) {
      for (chr_z_or_p in c("zscore", "centile")) {
        ref_tbl <- gigs::who_gs[[acronym]][[chr_sex]][[paste0(chr_z_or_p, "s")]]
        if (is.null(ref_tbl)) {
          ref_tbl <- gigs::who_gs[[acronym]][[chr_sex]][[1]]
        }
        dbl_z_or_p <- switch(chr_z_or_p,
                             centile = c(0.001, 0.01, 0.03, 0.05, 0.1, 0.15,
                                         0.25, 0.5, 0.75, 0.85, 0.9, 0.95, 0.97,
                                         0.99, 0.999),
                             zscore = -4:4)
        xvar <- ref_tbl[[1]]
        sexvar <- if (chr_sex != "female") "M" else "F"
        fn <- get(paste0("who_gs_", acronym, "_", chr_z_or_p, "2value"))
        pkg_tbl <- lapply(X = dbl_z_or_p,
                          FUN = \(zp) {
                            round(fn(zp, xvar, sexvar),
                                  digits = who_roundto())
                          }) |>
          do.call(what = cbind) |>
          as.data.frame() |>
          setNames(names(ref_tbl)[-1])
        ref_tbl <- ref_tbl[-1]

        if (chr_z_or_p == "centile") {
          # Centiles go past +3/-3 z-score bounds, at which the published WHO
          # tables fail to account for the WHO constraining procedure. Without
          # these lines, expect_true() fails for P01 and P999.
          ref_tbl <- ref_tbl[c(-1, -15)]
          pkg_tbl <- pkg_tbl[c(-1, -15)]
        }

        differences <- round(abs(pkg_tbl - ref_tbl), who_roundto())
        tolerance <- who_tolerance()
        maxdiff <- max(differences, na.rm = TRUE)
        expect_true(max(differences, na.rm = TRUE) <= tolerance)
      }
    }
  }
})

#' @srrstats {G5.6, G5.6a} Checks that conversion functionality works when
#'   converting values to z-scores/centiles AND vice versa.
test_that(desc = "Conversion of values to z-scores works", {
  for (acronym in names(gigs::who_gs)) {
    for (chr_z_or_p in c("zscore", "centile")) {
      xvar <- gigs::who_gs[[acronym]][[1]][[1]][[1]]
      set.seed(seed = 1000)
      dbl_z_or_p <- rnorm(n = length(xvar))
      set.seed(seed = 1000)
      sexvar <- sample(c("M", "F"), size = length(xvar), replace = TRUE)
      if (chr_z_or_p == "centile") dbl_z_or_p <- pnorm(dbl_z_or_p)

      fn_stem <- paste0("who_gs_", acronym)
      fn_zp2val <- get(paste0(fn_stem, "_", chr_z_or_p, "2value"))
      y_gigs <- fn_zp2val(dbl_z_or_p, xvar, sexvar)

      fn_val2zp <- get(paste0(fn_stem, "_value2", chr_z_or_p))
      gigs_z_or_p <- fn_val2zp(y_gigs, xvar, sexvar)

      expect_equal(gigs_z_or_p, expected = dbl_z_or_p, tolerance = 10e-10)
    }
  }
})

test_that(desc = "Interpolation of LMS values can be performed",
          code = {
            expect_false(
              anyNA(
                who_gs_value2zscore(y = 20.3,
                                    # All xvars interpolated
                                    x = 900:920 + 0.5,
                sex = "M",
                acronym = "acfa")
              ))
            expect_false(
              anyNA(who_gs_value2zscore(y = 20.3,
                                        # Half interpolated, half not
                                        x = seq(900, 910, by = 0.5),
                                        sex = "M",
                                        acronym = "acfa")
              ))
          })

test_that(desc = "Interpolation of LMS values with multiple standards/sexes",
          code = {
            expect_false(
              anyNA(
                who_gs_value2centile(y = 20.3,
                                     x = c(900, 905.5),
                                     sex = c("M", "F"),
                                     acronym = c("acfa", "tsfa")
              )))
          })
#
# test_that(desc = "NA values returned with out of range xvars",
#           code = {
#             expect_true(
#               anyNA(
#                 who_gs_zscore2value(z = rep_len(-3:3, 200),
#                                     x = seq(0, 99.5, by = 0.5),
#                                     sex = rep_len(c("M", "F"), 200),
#                                     acronym = rep_len(names(gigs::who_gs), 200))
#               ))
#           })
#
# test_that(
#   desc = "Bad input types cause errors.",
#   code = {
#     x <- seq(65, 95, by = 0.5)
#     len_x <- length(x)
#     z <- rep_len(-3:3, len_x)
#     sex <- rep_len(c("M", "F"), len_x)
#     acronym <- rep_len(names(gigs::who_gs), len_x)
#
#         error_msg <- function(name, wanted, got) {
#       paste0("Assertion on '", name, "' failed: Must be of type '", wanted,
#              "', not '", got, "'.")
#     }
#
#     # Test failures for each arg when converting zscores to values
#     expect_error(
#       who_gs_zscore2value(as.character(z), x, sex, acronym),
#       regexp = error_msg(name = "z", wanted = "numeric", got = "character")
#     )
#     expect_error(
#       who_gs_zscore2value(z, as.character(x), sex, acronym),
#       regexp = error_msg(name = "x", wanted = "numeric", got = "character")
#     )
#     expect_error(
#       who_gs_zscore2value(z, x, 1, acronym),
#       regexp = error_msg(name = "sex", wanted = "character", got = "double")
#     )
#     expect_error(
#       who_gs_zscore2value(z, x, sex, 1),
#       regexp = error_msg(name = "acronym", wanted = "character", got = "double")
#     )
#
#     # And for conversion of values to zscores
#     y <- who_gs_zscore2value(z, x, sex, acronym)
#     expect_error(
#       who_gs_value2zscore(as.character(y), x, sex, acronym),
#       regexp = error_msg(name = "y", wanted = "numeric", got = "character")
#     )
#     expect_error(
#       who_gs_value2zscore(y, as.character(x), sex, acronym),
#       regexp = error_msg(name = "x", wanted = "numeric", got = "character")
#     )
#     expect_error(
#       who_gs_value2zscore(y, x, 1, acronym),
#       regexp = error_msg(name = "sex", wanted = "character", got = "double")
#     )
#     expect_error(
#       who_gs_value2zscore(y, x, sex, 1),
#       regexp = error_msg(name = "acronym", wanted = "character", got = "double")
#     )
#
#
#     error_msg_bad_value <- function(name) {
#       paste0("No value in `", name, "` was valid.")
#     }
#     # All bad sex values cause function to error
#     expect_error(
#       object = who_gs_zscore2value(z = 0,
#                                    x = 50,
#                                    sex = "wrong_sex",
#                                    acronym = "wfa"),
#       regexp = error_msg_bad_value(name = "sex")
#     )
#
#     # All bad acronyms cause function to error
#     expect_error(
#       object = who_gs_zscore2value(z = 0,
#                                    x = 50,
#                                    sex = "M",
#                                    acronym = "wrong_acronym"),
#       regexp = error_msg_bad_value(name = "acronym")
#     )
# })
