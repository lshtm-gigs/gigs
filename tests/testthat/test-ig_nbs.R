# Correctness checks -----------------------------------------------------------

nbs_roundto <- function(acronym) {
  switch(acronym, wfga = 2, wlrfga = 2, ffmfga = 0,  1)
}

nbs_pvals <- function(acronym) {
  if (acronym %in% c("wfga", "hcfga", "lfga", "wlrfga")) {
    c(0.03, 0.05, 0.10, 0.50, 0.90, 0.95, 0.97)
  } else if (acronym %in% c("ffmfga", "bfpfga", "fmfga")) {
    c(0.03,0.10, 0.50, 0.90, 0.97)
  } else if (!acronym %in% names(gigs::ig_nbs)) {
    stop("Bad acronym.")
  }
}

nbs_zvals <- function(acronym) {
  if (acronym %in% c("wfga", "hcfga", "lfga", "wlrfga")) {
    -3:3
  } else if (acronym %in% c("ffmfga", "bfpfga", "fmfga")) {
    qnorm(c(0.03,0.10, 0.50, 0.90, 0.97))
  } else if (!acronym %in% names(gigs::ig_nbs)) {
    stop("Bad acronym.")
  }
}

nbs_tolerance <- function(sex, acronym) {
  # 23/08/2023: With old linear equations for body composition:
  ## FMFGA: male = 26, female = 15
  ## BFPFGA: male = 0.5, female = 0.5
  ## FFMFGA: male = 7, female = 5
  switch(acronym,
         wfga = 0.01,
         lfga = 0.1,
         hcfga = 0.1,
         wlrfga = 0.01,
         ffmfga = switch(sex, male = 6, female = 1),
         bfpfga = switch(sex, male = 0.1, female = 0.1),
         fmfga = switch(sex, male = 1.9, female = 0.1)) + 1e-13
}

#' @srrstats {G5.4, G5.4c} Tests ensure that `gigs` functions can be used to
#'   replicate published growth charts, within a tolerance.
test_that(desc = "Conversion of z-scores/centiles to values works", {
  for (acronym in names(gigs::ig_nbs)) {
    for (chr_sex in c("male", "female")) {
      for (chr_z_or_p in c("zscore", "centile")) {
        ref_tbl <- gigs::ig_nbs[[acronym]][[chr_sex]][[paste0(chr_z_or_p, "s")]]
        if (is.null(ref_tbl)) {
          ref_tbl <- gigs::ig_nbs[[acronym]][[chr_sex]][[1]]
        }
        dbl_z_or_p <- switch(chr_z_or_p,
                             centile = nbs_pvals(acronym),
                             zscore = nbs_zvals(acronym))
        xvar <- ref_tbl[[1]]
        sexvar <- if (chr_sex != "female") "M" else "F"
        fn <- get(paste0("ig_nbs_", acronym, "_", chr_z_or_p, "2value"))
        pkg_tbl <- lapply(X = dbl_z_or_p,
                          FUN = \(zp) {
                            round(fn(zp, xvar, sexvar),
                                  digits = nbs_roundto(acronym))
                          }) |>
          do.call(what = cbind) |>
          as.data.frame() |>
          setNames(names(ref_tbl)[-1])
        ref_tbl <- ref_tbl[-1]
        differences <- abs(pkg_tbl - ref_tbl)
        tolerance <- nbs_tolerance(chr_sex, acronym)
        maxdiff <- max(differences, na.rm = TRUE)
        expect_true(max(differences, na.rm = TRUE) <= tolerance)
      }
    }
  }
})

#' @srrstats {G5.6, G5.6a} Checks that conversion functionality works when
#'   converting values to z-scores/centiles AND vice versa.
test_that(desc = "Conversion of values to z-scores works", {
  for (acronym in names(gigs::ig_nbs)) {
    for (chr_z_or_p in c("zscore", "centile")) {
      xvar <- gigs::ig_nbs[[acronym]][[1]][[1]][[1]]
      set.seed(seed = 1000)
      dbl_z_or_p <- rnorm(n = length(xvar))
      sexvar <- sample(c("M", "F"), size = length(xvar), replace = TRUE)
      if (chr_z_or_p == "centile") dbl_z_or_p <- pnorm(dbl_z_or_p)

      fn_stem <- paste0("ig_nbs_", acronym)
      fn_zp2val <- get(paste0(fn_stem, "_", chr_z_or_p, "2value"))
      y_gigs <- fn_zp2val(dbl_z_or_p, xvar, sexvar)

      fn_val2zp <- get(paste0(fn_stem, "_value2", chr_z_or_p))
      gigs_z_or_p <- fn_val2zp(y_gigs, xvar, sexvar)

      expect_equal(gigs_z_or_p, expected = dbl_z_or_p, tolerance = 10e-10)
    }
  }
})

# #' Check that bad input is silent, warns the user, OR gives an error depending
# #' on the user's options
# test_that(desc = "Bad input gives NA", code = {
#   df_inputs <- data.frame(sex = c("M", "F", "F", "X", "M"),
#                           centiles = c(0.25, -1, 0.13, 0.84, 0.10),
#                           age = 32:36 * 7)
#   for (x in c("wfga", "lfga", "hcfga", "wlrfga")) {
#     fn <- get(paste0("ig_nbs_", x, "_centile2value"))
#     vals <- with(df_inputs, fn(p = centiles, sex = sex, gest_days = age))
#
#     # Should return 5 values
#     expect_vector(object = vals, double(), size = nrow(df_inputs))
#     # Elements 1,3,5 shouldn't return NA, as valid inputs
#     expect_false(any(is.na(vals[c(1, 3, 5)])))
#     # Should return NA:
#     # Element 2 has an out-of-bounds centile (-1)
#     # Element 4 has an invalid sex option ("X")
#     expect_true(all(is.na(vals[c(2, 4)])))
#   }
#
#   # Test that bad input gives NA in VPNS
#   df_inputs <- data.frame(sex = c("M", "F", "F", "X", "M"),
#                           zscores = c(0.5, -1, 0.5, 0.5, 0.5),
#                           age = 24:28 * 7)
#
#   for (x in c("wfga", "lfga", "hcfga", "wlrfga")) {
#     fn <- get(paste0("ig_nbs_", x, "_zscore2value"))
#     vals <- with(df_inputs, fn(z = zscores, sex = sex, gest_days = age))
#     # Should return 5 values
#     expect_vector(object = vals, double(), size = nrow(df_inputs))
#     # Elements 1,2,3,5 shouldn't return NA, as valid inputs
#     expect_false(any(is.na(vals[c(1, 2, 3, 5)])))
#     # Should return NA:
#     # Element 4 has an invalid sex option ("X")
#     expect_true(all(is.na(vals[4])))
#   }
# })

test_that(desc = "Coefficient interpolation possible on ints and doubles",
          code = {
            expect_false(
              anyNA(ig_nbs_value2zscore(y = 3.4,
                                        # Half will need interpolation
                                        gest_days = seq(260, 264, by = 0.5),
                                        sex = "M",
                                        acronym = "wfga")
              ))
          })

test_that(desc = "Interpolation of MSNT values with multiple standards/sexes",
          code = {
            expect_false(
              anyNA(ig_nbs_value2zscore(y = 3,
                                        gest_days = seq(250, 245.5,
                                                        length.out = 10),
                                        sex = rep(c("M", "F"), 5),
                                        acronym = rep(c("wfga", "lfga"), 5))))
          })

# test_that(
#   desc = "NA values returned with out of range gestage",
#   code = {
#     ga_range <- 130:310
#     out_len <- length(ga_range)
#     full_nbs_range <- 168:300
#     expect_equal(
#       sum(
#         !is.na(
#           ig_nbs_centile2value(
#             p = 0.5,
#             gest_days = ga_range,
#             sex = rep_len(c("M", "F"), out_len),
#             acronym = rep_len(names(gigs::ig_nbs)[1:4], out_len))
#         )
#       ),
#       length(full_nbs_range)
#     )
#     bodycomp_range <- 266:294
#     expect_equal(
#       sum(
#         !is.na(
#           ig_nbs_centile2value(
#             p = 0.5,
#             gest_days = ga_range,
#             sex = rep_len(c("M", "F"), out_len),
#             acronym = rep_len(names(gigs::ig_nbs[5:7]), out_len))
#         )
#       ),
#       length(bodycomp_range)
#     )
#   }
# )
#
# test_that(desc = "NA values returned with body composition standards", code = {
#   # Test that bad input gives NA
#   with(
#     data.frame(sex = c("M", "F", "F", "X", "M"),
#          centiles = c(0.25, -1, 0.13, 0.84, 0.10),
#          age = 7 * c(37, 38, 39, 40, 41)),
#     expr = {
#       vapply(
#         X = c("fmfga", "bfpfga", "ffmfga"),
#         FUN = function(x) {
#           fn <- get(paste0("ig_nbs_", x, "_centile2value"))
#           vals <- suppressWarnings(fn(sex = sex, gest_days = age, p = centiles))
#           expect_length(object = vals, n = length(sex))
#           expect_true(all(is.na(c(
#             vals[1], # Because age is out of bounds
#             vals[2], # Because centile is out of bounds
#             vals[4]  # Because sex is not one of "M" or "F"
#           ))))
#         },
#         FUN.VALUE = logical(length = 1L))
#     })
# })
#
# test_that(
#   desc = "Coefficient retrieval works when not all acronyms are valid",
#   code = {
#     expect_true(
#       anyNA(
#         ig_nbs_value2centile(
#           y = rnorm(n = 100, mean =  2.3, sd = 0.25),
#           gest_days = rep_len(232:300, length.out = 100),
#           sex = rep_len(c("M", "F"), length.out = 100),
#           acronym = rep_len(c(names(gigs::ig_nbs_coeffs)[1:2], "xfga"),
#                             length.out = 100)
#         )))
#     expect_true(
#       anyNA(
#         ig_nbs_centile2value(
#           p = rnorm(n = 100, mean = 0.5, sd = 0.05),
#           gest_days = rep_len(232:300, length.out = 100),
#           sex = rep_len(c("M", "F"), length.out = 100),
#           acronym = rep_len(c(names(gigs::ig_nbs_coeffs)[1:2], "xfga"),
#                             length.out = 100)
#         )))
#   }
# )
#
# test_that(
#   desc = "Bad input types cause errors.",
#   code = {
#     x <- seq(168, 300, by = 0.5)
#     len_x <- length(x)
#     p <- pnorm(rep_len(-3:3, len_x))
#     sex <- rep_len(c("M", "F"), len_x)
#     acronym <- rep_len(names(gigs::ig_nbs), len_x)
#
#     error_msg <- function(name, wanted, got) {
#       paste0("Assertion on '", name, "' failed: Must be of type '", wanted,
#              "', not '", got, "'.")
#     }
#
#     # Test failures for each arg when converting centiles to values
#     expect_error(
#       ig_nbs_centile2value(as.character(p), x, sex, acronym),
#       regexp = error_msg(name = "p", wanted = "numeric", got = "character")
#     )
#     expect_error(
#       ig_nbs_centile2value(p, as.character(x), sex, acronym),
#       regexp = error_msg(name = "x", wanted = "numeric", got = "character")
#     )
#     expect_error(
#       ig_nbs_centile2value(p, x, 1, acronym),
#       regexp = error_msg(name = "sex", wanted = "character", got = "double")
#     )
#     expect_error(
#       ig_nbs_centile2value(p, x, sex, 1),
#       regexp = error_msg(name = "acronym", wanted = "character", got = "double")
#     )
#
#     # And for conversion of values to centiles
#     y <- ig_nbs_centile2value(p, x, sex, acronym)
#     expect_error(
#       ig_nbs_value2centile(as.character(y), x, sex, acronym),
#       regexp = error_msg(name = "y", wanted = "numeric", got = "character")
#     )
#     expect_error(
#       ig_nbs_value2centile(y, as.character(x), sex, acronym),
#       regexp = error_msg(name = "x", wanted = "numeric", got = "character")
#     )
#     expect_error(
#       ig_nbs_value2centile(y, x, 1, acronym),
#       regexp = error_msg(name = "sex", wanted = "character", got = "double")
#     )
#     expect_error(
#       ig_nbs_value2centile(y, x, sex, 1),
#       regexp = error_msg(name = "acronym", wanted = "character", got = "double")
#     )
#
#
#     error_msg_bad_value <- function(name) {
#       paste0("No value in `", name, "` was valid.")
#     }
#     # All bad sex values cause function to error
#     expect_error(
#       object = ig_nbs_centile2value(p = 0.6,
#                                     gest_days = 50,
#                                     sex = "wrong_sex",
#                                     acronym = "wfa"),
#       regexp = error_msg_bad_value(name = "sex")
#     )
#
#     # All bad acronyms cause function to error
#     expect_error(
#       object = ig_nbs_centile2value(p = 0.6,
#                                     gest_days = 50,
#                                     sex = "M",
#                                     acronym = "wrong_acronym"),
#       regexp = error_msg_bad_value(name = "acronym")
#     )
# })