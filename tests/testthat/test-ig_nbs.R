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
test_that(
  desc = "Conversion of z-scores/centiles to values works",
  code = {
    for (acronym in names(gigs::ig_nbs)[5]) {
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
  }
)

#' @srrstats {G5.5, G5.6, G5.6a, G5.6b, G5.9b} Checks that conversion
#'   functionality works when converting values to z-scores/centiles AND vice
#'   versa. Uses multiple fixed seeds to generate random inputs, which do not
#'   affect the functions' results.
test_that(
  desc = "Conversion of values to z-scores works",
  code = {
    for (acronym in names(gigs::ig_nbs)) {
      for (chr_z_or_p in c("zscore", "centile")) {
        xvar <- gigs::ig_nbs[[acronym]][[1]][[1]][[1]]
        for (seed in seq(300, 400, 30)) {
          withr::with_seed(seed, {
            dbl_z_or_p <- rnorm(n = length(xvar))
            sexvar <- sample(c("M", "F"), size = length(xvar), replace = TRUE)
          })
          if (chr_z_or_p == "centile") dbl_z_or_p <- pnorm(dbl_z_or_p)

          fn_stem <- paste0("ig_nbs_", acronym)
          fn_zp2val <- get(paste0(fn_stem, "_", chr_z_or_p, "2value"))
          y_gigs <- fn_zp2val(dbl_z_or_p, xvar, sexvar)

          fn_val2zp <- get(paste0(fn_stem, "_value2", chr_z_or_p))
          gigs_z_or_p <- fn_val2zp(y_gigs, xvar, sexvar)

          expect_equal(gigs_z_or_p, expected = dbl_z_or_p, tolerance = 10e-10)
        }
      }
    }
  }
)

#' @srrstats {G5.9, G5.9a} Trivial noise does not meaningfully alter results.
test_that(
  desc = "Conversion of values to z-scores works with trivial noise",
  code = {
    for (acronym in names(gigs::ig_nbs)) {
      for (chr_z_or_p in c("zscore", "centile")) {
        xvar <- ig_nbs[[acronym]][[1]][[1]][[1]]
        xrange <- range(xvar)
        withr::with_seed(50, code = {
          xvar <- jitter(xvar, 1)
          xvar[xvar < xrange[1]] <- xrange[1]
          xvar[xvar > xrange[2]] <- xrange[2]
          dbl_z_or_p <- rnorm(n = length(xvar))
          sexvar <- sample(c("M", "F"), size = length(xvar), replace = TRUE)
        })
        if (chr_z_or_p == "centile") dbl_z_or_p <- pnorm(dbl_z_or_p)

        fn_stem <- paste0("ig_nbs_", acronym)
        fn_zp2val <- get(paste0(fn_stem, "_", chr_z_or_p, "2value"))
        y_gigs <- fn_zp2val(dbl_z_or_p, xvar, sexvar)

        fn_val2zp <- get(paste0(fn_stem, "_value2", chr_z_or_p))
        gigs_z_or_p <- fn_val2zp(y_gigs + .Machine$double.eps, xvar, sexvar)

        expect_equal(gigs_z_or_p, expected = dbl_z_or_p,
                     tolerance = sqrt(.Machine$double.eps))
      }
    }
  }
)

# Appropriate errors for zero-length data + data of wrong type -----------------

#' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
#' @srrstats {G5.8, G5.8a, G5.8b} Including errors for zero-length inputs and
#'   incorrect data types.
test_that(
  desc = "Correct errors occur when data is wrong type/length:",
  code = {
    # Error message testing: Iterate through ig_nbs functions, demonstrating
    #   with passed `expect_error()` tests that all error messages are unique
    #   to the data passed in.
    acronyms_to_test <- names(ig_nbs)
    fn_suffixes <- c("value2zscore", "value2centile", "centile2value",
                     "zscore2value")
    z <- 0
    p <- 0.5
    y <- 4
    for (fn_suffix in fn_suffixes) {
      for (str_acronym in acronyms_to_test) {
        fn_name <- paste("ig_nbs", str_acronym, fn_suffix, sep = "_")
        gigs_fn <- get(fn_name)
        arg_x_name <- gigs::ig_nbs[[str_acronym]][["x"]]
        y_name <- gigs::ig_nbs[[str_acronym]][["y"]]

        arg_zyp <- switch(fn_suffix, centile2value = p, zscore2value = z, y)
        arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                               zscore2value = "z", y_name)
        arg_x <- gigs::ig_nbs[[str_acronym]][[1]][[1]][[1]]
        arg_sex <- withr::with_seed(25, {
          sample(c("M", "F"), replace = TRUE, size = length(arg_x))
        })

        # G5.8a --> zero-length data inputs cause errors
        expect_error(object = gigs_fn(double(), arg_x, arg_sex),
                     regexp = test_error_zero_length(arg_zyp_name))
        expect_error(object = gigs_fn(arg_zyp, double(), arg_sex),
                     regexp = test_error_zero_length(arg_x_name))
        expect_error(object = gigs_fn(double(), arg_x, character()),
                     regexp = test_error_zero_length(c(arg_zyp_name, "sex")))

        # G5.8b --> incorrect data types
        expect_error(
          object = gigs_fn(as.character(arg_zyp), arg_x, arg_sex),
          regexp = test_error_wrong_type(arg_zyp_name, "numeric", "character")
        )
        expect_error(
          object = gigs_fn(arg_zyp, logical(1), arg_sex),
          regexp = test_error_wrong_type(arg_x_name, "numeric", "logical")
        )
        expect_error(
          object = gigs_fn(arg_zyp, arg_x, complex(1)),
          regexp = test_error_wrong_type("sex", "character", "complex")
        )

        # G5.2, --> errors if vector inputs cannot be recycled
        expect_error(
          object = gigs_fn(rep_len(arg_zyp, 2),
                           arg_x,
                           arg_sex),
          regexp = test_error_unrecyclable(names = arg_zyp_name)
        )
        expect_error(
          object = gigs_fn(rep_len(arg_zyp, 3),
                           rep_len(arg_x, 2),
                           arg_sex),
          regexp = test_error_unrecyclable(names = c(arg_zyp_name, arg_x_name))
        )
        expect_error(
          object = gigs_fn(rep(arg_zyp, 3),
                           rep_len(arg_x, 3),
                           rep_len(arg_sex, 2)),
          regexp = test_error_unrecyclable(names = "sex")
        )
      }

      # Repeat for standard-agnostic conversion function
      fn_name <- paste("ig_nbs", fn_suffix, sep = "_")
      gigs_fn <- get(fn_name)

      arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                             zscore2value = "z", "y")
      arg_acronym <- str_acronym

      # G5.8a --> zero-length data inputs cause errors
      expect_error(object = gigs_fn(double(), arg_x, arg_sex, arg_acronym),
                   regexp = test_error_zero_length(arg_zyp_name))
      expect_error(object = gigs_fn(arg_zyp, double(), arg_sex, arg_acronym),
                   regexp = test_error_zero_length("gest_days"))
      expect_error(object = gigs_fn(double(), arg_x, character(), arg_acronym),
                   regexp = test_error_zero_length(c(arg_zyp_name, "sex")))
      expect_error(object = gigs_fn(arg_zyp, double(), arg_sex, character()),
                   regexp = test_error_zero_length(c("gest_days", "acronym")))

      # G5.8b --> incorrect data types
      expect_error(
        object = gigs_fn(as.character(arg_zyp), arg_x, arg_sex, arg_acronym),
        regexp = test_error_wrong_type(arg_zyp_name, "numeric", "character")
      )
      expect_error(
        object = gigs_fn(arg_zyp, logical(1), arg_sex, arg_acronym),
        regexp = test_error_wrong_type("gest_days", "numeric", "logical")
      )
      expect_error(
        object = gigs_fn(arg_zyp, arg_x, 50L, arg_acronym),
        regexp = test_error_wrong_type("sex", "character", "integer")
      )
      expect_error(
        object = gigs_fn(arg_zyp, arg_x, arg_sex, complex(length(arg_zyp))),
        regexp = test_error_wrong_type("acronym", "character", "complex")
      )
      # G5.2, --> errors if vector inputs cannot be recycled
      expect_error(
        object = gigs_fn(rep_len(arg_zyp, 2),
                         arg_x,
                         arg_sex,
                         arg_acronym),
        regexp = test_error_unrecyclable(names = arg_zyp_name)
      )
      expect_error(
        object = gigs_fn(arg_zyp,
                         arg_x,
                         arg_sex,
                         rep_len(arg_acronym, 2)),
        regexp = test_error_unrecyclable(names = "acronym")
      )
      expect_error(
        object = gigs_fn(arg_zyp,
                         rep_len(arg_x, 2),
                         arg_sex,
                         rep_len(arg_acronym, 2)),
        regexp = test_error_unrecyclable(c("gest_days", "acronym"))
      )
    }
  }
)

# Appropriate handling of non-fatal issues with input data ---------------------

#' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
#' @srrstats{G5.8, G5.8d} Show that gigs can handle data outside the
#'   scope of its algorithms by replacing bad inputs with `NA`, and simply
#'   outputting `NA` for the output elements which map onto `NA` input
#'   elements.
#' @srrstats {EA6.0, EA6.0a} Also checks that classes/types of returned objects
#'   are correct using [checkmate::expect_numeric()]
test_that(
  desc = "Invalid values can be replaced with `NA` quietly",
  code = {
    # Make gigs *quietly* replace bad data with NA
    for (option in names(.gigs_options)) {
      gigs_option_set(option, new_value = "quiet", silent = TRUE)
    }

    acronyms_to_test <- names(ig_nbs)
    fn_suffixes <- c("value2zscore", "value2centile", "centile2value",
                     "zscore2value")
    z <- 0
    p <- 0.5

    # Random set of indices to replace with bad data in each case
    num_to_replace1 <- 6
    withr::with_seed(200, code = {
      replace_ints_1 <- sample(1L:20L, size = num_to_replace1)
    })
    num_to_replace2 <- 5
    withr::with_seed(200, code = {
      replace_ints_2 <- sample(1L:20L, size = num_to_replace2)
    })
    replace_ints_3 <- union(replace_ints_1, replace_ints_2)
    num_to_replace3 <- length(union(replace_ints_1, replace_ints_2))

    for (fn_suffix in fn_suffixes) {
      for (str_acronym in acronyms_to_test) {
        fn_name <- paste("ig_nbs", str_acronym, fn_suffix, sep = "_")
        gigs_fn <- get(fn_name)
        x <- gigs::ig_nbs[[str_acronym]][[1]][[1]][[1]]
        len_x <- length(x) * 4
        arg_x_name <- gigs::ig_nbs[[str_acronym]][["x"]]
        y_name <- gigs::ig_nbs[[str_acronym]][["y"]]

        arg_zyp <- rep_len(switch(fn_suffix,
                                  centile2value = p,
                                  zscore2value = z,
                                  gigs::ig_nbs[[str_acronym]][[1]][[1]][[3]]),
                           len_x)
        arg_zyp_name <- switch(fn_suffix,
                               centile2value = "p",
                               zscore2value = "z",
                               y_name)
        arg_x <- rep_len(x, len_x)
        arg_sex <- withr::with_seed(25, {
          sample(c("M", "F"), replace = TRUE, size = len_x)
        })

        # Bad input 1: Undefined data (NaN, Inf, -Inf) -------------------------
        for (undefined_val in c(NaN, Inf, -Inf)) {
          ## Replace z/y/p variable and x variable with undefined_val
          out <- gigs_fn(replace(arg_zyp, replace_ints_1, undefined_val),
                         replace(arg_x, replace_ints_2, undefined_val),
                         arg_sex)
          checkmate::expect_numeric(out, len = len_x)
          expect_true(all(is.na(out[replace_ints_3])))
        }

        # Bad input 2: Missing data (NA) ---------------------------------------
        ## Replace x variable with NA
        out <- gigs_fn(arg_zyp,
                       replace(arg_x, replace_ints_2, values = NA),
                       arg_sex)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_2])))

        ## Replace z/y/p variable and x variable with NA
        out <- gigs_fn(replace(arg_zyp, replace_ints_1, values = NA),
                       arg_x,
                       replace(arg_sex, replace_ints_2, values = NA))
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_3])))

        # Bad input 3: Out of bounds centiles (not `> 0` and `< 1`) ------------
        if (fn_suffix == "centile2value") {
          out <- gigs_fn(replace(arg_zyp, replace_ints_1, values = 2.5),
                         arg_x,
                         arg_sex)
          checkmate::expect_numeric(out, len = len_x)
          expect_true(all(is.na(out[replace_ints_1])))

          out <- gigs_fn(replace(arg_zyp, replace_ints_2, values = 2.5),
                         arg_x,
                         arg_sex)
          checkmate::expect_numeric(out, len = len_x)
          expect_true(all(is.na(out[replace_ints_2])))
        }

        # Bad input 4: Out of bounds `x` variable ------------------------------
        below_lower_bound <- min(x) - replace_ints_1
        above_upper_bound <- max(x) + replace_ints_2
        withr::with_seed(500, code = {
          out_of_bounds <- sample(c(below_lower_bound, above_upper_bound),
                                  size = num_to_replace3, replace = FALSE)
        })

        out <- gigs_fn(arg_zyp,
                       replace(arg_x, replace_ints_2, above_upper_bound),
                       arg_sex)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_2])))

        out <- gigs_fn(arg_zyp,
                       replace(arg_x, replace_ints_3, out_of_bounds),
                       arg_sex)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_3])))

        # Bad input 5: Non-"M"/"F" `sex` variables -----------------------------
        out <- gigs_fn(arg_zyp,
                       arg_x,
                       replace(arg_sex, replace_ints_2, "invalid_sex"))
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_2])))

        out <- gigs_fn(arg_zyp,
                       arg_x,
                       replace(arg_sex, replace_ints_3, "invalid_sex"))
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_3])))
      }

      fn_name <- paste("ig_nbs", fn_suffix, sep = "_")
      gigs_fn <- get(fn_name)
      arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                             zscore2value = "z", "y")
      arg_acronym <- rep_len(str_acronym, len_x)

      # Bad input 1: Undefined data (NaN, Inf, -Inf) -------------------------
      for (undefined_val in c(NaN, Inf, -Inf)) {
        ## Replace z/y/p variable and x variable with undefined_val
        out <- gigs_fn(replace(arg_zyp, replace_ints_1, undefined_val),
                       replace(arg_x, replace_ints_2, undefined_val),
                       arg_sex,
                       arg_acronym)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_3])))
      }

      # Bad input 2: Missing data (NA) ---------------------------------------
      ## Replace x variable with NA
      out <- gigs_fn(arg_zyp,
                     replace(arg_x, replace_ints_2, values = NA),
                     arg_sex,
                     arg_acronym)
      checkmate::expect_numeric(out, len = len_x)
      expect_true(all(is.na(out[replace_ints_2])))

      ## Replace z/y/p variable and x variable with NA
      out <- gigs_fn(replace(arg_zyp, replace_ints_1, values = NA),
                     arg_x,
                     replace(arg_sex, replace_ints_2, values = NA),
                     arg_acronym)
      checkmate::expect_numeric(out, len = len_x)
      expect_true(all(is.na(out[replace_ints_3])))

      ## Replace z/y/p variable and acronym variable with NA
      out <- gigs_fn(replace(arg_zyp, replace_ints_1, values = NA),
                     arg_x,
                     arg_sex,
                     replace(arg_acronym, replace_ints_2, values = NA))
      checkmate::expect_numeric(out, len = len_x)
      expect_true(all(is.na(out[replace_ints_3])))

      # Bad input 3: Out of bounds centiles (not `> 0` and `< 1`) ------------
      if (fn_suffix == "centile2value") {
        out <- gigs_fn(replace(arg_zyp, replace_ints_1, values = 2.5),
                       arg_x, arg_sex, arg_acronym)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_1])))

        out <- gigs_fn(replace(arg_zyp, replace_ints_2, values = 2.5),
                       arg_x, arg_sex, arg_acronym)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_2])))
      }

      # Bad input 4: Out of bounds `x` variable ------------------------------
      below_lower_bound <- min(x) - replace_ints_1
      above_upper_bound <- max(x) + replace_ints_2
      withr::with_seed(500, code = {
        out_of_bounds <- sample(c(below_lower_bound, above_upper_bound),
                                size = num_to_replace3, replace = FALSE)
      })

      out <- gigs_fn(arg_zyp,
                     replace(arg_x, replace_ints_2, above_upper_bound),
                     arg_sex,
                     arg_acronym)
      checkmate::expect_numeric(out, len = len_x)
      expect_true(all(is.na(out[replace_ints_2])))

      out <- gigs_fn(arg_zyp,
                     replace(arg_x, replace_ints_3, out_of_bounds),
                     arg_sex,
                     arg_acronym)
      checkmate::expect_numeric(out, len = len_x)
      expect_true(all(is.na(out[replace_ints_3])))

      # Bad input 5: Invalid `acronym` values (not in names(gigs::ig_nbs))
      out <- gigs_fn(arg_zyp,
                     arg_x,
                     arg_sex,
                     replace(arg_acronym, replace_ints_1, "invalid_acronym"))
      checkmate::expect_numeric(out, len = len_x)
      expect_true(all(is.na(out[replace_ints_1])))

      out <- gigs_fn(arg_zyp,
                     arg_x,
                     arg_sex,
                     replace(arg_acronym, replace_ints_2, "invalid_acronym"))
      checkmate::expect_numeric(out, len = len_x)
      expect_true(all(is.na(out[replace_ints_2])))

      # Bad input 6: Non-"M"/"F" `sex` variables -----------------------------
      out <- gigs_fn(arg_zyp,
                     arg_x,
                     replace(arg_sex, replace_ints_2, "invalid_sex"),
                     arg_acronym)
      checkmate::expect_numeric(out, len = len_x)
      expect_true(all(is.na(out[replace_ints_2])))

      out <- gigs_fn(arg_zyp,
                     arg_x,
                     replace(arg_sex, replace_ints_3, "invalid_sex"),
                     arg_acronym)
      checkmate::expect_numeric(out, len = len_x)
      expect_true(all(is.na(out[replace_ints_3])))
    }
  }
)

# Appropriate warnings/errors with bad input data ------------------------------

#' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
#' @srrstats{G5.8, G5.8d} Show that gigs can handle data outside the
#'   scope of its algorithms by replacing bad inputs with `NA` and giving clear
#'   warnings.
test_that(
  desc = "Invalid values can be replaced with `NA` with a warning",
  code = {
    # Make gigs *quietly* replace bad data with NA
    for (option in names(.gigs_options)) {
      gigs_option_set(option, new_value = "warn", silent = TRUE)
    }

    acronyms_to_test <- names(ig_nbs)
    fn_suffixes <- c("value2zscore", "value2centile", "centile2value",
                     "zscore2value")
    z <- 0
    p <- 0.5

    # Random set of indices to replace with bad data in each case
    num_to_replace1 <- 6
    withr::with_seed(200, code = {
      replace_ints_1 <- sample(1L:20L, size = num_to_replace1)
    })
    num_to_replace2 <- 5
    withr::with_seed(200, code = {
      replace_ints_2 <- sample(1L:20L, size = num_to_replace2)
    })
    replace_ints_3 <- union(replace_ints_1, replace_ints_2)
    num_to_replace3 <- length(union(replace_ints_1, replace_ints_2))

    for (fn_suffix in fn_suffixes) {
      for (str_acronym in acronyms_to_test) {
        fn_name <- paste("ig_nbs", str_acronym, fn_suffix, sep = "_")
        gigs_fn <- get(fn_name)
        x <- gigs::ig_nbs[[str_acronym]][[1]][[1]][[1]]
        len_x <- length(x) * 4
        arg_x_name <- gigs::ig_nbs[[str_acronym]][["x"]]
        y_name <- gigs::ig_nbs[[str_acronym]][["y"]]

        arg_zyp <- rep_len(switch(fn_suffix,
                                  centile2value = p,
                                  zscore2value = z,
                                  gigs::ig_nbs[[str_acronym]][[1]][[1]][[3]]),
                           len_x)
        arg_zyp_name <- switch(fn_suffix,
                               centile2value = "p",
                               zscore2value = "z",
                               y_name)
        arg_x <- rep_len(x, len_x)
        arg_sex <- withr::with_seed(25, {
          sample(c("M", "F"), replace = TRUE, size = len_x)
        })

        # Bad input 1: Undefined data (NaN, Inf, -Inf) -------------------------
        for (undefined_val in c(NaN, Inf, -Inf)) {
          ## Replace z/y/p variable and x variable with undefined_val
          warnings <- capture_warnings(
            gigs_fn(replace(arg_zyp, replace_ints_2, undefined_val),
                    replace(arg_x, replace_ints_3, undefined_val),
                    arg_sex)
          )
          expect_match(warnings[[1]],
                       test_msg_undefined(arg_zyp_name, len_x, num_to_replace2))
          expect_match(warnings[[2]],
                       test_msg_undefined(arg_x_name, len_x, num_to_replace3))
        }

        # Bad input 2: Missing data (NA) ---------------------------------------

        ## Replace z/y/p variable and x variable with NA
        warnings <- capture_warnings(
          gigs_fn(replace(arg_zyp, replace_ints_2, values = NA),
                  replace(arg_x, replace_ints_3, values = NA),
                  replace(arg_sex, replace_ints_1, values = NA))
        )
        expect_match(warnings[[1]],
                     test_msg_missing(arg_zyp_name, len_x, num_to_replace2))
        expect_match(warnings[[2]],
                     test_msg_missing(arg_x_name, len_x, num_to_replace3))
        expect_match(warnings[[3]],
                     test_msg_missing("sex", len_x, num_to_replace1))

        # Bad input 3: Out of bounds centiles (not `> 0` and `< 1`) ------------
        if (fn_suffix == "centile2value") {
          expect_warning(
            gigs_fn(replace(arg_zyp, replace_ints_1, values = 2.5),
                    arg_x,
                    arg_sex),
            test_msg_centile_oob(len_x, num_to_replace1)
          )
          expect_warning(
            gigs_fn(replace(arg_zyp, replace_ints_2, values = 2.5),
                    arg_x,
                    arg_sex),
            test_msg_centile_oob(len_x, num_to_replace2)
          )
        }

        # Bad input 4: Out of bounds `x` variable ------------------------------
        below_lower_bound <- min(x) - replace_ints_1
        above_upper_bound <- max(x) + replace_ints_2
        out_of_bounds <-withr::with_seed(500, code = {
          sample(c(below_lower_bound, above_upper_bound),
                 size = num_to_replace3, replace = FALSE)
        })
        expect_warning(
          gigs_fn(arg_zyp, replace(arg_x, replace_ints_2, above_upper_bound), arg_sex),
          test_msg_xvar_oob(arg_x_name, len_x, num_to_replace2, "ig_nbs")
        )
        expect_warning(
          gigs_fn(arg_zyp, replace(arg_x, replace_ints_3, out_of_bounds), arg_sex),
          test_msg_xvar_oob(arg_x_name, len_x, num_to_replace3, "ig_nbs")
        )

        # Bad input 5: Non-"M"/"F" `sex` variables -----------------------------
        expect_warning(
          gigs_fn(arg_zyp,
                  arg_x,
                  replace(arg_sex, replace_ints_2, "invalid_sex")),
          test_msg_acronym_sex_invalid("sex", len_x, num_to_replace2, "ig_nbs")
        )
        expect_warning(
          gigs_fn(arg_zyp,
                  arg_x,
                  replace(arg_sex, replace_ints_2, "invalid_sex")),
          test_msg_acronym_sex_invalid("sex", len_x, num_to_replace2, "ig_nbs")
        )
      }

      fn_name <- paste("ig_nbs", fn_suffix, sep = "_")
      gigs_fn <- get(fn_name)
      arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                             zscore2value = "z", "y")
      arg_acronym <- rep_len(str_acronym, len_x)
      arg_x_name <- "gest_days"

      # Bad input 1: Undefined data (NaN, Inf, -Inf) ---------------------------
      for (undefined_val in c(NaN, Inf, -Inf)) {
        ## Replace z/y/p variable and x variable with undefined_val
        warnings <- capture_warnings(
          gigs_fn(replace(arg_zyp, replace_ints_3, undefined_val),
                  replace(arg_x, replace_ints_2, undefined_val),
                  arg_sex,
                  arg_acronym)
        )
        expect_match(warnings[[1]],
                     test_msg_undefined(arg_zyp_name, len_x, num_to_replace3))
        expect_match(warnings[[2]],
                     test_msg_undefined(arg_x_name, len_x, num_to_replace2))
      }

      # Bad input 2: Missing data (NA) -----------------------------------------
      ## Replace z/y/p variable and acronym variable with NA
      warnings <- capture_warnings(
        gigs_fn(arg_zyp,
                arg_x,
                replace(arg_sex, replace_ints_2, values = NA),
                replace(arg_acronym, replace_ints_1, NA))
      )
      expect_match(warnings[[2]],
                   test_msg_missing("sex", len_x, num_to_replace2))
      expect_match(warnings[[1]],
                   test_msg_missing("acronym", len_x, num_to_replace1))

      # Bad input 3: Out of bounds centiles (not `> 0` and `< 1`) --------------
      if (fn_suffix == "centile2value") {
          expect_warning(
            gigs_fn(replace(arg_zyp, replace_ints_3, values = 2.5),
                    arg_x,
                    arg_sex,
                    arg_acronym),
            test_msg_centile_oob(len_x, num_to_replace3)
          )
          expect_warning(
            gigs_fn(replace(arg_zyp, replace_ints_2, values = 2.5),
                    arg_x,
                    arg_sex,
                    arg_acronym),
            test_msg_centile_oob(len_x, num_to_replace2)
          )
      }

      # Bad input 4: Out of bounds `x` variable --------------------------------
      expect_warning(
        gigs_fn(arg_zyp,
                replace(arg_x, replace_ints_3, out_of_bounds),
                arg_sex,
                arg_acronym),
        test_msg_xvar_oob(arg_x_name, len_x, num_to_replace3, "ig_nbs")
      )
      expect_warning(
        gigs_fn(arg_zyp,
                replace(arg_x, replace_ints_1, below_lower_bound),
                arg_sex,
                arg_acronym),
        test_msg_xvar_oob(arg_x_name, len_x, num_to_replace1, "ig_nbs")
      )

      # Bad input 5: Non-"M"/"F" `sex` variables -----------------------------
      expect_warning(
        gigs_fn(arg_zyp,
                arg_x,
                replace(arg_sex, replace_ints_2, "m"),
                arg_acronym),
        test_msg_acronym_sex_invalid("sex", len_x, num_to_replace2, "ig_nbs")
      )
      expect_warning(
        gigs_fn(arg_zyp,
                arg_x,
                replace(arg_sex, replace_ints_3, "f"),
                arg_acronym),
        test_msg_acronym_sex_invalid("sex", len_x, num_to_replace3, "ig_nbs")
      )

      # Bad input 6: Invalid `acronym` values (not in names(gigs::ig_nbs))
      expect_warning(
        gigs_fn(arg_zyp,
                arg_x,
                arg_sex,
                replace(arg_acronym, replace_ints_2, "invalid")),
        test_msg_acronym_sex_invalid("acronym", len_x, num_to_replace2,
                                     "ig_nbs")
      )
      expect_warning(
        gigs_fn(arg_zyp,
                arg_x,
                arg_sex,
                replace(arg_acronym, replace_ints_3, "invalid")),
        test_msg_acronym_sex_invalid("acronym", len_x, num_to_replace3,
                                     "ig_nbs")
      )
    }
  }
)

#' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
#' @srrstats{G5.8, G5.8d} Show that gigs can handle data outside the
#'   scope of its algorithms by replacing bad inputs with `NA` and giving clear
#'   errors.
#' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
#' @srrstats{G5.8, G5.8d} Show that gigs can handle data outside the
#'   scope of its algorithms by replacing bad inputs with `NA` and giving clear
#'   warnings.
test_that(
  desc = "Invalid values can be replaced with `NA` with errors",
  code = {
    # Make gigs stop() with bad data
    for (option in names(.gigs_options)) {
      gigs_option_set(option, new_value = "error", silent = TRUE)
    }

    acronyms_to_test <- names(ig_nbs)[5]
    fn_suffixes <- c("value2zscore", "value2centile", "centile2value",
                     "zscore2value")
    z <- 0
    p <- 0.5

    # Random set of indices to replace with bad data in each case
    num_to_replace1 <- 6
    withr::with_seed(200, code = {
      replace_ints_1 <- sample(1L:20L, size = num_to_replace1)
    })
    num_to_replace2 <- 5
    withr::with_seed(200, code = {
      replace_ints_2 <- sample(1L:20L, size = num_to_replace2)
    })
    replace_ints_3 <- union(replace_ints_1, replace_ints_2)
    num_to_replace3 <- length(union(replace_ints_1, replace_ints_2))

    for (fn_suffix in fn_suffixes) {
      for (str_acronym in acronyms_to_test) {
        fn_name <- paste("ig_nbs", str_acronym, fn_suffix, sep = "_")
        gigs_fn <- get(fn_name)
        x <- gigs::ig_nbs[[str_acronym]][[1]][[1]][[1]]
        len_x <- length(x) * 4
        arg_x_name <- gigs::ig_nbs[[str_acronym]][["x"]]
        y_name <- gigs::ig_nbs[[str_acronym]][["y"]]

        arg_zyp <- rep_len(switch(fn_suffix,
                                  centile2value = p,
                                  zscore2value = z,
                                  gigs::ig_nbs[[str_acronym]][[1]][[1]][[3]]),
                           len_x)
        arg_zyp_name <- switch(fn_suffix,
                               centile2value = "p",
                               zscore2value = "z",
                               y_name)
        arg_x <- rep_len(x, len_x)
        arg_sex <- withr::with_seed(25, {
          sample(c("M", "F"), replace = TRUE, size = len_x)
        })

        # Bad input 1: Undefined data (NaN, Inf, -Inf) -------------------------
        for (undefined_val in c(NaN, Inf, -Inf)) {
          ## Replace z/y/p variable and x variable with undefined_val
          expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_2, undefined_val),
                    replace(arg_x, replace_ints_3, undefined_val),
                    arg_sex),
            test_msg_undefined(arg_zyp_name, len_x, num_to_replace2)
          )
        }

        # Bad input 2: Missing data (NA) ---------------------------------------
        ## Replace only x variable with NA
        expect_error(
          gigs_fn(arg_zyp,
                  replace(arg_x, replace_ints_3, values = NA),
                  arg_sex),
          test_msg_missing(arg_x_name, len_x, num_to_replace3)
        )

        ## Replace z/y/p variable, x and sex variables with NA
        expect_error(
          gigs_fn(replace(arg_zyp, replace_ints_2, values = NA),
                  replace(arg_x, replace_ints_3, values = NA),
                  replace(arg_sex, replace_ints_1, values = NA)),
          test_msg_missing(arg_zyp_name, len_x, num_to_replace2)
        )

        # Bad input 3: Out of bounds centiles (not `> 0` and `< 1`) ------------
        if (fn_suffix == "centile2value") {
          expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_1, values = 2.5),
                    arg_x,
                    arg_sex),
            test_msg_centile_oob(len_x, num_to_replace1)
          )
          expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_2, values = 2.5),
                    arg_x,
                    arg_sex),
            test_msg_centile_oob(len_x, num_to_replace2)
          )
        }

        # Bad input 4: Out of bounds `x` variable ------------------------------
        below_lower_bound <- min(x) - replace_ints_1
        above_upper_bound <- max(x) + replace_ints_2
        out_of_bounds <-withr::with_seed(500, code = {
          sample(c(below_lower_bound, above_upper_bound),
                 size = num_to_replace3, replace = FALSE)
        })
        expect_error(
          gigs_fn(arg_zyp, replace(arg_x, replace_ints_2, above_upper_bound), arg_sex),
          test_msg_xvar_oob(arg_x_name, len_x, num_to_replace2, "ig_nbs")
        )
        expect_error(
          gigs_fn(arg_zyp, replace(arg_x, replace_ints_3, out_of_bounds), arg_sex),
          test_msg_xvar_oob(arg_x_name, len_x, num_to_replace3, "ig_nbs")
        )

        # Bad input 5: Non-"M"/"F" `sex` variables -----------------------------
        expect_error(
          gigs_fn(arg_zyp,
                  arg_x,
                  replace(arg_sex, replace_ints_2, "invalid_sex")),
          test_msg_acronym_sex_invalid("sex", len_x, num_to_replace2, "ig_nbs")
        )
        expect_error(
          gigs_fn(arg_zyp,
                  arg_x,
                  replace(arg_sex, replace_ints_2, "invalid_sex")),
          test_msg_acronym_sex_invalid("sex", len_x, num_to_replace2, "ig_nbs")
        )
      }

      fn_name <- paste("ig_nbs", fn_suffix, sep = "_")
      gigs_fn <- get(fn_name)
      arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                             zscore2value = "z", "y")
      arg_acronym <- rep_len(str_acronym, len_x)
      arg_x_name <- "gest_days"

      # Bad input 1: Undefined data (NaN, Inf, -Inf) ---------------------------
      for (undefined_val in c(NaN, Inf, -Inf)) {
        ## Replace z/y/p variable and x variable with undefined_val
        expect_error(
          gigs_fn(replace(arg_zyp, replace_ints_3, undefined_val),
                  replace(arg_x, replace_ints_2, undefined_val),
                  arg_sex,
                  arg_acronym),
          test_msg_undefined(arg_zyp_name, len_x, num_to_replace3)
        )
      }

      # Bad input 2: Missing data (NA) -----------------------------------------
      ## Replace z/y/p variable and acronym variable with NA
      expect_error(
        gigs_fn(arg_zyp,
                arg_x,
                replace(arg_sex, replace_ints_2, values = NA),
                replace(arg_acronym, replace_ints_1, NA)),
        test_msg_missing("acronym", len_x, num_to_replace1)
      )

      # Bad input 3: Out of bounds centiles (not `> 0` and `< 1`) --------------
      if (fn_suffix == "centile2value") {
          expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_3, values = 2.5),
                    arg_x,
                    arg_sex,
                    arg_acronym),
            test_msg_centile_oob(len_x, num_to_replace3)
          )
          expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_2, values = 2.5),
                    arg_x,
                    arg_sex,
                    arg_acronym),
            test_msg_centile_oob(len_x, num_to_replace2)
          )
      }

      # Bad input 4: Out of bounds `x` variable --------------------------------
      expect_error(
        gigs_fn(arg_zyp,
                replace(arg_x, replace_ints_3, out_of_bounds),
                arg_sex,
                arg_acronym),
        test_msg_xvar_oob(arg_x_name, len_x, num_to_replace3, "ig_nbs")
      )
      expect_error(
        gigs_fn(arg_zyp,
                replace(arg_x, replace_ints_1, below_lower_bound),
                arg_sex,
                arg_acronym),
        test_msg_xvar_oob(arg_x_name, len_x, num_to_replace1, "ig_nbs")
      )

      # Bad input 5: Non-"M"/"F" `sex` variables -----------------------------
      expect_error(
        gigs_fn(arg_zyp,
                arg_x,
                replace(arg_sex, replace_ints_2, "m"),
                arg_acronym),
        test_msg_acronym_sex_invalid("sex", len_x, num_to_replace2, "ig_nbs")
      )
      expect_error(
        gigs_fn(arg_zyp,
                arg_x,
                replace(arg_sex, replace_ints_3, "f"),
                arg_acronym),
        test_msg_acronym_sex_invalid("sex", len_x, num_to_replace3, "ig_nbs")
      )

      # Bad input 6: Invalid `acronym` values (not in names(gigs::ig_nbs))
      expect_error(
        gigs_fn(arg_zyp,
                arg_x,
                arg_sex,
                replace(arg_acronym, replace_ints_2, "invalid")),
        test_msg_acronym_sex_invalid("acronym", len_x, num_to_replace2,
                                     "ig_nbs")
      )
      expect_error(
        gigs_fn(arg_zyp,
                arg_x,
                arg_sex,
                replace(arg_acronym, replace_ints_3, "invalid")),
        test_msg_acronym_sex_invalid("acronym", len_x, num_to_replace3,
                                     "ig_nbs")
      )
    }
  }
)

#' @srrstats {G5.8c} Show how the ig_nbs functions handles all-`NA` inputs.
test_that(
  desc = "Test that 'ig_nbs' functions can handle all-`NA` inputs",
  code = {
    # Make gigs error if confronted with any bad data
    for (option in names(.gigs_options)) {
      gigs_option_set(option, new_value = "warn", silent = TRUE)
    }

    fn_suffixes <- c("value2zscore", "value2centile", "centile2value",
                     "zscore2value")
    z <- 0
    p <- 0.5

    for (fn_suffix in fn_suffixes) {
      fn_name <- paste("ig_nbs", fn_suffix, sep = "_")
      gigs_fn <- get(fn_name)
      for (seed in c(125, 175, 225)) {
        str_acronym <- withr::with_seed(seed, code = {
          sample(names(gigs::ig_nbs), size = 1)
        })
        x <- gigs::ig_nbs[[str_acronym]][[1]][[1]][[1]]
        len_x <- length(x)
        arg_zyp <- rep_len(switch(fn_suffix,
                                  centile2value = p,
                                  zscore2value = z,
                                  gigs::ig_nbs[[str_acronym]][[1]][[1]][[2]]),
                           len_x)
        arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                               zscore2value = "z", "y")
        arg_x <- x
        arg_acronym <- rep_len(str_acronym, len_x)
        arg_sex <- withr::with_seed(seed,
          sample(c("M", "F"), len_x, replace = TRUE)
        )
        all_NA <- rep_len(NA, len_x)

        # Will produce all NAs for `y` argument, with a warning
        expect_warning(
          gigs_fn(all_NA, arg_x, arg_sex, arg_acronym),
          test_msg_missing(name = arg_zyp_name, len_x, len_x)
        )
        # Will produce all NAs for the `x` argument, with a warning
        expect_warning(
          gigs_fn(arg_zyp, all_NA, arg_sex, arg_acronym),
          test_msg_missing(name = "gest_days", len_x, len_x)
        )
        # Will produce all NAs for the `sex` argument, with a warning
        expect_warning(
          gigs_fn(arg_zyp, arg_x, all_NA, arg_acronym),
          test_msg_missing(name = "sex", len_x, len_x)
        )
        # Will throw an error if all elements of `acronym` are missing
        expect_error(
          gigs_fn(arg_zyp, arg_x, arg_sex, all_NA),
          regexp = "Variable 'acronym': All elements were missing \\(`NA`\\)."
        )
        # All invalid `acronym`s will also produce an error
        expect_error(
          gigs_fn(arg_zyp,
                  arg_x,
                  arg_sex,
                  rep_len("not valid acronym!! oh no!", len_x)),
          regexp = paste0("Variable 'acronym': All elements were invalid. ",
                          "See the 'ig_nbs' documentation for valid 'acronym' ",
                          "values.")
        )

        # `NULL` z/y/p variable causes error
        expect_error(
          gigs_fn(NULL, arg_x, arg_sex, arg_acronym),
          paste0("Your 'y'/'z'/'p' argument was `NULL`. Ensure it is not ",
                 "`NULL`, then try again.")
        )
      }
    }
  }
)
