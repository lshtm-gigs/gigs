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
          set.seed(seed = seed)
          dbl_z_or_p <- rnorm(n = length(xvar))
          set.seed(seed = seed)
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
        set.seed(seed = 50)
        dbl_z_or_p <- rnorm(n = length(xvar))
        set.seed(seed = 50)
        sexvar <- sample(c("M", "F"), size = length(xvar), replace = TRUE)
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
  desc = "Correct errors occur when .gigs_options are set to 'quiet':",
  code = {
    #' Generate an expected bad data type error from `{checkmate}`
    #' @param name Single-length character vector with name of input.
    #' @param wanted Single-length character vector with name of expected data
    #'   type.
    #' @param got Single-length character vector with name of received data
    #'   type.
    #' @description Used to test `{checkmate}` error messages. These errors are
    #'   given by checkmate::assert_*()-style functions in check_params.R.
    error_msg_wrong_type <- function(name, wanted, got) {
      paste0("Assertion on '", name, "' failed: Must be of type '", wanted,
             "', not '", got, "'.")
    }

    #' Generate an expected zero-length error from [validate_parameter_lengths]
    #' @param name Name of input vector.
    error_msg_zero_length <- function(names) {
      count <- length(names)
      var_str <- if(count > 1) "Variables" else "Variable"
      input_str <- if(count > 1) "Inputs" else "Input"
      varnames_str <- paste0(names, collapse = "', '")
      paste0(var_str, " '", varnames_str, "': ", input_str, " had length 0, ",
             "but must have length 1 or greater.")
    }

    #' Generate an expected zero-length error from `{checkmate}`
    #' @param name Name of input vector.
    error_msg_unrecyclable <- function(names) {
      count <- length(names)
      var_str <- if(count > 1) "Variables" else "Variable"
      input_str <- if(count > 1) "These inputs" else "This input"
      varname_str <- paste0(names, collapse = "', '")
      paste0(var_str, " '", varname_str, "': ", input_str, " cannot be ",
             "recycled with `vctrs\\:\\:vec_recycle_common\\(\\)`.")
    }

    # Error message testing: Iterate through ig_nbs functions, demonstrating
    #   with passed `expect_error()` tests that all error messages are unique
    #   to the data passed in. This for-loop only goes through the four main
    #   exported conversion functions, but as these are called by the
    #   standard-specific conversion functions we only need to demonstrate
    #   that they do their job.
    fn_suffixes <- c("value2zscore", "value2centile", "centile2value",
                     "zscore2value")
    z <- 0
    p <- 0.5
    y <- 4
    for (fn_suffix in fn_suffixes) {
      fn_name <- paste("ig_nbs", fn_suffix, sep = "_")
      gigs_fn <- get(fn_name)

      # Sample three random acronyms --> doing all twenty balloons the runtime
      for (seed in c(150, 200, 250)) {
        set.seed(seed)
        acronym <- sample(names(gigs::ig_nbs), size = 1)
        x <- gigs::ig_nbs[[acronym]][[1]][[1]][[1]]

        arg_zyp <- switch(fn_suffix, centile2value = p, zscore2value = z, y)
        arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                               zscore2value = "z", "y")
        arg_x <- x
        set.seed(seed)
        arg_sex <- sample(c("M", "F"), size = 1)
        arg_acronym <- acronym

        # G5.8a --> zero-length data inputs cause errors
        expect_error(object = gigs_fn(double(), arg_x, arg_sex, arg_acronym),
                     regexp = error_msg_zero_length(arg_zyp_name))
        expect_error(object = gigs_fn(arg_zyp, double(), arg_sex, arg_acronym),
                     regexp = error_msg_zero_length("gest_days"))
        expect_error(object = gigs_fn(double(), arg_x, character(),
                                      arg_acronym),
                     regexp = error_msg_zero_length(c(arg_zyp_name, "sex")))
        expect_error(object = gigs_fn(double(), arg_x, arg_sex, character()),
                     regexp = error_msg_zero_length(c(arg_zyp_name, "acronym")))

        # G5.8b --> incorrect data types
        expect_error(
          object = gigs_fn(as.character(arg_zyp), arg_x, arg_sex, arg_acronym),
          regexp = error_msg_wrong_type(arg_zyp_name, "numeric", "character")
        )
        expect_error(
          object = gigs_fn(arg_zyp, TRUE, arg_sex, arg_acronym),
          regexp = error_msg_wrong_type("x", "numeric", "logical")
        )
        expect_error(
          object = gigs_fn(arg_zyp, arg_x, 50L, arg_acronym),
          regexp = error_msg_wrong_type("sex", "character", "integer")
        )
        expect_error(
          object = gigs_fn(arg_zyp, arg_x, arg_sex, complex(length.out = 1)),
          regexp = error_msg_wrong_type("acronym", "character", "complex")
        )

        # G5.2, --> errors if vector inputs cannot be recycled
        expect_error(
          object = gigs_fn(rep(arg_zyp, 2), arg_x, arg_sex, arg_acronym),
          regexp = error_msg_unrecyclable(names = arg_zyp_name)
        )
        expect_error(
          object = gigs_fn(arg_zyp, arg_x, arg_sex, rep(arg_acronym, 2)),
          regexp = error_msg_unrecyclable(names = "acronym")
        )
        expect_error(
          object = gigs_fn(arg_zyp, rep(arg_x, 2), arg_sex, rep(arg_acronym, 2)),
          regexp = error_msg_unrecyclable("acronym")
        )
        expect_error(
          object = gigs_fn(arg_zyp, rep(arg_x, 2), rep(arg_sex, 2), arg_acronym),
          regexp = error_msg_unrecyclable("sex")
        )
      }
    }
  }
)

# Appropriate handling of non-fatal issues with input data ---------------------

#' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
#' @srrstats{G5.8, G5.8d} Show that gigs can handle data outside the
#'   scope of its algorithms by replacing bad inputs with `NA`, and simply
#'   outputting `NA` for the output elements which map onto invalid input
#'   elements.
#' @srrstats {EA6.0, EA6.0a} Also checks that classes/types of returned objects
#'   are correct using checkmate's expect_numeric() function.
test_that(
  desc = "Invalid values can be replaced with `NA` quietly",
  code = {
    # Make gigs *quietly* replace bad data with NA
    for (option in names(.gigs_options)) {
      gigs_option_set(option, new_value = "quiet", silent = TRUE)
    }

    fn_suffixes <- c("value2zscore", "value2centile", "centile2value",
                     "zscore2value")
    z <- 0
    p <- 0.5

    # Random set of indices to replace with bad data in each case
    num_to_replace1 <- 6
    set.seed(200)
    replace_ints_1 <- sample(1L:17L, size = num_to_replace1, replace = FALSE)
    num_to_replace2 <- 5
    set.seed(300)
    replace_ints_2 <- sample(1L:17L, size = num_to_replace2, replace = FALSE)
    replace_ints_3 <- union(replace_ints_1, replace_ints_2)
    num_to_replace3 <- length(union(replace_ints_1, replace_ints_2))
    for (fn_suffix in fn_suffixes) {
      fn_name <- paste("ig_nbs", fn_suffix, sep = "_")
      gigs_fn <- get(fn_name)
      # Sample three random acronyms --> doing all twenty explodes the runtime
      for (seed in c(105, 155, 205)) {
        set.seed(seed)
        acronym <- sample(names(gigs::ig_nbs), size = 1)
        x_range <- range(gigs::ig_nbs[[acronym]][[1]][[1]][[1]])
        x <- seq(x_range[1], x_range[2], by = (x_range[2] - x_range[1]) / 50)
        len_x <- length(x)
        arg_zyp <- switch(fn_suffix,
                          centile2value = rep_len(p, len_x),
                          zscore2value = rep_len(z, len_x),
                          rep_len(gigs::ig_nbs[[acronym]][[1]][[1]][[1]], len_x))
        arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                               zscore2value = "z", "y")
        arg_x <- x
        set.seed(seed)
        arg_sex <- sample(c("M", "F"), size = len_x, replace = TRUE)
        arg_acronym <- rep_len(acronym, len_x)

        # Bad input 1: Undefined data (NaN, Inf, -Inf) -------------------------

        for (undefined_val in c(NaN, Inf, -Inf)) {
          ## Replace z/y/p variable with undefined_val
          out <- gigs_fn(replace(arg_zyp, replace_ints_1, values = undefined_val),
                         arg_x, arg_sex, arg_acronym)
          checkmate::expect_numeric(out, len = len_x)
          expect_true(all(!is.nan(out[replace_ints_1]) &
                            is.na(out[replace_ints_1])))

          ## Replace x variable with undefined_val
          out <- gigs_fn(arg_zyp,
                         replace(arg_x, replace_ints_2, values = undefined_val),
                         arg_sex, arg_acronym)
          checkmate::expect_numeric(out, len = len_x)
          expect_true(all(!is.nan(out[replace_ints_2]) &
                            is.na(out[replace_ints_2])))

          ## Replace z/y/p variable and x variable with undefined_val
          out <- gigs_fn(replace(arg_zyp, replace_ints_1, values = undefined_val),
                         replace(arg_x, replace_ints_2, values = undefined_val),
                         arg_sex, arg_acronym)
          checkmate::expect_numeric(out, len = len_x)
          expect_true(all(!is.nan(out[replace_ints_3]) &
                            is.na(out[replace_ints_3])))
        }


        # Bad input 2: Missing data (NA) ---------------------------------------
        ## Replace z/y/p variable with NA
        out <- gigs_fn(replace(arg_zyp, replace_ints_1, values = NA),
                       arg_x, arg_sex, arg_acronym)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_1])))

        ## Replace x variable with NA
        out <- gigs_fn(arg_zyp,
                       replace(arg_x, replace_ints_2, values = NA),
                       arg_sex, arg_acronym)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_2])))

        ## Replace z/y/p variable and x variable with NA
        out <- gigs_fn(replace(arg_zyp, replace_ints_1, values = NA),
                       replace(arg_x, replace_ints_2, values = NA),
                       arg_sex, arg_acronym)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_3])))

        ## Replace z/y/p variable and acronym variable with NA
        out <- gigs_fn(replace(arg_zyp, replace_ints_1, values = NA),
                       arg_x,
                       replace(arg_sex, replace_ints_2, values = NA),
                       arg_acronym)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_3])))

        ## Replace just sex variable with NA
        out <- gigs_fn(arg_zyp, arg_x, arg_sex,
                       replace(arg_acronym, replace_ints_3, values = NA))
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
        set.seed(500)
        out_of_bounds <- sample(c(below_lower_bound, above_upper_bound),
                                size = num_to_replace3, replace = FALSE)

        out <- gigs_fn(arg_zyp,
                       replace(arg_x, replace_ints_2, above_upper_bound),
                       arg_sex, arg_acronym)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_2])))

        out <- gigs_fn(arg_zyp,
                       replace(arg_x, replace_ints_3, out_of_bounds),
                       arg_sex, arg_acronym)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_3])))

        # Bad input 5: Invalid `acronym` values (not in names(gigs::ig_nbs))
        out <- gigs_fn(arg_zyp,
                       arg_x,
                       replace(arg_sex, replace_ints_1,
                               as.character(seq_len(num_to_replace1))),
                       arg_acronym)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_1])))

        out <- gigs_fn(arg_zyp,
                       arg_x,
                       replace(arg_sex,
                               replace_ints_2,
                               as.character(seq_len(num_to_replace2))),
                       arg_acronym)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_2])))

        # Bad input 6: Invalid `sex` values (not in c("M", "F"))
        out <- gigs_fn(arg_zyp, arg_x, arg_sex,
                       replace(arg_acronym,
                               replace_ints_1,
                               as.character(seq_len(num_to_replace1))))
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_1])))

        out <- gigs_fn(arg_zyp, arg_x, arg_sex,
                       replace(arg_acronym,
                               replace_ints_2,
                               as.character(seq_len(num_to_replace2))))
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_2])))
      }
    }
  }
)

# Appropriate warnings/errors with bad input data ------------------------------

#' Replicate error/warning messages from gigs for bad input data (TESTING
#' ONLY)
#' @param name Single-length character vector with a variable name.
#' @param length Expected length of the variable specified by `name`.
#' @param int_undefined Number of elements in `name` which were expected to
#'   be undefined by whatever test is calling this function.
#' @note Used for testing only. Mimics the output of [msg_undefined_data()].
msg_undefined <- function(name, length, int_undefined) {
  paste0("Variable '", name, "': ", int_undefined,
         " in ", length, " elements were undefined \\(`NaN`,",
         " `Inf`, or `-Inf`\\).")
}

#' Replicate error/warning messages from gigs for bad input data (TESTING
#' ONLY)
#' @param name Single-length character vector with a variable name.
#' @param length Expected length of the variable specified by `name`.
#' @param int_undefined Number of elements in `name` which were expected to
#'   be missing in whatever test is calling this function.
#' @note Used for testing only. Mimics the output of [msg_missing_data()].
msg_missing <- function(name, length, int_undefined) {
  paste0("Variable '", name, "': ", int_undefined,
         " in ", length, " elements were missing \\(`NA`\\).")
}

#' Replicate error/warning messages from gigs for bad input data (TESTING
#' ONLY)
#' @param name Single-length character vector with a variable name.
#' @param length Expected length of the variable specified by `name`.
#' @param int_undefined Number of elements in `p` which were expected to
#'   be outside the proper bounds (0 and 1) in whatever test is calling this
#'   function.
#' @note Used for testing only. Mimics the output of [msg_oob_centiles()].
msg_centile_oob <- function(length, int_undefined) {
  paste0("Variable 'p': ", int_undefined, " in ", length,
         " elements were not between 0 and 1.")
}

#' Replicate error/warning messages from gigs for bad input data (TESTING
#' ONLY)
#' @param name Single-length character vector with a variable name.
#' @param length Expected length of the variable specified by `name`.
#' @param int_undefined Number of elements in `p` which were expected to
#'   be outside the proper bounds (0 and 1) in whatever test is calling this
#'   function.
#' @note Used for testing only. Mimics the output of [msg_oob_centiles()].
msg_xvar_oob <- function(length, int_undefined) {
  paste0("Variable 'x': ", int_undefined, " in ", length, " elements were ",
         "out-of-bounds \\(see the 'ig_nbs' conversion functions ",
         "documentation\\).")
}

#' Replicate error/warning messages from gigs for bad input data (TESTING
#' ONLY)
#' @param var Single-length character vector with a variable name, either
#'   `"sex"` or `"acronym"`.
#' @param length Expected length of the variable specified by `name`.
#' @param int_undefined Number of elements in `x` which were expected to
#'   be invalid (not in `names(gigs::ig_nbs)`) in whatever test is calling this
#'   function.
#' @note Used for testing only. Mimics the output of
#'   [msg_invalid_sex_acronym()].
#' @noRd
msg_acronym_sex_invalid <- function(var, length, int_undefined) {
  see_sentence <- paste0("See the 'ig_nbs' documentation for valid ",
                           "'acronym' values.")
  if (var == "sex") {
    paste0("Variable 'sex': ", int_undefined, " in ", length, " elements were ",
           "neither \"M\" nor \"F\".")
  } else {
    paste0("Variable '", var,"': ", int_undefined, " in ", length,
           " elements were invalid. ", see_sentence)
  }
}

#' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
#' @srrstats{G5.8, G5.8d} Show that gigs can handle data outside the
#'   scope of its algorithms by replacing bad inputs with `NA` and giving clear
#'   warnings.
test_that(
  desc = "Invalid values can be replaced with `NA` with warnings",
  code = {
    # Make gigs *noisily* replace bad data with NA
    for (option in names(.gigs_options)) {
      gigs_option_set(option, new_value = "warn", silent = TRUE)
    }

    fn_suffixes <- c("value2zscore", "value2centile", "centile2value",
                     "zscore2value")
    z <- 0
    p <- 0.5

    # Random set of indices to replace with bad data in each case
    num_to_replace1 <- 6
    set.seed(200)
    replace_ints_1 <- sample(1L:17L, size = num_to_replace1, replace = FALSE)
    num_to_replace2 <- 5
    set.seed(300)
    replace_ints_2 <- sample(1L:17L, size = num_to_replace2, replace = FALSE)
    replace_ints_3 <- union(replace_ints_1, replace_ints_2)
    num_to_replace3 <- length(union(replace_ints_1, replace_ints_2))

    for (fn_suffix in fn_suffixes) {
      fn_name <- paste("ig_nbs", fn_suffix, sep = "_")
      gigs_fn <- get(fn_name)
      # Sample three random acronyms --> doing all twenty balloons the runtime
      for (seed in c(125, 175, 225)) {
        set.seed(seed)
        acronym <- sample(names(gigs::ig_nbs), size = 1)
        x_range <- range(gigs::ig_nbs[[acronym]][[1]][[1]][[1]])
        x <- seq(x_range[1], x_range[2], by = (x_range[2] - x_range[1]) / 50)
        len_x <- length(x)
        arg_zyp <- switch(fn_suffix,
                          centile2value = rep_len(p, len_x),
                          zscore2value = rep_len(z, len_x),
                          rep_len(gigs::ig_nbs[[acronym]][[1]][[1]][[1]],
                                  len_x))
        arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                               zscore2value = "z", "y")
        arg_x <- x
        set.seed(seed)
        arg_sex <- sample(c("M", "F"), size = len_x, replace = TRUE)
        arg_acronym <- rep_len(acronym, len_x)

        # Bad input 1: Undefined data (NaN, Inf, -Inf) -------------------------
        for (undefined_val in c(NaN, Inf, -Inf)) {
          ## Replace z/y/p variable with undefined_val
          expect_warning(
            gigs_fn(replace(arg_zyp, replace_ints_1, values = undefined_val),
                    arg_x, arg_sex, arg_acronym),
            regexp = msg_undefined(arg_zyp_name, length = len_x,
                                   int_undefined = num_to_replace1)
          )

          ## Replace x variable with undefined_val
          expect_warning(
            gigs_fn(arg_zyp,
                    replace(arg_x, replace_ints_2, values = undefined_val),
                    arg_sex, arg_acronym),
            regexp = msg_undefined("x", length = len_x,
                                   int_undefined = num_to_replace2)
          )

          ## Replace z/y/p variable and x variable with undefined_val
          warnings <- testthat::capture_warnings(
            gigs_fn(replace(arg_zyp, replace_ints_1, values = undefined_val),
                    replace(arg_x, replace_ints_2, values = undefined_val),
                    arg_sex,
                    arg_acronym)
          )
          expect_match(warnings[1], msg_undefined(arg_zyp_name, len_x,
                                                  num_to_replace1))
          expect_match(warnings[2], msg_undefined("x", len_x,
                                                  num_to_replace2))
        }

        # Bad input 2: Missing data (NA) ---------------------------------------
        ## Replace z/y/p variable with NA
        expect_warning(
          gigs_fn(replace(arg_zyp, replace_ints_1, NA), arg_x,
                  arg_sex, arg_acronym),
          regexp = msg_missing(arg_zyp_name, len_x, num_to_replace1)
        )

        ## Replace x variable with NA
        expect_warning(
          gigs_fn(arg_zyp, replace(arg_x, replace_ints_2, NA), arg_sex, arg_acronym),
          regexp = msg_missing("x", len_x, num_to_replace2)
        )

        ## Replace z/y/p variable and acronym variable with NA
        warnings <- testthat::capture_warnings(
            gigs_fn(replace(arg_zyp, replace_ints_1, values = NA),
                    arg_x,
                    replace(arg_sex, replace_ints_3, values = NA),
                    arg_acronym)
        )
        expect_match(warnings[1], msg_missing(arg_zyp_name, len_x,
                                              num_to_replace1))
        expect_match(warnings[2], msg_missing("sex", len_x,
                                                num_to_replace3))

        # Bad input 3: Out of bounds centiles (not `> 0` and `< 1`) ------------
        if (fn_suffix == "centile2value") {
          expect_warning(
            gigs_fn(replace(arg_zyp, replace_ints_1, values = 2.5), arg_x,
                    arg_sex, arg_acronym),
            msg_centile_oob(len_x, num_to_replace1)
          )
          expect_warning(
            gigs_fn(replace(arg_zyp, replace_ints_3, values = -.5), arg_x,
                    arg_sex, arg_acronym),
            msg_centile_oob(len_x, num_to_replace3)
          )
        }

        # Bad input 4: Out of bounds `x` variable ------------------------------
        below_lower_bound <- min(x) - replace_ints_1
        above_upper_bound <- max(x) + replace_ints_2
        set.seed(500)
        out_of_bounds <- sample(c(below_lower_bound, above_upper_bound),
                                size = num_to_replace3, replace = FALSE)

        expect_warning(
          gigs_fn(arg_zyp, replace(arg_x, replace_ints_2, above_upper_bound),
                  arg_sex, arg_acronym),
          msg_xvar_oob(len_x, num_to_replace2)
        )
        expect_warning(
          gigs_fn(arg_zyp, replace(arg_x, replace_ints_3, out_of_bounds),
                  arg_sex, arg_acronym),
          msg_xvar_oob(len_x, num_to_replace3)
        )

        # Bad input 5: Invalid `acronym` values (!%in% names(gigs::ig_nbs)) ----
        expect_warning(
          gigs_fn(arg_zyp, arg_x, arg_sex,
                  replace(arg_acronym, replace_ints_1,
                          as.character(seq_len(num_to_replace1)))),
          msg_acronym_sex_invalid(var = "acronym", len_x, num_to_replace1)
        )

        # Bad input 6: Invalid `sex` values (!%in% c("M", "F"")) ---------------
        expect_warning(
          gigs_fn(arg_zyp, arg_x,
                  replace(arg_sex, replace_ints_2,
                          as.character(seq_len(num_to_replace2))),
                  arg_acronym),
          msg_acronym_sex_invalid(var = "sex", len_x, num_to_replace2)
        )
      }
    }
  }
)

#' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
#' @srrstats{G5.8, G5.8c, G5.8d} Show that gigs can handle data outside the
#'   scope of its algorithms by replacing bad inputs with `NA` and giving clear
#'   warnings.
test_that(
  desc = "Invalid values can be replaced with `NA` with warnings",
  code = {
    # Make gigs *noisily* replace bad data with NA
    for (option in names(.gigs_options)) {
      gigs_option_set(option, new_value = "error", silent = TRUE)
    }

    fn_suffixes <- c("value2zscore", "value2centile", "centile2value",
                     "zscore2value")
    z <- 0
    p <- 0.5

    # Random set of indices to replace with bad data in each case
    num_to_replace1 <- 6
    set.seed(200)
    replace_ints_1 <- sample(1L:17L, size = num_to_replace1, replace = FALSE)
    num_to_replace2 <- 5
    set.seed(300)
    replace_ints_2 <- sample(1L:17L, size = num_to_replace2, replace = FALSE)
    replace_ints_3 <- union(replace_ints_1, replace_ints_2)
    num_to_replace3 <- length(union(replace_ints_1, replace_ints_2))

    for (fn_suffix in fn_suffixes) {
      fn_name <- paste("ig_nbs", fn_suffix, sep = "_")
      gigs_fn <- get(fn_name)
      # Sample three random acronyms --> doing all twenty balloons the runtime
      for (seed in c(125, 175, 225)) {
        set.seed(seed)
        acronym <- sample(names(gigs::ig_nbs), size = 1)
        x_range <- range(gigs::ig_nbs[[acronym]][[1]][[1]][[1]])
        x <- seq(x_range[1], x_range[2], by = (x_range[2] - x_range[1]) / 50)
        len_x <- length(x)
        arg_zyp <- switch(fn_suffix,
                          centile2value = rep_len(p, len_x),
                          zscore2value = rep_len(z, len_x),
                          rep_len(gigs::ig_nbs[[acronym]][[1]][[1]][[1]],
                                  len_x))
        arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                               zscore2value = "z", "y")
        arg_x <- x
        set.seed(seed)
        arg_sex <- sample(c("M", "F"), size = len_x, replace = TRUE)
        arg_acronym <- rep_len(acronym, len_x)

        # Bad input 1: Undefined data (NaN, Inf, -Inf) -------------------------
        for (undefined_val in c(NaN, Inf, -Inf)) {
          ## Replace z/y/p variable with undefined_val
          expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_1, values = undefined_val),
                    arg_x, arg_sex, arg_acronym),
            regexp = msg_undefined(arg_zyp_name, length = len_x,
                                   int_undefined = num_to_replace1)
          )

          ## Replace x variable with undefined_val
          expect_error(
            gigs_fn(arg_zyp,
                    replace(arg_x, replace_ints_2, values = undefined_val),
                    arg_sex, arg_acronym),
            regexp = msg_undefined("x", length = len_x,
                                   int_undefined = num_to_replace2)
          )

          ## Replace z/y/p variable and x variable with undefined_val
          expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_1, values = undefined_val),
                    replace(arg_x, replace_ints_2, values = undefined_val),
                    arg_sex,
                    arg_acronym),
            msg_undefined(arg_zyp_name, len_x, num_to_replace1)
          )
        }

        # Bad input 2: Missing data (NA) ---------------------------------------
        ## Replace z/y/p variable with NA
        expect_error(
          gigs_fn(replace(arg_zyp, replace_ints_1, NA), arg_x,
                  arg_sex, arg_acronym),
          regexp = msg_missing(arg_zyp_name, len_x, num_to_replace1)
        )

        ## Replace x variable with NA
        expect_error(
          gigs_fn(arg_zyp, replace(arg_x, replace_ints_2, NA), arg_sex, arg_acronym),
          regexp = msg_missing("x", len_x, num_to_replace2)
        )

        ## Replace z/y/p variable and acronym variable with NA
        warnings <- expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_1, values = NA),
                    arg_x,
                    replace(arg_sex, replace_ints_3, values = NA),
                    arg_acronym),
            msg_missing(arg_zyp_name, len_x, num_to_replace1)
        )

        # Bad input 3: Out of bounds centiles (not `> 0` and `< 1`) ------------
        if (fn_suffix == "centile2value") {
          expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_1, values = 2.5), arg_x,
                    arg_sex, arg_acronym),
            msg_centile_oob(len_x, num_to_replace1)
          )
          expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_3, values = -.5), arg_x,
                    arg_sex, arg_acronym),
            msg_centile_oob(len_x, num_to_replace3)
          )
        }

        # Bad input 4: Out of bounds `x` variable ------------------------------
        below_lower_bound <- min(x) - replace_ints_1
        above_upper_bound <- max(x) + replace_ints_2
        set.seed(500)
        out_of_bounds <- sample(c(below_lower_bound, above_upper_bound),
                                size = num_to_replace3, replace = FALSE)

        expect_error(
          gigs_fn(arg_zyp, replace(arg_x, replace_ints_2, above_upper_bound),
                  arg_sex, arg_acronym),
          msg_xvar_oob(len_x, num_to_replace2)
        )
        expect_error(
          gigs_fn(arg_zyp, replace(arg_x, replace_ints_3, out_of_bounds),
                  arg_sex, arg_acronym),
          msg_xvar_oob(len_x, num_to_replace3)
        )

        # Bad input 5: Invalid `acronym` values (!%in% names(gigs::ig_nbs)) ----
        expect_error(
          gigs_fn(arg_zyp, arg_x, arg_sex,
                  replace(arg_acronym, replace_ints_1,
                          as.character(seq_len(num_to_replace1)))),
          msg_acronym_sex_invalid(var = "acronym", len_x, num_to_replace1)
        )

        # Bad input 6: Invalid `sex` values (!%in% c("M", "F"")) ---------------
        expect_error(
          gigs_fn(arg_zyp, arg_x,
                  replace(arg_sex, replace_ints_2,
                          as.character(seq_len(num_to_replace2))),
                  arg_acronym),
          msg_acronym_sex_invalid(var = "sex", len_x, num_to_replace2)
        )
      }
    }
  }
)

#' @srrstats {G5.8c} Show how the ig_nbs functions handles all-`NA` inputs.
test_that(
  desc = "",
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
        set.seed(seed)
        acronym <- sample(names(gigs::ig_nbs), size = 1)
        x <- gigs::ig_nbs[[acronym]][[1]][[1]][[1]]
        len_x <- length(x)
        arg_zyp <- switch(fn_suffix,
                          centile2value = rep_len(p, len_x),
                          zscore2value = rep_len(z, len_x),
                          gigs::ig_nbs[[acronym]][[1]][[1]][[1]])
        arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                               zscore2value = "z", "y")
        arg_x <- x
        set.seed(seed)
        arg_sex <- sample(c("M", "F"), size = len_x, replace = TRUE)
        arg_acronym <- rep_len(acronym, len_x)
        all_NA <- rep(NA, len_x)

        # Will produce all NAs for `y` argument, with a warning
        expect_warning(
          gigs_fn(all_NA, arg_x, arg_sex, arg_acronym),
          msg_missing(name = arg_zyp_name, len_x, len_x)
        )
        # Will produce all NAs for the `x` argument
        expect_warning(
          gigs_fn(arg_zyp, all_NA, arg_sex, arg_acronym),
          msg_missing(name = "x", len_x, len_x)
        )
        # Will produce all NAs for the `sex` argument
        expect_warning(
          gigs_fn(arg_zyp, arg_x, all_NA, arg_acronym),
          msg_missing(name = "sex", len_x, len_x)
        )
        # Will produce an error with the `acronym` argument
        expect_error(
          gigs_fn(arg_zyp, arg_x, arg_sex, all_NA),
          regexp = "Variable 'acronym': All elements were missing \\(`NA`\\)."
        )
        # All invalid `acronym`s will also produce an error
        expect_error(
          gigs_fn(arg_zyp, arg_x, arg_sex, rep("not a valid acronym!!", len_x)),
          regexp = paste0("Variable 'acronym': All elements were invalid. ",
                          "See the 'ig_nbs' documentation for valid 'acronym' ",
                          "values.")
        )
      }
    }
  }
)