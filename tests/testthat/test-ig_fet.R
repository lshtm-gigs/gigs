# Correctness checks -----------------------------------------------------------

fet_zvals <- function(acronym) {
  switch(
    acronym,
    gafcrl = qnorm(c(0.03, 0.1, 0.5, 0.9, 0.97)),
    gwgfga = qnorm(c(0.03, 0.1, 0.25, 0.5, 0.75, 0.9, 0.97)),
    pifga = qnorm(c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97)),
    rifga = qnorm(c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97)),
    sdrfga = qnorm(c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97)),
    tcdfga = qnorm(c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97)),
    gaftcd = qnorm(c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97)),
    poffga = qnorm(c(0.03, 0.05, 0.5, 0.95, 0.97)),
    sffga = qnorm(c(0.03, 0.05, 0.5, 0.95, 0.97)),
    avfga = qnorm(c(0.03, 0.05, 0.5, 0.95, 0.97)),
    pvfga = qnorm(c(0.03, 0.05, 0.5, 0.95, 0.97)),
    cmfga = qnorm(c(0.03, 0.05, 0.5, 0.95, 0.97)),
    -3:3) # Where z-score tables are available
}

fet_pvals <- function(acronym) {
  switch(
    acronym,
    crlfga = c(0.03, 0.10, 0.25, 0.50, 0.75, 0.90, 0.97),
    gwgfga = c(0.03, 0.10, 0.25, 0.50, 0.75, 0.90, 0.97),
    gafcrl = c(0.03, 0.10, 0.50, 0.90, 0.97),
    poffga = c(0.03, 0.05, 0.5, 0.95, 0.97),
    sffga = c(0.03, 0.05, 0.5, 0.95, 0.97),
    avfga = c(0.03, 0.05, 0.5, 0.95, 0.97),
    pvfga = c(0.03, 0.05, 0.5, 0.95, 0.97),
    cmfga = c(0.03, 0.05, 0.5, 0.95, 0.97),
    c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97)
  )
}

fet_roundto <- function(acronym) {
  ifelse(test = acronym %in% c("pifga", "rifga", "sdrfga", "poffga", "sffga",
                               "avfga", "pvfga", "cmfga", "tcdfga", "gwgfga",
                               "gaftcd"),
         yes = 2,
         no = ifelse(acronym %in% c("efwfga", "gafcrl"), yes = 0, no = 1))
}


#' @srrstats {G5.4, G5.4c} Tests ensure that `gigs` functions can be used to
#'   replicate published growth charts, within a tolerance.
test_that(desc = "Conversion of z-scores/centiles to values works", {
  for (acronym in names(gigs::ig_fet)) {
    for (chr_z_or_p in c("zscore", "centile")) {
      ref_tbl <- gigs::ig_fet[[acronym]][[paste0(chr_z_or_p, "s")]]
      if (is.null(ref_tbl)) {
        ref_tbl <- gigs::ig_fet[[acronym]][[1]]
      }
      dbl_z_or_p <- switch(chr_z_or_p,
                           centile = fet_pvals(acronym),
                           zscore = fet_zvals(acronym))
      xvar <- ref_tbl[[1]]
      conv_fn <- get(paste0("ig_fet_", acronym, "_", chr_z_or_p, "2value"))
      pkg_tbl <- lapply(X = dbl_z_or_p,
                        FUN = \(zp) {
                          #' @srrstats {G5.5} Correctness test run with fixed
                          #'   random seed
                          set.seed(1000)
                          round(conv_fn(zp, xvar),
                                digits = fet_roundto(acronym))
                        }) |>
        do.call(what = cbind) |>
        as.data.frame() |>
        setNames(names(ref_tbl)[-1])
      col1_name <- names(ref_tbl)[1]
      pkg_tbl[[col1_name]] <- xvar
      pkg_tbl <- pkg_tbl[, c(ncol(pkg_tbl), 1:(ncol(pkg_tbl) - 1))]
      expect_equal(object = pkg_tbl, expected = ref_tbl, tolerance = 0.01,
                   ignore_attr = TRUE)
    }
  }
})

#' @srrstats {G5.5, G5.6, G5.6a, G5.6b, G5.9b} Checks that conversion
#'   functionality works when converting values to z-scores/centiles AND vice
#'   versa. Uses multiple fixed seeds to generate random inputs, which do not
#'   affect the functions' results.
test_that(
  desc = "Conversion of values to z-scores works",
  code = {
    for (acronym in names(gigs::ig_fet)) {
      for (chr_z_or_p in c("zscore", "centile")) {
        xvar <- ig_fet[[acronym]][[1]][[1]]
        for (seed in seq(300, 400, 30)) {
          set.seed(seed = seed)
          dbl_z_or_p <- rnorm(n = length(xvar))
          if (chr_z_or_p == "centile") dbl_z_or_p <- pnorm(dbl_z_or_p)

          fn_stem <- paste0("ig_fet_", acronym)
          fn_zp2val <- get(paste0(fn_stem, "_", chr_z_or_p, "2value"))
          set.seed(seed = seed)
          y_gigs <- fn_zp2val(dbl_z_or_p, xvar)

          fn_val2zp <- get(paste0(fn_stem, "_value2", chr_z_or_p))
          set.seed(seed = seed)
          gigs_z_or_p <- fn_val2zp(y_gigs, xvar)

          expect_equal(gigs_z_or_p, expected = dbl_z_or_p,
                       tolerance = sqrt(.Machine$double.eps))
        }
      }
    }
  }
)

#' @srrstats {G5.9, G5.9a} Trivial noise does not meaningfully alter results.
test_that(
  desc = "Conversion of values to z-scores works with trivial noise",
  code = {
    for (acronym in names(gigs::ig_fet)) {
      for (chr_z_or_p in c("zscore", "centile")) {
        xvar <- ig_fet[[acronym]][[1]][[1]]
        set.seed(seed = 50)
        dbl_z_or_p <- rnorm(n = length(xvar))
        if (chr_z_or_p == "centile") dbl_z_or_p <- pnorm(dbl_z_or_p)

        fn_stem <- paste0("ig_fet_", acronym)
        fn_zp2val <- get(paste0(fn_stem, "_", chr_z_or_p, "2value"))
        y_gigs <- fn_zp2val(dbl_z_or_p, xvar)

        fn_val2zp <- get(paste0(fn_stem, "_value2", chr_z_or_p))
        gigs_z_or_p <- fn_val2zp(y_gigs + .Machine$double.eps, xvar)

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

    # Error message testing: Iterate through ig_fet functions, demonstrating
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
      fn_name <- paste("ig_fet", fn_suffix, sep = "_")
      gigs_fn <- get(fn_name)

      # Sample three random acronyms --> doing all twenty balloons the runtime
      for (seed in c(150, 200, 250)) {
        acronym <- sample(names(gigs::ig_fet), size = 1)
        x <- gigs::ig_fet[[acronym]][[1]][[1]]

        arg_zyp <- switch(fn_suffix, centile2value = p, zscore2value = z, y)
        arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                               zscore2value = "z", "y")
        arg_x <- x
        arg_acronym <- acronym

        # G5.8a --> zero-length data inputs cause errors
        expect_error(object = gigs_fn(double(), arg_x, arg_acronym),
                     regexp = error_msg_zero_length(arg_zyp_name))
        expect_error(object = gigs_fn(arg_zyp, double(), arg_acronym),
                     regexp = error_msg_zero_length("x"))
        expect_error(object = gigs_fn(double(), arg_x, character()),
                     regexp = error_msg_zero_length(c(arg_zyp_name, "acronym")))

        # G5.8b --> incorrect data types
        expect_error(
          object = gigs_fn(as.character(arg_zyp), arg_x, arg_acronym),
          regexp = error_msg_wrong_type(arg_zyp_name, "numeric", "character")
        )
        expect_error(
          object = gigs_fn(arg_zyp, TRUE, arg_acronym),
          regexp = error_msg_wrong_type("x", "numeric", "logical")
        )
        expect_error(
          object = gigs_fn(arg_zyp, arg_x, 50L),
          regexp = error_msg_wrong_type("acronym", "character", "integer")
        )

        # G5.2, --> errors if vector inputs cannot be recycled
        expect_error(
          object = gigs_fn(rep(arg_zyp, 2), arg_x, arg_acronym),
          regexp = error_msg_unrecyclable(names = arg_zyp_name)
        )
        expect_error(
          object = gigs_fn(arg_zyp, arg_x, rep(arg_acronym, 2)),
          regexp = error_msg_unrecyclable(names = "acronym")
        )
        expect_error(
          object = gigs_fn(arg_zyp, rep(arg_x, 2), rep(arg_acronym, 2)),
          regexp = error_msg_unrecyclable("acronym")
        )
      }
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
      fn_name <- paste("ig_fet", fn_suffix, sep = "_")
      gigs_fn <- get(fn_name)
      # Sample three random acronyms --> doing all twenty explodes the runtime
      for (seed in c(125, 175, 225)) {
        acronym <- sample(names(gigs::ig_fet), size = 1)
        x <- gigs::ig_fet[[acronym]][[1]][[1]]
        len_x <- length(x)
        arg_zyp <- switch(fn_suffix,
                          centile2value = rep_len(p, len_x),
                          zscore2value = rep_len(z, len_x),
                          gigs::ig_fet[[acronym]][[1]][[1]])
        arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                               zscore2value = "z", "y")
        arg_x <- x
        arg_acronym <- rep_len(acronym, len_x)

        # Bad input 1: Undefined data (NaN, Inf, -Inf) -------------------------

        for (undefined_val in c(NaN, Inf, -Inf)) {
          ## Replace z/y/p variable with undefined_val
          out <- gigs_fn(replace(arg_zyp, replace_ints_1, values = undefined_val),
                         arg_x, arg_acronym)
          checkmate::expect_numeric(out, len = len_x)
          expect_true(all(!is.nan(out[replace_ints_1]) &
                            is.na(out[replace_ints_1])))

          ## Replace x variable with undefined_val
          out <- gigs_fn(arg_zyp,
                         replace(arg_x, replace_ints_2, values = undefined_val),
                         arg_acronym)
          checkmate::expect_numeric(out, len = len_x)
          expect_true(all(!is.nan(out[replace_ints_2]) &
                            is.na(out[replace_ints_2])))

          ## Replace z/y/p variable and x variable with undefined_val
          out <- gigs_fn(replace(arg_zyp, replace_ints_1, values = undefined_val),
                         replace(arg_x, replace_ints_2, values = undefined_val),
                         arg_acronym)
          checkmate::expect_numeric(out, len = len_x)
          expect_true(all(!is.nan(out[replace_ints_3]) &
                            is.na(out[replace_ints_3])))
        }


        # Bad input 2: Missing data (NA) ---------------------------------------
        ## Replace z/y/p variable with NA
        out <- gigs_fn(replace(arg_zyp, replace_ints_1, values = NA),
                       arg_x, arg_acronym)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_1])))

        ## Replace x variable with NA
        out <- gigs_fn(arg_zyp,
                       replace(arg_x, replace_ints_2, values = NA),
                       arg_acronym)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_2])))

        ## Replace z/y/p variable and x variable with NA
        out <- gigs_fn(replace(arg_zyp, replace_ints_1, values = NA),
                       replace(arg_x, replace_ints_2, values = NA),
                       arg_acronym)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_3])))

        ## Replace z/y/p variable and acronym variable with NA
        out <- gigs_fn(replace(arg_zyp, replace_ints_1, values = NA),
                       arg_x,
                       replace(arg_acronym, replace_ints_2, values = NA))
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_3])))

        # Bad input 3: Out of bounds centiles (not `> 0` and `< 1`) ------------
        if (fn_suffix == "centile2value") {
          out <- gigs_fn(replace(arg_zyp, replace_ints_1, values = 2.5),
                         arg_x, arg_acronym)
          checkmate::expect_numeric(out, len = len_x)
          expect_true(all(is.na(out[replace_ints_1])))

          out <- gigs_fn(replace(arg_zyp, replace_ints_2, values = 2.5),
                         arg_x, arg_acronym)
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
                       arg_acronym)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_2])))

        out <- gigs_fn(arg_zyp,
                       replace(arg_x, replace_ints_3, out_of_bounds),
                       arg_acronym)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_3])))

        # Bad input 5: Invalid `acronym` values (not in names(gigs::ig_fet))
        out <- gigs_fn(arg_zyp,
                       arg_x,
                       replace(arg_acronym,
                               replace_ints_1,
                               as.character(seq_len(num_to_replace1))))
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_1])))

        out <- gigs_fn(arg_zyp,
                       arg_x,
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
         "out-of-bounds \\(see the 'ig_fet' conversion functions ",
         "documentation\\).")
}

#' Replicate error/warning messages from gigs for bad input data (TESTING
#' ONLY)
#' @param name Single-length character vector with a variable name.
#' @param length Expected length of the variable specified by `name`.
#' @param int_undefined Number of elements in `x` which were expected to
#'   be invalid (not in `names(gigs::ig_fet)`) in whatever test is calling this
#'   function.
#' @note Used for testing only. Mimics the output of [msg_oob_centiles()].
#' @noRd
msg_acronym_invalid <- function(length, int_undefined) {
  see_sentence <- paste0("See the 'ig_fet' documentation for valid ",
                           "'acronym' values.")
  if (int_undefined == length) {
    stop("Variable 'acronym': All elements were invalid. ", see_sentence,
         call. = FALSE)
  }
  paste0("Variable 'acronym': ", int_undefined, " in ", length,
         " elements were invalid. ", see_sentence)
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
      fn_name <- paste("ig_fet", fn_suffix, sep = "_")
      gigs_fn <- get(fn_name)
      # Sample three random acronyms --> doing all twenty balloons the runtime
      for (seed in c(125, 175, 225)) {
        acronym <- sample(names(gigs::ig_fet), size = 1)
        x <- gigs::ig_fet[[acronym]][[1]][[1]]
        len_x <- length(x)
        arg_zyp <- switch(fn_suffix,
                          centile2value = rep_len(p, len_x),
                          zscore2value = rep_len(z, len_x),
                          gigs::ig_fet[[acronym]][[1]][[1]])
        arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                               zscore2value = "z", "y")
        arg_x <- x
        arg_acronym <- rep_len(acronym, len_x)

        # Bad input 1: Undefined data (NaN, Inf, -Inf) -------------------------
        for (undefined_val in c(NaN, Inf, -Inf)) {
          ## Replace z/y/p variable with undefined_val
          expect_warning(
            gigs_fn(replace(arg_zyp, replace_ints_1, values = undefined_val),
                    arg_x, arg_acronym),
            regexp = msg_undefined(arg_zyp_name, length = len_x,
                                   int_undefined = num_to_replace1)
          )

          ## Replace x variable with undefined_val
          expect_warning(
            gigs_fn(arg_zyp,
                    replace(arg_x, replace_ints_2, values = undefined_val),
                    arg_acronym),
            regexp = msg_undefined("x", length = len_x,
                                   int_undefined = num_to_replace2)
          )

          ## Replace z/y/p variable and x variable with undefined_val
          warnings <- testthat::capture_warnings(
            gigs_fn(replace(arg_zyp, replace_ints_1, values = undefined_val),
                    replace(arg_x, replace_ints_2, values = undefined_val),
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
          gigs_fn(replace(arg_zyp, replace_ints_1, NA), arg_x, arg_acronym),
          regexp = msg_missing(arg_zyp_name, len_x, num_to_replace1)
        )

        ## Replace x variable with NA
        expect_warning(
          gigs_fn(arg_zyp, replace(arg_x, replace_ints_2, NA), arg_acronym),
          regexp = msg_missing("x", len_x, num_to_replace2)
        )

        ## Replace z/y/p variable and acronym variable with NA
        warnings <- testthat::capture_warnings(
            gigs_fn(replace(arg_zyp, replace_ints_1, values = NA),
                    arg_x,
                    replace(arg_acronym, replace_ints_3, values = NA))
        )
        expect_match(warnings[1], msg_missing(arg_zyp_name, len_x,
                                              num_to_replace1))
        expect_match(warnings[2], msg_missing("acronym", len_x,
                                                num_to_replace3))

        # Bad input 3: Out of bounds centiles (not `> 0` and `< 1`) ------------
        if (fn_suffix == "centile2value") {
          expect_warning(
            gigs_fn(replace(arg_zyp, replace_ints_1, values = 2.5), arg_x, arg_acronym),
            msg_centile_oob(len_x, num_to_replace1)
          )
          expect_warning(
            gigs_fn(replace(arg_zyp, replace_ints_3, values = -.5), arg_x, arg_acronym),
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
                  arg_acronym),
          msg_xvar_oob(len_x, num_to_replace2)
        )
        expect_warning(
          gigs_fn(arg_zyp, replace(arg_x, replace_ints_3, out_of_bounds),
                  arg_acronym),
          msg_xvar_oob(len_x, num_to_replace3)
        )

        # Bad input 5: Invalid `acronym` values (!%in% names(gigs::ig_fet)) ----
        expect_warning(
          gigs_fn(arg_zyp, arg_x,
                  replace(arg_acronym, replace_ints_1,
                          as.character(seq_len(num_to_replace1)))),
          msg_acronym_invalid(len_x, num_to_replace1)
        )
        expect_warning(
          gigs_fn(arg_zyp, arg_x,
                  replace(arg_acronym, replace_ints_2,
                          as.character(seq_len(num_to_replace2)))),
          msg_acronym_invalid(len_x, num_to_replace2)
        )
      }
    }
  }
)

#' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
#' @srrstats{G5.8, G5.8d} Show that gigs can handle data outside the scope of
#'   its algorithms by throwing errors in response to bad inputs.
test_that(
  desc = "Invalid values can be replaced with `NA` with warnings",
  code = {
    # Make gigs error if confronted with any bad data
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
      fn_name <- paste("ig_fet", fn_suffix, sep = "_")
      gigs_fn <- get(fn_name)
      # Sample three random acronyms --> doing all twenty balloons the runtime
      for (seed in c(125, 175, 225)) {
        acronym <- sample(names(gigs::ig_fet), size = 1)
        x <- gigs::ig_fet[[acronym]][[1]][[1]]
        len_x <- length(x)
        arg_zyp <- switch(fn_suffix,
                          centile2value = rep_len(p, len_x),
                          zscore2value = rep_len(z, len_x),
                          gigs::ig_fet[[acronym]][[1]][[1]])
        arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                               zscore2value = "z", "y")
        arg_x <- x
        arg_acronym <- rep_len(acronym, len_x)

        # Bad input 1: Undefined data (NaN, Inf, -Inf) -------------------------
        for (undefined_val in c(NaN, Inf, -Inf)) {
          ## Replace z/y/p variable with undefined_val
          expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_1, values = undefined_val),
                    arg_x, arg_acronym),
            regexp = msg_undefined(arg_zyp_name, length = len_x,
                                   int_undefined = num_to_replace1)
          )

          ## Replace x variable with undefined_val
          expect_error(
            gigs_fn(arg_zyp,
                    replace(arg_x, replace_ints_2, values = undefined_val),
                    arg_acronym),
            regexp = msg_undefined("x", length = len_x,
                                   int_undefined = num_to_replace2)
          )

          ## Replace z/y/p variable and x variable with undefined_val
          expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_1, values = undefined_val),
                    replace(arg_x, replace_ints_2, values = undefined_val),
                    arg_acronym),
            msg_undefined(arg_zyp_name, len_x, num_to_replace1))
        }

        # Bad input 2: Missing data (NA) ---------------------------------------
        ## Replace z/y/p variable with NA
        expect_error(
          gigs_fn(replace(arg_zyp, replace_ints_1, NA), arg_x, arg_acronym),
          regexp = msg_missing(arg_zyp_name, len_x, num_to_replace1)
        )

        ## Replace x variable with NA
        expect_error(
          gigs_fn(arg_zyp, replace(arg_x, replace_ints_2, NA), arg_acronym),
          regexp = msg_missing("x", len_x, num_to_replace2)
        )

        ## Replace z/y/p variable and acronym variable with NA
        expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_1, values = NA),
                    arg_x,
                    replace(arg_acronym, replace_ints_3, values = NA)),
            msg_missing(arg_zyp_name, len_x, num_to_replace1)
        )

        # Bad input 3: Out of bounds centiles (not `> 0` and `< 1`) ------------
        if (fn_suffix == "centile2value") {
          expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_1, values = 2.5), arg_x, arg_acronym),
            msg_centile_oob(len_x, num_to_replace1)
          )
          expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_3, values = -.5), arg_x, arg_acronym),
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
                  arg_acronym),
          msg_xvar_oob(len_x, num_to_replace2)
        )
        expect_error(
          gigs_fn(arg_zyp, replace(arg_x, replace_ints_3, out_of_bounds),
                  arg_acronym),
          msg_xvar_oob(len_x, num_to_replace3)
        )

        # Bad input 5: Invalid `acronym` values (!%in% names(gigs::ig_fet)) ----
        expect_error(
          gigs_fn(arg_zyp, arg_x,
                  replace(arg_acronym, replace_ints_1,
                          as.character(seq_len(num_to_replace1)))),
          msg_acronym_invalid(len_x, num_to_replace1)
        )
        expect_error(
          gigs_fn(arg_zyp, arg_x,
                  replace(arg_acronym, replace_ints_2,
                          as.character(seq_len(num_to_replace2)))),
          msg_acronym_invalid(len_x, num_to_replace2)
        )
      }
    }
  }
)

#' @srrstats {G5.8c} Show how the ig_fet functions handles all-`NA` inputs.
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
      fn_name <- paste("ig_fet", fn_suffix, sep = "_")
      gigs_fn <- get(fn_name)
      for (seed in c(125, 175, 225)) {
        set.seed(seed)
        acronym <- sample(names(gigs::ig_fet), size = 1)
        x <- gigs::ig_fet[[acronym]][[1]][[1]]
        len_x <- length(x)
        arg_zyp <- switch(fn_suffix,
                          centile2value = rep_len(p, len_x),
                          zscore2value = rep_len(z, len_x),
                          gigs::ig_fet[[acronym]][[1]][[1]])
        arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                               zscore2value = "z", "y")
        arg_x <- x
        arg_acronym <- rep_len(acronym, len_x)
        all_NA <- rep(NA, len_x)

        # Will produce all NAs for `y` argument, with a warning
        expect_warning(
          gigs_fn(all_NA, arg_x, arg_acronym),
          msg_missing(name = arg_zyp_name, len_x, len_x)
        )
        # Will produce all NAs for the `x` argument
        expect_warning(
          gigs_fn(arg_zyp, all_NA, arg_acronym),
          msg_missing(name = "x", len_x, len_x)
        )
        # Will produce an error with the `acronym` argument
        expect_error(
          gigs_fn(arg_zyp, arg_x, all_NA),
          regexp = "Variable 'acronym': All elements were missing \\(`NA`\\)."
        )
        # All invalid `acronym`s will also produce an error
        expect_error(
          gigs_fn(arg_zyp, arg_x, rep("not valid acronym!! oh no!", len_x)),
          regexp = paste0("Variable 'acronym': All elements were invalid. ",
                          "See the 'ig_fet' documentation for valid 'acronym' ",
                          "values.")
        )
      }
    }
  }
)

# Testing against other R implementations --------------------------------------

#' @srrstats {G5.4, G5.4a, G5.4b} Test correctness against other R
#'   implementations.
test_that(
  desc = "GIGS aligns with `growthstandards` package",
  code = {
    skip_if_not_installed(pkg = "growthstandards")
    y_gigs <- 45
    y_growthstandards <- 45 / 10
    gs_fns <- list(growthstandards::igfet_hccm2zscore,
                   growthstandards::igfet_bpdcm2zscore,
                   growthstandards::igfet_accm2zscore,
                   growthstandards::igfet_flcm2zscore,
                   growthstandards::igfet_ofdcm2zscore)
    gigs_fns <- list(ig_fet_hcfga_value2zscore,
                     ig_fet_bpdfga_value2zscore,
                     ig_fet_acfga_value2zscore,
                     ig_fet_flfga_value2zscore,
                     ig_fet_ofdfga_value2zscore)
    for (idx in seq_along(gs_fns)) {
      gigs <- gigs_fns[[idx]](y_gigs, 154)
      gs <- gs_fns[[idx]](154, y_growthstandards)
      # Set tolerance as 10e-5 to accomodate slight error in BPD-for-GA
      # equations in `growthstandards`
      expect_equal(object = gigs, expected = gs, tolerance = 10e-5)
    }
  }
)

# Functions still work with odd class structures -------------------------------

#' @srrstats {G2.6, EA2.6} INTERGROWTH-21st Fetal Growth functions still
#' operate even when univariate inputs have unusual class structures.
test_that(
  desc = "Test that univariate input works with alternate class structures",
  code = {
    skip_on_cran()

    # Conversion of values to z-scores works
    y <- units::set_units(x = 2500, g)
    ga <- units::set_units(x = 234, days)
    acronym <- "efwfga"
    expect_vector(ig_fet_value2centile(y, ga, acronym),
                  ptype = double(),
                  size = 1)

    # Estimation of fetal weight works
    headcirc <- units::set_units(x = 29L, cm)
    abdocirc <- units::set_units(x = 26L, cm)
    efw <- ig_fet_estimate_fetal_weight(headcirc_mm = headcirc,
                                        abdocirc_mm = abdocirc)
    expect_vector(efw, ptype = double(), size = 1)
  }
)