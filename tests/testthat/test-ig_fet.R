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
                          #'   seed
                          withr::with_seed(seed = 1000, {
                            round(conv_fn(zp, xvar),
                                  digits = fet_roundto(acronym))
                          })
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
          dbl_z_or_p <- withr::with_seed(seed, code = {
            rnorm(n = length(xvar))
          })
          if (chr_z_or_p == "centile") dbl_z_or_p <- pnorm(dbl_z_or_p)

          fn_stem <- paste0("ig_fet_", acronym)
          fn_zp2val <- get(paste0(fn_stem, "_", chr_z_or_p, "2value"))
          y_gigs <- fn_zp2val(dbl_z_or_p, xvar)

          fn_val2zp <- get(paste0(fn_stem, "_value2", chr_z_or_p))
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
        withr::with_seed(
          seed = 50,
          code = {
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
        )
      }
    }
  }
)

# Appropriate errors for zero-length data + data of wrong type -----------------

#' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
#' @srrstats {G5.8, G5.8a, G5.8b} Including errors for zero-length inputs and
#'   incorrect data types.
test_that(
  desc = "Correct errors occur with zero-length/incorrectly typed data:",
  code = {
    # Error message testing: Iterate through ig_fet functions, demonstrating
    #   with passed `expect_error()` tests that all error messages are unique
    #   to the data passed in.
    ig_fet_acronyms <- names(ig_fet)
    acronyms_to_test <- withr::with_seed(150, sample(ig_fet_acronyms, 5))
    fn_suffixes <- c("value2zscore", "value2centile", "centile2value",
                     "zscore2value")
    z <- 0
    p <- 0.5
    y <- 4
    for (fn_suffix in fn_suffixes) {
      for (str_acronym in acronyms_to_test) {
        fn_name <- paste("ig_fet", str_acronym, fn_suffix, sep = "_")
        gigs_fn <- get(fn_name)
        arg_x_name <- gigs::ig_fet[[str_acronym]][["x"]]
        y_name <- gigs::ig_fet[[str_acronym]][["y"]]

        arg_zyp <- switch(fn_suffix, centile2value = p, zscore2value = z, y)
        arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                               zscore2value = "z", y_name)
        arg_x <- gigs::ig_fet[[str_acronym]][[1]][[1]]

        # G5.8a --> zero-length data inputs cause errors
        expect_error(object = gigs_fn(double(), arg_x),
                     regexp = test_error_zero_length(arg_zyp_name))
        expect_error(object = gigs_fn(arg_zyp, double()),
                     regexp = test_error_zero_length(arg_x_name))
        expect_error(object = gigs_fn(double(), double()),
                     regexp = test_error_zero_length(c(arg_zyp_name, arg_x_name)))

        # G5.8b --> incorrect data types
        expect_error(
          object = gigs_fn(as.character(arg_zyp), arg_x),
          regexp = test_error_wrong_type(arg_zyp_name, "numeric", "character")
        )
        expect_error(
          object = gigs_fn(arg_zyp, TRUE),
          regexp = test_error_wrong_type(arg_x_name, "numeric", "logical")
        )

        # G5.2, --> errors if vector inputs cannot be recycled
        expect_error(
          object = gigs_fn(rep_len(arg_zyp, 2), arg_x),
          regexp = test_error_unrecyclable(names = arg_zyp_name)
        )
        expect_error(
          object = gigs_fn(rep_len(arg_zyp, 3), rep_len(arg_x, 2)),
          regexp = test_error_unrecyclable(names = "gest_days")
        )
      }

      # Repeat for standard-agnostic conversion function
      fn_name <- paste("ig_fet", fn_suffix, sep = "_")
      gigs_fn <- get(fn_name)

      arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                             zscore2value = "z", "y")
      arg_acronym <- str_acronym
      # G5.8a --> zero-length data inputs cause errors
      expect_error(object = gigs_fn(double(), arg_x, arg_acronym),
                   regexp = test_error_zero_length(arg_zyp_name))
      expect_error(object = gigs_fn(arg_zyp, double(), arg_acronym),
                   regexp = test_error_zero_length("x"))
      expect_error(object = gigs_fn(double(), arg_x, character()),
                   regexp = test_error_zero_length(c(arg_zyp_name, "acronym")))
      # G5.8b --> incorrect data types
      expect_error(
        object = gigs_fn(as.character(arg_zyp), arg_x, arg_acronym),
        regexp = test_error_wrong_type(arg_zyp_name, "numeric", "character")
      )
      expect_error(
        object = gigs_fn(arg_zyp, TRUE, arg_acronym),
        regexp = test_error_wrong_type("x", "numeric", "logical")
      )
      expect_error(
        object = gigs_fn(arg_zyp, arg_x, 50L),
        regexp = test_error_wrong_type("acronym", "character", "integer")
      )
      # G5.2, --> errors if vector inputs cannot be recycled
      expect_error(
        object = gigs_fn(rep_len(arg_zyp, 2),
                         arg_x,
                         arg_acronym),
        regexp = test_error_unrecyclable(names = arg_zyp_name)
      )
      expect_error(
        object = gigs_fn(arg_zyp,
                         arg_x,
                         rep_len(arg_acronym, 2)),
        regexp = test_error_unrecyclable(names = "acronym")
      )
      expect_error(
        object = gigs_fn(rep_len(arg_zyp, 5),
                         rep_len(arg_x, 4),
                         rep_len(arg_acronym, 3)),
        regexp = test_error_unrecyclable(c("x", "acronym"))
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

    ig_fet_acronyms <- names(ig_fet)
    acronyms_to_test <- withr::with_seed(800, sample(ig_fet_acronyms, 10))
    fn_suffixes <- c("value2zscore", "value2centile", "centile2value",
                     "zscore2value")
    z <- 0
    p <- 0.5

    # Random set of indices to replace with bad data in each case
    num_to_replace1 <- 6
    withr::with_seed(200, code = {
      replace_ints_1 <- sample(1L:17L, size = num_to_replace1)
    })
    num_to_replace2 <- 5
    withr::with_seed(200, code = {
      replace_ints_2 <- sample(1L:17L, size = num_to_replace2)
    })
    replace_ints_3 <- union(replace_ints_1, replace_ints_2)
    num_to_replace3 <- length(union(replace_ints_1, replace_ints_2))

    for (fn_suffix in fn_suffixes) {
      for (str_acronym in acronyms_to_test) {
        fn_name <- paste("ig_fet", str_acronym, fn_suffix, sep = "_")
        gigs_fn <- get(fn_name)
        x <- gigs::ig_fet[[str_acronym]][[1]][[1]]
        len_x <- length(x)
        arg_x_name <- gigs::ig_fet[[str_acronym]][["x"]]
        y_name <- gigs::ig_fet[[str_acronym]][["y"]]

        arg_zyp <- rep_len(switch(fn_suffix,
                                  centile2value = p,
                                  zscore2value = z,
                                  gigs::ig_fet[[str_acronym]][[1]][[2]]),
                           len_x)
        arg_zyp_name <- switch(fn_suffix,
                               centile2value = "p",
                               zscore2value = "z",
                               y_name)
        arg_x <- x

        # Bad input 1: Undefined data (NaN, Inf, -Inf) -------------------------
        for (undefined_val in c(NaN, Inf, -Inf)) {
          ## Replace z/y/p variable and x variable with undefined_val
          out <- gigs_fn(replace(arg_zyp, replace_ints_1, undefined_val),
                         replace(arg_x, replace_ints_2, undefined_val))
          checkmate::expect_numeric(out, len = len_x)
          expect_true(all(is.na(out[replace_ints_3])))
        }

        # Bad input 2: Missing data (NA) ---------------------------------------
        ## Replace x variable with NA
        out <- gigs_fn(arg_zyp,
                       replace(arg_x, replace_ints_2, values = NA))
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_2])))

        ## Replace z/y/p variable and x variable with NA
        out <- gigs_fn(replace(arg_zyp, replace_ints_1, values = NA),
                       replace(arg_x, replace_ints_2, values = NA))
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_3])))

        # Bad input 3: Out of bounds centiles (not `> 0` and `< 1`) ------------
        if (fn_suffix == "centile2value") {
          out <- gigs_fn(replace(arg_zyp, replace_ints_1, values = 2.5), arg_x)
          checkmate::expect_numeric(out, len = len_x)
          expect_true(all(is.na(out[replace_ints_1])))

          out <- gigs_fn(replace(arg_zyp, replace_ints_2, values = 2.5), arg_x)
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
                       replace(arg_x, replace_ints_2, above_upper_bound))
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_2])))

        out <- gigs_fn(arg_zyp,
                       replace(arg_x, replace_ints_3, out_of_bounds))
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_3])))
      }

      fn_name <- paste("ig_fet", fn_suffix, sep = "_")
      gigs_fn <- get(fn_name)
      arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                             zscore2value = "z", "y")
      arg_acronym <- rep_len(str_acronym, len_x)

      # Bad input 1: Undefined data (NaN, Inf, -Inf) -------------------------
      for (undefined_val in c(NaN, Inf, -Inf)) {
        ## Replace z/y/p variable and x variable with undefined_val
        out <- gigs_fn(replace(arg_zyp, replace_ints_1, undefined_val),
                       replace(arg_x, replace_ints_2, undefined_val),
                       arg_acronym)
        checkmate::expect_numeric(out, len = len_x)
        expect_true(all(is.na(out[replace_ints_3])))
      }

      # Bad input 2: Missing data (NA) ---------------------------------------
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
      withr::with_seed(500, code = {
        out_of_bounds <- sample(c(below_lower_bound, above_upper_bound),
                                size = num_to_replace3, replace = FALSE)
      })

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

    ig_fet_acronyms <- names(ig_fet)
    acronyms_to_test <- withr::with_seed(800, sample(ig_fet_acronyms, 10))
    fn_suffixes <- c("value2zscore", "value2centile", "centile2value",
                     "zscore2value")
    z <- 0
    p <- 0.5

    # Random set of indices to replace with bad data in each case
    num_to_replace1 <- 6
    withr::with_seed(200, code = {
      replace_ints_1 <- sample(1L:17L, size = num_to_replace1)
    })
    num_to_replace2 <- 5
    withr::with_seed(200, code = {
      replace_ints_2 <- sample(1L:17L, size = num_to_replace2)
    })
    replace_ints_3 <- union(replace_ints_1, replace_ints_2)
    num_to_replace3 <- length(union(replace_ints_1, replace_ints_2))

    for (fn_suffix in fn_suffixes) {
      for (str_acronym in acronyms_to_test) {
        fn_name <- paste("ig_fet", str_acronym, fn_suffix, sep = "_")
        gigs_fn <- get(fn_name)
        x <- gigs::ig_fet[[str_acronym]][[1]][[1]]
        len_x <- length(x)
        arg_x_name <- gigs::ig_fet[[str_acronym]][["x"]]
        arg_y_name <- gigs::ig_fet[[str_acronym]][["y"]]

        arg_zyp <- rep_len(switch(fn_suffix,
                                  centile2value = p,
                                  zscore2value = z,
                                  gigs::ig_fet[[str_acronym]][[1]][[2]]),
                           len_x)
        arg_zyp_name <- switch(fn_suffix,
                               centile2value = "p",
                               zscore2value = "z",
                               arg_y_name)
        arg_x <- x

        # Bad input 1: Undefined data (NaN, Inf, -Inf) -------------------------
        for (undefined_val in c(NaN, Inf, -Inf)) {
          ## Replace z/y/p variable and x variable with undefined_val
          warnings <- capture_warnings(
            gigs_fn(replace(arg_zyp, replace_ints_1, undefined_val),
                    replace(arg_x, replace_ints_2, undefined_val))
          )
          expect_match(warnings[[1]],
                       test_msg_undefined(arg_zyp_name, len_x, num_to_replace1))
          expect_match(warnings[[2]],
                       test_msg_undefined(arg_x_name, len_x, num_to_replace2))
        }

        # Bad input 2: Missing data (NA) ---------------------------------------

        ## Replace z/y/p variable and x variable with NA
        warnings <- capture_warnings(
          gigs_fn(replace(arg_zyp, replace_ints_2, values = NA),
                  replace(arg_x, replace_ints_1, values = NA))
        )
        expect_match(warnings[[1]],
                     test_msg_missing(arg_zyp_name, len_x, num_to_replace2))
        expect_match(warnings[[2]],
                     test_msg_missing(arg_x_name, len_x, num_to_replace1))

        # Bad input 3: Out of bounds centiles (not `> 0` and `< 1`) ------------
        if (fn_suffix == "centile2value") {
          expect_warning(
            gigs_fn(replace(arg_zyp, replace_ints_3, values = 2.5), arg_x),
            test_msg_centile_oob(len_x, num_to_replace3)
          )
          expect_warning(
            gigs_fn(replace(arg_zyp, replace_ints_2, values = 2.5), arg_x),
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
          gigs_fn(arg_zyp, replace(arg_x, replace_ints_2, above_upper_bound)),
          test_msg_xvar_oob(arg_x_name, len_x, num_to_replace2, "ig_fet")
        )
        expect_warning(
          gigs_fn(arg_zyp, replace(arg_x, replace_ints_3, out_of_bounds)),
          test_msg_xvar_oob(arg_x_name, len_x, num_to_replace3, "ig_fet")
        )
      }

      fn_name <- paste("ig_fet", fn_suffix, sep = "_")
      gigs_fn <- get(fn_name)
      arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                             zscore2value = "z", "y")
      arg_acronym <- rep_len(str_acronym, len_x)
      arg_x_name <- "x"

      # Bad input 1: Undefined data (NaN, Inf, -Inf) ---------------------------
      for (undefined_val in c(NaN, Inf, -Inf)) {
        ## Replace z/y/p variable and x variable with undefined_val
        warnings <- capture_warnings(
          gigs_fn(replace(arg_zyp, replace_ints_1, undefined_val),
                  replace(arg_x, replace_ints_2, undefined_val),
                  arg_acronym)
        )
        expect_match(warnings[[1]],
                     test_msg_undefined(arg_zyp_name, len_x, num_to_replace1))
        expect_match(warnings[[2]],
                     test_msg_undefined(arg_x_name, len_x, num_to_replace2))
      }

      # Bad input 2: Missing data (NA) -----------------------------------------
      ## Replace z/y/p variable and acronym variable with NA
      warnings <- capture_warnings(
        gigs_fn(replace(arg_zyp, replace_ints_2, values = NA),
                arg_x,
                replace(arg_acronym, replace_ints_1, NA))
      )
      expect_match(warnings[[1]],
                   test_msg_missing(arg_zyp_name, len_x, num_to_replace2))
      expect_match(warnings[[2]],
                   test_msg_missing("acronym", len_x, num_to_replace1))

      # Bad input 3: Out of bounds centiles (not `> 0` and `< 1`) --------------
      if (fn_suffix == "centile2value") {
          expect_warning(
            gigs_fn(replace(arg_zyp, replace_ints_3, values = 2.5),
                    arg_x,
                    arg_acronym),
            test_msg_centile_oob(len_x, num_to_replace3)
          )
          expect_warning(
            gigs_fn(replace(arg_zyp, replace_ints_2, values = 2.5),
                    arg_x,
                    arg_acronym),
            test_msg_centile_oob(len_x, num_to_replace2)
          )
      }

      # Bad input 4: Out of bounds `x` variable --------------------------------
      expect_warning(
        gigs_fn(arg_zyp,
                replace(arg_x, replace_ints_2, above_upper_bound),
                arg_acronym),
        test_msg_xvar_oob(arg_x_name, len_x, num_to_replace2, "ig_fet")
      )
      expect_warning(
        gigs_fn(arg_zyp,
                replace(arg_x, replace_ints_3, out_of_bounds),
                arg_acronym),
        test_msg_xvar_oob(arg_x_name, len_x, num_to_replace3, "ig_fet")
      )

      # Bad input 5: Invalid `acronym` values (not in names(gigs::ig_fet))
      expect_warning(
        gigs_fn(arg_zyp,
                arg_x,
                replace(arg_acronym, replace_ints_2, "invalid")),
        test_msg_acronym_sex_invalid("acronym", len_x, num_to_replace2,
                                     "ig_fet")
      )
      expect_warning(
        gigs_fn(arg_zyp, arg_x,
                replace(arg_acronym, replace_ints_3, "invalid")),
        test_msg_acronym_sex_invalid("acronym", len_x, num_to_replace3,
                                     "ig_fet")
      )
    }
  }
)

#' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
#' @srrstats{G5.8, G5.8d} Show that gigs can handle data outside the
#'   scope of its algorithms by replacing bad inputs with `NA` and giving
#'   errors.
test_that(
  desc = "Invalid values can be confronted with errors",
  code = {
    # Make gigs *quietly* replace bad data with NA
    for (option in names(.gigs_options)) {
      gigs_option_set(option, new_value = "error", silent = TRUE)
    }

    ig_fet_acronyms <- names(ig_fet)
    acronyms_to_test <- withr::with_seed(800, sample(ig_fet_acronyms, 10))
    fn_suffixes <- c("value2zscore", "value2centile", "centile2value",
                     "zscore2value")
    z <- 0
    p <- 0.5

    # Random set of indices to replace with bad data in each case
    num_to_replace1 <- 6
    withr::with_seed(200, code = {
      replace_ints_1 <- sample(1L:17L, size = num_to_replace1)
    })
    num_to_replace2 <- 5
    withr::with_seed(200, code = {
      replace_ints_2 <- sample(1L:17L, size = num_to_replace2)
    })
    replace_ints_3 <- union(replace_ints_1, replace_ints_2)
    num_to_replace3 <- length(union(replace_ints_1, replace_ints_2))

    for (fn_suffix in fn_suffixes) {
      for (str_acronym in acronyms_to_test) {
        fn_name <- paste("ig_fet", str_acronym, fn_suffix, sep = "_")
        gigs_fn <- get(fn_name)
        x <- gigs::ig_fet[[str_acronym]][[1]][[1]]
        len_x <- length(x)
        arg_x_name <- gigs::ig_fet[[str_acronym]][["x"]]
        arg_y_name <- gigs::ig_fet[[str_acronym]][["y"]]

        arg_zyp <- rep_len(switch(fn_suffix,
                                  centile2value = p,
                                  zscore2value = z,
                                  gigs::ig_fet[[str_acronym]][[1]][[2]]),
                           len_x)
        arg_zyp_name <- switch(fn_suffix,
                               centile2value = "p",
                               zscore2value = "z",
                               arg_y_name)
        arg_x <- x

        # Bad input 1: Undefined data (NaN, Inf, -Inf) -------------------------
        for (undefined_val in c(NaN, Inf, -Inf)) {
          ## Replace z/y/p variable and x variable with undefined_val
          expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_1, undefined_val),
                    replace(arg_x, replace_ints_2, undefined_val)),
            test_msg_undefined(arg_zyp_name, len_x, num_to_replace1)
          )
          expect_error(
            gigs_fn(arg_zyp,
                    replace(arg_x, replace_ints_2, undefined_val)),
            test_msg_undefined(arg_x_name, len_x, num_to_replace2)
          )
        }

        # Bad input 2: Missing data (NA) ---------------------------------------
        ## Replace z/y/p variable and x variable with NA
        expect_error(
          gigs_fn(replace(arg_zyp, replace_ints_1, NA),
                  replace(arg_x, replace_ints_2, NA)),
          test_msg_missing(arg_zyp_name, len_x, num_to_replace1)
        )
        expect_error(
          gigs_fn(arg_zyp,
                  replace(arg_x, replace_ints_2, NA)),
          test_msg_missing(arg_x_name, len_x, num_to_replace2)
        )

        # Bad input 3: Out of bounds centiles (not `> 0` and `< 1`) ------------
        if (fn_suffix == "centile2value") {
          expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_3, values = 2.5), arg_x),
            test_msg_centile_oob(len_x, num_to_replace3)
          )
          expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_2, values = 2.5), arg_x),
            test_msg_centile_oob(len_x, num_to_replace2)
          )
        }

        # Bad input 4: Out of bounds `x` variable ------------------------------
        # Out of bounds x variables - initialise outside loops
        below_lower_bound <- min(x) - replace_ints_1
        above_upper_bound <- max(x) + replace_ints_2
        out_of_bounds <-withr::with_seed(500, code = {
          sample(c(below_lower_bound, above_upper_bound),
                 size = num_to_replace3, replace = FALSE)
        })
        expect_error(
          gigs_fn(arg_zyp, replace(arg_x, replace_ints_2, above_upper_bound)),
          test_msg_xvar_oob(arg_x_name, len_x, num_to_replace2, "ig_fet")
        )
        expect_error(
          gigs_fn(arg_zyp, replace(arg_x, replace_ints_3, out_of_bounds)),
          test_msg_xvar_oob(arg_x_name, len_x, num_to_replace3, "ig_fet")
        )
      }

      fn_name <- paste("ig_fet", fn_suffix, sep = "_")
      gigs_fn <- get(fn_name)
      arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                             zscore2value = "z", "y")
      arg_acronym <- rep_len(str_acronym, len_x)
      arg_x_name <- "x"

      # Bad input 1: Undefined data (NaN, Inf, -Inf) ---------------------------
      for (undefined_val in c(NaN, Inf, -Inf)) {
        expect_error(
          gigs_fn(replace(arg_zyp, replace_ints_1, undefined_val),
                  replace(arg_x, replace_ints_2, undefined_val),
                  arg_acronym),
          test_msg_undefined(arg_zyp_name, len_x, num_to_replace1)
        )
        expect_error(
          gigs_fn(arg_zyp,
                  replace(arg_x, replace_ints_2, undefined_val),
                  arg_acronym),
          test_msg_undefined(arg_x_name, len_x, num_to_replace2)
        )
      }

      # Bad input 2: Missing data (NA) -----------------------------------------
      expect_error(
        gigs_fn(replace(arg_zyp, replace_ints_1, NA),
                replace(arg_x, replace_ints_2, NA),
                arg_acronym),
        test_msg_missing(arg_zyp_name, len_x, num_to_replace1)
      )
      expect_error(
        gigs_fn(arg_zyp,
                arg_x,
                replace(arg_acronym, replace_ints_2, NA)),
        test_msg_missing("acronym", len_x, num_to_replace2)
      )

      # Bad input 3: Out of bounds centiles (not `> 0` and `< 1`) --------------
      if (fn_suffix == "centile2value") {
          expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_3, values = 2.5),
                    arg_x,
                    arg_acronym),
            test_msg_centile_oob(len_x, num_to_replace3)
          )
          expect_error(
            gigs_fn(replace(arg_zyp, replace_ints_2, values = 2.5),
                    arg_x,
                    arg_acronym),
            test_msg_centile_oob(len_x, num_to_replace2)
          )
      }

      # Bad input 4: Out of bounds `x` variable --------------------------------
      expect_error(
        gigs_fn(arg_zyp,
                replace(arg_x, replace_ints_2, above_upper_bound),
                arg_acronym),
        test_msg_xvar_oob(arg_x_name, len_x, num_to_replace2, "ig_fet")
      )
      expect_error(
        gigs_fn(arg_zyp,
                replace(arg_x, replace_ints_3, out_of_bounds),
                arg_acronym),
        test_msg_xvar_oob(arg_x_name, len_x, num_to_replace3, "ig_fet")
      )

      # Bad input 5: Invalid `acronym` values (not in names(gigs::ig_fet))
      expect_error(
        gigs_fn(arg_zyp,
                arg_x,
                replace(arg_acronym, replace_ints_2, "invalid")),
        test_msg_acronym_sex_invalid("acronym", len_x, num_to_replace2,
                                     "ig_fet")
      )
      expect_error(
        gigs_fn(arg_zyp, arg_x,
                replace(arg_acronym, replace_ints_3, "invalid")),
        test_msg_acronym_sex_invalid("acronym", len_x, num_to_replace3,
                                     "ig_fet")
      )
    }
  }
)

#' @srrstats {G5.8c} Show how the ig_fet functions handles all-`NA` inputs.
test_that(
  desc = "Test that 'ig_fet' functions can handle all-`NA` inputs",
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
        withr::with_seed(seed, code = {
          str_acronym <- sample(names(gigs::ig_fet), size = 1)
        })
        x <- gigs::ig_fet[[str_acronym]][[1]][[1]]
        len_x <- length(x)
        arg_zyp <- rep_len(switch(fn_suffix,
                                  centile2value = p,
                                  zscore2value = z,
                                  gigs::ig_fet[[str_acronym]][[1]][[2]]),
                           len_x)
        arg_zyp_name <- switch(fn_suffix, centile2value = "p",
                               zscore2value = "z", "y")
        arg_x <- x
        arg_acronym <- rep_len(str_acronym, len_x)
        all_NA <- rep_len(NA, len_x)

        # Will produce all NAs for `y` argument, with a warning
        expect_warning(
          gigs_fn(all_NA, arg_x, arg_acronym),
          test_msg_missing(name = arg_zyp_name, len_x, len_x)
        )
        # Will produce all NAs for the `x` argument, with a warning
        expect_warning(
          gigs_fn(arg_zyp, all_NA, arg_acronym),
          test_msg_missing(name = "x", len_x, len_x)
        )
        # Will throw an error if all elements of `acronym` are missing
        expect_error(
          gigs_fn(arg_zyp, arg_x, all_NA),
          regexp = "Variable 'acronym': All elements were missing \\(`NA`\\)."
        )
        # All invalid `acronym`s will also produce an error
        expect_error(
          gigs_fn(arg_zyp,
                   arg_x,
                   rep_len("not valid acronym!! oh no!", len_x)),
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