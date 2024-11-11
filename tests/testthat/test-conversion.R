## Correctness testing for the functions in conversion.R is performed in
## 'test-who_gs.R', 'test-ig_fet.R', 'test-ig_nbs.R', and 'test-ig_png.R'.

# Appropriate errors for zero-length data + data of wrong type -----------------
# standard_families <- c("who_gs", "ig_fet", "ig_nbs", "ig_png")
conv_fns <- c("value2zscore", "value2centile", "centile2value", "zscore2value")
zyp_cache <- c("value2zscore" = 4,
               "value2centile" = 4,
               "centile2value" = 0.5,
               "zscore2value" = 0)
zyp_name_cache <- c("value2zscore" = "y",
                    "value2centile" = "y",
                    "centile2value" = "p",
                    "zscore2value" = "z")

#' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
#' @srrstats {G5.8, G5.8a, G5.8b} Including errors for zero-length inputs and
#'   incorrect data types.
test_that("Correct errors occur in GIGS conversion functions:", {
  # Error message testing: Iterate through GIGS conversion functions,
  #   demonstrating with passed `expect_error()` tests that errors are specific
  #   to the data passed in.

  family <- "ig_png"
  acronym <- "wfa"
  x <- gigs::ig_png[[acronym]][[1]][[1]][[1]]
  sex <- withr::with_seed(25, {
    sample(c("M", "F"), replace = TRUE, size = length(x))
  })
  for (fn_name in conv_fns) {
    gigs_fn <- getFromNamespace(fn_name, ns = "gigs")
    zyp <- zyp_cache[[fn_name]]
    zyp_name <- zyp_name_cache[[fn_name]]

    # G5.8a --> zero-length data inputs cause errors
    expect_error(gigs_fn(double(), x, sex, family, acronym),
                 class = "gigs_err_zero_length")
    expect_error(gigs_fn(zyp, integer(), sex, family, acronym),
                 class = "gigs_err_zero_length")
    expect_error(gigs_fn(zyp, x, character(), family, acronym),
                 class = "gigs_err_zero_length")
    expect_error(gigs_fn(zyp, x, sex, character(), acronym),
                 class = "gigs_string_bad_length")
    expect_error(gigs_fn(zyp, x, sex, family, character()),
                 class = "gigs_string_bad_length")
    # If a scalar and non-scalar input are both zero-length, the scalar
    # argument will cause the error:
    expect_error(gigs_fn(zyp, double(), sex, family, character()),
                 class = "gigs_string_bad_length")

    # G5.8b --> incorrect data types
    expect_error(
      object = gigs_fn(as.character(zyp), x, sex, family, acronym),
      regexp = test_error_wrong_type(zyp_name, "numeric", "character")
    )
    expect_error(
      object = gigs_fn(zyp, logical(1), sex, family, acronym),
      regexp = test_error_wrong_type("x", "numeric", "logical")
    )
    expect_error(
      object = gigs_fn(zyp, x, complex(1), family, acronym),
      regexp = test_error_wrong_type("sex", "character", "complex")
    )
    expect_error(
      object = gigs_fn(zyp, x, sex, complex(1), acronym),
      class = "gigs_string_not_char"
    )
    expect_error(
      object = gigs_fn(zyp, x, sex, family, numeric(1)),
      class = "gigs_string_not_char"
    )
    # If a scalar string input and non-scalar input are both bad data types,
    # the scalar argument will cause the error:
    expect_error(gigs_fn(zyp, character(1), sex, family, double(1)),
                 class = "gigs_string_not_char")

    # G5.2 --> errors if vector inputs cannot be recycled
    expect_error(gigs_fn(rep_len(zyp, 2), x, sex, family, acronym),
                 class = "gigs_err_unrecyclable")
    expect_error(gigs_fn(rep_len(zyp, 3), rep_len(x, 2),
                         sex, family, acronym),
                 class = "gigs_err_unrecyclable")
    expect_error(gigs_fn(rep(zyp, 3), rep_len(x, 3),
                         rep_len(sex, 2), family, acronym),
      class = "gigs_err_unrecyclable")
  }
})

# Appropriate handling of non-fatal issues with input data ---------------------

#' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
#' @srrstats{G5.8, G5.8d} Show that gigs can handle data outside the
#'   scope of its algorithms by replacing bad inputs with `NA`, and simply
#'   outputting `NA` for the output elements which map onto `NA` input
#'   elements.
#' @srrstats {EA6.0, EA6.0a} Also checks that classes/types of returned objects
#'   are correct using [checkmate::expect_numeric()]
test_that(desc = "Invalid values can be replaced with `NA` quietly", {
  # Make gigs *quietly* replace bad data with NA
  gigs_input_options_set(new_value = "quiet", silent = TRUE)

  # Random set of indices to replace with bad data in each case
  num_to_replace1 <- 85
  withr::with_seed(200, code = {
    replace_ints_1 <- sample(150:450, size = num_to_replace1)
  })
  num_to_replace2 <- 125
  withr::with_seed(400, code = {
    replace_ints_2 <- sample(150:450, size = num_to_replace2)
  })
  replace_ints_3 <- union(replace_ints_1, replace_ints_2)
  num_to_replace3 <- length(union(replace_ints_1, replace_ints_2))

  family <- "who_gs"
  acronym <- "wfh"
  x <- gigs::who_gs[[acronym]][[1]][[1]][[1]]
  len_x <- length(x)
  sex <- withr::with_seed(25, {
    sample(c("M", "F"), replace = TRUE, size = len_x)
  })

  for (fn_name in conv_fns) {
    gigs_fn <- getFromNamespace(fn_name, ns = "gigs")
    zyp <- rep(zyp_cache[[fn_name]], len_x)
    zyp_name <- zyp_name_cache[[fn_name]]

    # Bad input 1: Undefined data (NaN, Inf, -Inf) -----------------------------
    for (undefined_val in c(NaN, Inf, -Inf)) {
      ## Replace z/y/p variable and x variable with undefined_val
      out <- gigs_fn(replace(zyp, replace_ints_1, undefined_val),
                     replace(x, replace_ints_2, undefined_val),
                     sex, family, acronym)
      checkmate::expect_numeric(out, len = len_x)
      expect_true(all(is.na(out[replace_ints_3])))
    }

    # Bad input 2: Missing data (NA) -------------------------------------------
    ## Replace x variable with NA
    out <- gigs_fn(zyp,
                   replace(x, replace_ints_2, values = NA),
                   sex, family, acronym)
    checkmate::expect_numeric(out, len = len_x)
    expect_true(all(is.na(out[replace_ints_2])))
    ## Replace z/y/p variable and x variable with NA
    out <- gigs_fn(replace(zyp, replace_ints_1, values = NA),
                   x,
                   replace(sex, replace_ints_2, values = NA),
                   family, acronym)
    checkmate::expect_numeric(out, len = len_x)
    expect_true(all(is.na(out[replace_ints_3])))

    # Bad input 3: Out of bounds centiles (not `> 0` and `< 1`) ----------------
    if (fn_name == "centile2value") {
      out <- gigs_fn(replace(zyp, replace_ints_1, values = 2.5),
                     x, sex, family, acronym)
      checkmate::expect_numeric(out, len = len_x)
      expect_true(all(is.na(out[replace_ints_1])))
      out <- gigs_fn(replace(zyp, replace_ints_2, values = 2.5),
                     x, sex, family, acronym)
      checkmate::expect_numeric(out, len = len_x)
      expect_true(all(is.na(out[replace_ints_2])))
    }
    
    # Bad input 4: Out of bounds `x` variable ----------------------------------
    below_lower_bound <- min(x) - replace_ints_1
    above_upper_bound <- max(x) + replace_ints_2
    withr::with_seed(500, code = {
      out_of_bounds <- sample(c(below_lower_bound, above_upper_bound),
                              size = num_to_replace3, replace = FALSE)
    })
    out <- gigs_fn(zyp,
                   replace(x, replace_ints_2, above_upper_bound),
                   sex, family, acronym)
    checkmate::expect_numeric(out, len = len_x)
    expect_true(all(is.na(out[replace_ints_2])))
    out <- gigs_fn(zyp,
                   replace(x, replace_ints_3, out_of_bounds),
                   sex, family, acronym)
    checkmate::expect_numeric(out, len = len_x)
    expect_true(all(is.na(out[replace_ints_3])))
    
    # Bad input 5: Non-"M"/"F" `sex` variables ---------------------------------
    out <- gigs_fn(zyp, x,
                   replace(sex, replace_ints_2, "invalid_sex"),
                   family, acronym)
    checkmate::expect_numeric(out, len = len_x)
    expect_true(all(is.na(out[replace_ints_2])))
    out <- gigs_fn(zyp, x,
                   replace(sex, replace_ints_3, "invalid_sex"),
                   family, acronym)
    checkmate::expect_numeric(out, len = len_x)
    expect_true(all(is.na(out[replace_ints_3])))
  }
})

# Appropriate warnings/errors with bad input data ------------------------------

#' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
#' @srrstats{G5.8, G5.8d} Show that gigs can handle data outside the
#'   scope of its algorithms by replacing bad inputs with `NA` and giving clear
#'   warnings.
test_that(desc = "Invalid values can be replaced with `NA` with a warning", {
  # Make gigs *noisily* replace bad data with NA
  gigs_input_options_set(new_value = "warn", silent = TRUE)
  
  # Random set of indices to replace with bad data in each case
  num_to_replace1 <- 30
  withr::with_seed(800, code = {
    replace_ints_1 <- sample(50:110, size = num_to_replace1)
  })
  num_to_replace2 <- 65
  withr::with_seed(400, code = {
    replace_ints_2 <- sample(10:110, size = num_to_replace2)
  })
  replace_ints_3 <- union(replace_ints_1, replace_ints_2)
  num_to_replace3 <- length(union(replace_ints_1, replace_ints_2))
  
  family <- "ig_nbs"
  acronym <- "hcfga"
  x <- gigs::ig_nbs[[acronym]][[1]][[1]][[1]]
  len_x <- length(x)
  sex <- withr::with_seed(25, {
    sample(c("M", "F"), replace = TRUE, size = len_x)
  })
  
  for (fn_name in conv_fns) {
    gigs_fn <- getFromNamespace(fn_name, ns = "gigs")
    zyp <- rep(zyp_cache[[fn_name]], len_x)
    zyp_name <- zyp_name_cache[[fn_name]]
    
    # Bad input 1: Undefined data (NaN, Inf, -Inf) -----------------------------
    for (undefined_val in c(NaN, Inf, -Inf)) {
      ## Replace z/y/p variable and x variable with undefined_val
      warnings <- capture_warnings(
        gigs_fn(replace(zyp, replace_ints_2, undefined_val),
                replace(x, replace_ints_3, undefined_val),
                sex, family, acronym)
      )
      expect_match(warnings,
                   test_msg_undefined(zyp_name, len_x, num_to_replace2))
      expect_match(warnings,
                   test_msg_undefined("x", len_x, num_to_replace3))
    }
    
    # Bad input 2: Missing data (NA) -------------------------------------------
    ## Replace z/y/p variable and x variable with NA
    warnings <- capture_warnings(
      gigs_fn(replace(zyp, replace_ints_2, values = NA),
              replace(x, replace_ints_3, values = NA),
              replace(sex, replace_ints_1, values = NA),
              family, acronym)
    )
    expect_match(warnings,
                 test_msg_missing(zyp_name, len_x, num_to_replace2),
                 fixed = TRUE)
    expect_match(warnings,
                 test_msg_missing("x", len_x, num_to_replace3),
                 fixed = TRUE)
    expect_match(warnings,
                 test_msg_missing("sex", len_x, num_to_replace1),
                 fixed = TRUE)
    
    # Bad input 3: Out of bounds centiles (not `> 0` and `< 1`) ----------------
    if (fn_name == "centile2value") {
      expect_warning(
        gigs_fn(replace(zyp, replace_ints_1, values = 2.5),
                x, sex, family, acronym),
        test_msg_centile_oob(len_x, num_to_replace1)
      )
      expect_warning(
        gigs_fn(replace(zyp, replace_ints_2, values = 2.5),
                x, sex, family, acronym),
        test_msg_centile_oob(len_x, num_to_replace2)
      )
    }
    
    # Bad input 4: Out of bounds `x` variable ----------------------------------
    below_lower_bound <- min(x) - replace_ints_1
    above_upper_bound <- max(x) + replace_ints_2
    out_of_bounds <-withr::with_seed(500, code = {
      sample(c(below_lower_bound, above_upper_bound),
             size = num_to_replace3, replace = FALSE)
    })
    expect_warning(
      gigs_fn(zyp, replace(x, replace_ints_2, above_upper_bound), sex, family,
              acronym),
      test_msg_xvar_oob("x", len_x, num_to_replace2)
    )
    expect_warning(
      gigs_fn(zyp, replace(x, replace_ints_3, out_of_bounds), sex, family,
              acronym),
      test_msg_xvar_oob("x", len_x, num_to_replace3)
    )
    
    # Bad input 5: Non-"M"/"F" `sex` variables ---------------------------------
    expect_warning(
      gigs_fn(zyp, x, replace(sex, replace_ints_2, "invalid_sex"),
              family, acronym),
      test_msg_sex_invalid(len_x, num_to_replace2)
    )
    expect_warning(
      gigs_fn(zyp, x, replace(sex, replace_ints_3, "invalid_sex"),
              family, acronym),
      test_msg_sex_invalid(len_x, num_to_replace3)
    )
  }
})

#' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
#' @srrstats{G5.8, G5.8d} Show that gigs can handle data outside the
#'   scope of its algorithms by issuing clear errors, if users want this
#'   behaviour.
test_that("GIGS conversion functions can throw informative errors", {
  # Make gigs error on bad data
  gigs_input_options_set(new_value = "error", silent = TRUE)
  
  # Random set of indices to replace with bad data in each case
  num_to_replace1 <- 30
  withr::with_seed(100, code = {
    replace_ints_1 <- sample(50:110, size = num_to_replace1)
  })
  num_to_replace2 <- 25
  withr::with_seed(450, code = {
    replace_ints_2 <- sample(10:110, size = num_to_replace2)
  })
  replace_ints_3 <- union(replace_ints_1, replace_ints_2)
  num_to_replace3 <- length(union(replace_ints_1, replace_ints_2))
  
  family <- "ig_nbs"
  acronym <- "wfga"
  x <- gigs::ig_nbs[[acronym]][[1]][[1]][[1]]
  len_x <- length(x)
  sex <- withr::with_seed(25, {
    sample(c("M", "F"), replace = TRUE, size = len_x)
  })
  
  for (fn_name in conv_fns) {
    gigs_fn <- getFromNamespace(fn_name, ns = "gigs")
    zyp <- rep(zyp_cache[[fn_name]], len_x)
    zyp_name <- zyp_name_cache[[fn_name]]
    
    # Bad input 1: Undefined data (NaN, Inf, -Inf) -----------------------------
    for (undefined_val in c(NaN, Inf, -Inf)) {
      ## Replace z/y/p variable and x variable with undefined_val
      expect_error(
        gigs_fn(replace(zyp, replace_ints_2, undefined_val),
                replace(x, replace_ints_3, undefined_val),
                sex, family, acronym),
        test_msg_undefined("x", len_x, num_to_replace3)
      )
    }
    
    # Bad input 2: Missing data (NA) -------------------------------------------
    ## Replace only x variable with NA
    expect_error(
      gigs_fn(zyp, replace(x, replace_ints_3, values = NA),
              sex, family, acronym),
      test_msg_missing("x", len_x, num_to_replace3),
      fixed = TRUE
    )
    ## Replace z/y/p variable, x and sex variables with NA
    expect_error(
      gigs_fn(replace(zyp, replace_ints_2, values = NA),
              replace(x, replace_ints_3, values = NA),
              replace(sex, replace_ints_1, values = NA),
              family, acronym),
      test_msg_missing("sex", len_x, num_to_replace1),
      fixed = TRUE
    )
    # Bad input 3: Out of bounds centiles (not `> 0` and `< 1`) ----------------
    if (fn_name == "centile2value") {
      expect_error(
        gigs_fn(replace(zyp, replace_ints_1, values = 2.5),
                x, sex, family, acronym),
        test_msg_centile_oob(len_x, num_to_replace1)
      )
      expect_error(
        gigs_fn(replace(zyp, replace_ints_2, values = 2.5),
                x, sex, family, acronym),
        test_msg_centile_oob(len_x, num_to_replace2)
      )
    }
    # Bad input 4: Out of bounds `x` variable ----------------------------------
    below_lower_bound <- min(x) - replace_ints_1
    above_upper_bound <- max(x) + replace_ints_2
    out_of_bounds <-withr::with_seed(500, code = {
      sample(c(below_lower_bound, above_upper_bound),
             size = num_to_replace3, replace = FALSE)
    })
    expect_error(
      gigs_fn(zyp, replace(x, replace_ints_2, above_upper_bound), sex, family,
              acronym),
      test_msg_xvar_oob("x", len_x, num_to_replace2)
    )
    expect_error(
      gigs_fn(zyp, replace(x, replace_ints_3, out_of_bounds), sex, family,
              acronym),
      test_msg_xvar_oob("x", len_x, num_to_replace3)
    )
    # Bad input 5: Non-"M"/"F" `sex` variables ---------------------------------
    expect_error(
      gigs_fn(zyp, x, replace(sex, replace_ints_2, "invalid_sex"),
              family, acronym),
      test_msg_sex_invalid(len_x, num_to_replace2))
    expect_error(
      gigs_fn(zyp, x, replace(sex, replace_ints_2, "invalid_sex"),
              family, acronym),
      test_msg_sex_invalid(len_x, num_to_replace2))
  }
})


#' @srrstats {G5.8c} Show how the GIGS conversion functions handle all-`NA`
#'   inputs.
test_that("GIGS conversion functions can handle all-`NA` inputs", {
  # Make gigs issue warnings when confronted with suboptimal inputs
  gigs_input_options_set(new_value = "warn", silent = TRUE)
  
  family <- "ig_png"
  acronym <- "hcfa"
  x <- gigs::ig_png[[acronym]][[1]][[1]][[1]]
  len_x <- length(x)
  sex <- withr::with_seed(25, {
    sample(c("M", "F"), replace = TRUE, size = len_x)
  })

  for (fn_name in conv_fns) {
    gigs_fn <- getFromNamespace(fn_name, ns = "gigs")
    zyp <- rep(zyp_cache[[fn_name]], len_x)
    zyp_name <- zyp_name_cache[[fn_name]]

    all_NA <- rep_len(NA, len_x)

    # Will produce all NAs; with a warning
    expect_warning(
      gigs_fn(all_NA, x, sex, family, acronym),
      test_msg_missing(zyp_name, len_x, len_x),
      fixed = TRUE
    )
    # Will produce all NAs; with a warning
    expect_warning(
      gigs_fn(zyp, all_NA, sex, family, acronym),
      test_msg_missing("x", len_x, len_x),
      fixed = TRUE
    )
    # Will produce all NAs; with a warning
    expect_warning(
      gigs_fn(zyp, x, all_NA, family, acronym),
      test_msg_missing("sex", len_x, len_x),
      fixed = TRUE
    )
  }
})

#' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
test_that("GIGS conversion functions warn on NULL y/z/p", {
  family <- "ig_fet"
  acronym <- "hcfga"
  x <- gigs::ig_fet[[acronym]][[1]][[1]][[1]]
  len_x <- length(x)

  for (fn_name in conv_fns) {
    gigs_fn <- getFromNamespace(fn_name, ns = "gigs")
    zyp <- rep(zyp_cache[[fn_name]], len_x)
    zyp_name <- zyp_name_cache[[fn_name]]

    expect_error(
      gigs_fn(NULL, x, sex = NULL, family, acronym),
      regexp = "must not be `NULL`."
    )
  }
})

test_that("GIGS conversion functions warn on bad `family` or `acronym`", {
  family <- "ig_fet"
  acronym <- "bfpfga"
  x <- gigs::ig_png[[acronym]][[1]][[1]][[1]]
  len_x <- length(x)
  sex <- withr::with_seed(25, {
    sample(c("M", "F"), replace = TRUE, size = len_x)
  })

  for (fn_name in conv_fns) {
    gigs_fn <- getFromNamespace(fn_name, ns = "gigs")
    zyp <- rep(zyp_cache[[fn_name]], len_x)
    zyp_name <- zyp_name_cache[[fn_name]]

    expect_error(
      gigs_fn(zyp, x, sex, "BAD FAMILY", acronym),
      regexp = "`family` was \"BAD FAMILY\"",
      class = "gigs_string_bad_choice"
    )

    expect_error(
      gigs_fn(zyp, x, sex, family, "BAD ACRONYM"),
      regexp = "`acronym` was \"BAD ACRONYM\"",
      class = "gigs_string_bad_choice"
    )
  }
})

# Testing of `report_units()` --------------------------------------------------

#' @srrstats {G5.4, G5.4c} Tests ensure that `gigs` functions can be used to
#'   replicate published growth charts, within a tolerance.
#' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
test_that("The `report_units()` function works as expected", {
  family <- "ig_fet"
  acronym  <- "hefwfga"
  expect_message(report_units(family = family, acronym = acronym),
                 regexp = "from the INTERGROWTH-21st Fetal Standards")

  family <- "ig_nbs"
  acronym  <- "wfga"
  expect_message(report_units(family = family, acronym = acronym),
                 regexp = "from the INTERGROWTH-21st Newborn Size Standards")

  family <- "ig_png"
  acronym  <- "wfa"
  expect_message(report_units(family = family, acronym = acronym),
                 regexp = "the INTERGROWTH-21st Postnatal Growth Standards")

  family <- "who_gs"
  acronym  <- "wfa"
  expect_message(report_units(family = family, acronym = acronym),
                 regexp = "from the WHO Child Growth Standards")

  # Should issue specific errors if bad data are provided...
  expect_error(
    report_units(family = "BAD FAMILY", acronym = acronym),
    regexp = "`family` was \"BAD FAMILY\"",
    class = "gigs_string_bad_choice"
  )
  expect_error(
    report_units(family = family, acronym = "BAD ACRONYM"),
    regexp = "`acronym` was \"BAD ACRONYM\"",
    class = "gigs_string_bad_choice"
  )

  # ... including not giving character data...
  expect_error(
    report_units(family = character(), acronym = acronym),
    class = "gigs_string_bad_length"
  )
  expect_error(
    report_units(family = family, acronym = character()),
    class = "gigs_string_bad_length"
  )

  # ... or providing non-character data...
  expect_error(
    report_units(family = integer(1), acronym = acronym),
    class = "gigs_string_not_char"
  )
  expect_error(
    report_units(family = family, acronym = complex(1)),
    class = "gigs_string_not_char"
  )
})