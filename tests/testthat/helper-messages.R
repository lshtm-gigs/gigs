# Error message functions for testing gigs package -----------------------------

#' Generate an expected bad data type error from `{checkmate}`
#' @param name Single-length character vector with name of input.
#' @param wanted Single-length character vector with name of expected data
#'   type.
#' @param got Single-length character vector with name of received data
#'   type.
#' @description Used to test `{checkmate}` error messages. These errors are
#'   given by checkmate::assert_*()-style functions in check_params.R.
test_error_wrong_type <- function(name, wanted, got) {
  paste0("Assertion on '", name, "' failed: Must be of type '", wanted,
         "', not '", got, "'.")
}

#' Generate an expected zero-length error from [validate_parameter_lengths]
#' @param name Name of input vector.
test_error_zero_length <- function(names) {
  count <- length(names)
  var_str <- if(count > 1) "Variables" else "Variable"
  input_str <- if(count > 1) "Inputs" else "Input"
  varnames_str <- paste0(names, collapse = "', '")
  paste0(var_str, " '", varnames_str, "': ", input_str, " had length 0, ",
         "but must have length 1 or greater.")
}

#' Generate an expected zero-length error from `{checkmate}`
#' @param name Name of input vector.
test_error_unrecyclable <- function(names) {
  count <- length(names)
  var_str <- if(count > 1) "Variables" else "Variable"
  input_str <- if(count > 1) "These inputs" else "This input"
  varname_str <- paste0(names, collapse = "', '")
  paste0(var_str, " '", varname_str, "': ", input_str, " cannot be ",
         "recycled with `vctrs\\:\\:vec_recycle_common\\(\\)`.")
}

# Error/warning string functions for .gigs_options issues ----------------------

#' Replicate error/warning messages from gigs for bad input data (TESTING
#' ONLY)
#' @param name Single-length character vector with a variable name.
#' @param length Expected length of the variable specified by `name`.
#' @param int_undefined Number of elements in `name` which were expected to
#'   be undefined by whatever test is calling this function.
#' @note Used for testing only. Mimics the output of [msg_undefined_data()].
test_msg_undefined <- function(name, length, int_undefined) {
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
test_msg_missing <- function(name, length, int_undefined) {
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
test_msg_centile_oob <- function(length, int_undefined) {
  paste0("Variable 'p': ", int_undefined, " in ", length,
         " elements were not between 0 and 1.")
}

#' Replicate error/warning messages from gigs for bad input data (TESTING
#' ONLY)
#' @param x_name Single-length character vector with the `x` variable name.
#' @param length Expected length of the variable specified by `name`.
#' @param int_undefined Number of elements in `x` which were expected to
#'   be outside the proper bounds (for each standard/acronym) in whatever test
#'   is calling this function.
#' @param standard Single-length character vector denoting the collection of
#'   growth standards in use. Should be one of `"ig_fet"`, `"ig_nbs"`,
#'   `"ig_png"`, or `"who_gs"`.
#' @note Used for testing only. Mimics the output of [msg_oob_xvar()].
test_msg_xvar_oob <- function(x_name, length, int_undefined, standard) {
  paste0("Variable '", x_name, "': ", int_undefined, " in ", length,
         " elements were out-of-bounds \\(see the '", standard, "' conversion ",
         "functions documentation\\).")
}

#' Replicate error/warning messages from gigs for bad input data (TESTING
#' ONLY)
#' @param var Single-length character vector with a variable name, either
#'   `"sex"` or `"acronym"`.
#' @param length Expected length of the variable specified by `name`.
#' @param int_undefined Number of elements in `x` which were expected to
#'   be invalid (not in `names(gigs::ig_nbs)`) in whatever test is calling this
#'   function.
#' @param standard Collection of growth standards in use. Should be one of
#'   `"ig_fet"`, `"ig_nbs"`, `"ig_png"`, or `"who_gs"`.
#' @note Used for testing only. Mimics the output of
#'   [msg_invalid_sex_acronym()].
#' @noRd
test_msg_acronym_sex_invalid <- function(var, length, int_undefined, standard) {
  see_sentence <- paste0("See the '", standard, "' documentation for valid ",
                         "'acronym' values.")
  if (var == "sex") {
    paste0("Variable 'sex': ", int_undefined, " in ", length, " elements were ",
           "neither \"M\" nor \"F\".")
  } else {
    paste0("Variable '", var,"': ", int_undefined, " in ", length,
           " elements were invalid. ", see_sentence)
  }
}