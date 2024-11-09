# Error message functions for testing gigs package -----------------------------

#' Generate an expected bad data type error from `{checkmate}`
#' @param name Single-length character vector with name of input.
#' @param wanted Single-length character vector with name of expected data
#'   type.
#' @param got Single-length character vector with name of received data
#'   type.
#' @returns A single-length character vector which describes an error message
#'   that gigs should output when inputs are the wrong type.
#' @description Used to test `{checkmate}` error messages. These errors are
#'   given by checkmate::assert_*()-style functions in check_params.R.
#' @noRd
test_error_wrong_type <- function(name, wanted, got) {
  paste0("Assertion on '", name, "' failed(:|\\.) Must be of (class|type) '",
         wanted, "', not '", got, "'\\.")
}

#' Generate an expected zero-length error from [err_input_is_zero_length]
#' @param names A character vector with variable names that had length zero.
#' @returns A single-length character vector which describes an error message
#'   that gigs should output for zero-length inputs.
#' @noRd
test_error_zero_length <- function(names) {
  count <- length(names)
  var_str <- if (count > 1) "Variables" else "Variable"
  input_str <- if (count > 1) "Inputs" else "Input"
  varnames_str <- paste0(names, collapse = "`, `")
  paste0(var_str, " `", varnames_str, "`: ", input_str, " had length 0, ",
         "but must have length 1 or greater.")
}

#' Generate an unrecyclable vectors error from [err_inputs_unrecyclable()].
#' @param names A character vector with variable names that had length zero.
#' @returns A single-length character vector which describes an error message
#'   that gigs should output for unrecyclable data.
#' @noRd
test_error_unrecyclable <- function(names) {
  count <- length(names)
  var_str <- if(count > 1) "Variables" else "Variable"
  input_str <- if(count > 1) "Inputs" else "Input"
  varname_str <- paste0(names, collapse = "', '")
  paste0(var_str, " '", varname_str, "': ", input_str, " cannot be ",
         "recycled with `vctrs\\:\\:vec_recycle_common\\(\\)`.")
}

#' Generate a 'your acronym is too long!' error, like from [validate_acronym()]
#' @param length The length of the non-scalar acronym data, used to generate
#'   the error message
#' @returns A single-length character vector which describes an error message
#'   that gigs should output for non-scalar acronym inputs.
test_error_acronym_not_scalar <- function(length) {
  paste0("Assertion on 'acronym' failed. Must be of length == 1, but has ",
         "length ", length, ".")
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
  paste0("Argument `", name, "`: ", int_undefined,
         " in ", length, " elements were undefined \\(`NaN`,",
         " `Inf`, or `-Inf`\\).")
}

#' Replicate error/warning messages from gigs for bad input data (TESTING
#' ONLY)
#' @param varname Single-length character vector with a variable name.
#' @param len Expected length of the variable specified by `name`.
#' @param int_undefined Number of elements in `name` which were expected to
#'   be missing in whatever test is calling this function.
#' @note Used for testing only. Mimics the output of [msg_missing_data()].
test_msg_missing <- function(varname, len, int_undefined) {
  lgl <- rep_len(x = FALSE, length.out = len)
  force(int_undefined)
  lgl[(1:int_undefined)] <- TRUE
  msg_missing_data(lgl_missing_data = lgl, varname = varname)
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
  paste0("Argument `p`: ", int_undefined, " in ", length,
         " elements were not between 0 and 1.")
}

#' Replicate error/warning messages from gigs for bad input data (TESTING
#' ONLY)
#' @param x_name Single-length character vector with the `x` variable name.
#' @param length Expected length of the variable specified by `name`.
#' @param int_undefined Number of elements in `x` which were expected to
#'   be outside the proper bounds (for each standard/acronym) in whatever test
#'   is calling this function.
#' @note Used for testing only. Mimics the output of [msg_oob_xvar()].
test_msg_xvar_oob <- function(x_name, length, int_undefined) {
  paste0("Argument `", x_name, "`: ", int_undefined, " in ", length,
         " elements were out-of-bounds \\(see the GIGS conversion",
         " function documentation for bounds\\).")
}


#' Replicate error/warning messages from gigs for bad input data (TESTING
#' ONLY)
#' @param length Expected length of the variable specified by `name`.
#' @param int_undefined Number of elements in `x` which were expected to
#'   be invalid (not in `names(gigs::ig_nbs)`) in whatever test is calling this
#'   function.
#' @param standard Collection of growth standards in use. Should be one of
#'   `"ig_fet"`, `"ig_nbs"`, `"ig_png"`, or `"who_gs"`. Default = `"ig_nbs"`.
#' @note Used for testing only. Mimics the output of
#'   [msg_invalid_sex_chrs()].
#' @noRd
test_msg_sex_invalid <- function(len, int_undefined) {
  lgl <- rep_len(x = FALSE, length.out = len)
  force(int_undefined)
  lgl[(1:int_undefined)] <- TRUE
  msg_invalid_sex_chrs(lgl_invalid_sex = lgl, varname = "sex")
}

#' Replicate error/warning messages from gigs for bad input data (TESTING
#' ONLY)
#' @param varname Scalar character variable.
#' @param length_wanted Scalar numeric vector denoting expected length of
#'   `varname`.
#' @param length_got Scalar numeric vector denoting actual length `varname`.
#' @note Used for testing only. Mimics the output of [checkmate::qassert()] when
#'   rules are `"S1"` but got non-scalar input.
#' @noRd
regex_error_wrong_length <- function(varname, length_wanted, length_got) {
  paste0("Assertion on '", varname,
         "' failed\\. Must be of length == ", length_wanted, ", but has ",
         "length ", length_got, "\\.")
}

#' Replicate error/warning messages from gigs for bad input data (TESTING
#' ONLY)
#' @param varname Name of a non-scalar variable (which should be scalar).
#' @note Used for testing only. Mimics the output of [checkmate::qassert()] when
#'   rules are `"S1"` but got non-scalar input.
#' @noRd
regex_error_not_scalar <- function(varname, length_got) {
  regex_error_wrong_length(varname, length_wanted = 1, length_got)
}