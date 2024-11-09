# Convert between values, z-scores, and centiles -------------------------------

#' Convert anthropometric measures to z-scores/centiles using international
#' growth standards
#' @param y Numeric vector of length one or more with anthropometric
#'   measurement(s) to convert to centiles/z-scores. Units depend on which
#'   combination of `family` and `acronym` are in use, which you can check with
#'   [report_units()]. By default, GIGS will warn you if elements in `y` are
#'   missing (`NA`) or undefined (`NaN`/`Inf`), and replace the latter with
#'   `NA`. You can customise this behaviour using the [GIGS package-level
#'   options][gigs_options].
#' @param x Numeric vector of x values, which take on different units depending
#'   on the growth standard in use. To know which units to use, run
#'   `report_units(family, acronym)`. Limits for each combination are as
#'   follows:
#'
#'   * WHO Growth Standards (family = `"who_gs"`)
#'     - Between 0 and 1856 days for `"wfa"`, `"bfa"`, `"lhfa"`, and `"hcfa"`.
#'     - Between 45 and 110 cm for `"wfl"`.
#'     - Between 65 and 120 cm for `"wfh"`.
#'     - Between 91 and 1856 days for `"acfa"`, `"ssfa"`, `"tsfa"`.
#'   * INTERGROWTH-21<sup>st</sup> Fetal Standards (family = `"ig_fet"`)
#'     - Between 98 and 280 days for `"hcfga"`, `"bpdfga"`, `"acfga"`,
#'       `"flfga"`, `"ofdfga"`, and<br> `"tcdfga"`.
#'     - Between 154 and 280 days for `"efwfga"`.
#'     - Between 112 and 294 days for `"sfhfga"`.
#'     - Between 58 and 105 days for `"crlfga"`.
#'     - Between 19 and 95 mm for `"gafcrl"`.
#'     - Between 105 and 280 days for `"gwgfga"`.
#'     - Between 168 and 280 days for `"pifga"`, `"rifga"`, and `"sdrfga"` (the
#'       INTERGROWTH-21<sup>st</sup> Fetal Doppler standards).
#'     - Between 12 and 55 mm for`"tcdfga"`.
#'     - Between 105 and 252 days for `"poffga"`, `"sffga"`, `"avfga"`, `"pvfga"`,
#'       and `"cmfga"` (the INTERGROWTH-21<sup>st</sup> Fetal Brain Development
#'       standards).
#'     - Between 126 and 287 days for `"hefwfga"` (the INTERGROWTH-21<sup>st</sup>
#'       standard for Hadlock-based estimated fetal weight).
#'   * INTERGROWTH-21<sup>st</sup> Newborn Size Standards (family = `"ig_nbs"`)
#'     - Between 168 and 300 days for `"wfga"`, `"lfga"`, `"hcfga"`, and
#'       `"wlrfga"`.
#'     - Between 266 and 280 days for `"fmfga"`, `"bfpfga"`, and `"ffmfga"`.
#'   * INTERGROWTH-21<sup>st</sup> Postnatal Growth Standards (family =
#'     `"ig_png"`)
#'     - Between 27 and 64 weeks for `"wfa"`, `"lfa"`, and `"hcfa"`.
#'     - Between 35 and 65 cm for `"wfl"`.
#'
#'   By default, gigs will replace out-of-bounds elements in `x` with `NA` and
#'   warn you. It will also warn you about elements in `x` which are missing
#'   (`NA`) or undefined (`NaN`/`Inf`). You can customise this behaviour using
#'   the [GIGS package-level options][gigs_options].
#' @param sex Character vector of length one or more with sex(es), either `"M"`
#'   (male) or `"F"` (female). This argument should be left as `NULL` (the
#'   default) if `family == "ig_fet"`. This argument is case-sensitive. By
#'   default, gigs will replace elements of `sex` which are not `"M"` or `"F"`
#'   with `NA` and warn you. You can customise this  behaviour using the [GIGS
#'   package-level options][gigs_options].
#' @param family A single string denoting the family of growth standards in use.
#'   Must be one of `"ig_fet"`, `"ig_nbs"`, `"ig_png"`, or `"who_gs"`, or the
#'   function will throw an error. This argument is case-sensitive.
#' @param acronym A single string denoting the specific growth standard in use.
#'   The allowed values of `acronym` depend on the value of `family`, and are
#'   listed in the documentation for the `x` argument. Incompatible `family` and
#'   `acronym` values will cause errors to be thrown. This argument is
#'   case-sensitive.
#' @srrstats {G2.3b} Explicit reference to `acronym` and `family`
#'   case-sensitivity.
#' @note Inputs other than `acronym` and `family` will be recycled by
#'   [vctrs::vec_recycle_common()], and must adhere to the [vctrs] recycling
#'   rules.
#' @seealso [report_units()] will tell you the units needed for `y` and `x`, for
#'   any combination of `family` and `acronym`.
#' @returns Numeric vector of z-scores/centiles with length equal to the longest
#'   input vector.
#' @examples
#' # Convert in the weight-for-age standard from the WHO Child Growth Standards
#' value2zscore(3, 0, "M", family = "who_gs", acronym = "wfa")
#'
#' # Or obtain centiles...
#' value2centile(3, 0, "M", family = "who_gs", acronym = "wfa")
#'
#' # Inputs will be recycled if necessary
#' value2centile(3, 0, c("M", "F"), family = "ig_nbs", acronym = "wfga")
#' @importFrom stats qnorm pnorm
#' @export
value2zscore <- function(y, x, sex = NULL, family, acronym) {
  inputs <- validate_inputs(y = y, x = x, sex = sex,
                            family = family, acronym = acronym)
  fn <- retrieve_internal_v2_fn(family)
  out <- do.call(what = fn, args = inputs[!names(inputs) == "family"])
  if (family == "ig_nbs") out <- qnorm(out)
  out
}

#' @rdname value2zscore
#' @export
value2centile <- function(y, x, sex = NULL, family, acronym) {
  inputs <- validate_inputs(y = y, x = x, sex = sex,
                            family = family, acronym = acronym)
  fn <- retrieve_internal_v2_fn(family)
  out <- do.call(what = fn, args = inputs[!names(inputs) == "family"])
  if (family != "ig_nbs") out <- pnorm(out)
  out
}

#' Convert z-scores/centiles to anthropometric measures using international
#' growth standards
#' @param z,p Numeric vector of length one or more with centiles/z-scores to
#'   convert to values. For `p`, gigs will warn you if elements of `p` are not
#'   between `0` and `1`. You can customise this behaviour using the [GIGS
#'   package-level options][gigs_options].
#' @returns Numeric vector of expected measurements with length equal to the
#'   longest input vector.
#' @examples
#' # Convert in the weight-for-age standard from the WHO Child Growth Standards
#' zscore2value(-2:2, 0, "M", family = "who_gs", acronym = "wfa")
#'
#' # Or obtain centiles...
#' centile2value(pnorm(-2:2), 0, "M", family = "who_gs", acronym = "wfa")
#' @inheritParams value2zscore
#' @rdname zscore2value
#' @export
zscore2value <- function(z, x, sex = NULL, family, acronym) {
  inputs <- validate_inputs(z = z, x = x, sex = sex, family = family,
                            acronym = acronym, yzp_name = "z")
  fn <- retrieve_internal_2v_fn(family)
  if (family == "ig_nbs") {
    names(inputs)[1] <- "p"
    inputs[[1]] <- pnorm(inputs[[1]])
  }
  do.call(what = fn, args = inputs[!names(inputs) == "family"])
}

#' @rdname zscore2value
#' @export
centile2value <- function(p, x, sex = NULL, family, acronym) {
  inputs <- validate_inputs(p = p, x = x, sex = sex, family = family,
                            acronym = acronym, yzp_name = "p")
  fn <- retrieve_internal_2v_fn(family)
  if (family != "ig_nbs") {
    names(inputs)[1] <- "z"
    inputs[[1]] <- qnorm(inputs[[1]])
  }
  do.call(what = fn, args = inputs[!names(inputs) == "family"])
}

#' Check user-inputted variables in GIGS conversion functions
#'
#' @param y,z,p Either a numeric vector of length one or more, or NULL. Checks
#'   will fail if more than one of these arguments are provided; see
#'   [validate_yzp()] documentation.
#' @param x Numeric vector of length one or more with user-inputted x values.
#' @param sex Character vector of length one or more with user-inputted sex
#'   values. Values which are not `"M"` or `"F"` will be replaced with `NA`.
#' @param family A scalar character vector with the family of growth standards
#'   in use.
#' @param acronym A scalar character vector with the specific acronym for the
#'   growth standard in use.
#' @param yzp_name,x_name Scalar character vectors with the name of the `y`/`x`
#'   argument. These should only be specified when calling `validate_inputs()`
#'   from  functions like [classify_sfga()], which call for specific `
#'   y`/`x` units.
#' @noRd
validate_inputs <- function(y = NULL,
                            z = NULL,
                            p = NULL,
                            x,
                            sex = NULL,
                            family,
                            acronym,
                            yzp_name = "y",
                            x_name = "x") {
  call <- rlang::caller_env()
  # Check if family/acronym are accurate to one-another ------------------------
  check_family(family = family, call = call)
  check_acronym(acronym = acronym, family = family, call = call)
  check_sex_null_status(family = family, sex = sex, call = call)
  # Check that other vectors are not zero-length and recyclable ----------------
  validate_parameter_lengths(y = y, z = z, p = p, x = x, sex = sex,
                             call = call,
                             yzp_name = yzp_name, x_name = x_name)

  catch_and_throw_validate_issues({
    if (!is.null(sex)) sex <- validate_sex(sex)
    x <- validate_xvar(x, family, acronym, varname = x_name)
    yzp <- validate_yzp(y = y, z = z, p = p, yzp_name = yzp_name)
  }, call = call)
  recycled <- vctrs::vec_recycle_common(
    y = yzp[[1]], z = yzp[[2]], p = yzp[[3]], x = x, sex = sex
  )
  recycled[["family"]] <- family
  recycled[["acronym"]] <- acronym
  vctrs::list_drop_empty(recycled)
}

#' Check required units for a conversion using gigs
#' @inheritParams value2zscore
#' @examples
#' # Get units for working with the IG-21st Newborn Size Standard for
#' # weight-for-GA
#' report_units("ig_nbs", "wfga")
#'
#' # Get units for working with the IG-21st Fetal Standard for
#' # gestational weight gain
#' report_units("ig_fet", "gwgfga")
#' @returns Returns a message describing which units are required for conversion
#'   in the desired growth standard.
#' @export
report_units <- function(family, acronym) {
  call <- rlang::current_env()
  # Check if family/acronym are accurate to one-another ------------------------
  check_family(family = family, call = call)
  check_acronym(acronym = acronym, family = family, call = call)
  chr_family <- switch(family,
                       ig_fet = "INTERGROWTH-21st Fetal Standards",
                       ig_nbs = "INTERGROWTH-21st Newborn Size Standards",
                       ig_png = "INTERGROWTH-21st Postnatal Growth Standards",
                       who_gs = "WHO Child Growth Standards")
  data <- get(family, envir = asNamespace("gigs"))[[acronym]]
  xvar <- gigs_var_strings[[data[["x"]]]]
  yvar <- gigs_var_strings[[data[["y"]]]]
  cli::cli_inform(
    c("You're using {.val {acronym}} from the {chr_family} ({.val {family}}).",
      "i" = "Units for {.var y}: {.strong {yvar}}.",
      "i" = "Units for {.var x}: {.strong {xvar}}."
    )
  )
}

#' Retrieve an internal 'value2zscore/value2centile' function from GIGS
#' @param family A scalar string denoting the family of growth standards in use.
#'   Will be one of `c("ig_fet", "ig_nbs", "ig_png", "who_gs")`.
#' @returns An internal conversion function from gigs.
#' @rdname retrieve_internal_conv_fns
#' @noRd
retrieve_internal_v2_fn <- function(family) {
  string <- paste0(c(family, switch(family, ig_nbs = "v2c", "v2z"), "internal"),
                   collapse = "_")
  get(string, envir = asNamespace("gigs"))
}

#' @rdname retrieve_internal_conv_fns
#' @noRd
retrieve_internal_2v_fn <- function(family) {
  string <- paste0(c(family, switch(family, ig_nbs = "c2v", "z2v"), "internal"),
                   collapse = "_")
  get(string, envir = asNamespace("gigs"))
}


# Parameter checking - family + acronym only -----------------------------------

#' Check that strings (i.e. 1-length character vector inputs) are appropriate
#' when using GIGS conversion functions
#' @param string,family,acronym A character vector input to a GIGS conversion
#'   function which should be scalar, and be one of specific values. This is
#'   either `family` or `acronym`.
#' @param options A character vector with options, one of which should match
#'   `string`.
#' @param call An rlang <call> object, used to pass down context for throwing
#'   better errors.
#' @returns Aborts if `string` is not a character vector, is non-scalar, or is
#'   not one of `options`.
#' @rdname check_string_input
#' @noRd
check_string_input <- function(string,
                               options,
                               call) {
  varname <- deparse(substitute(string))
  if (!checkmate::test_character(x = string, typed.missing = TRUE)) {
    cli::cli_abort(
      c("!" = "{.var {varname}} must be a {.cls character} vector.",
        "x" = "You supplied an object of class {.cls {class(string)}}."),
      class = "gigs_string_not_char",
      call = call
    )
  }
  if (!checkmate::test_string(string)) {
    cli::cli_abort(
      c("!" = "{.var {varname}} must have length 1.",
        "x" = "{.var {varname}} had {.val {length(string)}} elements."),
      class = "gigs_string_bad_length",
      call = call
    )
  }
  if (!checkmate::test_subset(string, choices = options)) {
    opts <- cli::cli_vec(options, list("vec-trunc" = 30, "vec-last" = " or "))
    cli::cli_abort(
      c("!" = "{.var {varname}} must be one of {.val {opts}}.",
        "x" = "{.var {varname}} was {.val {string}}."),
      class = "gigs_string_bad_choice",
      call = call
    )
  }
}

#' @rdname check_string_input
#' @noRd
check_family <- function(family, call) {
  check_string_input(string = family,
                     options = gigs_family_names,
                     call = call)
}

#' @rdname check_string_input
#' @noRd
check_acronym <- function(acronym, family, call) {
  check_string_input(
    string = acronym,
    options = names(getExportedValue(ns = "gigs", name = family)),
    call = call
  )
}

#' Check that users have/have not supplied sex information to GIGS conversion
#' functions
#' @param family A scalar character vector denoting which family of growth
#'   standards are in use.
#' @param sex A character vector of user-inputted sex values. This function
#'   only checks whether `sex` is `NULL`, not whether `sex` is a character
#'   vector itself.
#' @param call An rlang <call> used to provide better context for thrown error
#'   messages.
#' @return Throws an error if appropriate, or invisibly returns `NULL`.
#' @noRd
check_sex_null_status <- function(family, sex, call) {
  if (family == "ig_fet") {
    if (!is.null(sex)) {
      cli::cli_abort(
        c("!" = c("{.var sex} must be {.var NULL} when {.var family} is ",
                  "{.val ig_fet}.")),
        class = "gigs_sex_not_null_for_ig_fet",
        call = call
      )
    }
  } else {
    if (is.null(sex)) {
      cli::cli_abort(
        c("!" = c("{.var sex} must not be {.var NULL} when {.var family} is ",
                  "not {.val ig_fet}.")),
        class = "gigs_sex_null_for_not_ig_fet",
        call = call
      )
    }
  }
  invisible()
}


# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.0} Primary literature referenced for each exported function,
#'   and for internal functions.
#' @srrstats {G1.4, G1.4a} All functions in file documented using
#'   `{roxygen2}`.
#' @srrstats {G2.0a, G2.1a, EA1.3} Exported function in this file document
#'   expectations on the length of inputs and their data types.
#' @srrstats {G2.0, G2.1, G2.2, G2.3, G2.3a, G2.6} These standards
#'   are met in all exported functions by passing inputs to [validate_inputs()].
#'   All internal functions in this script are provided with vectors that have
#'   already been validated.
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.16} These standards are met
#'   in all exported functions by passing inputs to [validate_inputs()]. All
#'   internal functions in this script are provided with vectors that have
#'   already checked for missing/undefined/out-of-bounds data.
NULL
