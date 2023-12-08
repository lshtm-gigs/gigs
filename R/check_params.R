# General parameter checking for conversion in growth standards ----------------

#' Validate the first argument (i.e. `y`/`z`/`p`) prior to growth standard
#' conversion
#'
#' @param y,z,p Either `NULL` or a numeric vector with length equal to or
#'   greater than one.
#' @note The function will `stop()` if more than one of `y`, `z`, or `p` are not
#'   `NULL`.
#' @returns Returns `vec`. Throws an error if `vec` is not numeric, is not at
#'   least length 1, or if all values in `vec` are missing.
#' @noRd
validate_yzp <- function(y = NULL, z = NULL, p = NULL) {
  args <- list(y = y, z = z, p = p)
  arg_nulls <- c(y = is.null(y), z = is.null(z), p = is.null(p))
  stopifnot(sum(arg_nulls) == 2) # Error should never be seen by user
  vec <- args[!arg_nulls][[1]]
  varname <- names(arg_nulls)[!arg_nulls]
  checkmate::assert_numeric(vec, min.len = 1, all.missing = FALSE,
                            .var.name = varname)
  if (!arg_nulls[["p"]]) {
    vec <- replace(vec, !inrange(vec, c(0,1)), NA)
  }
  assign(x = varname, value = vec, envir = parent.frame(n = 1))
}

#' Validate user-inputted x variables prior to growth standard conversion
#'
#' @param x A vector of `x` values to check, in the sense of converting in a
#'   growth standard.
#' @param range Range within which `x` values are expected to be. Values outside
#'   this range will be set to `NA` with `replace()`.
#' @param rounded A single logical value. If `TRUE`, `x` values will be set to
#'   `NA` if they are not divisible by zero. This is important for the
#'   INTERGROWTH-21st PNG standards which use post-menstrual age in whole weeks.
#' @returns A numeric vector identical to `x`, except values outside `lower` and
#'   `upper` are replaced with NAs. Throws an error if `x` is not numeric.
#' @noRd
validate_xvar <- function(x, range, rounded = FALSE) {
  checkmate::qassert(rounded, rules = "B1")
  checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)
  out_of_range <- !inrange(x, range)
  lgl_replace <- if (!rounded) out_of_range else out_of_range & x %% 1 != 0
  replace(x = x, list = lgl_replace, values = NA_real_)
}

#' Validate user-inputted character vectors
#'
#' @param chr User-inputted character vector. Will be either `sex` or `acronym`.
#' @param options A range of options. If no values in `chr` are `%in% options`,
#'   the function will throw an appropriate-formatted error.
#' @returns The vector `chr`, with all elements where `!chr %in% options`
#'   replaced with `NA`.
#' @noRd
validate_chr <- function(chr, options) {
  varname <- deparse(substitute(chr))
  checkmate::assert_character(chr, min.len = 1, all.missing = FALSE,
                              .var.name = varname)
  invalid <- !chr %in% options
  if (all(invalid)) {
    len <- length(options)
    stop(paste0("No value in `", varname, "` was valid. Check your `", varname,
                "` parameter - it should contain at least one of \"",
                paste(options[seq_len(len - 1)], collapse = "\", \""),
                "\", or \"", options[len], "\"."),
         call. = FALSE)
  }
  replace(chr, invalid, NA_character_)
}

#' Validate user-inputted sexes prior to growth standard conversion
#'
#' @param sex Character vector with user-inputted sex values. Values not in
#' @returns Returns `sex`, with values not `%in% c("M", "F", "U")` set to `NA`.
#'   Will throw an error if `sex` is not a character vector.
#' @noRd
validate_sex <- function(sex) {
  validate_chr(sex, options = c("M", "F", "U"))
}

#' Validate user-inputted acronyms prior to growth standard conversion
#'
#' @param acronym Character vector with user-inputted acronym values.
#' @param allowed_acronyms Character vector with acceptable values for
#'   `acronym`. Members of `acronym` which are not `%in% allowed_acronyms` will
#'   be set to `NA`.
#' @returns Returns `sex`, with values not `%in% allowed_acronyms` set to `NA`.
#'   Will throw an error if `acronym` is not a character vector.
#' @noRd
validate_acronym <- function(acronym, allowed_acronyms) {
  validate_chr(acronym, options = allowed_acronyms)
}

# Standard-specific parameter validation ---------------------------------------

#' Check whether user-inputted `sex` and `acronym` values are valid in `who_gs`
#' functions
#'
#' @param y,z,p Either a numeric vector or NULL. Checks will fail if more than
#'   one of these arguments are provided; see [validate_yzp()] documentation.
#' @param x Numeric vector with user-inputted x values.
#' @param sex Character vector with user-inputted sex values.
#' @param acronym Character vector with user-inputted acronym values.
#' @returns List with names `"sex"` and `"acronym"` contain vectors where
#'   invalid sex or acronym values have been replaced with NA. If any of `x`,
#'   `sex`, or `acronym` are the wrong type, `check_who_params()` will throw an
#'   error. Will also contain one of `"y"`, `"z"`, or `"p"`, depending on which
#'   was provided to the function.
#' @noRd
validate_who_gs <- function(y = NULL, z = NULL, p = NULL, x, sex, acronym) {
  validate_yzp(y = y, z = z, p = p) # Uses assign() to replace one of y/z/p
  # For WHO GS standards, the use of `approx()` use in coefficients.R will
  # make out-of-bounds `x` values output as `NA`
  x <- validate_xvar(x, range(gigs::who_gs$wfa$male$zscores$age_days))
  sex <- validate_sex(sex)
  acronym <- validate_acronym(acronym = acronym,
                              allowed_acronyms = names(gigs::who_gs))
  list(y = y, z = z, p = p, x = x, sex = sex, acronym = acronym) |>
    drop_null_elements()
}

#' Check whether user-inputted `sex` and `acronym` values are valid in `ig_nbs`
#' functions
#'
#' @param y,z,p Either a numeric vector or NULL. Checks will fail if more than
#'   one of these arguments are provided; see [validate_yzp()] documentation.
#' @param gest_days Numeric vector with user-inputted gestational age values.
#' @param sex Character vector with user-inputted sex values.
#' @param acronym Character vector with user-inputted acronym values.
#' @returns List with names `"age"`, `"sex"`, and `"acronym"`, containing
#'   vectors where invalid sex or acronym values have been replaced with NA. If
#'   any of `x`, `sex`, or `acronym` are the wrong type, `validate_ig_nbs()`
#'   will throw an error. Will also contain one of `"y"`, `"z"`, or `"p"`,
#'   depending on which was provided to the function.
#' @noRd
validate_ig_nbs <- function(y = NULL,
                            z = NULL,
                            p = NULL,
                            gest_days,
                            sex,
                            acronym) {
  validate_yzp(y = y, z = z, p = p) # Uses assign() to replace one of y/z/p
  # The bfpfga, fmfga, and ffmfga standards have a smaller range of gestational
  # ages than the other INTERGROWTH-21st standards
  is_bodycomp <- acronym %in% names(gigs::ig_nbs)[5:7]
  gest_days <- ifelse(
    is_bodycomp,
    yes = validate_xvar(gest_days,
                        range(gigs::ig_nbs$bfpfga$male$centiles$gest_days)),
    no = validate_xvar(gest_days,
                       range(gigs::ig_nbs$wfga$male$zscores$gest_days))
  )
  sex <- validate_sex(sex)
  acronym <- validate_acronym(acronym = acronym,
                              allowed_acronyms = names(gigs::ig_nbs))
  list(y = y, z = z, p = p,
       gest_days = gest_days, sex = sex, acronym = acronym) |>
    drop_null_elements()
}

#' Check whether user-inputted `sex` and `acronym` values are valid in `ig_png`
#' functions
#'
#' @param y,z,p Either a numeric vector or NULL. Checks will fail if more than
#'   one of these arguments are provided; see [validate_yzp()] documentation.
#' @param x Numeric vector with user-inputted x values.
#' @param sex Character vector with user-inputted sex values.
#' @param acronym Character vector with user-inputted acronym values.
#' @returns List with names `"x"`, `"sex"`, and `"acronym"`, containing
#'   vectors where invalid sex or acronym values have been replaced with NA. If
#'   any of `x`, `sex`, or `acronym` are the wrong type, `validate_ig_png()`
#'   will throw an error. Will also contain one of `"y"`, `"z"`, or `"p"`,
#'   depending on which was provided to the function.
#' @noRd
validate_ig_png <- function(y = NULL, z = NULL, p = NULL, x, sex, acronym) {
  validate_yzp(y = y, z = z, p = p) # Uses assign() to replace one of y/z/p
  # The wfa, lfa and hcfa standards use post-menstrual age, whereas the wfl
  # standard uses length in cm --> this code specifies whether to compare x to
  # PMA or length in cm
  uses_pma <- acronym != names(gigs::ig_png)[4]
  x <- ifelse(
    uses_pma,
    yes = validate_xvar(x, range(gigs::ig_png$wfa$male$zscores$pma_weeks),
                        rounded = TRUE),
    no = validate_xvar(x, range(gigs::ig_png$wfl$male$zscores$length_cm),
                       rounded = FALSE)
  )
  sex <- validate_sex(sex)
  acronym <- validate_acronym(acronym = acronym,
                              allowed_acronyms = names(gigs::ig_png))
  list(y = y, z = z, p = p, x = x, sex = sex, acronym = acronym) |>
    drop_null_elements()
}
