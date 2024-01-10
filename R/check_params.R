# General parameter checking for conversion in growth standards ----------------

#' Validate the first argument (i.e. `y`/`z`/`p`) prior to growth standard
#' conversion
#'
#' @param y,z,p Either `NULL` or a numeric vector with length equal to or
#'   greater than one.
#' @note The function will `stop()` if more than one of `y`, `z`, or `p` are not
#'   `NULL`. All checks on type/length are performed by `{checkmate}`.
#' @returns Returns `vec`. Throws an error if `vec` is not numeric, is not at
#'   least length 1, or if all values in `vec` are missing.
#' @noRd
validate_yzp <- function(y = NULL, z = NULL, p = NULL) {
  args <- list(y = y, z = z, p = p)
  arg_nulls <- c(y = is.null(y), z = is.null(z), p = is.null(p))
  stopifnot(sum(arg_nulls) == 2L) # Error should never be seen by user
  varname <- names(arg_nulls)[!arg_nulls]
  vec <- args[!arg_nulls][[1]] |>
    checkmate::assert_numeric(min.len = 1, .var.name = varname) |>
    checkmate::assert_atomic_vector() |>
    remove_attributes()
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
#' @returns A numeric vector identical to `x`, except values outside `lower` and
#'   `upper` are replaced with NAs. Throws an error if `x` is not numeric.
#' @noRd
validate_xvar <- function(x, range) {
  x <- checkmate::assert_numeric(x, min.len = 1) |>
    checkmate::assert_atomic_vector() |>
    remove_attributes()
  out_of_range <- !inrange(x, range)
  replace(x = x, list = out_of_range, values = NA_real_)
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
                              .var.name = varname) |>
    checkmate::assert_atomic_vector()
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
  uses_pma <- acronym != "wfl"
  x <- ifelse(
    uses_pma,
    yes = validate_xvar(x, range(gigs::ig_png$wfa$male$zscores$pma_weeks)),
    no = validate_xvar(x, range(gigs::ig_png$wfl$male$zscores$length_cm))
  )
  sex <- validate_sex(sex)
  acronym <- validate_acronym(acronym = acronym,
                              allowed_acronyms = names(gigs::ig_png))
  list(y = y, z = z, p = p, x = x, sex = sex, acronym = acronym) |>
    drop_null_elements()
}

#' Check whether user-inputted `z`, `y`, `gest_days` and `acronym` values are
#' valid in `ig_fet` functions
#'
#' @param y,z,p Either a numeric vector or NULL. Checks will fail if more than
#'   one of these arguments are provided; see [validate_yzp()] documentation.
#' @param x Numeric vector with user-inputted gestational age values in
#'   days.
#' @param acronym Character vector with user-inputted acronym values.
#' @note If any arguments are found to be the wrong type or have length < 1, an
#'   error will be thrown. Values in `gest_days` and `acronym` which are not
#'   acceptable will be replaced with `NA`.
#' @returns List with names `"gest_days"`, and `"acronym"`, containing vectors
#'   where invalid `gest_days` and `acronym` values have been replaced with
#'   `NA`. Will also contain one of `"y"`, `"z"`, or `"p"`, depending on which
#'   was provided to the function.
#' @noRd
validate_ig_fet <- function(y = NULL, z = NULL, p = NULL, x, acronym) {
  validate_yzp(y = y, z = z, p = p) # Uses assign() to replace one of y/z/p
  fet_growth_acros <- c("hcfga", "bpdfga", "acfga", "flfga", "ofdfga", "tcdfga")
  brain_dev_acros <- c("poffga", "sffga", "avfga", "pvfga", "cmfga")
  doppler_acros <- c("pifga", "rifga", "sdrfga")

  #' Validate that some subset of `x` using a given range
  #' @param lgl_subset Logical statement used to subset `x`
  #' @param range Two-length vector giving acceptable lower + upper bounds for
  #'   values in `x`
  #' @return Invisibly returns NULL, but overwrites `x[lgl_subset]` with
  #'   validated values (i.e. outside bounds of `range` changed to NA).
  #' @noRd
  validate_subset <- function(lgl_subset, range) {
    if (length(x[lgl_subset]) != 0) {
      x[lgl_subset] <- validate_xvar(x[lgl_subset], range)
    }
  }
  validate_subset(acronym %in% fet_growth_acros, c(98, 280)) # days
  validate_subset(acronym %in% brain_dev_acros, c(105, 252)) # days
  validate_subset(acronym %in% doppler_acros, c(168, 280)) # days
  validate_subset(acronym == "efwfga", c(154, 280)) # days
  validate_subset(acronym == "sfhfga", c(112, 294)) # days
  validate_subset(acronym == "crlfga", c(58, 105)) # days
  validate_subset(acronym == "gafcrl", c(15, 95)) # days
  validate_subset(acronym == "gwgfga", c(98, 280)) # days
  # validate_subset(acronym == "gaftcd", c(12, 55)) # mm
  x <- remove_attributes(x)

  acronym <- validate_acronym(acronym = acronym,
                              allowed_acronyms = names(gigs::ig_fet))
  list(y = y, z = z, p = p, x = x, acronym = acronym) |>
    drop_null_elements()
}