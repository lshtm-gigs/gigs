# General parameter checking for conversion in growth standards ----------------

#' Validate the first argument (i.e. `y`/`z`/`p`) prior to growth standard
#' conversion
#'
#' @param y,z,p Either `NULL` or a numeric vector with length equal to or
#'   greater than one.
#' @note The function will `stop()` if more than one of `y`, `z`, or `p` are not
#'   `NULL`. Whether `y`, `z`, or `p` is provided, an error will be thrown if
#'   they are not an atomic vector with length 1 or greater. If `p` is provided,
#'   values of `p` which are not between `0` and `1` will be replaced with `NA`.
#' @returns This function invisibly returns `NULL`. It is called for its side
#'   effect, which is to replace either `y`, `z`, or `p` in the frame 'above'
#'   this function, after checking the validity for use in gigs.
#' @noRd
validate_yzp <- function(y = NULL, z = NULL, p = NULL) {
  args <- list(y = y, z = z, p = p)
  arg_nulls <- c(y = is.null(y), z = is.null(z), p = is.null(p))
  stopifnot(sum(arg_nulls) == 2L) # Error should never be seen by user
  varname <- names(arg_nulls)[!arg_nulls]
  vec <- args[!arg_nulls][[1]]  |>
    checkmate::assert_numeric(min.len = 1, .var.name = varname) |>
    checkmate::assert_atomic_vector() |>
    remove_attributes() |>
    handle_missing_data(varname = varname) |>
    handle_undefined_data(varname = varname)

  # Flag bad centile values
  if (!arg_nulls[["p"]]) {
    vec <- handle_oob_centiles(vec)
  }
  assign(x = varname, value = vec, envir = parent.frame(n = 1))
}

#' Validate user-inputted x variables prior to growth standard conversion
#'
#' @param x A numeric vector of `x` values to check.
#' @param standard A single string containing a code for a set of growth
#'   standards, either `"ig_fet"`, `"ig_nbs`", `"ig_png"` or `"who_gs"`.
#' @param acronym An already validated `acronym` variable, the same length as
#'   `x`. This should always be a character vector, as it will have been passed
#'   through [validate_acronym()].
#' @returns A numeric vector identical to `x`, except elements not within the
#'   permitted bounds of different `acronym`s  are replaced with `NA`. Throws an
#'   error if `x` is not numeric, is non-atomic, or is zero-length.
#' @noRd
validate_xvar <- function(x, acronym, standard) {
  checkmate::qassert(standard, rules = "S1")
  # Leave function if vector already validated
  x <- x |>
    checkmate::assert_numeric(min.len = 1) |>
    checkmate::assert_atomic_vector() |>
    remove_attributes()
  unique_acronyms <- unique(acronym[!is.na(acronym)])
  is_oob_overall <- logical(length = length(x))
  is_na_x <- is.na(x)
  for (idx in length(unique_acronyms)) {
    chr_acronym <- unique_acronyms[idx]
    is_curr_acronym <- acronym == chr_acronym
    range <- xvar_ranges[[standard]][[chr_acronym]]
    is_oob_overall[!is_na_x & is_curr_acronym & !inrange(x, range)] <- TRUE
  }

  handle_oob_xvar(vec = x, varname = checkmate::vname(x),
                  is_oob = is_oob_overall)
}

#' Validate user-inputted character vectors
#'
#' @param chr User-inputted character vector. Will be either `sex` or `acronym`,
#'   passed down from an exported growth standard conversion function.
#' @param options A character vector with permitted values for `chr`. If no
#'   values in `chr` are `%in% options` the function will throw an
#'   appropriately-formatted error.
#' @returns The vector `chr`, with all elements where `!chr %in% options` are
#'   replaced with `NA`.
#' @srrstats {G2.3, G2.3a, G2.3b} Univariate character input assertions.
#' @noRd
validate_chr <- function(chr, options) {
  varname <- deparse(substitute(chr))
  gigs_opt <- paste0("handle_invalid_", varname)
  chr |>
    checkmate::assert_character(min.len = 1, .var.name = varname) |>
    checkmate::assert_atomic_vector() |>
    remove_attributes() |>
    handle_missing_data(varname = varname) |>
    handle_undefined_data(varname = varname) |>
    handle_invalid_chr_options(gigs_opt = gigs_opt,
                               varname = varname,
                               options = options)
}

#' Validate user-inputted sexes prior to growth standard conversion
#'
#' @param sex Character vector with length more than or equal to one containing
#'   user-inputted sex values.
#' @returns Returns `sex`, with values not `%in% c("M", "F")` set to `NA`.
#'   Will throw an error if `sex` is not a character vector.
#' @noRd
validate_sex <- function(sex) {
  # May add "U" back to package later
  validate_chr(sex, options = c("M", "F"))
}

#' Validate user-inputted acronyms prior to growth standard conversion
#'
#' @param acronym Character vector with user-inputted acronym values.
#' @param allowed_acronyms Character vector with acceptable values for
#'   `acronym`. Members of `acronym` which are not `%in% allowed_acronyms` will
#'   be set to `NA`.
#' @returns Returns `acronym`, with values not `%in% allowed_acronyms` set to
#'   `NA`. Will throw an error if `acronym` is not a character vector.
#' @noRd
validate_acronym <- function(acronym, allowed_acronyms, standard) {
  validate_chr(acronym, options = allowed_acronyms)
}

#' Check if an object has a named attribute
#' @description Used to tag data after validation by gigs, so that inputs are
#'   not checked multiple times unecessarily
#' @param obj Object to check for a given attribute.
#' @param attr_name A single-length character vector with the name of the
#'   attribute to check for in `obj`.
#' @returns A single logical value. Will be `TRUE` if `obj` has an attribute
#'   named `attr_name`, otherwise `FALSE`.
#' @noRd
has_attribute <- function(obj, attr_name) {
  any(names(attributes(obj)) == attr_name)
}

# Standard-specific parameter validation ---------------------------------------

#' Check whether user-inputted `sex` and `acronym` values are valid in `who_gs`
#' functions
#'
#' @param y,z,p Either a numeric vector or NULL. Checks will fail if more than
#'   one of these arguments are provided; see [validate_yzp()] documentation.
#' @param x Numeric vector of length one or more with user-inputted x values.
#' @param sex Character vector of length one or more with user-inputted sex
#'   values. Values which are not `"M"` or `"F"` will be replaced with `NA`.
#' @param acronym Character vector of length one or more with user-inputted
#'   acronym values. Values which are not in `names(gigs::who_gs)` will be
#'   replaced with `NA`.
#' @returns List with names `"x"`, `"sex"`, and `"acronym"`, containing
#'   vectors where invalid `sex` or `acronym` elements have been replaced with
#'   NA. If any of `x`, `sex`, or `acronym` are the wrong type or length, an
#'   error will be thrown. Will also contain one of `"y"`, `"z"`, or `"p"`,
#'   depending on which was provided to the function.
#' @noRd
validate_who_gs <- function(y = NULL, z = NULL, p = NULL, x, sex, acronym) {
  validate_yzp(y = y, z = z, p = p) # Uses assign() to replace one of y/z/p
  standard <- "who_gs"
  acronym <- validate_acronym(acronym = acronym,
                              allowed_acronyms = names(gigs::who_gs),
                              standard = standard)
  x <- validate_xvar(x, standard = standard, acronym = acronym)
  sex <- validate_sex(sex)

  list(y = y, z = z, p = p, x = x, sex = sex, acronym = acronym) |>
    drop_null_elements()
}

#' Check whether user-inputted `sex` and `acronym` values are valid in `ig_nbs`
#' functions
#'
#' @param y,z,p Either a numeric vector or NULL. Checks will fail if more than
#'   one of these arguments are provided; see [validate_yzp()] documentation.
#' @param gest_days Numeric vector of length one or more with user-inputted
#'   gestational age values in days.
#' @param sex Character vector of length one or more with user-inputted sex
#'   values. Values which are not `"M"` or `"F"` will be replaced with `NA`.
#' @param acronym Character vector of length one or more with user-inputted
#'   acronym values. Values which are not in `names(gigs::ig_nbs)` will be
#'   replaced with `NA`.
#' @returns List with names `"x"`, `"sex"`, and `"acronym"`, containing
#'   vectors where invalid `sex` or `acronym` elements have been replaced with
#'   NA. If any of `x`, `sex`, or `acronym` are the wrong type or length, an
#'   error will be thrown. Will also contain one of `"y"`, `"z"`, or `"p"`,
#'   depending on which was provided to the function.
#' @noRd
validate_ig_nbs <- function(y = NULL,
                            z = NULL,
                            p = NULL,
                            gest_days,
                            sex,
                            acronym) {
  validate_yzp(y = y, z = z, p = p) # Uses assign() to replace one of y/z/p
  standard <- "ig_nbs"
  acronym <- validate_acronym(acronym = acronym,
                              allowed_acronyms = names(gigs::ig_nbs),
                              standard = standard)
  gest_days <- validate_xvar(gest_days, standard = standard, acronym = acronym)
  sex <- validate_sex(sex)
  list(y = y, z = z, p = p, gest_days = gest_days, sex = sex,
       acronym = acronym) |>
    drop_null_elements()
}

#' Check whether user-inputted `sex` and `acronym` values are valid in `ig_png`
#' functions
#'
#' @param y,z,p Either a numeric vector or NULL. Checks will fail if more than
#'   one of these arguments are provided; see [validate_yzp()] documentation.
#' @param x Numeric vector with user-inputted x values.
#' @param sex Character vector of length one or more with user-inputted sex
#'   values. Values which are not `"M"` or `"F"` will be replaced with `NA`.
#' @param acronym Character vector of length one or more with user-inputted
#'   acronym values. Values which are not in `names(gigs::ig_png)` will be
#'   replaced with `NA`.
#' @returns List with names `"x"`, `"sex"`, and `"acronym"`, containing
#'   vectors where invalid `sex` or `acronym` elements have been replaced with
#'   NA. If any of `x`, `sex`, or `acronym` are the wrong type or length, an
#'   error will be thrown. Will also contain one of `"y"`, `"z"`, or `"p"`,
#'   depending on which was provided to the function.
#' @noRd
validate_ig_png <- function(y = NULL, z = NULL, p = NULL, x, sex, acronym) {
  validate_yzp(y = y, z = z, p = p) # Uses assign() to replace one of y/z/p
  standard <- "ig_png"
  acronym <- validate_acronym(acronym = acronym,
                              allowed_acronyms = names(gigs::ig_png),
                              standard = standard)
  x <- validate_xvar(x, standard = standard, acronym = acronym)
  sex <- validate_sex(sex)
  list(y = y, z = z, p = p, x = x, sex = sex, acronym = acronym) |>
    drop_null_elements()
}

#' Check whether user-inputted `z`, `y`, `gest_days` and `acronym` values are
#' valid in `ig_fet` functions
#'
#' @param y,z,p Either a numeric vector or NULL. Checks will fail if more than
#'   one of these arguments are provided; see [validate_yzp()] documentation.
#' @param x Numeric vector of length one or more with user-inputted gestational
#'   age values in days.
#' @param sex Character vector of length one or more with user-inputted sex
#'   values. Values which are not `"M"` or `"F"` will be replaced with `NA`.
#' @returns List with names `"gest_days"`, and `"acronym"`, containing vectors
#'   where invalid `gest_days` and `acronym` elements have been replaced with
#'   `NA`. If either of `x` or `acronym` are the wrong type or length, an
#'   error will be thrown. Will also contain one of `"y"`, `"z"`, or `"p"`,
#'   depending on which was provided to the function.
#' @noRd
validate_ig_fet <- function(y = NULL, z = NULL, p = NULL, x, acronym) {
  validate_yzp(y = y, z = z, p = p) # Uses assign() to replace one of y/z/p
  standard <- "ig_fet"
  acronym <- validate_acronym(acronym = acronym,
                              allowed_acronyms = names(gigs::ig_fet),
                              standard = standard)
  x <- validate_xvar(x, standard = standard, acronym = acronym)
  list(y = y, z = z, p = p, x = x, acronym = acronym) |>
    drop_null_elements()
}

# Parameter validation for IG-21st estimation equations

#' Validate inputs to the INTERGROWTH-21st estimation functions
#' @param var An input to [ig_fet_estimate_ga()] or
#'   [ig_fet_estimate_fetal_weight()]. Should either be `NULL` or a numeric
#'   vector of length one or more.
#' @return Returns `NULL` if `vec` is `NULL`, else returns `vec` with attributes
#'   removed.
#' @noRd
validate_ig_fet_estimation_param <- function(var) {
  if (is.null(var)) {
    return(var)
  }
  varname <- deparse(substitute(var))
  checkmate::assert_numeric(var, min.len = 1, .var.name = varname) |>
    checkmate::assert_atomic_vector(.var.name = varname) |>
    remove_attributes() |>
    handle_missing_data(varname = varname) |>
    handle_undefined_data(varname = varname)
}

# Handle missing, undefined, or invalid data inputs ----------------------------

#' @rdname handle_var
#' @noRd
handle_missing_data <- function(vec, varname) {
  handle_var(vec = vec,
             varname = varname,
             option = "handle_missing_data",
             test_fn = \(x) is.na(x) & !is.nan(x),
             msg_fn = msg_missing_data)
}

#' @rdname handle_var
#' @noRd
handle_undefined_data <- function(vec, varname) {
  handle_var(vec = vec,
             varname = varname,
             option = "handle_undefined_data",
             test_fn = \(x) is.infinite(x) | is.nan(x),
             msg_fn = msg_undefined_data)
}

#' @rdname handle_var
#' @noRd
handle_oob_centiles <- function(vec) {
  handle_var(vec = vec,
             varname = "p",
             option = "handle_oob_centiles",
             test_fn = \(x) !is.na(x) & !(x > 0 & x < 1),
             msg_fn = msg_oob_centiles)
}

#' @rdname handle_var
#' @noRd
handle_invalid_chr_options <- function(vec, gigs_opt, varname, options) {
  handle_var(vec = vec,
             varname = varname,
             option = gigs_opt,
             test_fn = \(x) !is.na(x) & !x %in% options,
             msg_fn = msg_invalid_sex_acronym)
}

#' @rdname handle_var
#' @param is_oob Denotes whether values in `x` were out of bounds. This checking
#'   step is performed in [validate_xvar()].
#' @noRd
handle_oob_xvar <- function(vec, varname, is_oob) {
  handle_var(vec = vec,
             varname = varname,
             option = "handle_oob_xvar",
             test_fn = \(x) is_oob,
             msg_fn = msg_oob_xvar)
}

#' Handle variables based on gigs options
#'
#' @param option A single-length character vector which should be one of
#'   `names(.gigs_options)`.
#' @param vec Vector of length one or more which will be checked for invalid
#'   values by `test_fn`.
#' @param varname Single-length character vector with the variable name for
#'   `vec` which should be printed out by `msg_fn`.
#' @param test_fn A logical vector-producing function (anonymous or otherwise)
#'   that is used with `vec` as input.
#' @param msg_fn A string-producing function to produce a helpful message when
#'   `option` is set to `"warn"` or `"error"`.
#' @seealso Check out [gigs_options], which can be used to define how this
#'   function behaves for different sorts of data.
#' @rdname handle_var
#' @noRd
handle_var <- function(vec, varname, option, test_fn, msg_fn) {
  lgl_is_invalid <- test_fn(vec)
  if (!any(lgl_is_invalid)) {
    return(vec)
  }

  opt_value <- gigs_option_get(option, silent = TRUE)
  out <- replace(vec, lgl_is_invalid, values = NA)
  str_msg <- msg_fn(lgl_is_invalid, varname = varname)
  if (opt_value == "quiet") {
    return(out)
  }
  if (opt_value == "error") {
    stop(str_msg, call. = FALSE)
  }
  warning(str_msg, call. = FALSE)
  out
}

# `error()`/`warning()` messages for missing/undefined/invalid inputs ----------

#' String production functions for `error()`/`warning()` messages regarding
#'   missing, undefined, or invalid data inputs
#'
#' @param lgl_oob_centiles Logical vector of length one or more denoting whether
#'   a centile value provided to a gigs `*_centile2value()` function was between
#'   0 and 1.
#' @param varname The variable name, which will be included in the returned
#'   string.
#' @returns Single-length character vector with information on the number of
#'   invalid data points relevant to the inputted `lgl_*` parameter.
#' @rdname msg_oob_centiles
#' @noRd
msg_oob_centiles <- function(lgl_oob_centiles, varname) {
  paste0("Variable '", varname, "': ", sum(lgl_oob_centiles), " in ",
         length(lgl_oob_centiles), " elements were not between 0 and 1.")
}

#' @rdname msg_oob_centiles
#' @param lgl_invalid_sex Logical vector of length one or more denoting
#'   whether each element of a vector was `!is.na()`.
#' @noRd
msg_missing_data <- function(lgl_missing_data, varname) {
  paste0("Variable '", varname, "': ", sum(lgl_missing_data), " in ",
         length(lgl_missing_data), " elements were missing (`NA`).")
}

#' @rdname msg_oob_centiles
#' @param lgl_undefined_data Logical vector of length one or more denoting
#'   whether each element of a vector was either `NaN`, `-Inf`, or `Inf`.
#' @noRd
msg_undefined_data <- function(lgl_undefined_data, varname) {
  paste0("Variable '", varname, "': ", sum(lgl_undefined_data),
         " in ", length(lgl_undefined_data), " elements were undefined (`NaN`,",
         " `Inf`, or `-Inf`).")
}

#' @rdname msg_oob_centiles
#' @param lgl_oob_xvar Logical vector of length one or more denoting whether
#'   each element of `x`/an equivalent argument provided to a gigs conversion
#'   function was within the bounds for its growth standard.
#' @noRd
msg_oob_xvar <- function(lgl_oob_xvar, varname) {
  standard <- get(x = "standard", envir = parent.frame(n = 3))
  paste0("Variable '", varname, "': ", sum(lgl_oob_xvar), " in ",
         length(lgl_oob_xvar), " elements were out-of-bounds (see the '",
         standard, "' conversion functions documentation).")
}

#' @rdname msg_oob_centiles
#' @param lgl_invalid_sex_acronym Logical vector of length one or more denoting
#'   whether elements of `sex` or `acronym` are valid. For `sex`, this means
#'   being one of `"M"` or `"F"`. For `acronym`, this means being a valid
#'   growth standard acronym as provided in the documentation for the different
#'   gigs conversion functions.
#' @noRd
msg_invalid_sex_acronym <- function(lgl_invalid_sex_acronym, varname) {
  count <- sum(lgl_invalid_sex_acronym)
  len <- length(lgl_invalid_sex_acronym)

  if (varname == "sex") {
    paste0("Variable 'sex': ", count, " in ", len, " elements were neither ",
           "\"M\" nor \"F\".")
  } else {
    standard <- get(x = "standard", envir = parent.frame(n = 4))
    see_sentence <- paste0("See the '", standard, "' documentation for valid ",
                           "'acronym' values.")
    if (count == len) {
      stop("Variable 'acronym': All elements were invalid. ", see_sentence,
           call. = FALSE)
    }
    paste0("Variable 'acronym': ", count, " in ", len, " elements were ",
           "invalid. ", see_sentence)
  }
}

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.4a} This file's functions are all documented with `{roxygen2}`.
#' @srrstats {G2.0, G2.1, G2.2} Length/type/univariate input assertions using
#'   `{checkmate}` package.
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.16} The various `handle_*`
#'   functions in this file provide a framework for detecting and handling
#'   missing, undefined, and other invalid data inputs to gigs functions. The
#'   behaviour of these functions can be customised using [gigs_options_set()]
#'   to replace invalid values with `NA` silently, replace invalid values with
#'   `NA` with `warning()`s, or throw errors.
#' @srrstats {EA2.6} The `remove_attributes()` function is used to make sure
#'   vector-type inputs with odd class structures (e.g. as in `units` package)
#'   can still be used as input to [gigs]. This is done after assertions on
#'   length etc.