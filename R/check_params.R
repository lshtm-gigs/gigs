# Handle missing, undefined, or invalid data inputs ----------------------------

#' Handle variables based on gigs options
#'
#' @param option A single-length character vector which should be one of
#'   `names(.gigs_options)`.
#' @param vec Vector of length one or more which will be checked for invalid
#'   values by `test_fn`.
#' @param varname Single-length character vector with the variable name for
#'   `vec` which should be printed out by `msg_fn`.
#' @param test_lgl A logical vector-producing function (anonymous or otherwise)
#'   that is used with `vec` as input.
#' @param msg_fn A string-producing function to produce a helpful message when
#'   `option` is set to `"warn"` or `"error"`.
#' @seealso Check out the [GIGS package-level options][gigs_options], which can
#'   be used to define how this function behaves for different sorts of data.
#' @rdname handle_var
#' @noRd
handle_var <- function(vec, varname, option, test_lgl, msg_fn) {
  lgl_is_invalid <- test_lgl
  if (!any(lgl_is_invalid)) {
    return(vec)
  }
  out <- replace(vec, lgl_is_invalid, values = NA)
  str_msg <- msg_fn(lgl_is_invalid, varname = varname)
  option_value <- gigs_option_get(option, silent = TRUE)
  warn_stop_fn <- switch(option_value,
                         warn = rlang::warn,
                         error = rlang::abort,
                         quiet = return(out))
  warn_stop_fn(str_msg, call = NULL, class = sprintf("gigs_%s", option))
  out
}

#' @rdname handle_var
#' @param is_na Logical vector the same length as `vec`, which is the same as
#'   `is.na(vec)`. This check is cached in a specific object to prevent
#'   re-computing `is.na(vec)`.
#' @param is_na Logical vector the same length as `vec`, which is the same as
#'   `is.nan(vec)`. This check is cached in a specific object to prevent
#'   re-computing `is.nan(vec)`.
#' @noRd
handle_missing_data <- function(vec, varname, is_na, is_nan) {
  handle_var(vec = vec,
             varname = varname,
             option = "handle_missing_data",
             test_lgl = is_na & !is_nan,
             msg_fn = msg_missing_data)
}

#' @rdname handle_var
#' @noRd
handle_undefined_data <- function(vec, varname, is_nan) {
  handle_var(vec = vec,
             varname = varname,
             option = "handle_undefined_data",
             test_lgl = is.infinite(vec) | is_nan,
             msg_fn = msg_undefined_data)
}

#' @rdname handle_var
#' @noRd
handle_oob_centiles <- function(vec) {
  handle_var(vec = vec,
             varname = "p",
             option = "handle_oob_centiles",
             test_lgl = !is.na(vec) & (0.5 < abs(vec - 0.5)),
             msg_fn = msg_oob_centiles)
}

#' @rdname handle_var
#' @noRd
handle_invalid_chr_options <- function(vec, gigs_opt, varname, options, is_na) {
  handle_var(vec = vec,
             varname = varname,
             option = gigs_opt,
             test_lgl = !is_na & !vec %in% options,
             msg_fn = msg_invalid_sex_acronym)
}

#' @rdname handle_var
#' @param is_oob A logical vector the same length as `vec` which denotes whether
#'   elements in `vec` are out of bounds for the growth standard being used.
#' @seealso [validate_xvar()], which this function is called by.
#' @noRd
handle_oob_xvar <- function(vec, varname, is_oob) {
  handle_var(vec = vec,
             varname = varname,
             option = "handle_oob_xvar",
             test_lgl = is_oob,
             msg_fn = msg_oob_xvar)
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
  paste0("Argument `", varname, "`: ", sum(lgl_oob_centiles), " in ",
         length(lgl_oob_centiles), " elements were not between 0 and 1.")
}

#' @rdname msg_oob_centiles
#' @param lgl_missing_data Logical vector of length one or more denoting
#'   whether each element of a vector was `!is.nan() & is.na()`.
#' @noRd
msg_missing_data <- function(lgl_missing_data, varname) {
  if (all(lgl_missing_data) & varname == "acronym") {
    stop("Argument `", varname, "`: All elements were missing (`NA`).")
  }
  paste0("Argument `", varname, "`: ", sum(lgl_missing_data), " in ",
         length(lgl_missing_data), " elements were missing (`NA`).")
}

#' @rdname msg_oob_centiles
#' @param lgl_undefined_data Logical vector of length one or more denoting
#'   whether each element of a vector was either `NaN`, `-Inf`, or `Inf`.
#' @noRd
msg_undefined_data <- function(lgl_undefined_data, varname) {
  paste0("Argument `", varname, "`: ", sum(lgl_undefined_data),
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
  paste0("Argument `", varname, "`: ", sum(lgl_oob_xvar), " in ",
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
  all_invalid <- count == len
  chr_x_in_y <- if (all_invalid) "All" else paste0(count, " in ", len)

  if (varname == "sex") {
    paste0("Argument `sex`: ", chr_x_in_y, " elements were neither ",
             "\"M\" nor \"F\".")
  } else {
    if (all_invalid) {
      acronym <- get(x = "acronym", envir = parent.frame(n = 3))
      standard <- get(x = "standard", envir = parent.frame(n = 4))
      valid_acronyms <- names(get(standard, envir = rlang::ns_env("gigs")))
      valid_acronyms <- paste_sep_commas_quoted(valid_acronyms)
      rlang::abort(
        c("\nInvalid `acronym` value:",
          "x" = paste0("Value of `acronym` was `\"", acronym, "\"`."),
          "!" = paste0("Value of `acronym` must be one of ", valid_acronyms,
                       ".")),
        call = rlang::caller_env(n = 5),
        class = "gigs_invalid_acronym"
      )
    }
  }
}

# Parameter-checking functions for character/numeric vectors -------------------

#' Validate a numeric vector used in gigs
#'
#' @param num Numeric vector of length one or greater. Will be checked for
#'   data type and length > 0, then stripped of attributes. Will be checked
#'   for missing/undefined data with [handle_missing_data()] and
#'   [handle_undefined_data()].
#' @param y_name Single-length character vector with standard-specific
#'   names for `y`. If `NULL`, error messages will print out that there are
#'   issues with `'y'`, instead of standard-specific variables like `lenght_cm`
#'   or `weight_kg`.
#' @returns The vector `num`, stripped of attributes and with any undefined
#'   values set to `NA`.
#' @noRd
validate_numeric <- function(num, varname) {
  num <- num |>
    checkmate::assert_numeric(min.len = 1, .var.name = varname) |>
    checkmate::assert_atomic_vector()
  # Save on recomputation
  num_is_na <- is.na(num)
  num_is_nan <- is.nan(num)
  num <- num |>
    remove_attributes() |>
    handle_missing_data(varname = varname, is_na = num_is_na,
                        is_nan = num_is_nan) |>
    handle_undefined_data(varname = varname, is_nan = num_is_nan)
}

# General parameter checking for conversion in growth standards ----------------

#' Validate the first argument (i.e. `y`/`z`/`p`) prior to growth standard
#' conversion
#'
#' @param y,z,p Either `NULL` or a numeric vector with length equal to or
#'   greater than one.
#' @param y_name Single-length character vector with standard-specific
#'   names for `y`. If `NULL`, error messages will print out that there are
#'   issues with `'y'`, instead of standard-specific variables like `lenght_cm`
#'   or `weight_kg`.
#' @note The function will `stop()` if more than one of `y`, `z`, or `p` are not
#'   `NULL`. Whether `y`, `z`, or `p` is provided, an error will be thrown if
#'   they are not an atomic vector with length 1 or greater. If `p` is provided,
#'   values of `p` which are not between `0` and `1` will be replaced with `NA`.
#' @returns Returns a list containing `y`, `z`, or `p`, with the
#'   non-`NULL` of the input arguments having been checked for (1) being
#'   numeric, (2) having length >= 1, (3) being atomic, and (4) + (5) being
#'   checked for missing/undefined data.
#' @noRd
validate_yzp <- function(y = NULL, z = NULL, p = NULL, y_name = NULL) {
  yzp <- list(y = y, z = z, p = p)
  yzp_nulls <- c(y = is.null(y), z = is.null(z), p = is.null(p))
  if (sum(yzp_nulls) == 3L) {
    rlang::abort("Your `y`/`z`/`p` argument must not be `NULL`.", call = NULL)
  }
  yzp_name <- if (is.null(y_name)) names(yzp_nulls)[!yzp_nulls] else y_name
  vec <- yzp[!yzp_nulls][[1]]
  vec <- validate_numeric(vec, yzp_name)

  # Flag bad centile values
  if (!yzp_nulls[["p"]]) {
    vec <- handle_oob_centiles(vec)
  }
  yzp[!yzp_nulls][[1]] <- vec
  yzp
}

#' Validate user-inputted x variables prior to growth standard conversion
#'
#' @param x A numeric vector of `x` values to check.
#' @param standard A single string containing a code for a set of growth
#'   standards, either `"ig_fet"`, `"ig_nbs`", `"ig_png"` or `"who_gs"`.
#' @param acronym A single-length character vector containing an
#'   already-validated `acronym` value.
#' @returns A numeric vector identical to `x`, except elements not within the
#'   permitted bounds for `acronym` are replaced with `NA`. Throws informative
#'   errors if `x` is not numeric, is non-atomic, or is zero-length.
#' @noRd
validate_xvar <- function(x, acronym, standard, x_name) {
  checkmate::qassert(standard, rules = "S1")
  if (is.null(x_name)) x_name <- "x"
  x <- validate_numeric(x, varname = x_name)

  # Check for out of bounds where `x` was not NA or undefined
  range <- xvar_ranges[[standard]][[acronym]]
  is_oob <- !is.na(x) & !inrange(x, range)
  handle_oob_xvar(vec = x, varname = x_name, is_oob = is_oob)
}

#' Validate user-inputted sexes prior to growth standard conversion
#'
#' @param sex Character vector with length more than or equal to one containing
#'   user-inputted sex values.
#' @returns Returns `sex`, with values not `%in% c("M", "F")` set to `NA`.
#'   Will throw an error if `sex` is not a character vector.
#' @noRd
validate_sex <- function(sex) {
  varname <- "sex"
  chr <- sex |>
    checkmate::assert_character(min.len = 1, .var.name = varname) |>
    checkmate::assert_atomic_vector()
  chr_is_na <- is.na(chr)
  chr |>
    remove_attributes() |>
    handle_missing_data(varname = varname, is_na = chr_is_na, is_nan = FALSE) |>
    handle_invalid_chr_options(gigs_opt = "handle_invalid_sex",
                               varname = varname,
                               options = c("M", "F"),
                               is_na = chr_is_na)
}

#' Validate user-inputted acronyms prior to growth standard conversion
#'
#' @param acronym A single-length character vector; will be a user-inputted
#'   acronym.
#' @param allowed_acronyms Character vector with acceptable values for
#'   `acronym`. An error will be thrown is `acronym` is not
#'   `%in% allowed_acronyms`.
#' @returns Returns `acronym`, with values not `%in% allowed_acronyms` set to
#'   `NA`. Will throw an error if `acronym` is not a character vector.
#' @noRd
validate_acronym <- function(acronym, allowed_acronyms, standard) {
  varname <- "acronym"
  gigs_opt <- "handle_invalid_acronym"
  acronym <- acronym |>
    checkmate::qassert(rules = "S1", .var.name = varname) |>
    checkmate::qassert(rules = "V1", .var.name = varname)
  acronym |>
    remove_attributes() |>
    handle_invalid_chr_options(gigs_opt = gigs_opt, varname = varname,
                               options = allowed_acronyms, is_na = FALSE)
}

#' Validate user-inputted IDs for z-scoring/growth analysis
#' @param id An `id` input from one of the gigs_zscoring functions, i.e.
#'   `[gigs_waz()]`, `[gigs_lhaz()]`, `[gigs_wlz()]`, `[gigs_hcaz()]`. This must
#'   be a factor or `NULL` (the default). If a factor, `id` does not have to be
#'   either `ordered` or unordered.
#' @returns If `id` was `NULL`, returns a single-length factor variable of
#'   `"A"`, which can be recycled to any length. Otherwise returns `id` if
#'   `checkmate::assert_factor()` has no issues.
#' @noRd
validate_id <- function(id) {
  if (!is.null(id)) {
    varname <- "id"
    checkmate::assert_factor(id,
                             min.len = 1,
                             .var.name = varname) |>
      checkmate::assert_atomic_vector()
    curr_gigs_opt <- gigs_option_get("handle_missing_data", silent = TRUE)
    on.exit(gigs_option_set("handle_missing_data", curr_gigs_opt, TRUE))
    gigs_option_set("handle_missing_data", "error", TRUE)
    handle_var(vec = id,
               varname = varname,
               option = "handle_missing_data",
               test_lgl = is.na(id) & !is.nan(id),
               msg_fn = msg_missing_data)
  } else {
    factor(x = "A")
  }
}

# Prettier printing of warnings/errors when validating user inputs -------------
## These functions capture warnings/errors from `handle_*()` functions and print
## them nicely for end-users with rlang::warn and rlang::abort

#' Pretty formatting for GIGS warnings/errors
#' @param alerts A list of warnings/errors captured by
#'   `catch_and_throw_validate_issues()`
#' @param mode A single-length character variable, one of either `warning` or
#'   `error`. This controls whether `rlang::warn()` or `rlang::error()` is used
#'   to deliver the messages, respectively.
#' @inheritParams catch_and_throw_validate_issues
#' @returns Returns NULL invisibly if `alerts` has length 0, else throw errors
#'   or warnings depending on `mode`.
#' @noRd
validation_alerts <- function(alerts,
                              mode,
                              call = rlang::caller_env(),
                              arg_type = "arguments") {
  mode <- checkmate::qassert(mode, rules = "S1") |>
    checkmate::assert_subset(c("warn", "abort"))
  arg_type <- checkmate::qassert(arg_type, rules = "S1") |>
    checkmate::assert_subset(c("arguments", "columns"))
  if (length(alerts) < 1) {
    return(invisible(NULL))
  }
  fn_alert <- switch(mode, warn = rlang::warn, abort = rlang::abort)
  bullet <- switch(mode, warn = "i", abort = "!")
  alerts <- setNames(unlist(alerts), rep_len(bullet, length(alerts)))
  alert_class <- sprintf("gigs_validation_%s", mode)
  fn_alert(
    message = c(sprintf("Input %s have invalid values:", arg_type),
                alerts),
    call = call, class = alert_class
  )
}

#' Throw warnings *then* errors when validating GIGS inputs.
#' @param warnings,errors A list of warnings or errors, captured by the
#'   `rlang::try_fetch()`/`rlang::cnd_muffle()`/`rlang::zap()` construct in
#'   `catch_and_throw_validate_issues()`.
#' @inheritParams catch_and_throw_validate_issues
#' @noRd
validation_warnings_errors <- function(warnings,
                                       errors,
                                       call = rlang::caller_env()) {
  validation_alerts(warnings, mode = "warn", call)
  validation_alerts(errors, mode = "abort", call)
}

#' Catch warnings/errors thrown up during parameter validation, and print them
#' prettily
#' @param An R expression which contains multiple `validate_*()` function calls.
#'   These will be caught, collected together, and returned to the user in one
#'   block.
#' @param call A calling environment, which defaults to rlang::caller_env().
#'   This parameter ensures the warnings/errors print with the right function
#'   call at the start.
#' @returns Either `expr` is evaluated and NULL is returned by
#'   `validation_warnings_errors()`, or the function ends up printing the
#'   errors/warnings collected in `warnings` and `errors`.
#' @noRd
catch_and_throw_validate_issues <- function(expr, call = rlang::caller_env()) {
  warnings <- list()
  errors <- list()
  rlang::try_fetch(
    expr = expr,
    warning = \(cnd) {
      warnings_temp <- get("warnings", envir = rlang::env_parent())
      assign(x = "warnings",
             value = c(cnd$message, warnings_temp),
             envir = rlang::env_parent())
      rlang::cnd_muffle(cnd)
      rlang::zap()
    },
    error = \(cnd) {
      errors_temp <- get("errors", envir = rlang::env_parent())
      assign(x = "errors",
             value = c(cnd$message, errors_temp),
             envir = rlang::env_parent())
      rlang::cnd_muffle(cnd)
    })
  validation_warnings_errors(warnings, errors, call = call)
}

# Length checking --------------------------------------------------------------

#' Throw an error if inputs cannot be recycled, with a special message if any
#' input was zero-length
#' @param ... A set of named arguments. These must be named as some names
#'   (e.g. `"acronym"`) are treated differently, and error message formatting
#'   needs to name the wacky arguments.
#' @param y_name,x_name Single-length character vectors with standard-specific
#'   names for `y` and `x`. If `NULL`, error messages will print out that there
#'   are issues with `'y'` and `'x'`, instead of standard-specific variables
#'   like `bpd_mm` or `gest_days`.
#' @returns Invisibly returns inputs as list generated by `list(...)`. Called
#'   for its side effect of throwing errors if inputs have bad lengths, either
#'   because they are zero length or because they cannot be recycled.
#' @noRd
validate_parameter_lengths <- function(...,
                                       y_name = NULL,
                                       x_name = NULL,
                                       call = rlang::caller_env()) {
  inputs <- list(...)
  if (!is.null(y_name)) {
    names(inputs)[names(inputs) == "y"] <- y_name
  }
  if (!is.null(x_name)) {
    names(inputs)[names(inputs) == "x"] <- x_name
  }
  null_inputs <- vapply(inputs, FUN = is.null, FUN.VALUE = logical(1))
  if (all(null_inputs)) {
    stop("All inputs were `NULL`. Ensure some inputs are not `NULL`, then ",
         "try again.", call. = FALSE)
  }
  input_lengths <- lengths(inputs)[!null_inputs]
  varnames <- names(input_lengths)

  # Stop with informative error if any inputs have length 0
  is_zero_length <- input_lengths == 0
  if (any(is_zero_length)) {
    err_input_is_zero_length(varnames, is_zero_length, call)
  }

  # Stop with informative error if inputs are not recyclable
  is_max_input_length <- input_lengths == max(input_lengths)
  is_unrecyclable <- input_lengths != 1 & !is_max_input_length
  is_unrecyclable[varnames == "acronym"] <- FALSE
  if (any(is_unrecyclable)) {
    err_inputs_unrecyclable(varnames, is_unrecyclable, call)
  }
  invisible(inputs)
}

#' Format errors where inputs are zero-length
#' @param varnames A character vector with variable names.
#' @param is_zero_length A logical vector the same length as `varnames`, which
#'   is used to pluck out values in `varnames` to be used by `rlang::abort()`.
#' @returns Is only called to throw errors, which tell the user which of their
#'   input(s) was zero-length.
#' @noRd
err_input_is_zero_length <- function(varnames,
                                     is_zero_length,
                                     call = rlang::caller_env) {
  zero_len_names <- varnames[is_zero_length]
  count <- sum(is_zero_length)

  if (any(zero_len_names == "acronym")) {
    str_acronym_err <- paste0("Variable 'acronym': Input had length 0, but ",
                              "must have length 1.")
  } else {
    str_acronym_err <- ""
  }

  # If only one input is zero-length, throw errors for one variable
  if (all(zero_len_names == "acronym")) {
    rlang::abort(str_acronym_err, call = call)
  } else if (count == 1) {
    rlang::abort(
      paste0("Variable '", zero_len_names, "': Input had length 0, but ",
             "must have length 1 or greater."),
      call = call, class = "gigs_err_zero_length"
    )
  }

  # If multiple inputs are zero-length, need to print differently for vectors
  # (`z`/`y`/`p`/`x`/`sex` etc.) vs scalar (`acronym`)
  varname_str <- paste0(zero_len_names[zero_len_names != "acronym"],
                        collapse = "', '")
  str_acronym_err <- paste0("\n", str_acronym_err)
  if (length(zero_len_names[zero_len_names != "acronym"]) == 1) {
    var_str <- "Variable"
  } else {
    var_str <- "Variables"
  }
  rlang::abort(
    paste0(var_str, " '", varname_str, "': Inputs had length 0, but ",
           "must have length 1 or greater.", str_acronym_err),
    call = call, class = "gigs_err_zero_length"
  )
}

#' Format errors where inputs are unrecyclable
#' @param varnames A character vector with variable names.
#' @param is_unrecyclable A logical vector the same length as `varnames`, which
#'   is used to pluck out values in `varnames` to be used by `rlang::abort()`.
#' @returns Is only called to throw errors, which tell the user which of their
#'   inputs have bad lengths for tidyverse-style recycling.
#' @noRd
err_inputs_unrecyclable <- function(varnames,
                                    is_unrecyclable,
                                    call = rlang::caller_env()) {
  unrecyclable_names <- varnames[is_unrecyclable]
  count <- sum(is_unrecyclable)
  var_str <- if(count > 1) "Variables" else "Variable"
  input_str <- if(count > 1) "Inputs" else "Input"
  varname_str <- paste0(unrecyclable_names, collapse = "', '")
  rlang::abort(
    message =
      paste0(var_str, " '", varname_str, "': ", input_str, " cannot be ",
             "recycled with `vctrs::vec_recycle_common()`. Check the ",
             "documentation to ensure your inputs adhere to the vctrs ",
             "recycling rules ",
             "(https://vctrs.r-lib.org/reference/vec_recycle.html)."),
    class = "gigs_err_unrecyclable",
    call = call
  )
}

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.4a} This file's functions are all documented with `{roxygen2}`.
#' @srrstats {G2.0, G2.1, G2.2} Length/type/univariate input assertions using
#'   `{checkmate}` package.
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.16} The various `handle_*`
#'   functions in this file provide a framework for detecting and handling
#'   missing, undefined, and other invalid data inputs to gigs functions. The
#'   behaviour of these functions can be customised using [gigs_options_set()]
#'   to set invalid values to `NA` silently, set invalid values to `NA` with
#'   warnings, or to throw errors.
#' @srrstats {EA2.6} The `remove_attributes()` function is used to make sure
#'   vector-type inputs with odd class structures (e.g. as in `units` package)
#'   can still be used as input to [gigs]. This is done after assertions on
#'   length etc.
NULL
