#' Functions to get and set gigs package-level options
#'
#' @description By default, gigs will handle invalid input data, e.g. missing
#'   (`NA`) or undefined (`NaN`, `Inf` and `-Inf`) values, by warning you of
#'   their presence and replacing the invalid values with `NA`. You can change
#'   this behaviour with `gigs_option_set()` so that gigs does this silently, or
#'   make gigs throw errors when confronted with bad data. You can also change
#'   all GIGS options at once with `gigs_options_set()`.
#' @param option A single case-sensitive character variable, one of:
#'   * `"handle_missing_data"` - How should gigs handle missing (`NA`) elements?
#'   * `"handle_undefined_data"` - How should gigs handle undefined (`NaN`,
#'     `Inf`, `-Inf`) elements?
#'   * `"handle_oob_xvar"` - How should gigs handle elements of `x` variables
#'     which are out of bounds for the standard in use?
#'   * `"handle_invalid_sex"` - How should gigs handle elements of `sex` which
#'     are not one of `"M"` or `"F"`?
#'   * `"handle_oob_centiles"` - In `centile2value` functions, how should
#'     gigs handle elements of `p` that are not between `0` and `1`?
#' @param silent A single logical value controlling whether a `message()` will
#'   be printed to the console describing either:
#'   * The current value of `option` in `gigs_option_get()`.
#'   * The `new_value` of `option` in `gigs_option_set()`.
#'
#'   This argument will not accept an integer in place of a logical value.
#' @param new_value A single case-sensitive character variable, one of:
#'   * `"quiet"` - gigs will replace invalid vector elements with `NA` quietly.
#'   * `"warn"` - gigs will replace invalid vector elements with `NA` loudly.
#'   * `"error"` - gigs will throw informative errors if any invalid inputs are
#'     encountered.
#' @returns A single-length character vector. For `gigs_option_get()`, the
#'   current value of `option`; for `gigs_option_get()`, the new value of
#'   `option`; for `gigs_options_set()`, `new_value`. If `silent = FALSE`
#'   (default), then informative messages will be printed to the console
#'   regarding the current/new values of `option`.
#' @examples
#' # Show the names of all available options
#' names(.gigs_options)
#'
#' # Retrieve the value of a gigs option (if `silent = FALSE`, would print a
#' # message)
#' option_value <- gigs_option_get("handle_missing_data", silent = TRUE)
#' print(option_value)
#'
#' # Set the value of an option
#' gigs_option_set("handle_undefined_data", "error", silent = TRUE)
#'
#' # Check that change has occurred
#' option_value <- gigs_option_get("handle_undefined_data", silent = TRUE)
#' print(option_value)
#'
#' # Suppress printed output with `silent = TRUE`
#' gigs_option_set("handle_undefined_data", "quiet", silent = TRUE)
#'
#' # Set all GIGS options
#' gigs_options_set("warn", silent = TRUE)
#' @name gigs_options
#' @export
gigs_option_get <- function(option, silent = FALSE) {
  checkmate::qassert(option, rules = "S1")
  checkmate::qassert(silent, rules = "B1")
  checkmate::assert_subset(option, choices = names(gigs::.gigs_options))
  value <- get(option, envir = gigs::.gigs_options, inherits = FALSE)
  if (!silent) {
    rlang::inform(
      c("i" = paste0("gigs options: '", option, "' is currently set to \"",
                     value, "\"."))
    )
  }
  invisible(value)
}

#' @name gigs_options
#' @export
gigs_option_set <- function(option, new_value, silent = FALSE) {
  checkmate::qassert(option, rules = "S1")
  checkmate::qassert(silent, rules = "B1")
  checkmate::qassert(new_value, rules = "S1")
  checkmate::assert_subset(option, choices = names(gigs::.gigs_options))
  checkmate::assert_subset(new_value, choices = c("quiet", "warn", "error"))
  assign(x = option, value = new_value, envir = gigs::.gigs_options,
         inherits = FALSE)
  if (!silent) {
    rlang::inform(
      c("i" = paste0("gigs options: '",  option, "' is now set to \"",
                     new_value, "\"."))
    )
  }
  invisible(new_value)
}

#' @name gigs_options
#' @export
gigs_options_set <- function(new_value, silent = FALSE) {
  checkmate::qassert(silent, rules = "B1")
  checkmate::qassert(new_value, rules = "S1")
  checkmate::assert_subset(new_value, choices = c("quiet", "warn", "error"))
  for (option in names(.gigs_options)) {
    gigs_option_set(option, new_value, silent = silent)
  }
  invisible(new_value)
}

#' @name .gigs_options
#' @title Package-level gigs options
#' @description An environment containing six named character vectors. These
#'   define how gigs handles inputs with missing, undefined, or invalid
#'   elements:
#'   * `"handle_missing_data"` - How should gigs handle missing (`NA`) elements?
#'   * `"handle_undefined_data"` - How should gigs handle undefined (`NaN`,
#'       `Inf`, `-Inf`) elements?
#'   * `"handle_oob_xvar"` - How should gigs handle elements of `x`
#'       variables which are out of bounds for the standard in use?
#'   * `"handle_invalid_sex"` - How should gigs handle elements of `sex` which
#'     are not one of `"M"` or `"F"`?
#'   * `"handle_oob_centiles"` - In `centile2value` functions, how should
#'     gigs handle elements of `p` that are not between `0` and `1`?
#'
#'   Each of these options can take one of three values:
#'   * `"quiet"` - Invalid elements are set to `NA`, silently.
#'   * `"warn"` - Invalid elements are set to `NA`, with warnings issued when
#'     this is done.
#'   * `"error"` - Invalid elements will cause informative errors.
#'
#'   By default, each option in `.gigs_options` is set to `"warn"`, so you will
#'   be informed of any invalid data used as input. Use  `gigs_option_set()` or
#'   `gigs_option_set()` to change this behaviour.
#' @seealso The `gigs_option_get()`, `gigs_option_set()` and
#'   `gigs_options_set()` functions, which can be used to get and set values in
#'   `.gigs_options`.
#' @examples
#' # Get the names of all available options
#' names(.gigs_options)
#' @export
NULL

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G2.0, G2.0a, G2.1, G2.1a, G2.2, G2.3, EA1.3} Validation of data
#'   inputs using the `checkmate` package, and clear documentation of what
#'   types/lengths of input are supported.
NULL