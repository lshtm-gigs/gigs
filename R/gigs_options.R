#' Get and set gigs package options
#'
#' @param option A single character variable, one of:
#'   * `"handle_missing"` - How should gigs handle missing (`NA`) data?
#'   * `"handle_undefined"` - How should gigs handle undefined (`NaN`, `Inf`,
#'       `-Inf`) data?
#'   * `"handle_invalid_xvar"` - How should gigs handle `x` variables that
#'     are out of bounds for the standard in use?
#'   * `"handle_invalid_sex"` - How should gigs handle elements of `sex` which
#'     are not one of `"M"` or `"F"`?
#'   * `"handle_invalid_acronym"` - In conversion functions, how should gigs
#'     handle `acronym` elements which do not map onto any implemented growth
#'     standard?
#'   * `"handle_invalid_centiles"` - In `centile2value` functions, how should
#'     gigs handle elements of `p` that are not between `0` and `1`?
#' @description By default, gigs will handle invalid input data, e.g. missing
#'   (`NA`) or undefined (`NaN`, `Inf` and `-Inf`) values, by warning you of
#'   their presence and replacing the invalid values with `NA`. You can change
#'   this behaviour with `gigs_option_set()` so that gigs does this silently, or
#'   make gigs throw errors when confronted with bad data.
#' @returns For [gigs_option_get()], noisiliy returns the current value of
#'   `option`. For [gigs_option_set()], invisibly returns the new value of
#'   `option`.
#' @examples
#' # Get the names of all available options
#' names(.gigs_options)
#'
#' # Retrieve the value of a given option --> will invisibly return value and
#' # print to console
#' gigs_option_get("handle_missing_data")
#'
#' # Set the value of an option --> will invisibly return the new value and
#' # print your change to the console
#' gigs_option_set("handle_missing_data", "quiet")
#' @name gigs_options
#' @export
gigs_option_get <- function(option, silent = FALSE) {
  checkmate::qassert(option, rules = "S1") |>
    checkmate::assert_subset(choices = names(.gigs_options))
  value <- get(option, envir = .gigs_options, inherits = FALSE)
  if (!silent) {
    message("gigs options: `", option, "` is currently set to \"", value, "\".")
  }
  value
}

#' @name gigs_options
#' @param new_value A single character variable, one of:
#'   * `"quiet"` - gigs will replace invalid vector elements to `NA` quietly
#'   * `"warn"` - gigs will replace invalid vector elements with `NA` loudly
#'   * `"error"` - gigs will throw informative errors if invalid inputs are
#'     encountered
#' @param silent A single logical value denoting whether the function will send
#'   a `message()` to the console describing either:
#'   * The current value of `option` for `gigs_option_get()`.
#'   * The newly-set value of `option` for `gigs_option_set()`.
#' @export
gigs_option_set <- function(option, new_value, silent = FALSE) {
  checkmate::qassert(silent, rules = "B1")
  checkmate::qassert(option, rules = "S1")
  checkmate::assert_subset(option, choices = names(.gigs_options))
  checkmate::qassert(new_value, rules = "S1")
  checkmate::assert_subset(new_value, choices = c("quiet", "warn", "error"))
  assign(x = option, value = new_value, envir = .gigs_options, inherits = FALSE)
  if (!silent) {
    message("gigs options: `",  option, "` is now set to \"", new_value, "\".")
  }
  invisible(new_value)
}
