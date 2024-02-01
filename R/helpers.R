# Conversion function helpers --------------------------------------------------

#' Convert between z-scores and values using $mu$ and $sigma$
#'
#' @param z Numeric vector of length one or more with z-scores to convert to
#'   y values.
#' @param y Numeric vector of length one or more with y values to convert to
#'   z-scores.
#' @param mu Numeric vector of same length as `z`/`y` mean value(s).
#' @param mu Numeric vector of same length as `z`/`y` standard deviation(s).
#' @returns Numeric vector the same length of `z`/`y`, `mu`, and `sigma` with
#'   either expected anthropometric values or z-scores depending on which
#'   function is called.
#' @rdname mu_sigma_conv
#' @noRd
mu_sigma_z2y <- function(z, mu, sigma) mu + z * sigma

#' @noRd
#' @rdname mu_sigma_conv
mu_sigma_y2z <- function(y, mu, sigma) (y - mu) / sigma

# Parameter checking -----------------------------------------------------------

#' Check if values in `x` are within `min()` and `max()` of another numeric
#'   vector
#'
#' @param x Numeric vector to compare against min/max values of `vec`.
#' @param vec Numeric vector with one or more elements from which min/max bounds
#'   are defined. Should contain no missing (`NA`) or undefined (`NaN`, `Inf`,
#'   or `-Inf`) values.
#' @returns Logical vector with `TRUE` if `x` is within the minimum and maximum
#'   values of `vec`, else `FALSE`.
#' @noRd
inrange <- function(x, vec) {
  checkmate::qassert(vec, rules = "N+()")
  x >= min(vec) & x <= max(vec)
}

# Custom error messages --------------------------------------------------------

#' Throw an error if object lengths are unequal
#' @param ... An arbitrary number of arguments which will be checked for
#'   equality of length.
#' @returns Invisibly returns inputs in list. Throws an error if the lengths of
#'   passed-in objects are not equal.
#' @noRd
stop_if_lengths_unequal <- function(...) {
  lengths <- lengths(x = list(...))
  if (length(unique(lengths)) > 1) {
    stop(paste("Your inputs had different lengths. Please give the function",
               "input vectors of the same length."), call. = FALSE)
  }
  invisible(list(...))
}

# Extra ------------------------------------------------------------------------

#' Drop null elements from a list
#'
#' @param list List which may or may not contain `NULL` elements.
#' @returns The list supplied as `list`, but with `NULL` elements removed.
#' @noRd
drop_null_elements <- function(list) {
  null_elems <- vapply(list, is.null, FUN.VALUE = logical(length = 1))
  list[!null_elems]
}

#' Remove attributes from a vector
#'
#' @param x Object from which to remove attributes.
#' @note This function is used within the package to ensure that atomic vector
#'   inputs can be used irrespective of their class. Internally, it is only used
#'   following calls to assert atomic vector input.
#' @srrstats {G2.6, EA2.6} Infrastructure to ensure atomic vector inputs with
#'   odd classes do not cause errors.
#' @returns `x`, without any attributes.
#' @noRd
remove_attributes <- function(x) {
  attributes(x) <- NULL
  x
}

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.4a} This file's functions are all documented with `{roxygen2}`.
NULL
