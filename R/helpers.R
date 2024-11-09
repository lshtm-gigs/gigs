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

#' Run an internal GIGS conversion function over subsetted input vectors
#'
#' Sometimes it is helpful apply a growth standard in specific elements of a
#' vector. This function does just that, and works for converting in all but the
#' INTERGROWTH-21<sup>st</sup> Fetal standards.
#'
#' @param fn Function to run using `in1`, `in2`, `in3`, each of which will be
#'   subsetted to where elements of `lgl` are `TRUE`.
#' @param lgl Logical vector of length one or more indicating which indices of
#'   `in1` `in2` and `in3` should be operated on `by `fn`.
#' @param in1,in2,in3 Three vectors of the same length as `lgl` which are
#'   used as inputs to `fn`.
#' @param acronym A single-length character vector denoting which growth
#'   standard to use.
#' @return Returns an empty `double()` vector if `any(lgl) == FALSE`, else
#'   returns the result of applying `fn()` over `in1[lgl]`, `in2[lgl]`, and
#'   `in3[lgl]`.
#' @noRd
fn_on_subset <- function(fn, lgl, in1, in2, in3, acronym) {
  if (any(lgl)) {
     fn(in1[lgl], in2[lgl], in3[lgl], acronym)
  } else {
    double(length = 0L)
  }
}

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

# Extra ------------------------------------------------------------------------

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

#' Use `paste()` to separate elements in a vector with commas
#' @param x A character vector to paste and separate with commas
#' @returns A single string with the elements of `x` separated by commas, e.g.
#'   `paste_sep_commas(c("A", "B", "C"))` becomes `"\`A\`, \`B\`, \`C\`"`.
#' @noRd
paste_sep_commas_quoted <- function(x) {
  paste0("`\"", x, "\"`", collapse = ", ")
}

#' Use `paste()` to separate elements in a vector with commas
#' @param x A character vector to paste and separate with commas
#' @returns A single string with the elements of `x` separated by commas, e.g.
#'   `paste_sep_commas(c("A", "B", "C"))` becomes `"\`A\`, \`B\`, \`C\`"`.
#' @noRd
paste_sep_commas <- function(x) {
  paste0("`", x, "`", collapse = ", ")
}

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.4a} This file's functions are all documented with `{roxygen2}`.
NULL
