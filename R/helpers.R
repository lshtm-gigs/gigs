#' Convert between z-scores and values using $mu$ and $sigma$
#'
#' @param z $z$-scores to convert to $y$ values
#' @param y $y$ values to convert to z-scores
#' @param mu Mean value(s).
#' @param sigma Standard deviation(s).
#' @returns Y values or z-scores depending on which function is called
#' @keywords internal
#' @noRd
#' @family mu_sigma_conv
mu_sigma_z2y <- function(z, mu, sigma) mu + z * sigma

#' @keywords internal
#' @noRd
#' @family mu_sigma_conv
mu_sigma_y2z <- function(y, mu, sigma) (y - mu) / sigma

#' Return male/female mean if sex is undefined
#'
#' @param fn Conversion function to call
#' @param arg1 z, p or y values to pass to `fn`
#' @param x_arg X values (usually age values) to pass to `fn`
#' @param acronym Acronym values to pass to `fn`
#' @keywords internal
#' @noRd
mean_if_sex_undefined <- function(fn, arg1, x_arg, acronym) {
  rowMeans(cbind(fn(arg1, x_arg, "M", acronym), fn(arg1, x_arg, "F", acronym)))
}

#' Round, but round 0.5 up in all cases
#'
#' @param x Value(s) to round
#' @param digits Number of digits to round to
#' @note Taken from https://stackoverflow.com/questions/12688717/round-up-from-5
#' @return Values of `x` rounded to `digits` number of digits.
#' @keywords internal
#' @noRd
round2 <- function(x, digits) {
  posneg <- sign(x)
  z <- abs(x) * 10 ^ digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z / 10 ^ digits
  z*posneg
}
