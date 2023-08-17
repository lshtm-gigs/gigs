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

#' Linearly interpolate between LMS or MSNT coefficients
#' @param coeff_tbl_long A table of reference LMS/MSNT coefficients, from within
#' `gigs::who_gs_coeffs` or `gigs:::ig_nbs_coeffs`
#' @param xvar A value of x which is not found in a coefficient table but is
#' between two values in that coefficient table
#' @param sex A character denoting male (`"M"`) or female (`"F"`)
#' @return Data frame containing LMS/MSNT values which have been sourced from
#' interpolated between existing LMS/MSNT values.
#' @note All inputs should be length one. The function will also fail if
#' `coeff_tbl_long` does not contain named LMS/MSNT values.
#' @importFrom stats approx
#' @keywords internal
#' @noRd
interpolate_coeffs <- function(coeff_tbl_long, xvars, sex, acronym) {
  if (all(c("L", "M", "S") %in% names(coeff_tbl_long))) {
    coeff_names <-  c("L", "M", "S")
    out <- data.frame(x = xvars, sex = sex, acronym = acronym)
  } else if (all(c("mu", "sigma", "nu", "tau") %in% names(coeff_tbl_long))) {
    coeff_names <- c("mu", "sigma", "nu", "tau")
    out <- data.frame(gest_age = xvars, sex = sex, acronym = acronym)
  }
  for (coeff in coeff_names) {
    out[coeff] <- approx(x = coeff_tbl_long[,1],
                         y = coeff_tbl_long[,coeff],
                         xout = xvars)$y
  }
  out
  stop("Fails when multiple standards/sexes are in use. Needs a refactor OR apply() use.")
}