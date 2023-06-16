#' Return male/female mean if sex is undefined
#'
#' @param fn Conversion function to call
#' @param arg1 z, p or y values to pass to `fn`
#' @param x_arg X values (usually age values) to pass to `fn`
#' @param acronym Acronym values to pass to `fn`
#' @keywords internal
mean_if_sex_undefined <- function(fn, arg1, x_arg, acronym) {
  rowMeans(cbind(fn(arg1, x_arg, "M", acronym), fn(arg1, x_arg, "F", acronym)))
}

#' Round, but round 0.5 up in all cases
#'
#' @param x Value(s) to round
#' @param digits Number of digits to round to
#' @note Taken from https://stackoverflow.com/questions/12688717/round-up-from-5
#' @returns Values of `x` rounded to `digits` number of digits.
#' @keywords internal
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
#' @returns Data frame containing LMS/MSNT values which have been sourced from
#' interpolated between existing LMS/MSNT values.
#' @note All inputs should be length one. The function will also fail if
#' `coeff_tbl_long` does not contain named LMS/MSNT values.
#' @importFrom stats approx
#' @keywords internal
interpolate_coeffs <- function(coeff_tbl_long, xvar, sex, acronym) {
  xfloor <- floor(xvar)
  xceiling <- ceiling(xvar)
  coeffs <- data.frame(c(xfloor, xceiling),
                       sex = rep_len(sex, length.out = 2),
                       acronym = rep_len(acronym, length.out = 2))
  if (all(c("L", "M", "S") %in% names(coeff_tbl_long))) {
    coeff_names <-  c("L", "M", "S")
  } else if (all(c("mu", "sigma", "nu", "tau") %in% names(coeff_tbl_long))) {
    coeff_names <- c("mu", "sigma", "nu", "tau")
  }
  names(coeffs)[1] <- names(coeff_tbl_long[1])

  coeffs <- merge(coeffs, coeff_tbl_long, all.x = TRUE, sort = FALSE)
  lerped_coeffs <- sapply(
    X = 4:length(names(coeffs)),
    FUN = function(x) {
      coeff1 <- coeffs[1, x]
      coeff2 <- coeffs[2, x]
      # Do not lerp if adjacent LMS/MSNT values are equal
      if (coeff1 == coeff2) {
        return(coeff1)
      } else {
        approx(c(xfloor, xceiling), c(coeff1, coeff2), xout = xvar)$y
      }
    }) |>
    t()|>
    as.data.frame()
  names(lerped_coeffs) <- coeff_names
  final_df <- cbind(coeffs[1, 1:3], lerped_coeffs)
  final_df[1,1] <- xvar
  final_df
}