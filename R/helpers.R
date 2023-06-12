#' Extend vectors in list to the length of the longest vector
#'
#' @param vec_list List of vectors to extend out to the length of the longest
#' vector in that list.
#' @returns List of vectors recycled to length of longest vector in input list.
#' @keywords internal
rep_to_longest <- function(vec_list) {
  lengths <- vapply(X = vec_list, FUN = length, FUN.VALUE = numeric(length = 1))
  max_len <-  max(lengths)
  if (any(lengths != 1 & lengths != max_len)) {
    bad_len <- lengths[which(lengths != 1 & lengths != max_len)]
    stop(paste0("Inputs must be length 1 or the length of the longest input ",
                "vector (", max_len,
                "). You provided an input/input with length(s)", bad_len, "."),
         .call = NULL)
  }
  lengthened <- lapply(X = vec_list, FUN = rep_len, length.out = max_len)
}

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
#'
#' @param coeff_tbl_long A table of reference LMS/MSNT coefficients, from within
#' `gigs::who_gs_coeffs` or `gigs:::ig_nbs_coeffs`
#' @param xvar A value of x which is not found in a coefficient table but is
#' between two values in that coefficient table
#' @param sex A character denoting male (`"M"`) or female (`"F"`)
#' @note All inputs should be length one. The function will also fail if
#' `coeff_tbl_long` does not contain named LMS/MSNT values.
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
  n <- 100
  interval <- (xvar - xfloor) / (xceiling - xfloor) * n
  lerped_coeffs <- sapply(
    X = 4:length(names(coeffs)),
    FUN = function(x) {
      # Do not bother lerping if adjacent LMS/MSNT values are equal
      if (coeffs[1, x] == coeffs[2, x]) {
        return(coeffs[1, x])
      } else {
        approx(seq_along(coeffs[, x]), coeffs[, x], n = n)$y[interval]
      }
    }) |>
    t()|>
    as.data.frame()
  names(lerped_coeffs) <- coeff_names
  final_df <- cbind(coeffs[1, 1:3], lerped_coeffs)
  final_df[1,1] <- xvar
  final_df
}