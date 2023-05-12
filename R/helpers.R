#' Extend vectors in list to the length of the longest vector
#'
#' @param vec_list List of vectors to extend out to the length of the longest vector in that list.
#' @returns List of vectors recycled to length of longest vector in input list.
#' @keywords internal
rep_to_longest <- function(vec_list) {
  lengths <- vapply(X = vec_list, FUN = length, FUN.VALUE = numeric(length = 1))
  max_len <-  max(lengths)
  if (any(lengths != 1 & lengths != max_len)) {
    bad_len <- lengths[which(lengths != 1 & lengths != max_len)]
    stop(paste0("Inputs must be length 1 or the length of the longest input vector (", max_len, 
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