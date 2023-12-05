#' Convert between z-scores and values using $mu$ and $sigma$
#'
#' @param z Nuumeric vector of z-scores to convert to y values.
#' @param y Numeric vector of y values to convert to z-scores.
#' @param mu Numeric vector of mean value(s).
#' @param sigma Numeric vector of standard deviation(s).
#' @returns Y values or z-scores depending on which function is called
#' @noRd
#' @family mu_sigma_conv
mu_sigma_z2y <- function(z, mu, sigma) mu + z * sigma

#' @noRd
#' @family mu_sigma_conv
mu_sigma_y2z <- function(y, mu, sigma) (y - mu) / sigma

#' Return male/female mean if sex is undefined
#'
#' @param fn Conversion function to call
#' @param arg1 `z`, `p` or `y` values to pass to `fn`
#' @param x_arg `x` values (usually age/gestational age values) to pass to `fn`
#' @param acronym Acronyms to pass to `fn`
#' @noRd
mean_if_sex_undefined <- function(fn, arg1, x_arg, acronym) {
  len_sex <- length(arg1)
  rowMeans(cbind(fn(arg1, x_arg, rep("M", len_sex), acronym),
                 fn(arg1, x_arg, rep("F", len_sex), acronym)))
}

# Parameter checking -----------------------------------------------------------

#' Check if values in `x` are within upper and lower bounds of another numeric
#' vector.
#'
#' @param x Numeric vector to compare against min/max values of `vec`.
#' @param vec Numeric vector from which min/max bounds are defined.
#' @note The `min()` and `max()` calls in this function have `na.rm = TRUE`.
#' @return Logical vector with `TRUE` if `x` is within the minimum and maximum
#'   values of `vec`, else `FALSE`. Where `x` is `NA`, `inrange()` will return
#'   `NA`.
#' @noRd
inrange <- function(x, vec) {
  x >= min(vec, na.rm = TRUE) & x <= max(vec, na.rm = TRUE)
}

#' Check if `pma_weeks` values are within INTERGROWTH-21<sup>st</sup> Postnatal
#' Growth Standards range.
#'
#' @param pma_weeks Post-menstrual age in weeks.
#' @returns Logical vector with `TRUE` where `pma_weeks` is between 27 to 64
#'   weeks, else `FALSE`.
#' @noRd
is_valid_pma_weeks <- function(pma_weeks) inrange(pma_weeks, c(27, 64))

# Custom error messages --------------------------------------------------------

#' Throw an error if object lengths are unequal
#' @param ... An arbitrary number of arguments which will be checked for
#'   equality of length.
#' @return Invisibly returns inputs in list. Throws an error if the lengths of
#'   passed-in objects are not equal.
#' @noRd
stop_if_lengths_unequal <- function(...) {
  lengths <- vapply(X = list(...),
                    FUN = length,
                    FUN.VALUE = numeric(length = 1L))
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
  null_elems <- vapply(list, is.null, FUN.VALUE = logical(1))
  list[!null_elems]
}
