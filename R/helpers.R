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
#' @param arg1 z, p or y values to pass to `fn`
#' @param x_arg X values (usually age values) to pass to `fn`
#' @param acronym Acronym values to pass to `fn`
#' @noRd
mean_if_sex_undefined <- function(fn, arg1, x_arg, acronym) {
  rowMeans(cbind(fn(arg1, x_arg, "M", acronym), fn(arg1, x_arg, "F", acronym)))
}

#' Stop with helpful information with a vector of the wrong type.
#'
#' @param vec Vector to test the type of.
#' @param type One-length character vector describing the second part of some
#'   `is.*()`-type function, usually `"logical"`, `"numeric"`, or `"character"`
#'   (which will call `is.logical()`, `is.numeric()`, `is.character()`,
#'   respectively.
#' @return Returns `vec` invisibly. Will stop with error if the type of `vec` is
#'   not the same as `expected`.
#' @noRd
stop_if_wrong_type <- function(vec, type) {
  vec_outer_name <- deparse(substitute(vec))
  is_expected_type <- get(paste0("is.", type), asNamespace("base"))
  if (!is_expected_type(vec)) {
    stop("`", vec_outer_name, "` has the wrong type. Should be `", type,
         "` but was actually `", typeof(vec), "`.", call. = FALSE)
  }
  invisible(vec)
}

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