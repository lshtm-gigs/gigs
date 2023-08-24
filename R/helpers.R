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

#' Retrieve coefficients for coefficient-based standards
#'
#' @description Retrieves growth standard coefficient values where needed for
#' the WHO Child Growth Standards and INTERGROWTH-21<sup>st</sup> Newborn Size
#' @param x X variable in whatever range and units are appropriate for the
#' standard(s) in use.
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param acronym Acronym(s) denoting which LMS/GAMLSS-based growth standards
#' are in use.
#' @return A dataframe containing coefficients (where avilable) for each
#' provided combination of sex, gestational age, and acronym.
#' @rdname retrieve_coefficients
#' @importFrom data.table rbindlist
#' @importFrom data.table merge.data.table
#' @importFrom data.table setorder
#' @keywords internal
#' @noRd
retrieve_coefficients <- function(x, sex, acronym, coeff_tbls, coeff_names) {
  # 1. Make DT with x, sex and acronym
  new_dt <- data.table::data.table(x = x, sex = sex, acronym = acronym,
                                   sex_acronym = paste0(sex, "_", acronym),
                                   sort = seq_along(x))

  # 2. Load coeff tables as needed
  unique_sex_acronym <- unique(.subset2(new_dt, "sex_acronym"))
  coeffs_li <- lapply(
    X = unique_sex_acronym,
    FUN = \(x) load_coeff_tables(sex_acronym = x, coeff_tbls = coeff_tbls))

  # 2a. Return NA coefficient values if no coeff tables were loaded
  no_coeff_tables <- all(vapply(coeffs_li,
                                FUN = is.null,
                                FUN.VALUE = logical(length = length(1))))
  if (no_coeff_tables) {
    new_dt[, (coeff_names) := as.list(rep(NA, length(coeff_names)))]
    return(new_dt[, c("sort", "sex_acronym") := NULL])
  }

  # For each set of sex/acronym combination, check against the coefficient table
  names(coeffs_li) <- unique_sex_acronym
  coeffs_interpolated <- lapply(
    X = unique_sex_acronym,
    FUN = \(sex_acro) {
      coeff_dt <- .subset2(coeffs_li, sex_acro)
      is_curr_sex_acro <- new_dt$sex_acronym == sex_acro
      current <- new_dt[is_curr_sex_acro, ]
      not_in_coeff_tbl <- !current$x %in% coeff_dt$x &
                            inrange(current$x, coeff_dt$x)
      if (length(not_in_coeff_tbl)) {
        not_in_coeff_tbl_dt <- current[not_in_coeff_tbl, ]
        interp_dt <- interpolate_coeffs(
          needs_interp_dt = not_in_coeff_tbl_dt,
          coeff_names = coeff_names,
          coeff_tbl_long = coeff_dt)
      }
    }
  ) |>
    data.table::rbindlist()

  # 4. Get coeffs for variables not needing interpolation; combine
  #    non-interpolated with interpolated coeffs if necessary; return
  coeffs_long <- data.table::rbindlist(coeffs_li)
  in_coeff_tbl <- x %in% coeffs_long$x & inrange(x, coeffs_long$x)
  merge_dt <- new_dt[in_coeff_tbl, ]
  # This merge.data.table call is the slowest one - find a faster alternative to
  # a full table match *if* speed becomes an issue
  out <- data.table::merge.data.table(merge_dt, coeffs_long,
                                      all.x = TRUE, sort = FALSE,
                                      by = c("x", "sex", "acronym"))
  if (exists(x = "coeffs_interpolated")) {
    out <- rbind(out, coeffs_interpolated, fill = TRUE)
  }
  out <- data.table::merge.data.table(new_dt, out, all.x = TRUE)
  data.table::setorder(out, sort)
  out[, c("sort", "sex_acronym") := NULL]
}

#' Load coefficient tables for `retrieve_coefficients()`
#'
#' @param sex Sex, either `"M"` (male) or `"F"` (female).
#' @param acronym Acronym denoting which coefficient table is needed.
#' @param coeff_tbls A nested list of coefficient tables from either the WHO
#' Child Growth Standards/INTERGROWTH-21<sup>st</sup> project.
#' @return A `data.table` containing coefficients specified by the combination
#' of `sex`, `acronym` and `coeff_tbls`.
#' @importFrom data.table setDT
#' @keywords internal
#' @noRd
load_coeff_tables <- function(sex_acronym, coeff_tbls) {
  split <- unlist(strsplit(sex_acronym, split = "_"))
  sex <- split[1]
  acronym <- split[2]
  if (acronym == "NA") {
    return(NULL)
  }
  sex_long <- if (sex == "M") "male" else "female"
  coeff_tbl <- data.table::setDT(coeff_tbls[[acronym]][[sex_long]])
  coeff_tbl[, c("sex", "acronym") := list(sex, acronym)]
  colnames(coeff_tbl)[1] <- "x"
  coeff_tbl
}

#' Linearly interpolate between LMS or MSNT coefficients
#' @param coeff_tbl_long A table of reference LMS/MSNT coefficients, from within
#' `gigs::who_gs_coeffs` or `gigs:::ig_nbs_coeffs`
#' @param xvar A value of x which is not found in a coefficient table but is
#' between two values in that coefficient table.
#' @param sex A character denoting male (`"M"`) or female (`"F"`).
#' @return Data frame containing LMS/MSNT values which have been sourced from
#' interpolated between existing LMS/MSNT values.
#' @note All inputs should be length one. The function will also fail if
#' `coeff_tbl_long` does not contain named LMS/MSNT values.
#' @importFrom stats approx
#' @keywords internal
#' @noRd
interpolate_coeffs <- function(needs_interp_dt, coeff_tbl_long, coeff_names) {
  interpolated <- lapply(
    X = coeff_names,
    FUN = \(coeff_name) {
      stats::approx(x = .subset2(coeff_tbl_long, "x"),
                    y = .subset2(coeff_tbl_long, coeff_name),
                    xout = .subset2(needs_interp_dt, "x"))$y
    })
  names(interpolated) <- coeff_names
  needs_interp_dt[, names(interpolated) := interpolated]
}