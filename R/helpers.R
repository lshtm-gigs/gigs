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
#' @keywords internal
#' @noRd
retrieve_coefficients <- function(x, sex, acronym, coeff_tbls, coeff_names) {
  stop("Run devtools::testthat() --> interpolation tests are broken")
  sex_acronym <- paste0(sex, "_", acronym)
  key_x <- seq_along(x)

  # 2. Load coeff tables as needed
  unique_sex_acronym <- unique(sex_acronym)
  coeffs_li <- lapply(
    X = unique_sex_acronym,
    FUN = \(x) load_coeff_matrices(sex_acronym = x, coeff_tbls = coeff_tbls)
  )

  # 2a. Return NA coefficient values if no coeff tables were loaded
  no_coeff_tables <- all(vapply(coeffs_li,
                                FUN = is.null,
                                FUN.VALUE = logical(length = 1L)))
  if (no_coeff_tables) {
    return(matrix(ncol = length(coeff_names), nrow = length(x)))
  }

  # For each set of sex/acronym combination, check against the coefficient table
  names(coeffs_li) <- unique_sex_acronym
  coeffs_interpolated <- lapply(
    X = unique_sex_acronym,
    FUN = \(sex_acro) {
      coeff_mat <- .subset2(coeffs_li, sex_acro)
      is_curr_sex_acro <- sex_acronym == sex_acro
      current_x <- x[is_curr_sex_acro]
      not_in_coeff_tbl <- !current_x %in% coeff_mat[,1] &
                            inrange(current_x, coeff_mat[,1])
      if (any(not_in_coeff_tbl)) {
        key_interped_coeffs <- key_x[is_curr_sex_acro][not_in_coeff_tbl]
        interpolated <- interpolate_coeffs(
          x_to_interp = current_x[not_in_coeff_tbl],
          coeff_mat = coeff_mat,
          coeff_names = coeff_names)
          rownames(interpolated) <- key_interped_coeffs
        interpolated
      }
    }
  ) |>
    do.call(what = "rbind")

  # 4. Get coeffs for variables not needing interpolation; combine
  #    non-interpolated with interpolated coeffs if necessary; return

  # 4a. Get matrix keys
  #       i. Add an NA matrix to the end of coeffs_li
  coeffs_li[[length(coeffs_li) + 1]] <- matrix(nrow = 1,
                                               ncol = 1 + length(coeff_names),
                                               dimnames = list("NA_matkey"))
  #      ii. Bind list of matrices into one big matrix
  coeffs_long_mat <- do.call(what = "rbind", coeffs_li)
  #     iii. Test which members of X are in this long coefficient matrix
  # TODO: Broken -> if x is a standard from 91 days this returns TRUE, this causes
  # TODO: the right behaviour but is NOT clear
  in_coeff_tbl <- x %in% coeffs_long_mat[,1] & inrange(x, coeffs_long_mat[,1])
  if (any(in_coeff_tbl)) {
    # 4b. Merge on matrix keys IF all matrix keys are valid
    #       i. Make logical vector based on set inclusion
    mat_keys <- paste0(sex_acronym[in_coeff_tbl], "_", x)
    valid_mat_keys_lgl <- mat_keys %in% rownames(coeffs_long_mat)
    # If any matrix keys are invalid, set to NA row accessor
    if (!all(valid_mat_keys_lgl)) mat_keys[!valid_mat_keys_lgl] <- "NA_matkey"
    #    ii. Subset now all mat_keys are valid
    coeffs_no_lerp <- coeffs_long_mat[mat_keys,
                                      2:(1 + length(coeff_names)),
                                      drop = FALSE]
    rownames(coeffs_no_lerp) <- key_x
  }

  notnull_interp <- !is.null(coeffs_interpolated)
  exists_no_lerp <- exists(x = "coeffs_no_lerp")
  if (exists_no_lerp && notnull_interp) {
    out <- rbind(coeffs_no_lerp, coeffs_interpolated)
  } else if (notnull_interp) {
    out <- coeffs_interpolated
  } else if (exists_no_lerp) {
    out <- coeffs_no_lerp
  } else {
    return(matrix(ncol = length(coeff_names), nrow = length(x)))
  }
  out[as.character(key_x), , drop = FALSE]
}

#' Load coefficient tables for `retrieve_coefficients()`
#'
#' @param sex Sex, either `"M"` (male) or `"F"` (female).
#' @param acronym Acronym denoting which coefficient table is needed.
#' @param coeff_tbls A nested list of coefficient tables, either
#' `gigs::ig_nbs_coeffs` or `gigs::who_gs_coeffs` for the coefficient-based
#' INTERGROWTH-21<sup>st</sup> Newborn Size standards or WHO Child Growth
#' Standards, respectively.
#' @note Uses the internal `coeff_rownames` object, information on which can be
#' found in `data-raw/sysdata.R`.
#' @returns A matrix with coefficients from `coeff_tbls` specified by the
#' combination of `sex` and `acronym`.
#' @keywords internal
#' @noRd
load_coeff_matrices <- function(sex_acronym, coeff_tbls) {
  split <- unlist(strsplit(sex_acronym, split = "_"))
  sex <- split[1]
  acronym <- split[2]
  if (acronym == "NA") {
    return(NULL)
  }
  sex_long <- if (sex == "M") "male" else "female"
  coeff_mat <- as.matrix(coeff_tbls[[acronym]][[sex_long]], ncol = 4)
  rownames(coeff_mat) <- coeff_rownames[[sex_acronym]]
  coeff_mat
}

#' Linearly interpolate in LMS/MSNT coefficient tables
#' @param x_to_interp X values at which to get interpolated values of the
#' coefficients named in `coeff_names`.
#' @param coeff_mat A matrix of coefficients in which to interpolate, with the
#' names found in `coeff_names`.
#' @param coeff_names The names of the coefficients to be interpolated.
#' @return Matrix containing interpolated coefficients, with the values of
#' `coeff_names` as column names.
#' @importFrom stats approx
#' @keywords internal
#' @noRd
interpolate_coeffs <- function(x_to_interp, coeff_mat, coeff_names) {
  interpolated <- vapply(
    X = coeff_names,
    FUN.VALUE = numeric(length = length(x_to_interp)),
    FUN = \(coeff_name) {
      stats::approx(x = coeff_mat[, 1],
                    y = coeff_mat[, coeff_name],
                    xout = x_to_interp)$y
    })
  if (!is.matrix(interpolated)) interpolated <- t(interpolated)
  interpolated
}