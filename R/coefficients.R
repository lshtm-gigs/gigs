#' Retrieve coefficients for coefficient-based standards
#'
#' @description Retrieves growth standard coefficient values where needed for
#'  the WHO Child Growth and INTERGROWTH-21<sup>st</sup> Newborn Size standards.
#' @param x X variable in whatever range and units are appropriate for the
#'  growth standard in use.
#' @param sex Character vector of sex(es), either `"M"` (male) or `"F"`
#'   (female).
#' @param acronym A single character variable denoting which coefficient-based
#'   growth standard is in use.
#' @param coeff_tbls A with LMS or MSNT coefficient tables for the growth
#'   standard specified by `acronym`, taken from  `gigs::who_gs_coeffs` or
#'   `gigs::ig_nbs_coeffs`.
#' @param coeff_names Character vector denoting the names of coefficients in
#'  `coeff_tbls`.
#' @note The coefficient tables provided by the WHO and INTERGROWTH-21st project
#'   have values for discrete values of their respective x variable (e.g.
#'   gestational age in days for the INTERGROWTH-21st NBS standards, age in days
#'   for some of the WHO standards, etc.). We therefore retrieve coefficients
#'   using `approx()`, as linear interpolation between known LMS/MSNT
#'   coefficients is more accurate than simply rounding to the nearest known
#'   coefficient (Kiger and Taylor, 2015 - \doi{10.1007/s10916-015-0389-x}).
#' @return A list containing coefficients where available for each observation.
#'  This list will have the same number of elements as `coeff_names`, and will
#'  be named according to the values in `coeff_names`.
#' @srrstats {G1.4a} This file's function is documented with `{roxygen2}`.
#' @rdname retrieve_coefficients
#' @importFrom stats setNames
#' @noRd
retrieve_coefficients <- function(x, sex, coeff_tbls, coeff_names) {
  # Initialise empty list which will contain vectors with each coeff
  len_x <- length(x)
  empty_vec <- rep(NA_real_, len_x)
  out_li <- vector(mode = "list", length = length(coeff_names)) |>
        rlang::set_names(coeff_names)
  for (i in seq_along(coeff_names)) out_li[[i]] <- empty_vec

  # Iterate through sexes, and reassign values in out_li based on outputs from
  # stats::approx(). For loops used to avoid the environment/assignment
  # constraints of nested *apply() calls
  xvars <- coeff_tbls[[1]][[1]]
  is_na_sex <- is.na(sex)
  for (chr_sex in names(coeff_tbls)) {
    curr_sex <- if (chr_sex == "male") "M" else "F"
    tbl_coeffs <- coeff_tbls[[chr_sex]]
    is_curr_sex <- curr_sex == sex
    is_curr_sex[is_na_sex] <- FALSE
    for (idx in seq_along(coeff_names)) {
      out_li[[idx]][is_curr_sex] <- stats::approx(x = xvars,
                                                  y = tbl_coeffs[[idx + 1]],
                                                  xout = x[is_curr_sex],
                                                  rule = 1)[["y"]]
    }
  }
  out_li
}