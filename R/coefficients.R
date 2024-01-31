#' Retrieve coefficients for coefficient-based standards
#'
#' @description Retrieves growth standard coefficient values where needed for
#'  the WHO Child Growth and INTERGROWTH-21<sup>st</sup> Newborn Size standards.
#' @param x X variable in whatever range and units are appropriate for the
#'  growth standard(s) in use.
#' @param sex Character vector of sex(es), either `"M"` (male) or `"F"`
#'   (female).
#' @param acronym Acronym(s) denoting which coefficient-based growth standards
#'  are in use.
#' @param coeff_tbls A list with LMS or MSNT coefficient tables, either
#'   `gigs::who_gs_coeffs` or `gigs::ig_nbs_coeffs`.
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
retrieve_coefficients <- function(x, sex, acronym, coeff_tbls, coeff_names) {
  acronyms <- unique(acronym)
  acronyms <- acronyms[!is.na(acronyms)]

  # 2. Load coeff tables as needed
  li_coeffs <- lapply(X = acronyms, FUN = \(acronym) coeff_tbls[[acronym]]) |>
    setNames(acronyms)

  # Initialise empty list which will contain vectors with each coeff
  len_x <- length(x)
  empty_vec <- rep(NA, len_x)
  out_li <- vector(mode = "list", length = length(coeff_names)) |>
        setNames(coeff_names)
  for (i in seq_along(coeff_names)) out_li[[i]] <- empty_vec

  # Iterate through acronym-sex combinations, and reassign values in out_li
  # based on outputs from stats::approx(). For loops used to avoid the
  # environment/assignment constraints of nested apply() calls
  for (chr_acronym in names(li_coeffs)) {
    li_sexes <- li_coeffs[[chr_acronym]]
    is_curr_acro <- chr_acronym == acronym
    xvars <- li_sexes[[1]][[1]]
    for (chr_sex in names(li_sexes)) {
      curr_sex <- if (chr_sex == "male") "M" else "F"
      is_curr_sex <- curr_sex == sex
      tbl_coeffs <- li_sexes[[chr_sex]]
      is_curr_sex_acro <- is_curr_acro & is_curr_sex
      is_curr_sex_acro[is.na(is_curr_sex_acro)] <- FALSE
      for (coeff_name in coeff_names) {
        coeff <- stats::approx(x = xvars,
                               y = tbl_coeffs[[coeff_name]],
                               xout = x,
                               rule = 1)[["y"]]
        out_li[[coeff_name]][is_curr_sex_acro] <- coeff[is_curr_sex_acro]
      }
    }
  }
  out_li
}