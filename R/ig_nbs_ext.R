# INTERNAL: Extended IG-21st Newborn Size standards conversion logic -----------

#' Convert centiles to values in the extended INTERGROWTH-21<sup>st</sup>
#' Newborn Size standards
#' @inherit zscore2value params return
#' @note This function will fail if `length(acronym) != 1`.
#' @noRd
ig_nbs_ext_c2v_internal <- function(p, x, sex, acronym) {
  gest_days <- x
  vpns_lim <- 231
  y <- ifelse(
    test = gest_days >= vpns_lim,
    yes = ig_nbs_msnt_p2v(p, gest_days, sex,
                          gigs::ig_nbs_ext_coeffs[[acronym]]),
    no = ig_vpns_zscore2value(z = qnorm(p), gest_days, sex, acronym)
  )
  y
}

#' Convert values to centiles in the extended INTERGROWTH-21<sup>st</sup>
#' Newborn Size standards
#' @inherit zscore2value params return
#' @note This function will fail if `length(acronym) != 1`.
#' @noRd
ig_nbs_ext_v2c_internal <- function(y, x, sex, acronym) {
  gest_days <- x
  vpns_lim <- 231
  p <- ifelse(
    test = gest_days >= vpns_lim,
    yes = ig_nbs_msnt_v2p(y, gest_days, sex,
                          gigs::ig_nbs_ext_coeffs[[acronym]]),
    no = pnorm(ig_vpns_value2zscore(y, gest_days, sex, acronym))
  )
  p
}

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.4, G1.4a} All functions in file documented using `{roxygen2}`.
#' @srrstats {G2.0, G2.1, G2.2, G2.3, G2.3a, G2.6} All internal functions in
#'   this script are provided with vectors that have already been validated.
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.16} All internal functions in
#'   this script are provided with vectors that have already checked for
#'   missing/undefined/out-of-bounds data.
NULL