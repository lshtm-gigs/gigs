#  - title: GIGS z-scoring procedures
#    desc: >
#      Z-scoring functions in which growth standards are applied according to
#      GIGS guidance.
#    contents:
#      - gigs_waz
#      - gigs_laz
#      - gigs_hcaz

#' Calculate z-scores for anthropometric measures according to GIGS guidance
#'
#' @description These functions calculate z-scores for either weight-for-age
#'   (WAZs), length-for-age (LAZs), head circumference-for-age (HCAZs), or
#'   weight-for-length (WLZs).
#'
#'   The z-scoring procedure used differs for each observation based on the
#'   gestational and post-menstrual age of the infants being analysed. The
#'   different procedures are based on the advised use of each growth standard:
#'   * `gest_days` < 24 weeks:
#'     - Birth: No standard available
#'     - Postnatal: IG-21<sup>st</sup> Postnatal Growth standards from 27 to 64
#'       weeks' PMA
#'     - Postnatal: Uncorrected WHO standard after 64 weeks' PMA
#'   * `gest_days` >= 24 weeks:
#'     - Birth: IG-21<sup>st</sup> Newborn Size standards at birth (including
#'       Very Preterm Newborn Size standards if born before 33 weeks).
#'     - Postnatal: IG-21<sup>st</sup> Postnatal Growth standards from 27 to 64
#'       weeks' PMA
#'     - Postnatal: Uncorrected WHO Child Growth standards after 64 weeks' PMA
#'   * `gest_days` ≥ 37 weeks:
#'     - Birth: IG-21<sup>st</sup> Newborn Size standards
#'     - Postnatal: Uncorrected WHO Child Growth standards
#'   * `gest_days` ≥ 43 weeks:
#'     - Birth: No standard available
#'     - Postnatal: Uncorrected WHO Child Growth standards
#' @note This function expects vectors of the same length, and will fail if
#'   these are not provided.
#' @rdname gigs_waz
#' @noRd
gigs_waz <- function(weight_kg, gest_days, age_days, sex) {
  stop_if_lengths_unequal(weight_kg, gest_days, age_days, sex)
  # Set up PMA
  pma_days <- gest_days + age_days
  pma_weeks <- pma_days / 7
  pma_weeks[!inrange(x = pma_weeks, vec = c(27, 64))] <- NA

  gigs_lgls <- gigs_xaz_lgls(gest_days = gest_days, age_days = age_days)

  z_ig_nbs <- fn_on_subset(ig_nbs_wfga_value2zscore, gigs_lgls[["ig_nbs"]],
                           weight_kg, gest_days, sex)
  z_ig_png <- fn_on_subset(ig_png_wfa_value2zscore, gigs_lgls[["ig_png"]],
                           weight_kg, floor(pma_weeks), sex)
  z_who_gs <- fn_on_subset(who_gs_wfa_value2zscore, gigs_lgls[["who_gs"]],
                           weight_kg, age_days, sex)

  z_out <- rep(NA_real_, length(weight_kg))
  z_out[gigs_lgls[["ig_nbs"]]] <- z_ig_nbs
  z_out[gigs_lgls[["ig_png"]]] <- z_ig_png
  z_out[gigs_lgls[["who_gs"]]] <- z_who_gs
  z_out
}

#' @rdname gigs_waz
#' @param lenht_cm Numeric vector with lengths/heights in cm. It is assumed that
#'   all `lenht_cm` values where `age_days` < 731 are recumbent length
#'   measurements, and all `lenht_cm` values where `age_days` >= 731 are
#'   standing height measurements.
#' @noRd
gigs_laz <- function(lenht_cm, gest_days, age_days, sex) {
  stop_if_lengths_unequal(lenht_cm, gest_days, age_days, sex)
  # Set up PMA
  pma_days <- gest_days + age_days
  pma_weeks <- pma_days / 7
  pma_weeks[!inrange(x = pma_weeks, vec = c(27, 64))] <- NA

  gigs_lgls <- gigs_xaz_lgls(gest_days = gest_days, age_days = age_days)

  z_ig_nbs <- fn_on_subset(ig_nbs_lfga_value2zscore, gigs_lgls[["ig_nbs"]],
                           lenht_cm, gest_days, sex)
  z_ig_png <- fn_on_subset(ig_png_lfa_value2zscore, gigs_lgls[["ig_png"]],
                           lenht_cm, floor(pma_weeks), sex)
  z_who_gs <- fn_on_subset(who_gs_lhfa_value2zscore, gigs_lgls[["who_gs"]],
                           lenht_cm, age_days, sex)

  z_out <- rep(NA_real_, length(lenht_cm))
  z_out[gigs_lgls[["ig_nbs"]]] <- z_ig_nbs
  z_out[gigs_lgls[["ig_png"]]] <- z_ig_png
  z_out[gigs_lgls[["who_gs"]]] <- z_who_gs
  z_out
}

#' @rdname gigs_waz
#' @param headcirc_cm Numeric vector with head circumferences in cm.
#' @noRd
gigs_hcaz <- function(headcirc_cm, gest_days, age_days, sex) {
  stop_if_lengths_unequal(headcirc_cm, gest_days, age_days, sex)
  # Set up PMA
  pma_days <- gest_days + age_days
  pma_weeks <- pma_days / 7
  pma_weeks[!inrange(x = pma_weeks, vec = c(27, 64))] <- NA

  gigs_lgls <- gigs_xaz_lgls(gest_days = gest_days, age_days = age_days)

  z_ig_nbs <- fn_on_subset(ig_nbs_hcfga_value2zscore, gigs_lgls[["ig_nbs"]],
                           headcirc_cm, gest_days, sex)
  z_ig_png <- fn_on_subset(ig_png_hcfa_value2zscore, gigs_lgls[["ig_png"]],
                           headcirc_cm, floor(pma_weeks), sex)
  z_who_gs <- fn_on_subset(who_gs_hcfa_value2zscore, gigs_lgls[["who_gs"]],
                           headcirc_cm, age_days, sex)

  z_out <- rep(NA_real_, length(headcirc_cm))
  z_out[gigs_lgls[["ig_nbs"]]] <- z_ig_nbs
  z_out[gigs_lgls[["ig_png"]]] <- z_ig_png
  z_out[gigs_lgls[["who_gs"]]] <- z_who_gs
  z_out
}

#' @rdname gigs_waz
#' @noRd
gigs_wlz <- function(weight_kg, lenht_cm, gest_days, age_days, sex) {
  stop_if_lengths_unequal(weight_kg, lenht_cm, gest_days, age_days, sex)

  # Set up PMA
  pma_days <- gest_days + age_days
  pma_weeks <- pma_days / 7
  pma_weeks[!inrange(x = pma_weeks, vec = c(27, 64))] <- NA

  gigs_lgls <- gigs_xaz_lgls(gest_days = gest_days, age_days = age_days)
  use_who_wfl <- gigs_lgls[["who_gs"]] & age_days < 731
  use_who_wfh <- gigs_lgls[["who_gs"]] & age_days >= 731

  z_ig_png <- fn_on_subset(ig_png_wfl_value2zscore, gigs_lgls[["ig_png"]],
                           weight_kg, lenht_cm, sex)
  z_who_gs_wfl <- fn_on_subset(who_gs_wfl_value2zscore, use_who_wfl,
                               weight_kg, lenht_cm, sex)
  z_who_gs_wfh <- fn_on_subset(who_gs_wfh_value2zscore, use_who_wfh,
                               weight_kg, lenht_cm, sex)

  z_out <- rep(NA_real_, length(weight_kg))
  z_out[gigs_lgls[["ig_png"]]] <- z_ig_png
  z_out[use_who_wfl] <- z_who_gs_wfl
  z_out[use_who_wfh] <- z_who_gs_wfh
  z_out
}

#' Get logical vectors describing which standards to apply for an observation
#' using GIGS cut-off logic
#'
#' @description Applies gestational age/PMA cutoffs for term and preterm infants
#'   to generate logical vectors describing where to apply GIGS-compliant
#'   growth standards.
#' @param gest_days Numeric vector with gestational age(s) at birth in days.
#' @param age_days Numeric vector with age(s) in days.
#' @note The logical vectors returned by this function will be `FALSE` if either
#'   `gest_days` or `age_days` is NA.
#' @returns Named list with three logical vectors `ig_nbs`, `ig_png`, and
#'   `who_gs`. Where these logical vectors are `TRUE`, then the growth standard
#'   from that named list should be used.
#' @rdname gigs_xaz
#' @noRd
gigs_xaz_lgls <- function(gest_days, age_days) {
  # Set up vars for use later
  term_cutoff_days <- 37 * 7 # i.e. 37 weeks = term baby
  is_term <- gest_days >= term_cutoff_days
  pma_days <- gest_days + age_days
  pma_weeks <- pma_days / 7

  use_ig_nbs <- age_days == 0
  use_ig_png <- age_days > 0 & !is_term & is_valid_pma_weeks(pma_weeks)
  use_who_gs <- age_days > 0 & is_term | (!is_term & pma_weeks > 64)

  # Prevents `NAs are not allowed in subscripted assignments` error
  use_ig_nbs[is.na(use_ig_nbs)] <- FALSE
  use_ig_png[is.na(use_ig_png)] <- FALSE
  use_who_gs[is.na(use_who_gs)] <- FALSE
  list(ig_nbs = use_ig_nbs, ig_png = use_ig_png, who_gs = use_who_gs)
}

#' Run a function over a subset of inputs
#' @param fn Function to run over `in1`, `in2`, `in3`.
#' @param lgl Logical vector indicating which indices of `in1` `in2` and `in3`
#'   to operate on.
#' @param in1,in2,in3 Three vectors of the same length as `lgl` which are
#'   used as inputs to the function specified by `fn`.
#' @return Returns a vector the same length as `sum(lgl)`.
#' @noRd
fn_on_subset <- function(fn, lgl, in1, in2, in3) {
  fn(in1[lgl], in2[lgl], in3[lgl])
}
