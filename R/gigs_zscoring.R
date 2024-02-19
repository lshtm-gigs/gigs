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
#' @note These functions expect vectors which have lengths such that they can
#'   be recycled with [vctrs::vec_recycle_common()].
#' @rdname gigs_waz
#' @noRd
gigs_waz <- function(weight_kg, age_days, gest_days, sex) {
  validated <- validate_waz_params(weight_kg = weight_kg,
                                   age_days = age_days,
                                   gest_days = gest_days,
                                   sex = sex)
  weight_kg <- validated[[1]]
  age_days <- validated[[2]]
  gest_days <- validated[[3]]
  sex <- validated[[4]]

  # Set up PMA
  pma_days <- gest_days + age_days
  pma_weeks <- pma_days / 7
  pma_weeks[!inrange(x = pma_weeks, vec = c(27, 64))] <- NA

  gigs_lgls <- gigs_xaz_lgls(gest_days = gest_days, age_days = age_days) |>
    lapply(FUN = \(lgl) lgl & !is.na(weight_kg))

  z_ig_nbs <- fn_on_subset(ig_nbs_wfga_value2zscore, gigs_lgls[["ig_nbs"]],
                           weight_kg, gest_days, sex)
  z_ig_png <- fn_on_subset(ig_png_wfa_value2zscore, gigs_lgls[["ig_png"]],
                           weight_kg, pma_weeks, sex)
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
gigs_laz <- function(lenht_cm, age_days, gest_days, sex) {
  validated <- validate_laz_params(lenht_cm = lenht_cm,
                                   age_days = age_days,
                                   gest_days = gest_days,
                                   sex = sex)
  lenht_cm <- validated[[1]]
  age_days <- validated[[2]]
  gest_days <- validated[[3]]
  sex <- validated[[4]]

  # Set up PMA
  pma_days <- gest_days + age_days
  pma_weeks <- pma_days / 7
  pma_weeks[!inrange(x = pma_weeks, vec = c(27, 64))] <- NA

  gigs_lgls <- gigs_xaz_lgls(gest_days = gest_days, age_days = age_days) |>
    lapply(FUN = \(lgl) lgl & !is.na(lenht_cm))

  z_ig_nbs <- fn_on_subset(ig_nbs_lfga_value2zscore, gigs_lgls[["ig_nbs"]],
                           lenht_cm, gest_days, sex)
  z_ig_png <- fn_on_subset(ig_png_lfa_value2zscore, gigs_lgls[["ig_png"]],
                           lenht_cm, pma_weeks, sex)
  z_who_gs <- fn_on_subset(who_gs_lhfa_value2zscore, gigs_lgls[["who_gs"]],
                           lenht_cm, age_days, sex)

  z_out <- rep(NA_real_, length(lenht_cm))
  z_out[gigs_lgls[["ig_nbs"]]] <- z_ig_nbs
  z_out[gigs_lgls[["ig_png"]]] <- z_ig_png
  z_out[gigs_lgls[["who_gs"]]] <- z_who_gs
  z_out
}

#' @rdname gigs_waz
#' @param headcirc_cm Numeric vector of length one or more with head
#'   circumferences in cm.
#' @noRd
gigs_hcaz <- function(headcirc_cm, age_days, gest_days, sex) {
  validated <- validate_hcaz_params(headcirc_cm = headcirc_cm,
                                    age_days = age_days,
                                    gest_days = gest_days,
                                    sex = sex)
  headcirc_cm <- validated[[1]]
  age_days <- validated[[2]]
  gest_days <- validated[[3]]
  sex <- validated[[4]]

  # Set up PMA
  pma_days <- gest_days + age_days
  pma_weeks <- pma_days / 7
  pma_weeks[!inrange(x = pma_weeks, vec = c(27, 64))] <- NA

  gigs_lgls <- gigs_xaz_lgls(gest_days = gest_days, age_days = age_days) |>
    lapply(FUN = \(lgl) lgl & !is.na(headcirc_cm))

  z_ig_nbs <- fn_on_subset(ig_nbs_hcfga_value2zscore, gigs_lgls[["ig_nbs"]],
                           headcirc_cm, gest_days, sex)
  z_ig_png <- fn_on_subset(ig_png_hcfa_value2zscore, gigs_lgls[["ig_png"]],
                           headcirc_cm, pma_weeks, sex)
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
gigs_wlz <- function(weight_kg, lenht_cm, age_days, gest_days, sex) {
  validated <- validate_wlz_params(weight_kg = weight_kg, lenht_cm = lenht_cm,
                                   age_days = age_days, gest_days = gest_days,
                                   sex = sex)
  weight_kg <- validated[[1]]
  lenht_cm <- validated[[2]]
  age_days <- validated[[3]]
  gest_days <- validated[[4]]
  sex <- validated[[5]]

  # Set up PMA
  pma_days <- gest_days + age_days
  pma_weeks <- pma_days / 7
  pma_weeks[!inrange(x = pma_weeks, vec = c(27, 64))] <- NA

  gigs_lgls <- gigs_xaz_lgls(gest_days = gest_days, age_days = age_days) |>
    lapply(FUN = \(lgl) lgl & !(is.na(weight_kg) | is.na(lenht_cm)))

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
#' @srrstats {G3.0} Using `abs() < sqrt(.Machine$double.eps)` for floating point
#'   equality.
#' @noRd
gigs_xaz_lgls <- function(age_days, gest_days) {
  # Set up vars for use later
  term_cutoff_days <- 37 * 7 # i.e. 37 weeks = term baby
  is_term <- gest_days >= term_cutoff_days
  pma_days <- gest_days + age_days
  pma_weeks <- pma_days / 7

  use_ig_nbs <- abs(age_days) < sqrt(.Machine$double.eps)
  use_ig_png <- age_days > 0 & !is_term & inrange(pma_weeks, c(27, 64))
  use_who_gs <- age_days > 0 & is_term | (!is_term & pma_weeks > 64)

  # Prevents `NAs are not allowed in subscripted assignments` error
  use_ig_nbs[is.na(use_ig_nbs)] <- FALSE
  use_ig_png[is.na(use_ig_png)] <- FALSE
  use_who_gs[is.na(use_who_gs)] <- FALSE
  list(ig_nbs = use_ig_nbs, ig_png = use_ig_png, who_gs = use_who_gs)
}

#' Run a function over a subset of inputs
#' @param fn Function to run over `in1`, `in2`, `in3`.
#' @param lgl Logical vector of length one or more indicating which indices of
#'   `in1` `in2` and `in3` should be operated on `by `fn`.
#' @param in1,in2,in3 Three vectors of the same length as `lgl` which are
#'   used as inputs to `fn`.
#' @return Returns a vector of the type outputted by `fn`, with length dependent
#'   on what `fn` outputs.
#' @noRd
fn_on_subset <- function(fn, lgl, in1, in2, in3) {
  if (any(lgl)) fn(in1[lgl], in2[lgl], in3[lgl]) else double(length = 0L)
}

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.4a} This file's functions are all documented with `{roxygen2}`.
NULL
