# #  - title: GIGS z-scoring procedures
# #    desc: >
# #      Z-scoring functions in which growth standards are applied according to
# #      GIGS guidance.
# #    contents:
# #      - gigs_waz
# #      - gigs_laz
# #      - gigs_hcaz
#
# #' Calculate z-scores for anthropometric measures according to GIGS guidance
# #'
# #' @description These functions calculate z-scores for weight/length/head
# #' circumference-for-age, in which the growth standards available in `gigs` are
# #' applied according to the guidance and advice from GIGS.
# #' @note
# #' The z-scoring procedures differ based on the GA of the infant:
# #' * `gest_age` < 27 weeks:
# #'   - IG-21<sup>st</sup> NBS at birth
# #'   - IG-21<sup>st</sup> PNG from 27 to 64 weeks' PMA
# #'   - GA-corrected WHO standard from 64 weeks' PMA to 2 years old
# #'   - Uncorrected WHO standard after 2 years old
# #' * 27 ≤ `gest_age` < 37 weeks:
# #'   - IG-21<sup>st</sup> PNG from birth to 64 weeks' PMA
# #'   - GA-corrected WHO standard from 64 weeks' PMA to 2 years old
# #'   - Uncorrected WHO standard after 2 years old
# #' * `gest_age` ≥ 37 weeks:
# #'   - IG-21<sup>st</sup> NBS at birth
# #'   - Uncorrected WHO standard after birth
# #' @rdname gigs_waz
# #' @export
# gigs_waz <- function(weight_kg, gest_age, age_days, sex) {
#   gigs_xaz(x = weight_kg,
#            gest_age = gest_age,
#            age_days = age_days,
#            sex = sex,
#            fn_nbs = ig_nbs_wfga_zscore2value,
#            fn_png = ig_png_wfa_zscore2value,
#            fn_who = who_gs_wfa_zscore2value)
# }
#
# #' @rdname gigs_waz
# #' @export
# gigs_laz <- function(length_cm, gest_age, age_days, sex) {
#   gigs_xaz(x = length_cm,
#            gest_age = gest_age,
#            age_days = age_days,
#            sex = sex,
#            fn_nbs = ig_nbs_lfga_zscore2value,
#            fn_png = ig_png_lfa_zscore2value,
#            fn_who = who_gs_lfa_zscore2value)
# }
#
# #' @rdname gigs_waz
# #' @export
# gigs_hcaz <- function(headcirc_cm, gest_age, age_days, sex) {
#    gigs_xaz(x = headcirc_cm,
#             gest_age = gest_age,
#             age_days = age_days,
#             sex = sex,
#             fn_nbs = ig_nbs_hcfga_zscore2value,
#             fn_png = ig_png_hcfa_zscore2value,
#             fn_who = who_gs_hcfa_zscore2value)
# }
#
# # #' @rdname gigs_waz
# # #' @export
# # gigs_wlz <- function(weight_kg, length_cm, gest_age, age_days, sex) {
# #   # Set up vars for use later
# #   thirtyseven_wks <- 37 * 7
# #   forty_wks <- 40 * 7
# #   two_years <- 730.5
# #   preterm <- gest_age < thirtyseven_wks
# #   pma <- gest_age + age_days
# #   pma_wks <- round(pma)
# #   age_corr <- pma - forty_wks
# #
# #   # Return z-scores where GIGS guidance conditions met
# #   dplyr::case_when(
# #     preterm & age_days == 0 & gest_age < 27 * 7 ~ fn_nbs(x, gest_age, sex),
# #     preterm & age_days == 0 & gest_age >= 27 * 7 ~ fn_png(x, pma_wks, sex),
# #     preterm & age_days > 0 & pma_wks < 65 ~ fn_png(x, pma_wks, sex),
# #     preterm & age_days > 0 & pma_wks >= 65 & age_days < two_years ~
# #       fn_who(x, age_corr, sex),
# #     preterm & age_days > 0 & pma_wks >= 65 & age_days >= two_years ~
# #       fn_who(x, age_days, sex),
# #     term & age_days == 0 ~ fn_nbs(x, gest_age, sex),
# #     term & age_days > 0 ~ fn_who(x, age_days, sex)
# #   )
# # }
#
# #' Apply the internal GIGS cut-off logic for the `gigs_xaz()` functions
# #'
# #' @description Applies PMA/gestational age cutoffs for term and preterm infants
# #' for generating GIGS-compliant WAZs, LAZs, and HCAZs.
# #' @rdname gigs_xaz
# #' @importFrom dplyr case_when
# #' @keywords internal
# #' @noRd
# gigs_xaz <- function(x, gest_age, age_days, sex, fn_nbs, fn_png, fn_who) {
#   # Set up vars for use later
#   thirtyseven_wks <- 37 * 7
#   forty_wks <- 40 * 7
#   two_years <- 730.5
#   preterm <- gest_age < thirtyseven_wks
#   pma <- gest_age + age_days
#   pma_wks <- round(pma)
#   age_corr <- pma - forty_wks
#
#   # Return z-scores where GIGS guidance conditions met
#   dplyr::case_when(
#     preterm & age_days == 0 & gest_age < 27 * 7 ~ fn_nbs(x, gest_age, sex),
#     preterm & age_days == 0 & gest_age >= 27 * 7 ~ fn_png(x, pma_wks, sex),
#     preterm & age_days > 0 & pma_wks < 65 ~ fn_png(x, pma_wks, sex),
#     preterm & age_days > 0 & pma_wks >= 65 & age_days < two_years ~
#       fn_who(x, age_corr, sex),
#     preterm & age_days > 0 & pma_wks >= 65 & age_days >= two_years ~
#       fn_who(x, age_days, sex),
#     term & age_days == 0 ~ fn_nbs(x, gest_age, sex),
#     term & age_days > 0 ~ fn_who(x, age_days, sex)
#   )
# }