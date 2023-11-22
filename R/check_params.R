#' Check whether user-inputted sex and acronym values are valid
#'
#' @param sex Character vector with user-inputted sex values.
#' @param acronym Character vector with user-inputted acronym values.
#' @param allowed_acronyms Character vector with acceptable values for
#'   `acronym`. Members of `acronym` which are not in `allowed_acronyms` will be
#'   set to `NA`.
#' @returns List with names `"sex"` and `"acronym"`, which contains vectors
#'   where invalid `sex` or `acronym` values have been replaced with `NA`.
#' @noRd
check_sex_acronym <- function(sex, acronym, allowed_acronyms) {
  sex <- stop_if_wrong_type(sex, "character")
  acronym <- stop_if_wrong_type(acronym, "character")
  acronym[which(!acronym %in% allowed_acronyms)] <- NA_character_
  sex[which(!sex %in% c("M", "F", "U"))] <- NA_character_
  list(sex = sex, acronym = acronym)
}

#' Check whether user-inputted `sex` and `acronym` values are valid in `who_gs`
#' functions
#'
#' @param x Numeric vector with user-inputted x values.
#' @param sex Character vector with user-inputted sex values.
#' @param acronym Character vector with user-inputted acronym values.
#' @returns List with names `"sex"` and `"acronym"` contain vectors where
#'   invalid sex or acronym values have been replaced with NA. If any of `x`,
#'   `sex`, or `acronym` are the wrong type, `check_who_params()` will throw an
#'   error.
#' @noRd
check_who_params <- function(x, sex, acronym) {
  stop_if_wrong_type(x, "numeric")
  check_sex_acronym(sex = sex,
                    acronym = acronym,
                    allowed_acronyms = names(gigs::who_gs))
}

#' Check whether user-inputted `sex` and `acronym` values are valid in `ig_nbs`
#' functions
#'
#' @param gest_days Numeric vector with user-inputted gestational age values.
#' @param sex Character vector with user-inputted sex values.
#' @param acronym Character vector with user-inputted acronym values.
#' @returns List with names `"age"`, `"sex"`, and `"acronym"`, containing
#'   vectors where invalid sex or acronym values have been replaced with NA. If
#'   any of `x`, `sex`, or `acronym` are the wrong type, `check_nbs_params()`
#'   will throw an error.
#' @noRd
check_nbs_params <- function(gest_days, sex, acronym) {
  sex_acro <- check_sex_acronym(sex = sex,
                                acronym = acronym,
                                allowed_acronyms = names(gigs::ig_nbs))
  # The bfpfga, fmfga, and ffmfga standards have a smaller range of gestational
  # ages than the other INTERGROWTH-21st standards --> this code specifies which
  # range of GAs to compare with 'gest_age'
  is_bodycomp <- acronym %in% names(gigs::ig_nbs)[5:7]
  bodycomp_xrange <- gigs::ig_nbs$bfpfga$male$centiles$gest_days
  full_xrange <- gigs::ig_nbs$wfga$male$zscores$gest_days
  stop_if_wrong_type(gest_days, "numeric")
  valid_x <- ifelse(is_bodycomp,
                    yes = inrange(gest_days, bodycomp_xrange),
                    no = inrange(gest_days, full_xrange))
  gest_days[!valid_x] <- NA_real_
  list(age = gest_days, sex = sex_acro$sex, acronym = sex_acro$acronym)
}

#' Check whether user-inputted `sex` and `acronym` values are valid in `ig_png`
#' functions
#'
#' @param x Numeric vector with user-inputted x values.
#' @param sex Character vector with user-inputted sex values.
#' @param acronym Character vector with user-inputted acronym values.
#' @returns List with names `"x"`, `"sex"`, and `"acronym"`, containing
#'   vectors where invalid sex or acronym values have been replaced with NA. If
#'   any of `x`, `sex`, or `acronym` are the wrong type, `check_png_params()`
#'   will throw an error.
#' @noRd
check_png_params <- function(x, sex, acronym) {
  sex_acro <- check_sex_acronym(sex = sex,
                                acronym = acronym,
                                allowed_acronyms = names(gigs::ig_png))
  # The wfa, lfa and hcfa standards use post-menstrual age, whereas the wfl
  # standard uses length in cm --> this code specifies whether to compare x to
  # PMA or length in cm
  uses_pma <- acronym %in% names(gigs::ig_png)[1:3]
  pma_range <- gigs::ig_png$wfa$male$zscores$pma_weeks
  len_range <- gigs::ig_png$wfl$male$zscores$length_cm
  stop_if_wrong_type(x, "numeric")
  valid_x <- ifelse(uses_pma,
                    yes = x %in% pma_range,
                    no = inrange(x, len_range))
  x[!valid_x] <- NA_real_
  list(x = x, sex = sex_acro$sex, acronym = sex_acro$acronym)
}
