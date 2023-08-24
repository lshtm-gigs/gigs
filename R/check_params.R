check_sex_acronym <- function(sex, acronym, allowed_acronyms) {
  acronym[which(!acronym %in% allowed_acronyms)] <- NA_character_
  sex[which(!sex %in% c("M", "F", "U"))] <- NA_character_
  list(sex = sex, acronym = acronym)
}

check_who_params <- function(sex, acronym) {
  check_sex_acronym(sex = sex,
                    acronym = acronym,
                    allowed_acronyms = names(gigs::who_gs))
}

check_nbs_params <- function(gest_age, sex, acronym) {
  sex_acro <- check_sex_acronym(sex = sex,
                                acronym = acronym,
                                allowed_acronyms = names(gigs::ig_nbs))
  # The bfpfga, fmfga, and ffmfga standards have a smaller range of gestational
  # ages than the other INTERGROWTH-21st standards --> this code specify which
  # range of GAs to compare with 'gest_age'
  has_lower_xrange <- acronym %in% names(gigs::ig_nbs)[5:7]
  small_xrange <- gigs::ig_nbs$bfpfga$male$percentiles$gest_age
  full_xrange <- gigs::ig_nbs$wfga$male$zscores$gest_age
  valid_x <- ifelse(has_lower_xrange,
                    yes = inrange(gest_age, small_xrange),
                    no = inrange(gest_age, full_xrange))
  gest_age[!valid_x] <- NA_real_
  list(age = gest_age, sex = sex_acro$sex, acronym = sex_acro$acronym)
}

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
  valid_x <- ifelse(uses_pma,
                    yes = x %in% pma_range,
                    no = inrange(x, len_range))
  x[!valid_x] <- NA_real_
  list(x = x, sex = sex_acro$sex, acronym = sex_acro$acronym)
}


#' Check if x is within upper and lower bounds of a vector, ignoring NA
#' @keywords internal
#' @noRd
inrange <- function(x, vec) {
  x >= min(vec, na.rm = TRUE) & x <= max(vec, na.rm = TRUE)
}