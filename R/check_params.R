check_params <- function(sex, acronym, allowed_acronyms, age = NULL, age_range = NA) {
  acronym[which(!acronym %in% allowed_acronyms)] <- NA_character_
  sex[which(!sex %in% c("M", "F", "U"))] <- NA_character_
  if (!any(is.null(c(age, age_range)))) {
    age[!age %in% age_range] <- NA_real_
    return(list(age = age, sex = sex, acronym = acronym))
  }
  return(list(sex = sex, acronym = acronym))
}

check_who_params <- function(sex, acronym) {
  check_params(sex = sex,
               acronym = acronym,
               allowed_acronyms = names(gigs::who_gs))
}

check_nbs_params <- function(gest_age, sex, acronym) {
  check_params(sex = sex,
               acronym = acronym,
               allowed_acronyms = names(gigs::ig_nbs),
               age = gest_age,
               age_range = gigs::ig_nbs$wfga$male$zscores$gest_age)
}

check_png_params <- function(pma_weeks, sex, acronym) {
  check_params(sex = sex,
               acronym = acronym,
               allowed_acronyms = names(gigs::ig_png),
               age = pma_weeks,
               age_range = gigs::ig_png$wfa$male$zscores$pma_weeks)
}