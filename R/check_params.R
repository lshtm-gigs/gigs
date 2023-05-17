check_params <- function(sex, acronym, allowed_acronyms, standard, xvars = NULL, xvar_range = NULL) {
  acronym[which(!acronym %in% allowed_acronyms)] <- NA_character_
  sex[which(!sex %in% c("M", "F", "U"))] <- NA_character_
  if (standard == "who_gs") {
   return(list(sex = sex, acronym = acronym))
  }
  bools_standard <- rep_len(standard == "ig_nbs", length(acronym))
  bools_acronyms <- !acronym %in% names(gigs::ig_nbs)[4:7]
  uses_coeffs <- bools_standard & bools_acronyms
  invalid_x <- ifelse(uses_coeffs,
                      yes = !inrange(xvars, xvar_range),
                      no = !xvars %in% xvar_range)
  xvars[invalid_x] <- NA_real_
  list(age = xvars, sex = sex, acronym = acronym)
}

check_who_params <- function(sex, acronym) {
  check_params(sex = sex,
               acronym = acronym,
               allowed_acronyms = names(gigs::who_gs),
               standard = "who_gs")
}

check_nbs_params <- function(gest_age, sex, acronym) {
  check_params(sex = sex,
               acronym = acronym,
               allowed_acronyms = names(gigs::ig_nbs),
               standard = "ig_nbs",
               xvars = gest_age,
               xvar_range = gigs::ig_nbs$wfga$male$zscores$gest_age)
}

check_png_params <- function(pma_weeks, sex, acronym) {
  check_params(sex = sex,
               acronym = acronym,
               allowed_acronyms = names(gigs::ig_png),
               standard = "ig_png",
               xvars = pma_weeks,
               xvar_range = gigs::ig_png$wfa$male$zscores$pma_weeks)
}

#' Check if x is within upper and lower bounds of a vector
#' @keywords internal
inrange <- function(x, vec) x >= min(vec) & x <= max(vec)