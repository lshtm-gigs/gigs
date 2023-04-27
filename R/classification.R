#' Classify size for gestational age using INTERGROWTH-21st Newborn Size Standards (including very preterm)
#'
#' Size for gestational age categories are split by centile: small-for-GA (SGA), appropriate-for-GA (AGA) and
#' large-for-GA (LGA). SGA is <10th centile, LGA is >90th centile, and AGA is in between. Some prefer to note children
#' below the third centile as severely SGA.
#'
#' @param weight_kg Anthropometric measurement(s) to assess.
#' @param gest_age Gestational age(s) at birth in weeks. Must be between `24` and `42 + 6/7`.
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param coarse If `FALSE`, specify which SGA values are below the third percentile. Default = `TRUE`.
#' @returns Factor with gestational age classification(s). If `coarse = TRUE`, levels are `c("SGA", "AGA",  "LGA")`.
#' If `coarse = FALSE`, levels are `c("SGA(<3)", "SGA", "AGA",  "LGA")`.
#'
#' @examples
#' # Without coarse flag, does not differentiate between p < 0.03 and p < 0.10
#' classify_sga(
#'   y = c(2.2, 3.4, 4.2),
#'   gest_age = 38 + 1/7,
#'   sex = "F"
#' )
#'
#' # With coarse = FALSE, highlights p < 0.03
#' classify_sga(
#'   y = c(2.2, 3.4, 4.2),
#'   gest_age = 38 + 1/7,
#'   sex = "F",
#'   coarse = FALSE
#' )
#' @export
classify_sga <- function(weight_kg, gest_age, sex, coarse = TRUE) {
  percentiles <- ig_nbs_wfga_value2percentile(weight_kg = weight_kg, sex = sex, gest_age = gest_age)
  out <- rep(NA_character_, length(percentiles))
  out[which(percentiles <= 0.1)] <- "SGA"
  out[which(percentiles > 0.1 & percentiles < 0.9)] <- "AGA"
  out[which(percentiles >= 0.9)] <- "LGA"
  if (!coarse) {
    out[which(percentiles < 0.03)] <- "SGA(<3)"
  }
  levels <- if (coarse) c("SGA", "AGA",  "LGA") else c("SGA(<3)", "SGA", "AGA",  "LGA")
  factor(out, levels = levels)
}

#' Classify stunting according to WHO or INTERGROWTH-21st length/height-for-age standards
#'
#' Classify stunting (low height-for-age) using INTERGROWTH-21st or WHO Growth Standards depending on the gestational
#' age at birth of the infant. Severe stunting is less than 3 SD relative to growth standards, whereas moderate stunting
#' is -2SD from the median.
#'
#' @param lenht_cm Length/height measurement(s) in cm.
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param age_days Age(s) in days for each child.
#' @param ga_at_birth Gestational age(s) at birth in weeks.
#' @param lenht_method `"H"` or `"L"` value(s) describing whether lenht_cm was recorded as recumbent length or
#' standing height.
#' @returns Factor of stunting classification(s) with levels `c("implausible", "stunting_severe", "stunting",
#' "normal")`.
#'
#' @note WHO guidelines stipulate that recumbent length should not be measured after 730 days. Therefore recumbent
#' length values for children over 730 days old have 0.7 cm taken away, and height values for children less than 731
#' days old have 0.7 cm added on. Implausible z-scores are sourced from the referenced WHO report, and classification
#' criteria from the DHS manual.
#' @references
#' **'Implausible z-score values'** *in* World Health Organization (ed.)
#' *Recommendations for data collection, analysis and reporting on anthropometric indicators in children under 5
#' years old*. Geneva: World Health Organization and the United Nations Children's Fund UNICEF, (2019). pp. 64-65.
#'
#' **'Percentage of children stunted, wasted, and underweight, and mean z-scores for stunting, wasting and
#' underweight'** *in* *Guide to DHS Statistics DHS-7* Rockville, Maryland, USA: ICF (2020). pp. 431-435.
#' <https://dhsprogram.com/data/Guide-to-DHS-Statistics/Nutritional_Status.htm>
#'
#' @examples
#' # The first observation uses the INTERGROWTH-21st post-natal growth standards; the next two
#' # use the WHO Growth Standards.
#' classify_stunting(
#'   lenht_cm = c(52.2, 75.4, 63.1),
#'   age_days = c(357, 375, 250),
#'   ga_at_birth = c(28, 41, 38),
#'   sex = c("M", "M", "F"),
#'   lenht_method = c("H", "H", "H")
#' )
#' @export
classify_stunting <- function(lenht_cm, age_days, ga_at_birth, sex, lenht_method = NA) {
  lgth_lenht_method <- length(lenht_method)
  lgth_age_days <- length(age_days)
  if (lgth_lenht_method != 1 & lgth_lenht_method != lgth_age_days) {
    stop(paste0("lenht_method should be as long as the input vectors or length 1. Your input was length",
                lgth_lenht_method), call. = F)
  }
  if (lgth_lenht_method != lgth_age_days) lenht_method <- rep_to_longest(list(age_days, lenht_method))[[2]]
  lenht_method <- ifelse(is.na(lenht_method), yes = ifelse(age_days < 731, yes = "L", no = "H"), no = lenht_method)
  lenht_cm2 <- ifelse(age_days >= 731 & tolower(lenht_method) == "l",
                      yes = lenht_cm - 0.7,
                      no = lenht_cm)
  lenht_cm2 <- ifelse(age_days < 731 & tolower(lenht_method) == "h",
                      yes = lenht_cm + 0.7,
                      no = lenht_cm2)
  pma_weeks <- round((age_days + ga_at_birth * 7) / 7)
  z_scores <- ifelse(
    pma_weeks %in% gigs::ig_png$wfa$male$zscores$pma_weeks & ga_at_birth >= 26 & ga_at_birth < 37,
    yes = ig_png_lfa_value2zscore(length_cm = lenht_cm2, pma_weeks = pma_weeks, sex = sex),
    no = who_gs_lhfa_value2zscore(lenht_cm = lenht_cm2, age_days = age_days, sex = sex)
  )
  out <- rep(NA_character_, length(z_scores))
  out[which(z_scores <= -2)] <- "stunting"
  out[which(z_scores <= -3)] <- "stunting_severe"
  out[which(z_scores < -6)] <- "implausible"
  out[which(z_scores > -2)] <- "normal"
  out[which(z_scores > 6)] <- "implausible"
  factor(out, levels = c("implausible", "stunting_severe", "stunting", "normal"))
}

#' Classify wasting according to WHO weight-for-length/height standards
#'
#' Classify wasting (low weight-for-height) using INTERGROWTH or WHO Growth Standards depending on the gestational age
#' at birth for the infant. Severe wasting is less than 3 SD relative to growth standards, whereas moderate wasting is
#' -2SD from the median.
#'
#' @param weight_kg Weight measurement(s) in kg.
#' @param lenht_cm Length/height measurement(s) in cm.
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param lenht_method `"H"` or `"L"` value(s) describing whether lenht_cm was recorded as recumbent length or
#' standing height.
#' @returns Factor of wasting classification(s) with levels `c("implausible", "wasting_severe", "wasting", "normal",
#' "overweight")`.
#'
#'
#' @note Implausible z-score bounds are sourced from the referenced WHO report, and classification criteria from the DHS
#' manual.
#' @references
#' **'Implausible z-score values'** *in* World Health Organization (ed.)
#' *Recommendations for data collection, analysis and reporting on anthropometric indicators in children under 5
#' years old*. Geneva: World Health Organization and the United Nations Children’s Fund UNICEF, (2019). pp. 64-65.
#'
#' **'Percentage of children stunted, wasted, and underweight, and mean z-scores for stunting, wasting and
#' underweight'** *in* *Guide to DHS Statistics DHS-7* Rockville, Maryland, USA: ICF (2020). pp. 431-435.
#' <https://dhsprogram.com/data/Guide-to-DHS-Statistics/Nutritional_Status.htm>
#'
#' @examples
#' # Where no lenht_method is given, classify_wasting() returns NA.
#' classify_wasting(
#'   weight_kg = c(5.75, 2.18, 3.00, 6.75),
#'   lenht_cm = c(67.7, 46.6, 50.0, 80.1),
#'   sex =  c("F", "M", "F", "M"),
#'   lenht_method = c("H", "L", NA_character_, "H")
#' )
#' @export
classify_wasting <- function(weight_kg, lenht_cm, sex, lenht_method) {
  weight_kg[which(!lenht_method %in% c("H", "L"))] <- NA_real_
  z_scores <- suppressWarnings(
    ifelse(tolower(lenht_method) == "h",
           yes = who_gs_wfh_value2zscore(weight_kg = weight_kg, height_cm = lenht_cm, sex = sex),
           no = who_gs_wfl_value2zscore(weight_kg = weight_kg, length_cm = lenht_cm, sex = sex))
  )
  out <- rep(NA_character_, length(z_scores))
  out[which(z_scores <= -2)] <- "wasting"
  out[which(z_scores <= -3)] <- "wasting_severe"
  out[which(abs(z_scores) < 2)] <- "normal"
  out[which(z_scores >= 2)] <- "overweight"
  out[which(abs(z_scores) > 5)] <- "implausible"
  factor(out, levels = c("implausible", "wasting_severe", "wasting", "normal", "overweight"))
}

#' Classify weight-for-age according to INTERGROWTH-21st and WHO post-natal growth standards
#'
#' Classify weight-for-ages using INTERGROWTH-21st data or WHO Growth Standards depending on the gestational age
#' at birth for the infant. Severely underweight is less than 3 SD below the median, underweight is less than 2 SD below
#' the median, and overweight is > 2 SDs above the median.
#'
#' @param weight_kg Weight measurement(s) in kg.
#' @param age_days Age(s) at recording of each infant in days.
#' @param ga_at_birth Gestational age(s) at birth in weeks.
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @returns Factor of weight classification(s) with levels `c("implausible", "underweight_severe", "underweight",
#' "normal", "overweight")`.
#'
#' @note Implausible z-score bounds are sourced from the referenced WHO report, and classification criteria from the DHS
#' manual.
#' @references
#' **'Implausible z-score values'** *in* World Health Organization (ed.)
#' *Recommendations for data collection, analysis and reporting on anthropometric indicators in children under 5
#' years old*. Geneva: World Health Organization and the United Nations Children’s Fund UNICEF, (2019). pp. 64-65.
#'
#' **'Percentage of children stunted, wasted, and underweight, and mean z-scores for stunting, wasting and
#' underweight'** *in* *Guide to DHS Statistics DHS-7* Rockville, Maryland, USA: ICF (2020). pp. 431-435.
#' <https://dhsprogram.com/data/Guide-to-DHS-Statistics/Nutritional_Status.htm>
#'
#' @examples
#' classify_wfa(
#'   weight_kg = c(7.2, 4.5, 9.1, 24),
#'   age_days = c(501, 435, 201, 707),
#'   ga_at_birth = c(27, 36, 40, 41),
#'   sex = c("F", "M", "F", "M")
#' )
#' @export
classify_wfa <- function(weight_kg, age_days, ga_at_birth, sex) {
  pma_weeks <- round((age_days + ga_at_birth * 7) / 7)
  z_scores <- ifelse(
    test = pma_weeks %in% gigs::ig_png$wfa$male$zscores$pma_weeks & ga_at_birth >= 26 & ga_at_birth < 37,
    yes = ig_png_wfa_value2zscore(weight_kg = weight_kg, pma_weeks = pma_weeks, sex = sex),
    no = who_gs_wfa_value2zscore(weight_kg = weight_kg, age_days = age_days, sex = sex)
  )
  out <- rep(NA_character_, length(z_scores))
  out[which(z_scores <= -2)] <- "underweight"
  out[which(z_scores <= -3)] <- "underweight_severe"
  out[which(z_scores < -6)] <- "implausible"
  out[which(abs(z_scores) < 2)] <- "normal"
  out[which(z_scores >= 2)] <- "overweight"
  out[which(z_scores > 5)] <- "implausible"
  factor(out, levels = c("implausible", "underweight_severe", "underweight", "normal", "overweight"))
}