#' Classify size for gestational age using the INTERGROWTH-21<sup>st</sup>
#' weight-for-gestational age standard
#'
#' Size for gestational age categories are split by percentile: small-for-GA
#' (SGA; 10<sup>th</sup> percentile), appropriate-for-GA (AGA; 10<sup>th</sup>
#' to 90<sup>th</sup> percentile) and large-for-GA (LGA; >90<sup>th</sup>
#' percentile). This function also supports classification of severe SGA
#' (<3<sup>rd</sup> percentile) using the `severe` parameter.
#'
#' @inheritParams shared_roxygen_params
#' @param weight_kg Numeric vector with weight value(s) in kg.
#' @param gest_age Numeric vector with gestational age(s) at birth in days.
#'   Values not between `168` and `300` will be set to `NA`.
#' @param severe If `TRUE`, specify which SGA values are below the third
#'   percentile. Default = `FALSE`.
#' @return Factor with gestational age classification(s). If `severe = FALSE`,
#'   levels are `c("SGA", "AGA",  "LGA")`. If `severe = TRUE`, levels are `
#'   c("SGA(<3)", "SGA", "AGA",  "LGA")`.
#' @note Input vectors will be recycled by [vctrs::vec_recycle_common()].
#' @examples
#' # Without severe flag, does not differentiate between
#' # p < 0.03 and p < 0.10
#' classify_sga(
#'   weight_kg = c(2.2, 3.4, 4.2),
#'   gest_age = 267,
#'   sex = "F"
#' )
#'
#' # With severe = TRUE, highlights p < 0.03
#' classify_sga(
#'   weight_kg = c(2.2, 3.4, 4.2),
#'   gest_age = 267,
#'   sex = "F",
#'   severe = TRUE
#' )
#' @references
#' WHO. **Physical status: the use and interpretation of anthropometry. Report
#' of a WHO Expert Committee.** *World Health Organisation Technical Report
#' Series 1995,* **854: 1–452**
#'
#' Royal College of Obstetricians and Gynaecologists. **The Investigation and
#' Management of the Small-for-Gestational-Age Fetus: Green-top Guideline No.
#' 31.** *Technical report, Royal College of Obstetricians and Gynaecologists,
#' London, 2013.*
#' @export
classify_sga <- function(weight_kg, gest_age, sex, severe = FALSE) {
  percentiles <- ig_nbs_wfga_value2percentile(weight_kg = weight_kg,
                                              sex = sex,
                                              gest_age = gest_age)
  sga <- rep(NA_character_, length(percentiles))
  sga[which(percentiles < 0.1)] <- "SGA"
  sga[which(percentiles >= 0.1 & percentiles <= 0.9)] <- "AGA"
  sga[which(percentiles > 0.9)] <- "LGA"
  if (severe) {
    sga[which(percentiles < 0.03)] <- "SGA(<3)"
  }
  levels <- c("SGA", "AGA", "LGA")
  levels <- if (severe) c("SGA(<3)", levels) else levels
  factor(sga, levels = levels)
}

#' Classify small vulnerable newborns according to the
#' INTERGROWTH-21<sup>st</sup> weight-for-gestational age standard
#'
#' A small vulnerable newborn (SVN) can be preterm (born too soon),
#' small-for-gestational age (born too small), or within both of these
#' categories. This function categorises newborn weights into SVN categories for
#' use in downstream analyses.
#'
#' @inherit classify_sga params note
#' @return Factor with small vulnerable newborn classification(s), with levels
#'   `c("Term non-SGA", "Term SGA", "Preterm non-SGA", "Preterm SGA")`.
#' @examples
#' classify_svn(
#'   weight_kg = c(1.5, 2.6, 2.6, 3.5),
#'   gest_age = c(235, 257, 275, 295),
#'   sex = c("F", "M", "F", "M")
#' )
#' @references
#' Lawn JE, Ohuma EO, Bradley E, Idueta LS, Hazel E, Okwaraji YB et al.
#' **Small babies, big risks: global estimates of prevalence and mortality for
#' vulnerable newborns to accelerate change and improve counting.** *The Lancet*
#' 2023, *401(10389):1707-1719.* \doi{10.1016/S0140-6736(23)00522-6}
#' @seealso [classify_sga()], which this function uses to classify newborns as
#'   small-for-gestational age.
#' @export
classify_svn <- function(weight_kg, gest_age, sex) {
  is_term <- ifelse(gest_age >= 37 * 7, yes = TRUE, no = FALSE)
  sga <- classify_sga(weight_kg = weight_kg,
                      sex = sex,
                      gest_age = gest_age,
                      severe = FALSE)
  is_sga <- sga == "SGA"
  levels <- c("Term non-SGA", "Term SGA", "Preterm non-SGA", "Preterm SGA")
  svn <- character(length = length(sga))
  svn[is_term & is_sga] <- "Term SGA"
  svn[is_term & !is_sga] <- "Term non-SGA"
  svn[!is_term & is_sga] <- "Preterm SGA"
  svn[!is_term & !is_sga] <-  "Preterm non-SGA"
  factor(x = svn, levels = levels)
}

#' Classify stunting using WHO or INTERGROWTH-21<sup>st</sup>
#' length/height-for-age standards
#'
#' Classify stunting (low length/height-for-age) using WHO or
#' INTERGROWTH-21<sup>st</sup> length/height-for-age standards depending on the
#' gestational age at birth of the child. Severe stunting is below <-3 SD
#' relative to median length/height at a given age, whereas moderate stunting is
#' -2SD from the median.
#'
#' @inheritParams classify_sga
#' @param lenht_cm Numeric vector with length/height measurement(s) in cm.
#' @param age_days Numeric vector with age(s) in days for each child. Should be
#'   between `0` to `1856` days.
#' @param ga_at_birth Numeric vector with gestational age(s) at birth in days.
#' @param lenht_method Character vector containig `"H"` or `"L"` value(s)
#'   describing whether `lenht_cm` was recorded as recumbent length or standing
#'   height. Missing (`NA`) values will be set to `"L"` for children <731 days
#'   old and to `"H"` for children 731 days old or more. Default = `NA`.
#' @return Factor of stunting classification(s) with levels `c("implausible",
#'   "stunting_severe", "stunting", "normal")`.
#' @note Input vectors will be recycled by [vctrs::vec_recycle_common()]. WHO
#'   guidelines stipulate that recumbent length should not be measured after 730
#'   days. Therefore recumbent length values for children over 730 days old have
#'   0.7 cm taken away, and height values for children less than 731 days old
#'   have 0.7 cm added on. Implausible z-score bounds are sourced from the
#'   referenced WHO report, and classification cut-offs from the DHS manual.
#' @references
#' **'Implausible z-score values'** *in* World Health Organization (ed.)
#' *Recommendations for data collection, analysis and reporting on
#' anthropometric indicators in children under 5 years old*. Geneva: World
#' Health Organization and the United Nations Children's Fund UNICEF, (2019).
#' pp. 64-65.
#'
#' **'Percentage of children stunted, wasted, and underweight, and mean z-scores
#' for stunting, wasting and underweight'** *in* *Guide to DHS Statistics DHS-7*
#' Rockville, Maryland, USA: ICF (2020). pp. 431-435.
#' <https://dhsprogram.com/data/Guide-to-DHS-Statistics/Nutritional_Status.htm>
#' @examples
#' # The first observation uses the INTERGROWTH-21st Postnatal Growth standards;
#' # the next two use the WHO Child Growth Standards.
#' classify_stunting(
#'   lenht_cm = c(52.2, 75.4, 63.1),
#'   age_days = c(357, 375, 250),
#'   ga_at_birth = c(196, 287, 266),
#'   sex = c("M", "M", "F"),
#'   lenht_method = c("H", "H", "H")
#' )
#' @export
classify_stunting <- function(lenht_cm, age_days, ga_at_birth, sex, lenht_method = NA) {
  lgth_lenht_method <- length(lenht_method)
  lgth_age_days <- length(age_days)
  if (lgth_lenht_method != 1 & lgth_lenht_method != lgth_age_days) {
    stop(paste0("lenht_method should be as long as the input vectors or length",
                " 1. Your input was length ", lgth_lenht_method, "."),
         call. = FALSE)
  }
  if (lgth_lenht_method != lgth_age_days) {
    lenht_method <- vctrs::vec_recycle_common(age_days, lenht_method)[[2]]
  }
  lenht_method <- ifelse(is.na(lenht_method),
                         yes = ifelse(age_days < 731, yes = "L", no = "H"),
                         no = lenht_method)
  lenht_cm2 <- ifelse(age_days >= 731 & tolower(lenht_method) == "l",
                      yes = lenht_cm - 0.7,
                      no = lenht_cm)
  lenht_cm2 <- ifelse(age_days < 731 & tolower(lenht_method) == "h",
                      yes = lenht_cm + 0.7,
                      no = lenht_cm2)
  pma_weeks <- round((age_days + ga_at_birth) / 7)
  ga_in_ig_png_range <- ga_at_birth >= 182 & ga_at_birth < 259
  z_scores <- ifelse(
    pma_weeks %in% gigs::ig_png$wfa$male$zscores$pma_weeks & ga_in_ig_png_range,
    yes = ig_png_lfa_value2zscore(length_cm = lenht_cm2, pma_weeks = pma_weeks,
                                  sex = sex),
    no = who_gs_lhfa_value2zscore(lenht_cm = lenht_cm2, age_days = age_days,
                                  sex = sex)
  )
  stunting <- rep(NA_character_, length(z_scores))
  stunting[which(z_scores <= -2)] <- "stunting"
  stunting[which(z_scores <= -3)] <- "stunting_severe"
  stunting[which(z_scores < -6)] <- "implausible"
  stunting[which(z_scores > -2)] <- "normal"
  stunting[which(z_scores > 6)] <- "implausible"
  factor(
    stunting, levels = c("implausible", "stunting_severe", "stunting", "normal")
  )
}

#' Classify wasting using WHO weight-for-length/height standards
#'
#' Classify wasting (low weight-for-length/height) using the WHO Child Growth
#' Standards, specifically either the weight-for-length or weight-for-height
#' standard depending on the age of the child. Severe wasting is <-3SD relative
#' to the median expected weight, whereas moderate wasting is -2SD from the
#' median.
#'
#' @inheritParams classify_sga
#' @inheritParams classify_stunting
#' @return Factor of wasting classification(s) with levels `c("implausible",
#'   "wasting_severe", "wasting", "normal", "overweight")`.
#' @note Input vectors will be recycled by [vctrs::vec_recycle_common()].
#'   Implausible z-score bounds are sourced from the referenced WHO report, and
#'   classification cut-offs from the DHS manual. Observations with invalid or
#'   missing `lenht_method` values will be returned as `NA`.
#' @inherit classify_stunting references
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
           yes = who_gs_wfh_value2zscore(weight_kg = weight_kg,
                                         height_cm = lenht_cm, sex = sex),
           no = who_gs_wfl_value2zscore(weight_kg = weight_kg,
                                        length_cm = lenht_cm, sex = sex))
  )
  wasting <- rep(NA_character_, length(z_scores))
  wasting[which(z_scores <= -2)] <- "wasting"
  wasting[which(z_scores <= -3)] <- "wasting_severe"
  wasting[which(abs(z_scores) < 2)] <- "normal"
  wasting[which(z_scores >= 2)] <- "overweight"
  wasting[which(abs(z_scores) > 5)] <- "implausible"
  factor(wasting,
         levels = c("implausible", "wasting_severe", "wasting", "normal",
                    "overweight"))
}

#' Classify weight-for-age using WHO or INTERGROWTH-21<sup>st</sup> standards
#'
#' Classify weight-for-age z-scores using the WHO Child Growth Standards or
#' INTERGROWTH-21<sup>st</sup> Postnatal Growth standards depending on the
#' gestational age at birth for the infant. Severely underweight is less than 3
#' SD below the median, underweight is less than 2 SD below the median, and
#' overweight is > 2 SDs above the median.
#'
#' @inheritParams classify_sga
#' @inherit classify_stunting params references
#' @return Factor of weight classification(s) with levels `c("implausible",
#'   "underweight_severe", "underweight", "normal", "overweight")`.
#' @note Input vectors will be recycled by [vctrs::vec_recycle_common()].
#'   Implausible z-score bounds are sourced from the referenced WHO report, and
#'   classification cut-offs from the DHS manual.
#' @examples
#' classify_wfa(
#'   weight_kg = c(7.2, 4.5, 9.1, 24),
#'   age_days = c(401, 185, 101, 607),
#'   ga_at_birth = 7 * c(27, 36, 40, 41),
#'   sex = c("F", "M", "F", "M")
#' )
#' @export
classify_wfa <- function(weight_kg, age_days, ga_at_birth, sex) {
  pma_weeks <- round((age_days + ga_at_birth) / 7)
  pma_in_ig_png_range <- pma_weeks %in% gigs::ig_png$wfa$male$zscores$pma_weeks
  # INTERGROWTH-21st PNG standards designed for babies born from 26 to before 37
  # weeks' GA
  ga_in_ig_png_range <- ga_at_birth >= 26 * 7 & ga_at_birth < 64 * 7
  z_scores <- ifelse(
    test = pma_in_ig_png_range & ga_in_ig_png_range,
    yes = ig_png_wfa_value2zscore(weight_kg = weight_kg, pma_weeks = pma_weeks,
                                  sex = sex),
    no = who_gs_wfa_value2zscore(weight_kg = weight_kg, age_days = age_days,
                                 sex = sex)
  )
  wfa <- rep(NA_character_, length(z_scores))
  wfa[which(z_scores <= -2)] <- "underweight"
  wfa[which(z_scores <= -3)] <- "underweight_severe"
  wfa[which(z_scores < -6)] <- "implausible"
  wfa[which(abs(z_scores) < 2)] <- "normal"
  wfa[which(z_scores >= 2)] <- "overweight"
  wfa[which(z_scores > 5)] <- "implausible"
  factor(wfa, levels = c("implausible", "underweight_severe", "underweight",
                         "normal", "overweight"))
}