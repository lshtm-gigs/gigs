#' Classify size for gestational age using the INTERGROWTH-21<sup>st</sup>
#' weight-for-gestational age standard
#'
#' Size for gestational age categories are split by centile: small-for-GA
#' (SGA; 10<sup>th</sup> centile), appropriate-for-GA (AGA; 10<sup>th</sup>
#' to 90<sup>th</sup> centile) and large-for-GA (LGA; >90<sup>th</sup>
#' centile). This function also supports classification of severe SGA
#' (<3<sup>rd</sup> centile) using the `severe` parameter.
#'
#' @inheritParams shared_roxygen_params
#' @param weight_kg Numeric vector with weight value(s) in kg.
#' @param gest_days Numeric vector with gestational age(s) at birth in days.
#'   Values not between `168` and `300` will be set to `NA`.
#' @param severe A single logical value specifying whether to categorise SGA
#'   values are below the third centile as `"SGA(<3)"`. Default = `FALSE`.
#' @return Factor with gestational age classification(s). If `severe = FALSE`,
#'   levels are `c("SGA", "AGA",  "LGA")`. If `severe = TRUE`, levels are
#'   `c("SGA(<3)", "SGA", "AGA",  "LGA")`.
#' @note Input vectors will be recycled by [vctrs::vec_recycle_common()].
#' @examples
#' # Without severe flag, does not differentiate between
#' # p < 0.03 and p < 0.10
#' classify_sga(
#'   weight_kg = c(2.2, 3.4, 4.2),
#'   gest_days = 267,
#'   sex = "F"
#' )
#'
#' # With severe = TRUE, highlights p < 0.03
#' classify_sga(
#'   weight_kg = c(2.2, 3.4, 4.2),
#'   gest_days = 267,
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
classify_sga <- function(weight_kg, gest_days, sex, severe = FALSE) {
  checkmate::qassert(severe, rules = "B1")
  centiles <- ig_nbs_wfga_value2centile(weight_kg = weight_kg,
                                        sex = sex,
                                        gest_days = gest_days)
  sga <- rep(NA_character_, length(centiles))
  sga[centiles < 0.1] <- "SGA"
  sga[centiles >= 0.1 & centiles <= 0.9] <- "AGA"
  sga[centiles > 0.9] <- "LGA"
  sga_lvls <- c("SGA", "AGA", "LGA")
  if (severe) {
    sga[centiles < 0.03] <- "SGA(<3)"
    sga_lvls <- c("SGA(<3)", sga_lvls)
  }
  factor(sga, levels = sga_lvls)
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
#'   `c("Preterm SGA", "Preterm AGA", "Preterm SGA", "Term SGA", "Term AGA",
#'   "Term SGA")`.
#' @examples
#' classify_svn(
#'   weight_kg = c(1.5, 2.6, 2.6, 3.5),
#'   gest_days = c(235, 257, 275, 295),
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
classify_svn <- function(weight_kg, gest_days, sex) {
  is_term <- gest_days >= 37 * 7
  sga <- classify_sga(weight_kg = weight_kg,
                      sex = sex,
                      gest_days = gest_days,
                      severe = FALSE)
  sga_cats <- levels(sga)
  levels <- c(paste("Preterm", sga_cats), paste("Term", sga_cats))
  svn <- character(length = length(sga))
  svn[!is_term & sga == sga_cats[1]] <- levels[1]
  svn[!is_term & sga == sga_cats[2]] <- levels[2]
  svn[!is_term & sga == sga_cats[3]] <- levels[3]
  svn[is_term & sga == sga_cats[1]] <- levels[4]
  svn[is_term & sga == sga_cats[2]] <- levels[5]
  svn[is_term & sga == sga_cats[3]] <- levels[6]
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
#' @param gest_days Numeric vector with gestational age(s) at birth in days.
#' @param outliers A single `TRUE` or `FALSE` value specifying whether
#'   implausible z-score value thresholds should be applied. Default = `FALSE`.
#' @return Factor of stunting classification(s) with levels
#'   `c("stunting_severe", "stunting", "normal")` or `c("stunting_severe",
#'   "stunting", "normal", "outlier")`, depending on whether the `outliers`
#'   argument is set to `TRUE`.
#' @note Input vectors will be recycled by [vctrs::vec_recycle_common()]. The
#'   function assumes that your measurements were taken according to the WHO
#'   guidelines, which stipulate that recumbent length should not be measured
#'   after 730 days. Implausible z-score bounds are sourced from the referenced
#'   WHO report, and classification cut-offs from the DHS manual.
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
#'   gest_days = c(196, 287, 266),
#'   sex = c("M", "M", "F")
#' )
#'
#' # And with outlier flagging:
#' classify_stunting(
#'   lenht_cm = c(52.2, 75.4, 63.1),
#'   age_days = c(357, 375, 250),
#'   gest_days = c(196, 287, 266),
#'   sex = c("M", "M", "F"),
#'   outliers = TRUE
#' )
#' @export
classify_stunting <- function(lenht_cm, age_days, gest_days, sex,
                              outliers = FALSE) {
  checkmate::qassert(outliers, rules = "B1")
  params <- vctrs::vec_recycle_common(lenht_cm, age_days, gest_days, sex)
  z_scores <- gigs_laz(lenht_cm = params[[1]], age_days = params[[2]],
                       gest_days = params[[3]], sex = params[[4]])
  stunting <- rep(NA_character_, length(z_scores))
  stunting[z_scores <= -2] <- "stunting"
  stunting[z_scores <= -3] <- "stunting_severe"
  stunting[z_scores > -2] <- "normal"
  stunting_lvls <- c("stunting_severe", "stunting", "normal")
  if (outliers) {
    stunting[abs(z_scores) > 6] <- "outlier"
    stunting_lvls <- c(stunting_lvls, "outlier")
  }
  factor(stunting, levels = stunting_lvls)
}

#' Classify wasting using INTERGROWTH-21<sup>st</sup> weight-for-length or WHO
#' weight-for-length/height standards
#'
#' Classify wasting (low weight-for-length/height) using the
#' INTERGROWTH-21<sup>st</sup> weight-for-length or WHO Child Growth standards,
#' specifically either the weight-for-length or weight-for-height standard
#' depending on the age of the child. Severe wasting is <-3SD relative to the
#' median expected weight, whereas moderate wasting is -2SD from the median.
#'
#' @inheritParams classify_sga
#' @inheritParams classify_stunting
#' @return Factor of wasting classifications with same length as longest input.
#'   If `outliers = FALSE`, levels are `c("wasting_severe", "wasting", "normal",
#'   "overweight")`. If `outliers = TRUE`, levels are `c("wasting_severe",
#'   "wasting", "normal", "overweight", "outlier")`.
#' @note Input vectors will be recycled by [vctrs::vec_recycle_common()].
#'   Implausible z-score bounds are sourced from the referenced WHO report, and
#'   classification cut-offs from the DHS manual.
#' @inherit classify_stunting references
#' @examples
#' # Returns factor with stunting classifications
#' classify_wasting(
#'   weight_kg = c(5.75, 2.18, 3.00, 6.75),
#'   lenht_cm = c(67.7, 46.6, 50.0, 80.1),
#'   gest_days = c(251, 197, 225, 243),
#'   age_days = c(251, 197, 225, 243),
#'   sex =  c("F", "M", "F", "M")
#' )
#'
#' # Outliers  can be flagged if `outliers` set to TRUE
#' classify_wasting(
#'   weight_kg = c(5.75, 2.18, 3.00, 6.75),
#'   lenht_cm = c(67.7, 46.6, 50.0, 80.1),
#'   gest_days = c(251, 197, 225, 243),
#'   age_days = c(251, 197, 225, 243),
#'   sex =  c("F", "M", "F", "M"),
#'   outliers = TRUE
#' )
#' @export
classify_wasting <- function(weight_kg, lenht_cm, gest_days, age_days, sex,
                             outliers = FALSE) {
  checkmate::qassert(outliers, rules = "B1")
  params <- vctrs::vec_recycle_common(weight_kg, lenht_cm, age_days,
                                      gest_days, sex)
  z_scores <- gigs_wlz(weight_kg = params[[1]], lenht_cm = params[[2]],
                       age_days = params[[3]], gest_days = params[[4]],
                       sex = params[[5]])
  wasting <- character(length = length(z_scores))
  wasting[z_scores <= -2] <- "wasting"
  wasting[z_scores <= -3] <- "wasting_severe"
  wasting[abs(z_scores) < 2] <- "normal"
  wasting[z_scores >= 2] <- "overweight"
  wasting_lvls <- c("wasting_severe", "wasting", "normal", "overweight")
  if (outliers) {
    wasting[abs(z_scores) > 5] <- "outlier"
    wasting_lvls <- c(wasting_lvls, "outlier")
  }
  factor(wasting, levels = wasting_lvls)
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
#' @return Factor of weight-for-age classifications with same length as longest
#'   input. If `outliers = FALSE`, levels are `c("underweight_severe",
#'   "underweight", "normal", "overweight")`. If `outliers = TRUE`, levels are
#'   `c("underweight_severe", "underweight", "normal", "overweight",
#'   "outlier")`.
#' @inherit classify_wasting note
#' @examples
#' classify_wfa(
#'   weight_kg = c(7.2, 4.5, 9.1, 24),
#'   age_days = c(401, 185, 101, 607),
#'   gest_days = 7 * c(27, 36, 40, 41),
#'   sex = c("F", "M", "F", "M")
#' )
#'
#' classify_wfa(
#'   weight_kg = c(7.2, 4.5, 9.1, 24),
#'   age_days = c(401, 185, 101, 607),
#'   gest_days = 7 * c(27, 36, 40, 41),
#'   sex = c("F", "M", "F", "M"),
#'   outliers = TRUE
#' )
#' @export
classify_wfa <- function(weight_kg, age_days, gest_days, sex, outliers = FALSE) {
  checkmate::qassert(outliers, rules = "B1")
  params <- vctrs::vec_recycle_common(weight_kg, age_days, gest_days, sex)
  z_scores <- gigs_waz(weight_kg = params[[1]], age_days = params[[2]],
                       gest_days = params[[3]], sex = params[[4]])
  wfa <- character(length(z_scores))
  wfa[z_scores <= -2] <- "underweight"
  wfa[z_scores <= -3] <- "underweight_severe"
  wfa[abs(z_scores) < 2] <- "normal"
  wfa[z_scores >= 2] <- "overweight"
  wfa_lvls <- c("underweight_severe", "underweight", "normal", "overweight")
  if (outliers) {
    wfa[z_scores < -6 | z_scores > 5] <- "outlier"
    wfa_lvls <- c(wfa_lvls, "outlier")
  }
  factor(wfa, levels = wfa_lvls)
}