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
#' @param weight_kg Numeric vector of length one or more with weight
#'   value(s) in kg.
#' @param gest_days Numeric vector of length one or more with gestational
#'   age(s) at birth in days between `168` and `300`. By default, gigs will
#'   replace out-of-bounds elements in `gest_days` with `NA` and warn you. This
#'   behaviour can be customised using the functions in [gigs_options].
#' @param severe A single logical value specifying whether to categorise SGA
#'   values are below the third centile as `"SGA(<3)"`. Default = `FALSE`.
#' @returns An object of class factor with the same length as the longest input
#'   vector, containing size-for-GA classifications. If `severe = FALSE`, levels
#'   are `c("SGA", "AGA", "LGA")`. If `severe = TRUE`, levels are `c("SGA(<3)",
#'   "SGA", "AGA", "LGA")`.
#' @note Input vectors are recycled by [vctrs::vec_recycle_common()], and must
#'   adhere to the [vctrs] recycling rules.
#' @seealso [ig_nbs_wfga_value2centile()], which this function calls to get
#'   centiles for each observation.
#' @examples
#' # By default, does not differentiate between p < 0.03 and p < 0.10
#' classify_sfga(
#'   weight_kg = c(2.2, 3.3, 4.2),
#'   gest_days = 267:269,
#'   sex = c("M", "F", "M")
#' )
#'
#' # With severe = TRUE, highlights p < 0.03
#' classify_sfga(
#'   weight_kg = c(2.2, 3.3, 4.2),
#'   gest_days = 267:269,
#'   sex = c("M", "F", "M"),
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
classify_sfga <- function(weight_kg, gest_days, sex, severe = FALSE) {
  checkmate::qassert(severe, rules = "B1")
  # ig_nbs_wfga_value2centile will do checks on weight_kg/gest_days/sex
  centiles <- ig_nbs_wfga_value2centile(weight_kg = weight_kg,
                                        gest_days = gest_days,
                                        sex = sex)
  sfga <- rep(NA_character_, length(centiles))
  sfga[centiles < 0.1] <- "SGA"
  sfga[centiles >= 0.1 & centiles <= 0.9] <- "AGA"
  sfga[centiles > 0.9] <- "LGA"
  sfga_lvls <- c("SGA", "AGA", "LGA")
  if (severe) {
    sfga[centiles < 0.03] <- "SGA(<3)"
    sfga_lvls <- c("SGA(<3)", sfga_lvls)
  }
  factor(sfga, levels = sfga_lvls)
}

#' Classify small vulnerable newborns according to the
#' INTERGROWTH-21<sup>st</sup> weight-for-gestational age standard
#'
#' A small vulnerable newborn (SVN) can be preterm (born too soon),
#' small-for-gestational age (born too small), or within both of these
#' categories. This function uses weight and gestational age to categorise
#' newborns according to their SVN type, for use in downstream analyses.
#'
#' @inherit classify_sfga params note
#' @returns An object of class factor with the same length as the longest input
#'   vector, containing small vulnerable newborn classifications. Its levels
#'   are `c("Preterm SGA", "Preterm AGA", "Preterm LGA", "Term SGA", "Term AGA",
#'   "Term LGA")`.
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
#' @seealso [classify_sfga()], which this function uses to stratify newborns
#'   into size-for-gestational age categories.
#' @export
classify_svn <- function(weight_kg, gest_days, sex) {
  is_term <- gest_days >= 37 * 7
  # classify_sfga/ig_nbs_value2centile will perform checks on params
  sfga <- classify_sfga(weight_kg = weight_kg,
                        gest_days = gest_days,
                        sex = sex,
                        severe = FALSE)
  sfga_cats <- levels(sfga)
  levels <- c(paste("Preterm", sfga_cats), paste("Term", sfga_cats))
  svn <- character(length = length(sfga))
  svn[!is_term & sfga == sfga_cats[1]] <- levels[1]
  svn[!is_term & sfga == sfga_cats[2]] <- levels[2]
  svn[!is_term & sfga == sfga_cats[3]] <- levels[3]
  svn[is_term & sfga == sfga_cats[1]] <- levels[4]
  svn[is_term & sfga == sfga_cats[2]] <- levels[5]
  svn[is_term & sfga == sfga_cats[3]] <- levels[6]
  factor(x = svn, levels = levels)
}

#' Classify stunting using WHO or INTERGROWTH-21<sup>st</sup>
#' length/height-for-age standards
#'
#' @description Classify stunting (low length/height-for-age) using WHO or
#'   INTERGROWTH-21<sup>st</sup> length/height-for-age standards depending on
#'   the gestational age at birth of the child. Severe stunting is below <-3 SD
#'   relative to mean length/height at a given age, whereas moderate stunting is
#'   -2 SD from the mean.
#' @inheritParams classify_sfga
#' @param lenht_cm Numeric vector of length one or more with length/height
#'   measurement(s) in cm.
#' @param age_days Numeric vector of length one or more with age(s) in days
#'   for each child. Should be between `0` to `1856` days. By default, gigs will
#'   replace out-of-bounds elements in `age_days` with `NA` and warn you. This
#'   behaviour can be customised using the functions in [gigs_options].
#' @param gest_days Numeric vector with gestational age(s) at birth in days.
#' @param outliers A single `TRUE` or `FALSE` value specifying whether
#'   implausible z-score value thresholds should be applied. Default = `FALSE`.
#' @returns An object of class factor with the same length as the longest input
#'   vector, containing stunting classifications. Its levels are
#'   `c("stunting_severe", "stunting", "not_stunting")` if `outliers =
#'   FALSE` (the default), else `c("stunting_severe", "stunting",
#'   "not_stunting", "outlier")`.
#'
#'   \tabular{lll}{
#'     \strong{Category} \tab \strong{Factor level} \tab
#'       \strong{Z-score bounds} \cr
#'     Severe stunting \tab `"stunting_severe"`     \tab `z` ≤ -3         \cr
#'     Stunting        \tab `"stunting"`            \tab -3 < `z` ≤ -2    \cr
#'     No stunting     \tab `"not_stunting"`        \tab `z` > -2         \cr
#'     Outlier         \tab `"outlier"`             \tab `abs(z)` > 5
#'   }
#' @note Input vectors are recycled by [vctrs::vec_recycle_common()], and must
#'   adhere to the [vctrs] recycling rules. This function assumes that your
#'   measurements were taken according to the WHO guidelines, which stipulate
#'   that recumbent length should not be measured after 730 days. Instead,
#'   standing height should be used. Implausible z-score bounds are sourced from
#'   the referenced WHO report, and classification cut-offs from the DHS manual.
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
classify_stunting <- function(lenht_cm,
                              age_days,
                              gest_days,
                              sex,
                              outliers = FALSE) {
  checkmate::qassert(outliers, rules = "B1")
  z_scores <- gigs_laz(lenht_cm = lenht_cm, age_days = age_days,
                       gest_days = gest_days, sex = sex)
  stunting <- rep(NA_character_, length(z_scores))
  stunting[z_scores <= -2] <- "stunting"
  stunting[z_scores <= -3] <- "stunting_severe"
  stunting[z_scores > -2] <- "not_stunting"
  stunting_lvls <- c("stunting_severe", "stunting", "not_stunting")
  if (outliers) {
    stunting[abs(z_scores) > 6] <- "outlier"
    stunting_lvls <- c(stunting_lvls, "outlier")
  }
  factor(stunting, levels = stunting_lvls)
}

#' Classify wasting using INTERGROWTH-21<sup>st</sup> weight-for-length or WHO
#' weight-for-length/height standards
#'
#'
#' @description Classify wasting (low weight-for-length/height) using the
#'   INTERGROWTH-21<sup>st</sup> weight-for-length or WHO Child Growth standards,
#'   specifically either the weight-for-length or weight-for-height standard
#'   depending on the age of the child. Severe stunting is `< -3` SD relative to
#'   the mean expected weight, whereas moderate wasting is `< -2` SD from the
#'   mean.
#' @inheritParams classify_sfga
#' @inheritParams classify_stunting
#' @returns An object of class factor with the same length as the longest input
#'   vector, containing wasting classifications. Its levels are
#'   `c("wasting_severe", "wasting", "not_wasting", "overweight")` if `outliers
#'   = FALSE` (the default), else `c("wasting_severe", "wasting", "not_wasting",
#'   "overweight", "outlier")`.
#'
#'   \tabular{lll}{
#'     \strong{Category} \tab \strong{Factor level} \tab
#'       \strong{Z-score bounds} \cr
#'     Severe wasting \tab `"wasting_severe"`       \tab `z` ≤ -3      \cr
#'     Wasting        \tab `"wasting"`              \tab -3 < `z` ≤ -2 \cr
#'     No wasting     \tab `"not_wasting"`          \tab `abs(z)` < 2  \cr
#'     Overweight     \tab `"overweight"`           \tab `z` ≥ 2       \cr
#'     Outlier        \tab `"outlier"`              \tab `abs(z)` > 5
#'   }
#' @note Input vectors will be recycled by [vctrs::vec_recycle_common()].
#'   Outlier z-score bounds are sourced from the referenced WHO report, and
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
#' # Outliers can be flagged if `outliers` set to TRUE
#' classify_wasting(
#'   weight_kg = c(5.75, 2.18, 3.00, 6.75),
#'   lenht_cm = c(67.7, 46.6, 50.0, 80.1),
#'   gest_days = c(251, 197, 225, 243),
#'   age_days = c(251, 197, 225, 243),
#'   sex =  c("F", "M", "F", "M"),
#'   outliers = TRUE
#' )
#' @export
classify_wasting <- function(weight_kg,
                             lenht_cm,
                             age_days,
                             gest_days,
                             sex,
                             outliers = FALSE) {
  checkmate::qassert(outliers, rules = "B1")
  z_scores <- gigs_wlz(weight_kg = weight_kg, lenht_cm = lenht_cm,
                       age_days = age_days, gest_days = gest_days, sex = sex)
  wasting <- character(length = length(z_scores))
  wasting[z_scores <= -2] <- "wasting"
  wasting[z_scores <= -3] <- "wasting_severe"
  wasting[abs(z_scores) < 2] <- "not_wasting"
  wasting[z_scores >= 2] <- "overweight"
  wasting_lvls <- c("wasting_severe", "wasting", "not_wasting", "overweight")
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
#' SD below the mean, underweight is less than 2 SD below the mean, and
#' overweight is > 2 SDs above the mean.
#'
#' @inheritParams classify_sfga
#' @inherit classify_stunting params references
#' @returns An object of class factor with the same length as the longest input
#'   vector, containing weight-for-age classifications. Its levels are
#'   `c("underweight_severe", "underweight", "normal", "overweight")` if
#'   `outliers = FALSE` (the default), else `c("underweight_severe",
#'   "underweight", "normal", "overweight", "outlier")`.
#'
#'   \tabular{lll}{
#'     \strong{Category}    \tab \strong{Factor level}  \tab
#'       \strong{Z-score bounds} \cr
#'     Severely underweight \tab `"underweight_severe"` \tab `z` ≤ -3      \cr
#'     Underweight          \tab `"underweight"`        \tab -3 < `z` ≤ -2 \cr
#'     Normal weight        \tab `"normal_weight"`      \tab `abs(z)` < 2  \cr
#'     Overweight           \tab `"overweight"`         \tab `z` ≥ 2       \cr
#'     Outlier              \tab `"outlier"`            \tab `z` < -6 or `z` > 5
#'   }
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
classify_wfa <- function(weight_kg,
                         age_days,
                         gest_days,
                         sex,
                         outliers = FALSE) {
  checkmate::qassert(outliers, rules = "B1")
  # gigs_waz does argument checking
  z_scores <- gigs_waz(weight_kg = weight_kg, age_days = age_days,
                       gest_days = gest_days, sex = sex)
  wfa <- character(length(z_scores))
  wfa[z_scores <= -2] <- "underweight"
  wfa[z_scores <= -3] <- "underweight_severe"
  wfa[abs(z_scores) < 2] <- "normal_weight"
  wfa[z_scores >= 2] <- "overweight"
  wfa_lvls <- c("underweight_severe", "underweight", "normal_weight",
                "overweight")
  if (outliers) {
    wfa[z_scores < -6 | z_scores > 5] <- "outlier"
    wfa_lvls <- c(wfa_lvls, "outlier")
  }
  factor(wfa, levels = wfa_lvls)
}

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.0} Primary literature referenced where relevant for each
#'   function in this script.
#' @srrstats {G1.4} This file's functions are all documented with `{roxygen2}`.
#' @srrstats {G2.0} Each classification function eventually passes its inputs to
#'   a `*_value2zscore()`/`*_value2centile()` function, which enforces specific
#'   conditions on input lengths.
#' @srrstats {G2.0a, EA1.3} Documentation in this script explicitly references
#'   length of inputs and recycling with [vctrs::vec_recycle_common()].
#'   Single-length inputs are explicitly referenced in the documentation and
#'   checked in their respective functions.
#' @srrstats {G2.1, G2.1a, G2.2} Documentation explicitly notes expected data
#'   types, lengths, and expected values for univariate input.
#' @srrstats {G2.3, G2.3a, G2.3b} Univariate character inputs are restricted to
#'   specific inputs by `{checkmate}` calls; these are case-sensitive and
#'   documented as such.
NULL