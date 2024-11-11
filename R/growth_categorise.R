# GROWTH CATEGORISATION --------------------------------------------------------
#   Takes a vector of centiles/z-scores, applies categorisation thresholds,
#   returns to user as factor of growth categories

#' Categorise birthweight centiles into size-for-gestational age strata
#'
#' @param p A numeric vector with birth weight centiles from the
#'   INTERGROWTH-21<sup>st</sup> Newborn Size standard. If elements of `p` are
#'   not between `0` and `1`, gigs will warn you. You can customise this
#'   behaviour with the [GIGS package-level options][gigs_options].
#' @param severe A single logical value specifying whether to categorise SGA
#'   values below the third centile as `"SGA(<3)"`. Default = `FALSE`.
#' @examples
#' p <- c(0.01, 0.07, 0.25, 0.75, 0.93, 0.99)
#' categorise_sfga(p = p, severe = FALSE)
#' categorise_sfga(p = p, severe = TRUE)
#' @returns An object of class factor with the same length as `p`, containing
#'   size-for-GA classifications. If `severe = FALSE`, levels are `c("SGA",
#'   "AGA", "LGA")`. If `severe = TRUE`, levels are `c("SGA(<3)", "SGA", "AGA",
#'   "LGA")`.
#' @details Cut-offs for size-for-gestational age categorisations are:
#'
#'   \tabular{lll}{
#'     \strong{Category} \tab \strong{Factor level} \tab
#'       \strong{Centile bounds} \cr
#'     Severely small-for-GA \tab `"SGA(<3)"` \tab `p` < 0.03      \cr
#'     Small-for-GA          \tab `"SGA"`     \tab `p` < 0.1       \cr
#'     Appropriate-for-GA    \tab `"AGA"`     \tab 0.1 =< `p` =< 0.9 \cr
#'     Large-for-GA          \tab `"LGA"`     \tab `p` > 0.9
#'   }
#' @seealso [classify_sfga()], which wraps this function for easy use in
#'   `data.frame`-based analytic pipelines.
#' @note The returned factor will have unused levels if none of your observed
#'   data fit given categories. To drop these, you will need to use
#'   [droplevels()] or similar.
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
categorise_sfga <- function(p, severe = FALSE) {
  checkmate::qassert(severe, rules = "B1")
  catch_and_throw_validate_issues(
    centiles <- vctrs::list_drop_empty(validate_yzp(p = p))[[1]]
  )
  categorise_sfga_internal(centiles, severe)
}

#' Categorise birthweight centiles and gestational ages into small vulnerable
#' newborn strata
#'
#' @inherit categorise_sfga params note
#' @param gest_days A numeric vector of length one or more with gestational
#'   age(s) in days. Used to determine whether infants are term or preterm (a
#'   gestational age > 259 days means an infant is term).
#' @returns An object of class factor with the same length as the longest input
#'   vector, containing small vulnerable newborn classifications. Its levels
#'   are `c("Preterm SGA", "Preterm AGA", "Preterm LGA", "Term SGA", "Term AGA",
#'   "Term LGA")`.
#' @examples
#' p <- c(0.01, 0.07, 0.25, 0.75, 0.93, 0.99)
#' gest_days <- c(250, 250, 250, 280, 280, 280) # 3 preterm, 3 term
#' categorise_svn(p = p, gest_days = gest_days)
#' @details Cut-offs for small, vulnerable newborn categorisations are:
#'
#'   \tabular{llll}{
#'     \strong{SVN category} \tab \strong{Factor level} \tab
#'       \strong{Gestational age range} \tab \strong{Centile bounds} \cr
#'     Preterm-SGA \tab `"Preterm SGA"` \tab `p` < 0.1       \tab
#'       `gest_days` < 259  \cr
#'     Preterm-AGA \tab `"Preterm AGA"` \tab 0.1 =< `p` =< 0.9 \tab
#'       `gest_days` < 259  \cr
#'     Preterm-LGA \tab `"Preterm LGA"` \tab `p` > 0.9       \tab
#'       `gest_days` < 259  \cr
#'     Term-SGA \tab `"Term SGA"`    \tab `p` < 0.1       \tab
#'       `gest_days` >= 259  \cr
#'     Term-AGA \tab `"Term AGA"`    \tab 0.1 =< `p` =< 0.9 \tab
#'       `gest_days` >= 259  \cr
#'     Term-LGA \tab `"Term LGA"`    \tab `p` > 0.9       \tab
#'       `gest_days` >= 259
#'   }
#'
#'   *Abbreviations:* SGA, small-for-gestational age; AGA,
#'     appropriate-for-gestational age; LGA, large-for-gestational age.
#' @note Input vectors are recycled by [vctrs::vec_recycle_common()], and must
#'   adhere to the [vctrs] recycling rules.
#' @references
#' Lawn JE, Ohuma EO, Bradley E, Idueta LS, Hazel E, Okwaraji YB et al.
#' **Small babies, big risks: global estimates of prevalence and mortality for
#' vulnerable newborns to accelerate change and improve counting.** *The Lancet*
#' 2023, *401(10389):1707-1719.* \doi{10.1016/S0140-6736(23)00522-6}
#' @seealso [classify_svn()], which wraps this function for easy use in
#'   analytic pipelines. Also check out [categorise_sfga()], which this function
#'   uses to stratify `p` into size-for-gestational age categories.
#' @export
categorise_svn <- function(p, gest_days) {
  validate_parameter_lengths(p = p, gest_days = gest_days)
  catch_and_throw_validate_issues(expr = {
    p <- vctrs::list_drop_empty(validate_yzp(p = p))[[1]]
    gest_days <- validate_numeric(gest_days, varname = "gest_days")
  })
  vctrs::vec_recycle_common(p = p, gest_days = gest_days) |>
    do.call(what = categorise_svn_internal)
}

#' Categorise length/height-for-age z-scores into stunting strata
#' @param lhaz A numeric vector of length one or more containing
#'   length/height-for-age z-scores (LHAZs).
#' @param outliers A single `TRUE` or `FALSE` value specifying whether
#'   implausible z-score thresholds should be applied. Default = `FALSE`.
#' @returns An object of class factor with the same length as the longest input
#'   vector, containing stunting classifications. Its levels are
#'   `c("stunting_severe", "stunting", "not_stunting")` if `outliers =
#'   FALSE` (the default), else `c("stunting_severe", "stunting",
#'   "not_stunting", "outlier")`.
#' @details Cut-offs for stunting categories are:
#'
#'   \tabular{lll}{
#'     \strong{Category} \tab \strong{Factor level} \tab
#'       \strong{Z-score bounds} \cr
#'     Severe stunting \tab `"stunting_severe"`     \tab `lhaz` =< -3         \cr
#'     Stunting        \tab `"stunting"`            \tab -3 < `lhaz` =< -2    \cr
#'     No stunting     \tab `"not_stunting"`        \tab `lhaz` > -2         \cr
#'     Outlier         \tab `"outlier"`             \tab `abs(lhaz)` > 6
#'   }
#' @examples
#' lhaz <- c(-6.5, -5, -3, 0, 3, 5, 6.5)
#' categorise_stunting(lhaz, outliers = FALSE)
#' categorise_stunting(lhaz, outliers = TRUE)
#' @note This function assumes that your measurements were taken according to
#'   WHO guidelines, which stipulate that recumbent length should not be
#'   measured after 730 days. Instead, standing height should be used.
#'   Implausible z-score bounds are sourced from the referenced WHO report, and
#'   classification cut-offs from the DHS manual.
#'
#'   The returned factor will have unused levels if none of your observed
#'   data fit given categories. To drop these, you will need to use
#'   [droplevels()] or similar.
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
#' @export
categorise_stunting <- function(lhaz, outliers = FALSE) {
  checkmate::qassert(outliers, rules = "B1")
  catch_and_throw_validate_issues(expr = {
    lhaz <- validate_numeric(lhaz, varname = "lhaz")
  })
  categorise_stunting_internal(lhaz, outliers)
}

#' Categorise weight-for-length/height z-scores into wasting strata
#' @param wlz A numeric vector of length one or more containing
#'   weight-for-length/height z-scores (WLZs).
#' @inherit categorise_stunting params
#' @returns An object of class factor with the same length as `wlz`, containing
#'   wasting classifications. Its levels are `c("wasting_severe", "wasting",
#'   "not_wasting", "overweight")` if `outliers = FALSE` (the default), else
#'   `c("wasting_severe", "wasting", "not_wasting", "overweight", "outlier")`.
#' @details Cut-offs for wasting categories are:
#'
#'   \tabular{lll}{
#'     \strong{Category} \tab \strong{Factor level} \tab
#'       \strong{Z-score bounds} \cr
#'     Severe wasting \tab `"wasting_severe"`       \tab `wlz` =< -3      \cr
#'     Wasting        \tab `"wasting"`              \tab -3 < `wlz` =< -2 \cr
#'     No wasting     \tab `"not_wasting"`          \tab `abs(wlz)` < 2  \cr
#'     Overweight     \tab `"overweight"`           \tab `wlz` >= 2      \cr
#'     Outlier        \tab `"outlier"`              \tab `abs(wlz)` > 5
#'   }
#' @note This function assumes that your measurements were taken according to
#'   WHO guidelines, which stipulate that recumbent length should not be
#'   measured after 730 days. Instead, standing height should be used.
#'   Implausible z-score bounds are sourced from the referenced WHO report, and
#'   classification cut-offs from the DHS manual.
#'
#'   The returned factor will have unused levels if none of your observed
#'   data fit given categories. To drop these, you will need to use
#'   [droplevels()] or similar.
#' @examples
#' wlz <- c(-5.5, -3, 0, 3, 5.5)
#' categorise_wasting(wlz, outliers = FALSE)
#' categorise_wasting(wlz, outliers = TRUE)
#' @inherit categorise_stunting references
#' @export
categorise_wasting <- function(wlz, outliers = FALSE) {
  checkmate::qassert(outliers, rules = "B1")
  catch_and_throw_validate_issues(expr = {
    wlz <- validate_numeric(wlz, varname = "wlz")
  })
  categorise_wasting_internal(wlz, outliers)
}

#' Categorise weight-for-age z-scores into weight-for-age strata
#'
#' @param waz A numeric vector of length one or more containing
#'   weight-for-age z-scores (WAZs).
#' @inherit categorise_stunting params
#' @returns An object of class factor with the same length as `waz`, containing
#'   weight-for-age classifications. Its levels are `c("underweight_severe",
#'   "underweight", "normal", "overweight")` if `outliers = FALSE` (the
#'   default), else `c("underweight_severe", "underweight", "normal",
#'   "overweight", "outlier")`.
#' @details Cut-offs for weight-for-age categories are:
#'
#'   \tabular{lll}{
#'     \strong{Category}    \tab \strong{Factor level}  \tab
#'       \strong{Z-score bounds} \cr
#'     Severely underweight \tab `"underweight_severe"` \tab `waz` =< -3      \cr
#'     Underweight          \tab `"underweight"`        \tab -3 < `waz` =< -2 \cr
#'     Normal weight        \tab `"normal_weight"`      \tab `abs(waz)` < 2  \cr
#'     Overweight           \tab `"overweight"`         \tab `waz` >= 2      \cr
#'     Outlier              \tab `"outlier"`        \tab `waz` < -6 or `waz` > 5
#'   }
#' @examples
#' waz <- c(-6.5, -3.5, -2.5, 0, 2.5, 3.5)
#' categorise_wfa(waz, outliers = FALSE)
#' categorise_wfa(waz, outliers = TRUE)
#' @inherit categorise_stunting references
#' @inherit categorise_sfga note
#' @export
categorise_wfa <- function(waz, outliers = FALSE) {
  checkmate::qassert(outliers, rules = "B1")
  catch_and_throw_validate_issues(expr = {
    waz <- validate_numeric(waz, varname = "waz")
  })
  categorise_wfa_internal(waz, outliers)
}

#' Categorise head circumference-for-age z-scores into head
#' circumference-for-age strata
#'
#' @param hcaz A numeric vector of length one or more containing
#'   head circumference-for-age z-scores (HCAZs).
#' @returns An object of class factor with the same length as `hcaz`, containing
#'   head circumference-for-age classifications. Its levels are
#'   `c("microcephaly_severe", "microcephaly", "normal_headcirc",
#'   "macrocephaly", "macrocephaly_severe")`.
#' @details Cut-offs for head size categories are:
#'
#'   \tabular{lll}{
#'     \strong{Category}    \tab \strong{Factor level}  \tab
#'       \strong{Z-score bounds} \cr
#'     Severe microcephaly       \tab `"microcephaly_severe"` \tab `hcaz` =< -3      \cr
#'     Microcephaly              \tab `"microcephaly"`        \tab -3 < `hcaz` =< -2 \cr
#'     Normal head circumference \tab `"normal_headcirc"`     \tab `abs(hcaz)` < 2   \cr
#'     Macrocephaly              \tab `"macrocephaly"`        \tab `hcaz` >= 2       \cr
#'     Severe macrocephaly       \tab `"macrocephaly_severe"` \tab `hcaz` >= 3
#'   }
#' @examples
#' hcaz <- c(-6.5, -3.5, -2.5, 0, 2.5, 3.5)
#' categorise_headsize(hcaz)
#' @references
#' Victora CG, Schuler-Faccini L, Matijasevich A, Ribeiro E, Pessoa A,
#' Barros FC. **Microcephaly in Brazil: how to interpret reported numbers?**
#' *The Lancet* 2016, *387(10019):621-624* \doi{10.1016/S0140-6736(16)00273-7}
#'
#' Accogli A, Geraldo AF, Piccolo G, Riva A, Scala M, Balagura G, et al.
#' **Diagnostic Approach to Macrocephaly in Children**. *Frontiers in
#' Paediatrics* 2022, *9:794069* \doi{10.3389/fped.2021.794069}
#' @inherit categorise_sfga note
#' @export
categorise_headsize <- function(hcaz) {
  catch_and_throw_validate_issues(expr = {
    hcaz <- validate_numeric(hcaz, varname = "hcaz")
  })
  categorise_headsize_internal(hcaz)
}

# Growth categorisation logic (INTERNAL) ---------------------------------------

#' Categorise birthweight centiles into size-for-GA strata (internal)
#' @description Internal size-for-GA categorisation logic, which assumes that
#'   inputs have been validated.
#' @inherit categorise_sfga params return references
#' @noRd
categorise_sfga_internal <- function(p, severe) {
  sfga <- rep_len(x = NA_character_, length(p))
  sfga[p < 0.1] <- "SGA"
  sfga[p >= 0.1 & p <= 0.9] <- "AGA"
  sfga[p > 0.9] <- "LGA"
  sfga_lvls <- c("SGA", "AGA", "LGA")
  if (severe) {
    sfga[p < 0.03] <- "SGA(<3)"
    sfga_lvls <- c("SGA(<3)", sfga_lvls)
  }
  factor(sfga, levels = sfga_lvls) |>
    handle_factor_levels(outcome = "size-for-GA")
}

#' Categorise birthweight centiles into small vulnerable newborn strata
#' (internal)
#' @description Internal size-for-GA categorisation logic, which assumes that
#'   inputs have been validated.
#' @inherit categorise_svn params return references
#' @noRd
categorise_svn_internal <- function(p, gest_days) {
  is_preterm <- gest_days < 259
  opt_unused_levels <- gigs_option_get(option = "handle_unused_levels",
                                       silent = TRUE)
  on.exit({
    gigs_option_set(option = "handle_unused_levels",
                    new_value = opt_unused_levels, silent = TRUE)
  })
  gigs_option_set("handle_unused_levels", "keep_silent", TRUE)
  sfga <- categorise_sfga_internal(p, severe = FALSE)
  gigs_option_set(option = "handle_unused_levels",
                  new_value = opt_unused_levels, silent = TRUE)
  sfga_lvls <- levels(sfga)
  levels <- c(paste("Preterm", sfga_lvls), paste("Term", sfga_lvls))

  svn <- character(length = length(sfga))
  svn[is_preterm & sfga == sfga_lvls[1]] <- levels[1]
  svn[is_preterm & sfga == sfga_lvls[2]] <- levels[2]
  svn[is_preterm & sfga == sfga_lvls[3]] <- levels[3]
  svn[!is_preterm & sfga == sfga_lvls[1]] <- levels[4]
  svn[!is_preterm & sfga == sfga_lvls[2]] <- levels[5]
  svn[!is_preterm & sfga == sfga_lvls[3]] <- levels[6]
  factor(svn, levels = levels) |>
    handle_factor_levels(outcome = "small vulnerable newborn")
}

#' Categorise length/height-for-age z-scores into stunting strata (internal)
#' @description Internal stunting categorisation logic, which assumes that
#'   inputs have been validated.
#' @inherit categorise_stunting params return references
#' @noRd
categorise_stunting_internal <- function(lhaz, outliers) {
  stunting <- rep(NA_character_, length(lhaz))
  stunting[lhaz <= -2] <- "stunting"
  stunting[lhaz <= -3] <- "stunting_severe"
  stunting[lhaz > -2] <- "not_stunting"
  stunting_lvls <- c("stunting_severe", "stunting", "not_stunting")
  if (outliers) {
    stunting[abs(lhaz) > 6] <- "outlier"
    stunting_lvls <- c(stunting_lvls, "outlier")
  }
  factor(stunting, levels = stunting_lvls) |>
    handle_factor_levels(outcome = "stunting")
}

#' Categorise weight-for-length/height z-scores into wasting strata (internal)
#' @description Internal wasting categorisation logic, which assumes that
#'   inputs have been validated.
#' @inherit categorise_wasting params return references
#' @noRd
categorise_wasting_internal <- function(wlz, outliers) {
  wasting <- character(length = length(wlz))
  wasting[wlz <= -2] <- "wasting"
  wasting[wlz <= -3] <- "wasting_severe"
  wasting[abs(wlz) < 2] <- "not_wasting"
  wasting[wlz >= 2] <- "overweight"
  wasting_lvls <- c("wasting_severe", "wasting", "not_wasting", "overweight")
  if (outliers) {
    wasting[abs(wlz) > 5] <- "outlier"
    wasting_lvls <- c(wasting_lvls, "outlier")
  }
  factor(wasting, levels = wasting_lvls) |>
    handle_factor_levels(outcome = "wasting")
}

#' Categorise weight-for-age z-scores into weight-for-age strata (internal)
#' @description Internal weight-for-age (underweight) categorisation logic,
#'   which assumes that inputs have been validated.
#' @inherit categorise_wfa params return references
#' @noRd
categorise_wfa_internal <- function(waz, outliers) {
  wfa <- character(length(waz))
  wfa[waz <= -2] <- "underweight"
  wfa[waz <= -3] <- "underweight_severe"
  wfa[abs(waz) < 2] <- "normal_weight"
  wfa[waz >= 2] <- "overweight"
  wfa_lvls <- c("underweight_severe", "underweight", "normal_weight",
                "overweight")
  if (outliers) {
    wfa[waz < -6 | waz > 5] <- "outlier"
    wfa_lvls <- c(wfa_lvls, "outlier")
  }
  factor(wfa, levels = wfa_lvls) |>
    handle_factor_levels(outcome = "weight-for-age (underweight)")
}

#' Categorise head circumference-for-age z-scores into head size strata
#' (internal)
#' @description Internal head circumference-for-age z-score categorisation
#'   logic, which assumes that inputs have been validated.
#' @inherit categorise_headsize params return references
#' @noRd
categorise_headsize_internal <- function(hcaz) {
  headsize <- character(length(hcaz))
  headsize[hcaz <= -2] <- "microcephaly"
  headsize[hcaz <= -3] <- "microcephaly_severe"
  headsize[abs(hcaz) < 2] <- "normal_headcirc"
  headsize[hcaz >= 2] <- "macrocephaly"
  headsize[hcaz >= 3] <- "macrocephaly_severe"
  headsize_lvls <- c("microcephaly_severe", "microcephaly", "normal_headcirc",
                     "macrocephaly", "macrocephaly_severe")
  factor(headsize, levels = headsize_lvls) |>
    handle_factor_levels(outcome = "head size")
}

#' Handle unused factor levels in categorise_*_internal() function outputs
#' @param fct A factor, from a categorise_*_internal() function
#' @param outcome A single-length character vector used to denote the 
#'   classification being done. Should be one of `c("size-for-GA", 
#'   "small vulnerable newborns", "stunting", "wasting", "weight-for-age 
#'   (underweight)", "head size")`. Is case-sensitive.
#' @returns A factor with unused levels dropped/kept based on how a user has set
#'   `.gigs_options$handle_unused_levels`.
#' @noRd
handle_factor_levels <- function(fct, outcome) {
  outcome_opts <- c("size-for-GA", "small vulnerable newborn", "stunting",
                    "wasting", "weight-for-age (underweight)", "head size")
  if (!outcome %in% outcome_opts) {
    cli::cli_abort(c("!" = "`outcome` must be one of {.val {outcome_opts}}",
                     "i" = "`outcome` was {.val {outcome}}."),
      call = rlang::current_env(),
      class = "gigs_handle_fctr_lvls_bad_outcome_str",
      .internal = TRUE
    )
  }   
  if (.gigs_options$handle_unused_levels %in% c("keep_silent", "keep_warn")) {
    if (.gigs_options$handle_unused_levels == "keep_warn") {
      unused <- setdiff(levels(fct), fct)
      if (length(unused) > 0) {
        cli::cli_alert_warning(
          paste0("Unused factor levels kept after {outcome} categorisation:",
                 " {.val {unused}}."),
          wrap = TRUE,
          class = "gigs_keeping_unused_fctr_lvls"
        )
      }
    }
    return(fct)
  }
  if (.gigs_options$handle_unused_levels %in% c("drop_silent", "drop_warn")) {
    fct_dropped <- droplevels(fct)
    if (.gigs_options$handle_unused_levels == "drop_warn") {
      unused <- setdiff(levels(fct), levels(fct_dropped))
      if (length(unused) > 0) {
        cli::cli_inform(
          paste0("Unused factor levels dropped after {outcome} categorisation:",
                 " {.val {unused}}."),
          wrap = TRUE,
          class = "gigs_dropping_unused_fctr_lvls"
        )
      }
    }
    return(fct_dropped)
  }
}

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.0} Primary literature referenced where relevant for each
#'   function in this script.
#' @srrstats {G1.4} This file's functions are all documented with `{roxygen2}`.
#' @srrstats {G2.0} Each exported function passes its inputs to
#'   `validate_*()` functions, which enforce specific input lengths.
#' @srrstats {G2.0a, EA1.3} Documentation in this script explicitly references
#'   length of inputs. Input lengths are explicitly referenced in the
#'   documentation and checked by `validate_*()` functions for each exported
#'   function.
#' @srrstats {G2.1, G2.1a, G2.2} Documentation explicitly notes expected data
#'   types, lengths, and expected values for univariate input.
#' @srrstats {G2.3, G2.3a, G2.3b} Univariate character inputs are restricted to
#'   specific inputs by `{checkmate}` calls; these are case-sensitive and
#'   documented as such.
NULL