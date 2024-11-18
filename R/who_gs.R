# INTERNAL: WHO Child Growth Standards conversion functions --------------------

#' Convert z-scores to values in the WHO Child Growth Standards
#' @inherit zscore2value params return
#' @note This function will fail if given inputs of different lengths.
#' @noRd
who_gs_z2v_internal <- function(z, x, sex, acronym) {
  lms <- who_gs_lms(x = x, sex = sex, acronym = acronym)
  with(lms, who_gs_lms_z2v(z = z, l = L, m = M, s = S, acronym = acronym))
}

#' Convert values to z-scores in the WHO Child Growth Standards
#' @inherit zscore2value params return
#' @note This function will fail if given inputs of different lengths.
#' @noRd
who_gs_v2z_internal <- function(y, x, sex, acronym) {
  lms <- who_gs_lms(x = x, sex = sex, acronym = acronym)
  with(lms, who_gs_lms_v2z(y, l = L, m = M, s = S, acronym = acronym))
}

# INTERNAL: Retriving LMS values -----------------------------------------------

#' Retrieve LMS values for WHO Child Growth Standards
#'
#' @param x Numeric vector of length one or more with x value(s) at which to
#'   retrieve LMS values. Elements which lie outside the bounds of valid `x`
#'   values for their respective `acronym` will return `NA` for `L`, `M`, and
#'   `S`.
#' @param sex Character vector of same length as `x` with sex(es), either `"M"`
#'   (male) or `"F"` (female).
#' @param acronym A single-length character vector denoting the WHO Child Growth
#'   standard in use. Should be one of `"wfa"` (weight-for-age), `"bfa"`
#'   (BMI-for-age), `"lhfa"` (length/height-for-age), `"wfl"`
#'   (weight-for-length), or `"wfh"` (weight-for-height), `"hcfa"` (head
#'   circumference-for-age), `"acfa"` (arm circumference-for-age), `"ssfa"`
#'   (subscapular skinfold-for-age), or `"tsfa"` (triceps skinfold-for-age).
#' @returns A named list with lambda (`"L"`), mu (`"M"`) and sigma (`"S"`)
#'   values for the WHO standard specified by `acronym`, for each
#'   elementwise combination of `x` and `sex`.
#' @references
#' World Health Organisation. **WHO child growth standards:
#' length/height-for-age, weight-for-age, weight-for-length, weight-for-height
#' and body mass index-for-age: methods and development.** *Technical report,
#' WHO, Geneva*, 2006.
#'
#' World Health Organisation. **WHO child growth standards: head
#' circumference-for-age, arm circumference-for-age, triceps skinfold-for-age
#' and subscapular skinfold-for-age: methods and development.** *Technical
#' report, WHO, Geneva*, 2007.
#'
#' Cole TJ. **The LMS method for constructing normalized growth standards** *Eur
#' J Clin Nutr.* 1990, **44(1):45-60.** PMID:
#' [2354692](https://pubmed.ncbi.nlm.nih.gov/2354692/)
#' @rdname who_gs_lms
#' @noRd
who_gs_lms <- function(x, sex, acronym) {
  retrieve_coefficients(x = x, sex = sex,
                        coeffs = gigs::who_gs_coeffs[[acronym]])
}

# INTERNAL: Manipulate LMS values for the WHO Growth standards -----------------

#' Convert values to z-scores in the WHO Child Growth standards using the
#' adapted WHO LMS method
#'
#' @param y A numeric vector of length one or more with measured values.
#' @param l,m,s Numeric vectors of the same length as `y` with lambda/mu/sigma
#'   values to use for conversion.
#' @note Uses [who_gs_lms_v2z_constrained()] to constrain any calculated
#'   z-scores which are `> 3` or `< -3`.
#' @returns Numeric vector of z-scores calculated using the WHO Child Growth
#'   standards' flavour of Cole (1990)'s LMS method.
#' @noRd
who_gs_lms_v2z <- function(y, l, m, s, acronym) {
  #' @srrstats {G3.0} Compare to sqrt(.Machine$double.eps) without `!= 0`
  z_unconstrained <- ifelse(test = abs(l) > sqrt(.Machine$double.eps),
                            yes = (abs((y / m)^l) - 1) / (s * l),
                            no = log(y / m) / s)
  ifelse(
    test = abs(z_unconstrained) <= 3 | acronym %in% c("hcfa", "lhfa"),
    yes = z_unconstrained,
    no = who_gs_lms_v2z_constrained(z_unconstrained, y, l, m, s)
  )
}

#' Convert values to z-scores in the WHO Child Growth standards with
#' restrainment
#'
#' @param z_unconstrained A numeric vector of length one or more with
#'   unconstrained z-scores calculated using values and Cole's (1990) LMS
#'   method.
#' @param y A numeric vector of the same length as `z_unconstrained` with
#'   measured values.
#' @param l,m,s Numeric vectors of the same length as `z_unconstrained` with
#'   lambda/mu/sigma values to use in the constraining procedure.
#' @note See WHO reports in attached references for the rationale for this
#'   constraining procedure.
#' @inherit who_gs_lms references
#' @returns Numeric vector of z-scores calculated using WHO Child Growth
#'   standards constraining procedures.
#' @noRd
who_gs_lms_v2z_constrained <- function(z_unconstrained, y, l, m, s) {
  ifelse(
    test = z_unconstrained > 3,
    yes = who_gs_lms_v2z_over_three(y, l, m, s),
    no = who_gs_lms_v2z_under_minus_three(y, l, m, s)
  )
}

#' Get constrained z-scores from values when unconstrained z-scores were `>3` in
#' the WHO Child Growth Standards
#' @inheritParams who_gs_lms_v2z_constrained
#' @inherit who_gs_lms references
#' @noRd
who_gs_lms_v2z_over_three <- function(y, l, m, s) {
  sd3pos <-  who_gs_lms2value(l = l, m = m, s = s, n_sd = 3)
  sd23pos <- sd3pos - who_gs_lms2value(l = l, m = m, s = s, n_sd = 2)
  3 + (y - sd3pos) / sd23pos
}

#' Get constrained z-scores from values when unconstrained z-scores were `< -3`
#' in the WHO Child Growth Standards
#' @inheritParams who_gs_lms_v2z_constrained
#' @inherit who_gs_lms references
#' @noRd
who_gs_lms_v2z_under_minus_three <- function(y, l, m, s) {
  sd3neg <-  who_gs_lms2value(l = l, m = m, s = s, n_sd = -3)
  sd23neg <- who_gs_lms2value(l = l, m = m, s = s, n_sd = -2) - sd3neg
  -3 + (y - sd3neg) / sd23neg
}

#' Convert z-scores to values in the WHO Child Growth standards using the
#' adapted WHO LMS method
#'
#' @param z A numeric vector of length one or more with z-scores.
#' @param l,m,s Numeric vectors of the same length as `y` with lambda/mu/sigma
#'   values to use for conversion.
#' @param acronym A single-length character vector denoting the WHO Child Growth
#'   standard in use. Should be one of `names(gigs::who_gs)`.
#' @note Performs constraining procedures described in WHO 2006/2007 reports
#'   using [who_gs_lms_z2v_over_three()] and
#'   [who_gs_lms_z2v_under_minus_three()], which compute an expected
#'   measurement accounting for the WHO constraining procedure.
#' @returns Numeric vector the same length as `z`, with expected values
#'   calculated using the WHO Child Growth standards' flavour of Cole (1990)'s
#'   LMS method.
#' @inherit who_gs_lms references
#' @noRd
who_gs_lms_z2v <- function(z, l, m, s, acronym) {
  ifelse(
    test = abs(z) <= 3 | acronym %in% c("hcfa", "lhfa"),
    yes = ifelse(
      #' @srrstats {G3.0} Compare to sqrt(.Machine$double.eps) without `!= 0`
      test = abs(l) > sqrt(.Machine$double.eps),
      yes = (z * s * l + 1)^(1 / l) * m,
      no = m * exp(s * z)),
    no = ifelse(
      test = z > 3,
      yes = who_gs_lms_z2v_over_three(z, l, m, s),
      no = who_gs_lms_z2v_under_minus_three(z, l, m, s))
  )
}

#' Get constrained values from z-scores when z-scores were `>3` for the WHO
#' Growth Standards
#' @inheritParams who_gs_lms_z2v
#' @inherit who_gs_lms references
#' @noRd
who_gs_lms_z2v_over_three <- function(z, l, m, s) {
  sd3pos <- who_gs_lms2value(l = l, m = m, s = s, n_sd = 3)
  sd23pos <- sd3pos - who_gs_lms2value(l = l, m = m, s = s, n_sd = 2)
  (z - 3) * sd23pos + sd3pos
}

#' Get constrained values from z-scores when z-scores were `< -3` for the WHO
#' Growth Standards
#' @inheritParams who_gs_lms_z2v
#' @noRd
who_gs_lms_z2v_under_minus_three <- function(z, l, m, s) {
  sd3neg <- who_gs_lms2value(l = l, m = m, s = s, n_sd = -3)
  sd23neg <- who_gs_lms2value(l = l, m = m, s = s, n_sd = -2) - sd3neg
  (z + 3) * sd23neg + sd3neg
}

#' Get values which are a specific z-score from the mean using WHO LMS
#' coefficients
#'
#' @param l,m,s Numeric vectors of length one or more containing lambda/mu/sigma
#'   value(s) from [who_gs_lms()]. Each of these vectors should have the same
#'   length.
#' @param n_sd A single-length numeric vector denoting the number of standard
#'   deviations from the mean at which to compute expected values. This is
#'   equivalent to a z-score, but is not called 'z' here to delineate between
#'   user-inputted z-scores and internally used `n_sd` values.
#' @inherit who_gs_lms references
#' @returns Numeric vector the same length as `l`/`m`/`s`, containing value(s)
#'   which are `n_sd` standard deviations from the `m` for the distributions
#'   described by each elementwise combination of `l`, `m`, and `s`.
#' @noRd
who_gs_lms2value <- function(l, m, s, n_sd) {
  m * (1 + l * s * n_sd)^(1 / l)
}

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.0} Primary literature referenced for internal functions.
#' @srrstats {G1.4, G1.4a} All functions in file documented using `{roxygen2}`.
#' @srrstats {G2.0, G2.1, G2.2, G2.3, G2.3a, G2.6} All internal functions in
#'   this script are provided with vectors that have already been validated.
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.16} All internal functions in
#'   this script are provided with vectors that have already checked for
#' missing/undefined/out-of-bounds data.
NULL
