#' Convert z-scores/centiles to values in the WHO Child Growth Standards
#'
#' @param x,age_days,length_cm,height_cm Numeric vector of length one or more
#'   with x values. Elements in `x` or its standard-specific equivalents
#'   (`age_days`, `length_cm`, and `height_cm`) should have specific units and
#'   be between certain values depending on the standard in use (defined by
#'   `acronym`). These are:
#'   * Between 0 and 1856 days for `"wfa"`, `"bfa"`, `"lhfa"`, and `"hcfa"`.
#'   * Between 45 and 110 days for `"wfl"`.
#'   * Between 65 and 120 days for `"wfh"`.
#'   * Between 91 and 1856 days for `"acfa"`, `"ssfa"`, `"tsfa"`.
#'
#'   By default, gigs will replace out-of-bounds elements in `x` with `NA` and
#'   warn you. This behaviour can be customised using the functions in
#'   [gigs_options].
#' @param acronym Character vector of length one or more denoting the WHO Child
#'   Growth standard(s) in use. Each element should be one of:
#'   * `"wfa"` (weight-for-age)
#'   * `"bfa"` (BMI-for-age)
#'   * `"lhfa"` (length/height-for-age)
#'   * `"wfl"` (weight-for-length)
#'   * `"wfh"` (weight-for-height)
#'   * `"hcfa"` (head circumference-for-age)
#'   * `"acfa"` (arm circumference-for-age)
#'   * `"ssfa"` (subscapular skinfold-for-age)
#'   * `"tsfa"` (triceps skinfold-for-age)
#'
#'   This argument is case-sensitive. By default, gigs will replace elements in
#'   `acronym` which are not one of the above values with `NA` and warn you.
#'   This behaviour can be customised using the functions in [gigs_options]. If
#'   all elements in `acronym` are not one of the above values, gigs will throw
#'   an error.
#' @srrstats {G2.3b} Explicit reference to `acronym` case-sensitivity.
#' @inherit shared_roxygen_params params note
#' @inherit shared_zscore2value_returns return
#' @references
#' de Onis M, Garza C, Victora CG, Onyango AW, Frongillo EA, Martines J. **The
#' WHO Multicentre Growth Reference Study: planning, study design, and
#' methodology** *Food Nutr Bull.* 2004, **25(1 Suppl):S15-26.**
#' \doi{10.1177/15648265040251s104}
#'
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
#' @examples
#' # Convert centiles to values
#' p <- 0.25 # 25th centile
#' who_gs_centile2value(p = p, x = 501, sex = "M", acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Or z-scores to values
#' z <- qnorm(p) # Z-score equivalent of 25th centile
#' who_gs_zscore2value(z = z, x = 501, sex = "M", acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' who_gs_zscore2value(z = z, x = 300, sex = "M", acronym = "lhfa") |>
#'   round(digits = 2)
#'
#' # ... or by using a standard-specific function
#' who_gs_lhfa_zscore2value(z = z, age_days = 300, sex = "M") |>
#'   round(digits = 2)
#'
#' # Inputs are recycled to the input of the longest length
#' who_gs_ssfa_centile2value(p = seq(0.1, 0.9, by = 0.2),
#'                           age_days = 670,
#'                           sex = "M") |>
#'   round(digits = 2)
#'
#' # Bad inputs will not stop the function but will instead return NA - here 60
#' # cm for height_cm is incompatible with the WHO Growth Standard for
#' # weight-for-height
#' who_gs_wfh_zscore2value(z = 0,
#'                         height_cm = c(60, 85, 105, 120),
#'                         sex = "M") |>
#'   round(digits = 2)
#' @rdname who_gs_zscore2value
#' @export
who_gs_zscore2value <- function(z, x, sex, acronym) {
  validate_who_gs(z = z, x = x, sex = sex, acronym = acronym) |>
    do.call(what = who_gs_z2v_internal)
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_wfa_zscore2value <- function(z, age_days, sex) {
  acronym <- "wfa"
  validate_who_gs(z = z, x = age_days, sex = sex, acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]]) |>
    do.call(what = who_gs_z2v_internal)
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_bfa_zscore2value <- function(z, age_days, sex) {
  acronym <- "bfa"
  validate_who_gs(z = z, x = age_days, sex = sex, acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]]) |>
    do.call(what = who_gs_z2v_internal)
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_lhfa_zscore2value <- function(z, age_days, sex) {
  acronym <- "lhfa"
  validate_who_gs(z = z, x = age_days, sex = sex, acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]]) |>
    do.call(what = who_gs_z2v_internal)
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_wfl_zscore2value <- function(z, length_cm, sex) {
  acronym <- "wfl"
  validate_who_gs(z = z, x = length_cm, sex = sex, acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]]) |>
    do.call(what = who_gs_z2v_internal)
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_wfh_zscore2value <- function(z, height_cm, sex) {
  acronym <- "wfh"
  validate_who_gs(z = z, x = height_cm, sex = sex, acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]]) |>
    do.call(what = who_gs_z2v_internal)
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_hcfa_zscore2value <- function(z, age_days, sex) {
  acronym <- "hcfa"
  validate_who_gs(z = z, x = age_days, sex = sex, acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]]) |>
    do.call(what = who_gs_z2v_internal)
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_acfa_zscore2value <- function(z, age_days, sex) {
  acronym <- "acfa"
  validate_who_gs(z = z, x = age_days, sex = sex, acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]]) |>
    do.call(what = who_gs_z2v_internal)
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_ssfa_zscore2value <- function(z, age_days, sex) {
  acronym <- "ssfa"
  validate_who_gs(z = z, x = age_days, sex = sex, acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]]) |>
    do.call(what = who_gs_z2v_internal)
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_tsfa_zscore2value <- function(z, age_days, sex) {
  acronym <- "tsfa"
  validate_who_gs(z = z, x = age_days, sex = sex, acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]]) |>
    do.call(what = who_gs_z2v_internal)
}

#' @rdname who_gs_zscore2value
#' @importFrom stats qnorm
#' @export
who_gs_centile2value <- function(p, x, sex, acronym) {
  validated <- validate_who_gs(p = p, x = x, sex = sex, acronym = acronym)
  with(validated, who_gs_z2v_internal(qnorm(p), x, sex, acronym))
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_wfa_centile2value <- function(p, age_days, sex) {
  acronym <- "wfa"
  validated <- validate_who_gs(p = p,
                               x = age_days,
                               sex = sex,
                               acronym = acronym,
                               x_name = gigs::who_gs[[acronym]][["x"]])
  with(validated, who_gs_z2v_internal(qnorm(p), x, sex, acronym))
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_bfa_centile2value <- function(p, age_days, sex) {
  acronym <- "bfa"
  validated <- validate_who_gs(p = p,
                               x = age_days,
                               sex = sex,
                               acronym = acronym,
                               x_name = gigs::who_gs[[acronym]][["x"]])
  with(validated, who_gs_z2v_internal(qnorm(p), x, sex, acronym))
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_lhfa_centile2value <- function(p, age_days, sex)  {
  acronym <- "lhfa"
  validated <- validate_who_gs(p = p,
                               x = age_days,
                               sex = sex,
                               acronym = acronym,
                               x_name = gigs::who_gs[[acronym]][["x"]])
  with(validated, who_gs_z2v_internal(qnorm(p), x, sex, acronym))
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_wfl_centile2value <- function(p, length_cm, sex) {
  acronym <- "wfl"
  validated <- validate_who_gs(p = p,
                               x = length_cm,
                               sex = sex,
                               acronym = acronym,
                               x_name = gigs::who_gs[[acronym]][["x"]])
  with(validated, who_gs_z2v_internal(qnorm(p), x, sex, acronym))
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_wfh_centile2value <- function(p, height_cm, sex) {
  acronym <- "wfh"
  validated <- validate_who_gs(p = p,
                               x = height_cm,
                               sex = sex,
                               acronym = acronym,
                               x_name = gigs::who_gs[[acronym]][["x"]])
  with(validated, who_gs_z2v_internal(qnorm(p), x, sex, acronym))
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_hcfa_centile2value <- function(p, age_days, sex) {
  acronym <- "hcfa"
  validated <- validate_who_gs(p = p,
                               x = age_days,
                               sex = sex,
                               acronym = acronym,
                               x_name = gigs::who_gs[[acronym]][["x"]])
  with(validated, who_gs_z2v_internal(qnorm(p), x, sex, acronym))
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_acfa_centile2value <- function(p, age_days, sex) {
  acronym <- "acfa"
  validated <- validate_who_gs(p = p,
                               x = age_days,
                               sex = sex,
                               acronym = acronym,
                               x_name = gigs::who_gs[[acronym]][["x"]])
  with(validated, who_gs_z2v_internal(qnorm(p), x, sex, acronym))
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_ssfa_centile2value <- function(p, age_days, sex) {
  acronym <- "ssfa"
  validated <- validate_who_gs(p = p,
                               x = age_days,
                               sex = sex,
                               acronym = acronym,
                               x_name = gigs::who_gs[[acronym]][["x"]])
  with(validated, who_gs_z2v_internal(qnorm(p), x, sex, acronym))
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_tsfa_centile2value <- function(p, age_days, sex) {
  acronym <- "tsfa"
  validated <- validate_who_gs(p = p,
                               x = age_days,
                               sex = sex,
                               acronym = acronym,
                               x_name = gigs::who_gs[[acronym]][["x"]])
  with(validated, who_gs_z2v_internal(qnorm(p), x, sex, acronym))
}

#' Convert values to z-scores/centiles in the WHO Child Growth Standards
#'
#' @param weight_kg Numeric vector of length one or more with weight
#'   measurement(s) in kg.
#' @param bmi Numeric vector of length one or more with body mass index
#'   measurement(s) in kg/m<sup>2</sup>.
#' @param lenht_cm Numeric vector of length one or more with length/height
#'   measurement(s) in cm.
#' @param headcirc_cm Numeric vector of length one or more with head
#'   circumference measurement(s) in cm.
#' @param armcirc_cm Numeric vector of length one or more with arm circumference
#'   measurement(s) in cm.
#' @param subscap_sf_mm Numeric vector of length one or more with subscapular
#'   skinfold measurement(s) in mm.
#' @param triceps_sf_mm Numeric vector of length one or more with triceps
#'   skinfold measurement(s) in mm.
#' @inherit shared_roxygen_params params note
#' @inherit who_gs_zscore2value params references
#' @inherit shared_value2zscore_returns return
#' @examples
#' # Convert values to centiles
#' who_gs_value2centile(y = 10.1, x = 505, sex = "M", acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Or values to z-scores
#' who_gs_value2zscore(y = 10.1, x = 505, sex = "M", acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' who_gs_value2centile(y = 75.2, x = 300, sex = "M", acronym = "lhfa") |>
#'   round(digits = 2)
#'
#' # ... or by using a standard-specific function
#' who_gs_lhfa_value2centile(lenht_cm = 75.2, age_days = 300, sex = "M") |>
#'   round(digits = 2)
#'
#' # Inputs are recycled to the input of the longest length
#' who_gs_ssfa_value2centile(subscap_sf_mm = 6.1,
#'                           age_days = seq(450, 650, by = 50),
#'                           sex = "M") |>
#'   round(digits = 2)
#'
#' # Bad inputs will not stop the function but will instead return NA - here
#' # 121 cm for height_cm is incompatible with the WHO Growth Standard for
#' # weight-for-height
#' who_gs_wfh_value2zscore(weight_kg = c(7.4, 14.2, 21.8, 22.4),
#'                         height_cm = c(65, 95, 120, 121),
#'                         sex = "M") |>
#'   round(digits = 2)
#' @rdname who_gs_value2zscore
#' @export
who_gs_value2zscore <- function(y, x, sex, acronym) {
  validate_who_gs(y = y, x = x, sex = sex, acronym = acronym) |>
    do.call(what = who_gs_v2z_internal)
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_wfa_value2zscore <- function(weight_kg, age_days, sex) {
  acronym <- "wfa"
  validate_who_gs(y = weight_kg,
                  x = age_days,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]],
                  y_name = gigs::who_gs[[acronym]][["y"]]) |>
    do.call(what = who_gs_v2z_internal)
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_bfa_value2zscore <- function(bmi, age_days, sex) {
  acronym <- "bfa"
  validate_who_gs(y = bmi,
                  x = age_days,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]],
                  y_name = gigs::who_gs[[acronym]][["y"]]) |>
    do.call(what = who_gs_v2z_internal)
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_lhfa_value2zscore <- function(lenht_cm, age_days, sex) {
  acronym <- "lhfa"
  validate_who_gs(y = lenht_cm,
                  x = age_days,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]],
                  y_name = gigs::who_gs[[acronym]][["y"]]) |>
    do.call(what = who_gs_v2z_internal)
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_wfl_value2zscore <- function(weight_kg, length_cm, sex) {
  acronym <- "wfl"
  validate_who_gs(y = weight_kg,
                  x = length_cm,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]],
                  y_name = gigs::who_gs[[acronym]][["y"]]) |>
    do.call(what = who_gs_v2z_internal)
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_wfh_value2zscore <- function(weight_kg, height_cm, sex) {
  acronym <- "wfh"
  validate_who_gs(y = weight_kg,
                  x = height_cm,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]],
                  y_name = gigs::who_gs[[acronym]][["y"]]) |>
    do.call(what = who_gs_v2z_internal)
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_hcfa_value2zscore <- function(headcirc_cm, age_days, sex) {
  acronym <- "hcfa"
  validate_who_gs(y = headcirc_cm,
                  x = age_days,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]],
                  y_name = gigs::who_gs[[acronym]][["y"]]) |>
    do.call(what = who_gs_v2z_internal)
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_acfa_value2zscore <- function(armcirc_cm, age_days, sex) {
  acronym <- "acfa"
  validate_who_gs(y = armcirc_cm,
                  x = age_days,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]],
                  y_name = gigs::who_gs[[acronym]][["y"]]) |>
    do.call(what = who_gs_v2z_internal)
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_ssfa_value2zscore <- function(subscap_sf_mm, age_days, sex) {
  acronym <- "ssfa"
  validate_who_gs(y = subscap_sf_mm,
                  x = age_days,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]],
                  y_name = gigs::who_gs[[acronym]][["y"]]) |>
    do.call(what = who_gs_v2z_internal)
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_tsfa_value2zscore <- function(triceps_sf_mm, age_days, sex) {
  acronym <- "tsfa"
  validate_who_gs(y = triceps_sf_mm,
                  x = age_days,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]],
                  y_name = gigs::who_gs[[acronym]][["y"]]) |>
    do.call(what = who_gs_v2z_internal)
}

#' @rdname who_gs_value2zscore
#' @importFrom stats pnorm
#' @export
who_gs_value2centile <- function(y, x, sex, acronym) {
  validate_who_gs(y = y, x = x, sex = sex, acronym = acronym) |>
    do.call(what = who_gs_v2z_internal) |>
    pnorm()
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_wfa_value2centile <- function(weight_kg, age_days, sex) {
  acronym <- "wfa"
  validate_who_gs(y = weight_kg,
                  x = age_days,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]],
                  y_name = gigs::who_gs[[acronym]][["y"]]) |>
    do.call(what = who_gs_v2z_internal) |>
    pnorm()
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_bfa_value2centile <- function(bmi, age_days, sex) {
  acronym <- "bfa"
  validate_who_gs(y = bmi,
                  x = age_days,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]],
                  y_name = gigs::who_gs[[acronym]][["y"]]) |>
    do.call(what = who_gs_v2z_internal) |>
    pnorm()
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_lhfa_value2centile <- function(lenht_cm, age_days, sex) {
  acronym <- "lhfa"
  validate_who_gs(y = lenht_cm,
                  x = age_days,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]],
                  y_name = gigs::who_gs[[acronym]][["y"]]) |>
    do.call(what = who_gs_v2z_internal) |>
    pnorm()
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_wfl_value2centile <- function(weight_kg, length_cm, sex) {
  acronym <- "wfl"
  validate_who_gs(y = weight_kg,
                  x = length_cm,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]],
                  y_name = gigs::who_gs[[acronym]][["y"]]) |>
    do.call(what = who_gs_v2z_internal) |>
    pnorm()
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_wfh_value2centile <- function(weight_kg, height_cm, sex) {
  acronym <- "wfh"
  validate_who_gs(y = weight_kg,
                  x = height_cm,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]],
                  y_name = gigs::who_gs[[acronym]][["y"]]) |>
    do.call(what = who_gs_v2z_internal) |>
    pnorm()
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_hcfa_value2centile <- function(headcirc_cm, age_days, sex) {
  acronym <- "hcfa"
  validate_who_gs(y = headcirc_cm,
                  x = age_days,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]],
                  y_name = gigs::who_gs[[acronym]][["y"]]) |>
    do.call(what = who_gs_v2z_internal) |>
    pnorm()
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_acfa_value2centile <- function(armcirc_cm, age_days, sex) {
  acronym <- "acfa"
  validate_who_gs(y = armcirc_cm,
                  x = age_days,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]],
                  y_name = gigs::who_gs[[acronym]][["y"]]) |>
    do.call(what = who_gs_v2z_internal) |>
    pnorm()
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_ssfa_value2centile <- function(subscap_sf_mm, age_days, sex) {
  acronym <- "ssfa"
  validate_who_gs(y = subscap_sf_mm,
                  x = age_days,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]],
                  y_name = gigs::who_gs[[acronym]][["y"]]) |>
    do.call(what = who_gs_v2z_internal) |>
    pnorm()
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_tsfa_value2centile <- function(triceps_sf_mm, age_days, sex) {
  acronym <- "tsfa"
  validate_who_gs(y = triceps_sf_mm,
                  x = age_days,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::who_gs[[acronym]][["x"]],
                  y_name = gigs::who_gs[[acronym]][["y"]]) |>
    do.call(what = who_gs_v2z_internal) |>
    pnorm()
}

# INTERNAL: WHO Child Growth Standards conversion functions --------------------

#' Convert z-scores to values in the INTERGROWTH-21<sup>st</sup> Postnatal
#' Growth standards
#' @inherit who_gs_zscore2value params return
#' @note This function will fail if given inputs of different lengths.
#' @noRd
who_gs_z2v_internal <- function(z, x, sex, acronym) {
  lms <- who_gs_lms(x = x, sex = sex, acronym = acronym)
  with(lms, who_gs_lms_z2v(z = z, l = L, m = M, s = S, acronym = acronym))
}

#' Convert values to z-scores in the INTERGROWTH-21<sup>st</sup> Postnatal
#' Growth standards
#' @inherit who_gs_zscore2value params return
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
#' @param acronym Character vector of same length as `x` with acronyms for the
#'   WHO Child Growth standards. Each element should be one of
#'  `"wfa"` (weight-for-age), `"bfa"` (BMI-for-age), `"lhfa"`
#'  (length/height-for-age), `"wfl"` (weight-for-length), or `"wfh"`
#'  (weight-for-height), `"hcfa"` (head circumference-for-age), `"acfa"`
#'  (arm circumference-for-age), `"ssfa"` (subscapular skinfold-for-age), or
#'  `"tsfa"` (triceps skinfold-for-age).
#' @returns A dataframe with lambda (`"L"`), mu (`"M"`) and sigma (`"S"`) values
#'   for the WHO standard(s) specified by the `acronym` parameter, for the
#'   supplied `x` and `sex` values.
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
  retrieve_coefficients(x = x, sex = sex, acronym = acronym,
                        coeff_tbls = gigs::who_gs_coeffs,
                        coeff_names = c("L", "M", "S"))
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
#' @param y A numeric vector of the same length as `z` with measured values.
#' @param l,m,s Numeric vectors of the same length as `z` with lambda/mu/sigma
#'   values to use in the constraining procedure.
#' @note See WHO reports in attached references for the rationale for this
#'   constraining procedure.
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

#' Get constrained z-scores from values when z-scores were `> 3` for the WHO
#' Growth Standards
#' @inheritParams who_gs_lms_v2z_constrained
#' @noRd
who_gs_lms_v2z_over_three <- function(y, l, m, s) {
  sd3pos <-  who_gs_lms2value(l = l, m = m, s = s, n_sd = 3)
  sd23pos <- sd3pos - who_gs_lms2value(l = l, m = m, s = s, n_sd = 2)
  3 + (y - sd3pos) / sd23pos
}

#' Get constrained z-scores from values when z-scores were `< -3` for the WHO
#' Growth Standards
#' @inheritParams who_gs_lms_v2z_constrained
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
#' @note Performs constraining procedures described in WHO 2006/2007 reports
#'   using [who_gs_lms_z2v_over_three()] and
#'   [who_gs_lms_z2v_under_minus_three()], which compute an expected
#'   measurement accounting for the WHO constraining procedure..
#' @returns Numeric vector of expected values calculated using the WHO Child
#'   Growth standards' flavour of Cole (1990)'s LMS method.
#' @noRd
who_gs_lms_z2v <- function(z, l, m, s, acronym) {
  ifelse(
    test = abs(z) <= 3 | acronym %in% c("hcfa", "lhfa"),
    yes = ifelse(
      #' @srrstats {G3.0} Compare to sqrt(.Machine$double.eps) without `!= 0`
      test = abs(l) > sqrt(.Machine$double.eps),
      yes = exponent(z * s * l + 1, (1 / l)) * m,
      no = m * exp(s * z)),
    no = ifelse(
      test = z > 3,
      yes = who_gs_lms_z2v_over_three(z, l, m, s),
      no = who_gs_lms_z2v_under_minus_three(z, l, m, s))
  )
}

#' Get constrained values from z-scores when z-scores were `>3` for the WHO
#' Growth Standards
#' @inheritParams who_gs_lms_v2z_constrained
#' @noRd
who_gs_lms_z2v_over_three <- function(z, l, m, s) {
  sd3pos <- who_gs_lms2value(l = l, m = m, s = s, n_sd = 3)
  sd23pos <- sd3pos - who_gs_lms2value(l = l, m = m, s = s, n_sd = 2)
  (z - 3) * sd23pos + sd3pos
}

#' Get constrained values from z-scores when z-scores were `< -3` for the WHO
#' Growth Standards
#' @inheritParams who_gs_lms_v2z_constrained
#' @noRd
who_gs_lms_z2v_under_minus_three <- function(z, l, m, s) {
  sd3neg <- who_gs_lms2value(l = l, m = m, s = s, n_sd = -3)
  sd23neg <- who_gs_lms2value(l = l, m = m, s = s, n_sd = -2) - sd3neg
  (z + 3) * sd23neg + sd3neg
}

#' Robust exponentiation for use internally by [who_gs_lms_z2v()]
#'
#' @param a Numeric vector of length one or more. Can be negative.
#' @param pow Power to raise `a` to. Can be fractional.
#' @note Starting using this function as I would sometimes get errors in
#'   `y_from_LMS()` from odd but necessary exponentiations.
#' @source From StackOverflow: https://stackoverflow.com/questions/29920302/
#' @noRd
exponent <- function(a, pow) (abs(a)^pow) * sign(a)

#' Get values which are a specific z-score from the mean using WHO LMS
#' coefficients
#'
#' @param l,m,s Numeric vectors of lambda/mu/sigma value(s) from [who_gs_lms()].
#'   Each of these vectors should have the same length.
#' @param n_sd Single number denoting the number of standard deviations from the
#'   mean at which to compute expected values. This is equivalent ot z-score,
#'   but is not called 'z' here to reduce confusion between user-inputted
#'   z-scores and internally required `n_sd` values.
#' @inherit who_gs_lms references
#' @returns Numeric vector the same length as `l`/`m`/`s`, containing value(s)
#'   which are `n_sd` standard deviations from the `m` for the distributions
#'   described by each elementwise combination of `l`, `m`, and `s`.
#' @noRd
who_gs_lms2value <- function(l, m, s, n_sd) {
  m * (1 + l * s * n_sd)^(1 / l)
}

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.0} Primary literature referenced for each exported function,
#'   and for internal functions.
#' @srrstats {G1.4, G1.4a} All functions in file documented using `{roxygen2}`.
#' @srrstats {G2.0a, G2.1a, EA1.3} Exported function in this file document
#'   expectations on the length of inputs and their data types.
#' @srrstats {G2.0, G2.1, G2.2, G2.3, G2.3a, G2.6} These standards
#'   are met in all exported functions by passing inputs to [validate_ig_nbs()].
#'   All internal functions in this script are provided with vectors that have
#'   already been validated.
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.16} These standards are met
#'   in all exported functions by passing inputs to [validate_ig_nbs()]. All
#'   internal functions in this script are provided with vectors that have
#'   already checked for missing/undefined/out-of-bounds data.
NULL
