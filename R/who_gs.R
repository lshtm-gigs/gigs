#' Convert z-scores/centiles to values in the WHO Child Growth Standards
#'
#' @param x,age_days,length_cm,height_cm Numeric vector with `x` values at which
#'   to convert inputs. Must be within bounds of available `x` values for given
#'   acronym. The standard-specific versions of each function specify `x`:
#'   either `age_days` (age since birth in days), `length_cm` (recumbent length
#'   measurement(s) in cm) or `height_cm` (standing height measurement(s) in
#'   cm).
#' @param acronym Valid acronym(s) for the WHO Child Growth standards: either
#'   `"wfa"` (weight-for-age), `"bfa"` (BMI-for-age), `"lhfa"`
#'   (length/height-for-age), `"wfl"` (weight-for-length), or `"wfh"`
#'   (weight-for-height), `"hcfa"` (head circumference-for-age), `"acfa"`
#'   (arm circumference-for-age), `"ssfa"` (subscapular skinfold-for-age), or
#'   `"tsfa"` (triceps skinfold-for-age).
#' @inherit shared_roxygen_params params note
#' @inherit shared_value2zscore_returns return
#' @references
#' de Onis M, Garza C, Victora CG, Onyango AW, Frongillo EA, Martines J. **The
#' WHO Multicentre Growth Reference Study: planning, study design, and
#' methodology** *Food Nutr Bull.* 2004, **25(1 Suppl):S15-26.** doi:
#' [10.1177/15648265040251s104](https://journals.sagepub.com/doi/10.1177/15648265040251S104)
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
  validated <- vctrs::vec_recycle_common(z = z,
                                         x = x,
                                         sex = sex,
                                         acronym = acronym) |>
    do.call(what = validate_who_gs) |>
    drop_null_elements()

  lms <- who_gs_lms(x = validated[["x"]],
                    sex = validated[["sex"]],
                    acronym = validated[["acronym"]])

  # from https://stackoverflow.com/questions/29920302/raising-vector-with-negative-numbers-to-a-fractional-exponent-in-r
  exponent <- function(a, pow) (abs(a)^pow) * sign(a)

  z_over_three <- function(l, m, s, z) {
    sd3pos <- who_gs_lms2value(l = l, m = m, s = s, n_sd = 3)
    sd23pos <- sd3pos - who_gs_lms2value(l = l, m = m, s = s, n_sd = 2)
    (z - 3) * sd23pos + sd3pos
  }

  z_under_minus_three <- function(l, m, s, z) {
    sd3neg <- who_gs_lms2value(l = l, m = m, s = s, n_sd = -3)
    sd23neg <- who_gs_lms2value(l = l, m = m, s = s, n_sd = -2) - sd3neg
    (z + 3) * sd23neg + sd3neg
  }

  y_from_LMS <- function(l, m, s, z, acronym) {
    ifelse(
      test = abs(z) <= 3 | acronym %in% c("hcfa", "lhfa"),
      yes = ifelse(
        test = l != 0,
        yes = exponent(z * s * l + 1, (1 / l)) * m,
        no = m * exp(s * z)),
      no = ifelse(
        test = z > 3,
        yes = z_over_three(l, m, s, z),
        no = z_under_minus_three(l, m, s, z))
    )
  }
  checkmate::assert_numeric(z)
  ifelse(validated[["sex"]] == "U",
         yes = mean_if_sex_undefined(who_gs_zscore2value,
                                     arg1 = z,
                                     x_arg = validated[["x"]],
                                     acronym = validated[["acronym"]]),
         no = y_from_LMS(l = lms[[1]],
                         m = lms[[2]],
                         s = lms[[3]],
                         validated[["z"]],
                         validated[["acronym"]]))
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_wfa_zscore2value <- function(z, age_days, sex) {
  who_gs_zscore2value(z = z, x = age_days, sex = sex, acronym = "wfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_bfa_zscore2value <- function(z, age_days, sex) {
  who_gs_zscore2value(z = z, x = age_days, sex = sex, acronym = "bfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_lhfa_zscore2value <- function(z, age_days, sex) {
  who_gs_zscore2value(z = z, x = age_days, sex = sex, acronym = "lhfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_wfl_zscore2value <- function(z, length_cm, sex) {
  who_gs_zscore2value(z = z, x = length_cm, sex = sex, acronym = "wfl")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_wfh_zscore2value <- function(z, height_cm, sex) {
  who_gs_zscore2value(z = z, x = height_cm, sex = sex, acronym = "wfh")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_hcfa_zscore2value <- function(z, age_days, sex) {
  who_gs_zscore2value(z = z, x = age_days, sex = sex, acronym = "hcfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_acfa_zscore2value <- function(z, age_days, sex) {
  who_gs_zscore2value(z = z, x = age_days, sex = sex, acronym = "acfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_ssfa_zscore2value <- function(z, age_days, sex) {
  who_gs_zscore2value(z = z, x = age_days, sex = sex, acronym = "ssfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_tsfa_zscore2value <- function(z, age_days, sex) {
  who_gs_zscore2value(z = z, x = age_days, sex = sex, acronym = "tsfa")
}

#' @rdname who_gs_zscore2value
#' @importFrom stats qnorm
#' @export
who_gs_centile2value <- function(p, x, sex, acronym) {
  who_gs_zscore2value(z = qnorm(p), x = x, sex = sex, acronym = acronym)
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_wfa_centile2value <- function(p, age_days, sex) {
  who_gs_centile2value(p = p, x = age_days, sex = sex, acronym = "wfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_bfa_centile2value <- function(p, age_days, sex) {
  who_gs_centile2value(p = p, x = age_days, sex = sex, acronym = "bfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_lhfa_centile2value <- function(p, age_days, sex)  {
  who_gs_centile2value(p = p, x = age_days, sex = sex, acronym = "lhfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_wfl_centile2value <- function(p, length_cm, sex) {
  who_gs_centile2value(p = p, x = length_cm, sex = sex, acronym = "wfl")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_wfh_centile2value <- function(p, height_cm, sex) {
  who_gs_centile2value(p = p, x = height_cm, sex = sex, acronym = "wfh")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_hcfa_centile2value <- function(p, age_days, sex) {
  who_gs_centile2value(p = p, x = age_days, sex = sex, acronym = "hcfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_acfa_centile2value <- function(p, age_days, sex) {
  who_gs_centile2value(p = p, x = age_days, sex = sex, acronym = "acfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_ssfa_centile2value <- function(p, age_days, sex) {
  who_gs_centile2value(p = p, x = age_days, sex = sex, acronym = "ssfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_tsfa_centile2value <- function(p, age_days, sex) {
  who_gs_centile2value(p = p, x = age_days, sex = sex, acronym = "tsfa")
}

#' Convert values to z-scores/centiles in the WHO Child Growth Standards
#'
#' @param weight_kg Numeric vector of weight measurement(s) in kg.
#' @param bmi Numeric vector of body mass index measurement(s) in
#'   kg/m<sup>2</sup>.
#' @param lenht_cm Numeric vector of length/height measurement(s) in cm.
#' @param headcirc_cm Numeric vector of head circumference measurement(s) in cm.
#' @param armcirc_cm Numeric vector of arm circumference measurement(s) in cm.
#' @param subscap_sf_mm Numeric vector of subscapular skinfold measurement(s) in
#'   mm.
#' @param triceps_sf_mm Numeric vector of triceps skinfold measurement(s) in mm.
#' @inherit shared_roxygen_params params note
#' @inherit shared_value2zscore_returns return
#' @inherit who_gs_zscore2value params references
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
  validated <- vctrs::vec_recycle_common(y = y,
                                         x = x,
                                         sex = sex,
                                         acronym = acronym) |>
    do.call(what = validate_who_gs) |>
    drop_null_elements()

  lms <- who_gs_lms(x = validated[["x"]],
                    sex = validated[["sex"]],
                    acronym = validated[["acronym"]])

  z_over_three <- function(l, m, s, y) {
    sd3pos <-  who_gs_lms2value(l = l, m = m, s = s, n_sd = 3)
    sd23pos <- sd3pos - who_gs_lms2value(l = l, m = m, s = s, n_sd = 2)
    3 + (y - sd3pos) / sd23pos
  }
  z_under_minus_three <- function(l, m, s, y) {
    sd3neg <-  who_gs_lms2value(l = l, m = m, s = s, n_sd = -3)
    sd23neg <- who_gs_lms2value(l = l, m = m, s = s, n_sd = -2) - sd3neg
    -3 + (y - sd3neg) / sd23neg
  }

  z_from_LMS <- function(l, m, s, y, acronym) {
    z <- ifelse(test = l != 0,
                yes = (abs((y / m)^l) - 1) / (s * l),
                no = log(y / m) / s)
    ifelse(
      test = abs(z) <= 3 | acronym %in% c("hcfa", "lhfa"),
      yes = z,
      no = ifelse(test = z > 3,
                  yes = z_over_three(l, m, s, y),
                  no =  z_under_minus_three(l, m, s, y))
    )
  }

  ifelse(validated[["sex"]] == "U",
         yes = mean(c(
           who_gs_value2zscore(y = validated[["y"]], x = lms[["x"]],
                               sex = "M", acronym = validated[["acronym"]]),
           who_gs_value2zscore(y = validated[["y"]], x = lms[["x"]],
                               sex = "F", acronym = validated[["acronym"]])
         )),
         no = z_from_LMS(l = lms[[1]],
                         m = lms[[2]],
                         s = lms[[3]],
                         y = validated[["y"]],
                         acronym = validated[["acronym"]]))
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_wfa_value2zscore <- function(weight_kg, age_days, sex) {
  who_gs_value2zscore(y = weight_kg, x = age_days, sex = sex, acronym = "wfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_bfa_value2zscore <- function(bmi, age_days, sex) {
  who_gs_value2zscore(y = bmi, x = age_days, sex = sex, acronym = "bfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_lhfa_value2zscore <- function(lenht_cm, age_days, sex) {
  who_gs_value2zscore(y = lenht_cm, x = age_days, sex = sex, acronym = "lhfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_wfl_value2zscore <- function(weight_kg, length_cm, sex) {
  who_gs_value2zscore(y = weight_kg, x = length_cm, sex = sex, acronym = "wfl")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_wfh_value2zscore <- function(weight_kg, height_cm, sex) {
  who_gs_value2zscore(y = weight_kg, x = height_cm, sex = sex, acronym = "wfh")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_hcfa_value2zscore <- function(headcirc_cm, age_days, sex) {
  who_gs_value2zscore(y = headcirc_cm, x = age_days, sex = sex, acronym = "hcfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_acfa_value2zscore <- function(armcirc_cm, age_days, sex) {
  who_gs_value2zscore(y = armcirc_cm, x = age_days, sex = sex, acronym = "acfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_ssfa_value2zscore <- function(subscap_sf_mm, age_days, sex) {
  who_gs_value2zscore(y = subscap_sf_mm, x = age_days, sex = sex, acronym = "ssfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_tsfa_value2zscore <- function(triceps_sf_mm, age_days, sex) {
  who_gs_value2zscore(y = triceps_sf_mm, x = age_days, sex = sex, acronym = "tsfa")
}

#' @rdname who_gs_value2zscore
#' @importFrom stats pnorm
#' @export
who_gs_value2centile <- function(y, x, sex, acronym) {
  pnorm(who_gs_value2zscore(y = y, x = x, sex = sex, acronym = acronym))
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_wfa_value2centile <- function(weight_kg, age_days, sex) {
  who_gs_value2centile(y = weight_kg, x = age_days, sex = sex, acronym = "wfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_bfa_value2centile <- function(bmi, age_days, sex) {
  who_gs_value2centile(y = bmi, x = age_days, sex = sex, acronym = "bfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_lhfa_value2centile <- function(lenht_cm, age_days, sex) {
  who_gs_value2centile(y = lenht_cm, x = age_days, sex = sex, acronym = "lhfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_wfl_value2centile <- function(weight_kg, length_cm, sex) {
  who_gs_value2centile(y = weight_kg, x = length_cm, sex = sex, acronym = "wfl")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_wfh_value2centile <- function(weight_kg, height_cm, sex) {
  who_gs_value2centile(y = weight_kg, x = height_cm, sex = sex, acronym = "wfh")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_hcfa_value2centile <- function(headcirc_cm, age_days, sex) {
  who_gs_value2centile(y = headcirc_cm, x = age_days, sex = sex, acronym = "hcfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_acfa_value2centile <- function(armcirc_cm, age_days, sex) {
  who_gs_value2centile(y = armcirc_cm, x = age_days, sex = sex, acronym = "acfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_ssfa_value2centile <- function(subscap_sf_mm, age_days, sex) {
  who_gs_value2centile(y = subscap_sf_mm, x = age_days, sex = sex, acronym = "ssfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_tsfa_value2centile <- function(triceps_sf_mm, age_days, sex) {
  who_gs_value2centile(y = triceps_sf_mm, x = age_days, sex = sex, acronym = "tsfa")
}

#' Retrieve LMS values for WHO Child Growth Standards
#'
#' @param x X value(s) at which to retrieve LMS values. Must be within bounds of
#' available x values for given acronym.
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param acronym Valid acronym for WHO Growth Standards datasets: one of
#' `"wfa"` (weight-for-age), `"bfa"` (BMI-for-age), `"lhfa"`
#' (length/height-for-age), `"wfl"` (weight-for-length), or `"wfh"`
#' (weight-for-height), `"hcfa"` (head circumference-for-age), `"acfa"`
#' (arm circumference-for-age), `"ssfa"` (subscapular skinfold-for-age), or
#' `"tsfa"` (triceps skinfold-for-age).
#' @return A dataframe with lambda, mu and sigma values for the WHO standard(s)
#' specified by the `acronym` parameter, for the supplied `x` and `sex` values.
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

#' Get values which are a specific z-score from the median using WHO LMS
#' coefficients
#'
#' @param l Lambda value as provided by [who_gs_lms()]
#' @param m Mu value as provided by [who_gs_lms()]
#' @param s Sigma value as provided by [who_gs_lms()]
#' @param n_sd Number of standard deviations from the median at which to compute
#'   a value.
#' @inherit who_gs_lms references
#' @return Numeric vector containing value(s) which are `n_sd` from the median
#'   for each inputted `l`/`m`/`s` combination.
#' @noRd
who_gs_lms2value <- function(l, m, s, n_sd) {
  m * (1 + l * s * n_sd)^(1 / l)
}