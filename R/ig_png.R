#' Convert z-score/percentiles to INTERGROWTH-21st postnatal growth standards for preterm infants values
#'
#' @param z,p Z-score(s)/percentile(s) to convert to a value.
#' @param pma_weeks Post-menstrual age(s) in exact weeks. Must be between `27` and `64`.
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param acronym Acronym(s) denoting an INTERGROWTH-21st standard for post-natal growth in preterm infants. Should be
#' one of `"wfa"`, `"lhfa"`, `"hcfa"`.
#'
#' @references
#' Villar J, Giuliani F, Bhutta ZA, Bertino E, Ohuma EO, Ismail LC et al. **Postnatal growth
#' standards for preterm infants: the Preterm Postnatal Follow-up Study of the INTERGROWTH-21st Project.** *Lancet
#' Glob Health* 2015, *3(11):e681-e691.* \doi{10.1016/S2214-109X(15)00163-1}
#'
#' @examples
#' # Convert percentiles to values
#' p <- 0.25 # 25th percentile
#' ig_png_percentile2value(p = p, pma_weeks = 55, sex = "M", acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Or z-scores to values
#' z <- qnorm(p)
#' ig_png_zscore2value(z = z, pma_weeks = 55, sex = "M", acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' ig_png_zscore2value(z = z, pma_weeks = 55, sex = "M", acronym = "lfa") |>
#'   round(digits = 2)
#'
#' # ... or by using a standard-specific function
#' ig_png_lfa_zscore2value(z = z, pma_weeks = 55, sex = "M") |>
#'   round(digits = 2)
#'
#' # Inputs are recycled to the input of the longest length
#' ig_png_lfa_zscore2value(z = seq(0.1, 0.9, by = 0.2),
#'                         pma_weeks = 40,
#'                         sex = "M") |>
#'   round(digits = 2)
#'
#' # Bad inputs will not stop the function but will instead return NA - here 25 weeks post-
#' # menstrual age is outside the bounds of the INTERGROWTH-21st postnatal growth standards
#' # for preterm infants
#' ig_png_hcfa_zscore2value(z = z,
#'                          pma_weeks = c(25, 27, 46, 64),
#'                          sex = "M") |>
#'   round(digits = 2)
#' @importFrom vctrs vec_recycle_common
#' @rdname ig_png_zscore2value
#' @export
ig_png_zscore2value <- function(z, pma_weeks, sex, acronym) {
  max_len_vecs <- vctrs::vec_recycle_common(z = z, pma_weeks = pma_weeks,
                                            sex = sex, acronym = acronym)

  df <- cbind(z, ig_png_equations(pma_weeks = max_len_vecs$pma_weeks, sex = max_len_vecs$sex, acronym = max_len_vecs$acronym))

  ifelse(
    test = max_len_vecs$sex == "U",
    yes = mean(c(ig_png_zscore2value(z = df$z, pma_weeks = df$pma_weeks, sex = "M", acronym = df$acronym),
                 ig_png_zscore2value(z = df$z, pma_weeks = df$pma_weeks, sex = "F", acronym = df$acronym))),
    no = ifelse(
      test = df$acronym != "hcfa",
      yes = exp(df$median + z * df$stddev),
      no = df$median + z * df$stddev
    ))
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_wfa_zscore2value <- function(z, pma_weeks, sex) {
  ig_png_zscore2value(z = z, pma_weeks = pma_weeks, sex = sex, acronym = "wfa")
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_lfa_zscore2value <- function(z, pma_weeks, sex) {
  ig_png_zscore2value(z = z, pma_weeks = pma_weeks, sex = sex, acronym = "lfa")
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_hcfa_zscore2value <- function(z, pma_weeks, sex) {
  ig_png_zscore2value(z = z, pma_weeks = pma_weeks, sex = sex, acronym = "hcfa")
}

#' @rdname ig_png_zscore2value
#' @importFrom stats qnorm
#' @export
ig_png_percentile2value <- function(p, pma_weeks, sex, acronym) {
  ig_png_zscore2value(z = qnorm(p), pma_weeks = pma_weeks, sex = sex, acronym = acronym)
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_wfa_percentile2value <- function(p, pma_weeks, sex) {
  ig_png_percentile2value(p = p, pma_weeks = pma_weeks, sex = sex, acronym = "wfa")
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_lfa_percentile2value <- function(p, pma_weeks, sex) {
  ig_png_percentile2value(p = p, pma_weeks = pma_weeks, sex = sex, acronym = "lfa")
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_hcfa_percentile2value <- function(p, pma_weeks, sex) {
  ig_png_percentile2value(p = p, pma_weeks = pma_weeks, sex = sex, acronym = "hcfa")
}

#' Convert postnatal growth measures to INTERGROWTH-21st postnatal growth standards
#'
#' @param y Value of the anthropometric measure.
#' @param pma_weeks Post-menstrual age in exact weeks. Must be between 27 and 64.
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param acronym Acronym(s) denoting an INTERGROWTH-21st standard for post-natal growth in preterm infants. Should be
#' one of `"wfa"`, `"lhfa"`, `"hcfa"`.
#' @param weight_kg Weight measurement(s) in kg.
#' @param length_cm Recumbent length measurement(s) in cm.
#' @param headcirc_cm Head circumference measurement(s) in cm.
#' @returns Z-score or percentile
#'
#' @references
#' Villar J, Giuliani F, Bhutta ZA, Bertino E, Ohuma EO, Ismail LC et al. **Postnatal growth
#' standards for preterm infants: the Preterm Postnatal Follow-up Study of the INTERGROWTH-21st Project.** *Lancet
#' Glob Health* 2015, *3(11):e681-e691.* \doi{10.1016/S2214-109X(15)00163-1}
#'
#' @examples
#' # Convert values to percentiles
#' ig_png_value2percentile(y = 5.94, pma_weeks = 55, sex = "M", acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Or values to z-scores
#' ig_png_value2zscore(y = 5.94, pma_weeks = 55, sex = "M", acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' ig_png_value2zscore(y = 65.1, pma_weeks = 55, sex = "F", acronym = "lfa") |>
#'   round(digits = 2)
#'
#' # ... or by using a standard-specific function
#' ig_png_lfa_value2zscore(length_cm = 65.1, pma_weeks = 55, sex = "F") |>
#'   round(digits = 2)
#'
#' # Inputs are recycled to the input of the longest length
#' ig_png_lfa_value2zscore(length_cm = c(51.10, 51.48, 51.86, 52.24, 52.63),
#'                         pma_weeks = 40,
#'                         sex = "M") |>
#'   round(digits = 2)
#'
#' # Bad inputs will not stop the function but will instead return NA - here 25 weeks post-
#' # menstrual age is outside the bounds of the INTERGROWTH-21st postnatal growth standards
#' # for preterm infants
#' ig_png_hcfa_value2percentile(headcirc_cm = c(20.6, 22.5, 38.2, 42.8),
#'                              pma_weeks = c(25, 27, 46, 64),
#'                              sex = "M") |>
#'   round(digits = 2)
#' @importFrom vctrs vec_recycle_common
#' @rdname ig_png_value2zscore
#' @export
ig_png_value2zscore <- function(y, pma_weeks, sex, acronym) {
  max_len_vecs <- vctrs::vec_recycle_common(y = y, pma_weeks = pma_weeks,
                                            sex = sex, acronym = acronym)

  df <- cbind(y, ig_png_equations(pma_weeks = max_len_vecs$pma_weeks, sex = max_len_vecs$sex, acronym = max_len_vecs$acronym))

  ifelse(
    test = max_len_vecs$sex == "U",
    yes = mean(c(ig_png_value2zscore(y = df$y, pma_weeks = df$pma_weeks, sex = "M", acronym = df$acronym),
                 ig_png_value2zscore(y = df$y, pma_weeks = df$pma_weeks, sex = "F", acronym = df$acronym))),
    no = ifelse(
      test = df$acronym != "hcfa",
      yes = (log(df$y) - df$median) / df$stddev,
      no = (df$y - df$median) /df$stddev
    ))
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_wfa_value2zscore <- function(weight_kg, pma_weeks, sex) {
  ig_png_value2zscore(y = weight_kg, pma_weeks = pma_weeks, sex = sex, acronym = "wfa")
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_lfa_value2zscore <- function(length_cm, pma_weeks, sex) {
  ig_png_value2zscore(y = length_cm, pma_weeks = pma_weeks, sex = sex, acronym = "lfa")
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_hcfa_value2zscore <- function(headcirc_cm, pma_weeks, sex) {
  ig_png_value2zscore(y = headcirc_cm, pma_weeks = pma_weeks, sex = sex, acronym = "hcfa")
}

#' @rdname ig_png_value2zscore
#' @importFrom stats pnorm
#' @export
ig_png_value2percentile <- function(y, pma_weeks, sex, acronym) {
  pnorm(ig_png_value2zscore(y = y, pma_weeks = pma_weeks, sex = sex, acronym = acronym))
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_wfa_value2percentile <- function(weight_kg, pma_weeks, sex) {
  ig_png_value2percentile(y = weight_kg, pma_weeks = pma_weeks, sex = sex, acronym = "wfa")
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_lfa_value2percentile <- function(length_cm, pma_weeks, sex) {
  ig_png_value2percentile(y = length_cm, pma_weeks = pma_weeks, sex = sex, acronym = "lfa")
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_hcfa_value2percentile <- function(headcirc_cm, pma_weeks, sex) {
  ig_png_value2percentile(y = headcirc_cm, pma_weeks = pma_weeks, sex = sex, acronym = "hcfa")
}

#' INTERGROWTH-21st equations for postnatal size for age in preterm infants
#'
#' Estimates median and standard deviation for different measures of postnatal growth in preterm infants.
#'
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param pma_weeks Post-menstrual age(s) in exact weeks. Must be between `27` and `64`.
#' @param acronym Acronym(s) denoting an INTERGROWTH-21st standard for post-natal growth in preterm infants. Should be
#' one of `"wfa"`, `"lhfa"`, `"hcfa"`.
#'
#' @note Weight for age and length for age values are logarithmic, so require slightly different treatment to use in
#' z-score conversions. In contrast, head circumference for gestational age returns the median and standard deviation
#' with no logarithm applied.
#'
#' @references
#' Villar J, Giuliani F, Bhutta ZA, Bertino E, Ohuma EO, Ismail LC et al.
#' **Postnatal growth standards for preterm infants: the Preterm Postnatal
#' Follow-up Study of the INTERGROWTH-21st Project.** *Lancet Glob Health* 2015,
#' *3(11):e681-e691.* \doi{10.1016/S2214-109X(15)00163-1}
#'
#' @rdname ig_png_equations
#' @keywords internal
ig_png_equations <- function(pma_weeks, sex, acronym) {
  checked_params <- check_png_params(pma_weeks = pma_weeks, sex = sex, acronym = acronym)

  wfa_logmedian <- function(pma_weeks, sex) {
    2.591277 - (0.01155 * (pma_weeks ^ 0.5)) - (2201.705 * (pma_weeks ^ -2)) + (0.0911639 * sex)
  }
  lfa_logmedian <- function(pma_weeks, sex) {
    4.136244 - (547.0018 * (pma_weeks ^ -2)) + 0.0026066 * pma_weeks + 0.0314961 * sex
  }
  hcfa_median <- function(pma_weeks, sex) {
    55.53617 - (852.0059 * (pma_weeks ^ -1)) + 0.7957903 * sex
  }
  wfa_stddev <- function(pma_weeks) {
    0.1470258 + 505.92394 / pma_weeks ^ 2 - 140.0576 / (pma_weeks ^ 2) * log(pma_weeks)
  }
  lfa_stddev <- function(pma_weeks) {
    0.050489 + (310.44761 * (pma_weeks ^ -2)) - (90.0742 * (pma_weeks ^ -2)) * log(pma_weeks)
  }
  hcfa_stddev <- function(pma_weeks) {
    3.0582292 + (3910.05 * (pma_weeks ^ -2)) - 180.5625 * pma_weeks ^ -1
  }

  out_df <- data.frame(pma_weeks = checked_params$age,
                       sex = checked_params$sex,
                       acronym = checked_params$acronym)
  sex_as_numeric <- ifelse(sex == "M", yes = 1, no = 0)
  out_df$median <- ifelse(acronym == "wfa",
                          yes = wfa_logmedian(out_df$pma_weeks, sex_as_numeric),
                          no = ifelse(acronym == "lfa",
                                      yes = lfa_logmedian(out_df$pma_weeks, sex_as_numeric),
                                      no = hcfa_median(out_df$pma_weeks, sex_as_numeric)))
  out_df$stddev <- ifelse(acronym == "wfa",
                          yes = wfa_stddev(out_df$pma_weeks),
                          no = ifelse(acronym == "lfa",
                                      yes = lfa_stddev(out_df$pma_weeks),
                                      no = hcfa_stddev(out_df$pma_weeks)))
  out_df$logarithmic <- ifelse(acronym == "wfa", yes = T, no = ifelse(acronym == "lfa", yes = T, no = F))
  out_df$median <- ifelse(is.na(checked_params$age) | is.na(checked_params$sex) | is.na(checked_params$acronym),
                          yes = NA,
                          no = out_df$median)
  return(out_df)
}