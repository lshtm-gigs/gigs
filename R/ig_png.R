#' Convert z-scores/percentiles to values in the INTERGROWTH-21<sup>st</sup>
#' Postnatal Growth Standards for preterm infants
#'
#' @param z,p Z-score(s)/percentiles to convert to a value/values.
#' @param x,pma_weeks,length_cm The `x` value at which to convert a
#' value to a z-score/percentile. Must be within bounds of available `x` values
#' for given acronym. The standard-specific versions of each function specify
#' `x`, either as `pma_weeks` (post-menstrual age in exact weeks; between `27`
#' and `64` weeks), or `length_cm` (recumbent length measurement(s) in cm;
#' between `35` and `65` cm).
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param acronym Acronym(s) denoting an INTERGROWTH-21<sup>st</sup> standard
#' for postnatal growth in preterm infants. Should be one of `"wfa"`
#' (weight-for-age), `"lfa"` (length-for-age), `"hcfa"`
#' (head circumference-for-age), or `"wfl"` (weight-for-length).
#' @note Input vectors will be recycled to the length of the longest vector
#' according to the rules of `vctrs::vec_recycle_common()`.
#' @return Expected measurements for each combination of z-score/percentile,
#' x variable, sex, and acronym provided to the function.
#' @references
#' Villar J, Giuliani F, Bhutta ZA, Bertino E, Ohuma EO, Ismail LC et al.
#' **Postnatal growth standards for preterm infants: the Preterm Postnatal
#' Follow-up Study of the INTERGROWTH-21st Project.** *Lancet Glob Health* 2015,
#' *3(11):e681-e691.* \doi{10.1016/S2214-109X(15)00163-1}
#' @examples
#' # Convert percentiles to values
#' p <- 0.25 # 25th percentile
#' ig_png_percentile2value(p = p, x = 55, sex = "M", acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Or z-scores to values
#' z <- qnorm(p)
#' ig_png_zscore2value(z = z, x = 55, sex = "M", acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' ig_png_zscore2value(z = z, x = 55, sex = "M", acronym = "lfa") |>
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
#' # Bad inputs will not stop the function but will instead return NA - here 25
#' # weeks post-menstrual age is outside the bounds of the INTERGROWTH-21st
#' # postnatal growth standards for preterm infants
#' ig_png_hcfa_zscore2value(z = z,
#'                          pma_weeks = c(25, 27, 46, 64),
#'                          sex = "M") |>
#'   round(digits = 2)
#' @importFrom vctrs vec_recycle_common
#' @rdname ig_png_zscore2value
#' @export
ig_png_zscore2value <- function(z, x, sex, acronym) {
  max_len_vecs <- vctrs::vec_recycle_common(z = z,
                                            x = x,
                                            sex = sex,
                                            acronym = acronym)

  df <- cbind(z, ig_png_equations(x = max_len_vecs$x,
                                  sex = max_len_vecs$sex,
                                  acronym = max_len_vecs$acronym))

  ifelse(
    test = max_len_vecs$sex == "U",
    yes = mean(c(ig_png_zscore2value(z = df$z, x = df$x,
                                     sex = "M", acronym = df$acronym),
                 ig_png_zscore2value(z = df$z, x = df$x,
                                     sex = "F", acronym = df$acronym))),
    no = ifelse(test = df$logarithmic,
                yes = exp(df$median + z * df$stddev),
                no = df$median + z * df$stddev
    ))
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_wfa_zscore2value <- function(z, pma_weeks, sex) {
  ig_png_zscore2value(z = z, x = pma_weeks, sex = sex, acronym = "wfa")
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_lfa_zscore2value <- function(z, pma_weeks, sex) {
  ig_png_zscore2value(z = z, x = pma_weeks, sex = sex, acronym = "lfa")
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_hcfa_zscore2value <- function(z, pma_weeks, sex) {
  ig_png_zscore2value(z = z, x = pma_weeks, sex = sex, acronym = "hcfa")
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_wfl_zscore2value <- function(z, length_cm, sex) {
  ig_png_zscore2value(z = z, x = length_cm, sex = sex, acronym = "wfl")
}

#' @rdname ig_png_zscore2value
#' @importFrom stats qnorm
#' @export
ig_png_percentile2value <- function(p, x, sex, acronym) {
  ig_png_zscore2value(z = qnorm(p), x = x, sex = sex, acronym = acronym)
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_wfa_percentile2value <- function(p, pma_weeks, sex) {
  ig_png_percentile2value(p = p, x = pma_weeks, sex = sex, acronym = "wfa")
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_lfa_percentile2value <- function(p, pma_weeks, sex) {
  ig_png_percentile2value(p = p, x = pma_weeks, sex = sex, acronym = "lfa")
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_hcfa_percentile2value <- function(p, pma_weeks, sex) {
  ig_png_percentile2value(p = p, x = pma_weeks, sex = sex, acronym = "hcfa")
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_wfl_percentile2value <- function(p, length_cm, sex) {
  ig_png_percentile2value(p = p, x = length_cm, sex = sex, acronym = "wfl")
}

#' Convert values to z-scores/percentiles in the INTERGROWTH-21<sup>st</sup>
#' Postnatal Growth Standards for preterm infants
#'
#' @param y Value(s) to convert to either a z-score/z-scores or a
#' percentile/percentiles.
#' @param x,pma_weeks The `x` value at which to convert a
#' value to a z-score/percentile. Must be within bounds of available `x` values
#' for given acronym. The standard-specific versions of each function specify
#' `x`, either as `pma_weeks` (post-menstrual age in exact weeks; between `27`
#' and `64` weeks), or `length_cm` (recumbent length measurement(s) in cm;
#' between `35` and `65` cm).
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param acronym Acronym(s) denoting an INTERGROWTH-21<sup>st</sup> Postnatal
#' Growth standard for preterm infants. Should be one of `"wfa"`
#' (weight-for-age), `"lfa"` (length-for-age), `"hcfa"`
#' (head circumference-for-age), or `"wfl"` (weight-for-length).
#' @param weight_kg Weight measurement(s) in kg.
#' @param length_cm Recumbent length measurement(s) in cm. Can be either an `x`
#' variable when using the length-for-age standard (`"lfa"`), or a `y` variable
#' when using the weight-for-length (`"wfl"`) standard.
#' @param headcirc_cm Head circumference measurement(s) in cm.
#' @note Input vectors will be recycled to the length of the longest vector
#' according to the rules of `vctrs::vec_recycle_common()`.
#' @return Z-scores/percentiles for each combination of measurement, x
#' variable, sex, and acronym provided to the function.
#' @references
#' Villar J, Giuliani F, Bhutta ZA, Bertino E, Ohuma EO, Ismail LC et al.
#' **Postnatal growth standards for preterm infants: the Preterm Postnatal
#' Follow-up Study of the INTERGROWTH-21st Project.** *Lancet Glob Health* 2015,
#' *3(11):e681-e691.* \doi{10.1016/S2214-109X(15)00163-1}
#' @examples
#' # Convert values to percentiles
#' ig_png_value2percentile(y = 5.94, x = 55, sex = "M", acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Or values to z-scores
#' ig_png_value2zscore(y = 5.94, x = 55, sex = "M", acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' ig_png_value2zscore(y = 65.1, x = 55, sex = "F", acronym = "lfa") |>
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
#' # Bad inputs will not stop the function but will instead return NA - here 25
#' # weeks post-menstrual age is outside the bounds of the INTERGROWTH-21st
#' # postnatal growth standards for preterm infants
#' ig_png_hcfa_value2percentile(headcirc_cm = c(20.6, 22.5, 38.2, 42.8),
#'                              pma_weeks = c(25, 27, 46, 64),
#'                              sex = "M") |>
#'   round(digits = 2)
#' @importFrom vctrs vec_recycle_common
#' @rdname ig_png_value2zscore
#' @export
ig_png_value2zscore <- function(y, x, sex, acronym) {
  max_len_vecs <- vctrs::vec_recycle_common(y = y, x = x,
                                            sex = sex, acronym = acronym)

  df <- cbind(y, ig_png_equations(x = max_len_vecs$x,
                                  sex = max_len_vecs$sex,
                                  acronym = max_len_vecs$acronym))

  ifelse(
    test = max_len_vecs$sex == "U",
    yes = mean(c(ig_png_value2zscore(y = df$y, x = df$x,
                                     sex = "M", acronym = df$acronym),
                 ig_png_value2zscore(y = df$y, x = df$x,
                                     sex = "F", acronym = df$acronym))),
    no = ifelse(test = df$logarithmic,
                yes = (log(df$y) - df$median) / df$stddev,
                no = (df$y - df$median) /df$stddev
    ))
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_wfa_value2zscore <- function(weight_kg, pma_weeks, sex) {
  ig_png_value2zscore(y = weight_kg, x = pma_weeks, sex = sex, acronym = "wfa")
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_lfa_value2zscore <- function(length_cm, pma_weeks, sex) {
  ig_png_value2zscore(y = length_cm, x = pma_weeks, sex = sex, acronym = "lfa")
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_hcfa_value2zscore <- function(headcirc_cm, pma_weeks, sex) {
  ig_png_value2zscore(y = headcirc_cm, x = pma_weeks, sex = sex, acronym = "hcfa")
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_wfl_value2zscore <- function(weight_kg, length_cm, sex) {
  ig_png_value2zscore(y = weight_kg, x = length_cm, sex = sex, acronym = "wfl")
}

#' @rdname ig_png_value2zscore
#' @importFrom stats pnorm
#' @export
ig_png_value2percentile <- function(y, x, sex, acronym) {
  pnorm(ig_png_value2zscore(y = y, x = x, sex = sex, acronym = acronym))
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_wfa_value2percentile <- function(weight_kg, pma_weeks, sex) {
  ig_png_value2percentile(y = weight_kg, x = pma_weeks, sex = sex, acronym = "wfa")
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_lfa_value2percentile <- function(length_cm, pma_weeks, sex) {
  ig_png_value2percentile(y = length_cm, x = pma_weeks, sex = sex, acronym = "lfa")
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_hcfa_value2percentile <- function(headcirc_cm, pma_weeks, sex) {
  ig_png_value2percentile(y = headcirc_cm, x = pma_weeks, sex = sex, acronym = "hcfa")
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_wfl_value2percentile <- function(weight_kg, length_cm, sex) {
  ig_png_value2percentile(y = weight_kg, x = length_cm, sex = sex, acronym = "wfl")
}

#' INTERGROWTH-21<sup>st</sup> equations for postnatal size for age in preterm
#' infants
#'
#' Estimates median and standard deviation for different measures of postnatal
#' growth in preterm infants.
#'
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param pma_weeks Post-menstrual age(s) in exact weeks. Must be between `27`
#' and `64`.
#' @param acronym Acronym(s) denoting an INTERGROWTH-21<sup>st</sup> standard
#' for postnatal growth in preterm infants. Should be one of `"wfa"`
#' (weight-for-age), `"lfa"` (length-for-age), `"hcfa"`
#' (head circumference-for-age), or `"wfl"` (weight-for-length).
#' @return A table with median(s) and standard deviation(s) for each
#' `age`/`sex`/`acronym` combination provided to the function.
#' @note The weight-for-age and length-for-age standards are logarithmic, so
#' require slightly different treatment to use in z-score conversions. In
#' contrast, head circumference for gestational age returns the median and
#' standard deviation with no logarithm applied. The weight-for-length standard
#' is not within the provided reference, but was instead supplied directly by
#' Dr Eric Ohuma.
#' @references
#' Villar J, Giuliani F, Bhutta ZA, Bertino E, Ohuma EO, Ismail LC et al.
#' **Postnatal growth standards for preterm infants: the Preterm Postnatal
#' Follow-up Study of the INTERGROWTH-21st Project.** *Lancet Glob Health* 2015,
#' *3(11):e681-e691.* \doi{10.1016/S2214-109X(15)00163-1}
#' @rdname ig_png_equations
#' @keywords internal
#' @noRd
ig_png_equations <- function(x, sex, acronym) {
  checked_params <- check_png_params(x = x,
                                     sex = sex,
                                     acronym = acronym)

  wfa_logmedian <- function(pma_weeks, sex) {
    2.591277 - (0.01155 * (pma_weeks ^ 0.5)) -
      (2201.705 * (pma_weeks ^ -2)) + (0.0911639 * sex)
  }
  lfa_logmedian <- function(pma_weeks, sex) {
    4.136244 - (547.0018 * (pma_weeks ^ -2)) +
      0.0026066 * pma_weeks + 0.0314961 * sex
  }
  hcfa_median <- function(pma_weeks, sex) {
    55.53617 - (852.0059 * (pma_weeks ^ -1)) + 0.7957903 * sex
  }
  wfl_median <- function(length_cm, sex) {
    ifelse(
      sex == "M",
      yes = 13.98383 + 203.5677 * (length_cm / 10) ^ -2 - 291.114 *
        ((length_cm / 10)^ -2 * log(length_cm/10)),
      no = ifelse(
        sex == "F",
        yes = 50.32492 + 140.8019 * (length_cm / 10) ^ -1 - 167.906 *
          (length_cm / 10) ^ -0.5,
        no = NA_real_))
  }
  wfa_stddev <- function(pma_weeks) {
    0.1470258 + 505.92394 / pma_weeks ^ 2 -
      140.0576 / (pma_weeks ^ 2) * log(pma_weeks)
  }
  lfa_stddev <- function(pma_weeks) {
    0.050489 + (310.44761 * (pma_weeks ^ -2)) -
      (90.0742 * (pma_weeks ^ -2)) * log(pma_weeks)
  }
  hcfa_stddev <- function(pma_weeks) {
    3.0582292 + (3910.05 * (pma_weeks ^ -2)) - 180.5625 * pma_weeks ^ -1
  }
  wfl_stddev <- function(length_cm, sex) {
    ifelse(
      sex == "M",
      yes = exp(-1.830098 + 0.0049708 * (length_cm / 10) ^ 3),
      no = ifelse(
        sex == "F",
        yes = 0.2195888 - 0.0046046 * (length_cm / 10) ^ 3 + 0.0033017 *
        (length_cm / 10) ^ 3 * log(length_cm / 10),
        no = NA_real_))
  }
  out_df <- data.frame(x = checked_params$x,
                       sex = checked_params$sex,
                       acronym = checked_params$acronym)
  sex_as_numeric <- ifelse(sex == "M", yes = 1, no = 0)
  out_df$median <- ifelse(
    acronym == "wfa",
    yes = wfa_logmedian(out_df$x, sex_as_numeric),
    no = ifelse(acronym == "lfa",
                yes = lfa_logmedian(out_df$x, sex_as_numeric),
                no = ifelse(acronym == "hcfa",
                            hcfa_median(out_df$x, sex_as_numeric),
                            wfl_median(out_df$x, sex))))
  out_df$stddev <- ifelse(
    acronym == "wfa",
    yes = wfa_stddev(out_df$x),
    no = ifelse(acronym == "lfa",
                yes = lfa_stddev(out_df$x),
                no = ifelse(acronym == "hcfa",
                            yes = hcfa_stddev(out_df$x),
                            no = wfl_stddev(out_df$x, sex)))
  )
  out_df$logarithmic <- acronym %in% c("wfa", "lfa")
  invalid_params <- is.na(checked_params$x) |
                      is.na(checked_params$sex) |
                      is.na(checked_params$acronym)
  out_df$median <- ifelse(invalid_params,
                          yes = NA,
                          no = out_df$median)
  out_df
}