#' Convert z-scores/centiles to values in the INTERGROWTH-21<sup>st</sup>
#' Postnatal Growth Standards for preterm infants
#'
#' @param x Numeric vector of length one or more with x values. Elements of `x`
#'   or its standard-specific equivalents (`pma_weeks`, `length_cm`) should have
#'   specific units and be between certain values depending on the standard in
#'   use (defined by `acronym`). These are:
#'   * Between 27 and 64 weeks for `"wfa"`, `"lfa"`, and `"hcfa"`.
#'   * Between 35 and 65 days for `"wfl"`.
#'
#'   By default, gigs will replace out-of-bounds elements in `x` with `NA` and
#'   warn you. This behaviour can be customised using the functions in
#'   [gigs_options].
#' @param acronym Character vector of length one or more denoting the
#'   INTERGROWTH-21<sup>st</sup> Postnatal Growth standard(s) in use. Each
#'   element should be one of:
#'   * `"wfa"` (weight-for-age)
#'   * `"lfa"` (length-for-age)
#'   * `"hcfa"` (head circumference-for-age)
#'   * `"wfl"` (weight-for-length)
#'
#'   This argument is case-sensitive. By default, gigs will replace elements in
#'   `acronym` which are not one of the above values with `NA` and warn you.
#'   This behaviour can be customised using the functions in [gigs_options]. If
#'   all elements in `acronym` are not one of the above values, gigs will throw
#'   an error.
#' @param pma_weeks,length_cm Numeric vector of length one or more with
#'   standard-specific `x` variables. See the documentation for `x` for
#'   information the acceptable bounds of these variables, and on how
#'   out-of-bounds elements will be handled.
#' @srrstats {G2.3b} Explicit reference to `acronym` case-sensitivity.
#' @inherit shared_roxygen_params params note
#' @inherit shared_zscore2value_returns return
#' @references
#' Villar J, Giuliani F, Bhutta ZA, Bertino E, Ohuma EO, Ismail LC et al.
#' **Postnatal growth standards for preterm infants: the Preterm Postnatal
#' Follow-up Study of the INTERGROWTH-21st Project.** *Lancet Glob Health* 2015,
#' *3(11):e681-e691.* \doi{10.1016/S2214-109X(15)00163-1}
#' @examples
#' # Convert centiles to values
#' p <- 0.25 # 25th centile
#' ig_png_centile2value(p = p, x = 55, sex = "M", acronym = "wfa") |>
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
#' @rdname ig_png_zscore2value
#' @export
ig_png_zscore2value <- function(z, x, sex, acronym) {
  validate_ig_png(z = z, x = x, sex = sex, acronym = acronym) |>
    do.call(what = ig_png_z2v_internal)
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_wfa_zscore2value <- function(z, pma_weeks, sex) {
  acronym <- "wfa"
  validate_ig_png(z = z,
                  x = pma_weeks,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::ig_png[[acronym]][["x"]]) |>
    do.call(what = ig_png_z2v_internal)
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_lfa_zscore2value <- function(z, pma_weeks, sex) {
  acronym <- "lfa"
  validate_ig_png(z = z,
                  x = pma_weeks,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::ig_png[[acronym]][["x"]]) |>
    do.call(what = ig_png_z2v_internal)
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_hcfa_zscore2value <- function(z, pma_weeks, sex) {
  acronym <- "hcfa"
  validate_ig_png(z = z,
                  x = pma_weeks,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::ig_png[[acronym]][["x"]]) |>
    do.call(what = ig_png_z2v_internal)
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_wfl_zscore2value <- function(z, length_cm, sex) {
  acronym <- "wfl"
  validate_ig_png(z = z,
                  x = length_cm,
                  sex = sex,
                  acronym = acronym,
                  x_name = gigs::ig_png[[acronym]][["x"]]) |>
    do.call(what = ig_png_z2v_internal)
}

#' @rdname ig_png_zscore2value
#' @importFrom stats qnorm
#' @export
ig_png_centile2value <- function(p, x, sex, acronym) {
  validated <- validate_ig_png(p = p, x = x, sex = sex, acronym = acronym)
  with(validated, ig_png_z2v_internal(qnorm(p), x, sex, acronym))
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_wfa_centile2value <- function(p, pma_weeks, sex) {
  acronym <- "wfa"
  validated <- validate_ig_png(p = p,
                               x = pma_weeks,
                               sex = sex,
                               acronym = acronym,
                               x_name = gigs::ig_png[[acronym]][["x"]])
  with(validated, ig_png_z2v_internal(qnorm(p), x, sex, acronym))
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_lfa_centile2value <- function(p, pma_weeks, sex) {
  acronym <- "lfa"
  validated <- validate_ig_png(p = p,
                               x = pma_weeks,
                               sex = sex,
                               acronym = acronym,
                               x_name = gigs::ig_png[[acronym]][["x"]])
  with(validated, ig_png_z2v_internal(qnorm(p), x, sex, acronym))
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_hcfa_centile2value <- function(p, pma_weeks, sex) {
  acronym <- "hcfa"
  validated <- validate_ig_png(p = p,
                               x = pma_weeks,
                               sex = sex,
                               acronym = acronym,
                               x_name = gigs::ig_png[[acronym]][["x"]])
  with(validated, ig_png_z2v_internal(qnorm(p), x, sex, acronym))
}

#' @rdname ig_png_zscore2value
#' @export
ig_png_wfl_centile2value <- function(p, length_cm, sex) {
  acronym <- "wfl"
  validated <- validate_ig_png(p = p,
                               x = length_cm,
                               sex = sex,
                               acronym = acronym,
                               x_name = gigs::ig_png[[acronym]][["x"]])
  with(validated, ig_png_z2v_internal(qnorm(p), x, sex, acronym))
}

#' Convert values to z-scores/centiles in the INTERGROWTH-21<sup>st</sup>
#' Postnatal Growth Standards for preterm infants
#'
#' @param weight_kg Numeric vector of length one or more with weight
#'   measurement(s) in kg.
#' @param pma_weeks Numeric vector of length one or more with post-menstrual
#'   age(s) in weeks. Values should be within the bounds defined in the
#'   documentation for `x`.
#' @param length_cm Numeric vector of length one or more with recumbent length
#'   measurement(s) in cm. This argument can be either an `x` variable when
#'   using the length-for-age standard (`"lfa"`), or a `y` variable when using
#'   the weight-for-length (`"wfl"`) standard.
#' @param headcirc_cm Numeric vector of length one or more with head
#'   circumference measurement(s) in cm.
#' @inherit ig_png_zscore2value params references
#' @inherit shared_roxygen_params params note
#' @inherit shared_value2zscore_returns return
#' @examples
#' # Convert values to centiles
#' ig_png_value2centile(y = 5.94, x = 55, sex = "M", acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Or values to z-scores
#' ig_png_value2zscore(y = 5.94, x = 55, sex = "M", acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' ig_png_value2zscore(y = 65.1, x = 55, sex = "M", acronym = "lfa") |>
#'   round(digits = 2)
#'
#' # ... or by using a standard-specific function
#' ig_png_lfa_value2zscore(length_cm = 65.1, pma_weeks = 55, sex = "M") |>
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
#' ig_png_hcfa_value2centile(headcirc_cm = c(20.6, 22.5, 38.2, 42.8),
#'                           pma_weeks = c(25, 27, 46, 64),
#'                           sex = "M") |>
#'   round(digits = 2)
#' @rdname ig_png_value2zscore
#' @export
ig_png_value2zscore <- function(y, x, sex, acronym) {
  validate_ig_png(y = y, x = x, sex = sex, acronym = acronym) |>
    do.call(what = ig_png_v2z_internal)
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_wfa_value2zscore <- function(weight_kg, pma_weeks, sex) {
  acronym <- "wfa"
  validate_ig_png(y = weight_kg,
                  x = pma_weeks,
                  sex = sex,
                  acronym = acronym,
                  y_name = gigs::ig_png[[acronym]][["y"]],
                  x_name = gigs::ig_png[[acronym]][["x"]]) |>
    do.call(what = ig_png_v2z_internal)
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_lfa_value2zscore <- function(length_cm, pma_weeks, sex) {
  acronym <- "lfa"
  validate_ig_png(y = length_cm,
                  x = pma_weeks,
                  sex = sex,
                  acronym = acronym,
                  y_name = gigs::ig_png[[acronym]][["y"]],
                  x_name = gigs::ig_png[[acronym]][["x"]]) |>
    do.call(what = ig_png_v2z_internal)
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_hcfa_value2zscore <- function(headcirc_cm, pma_weeks, sex) {
  acronym <- "hcfa"
  validate_ig_png(y = headcirc_cm,
                  x = pma_weeks,
                  sex = sex,
                  acronym = acronym,
                  y_name = gigs::ig_png[[acronym]][["y"]],
                  x_name = gigs::ig_png[[acronym]][["x"]]) |>
    do.call(what = ig_png_v2z_internal)
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_wfl_value2zscore <- function(weight_kg, length_cm, sex) {
  acronym <- "wfl"
  validate_ig_png(y = weight_kg,
                  x = length_cm,
                  sex = sex,
                  acronym = acronym,
                  y_name = gigs::ig_png[[acronym]][["y"]],
                  x_name = gigs::ig_png[[acronym]][["x"]]) |>
    do.call(what = ig_png_v2z_internal)
}

#' @rdname ig_png_value2zscore
#' @importFrom stats pnorm
#' @export
ig_png_value2centile <- function(y, x, sex, acronym) {
  validate_ig_png(y = y, x = x, sex = sex, acronym = acronym) |>
    do.call(what = ig_png_v2z_internal) |>
    pnorm()
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_wfa_value2centile <- function(weight_kg, pma_weeks, sex) {
  acronym <- "wfa"
  validate_ig_png(y = weight_kg,
                  x = pma_weeks,
                  sex = sex,
                  acronym = acronym,
                  y_name = gigs::ig_png[[acronym]][["y"]],
                  x_name = gigs::ig_png[[acronym]][["x"]]) |>
    do.call(what = ig_png_v2z_internal) |>
    pnorm()
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_lfa_value2centile <- function(length_cm, pma_weeks, sex) {
  acronym <- "lfa"
  validate_ig_png(y = length_cm,
                  x = pma_weeks,
                  sex = sex,
                  acronym = acronym,
                  y_name = gigs::ig_png[[acronym]][["y"]],
                  x_name = gigs::ig_png[[acronym]][["x"]]) |>
    do.call(what = ig_png_v2z_internal) |>
    pnorm()
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_hcfa_value2centile <- function(headcirc_cm, pma_weeks, sex) {
  acronym <- "hcfa"
  validate_ig_png(y = headcirc_cm,
                  x = pma_weeks,
                  sex = sex,
                  acronym = acronym,
                  y_name = gigs::ig_png[[acronym]][["y"]],
                  x_name = gigs::ig_png[[acronym]][["x"]]) |>
    do.call(what = ig_png_v2z_internal) |>
    pnorm()
}

#' @rdname ig_png_value2zscore
#' @export
ig_png_wfl_value2centile <- function(weight_kg, length_cm, sex) {
  acronym <- "wfl"
  validate_ig_png(y = weight_kg,
                  x = length_cm,
                  sex = sex,
                  acronym = acronym,
                  y_name = gigs::ig_png[[acronym]][["y"]],
                  x_name = gigs::ig_png[[acronym]][["x"]]) |>
    do.call(what = ig_png_v2z_internal) |>
    pnorm()
}

# INTERNAL: INTERGROWTH-21st Postnatal Growth standards conversion logic -------

#' Convert z-scores to values in the INTERGROWTH-21<sup>st</sup> Postnatal
#' Growth standards
#' @inherit ig_png_zscore2value params return
#' @note This function will fail if given inputs of different lengths.
#' @noRd
ig_png_z2v_internal <- function(z, x, sex, acronym) {
  png_coeffs <- ig_png_equations(x = x, sex = sex, acronym = acronym)
  with(png_coeffs,
       ifelse(test = is_logarithmic,
              yes = exp(mu_sigma_z2y(z = z, mu = mu, sigma = sigma)),
              no = mu_sigma_z2y(z = z, mu = mu, sigma = sigma))
  )
}

#' Convert values to z-scores in the INTERGROWTH-21<sup>st</sup> Postnatal
#' Growth standards
#' @inherit ig_png_zscore2value params return
#' @note This function will fail if given inputs of different lengths.
#' @noRd
ig_png_v2z_internal <- function(y, x, sex, acronym) {
  png_coeffs <- ig_png_equations(x = x, sex = sex, acronym = acronym)
  with(png_coeffs,
       mu_sigma_y2z(
         y = ifelse(test = is_logarithmic, yes = log(y), no = y),
         mu = mu, sigma = sigma
       )
  )
}

#' INTERGROWTH-21<sup>st</sup> equations for postnatal size for age in preterm
#' infants
#'
#' Estimates median and standard deviation for different measures of postnatal
#' growth in preterm infants.
#'
#' @param sex Character vector of length one or more with sex(es), either `"M"`
#'   (male) or `"F"` (female). This argument is case-sensitive.
#' @param pma_weeks Numeric vector with length equal to `sex`, with
#'   post-menstrual age(s) in exact weeks. Elements not between `27` and `64`
#'   will return invalid results.
#' @param acronym Character vector of length one or more with acronym(s)
#'   denoting an INTERGROWTH-21<sup>st</sup> Postnatal Growth standard. Should
#'   be one of `"wfa"` (weight-for-age), `"lfa"` (length-for-age), `"hcfa"`
#'   (head circumference-for-age), or `"wfl"` (weight-for-length).
#' @return A data frame with median(s) and standard deviation(s) for each
#'   `age`/`sex`/`acronym` combination provided to the function.
#' @note The weight-for-age and length-for-age standards are logarithmic, so
#'   require slightly different treatment to use in z-score conversions. In
#'   contrast, head circumference for gestational age returns the median and
#'   standard deviation with no logarithm applied. The weight-for-length
#'   standard is not within the provided reference, but was instead supplied
#'   directly by Dr Eric Ohuma.
#' @references
#' Villar J, Giuliani F, Bhutta ZA, Bertino E, Ohuma EO, Ismail LC et al.
#' **Postnatal growth standards for preterm infants: the Preterm Postnatal
#' Follow-up Study of the INTERGROWTH-21st Project.** *Lancet Glob Health* 2015,
#' *3(11):e681-e691.* \doi{10.1016/S2214-109X(15)00163-1}
#' @rdname ig_png_equations
#' @noRd
ig_png_equations <- function(x, sex, acronym) {
  wfa_log_mu <- function(pma_weeks, sex) {
    2.591277 - (0.01155 * (pma_weeks^0.5)) - (2201.705 * (pma_weeks^-2)) +
      (0.0911639 * sex)
  }
  lfa_log_mu <- function(pma_weeks, sex) {
    4.136244 - (547.0018 * (pma_weeks^-2)) + 0.0026066 * pma_weeks +
      0.0314961 * sex
  }
  hcfa_mu <- function(pma_weeks, sex) {
    55.53617 - (852.0059 * (pma_weeks^-1)) + 0.7957903 * sex
  }
  wfl_mu <- function(length_cm, sex) {
    ifelse(
      sex == "M",
      yes = 13.98383 + 203.5677 * (length_cm / 10)^-2 - 291.114 *
        ((length_cm / 10)^ -2 * log(length_cm/10)),
      no = ifelse(
        sex == "F",
        yes = 50.32492 + 140.8019 * (length_cm / 10)^-1 - 167.906 *
          (length_cm / 10)^-0.5,
        no = NA_real_))
  }
  wfa_sigma <- function(pma_weeks) {
    0.1470258 + 505.92394 / pma_weeks^2 -
      140.0576 / (pma_weeks^2) * log(pma_weeks)
  }
  lfa_sigma <- function(pma_weeks) {
    0.050489 + (310.44761 * (pma_weeks^-2)) -
      (90.0742 * (pma_weeks^-2)) * log(pma_weeks)
  }
  hcfa_sigma <- function(pma_weeks) {
    3.0582292 + (3910.05 * (pma_weeks^-2)) - 180.5625 * pma_weeks^-1
  }
  wfl_sigma <- function(length_cm, sex) {
    ifelse(
      sex == "M",
      yes = exp(-1.830098 + 0.0049708 * (length_cm / 10)^3),
      no = ifelse(
        sex == "F",
        yes = 0.2195888 - 0.0046046 * (length_cm / 10)^3 + 0.0033017 *
        (length_cm / 10)^3 * log(length_cm / 10),
        no = NA_real_))
  }
  out_df <- data.frame(x = x, sex = sex, acronym = acronym)
  sex_as_numeric <- ifelse(sex == "M", yes = 1, no = 0)
  out_df[["mu"]] <- ifelse(
    acronym == "wfa",
    yes = wfa_log_mu(out_df[["x"]], sex_as_numeric),
    no = ifelse(acronym == "lfa",
                yes = lfa_log_mu(out_df[["x"]], sex_as_numeric),
                no = ifelse(acronym == "hcfa",
                            hcfa_mu(out_df[["x"]], sex_as_numeric),
                            wfl_mu(out_df[["x"]], sex))))
  out_df[["sigma"]] <- ifelse(
    acronym == "wfa",
    yes = wfa_sigma(out_df[["x"]]),
    no = ifelse(acronym == "lfa",
                yes = lfa_sigma(out_df[["x"]]),
                no = ifelse(acronym == "hcfa",
                            yes = hcfa_sigma(out_df[["x"]]),
                            no = wfl_sigma(out_df[["x"]], sex)))
  )
  out_df[["is_logarithmic"]] <- acronym %in% c("wfa", "lfa")
  invalid_params <- !stats::complete.cases(out_df)
  out_df[["mu"]][invalid_params] <- NA
  out_df
}

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.0} Primary literature referenced for each exported function,
#'   and for internal functions.
#' @srrstats {G1.4, G1.4a} All functions in file documented using `{roxygen2}`.
#' @srrstats {G2.0a, G2.1a, EA1.3} Exported function in this file document
#'   expectations on the length of inputs and their data types.
#' @srrstats {G2.0, G2.1, G2.2, G2.3, G2.3a, G2.6} These standards
#'   are met in all exported functions by passing inputs to [validate_ig_png()].
#'   All internal functions in this script are provided with vectors that have
#'   already been validated.
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.16} These standards are met
#'   in all exported functions by passing inputs to [validate_ig_png()]. All
#'   internal functions in this script are provided with vectors that have
#'   already checked for missing/undefined/out-of-bounds data.
NULL
