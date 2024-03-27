#' Convert z-scores/centiles to values in the INTERGROWTH-21<sup>st</sup>
#' Postnatal Growth Standards for preterm infants
#'
#' @param x,pma_weeks,length_cm Numeric vector of length one or more with x
#'   values. Elements of `x` or its standard-specific equivalents (`pma_weeks`,
#'   `length_cm`) should have specific units and be between certain values
#'   depending on the standard in use (defined by `acronym`). These are:
#'   * Between 27 and 64 weeks for `"wfa"`, `"lfa"`, and `"hcfa"`.
#'   * Between 35 and 65 cm for `"wfl"`.
#'
#'   By default, gigs will replace out-of-bounds elements in `x` with `NA` and
#'   warn you. You can customise this behaviour using the [GIGS package-level
#'   options][gigs_options].
#' @param acronym Single-length character variable denoting the
#'   INTERGROWTH-21<sup>st</sup> Postnatal Growth standard(s) in use. Should be
#'   one of:
#'   * `"wfa"` (weight-for-age)
#'   * `"lfa"` (length-for-age)
#'   * `"hcfa"` (head circumference-for-age)
#'   * `"wfl"` (weight-for-length)
#'
#'   This argument is case-sensitive. If `acronym` is not one of the values
#'   listed above the function will throw an error.
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
#' ig_png_centile2value(p = c(0.25, 0.5, 0.75),
#'                      x = 54:56,
#'                      sex = c("M", "F", "M"),
#'                      acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Or z-scores to values
#' ig_png_zscore2value(z = qnorm(c(0.25, 0.5, 0.75)),
#'                     x = 54:56,
#'                     sex = c("M", "F", "M"),
#'                     acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' ig_png_zscore2value(z = qnorm(c(0.25, 0.5, 0.75)),
#'                     x = 54:56,
#'                     sex = c("M", "F", "M"),
#'                     acronym = "lfa") |>
#'   round(digits = 2)
#'
#' # ... or by using a standard-specific function
#' ig_png_lfa_zscore2value(z = qnorm(c(0.25, 0.5, 0.75)),
#'                         pma_weeks = 54:56,
#'                         sex = c("M", "F", "M")) |>
#'   round(digits = 2)
#'
#' # Inputs are recycled to the input of the longest length
#' ig_png_lfa_zscore2value(z = seq(-0.5, 0.5, by = 0.2),
#'                         pma_weeks = 40,
#'                         sex = "M") |>
#'   round(digits = 2)
#'
#' # Bad inputs will not stop the function but will instead produce `NA`s in the
#' # output - by default gigs will issue useful warnings
#' ig_png_hcfa_zscore2value(z = 0,
#'                          pma_weeks = c(25, 27, 46, 64),
#'                          sex = c("M", "M", NA, "M")) |>
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
#' ig_png_value2centile(y = c(5.79, 5.92, 7.25),
#'                      x = 54:56,
#'                      sex = c("M", "F", "M"),
#'                      acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Or values to z-scores
#' ig_png_value2zscore(y = c(5.79, 5.92, 7.25),
#'                     x = 54:56,
#'                     sex = c("M", "F", "M"),
#'                     acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' ig_png_value2zscore(y = c(60.2, 60.3, 64.1),
#'                     x = 54:56,
#'                     sex = c("M", "F", "M"),
#'                     acronym = "lfa") |>
#'   round(digits = 2)
#'
#' # ... or by using a standard-specific function
#' ig_png_lfa_value2zscore(length_cm = c(60.2, 60.3, 64.1),
#'                         pma_weeks = 54:56,
#'                         sex = c("M", "F", "M")) |>
#'   round(digits = 2)
#'
#' # Inputs are recycled to the input of the longest length
#' ig_png_lfa_value2zscore(length_cm = c(51.10, 51.48, 51.86, 52.24, 52.63),
#'                         pma_weeks = 40,
#'                         sex = "M") |>
#'   round(digits = 2)
#'
#' # Bad inputs will not stop the function but will instead produce `NA`s in the
#' # output - by default gigs will issue useful warnings
#' ig_png_hcfa_value2centile(headcirc_cm = c(20.6, NA, 38.2, 42.8),
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
  is_logarithmic <- acronym == "wfa" || acronym == "lfa"
  y <- with(png_coeffs, mu_sigma_z2y(z = z, mu = mu, sigma = sigma))
  if (is_logarithmic) exp(y) else y
}

#' Convert values to z-scores in the INTERGROWTH-21<sup>st</sup> Postnatal
#' Growth standards
#' @inherit ig_png_zscore2value params return
#' @note This function will fail if given inputs of different lengths.
#' @noRd
ig_png_v2z_internal <- function(y, x, sex, acronym) {
  png_coeffs <- ig_png_equations(x = x, sex = sex, acronym = acronym)
  is_logarithmic <- acronym == "wfa" || acronym == "lfa"
  if (is_logarithmic) {
    y <- log(y)
  }
  with(png_coeffs, mu_sigma_y2z(y = y, mu = mu, sigma = sigma))
}

#' INTERGROWTH-21<sup>st</sup> equations for postnatal size for age in preterm
#' infants
#'
#' Estimates median and standard deviation for different measures of postnatal
#' growth in preterm infants.
#'
#' @param x Numeric vector with length equal to `sex`, with
#'   post-menstrual age(s) in exact weeks. Elements not between `27` and `64`
#'   will return invalid results.
#' @param sex Character vector of length one or more with sex(es), either `"M"`
#'   (male) or `"F"` (female). This argument is case-sensitive.
#' @param acronym A single-length character vector denoting an
#'   INTERGROWTH-21<sup>st</sup> Postnatal Growth standard. Should be one of
#'   `"wfa"` (weight-for-age), `"lfa"` (length-for-age), `"hcfa"` (head
#'   circumference-for-age), or `"wfl"` (weight-for-length).
#' @return A named list of two numeric vectors, where each element corresponds
#'   to the element-wise combination of `x` and `sex`:
#'   * `mu` - mean value at `x` for this `sex`
#'   * `sigma` - standard deviation at `x` for this `sex`
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
  sex_as_numeric <- sex == "M"
  switch(acronym,
         wfa = list(
           mu = 2.591277 - 0.01155 * x^0.5 - 2201.705 * x^(-2) +
             0.0911639 * sex_as_numeric,
           sigma = 0.1470258 + 505.92394 / x^2 - 140.0576 / x^2 * log(x)
         ),
         lfa = list(
           mu = 4.136244 - 547.0018 * x^(-2) + 0.0026066 * x +
             0.0314961 * sex_as_numeric,
           sigma = 0.050489 + 310.44761 * x^(-2) - 90.0742 * x^(-2) * log(x)
         ),
         hcfa = list(
           mu = 55.53617 - 852.0059 * x^(-1) + 0.7957903 * sex_as_numeric,
           sigma = 3.0582292 + 3910.05 * x^(-2) - 180.5625 * x^(-1)
         ),
         wfl = {
           length_dm <- x / 10
           log_length <- log(length_dm)
           length_cubed <- length_dm^3
           list(
             mu = ifelse(
               sex == "M",
               yes = 13.98383 + 203.5677 * length_dm^(-2) - 291.114 *
                 length_dm^(-2) * log_length,
               no = ifelse(
                 sex == "F",
                 yes = 50.32492 + 140.8019 * length_dm^(-1) - 167.906 *
                   length_dm^(-0.5),
                 no = NA_real_
               )),
             sigma = ifelse(
               sex == "M",
               yes = exp(-1.830098 + 0.0049708 * length_cubed),
               no = ifelse(
                 sex == "F",
                 yes = 0.2195888 - 0.0046046 * length_cubed + 0.0033017 *
                   length_cubed * log_length,
                 no = NA_real_
               ))
           )
         })
}

# Parameter validation ---------------------------------------------------------

#' Check user-inputted variables in `ig_png` functions
#'
#' @param y,z,p Either a numeric vector or NULL. Checks will fail if more than
#'   one of these arguments are provided; see [validate_yzp()] documentation.
#' @param x Numeric vector with user-inputted x values.
#' @param sex Character vector of length one or more with user-inputted sex
#'   values. Values which are not `"M"` or `"F"` will be replaced with `NA`.
#' @param acronym A single-length character vector with user-inputted
#'   acronym value. An error will be thrown if `acronym` is not in
#'   `names(gigs::ig_png)`.
#' @param y_name,x_name Single-length character vectors with standard-specific
#'   names for `y` and `x`. If `NULL`, error messages will print out that there
#'   are issues with `'y'` and `'x'`, instead of standard-specific variables
#'   like `headcirc_cm` or `pma_weeks`.
#' @returns List with names `"x"`, `"sex"`, and `"acronym"`, containing
#'   vectors where invalid `sex` or `acronym` elements have been replaced with
#'   NA. If any of `x`, `sex`, or `acronym` are the wrong type or length, an
#'   error will be thrown. Will also contain one of `"y"`, `"z"`, or `"p"`,
#'   depending on which was provided to the function.
#' @noRd
validate_ig_png <- function(y = NULL,
                            z = NULL,
                            p = NULL,
                            x,
                            sex,
                            acronym,
                            y_name = NULL,
                            x_name = NULL) {
  validate_parameter_lengths(y = y, z = z, p = p, x = x, sex = sex,
                             acronym = acronym, y_name = y_name,
                             x_name = x_name)
  catch_and_throw_validate_issues({
    yzp <- validate_yzp(y = y, z = z, p = p, y_name = y_name)
    standard <- "ig_png"
    acronym <- validate_acronym(acronym, names(gigs::ig_png), standard)
    x <- validate_xvar(x, acronym, standard, x_name)
    sex <- validate_sex(sex)
  }, call = rlang::caller_env())
  recycled <- vctrs::vec_recycle_common(
    y = yzp[[1]], z = yzp[[2]], p = yzp[[3]], x = x, sex = sex
  )
  recycled[["acronym"]] <- acronym
  vctrs::list_drop_empty(recycled)
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
