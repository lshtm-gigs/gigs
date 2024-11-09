# INTERNAL: INTERGROWTH-21st Postnatal Growth standards conversion logic -------

#' Convert z-scores to values in the INTERGROWTH-21<sup>st</sup> Postnatal
#' Growth standards
#' @inherit zscore2value params return
#' @note This function will fail if given inputs of different lengths.
#' @noRd
ig_png_z2v_internal <- function(z, x, sex, acronym) {
  png_coeffs <- ig_png_mu_sigma(x = x, sex = sex, acronym = acronym)
  is_logarithmic <- acronym == "wfa" || acronym == "lfa"
  y <- with(png_coeffs, mu_sigma_z2y(z = z, mu = mu, sigma = sigma))
  if (is_logarithmic) exp(y) else y
}

#' Convert values to z-scores in the INTERGROWTH-21<sup>st</sup> Postnatal
#' Growth standards
#' @inherit zscore2value params return
#' @note This function will fail if given inputs of different lengths.
#' @noRd
ig_png_v2z_internal <- function(y, x, sex, acronym) {
  png_coeffs <- ig_png_mu_sigma(x = x, sex = sex, acronym = acronym)
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
#' @noRd
ig_png_mu_sigma <- function(x, sex, acronym) {
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

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.0} Primary literature referenced for internal functions.
#' @srrstats {G1.4, G1.4a} All functions in file documented using `{roxygen2}`.
#' @srrstats {G2.0, G2.1, G2.2, G2.3, G2.3a, G2.6} All internal functions in
#'   this script are provided with vectors that have already been validated.
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.16} All internal functions in
#'   this script are provided with vectors that have already checked for
#' missing/undefined/out-of-bounds data.
NULL
