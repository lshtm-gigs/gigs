#' Regression equations for the INTERGROWTH-21<sup>st</sup> Newborn Size
#' Standards for Very Preterm Infants
#'
#' Calculates median/standard deviation values for newborn size in very preterm
#' infants with varying gestational ages and sexes.
#'
#' @param gest_days Numeric vector of length one or more with gestational age(s)
#'   in days. Elements not between `168` and `230` will return invalid output.
#' @param sex Character vector of length one or more with sex(es), either `"M"`
#'   (male) or `"F"` (female). This argument is case-sensitive.
#' @param acronym A single string denoting the INTERGROWTH-21<sup>st</sup> VPNS
#'   standard to use. Must be one of `"wfga"`, `"lfga"`, or `"hcfga"` and is
#'   case-sensitive.
#' @return A data frame with mean and standard deviation values for each
#'   elementwise combination of `sex` and `gest_days`.
#' @note This function returns the **natural log** of the median and standard
#' deviations for weight (kg) for gestational age. In contrast, the medians and
#' standard deviations for length and head circumference for gestational age
#' have no logarithm applied. Though these functions are not given in the
#' supplied reference, they were provided directly by Dr Eric Ohuma, and
#' replicate the INTERGROWTH-21<sup>st</sup> standards from the reference at all
#' relevant gestational ages.
#' @references
#' Villar J, Giuliani F, Fenton TR, Ohuma EO, Ismail LC, Kennedy SH et al.
#' **INTERGROWTH-21st very preterm size at birth reference charts.** *Lancet*
#' 2016, **387(10021):844-45.** \doi{10.1016/S0140-6736(16)00384-6}
#' @importFrom stats complete.cases
#' @noRd
ig_vpns_equations <- function(gest_days, sex, acronym) {
  gest_days[gest_days >= 231] <- NA_real_
  ga_weeks <- gest_days / 7
  sex_as_numeric <- ifelse(sex == "M", yes = 1, no = 0)
  switch(acronym,
    wfga = list(
      mu = -7.00303 + 1.325911 * ga_weeks^0.5 + 0.0571937 * sex_as_numeric,
      sigma = sqrt(x = 0.0373218)
    ),
    lfga = list(
      mu = 1.307633 + 1.270022 * ga_weeks +  0.4263885 * sex_as_numeric,
      sigma = sqrt(x = 6.757543)
    ),
    hcfga = list(
      mu = 0.7866522 + 0.887638 * ga_weeks + 0.2513385 * sex_as_numeric,
      sigma = sqrt(x = 2.433481)
    )
  )
}

#' Convert z-scores to values in the INTERGROWTH-21<sup>st</sup> Newborn Size
#' Standards for Very Preterm Infants
#'
#' @param z Numeric vector of length one or more with z-score(s) to convert to a
#'   value/values.
#' @param gest_days Numeric vector of length one or more with gestational age(s)
#'   in days. Elements not between `168` and `230` will return invalid output.
#' @param sex Character vector of length one or more with sex(es), either `"M"`
#'   (male) or `"F"` (female). This argument is case-sensitive.
#' @param acronym Single-length character variable with an acronym
#'   denoting the INTERGROWTH-21<sup>st</sup> VPNS standard to use. Should be
#'   one of `"wfga"`, `"lfga"`, or `"hcfga"`.
#' @returns Numeric vector the same length as `z` with expected measurements for
#'   each element of `z`, `gest_days`, and `sex` provided to the
#'   function.
#' @references
#' Villar J, Giuliani F, Fenton TR, Ohuma EO, Ismail LC, Kennedy SH et al.
#' **INTERGROWTH-21st very preterm size at birth reference charts.** *Lancet*
#' 2016, **387(10021):844-45.** \doi{10.1016/S0140-6736(16)00384-6}
#' @noRd
ig_vpns_zscore2value <- function(z, gest_days, sex, acronym) {
  mu_sigma <- ig_vpns_equations(gest_days = gest_days,
                                sex = sex,
                                acronym = acronym)
  y <- with(mu_sigma, mu_sigma_z2y(z = z, mu = mu, sigma = sigma))
  if (acronym == "wfga") exp(y) else y
}

#' Convert values to z-scores in the INTERGROWTH-21<sup>st</sup> Newborn Size
#' Standards for Very Preterm Infants
#'
#' @param y Value(s) to convert to z-scores.
#' @param gest_days Numeric vector of length one or more with gestational age(s)
#'   in days. Elements not between `168` and `230` will return invalid output.
#' @param sex Character vector of length one or more with sex(es), either `"M"`
#'   (male) or `"F"` (female). This argument is case-sensitive.
#' @param acronym Single-length character variable with an acronym
#'   denoting the INTERGROWTH-21<sup>st</sup> VPNS standard to use. Should be
#'   one of `"wfga"`, `"lfga"`, or `"hcfga"`.
#' @returns Numeric vector the same length as `y` with z-scores for each element
#'  of `y`, `gest_days`, and `sex` provided to the function.
#' @references
#' Villar J, Giuliani F, Fenton TR, Ohuma EO, Ismail LC, Kennedy SH et al.
#' **INTERGROWTH-21st very preterm size at birth reference charts.** *Lancet*
#' 2016, **387(10021):844-45.** \doi{10.1016/S0140-6736(16)00384-6}
#' @srrstats {G1.0} Primary literature referenced here.
#' @noRd
ig_vpns_value2zscore <- function(y, gest_days, sex, acronym) {
  mu_sigma <- ig_vpns_equations(gest_days = gest_days,
                                sex = sex,
                                acronym = acronym)
  if (acronym == "wfga") {
    y <- log(y)
  }
  with(mu_sigma, mu_sigma_y2z(y = y, mu = mu, sigma = sigma))
}

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.0} Primary literature referenced for each internal function.
#' @srrstats {G1.4a} All functions in file documented using `{roxygen2}`.
NULL
