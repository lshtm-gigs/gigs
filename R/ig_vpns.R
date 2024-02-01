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
#' @param acronym Character vector of length one or more with acronym(s)
#'   denoting the INTERGROWTH-21<sup>st</sup> VPNS standard to use. Each element
#'   should be one of `"wfga"`, `"lfga"`, or `"hcfga"`.
#' @return A data frame with mean and standard deviation values for each
#' provided combination of sex, gestational age, and acronym.
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
  gest_days[gest_days >= 231] <- NA
  wfga_logmu <- function(ga_weeks, sex) {
    -7.00303 + 1.325911 * ga_weeks^0.5 + 0.0571937 * sex
  }
  lfga_mu <- function(ga_weeks, sex) {
    1.307633 + 1.270022 * ga_weeks +  0.4263885 * sex
  }
  hcfga_mu <- function(ga_weeks, sex) {
    0.7866522 + 0.887638 * ga_weeks + 0.2513385 * sex
  }
  wfga_sigma <- sqrt(x = 0.0373218)
  lfga_sigma <- sqrt(x = 6.757543)
  hcfga_sigma <- sqrt(x = 2.433481)

  incomplete <- !complete.cases(gest_days, sex, acronym)
  gest_days[incomplete] <- NA
  sex[incomplete] <- NA
  acronym[incomplete] <- NA

  sex_as_numeric <- ifelse(sex == "M", yes = 1, no = 0)
  gest_weeks <- gest_days / 7
  out_len <- length(gest_weeks)
  out <- list(mu = numeric(length = out_len),
              sigma = numeric(length = out_len))
  out[["mu"]] <- ifelse(acronym == "wfga",
                        yes = wfga_logmu(gest_weeks, sex_as_numeric),
                        no = ifelse(acronym == "lfga",
                                    yes = lfga_mu(gest_weeks, sex_as_numeric),
                                    no = hcfga_mu(gest_weeks, sex_as_numeric)))
  out[["sigma"]] <- ifelse(acronym == "wfga",
                           yes = wfga_sigma,
                           no = ifelse(acronym == "lfga",
                                       yes = lfga_sigma,
                                       no = hcfga_sigma))
  out[["logarithmic"]] <- acronym == "wfga"
  out
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
#' @param acronym Character vector of length one or more with acronym(s)
#'   denoting the INTERGROWTH-21<sup>st</sup> VPNS standard to use. Each element
#'   should be one of `"wfga"`, `"lfga"`, or `"hcfga"`.
#' @returns Numeric vector the same length as `z` with expected measurements for
#'   each element of `z`, `gest_days`, `sex`, and `acronym` provided to the
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
  ifelse(test = mu_sigma[["logarithmic"]],
         yes = exp(mu_sigma_z2y(z = z,
                                mu = mu_sigma[["mu"]],
                                sigma = mu_sigma[["sigma"]])),
         no = mu_sigma_z2y(z = z,
                           mu = mu_sigma[["mu"]],
                           sigma = mu_sigma[["sigma"]])
  )
}

#' Convert values to z-scores in the INTERGROWTH-21<sup>st</sup> Newborn Size
#' Standards for Very Preterm Infants
#'
#' @param y Value(s) to convert to z-scores.
#' @param gest_days Numeric vector of length one or more with gestational age(s)
#'   in days. Elements not between `168` and `230` will return invalid output.
#' @param sex Character vector of length one or more with sex(es), either `"M"`
#'   (male) or `"F"` (female). This argument is case-sensitive.
#' @param acronym Character vector of length one or more with acronym(s)
#'   denoting the INTERGROWTH-21<sup>st</sup> VPNS standard to use. Each element
#'   should be one of `"wfga"`, `"lfga"`, or `"hcfga"`.
#' @returns Numeric vector the same length as `y` with z-scores for each element
#'  of `y`, `gest_days`, `sex`, and `acronym` provided to the function.
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
  ifelse(
      test = mu_sigma[["logarithmic"]],
      yes = mu_sigma_y2z(y = log(y),
                         mu = mu_sigma[["mu"]],
                         sigma = mu_sigma[["sigma"]]),
      no = mu_sigma_y2z(y = y,
                        mu = mu_sigma[["mu"]],
                        sigma = mu_sigma[["sigma"]])
  )
}

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.0} Primary literature referenced for each internal function.
#' @srrstats {G1.4a} All functions in file documented using `{roxygen2}`.
NULL
