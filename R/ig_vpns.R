########################################################################################################################
########################################################################################################################

#' INTERGROWTH-21st regression equations for newborn size in very preterm infants
#'
#' Calculates median/standard deviation values for newborn size in very preterm infants with varying gestational ages
#' and sexes.
#'
#' @param gest_age Gestational age in weeks. Must be between `24` and `32 + 6/7` weeks.
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param acronym Acronym(s) denoting the INTERGROWTH-21st VPNS standard to use. Must be one of `"wfga"`,
#' `"lfga"`, or `"hcfga"`.
#' @returns A dataframe with mean and standard deviation values for each provided sex/gestational age/acronym
#' combination
#'
#' @note This function returns the **natural log** of the median and standard deviations for weight (kg) for
#' gestational age. In contrast, the medians and standard deviations for length and head circumference for gestational
#' age have no logarithm applied. Though these functions are not given in the supplied reference, they were provided
#' directly by Dr Eric Ohuma, and replicate the INTERGROWTH-21st standards from the reference at all relevant
#' gestational ages.
#'
#' @references
#' Villar J, Giuliani F, Fenton TR, Ohuma EO, Ismail LC, Kennedy SH et al. **INTERGROWTH-21st very
#' preterm size at birth reference charts.** *Lancet* 2016, **387(10021):844-45.**
#' \doi{10.1016/S0140-6736(16)00384-6}
#'
#' @rdname ig_vpns_equations
#' @keywords internal
ig_vpns_equations <- function(gest_age, sex, acronym) {
  checked_params <- check_nbs_params(gest_age = gest_age, sex = sex, acronym = acronym)
  checked_params$age[checked_params$age >= 38] <- NA
   wfga_logmedian <- function(gest_age, sex) {
    -7.00303 + 1.325911 * gest_age ^ 0.5 + 0.0571937 * sex
  }
  lfga_median <- function(gest_age, sex) {
    1.307633 + 1.270022 * gest_age +  0.4263885 * sex
  }
  hcfga_median <- function(gest_age, sex) {
    0.7866522 + 0.887638 * gest_age + 0.2513385 * sex
  }
  wfga_stddev <- sqrt(x = 0.0373218)
  lfga_stddev <- sqrt(x = 6.757543)
  hcfga_stddev <- sqrt(x = 2.433481)

  out_df <- data.frame(gest_age = checked_params$age,
                       sex = checked_params$sex,
                       acronym = checked_params$acronym)
  sex_as_numeric <- ifelse(sex == "M", yes = 1, no = 0)
  out_df$median <- ifelse(acronym == "wfga",
                          yes = wfga_logmedian(gest_age, sex_as_numeric),
                          no = ifelse(acronym == "lfga",
                                      yes = lfga_median(gest_age, sex_as_numeric),
                                      no = hcfga_median(gest_age, sex_as_numeric)))
  out_df$stddev <- ifelse(acronym == "wfga",
                          yes = wfga_stddev,
                          no = ifelse(acronym == "lfga", yes = lfga_stddev, no = hcfga_stddev))
  out_df$logarithmic <- ifelse(acronym == "wfga", yes = T, no = ifelse(acronym == "lfga", yes = F, no = F))
  out_df$median <- ifelse(is.na(checked_params$age) | is.na( checked_params$sex) | is.na(checked_params$acronym),
                          yes = NA,
                          no = out_df$median)
  return(out_df)
}

########################################################################################################################
########################################################################################################################

#' Convert z-score/percentiles to INTERGROWTH-21st very preterm newborn size values
#'
#' @param z Z-score(s) to convert to a value/values.
#' @param gest_age Gestational age in weeks. Must be between `24` and `32 + 6/7` weeks.
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param acronym Acronym(s) denoting the INTERGROWTH-21st VPNS standard to use. Must be one of `"wfga"`,
#' `"lfga"`, or `"hcfga"`.
#'
#' @references
#' Villar J, Giuliani F, Fenton TR, Ohuma EO, Ismail LC, Kennedy SH et al. **INTERGROWTH-21st very
#' preterm size at birth reference charts.** *Lancet* 2016, **387(10021):844-45.**
#' \doi{10.1016/S0140-6736(16)00384-6}
#'
#' @rdname ig_vpns_zscore2value
#' @keywords internal
ig_vpns_zscore2value <- function(z, gest_age, sex, acronym) {
  max_len_vecs <- rep_to_longest(list(z = z, gest_age = gest_age, sex = sex, acronym = acronym))

  df <- cbind(z, ig_vpns_equations(gest_age = max_len_vecs$gest_age, sex = max_len_vecs$sex, acronym = max_len_vecs$acronym))


  ifelse(
    test = max_len_vecs$sex == "U",
    yes = mean_if_sex_undefined(fn = ig_vpns_zscore2value, arg1 = df$z, x_arg = df$gest_age, acronym = df$acronym),
    no = ifelse(
      test = df$acronym == "wfga",
      yes = exp(df$median + z * df$stddev),
      no = df$median + z * df$stddev
    )
  )
}

#' Convert anthropometric values to INTERGROWTH-21st very preterm newborn size z-score/percentiles
#'
#' @param y Value(s) to convert to z-scores/percentiles.
#' @param gest_age Gestational age in weeks. Must be between `24` and `32 + 6/7` weeks.
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param acronym Acronym(s) denoting the INTERGROWTH-21st VPNS standard to use. Must be one of `"wfga"`,
#' `"lfga"`, or `"hcfga"`.
#'
#' @references
#' Villar J, Giuliani F, Fenton TR, Ohuma EO, Ismail LC, Kennedy SH et al. **INTERGROWTH-21st very
#' preterm size at birth reference charts.** *Lancet* 2016, **387(10021):844-45.**
#' \doi{10.1016/S0140-6736(16)00384-6}
#'
#' @rdname ig_vpns_value2zscore
#' @keywords internal
ig_vpns_value2zscore <- function(y, gest_age, sex, acronym) {
  max_len_vecs <- rep_to_longest(list(y = y, gest_age = gest_age, sex = sex, acronym = acronym))

  df <- cbind(y, ig_vpns_equations(gest_age = max_len_vecs$gest_age, sex = max_len_vecs$sex, acronym = max_len_vecs$acronym))

  ifelse(
    test = max_len_vecs$sex == "U",
    yes = mean_if_sex_undefined(fn = ig_vpns_value2zscore, arg1 = df$y, x_arg = df$gest_age, acronym = df$acronym),
    no = ifelse(
      test = df$acronym == "wfga",
      yes = (log(y) - df$median) / df$stddev,
      no = (y - df$median) / df$stddev
    )
  )
}

########################################################################################################################
########################################################################################################################
