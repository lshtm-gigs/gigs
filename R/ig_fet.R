# INTERNAL: INTERGROWTH-21st Fetal standards conversion logic ------------------

#' Convert z-scores to values in the INTERGROWTH-21<sup>st</sup> Fetal standards
#' @inherit zscore2value params return
#' @note This function will fail if given inputs of different lengths.
#' @noRd
ig_fet_z2v_internal <- function(z, x, acronym) {
  if (acronym %in% c("pifga", "rifga", "sdrfga")) {
    ig_fet_doppler_z2y(z = z, gest_days = x, acronym = acronym)
  } else if (acronym %in% c("efwfga", "hefwfga")) {
    ig_fet_lms_z2y(z = z, gest_days = x, acronym = acronym)
  } else if (acronym == "gwgfga") {
    ig_fet_gwg_z2y(z = z, gest_days = x)
  } else {
    ig_fet_mu_sigma_z2y(z = z, x = x, acronym = acronym)
  }
}

#' Convert values to z-scores in the INTERGROWTH-21<sup>st</sup> Fetal standards
#' @inherit zscore2value params return
#' @note This function will fail if given inputs of different lengths.
#' @noRd
ig_fet_v2z_internal <- function(y, x, acronym) {
  if (acronym %in% c("pifga", "rifga", "sdrfga")) {
    ig_fet_doppler_y2z(y = y, gest_days = x, acronym = acronym)
  } else if (acronym %in% c("efwfga", "hefwfga")) {
    ig_fet_lms_y2z(efw_g = y, gest_days = x, acronym = acronym)
  } else if (acronym == "gwgfga") {
    ig_fet_gwg_y2z(gwg_kg = y, gest_days = x)
  } else {
    ig_fet_mu_sigma_y2z(y = y, x = x, acronym = acronym)
  }
}

# INTERNAL: IG-21st Fetal standards: normally distributed ----------------------

#' INTERGROWTH-21<sup>st</sup> equations for normally distributed aspects of
#' fetal growth (internal)
#'
#' Estimates median and standard deviation for different measures of fetal
#' growth.
#'
#' @param x Numeric vector of length one or more with x variable, which should
#'   have the units/values described in the [value2zscore()] documentation.
#' @param acronym A single string denoting an INTERGROWTH-21<sup>st</sup> fetal
#'   standard. Each element should be one of the acronyms described in the
#'   [value2zscore()] documentation.
#' @returns A named list of two elements, `mu` and `sigma`, which are the mean
#'   and standard deviation(s) for each `x`/`acronym` combination
#'   provided to the function, respectively.
#' @references
#' Papageorghiou AT, Ohuma EO, Altman DG, Todros T, Cheikh Ismail L, Lambert A
#' et al. **International standards for fetal growth based on serial ultrasound
#' measurements: the Fetal Growth Longitudinal Study of the INTERGROWTH-21st
#' Project.** *Lancet* 2014, **384(9946):869-79.**
#' \doi{10.1016/S0140-6736(14)61490-2}
#'
#' Papageorghiou AT, Ohuma EO, Gravett MG, Lamber A, Noble JA, Pang R et al.
#' **International standards for symphysis-fundal height based on serial
#' measurements from the Fetal Growth Longitudinal Study of the INTERGROWTH-21st
#' Project: prospective cohort study in eight countries.** BMJ 2016,
#' **355:i5662** \doi{10.1136/bmj.i5662}
#'
#' Papageorghiou AT, Kennedy SH, Salomon LJ, Ohuma EO, Cheikh Ismail L, Barros
#' FC et al. **International standards for early fetal size and pregnancy dating
#' based on ultrasound measurement of crown-rump length in the first trimester
#' of pregnancy.** *Ultrasound Obstet Gynecol* 2014, **44(6):641-48**
#' \doi{10.1002/uog.13448}
#'
#' Napolitano R, Molloholli M, Donadono V, Ohuma EO, Wanyonyi SZ, Kemp B et al.
#' **International standards for fetal brain structures based on serial
#' ultrasound measurements from Fetal Growth Longitudinal Study of
#' INTERGROWTH-21st Project** *Ultrasound Obstet Gynecol* 2020,
#' **56(3):359-370** \doi{10.1002/uog.21990}
#' @rdname ig_fet_equations
#' @noRd
ig_fet_mu_sigma <- function(x, acronym) {
  x <- if (acronym %in% c("crlfga", "gafcrl", "gaftcd")) x else x / 7
  ga_wks <- x
  ga_days <- x
  tcd_mm <- x
  crl_mm <- x
  switch(
    acronym,
    hcfga = {
      log_ga <- log(ga_wks)
      list(
        mu = -28.2849 + 1.69267 * ga_wks^2 - 0.397485 * ga_wks^2 * log_ga,
        sigma = 1.98735 + 0.0136772 * ga_wks^3 - 0.00726264 * ga_wks^3 *
          log_ga + 0.000976253 * ga_wks^3 * log_ga^2
      )
    },
    bpdfga = {
      log_ga <- log(ga_wks)
      list(
        mu = 5.60878 + 0.158369 * ga_wks^2 - 0.00256379 * ga_wks^3,
        sigma = exp(
          0.101242 + 0.00150557 * ga_wks^3 - 0.000771535 * ga_wks^3 * log_ga +
            0.0000999638 * ga_wks^3 * log_ga^2
        )
      )
    },
    acfga = list(
      mu = -81.3243 + 11.6772 * ga_wks - 0.000561865 * ga_wks^3,
      sigma = -4.36302 + 0.121445 * ga_wks^2 - 0.0130256 * ga_wks^3 +
        0.00282143 * ga_wks^3 * log(ga_wks)
    ),
    flfga = list(
       mu = -39.9616 + 4.32298 * ga_wks - 0.0380156 * ga_wks^2,
       sigma = exp(0.605843 - 42.0014 * ga_wks^-2 + 0.00000917972 * ga_wks^3)
     ),
    ofdfga = {
      log_ga <- log(ga_wks)
      list(
        mu = -12.4097 + 0.626342 * ga_wks^2 - 0.148075 * ga_wks^2 * log_ga,
        sigma = exp(
          -0.880034 + 0.0631165 * ga_wks^2 - 0.0317136 * ga_wks^2 * log_ga +
            0.00408302 * ga_wks^2 * log_ga^2
        )
      )
    },
    sfhfga = list(
      mu = 5.133374 + 0.1058353119 * ga_wks^2 - 0.0231295 *
        ga_wks^2 * log(ga_wks),
      sigma = 0.9922667 + 0.0258087 * ga_wks
    ),
    crlfga = list(
      mu = -50.6562 + (0.815118 * ga_days) + (0.00535302 * (ga_days)^2),
      sigma = -2.21626 + (0.0984894 * ga_days)
    ),
    gafcrl = list(
      mu = 40.9041 + 3.21585 * crl_mm^0.5 + 0.348956 * crl_mm,
      sigma = 2.39102 + 0.0193474 * crl_mm
    ),
    tcdfga = list(
      mu = -13.10907 + 20.8941 * (ga_wks/10)^0.5 + 0.5035914 * (ga_wks/10)^3,
      sigma = 0.4719837 + 0.0500382 * (ga_wks / 10)^3
    ),
    gaftcd = {
      tcd_cm <- tcd_mm / 10
      list(
        # Use 7 * outputs to get GA in days, as eqns give GA in weeks
        mu = 7 * (3.957113 + 8.154074 * tcd_cm - 0.076187 * tcd_cm^3),
        sigma = 7 * (1.577198 - 1.30374 * tcd_cm^(-0.5))
      )
    },
    poffga = list(
      mu = 10.29428 - 122.8447 * ga_wks^(-1) + 0.00001038 * ga_wks^3,
      sigma = 1.596042 - 257.2297 * ga_wks^(-2)
    ),
    sffga = list(
      mu = 80.27012 - 32.7877 * ga_wks^(-0.5) -
        100.1593 * ga_wks^(-0.5) * log(ga_wks),
      sigma = 2.304501 - 353.814 * ga_wks^(-2)
    ),
    avfga = list(
      mu = 6.396214 + (0.00006205 * ga_wks^3),
      sigma = 1.204454
    ),
    pvfga = list(
      mu = 4.389214 + 38.10015 * ga_wks^(-1) + 0.0000020063 * ga_wks^3,
      sigma = 0.6707227 + (0.034258 * ga_wks)
    ),
    cmfga = list(
      mu = 2.098095 - 239.0659 * ga_wks^(-2) - 0.0000001547 * ga_wks^3,
      sigma = 0.2297936 + 8.1872 * ga_wks^(-2)
    )
  )
}

#' Convert values to z-scores for the INTERGROWTH-21st Fetal standards based on
#' simple mu-sigma models (internal; covers most Fetal standards)
#' @param y Numeric vector of length one or more with measurements.
#' @inheritParams ig_fet_mu_sigma
#' @return Numeric vector of z-scores, with length equal to `length(y)`.
#' @noRd
ig_fet_mu_sigma_y2z <- function(y, x, acronym) {
  mu_sigma <- ig_fet_mu_sigma(x = x, acronym = acronym)
  if (acronym == "cmfga") {
    y <- log(y)
  }
  with(mu_sigma, mu_sigma_y2z(y, mu, sigma))
}

#' Convert z-scores to values for the INTERGROWTH-21st Fetal standards based on
#' simple mu-sigma models (internal; covers most Fetal standards)
#' @param z Numeric vector of length one or more with z-scores.
#' @inheritParams ig_fet_mu_sigma
#' @returns Numeric vector of expected measurements, with length equal to
#'   `length(z)`.
#' @noRd
ig_fet_mu_sigma_z2y <- function(z, x, acronym) {
  mu_sigma <- ig_fet_mu_sigma(x = x, acronym = acronym)
  y <- with(mu_sigma, mu_sigma_z2y(z, mu, sigma))
  if (acronym == "cmfga") exp(y) else y
}

# INTERNAL: Estimated fetal weight functions -----------------------------------

#' Get lambda/mu/sigma values for some INTERGROWTH-21st fetal standards
#' (internal; part of the INTERGROWTH-21st Fetal Growth standards)
#'
#' @param gest_days Numeric vector of gestational age in days, ranging from
#'   `154` to `280`.
#' @param acronym A single string denoting an INTERGROWTH-21<sup>st</sup> fetal
#'   standard. Should be one of `"efwfga"` or `"hewfga"`.
#' @references
#' Stirnemann J, Villar J, Salomon LJ, Ohuma EO, Lamber A, Victoria CG et al.
#' **International Estimated Fetal Weight Standards of the INTERGROWTH-21st
#' Project.** *Ultrasound Obstet Gynecol* 2016, **49:478-486**
#' \doi{10.1002/uog.17347}
#'
#' Stirnemann J, Salomon LJ, Papageorghiou AT. **INTERGROWTH-21st standards for
#' Hadlock's estimation of fetal weight.** *Ultrasound Obstet Gynecol* 2020,
#' **56(6):946-948** \doi{10.1002/uog.22000}
#' @returns Named list with three elements `l` (lambda), `m` (mu), and `s`
#'   (sigma), each of which is the same length as `gest_days`.
#' @noRd
ig_fet_lms <- function(gest_days, acronym) {
  GA <- gest_days / 7
  GA_cubed <- GA^3 # for both EFW standards
  efwfga_coeff <- GA_cubed * log(GA) # for IG-21st EFW standard
  GA_10 <- GA / 10 # for Hadlock EFW standard
  hefwfga_coeff <- log(GA_10) * (GA_10)^(-2) # for Hadlock EFW standard
  switch(
    acronym,
    efwfga = list(
      l = -4.257629 - 2162.234 * GA^(-2) + 0.0002301829 * GA_cubed,
      m = 4.956737 + 0.0005019687 * GA_cubed - 0.0001227065 * efwfga_coeff,
      s = 1e-04 * (-6.997171 + 0.057559 * GA_cubed - 0.01493946 * efwfga_coeff)
    ),
    hefwfga = list(
      l = 9.43643 + 9.41579 * (GA_10)^(-2) - 83.54220 * hefwfga_coeff,
      m = -2.42272 + 1.86478 * GA^0.5 - 1.93299e-5 * GA_cubed,
      s =  0.0193557  + 0.0310716 * (GA_10)^(-2) - 0.0657587 * hefwfga_coeff
    )
  )
}

#' Convert values to z-scores in the INTERGROWTH-21st estimated fetal weight
#' standard (internal; part of the INTERGROWTH-21st Fetal Growth standards)
#'
#' @param efw_g Numeric vector with length one or more of estimated fetal
#'   weight(s) in grams.
#' @param gest_days Numeric vector with same length as `efw_g` of gestational
#'   age(s) in days.
#' @references
#' Stirnemann J, Villar J, Salomon LJ, Ohuma EO, Lamber A, Victoria CG et al.
#' **International Estimated Fetal Weight Standards of the INTERGROWTH-21st
#' Project.** *Ultrasound Obstet Gynecol* 2016, **49:478-486**
#' \doi{10.1002/uog.17347}
#'
#' Stirnemann J, Salomon LJ, Papageorghiou AT. **INTERGROWTH-21st standards for
#' Hadlock's estimation of fetal weight.** *Ultrasound Obstet Gynecol* 2020,
#' **56(6):946-948** \doi{10.1002/uog.22000}
#' @returns Numeric vector with same length as `z` and `gest_days` containing
#'   z-scores.
#' @srrstats {G3.0} Using `abs() < sqrt(.Machine$double.eps)` for floating point
#'   equality.
#' @noRd
ig_fet_lms_y2z <- function(efw_g, gest_days, acronym) {
  lms <- ig_fet_lms(gest_days, acronym)
  efw_g[efw_g < 0] <- NA
  log_efw <- log(efw_g)
  with(lms,
       ifelse(test = abs(l) < sqrt(.Machine$double.eps),
              yes = s^(-1) * log(log_efw / m),
              no = (s * l)^(-1) * ((log_efw / m)^l - 1))
  )
}

#' Convert z-scores to values in the INTERGROWTH-21st estimated fetal weight
#' standard (internal; part of the INTERGROWTH-21st Fetal Growth standards)
#'
#' @param z Numeric vector of length one or more of z-scores.
#' @param gest_days Numeric vector with same length as `z` of gestational
#'   age(s) in days.
#' @references
#' Stirnemann J, Villar J, Salomon LJ, Ohuma EO, Lamber A, Victoria CG et al.
#' **International Estimated Fetal Weight Standards of the INTERGROWTH-21st
#' Project.** *Ultrasound Obstet Gynecol* 2016, **49:478-486**
#' \doi{10.1002/uog.17347}
#'
#' Stirnemann J, Salomon LJ, Papageorghiou AT. **INTERGROWTH-21st standards for
#' Hadlock's estimation of fetal weight.** *Ultrasound Obstet Gynecol* 2020,
#' **56(6):946-948** \doi{10.1002/uog.22000}
#' @returns Numeric vector with same length as `z` and `gest_days` containing
#'   expected estimated fetal weight values in grams.
#' @srrstats {G3.0} Using `abs() < sqrt(.Machine$double.eps)` for floating point
#'   equality.
#' @noRd
ig_fet_lms_z2y <- function(z, gest_days, acronym) {
  lms <- ig_fet_lms(gest_days, acronym)
  log_efw <- with(lms,
                  ifelse(test = abs(l) < sqrt(.Machine$double.eps),
                         yes = m * exp(s * z),
                         no = m * (z * s * l + 1)^(1 / l)))
  exp(log_efw)
}

# INTERNAL: IG-21st Fetal Doppler standards ------------------------------------

#' INTERGROWTH-21<sup>st</sup> fetal doppler gamma/mu/sigma equations
#'   (internal; used for the INTERGROWTH-21st Fetal Doppler standards)
#'
#' @param gest_days Numeric vector of length one or more with gestational age(s)
#'   in days.
#' @param acronym A single string denoting the INTERGROWTH-21st Fetal Doppler
#'   standard to use. Only `"pifga"`, `"rifga"` and `"sdrfga"` will
#'   return gamma/mu/sigma values.
#' @note The GMS values produced here map onto the coefficients used in
#'   Royston & Wright's exponential normal (EN) model. See
#'   [ig_fet_doppler_y2z()] for the reference.
#' @references
#' Drukker L, Staines-Urias E, Villar J, Barros FC, Carvalho M, Munim S et al.
#' **International gestational age-specific centiles for umbilical artery
#' Doppler indices: a longitudinal prospective cohort study of the
#' INTERGROWTH-21st Project.** *Am J Obstet Gynecol* 2021,
#' **222(6):602.e1-602.e15** \doi{10.1016/j.ajog.2020.01.012}
#' @returns Named list of three numeric vectors (`"gamma"`, `"mu"` and
#'   `"sigma"`), which are equal in length to `gest_days`.
#' @noRd
ig_fet_doppler_gms <- function(gest_days, acronym) {
  ig_fet_doppler_G <- c(pifga = -0.0768617,
                        rifga = 0.0172944,
                        sdrfga = -0.2752483)
  ig_fet_doppler_M <- list(
    pifga = \(GA) 1.02944 + 77.7456 * GA^-2 - 0.000004455 * GA^3,
    rifga = \(GA) 0.674914 + 25.3909 * GA^-2 - 0.0000022523 * GA^3,
    sdrfga = \(GA) 2.60358 + 445.991 * GA^-2 - 0.0000108754 * GA^3
  )
  ig_fet_doppler_S <- list(
    pifga = \(GA) -0.00645693 + 254.885 * log(GA) * GA^(-2) - 715.949 * GA^(-2),
    rifga = \(GA) 0.0375921 + 60.7614 * log(GA) * GA^(-2) - 183.336 * GA^(-2),
    sdrfga = \(GA) -0.503202 + 1268.37 * log(GA) * GA^(-2) - 3417.37 * GA^(-2)
  )

  # Initialise empty vectors
  len_out <- length(gest_days)
  gamma <- rep(NA_real_, len_out)
  mu <- rep(NA_real_, len_out)
  sigma <- rep(NA_real_, len_out)
  gest_wks <- gest_days / 7

  is_na_acronym <- is.na(acronym)
  has_doppler_acronym <- acronym %in% names(ig_fet_doppler_G) & !is_na_acronym
  acronyms <- unique(acronym[has_doppler_acronym])
  for (curr_acronym in acronyms) {
    has_curr_acronym <- acronym == curr_acronym & !is_na_acronym
    ga <- gest_wks[has_curr_acronym]
    gamma[has_curr_acronym] <- ig_fet_doppler_G[[curr_acronym]]
    mu[has_curr_acronym] <- ig_fet_doppler_M[[curr_acronym]](GA = ga)
    sigma[has_curr_acronym] <- ig_fet_doppler_S[[curr_acronym]](GA = ga)
  }
  list(g = gamma, m = mu, s = sigma)
}

#' Convert fetal doppler values to z-scores for specific gestational ages
#' (internal)
#'
#' @param y Numeric vector of length one or more with umibilical artery doppler
#'   values, specific to the acronym(s) in use.
#' @param gest_days Numeric vector of same length as `y` with gestational age(s)
#'   in days.
#' @param acronym A single string denoting the INTERGROWTH-21st Fetal Doppler
#'   standard to use. Should be one of `"pifga"`, `"rifga"` or `"sdrfga"`.
#' @note Uses an inverted form of the exponential normal model equation defined
#'   in section 3.1.6 of the attached reference. This model adjusts for skewness
#'   in the distribution of umbilical artery indices at different gestational
#'   ages.
#' @returns Numeric vector of z-scores of the same length as `y`.
#' @references
#' Royston P, Wright EM. **A Method for Estimating Age-Specific Reference
#' Intervals (‘Normal Ranges’) Based on Fractional Polynomials and Exponential
#' Transformation** *J R Statist Soc A* 1998, **161(Part 1):79-101**
#' \doi{10.1111/1467-985X.00091}
#' @noRd
ig_fet_doppler_y2z <- function(y, gest_days, acronym) {
  gms <- ig_fet_doppler_gms(gest_days, acronym)
  # Inversion of EN model equation from section 3.1.6 of referenced paper
  with(gms, (exp((y - m) * g * s^(-1)) - 1) / g)
}

#' Convert fetal doppler z-scores to values for specific gestational ages
#' (internal)
#'
#' @param z Numeric vector of length one or more with z-scores.
#' @param gest_days Numeric vector of same length as `y` with gestational age(s)
#'   in days.
#' @param acronym A single string denoting the INTERGROWTH-21st Fetal Doppler
#'   standard to use. Should be one of `"pifga"`, `"rifga"` or `"sdrfga"`.
#' @note Uses the exponential normal model equation defined in section 3.1.6 of
#'   the attached reference. This model adjusts for skewness in the distribution
#'   of umbilical artery indices at different gestational ages.
#' @returns Numeric vector of expected measurements, with the same length as
#'   `z`.
#' @references
#' Royston P, Wright EM. **A Method for Estimating Age-Specific Reference
#' Intervals (‘Normal Ranges’) Based on Fractional Polynomials and Exponential
#' Transformation** *J R Statist Soc A* 1998, **161(Part 1):79-101**
#' \doi{10.1111/1467-985X.00091}
#' @noRd
ig_fet_doppler_z2y <- function(z, gest_days, acronym) {
  gms <- ig_fet_doppler_gms(gest_days, acronym)
  # EN model equation from section 3.1.6 of referenced paper
  with(gms, {
    log_val <- 1 + g * z
    out <- rep.int(x = NA_real_, length(z))
    use <- !is.na(log_val) & log_val > 0
    out[use] <- m[use] + s[use] * log(log_val[use]) / g[use]
    out
  })
}

# INTERNAL: IG-21st Gestational Weight Gain ------------------------------------

#' INTERGROWTH-21<sup>st</sup> gestational weight gain mu/sigma equations
#'   (internal; used for the INTERGROWTH-21st gestational weight gain standard)
#'
#' @param gest_days Numeric vector with gestational age in days.
#' @note The mu/sigma values returned by this equation have the units
#'   log(gestational weight gain).
#' @returns Named list with two elements:
#'   * `log_mu` - log(mean of GWG at a given gestational age)
#'   * `log_sigma` - log(standard deviation of GWG at a given gestational age)
#' @references
#' Cheikh Ismail L, Bishop DC, Pang R, Ohuma EO, Kac G, Abrams B et al.
#' **Gestational weight gain standards based on women enrolled in the Fetal
#' Growth Longitudinal Study of the INTERGROWTH-21st Project: a prospective
#' longitudinal cohort study.** *BMJ* 2016, **352:i555** \doi{10.1136/bmj.i555}
#' @returns List of three numeric vectors equal in length to `gest_days`.
#' @noRd
ig_fet_gwg_mu_sigma <- function(gest_days) {
  # Gestational weight gain standards
  GA <- gest_days / 7
  log_mu <- 1.382972 - 56.14743 * GA^(-2) + 0.2787683 * GA^0.5
  log_sigma <- 0.2501993731 + 142.4297879 * GA^(-2) -
    61.45345 * GA^(-2) * log(GA)
  list(log_mu = log_mu, log_sigma = log_sigma)
}

#' Convert gestational weight gain values to z-scores for specific gestational
#'   ages (internal)
#'
#' @param gwg_kg Numeric vector of length one or more with gestational weight
#'   gain value(s) in kg.
#' @param gest_days Numeric vector of same length as `gwg_kg` with gestational
#'   age(s) in days.
#' @returns Numeric vector of expected measurements, of same length as `gwg_kg`.
#' @note Uses the equations/method in Table 4 of the attached reference to
#'   calculate z-scores.
#' @references
#' Cheikh Ismail L, Bishop DC, Pang R, Ohuma EO, Kac G, Abrams B et al.
#' **Gestational weight gain standards based on women enrolled in the Fetal
#' Growth Longitudinal Study of the INTERGROWTH-21st Project: a prospective
#' longitudinal cohort study.** *BMJ* 2016, **352:i555** \doi{10.1136/bmj.i555}
#' @noRd
ig_fet_gwg_y2z <- function(gwg_kg, gest_days) {
  log_mu_sigma <- ig_fet_gwg_mu_sigma(gest_days)
  with(log_mu_sigma, (log(gwg_kg + 8.75) - log_mu) / log_sigma)
}

#' Convert gestational weight gain z-scores to values for specific gestational
#'   ages (internal)
#'
#' @param z Numeric vector of length one or more with z-score(s).
#' @param gest_days Numeric vector of same length as `z` with gestational age(s)
#'   in days.
#' @note Uses the equations/method in Table 4 of the attached reference to
#'   calculate z-scores.
#' @returns Numeric vector of expected measurements, of same length as `z`.
#' @references
#' Cheikh Ismail L, Bishop DC, Pang R, Ohuma EO, Kac G, Abrams B et al.
#' **Gestational weight gain standards based on women enrolled in the Fetal
#' Growth Longitudinal Study of the INTERGROWTH-21st Project: a prospective
#' longitudinal cohort study.** *BMJ* 2016, **352:i555** \doi{10.1136/bmj.i555}
#' @noRd
ig_fet_gwg_z2y <- function(z, gest_days) {
  log_mu_sigma <- ig_fet_gwg_mu_sigma(gest_days)
  with(log_mu_sigma, exp(log_mu + z * log_sigma) - 8.75)
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
