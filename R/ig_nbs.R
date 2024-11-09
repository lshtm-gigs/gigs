# INTERNAL: INTERGROWTH-21st Newborn Size standards conversion logic -----------

#' Convert z-scores to values in the INTERGROWTH-21<sup>st</sup> Fetal standards
#' @inherit zscore2value params return
#' @note This function will fail if given inputs of different lengths.
#' @noRd
ig_nbs_c2v_internal <- function(p, x, sex, acronym) {
  gest_days <- x
  if (acronym == "wlrfga") {
    y <- ig_nbs_wlrfga_p2v(p, gest_days, sex)
  } else if (acronym == "fmfga" || acronym == "bfpfga" || acronym == "ffmfga") {
    y <- ig_nbs_bodycomp_p2v(p, gest_days, sex, acronym)
  } else {
    vpns_lim <- 231
    # y <- rep_len(x = NA_real_, length = length(p))
    y <- ifelse(
      test = gest_days >= vpns_lim,
      yes = ig_nbs_msnt_p2v(p, gest_days, sex, acronym),
      no = ig_vpns_zscore2value(z = qnorm(p), gest_days, sex, acronym)
    )
  }
  y
}

#' Convert z-scores to values in the INTERGROWTH-21<sup>st</sup> Fetal standards
#' @inherit zscore2value params return
#' @note This function will fail if given inputs of different lengths.
#' @noRd
ig_nbs_v2c_internal <- function(y, x, sex, acronym) {
  gest_days <- x
  if (acronym == "wlrfga") {
    p <- ig_nbs_wlrfga_v2p(y, gest_days, sex)
  } else if (acronym == "fmfga" || acronym == "bfpfga" || acronym == "ffmfga") {
    p <- ig_nbs_bodycomp_v2p(y, gest_days, sex, acronym)
  } else {
    vpns_lim <- 231
    p <- ifelse(
      test = gest_days >= vpns_lim,
      yes = ig_nbs_msnt_v2p(y, gest_days, sex, acronym),
      no = pnorm(ig_vpns_value2zscore(y, gest_days, sex, acronym))
    )
  }
  p
}

# INTERNAL: mu/sigma/nu/tau-based INTERGROWTH-21st Newborn Size standards ------

#' Retrieve GAMLSS coefficients for INTERGROWTH-21<sup>st</sup> Newborn Size
#' standards
#'
#' Retrieves mu/sigma/nu/tau values for GAMLSS-based conversion between values
#' and z-scores/centiles in the INTERGROWTH-21<sup>st</sup> Newborn Size
#' standards.
#'
#' @param gest_days Numeric vector of length one or more with gestational age(s)
#'   in days. Elements not between `231` and `300` will return `NA`.
#' @param sex Character vector of length one or more with sex(es), either `"M"`
#'   (male) or `"F"` (female).
#' @param acronym A single string denoting which coefficient-based
#'   INTERGROWTH-21<sup>st</sup> standard to use. Must be one of `"wfga"`,
#'   `"lfga"`, or `"hcfga"` and is case-sensitive.
#' @return A list with names `"mu"`, `"sigma"`, `"nu"`, and `"tau"`, where each
#'   is a numeric vector with mu, sigma, nu or tau values for each elementwise
#'   combination of `gest_days`, `sex`, and `acronym`.
#' @note The mu/sigma/nu/tau coefficients in gigs are not included in the
#'   referenced publication, but were supplied directly by Eric Ohuma. Villar
#'   *et al.* used these coefficients to construct the growth curves they
#'   described in the attached reference, and in testing we found these
#'   coefficients output the INTERGROWTH-21<sup>st</sup> standards exactly once
#'   rounded to the same number of decimal places as the published tables.
#' @references
#' Villar J, Cheikh Ismail L, Victora CG, Ohuma EO, Bertino E, Altman DG, et al.
#' **International standards for newborn weight, length, and head circumference
#' by gestational age and sex: the Newborn Cross-Sectional Study of the
#' INTERGROWTH-21st Project.** *Lancet* 2014, **384(9946):857-68.**
#' \doi{10.1016/S0140-6736(14)60932-6}
#' @noRd
ig_nbs_msnt <- function(gest_days, sex, acronym) {
  retrieve_coefficients(gest_days, sex, gigs::ig_nbs_coeffs[[acronym]],
                        c("mu", "sigma", "nu", "tau"))
}

#' @rdname ig_nbs_msnt
#' @param p Numeric vector of same length as `gest_days`/`sex`/`acronym` with
#'   centiles to convert to expected measurements using `gamlss.dist::qST3C()`.
#' @returns Numeric vector the same length as `p` with expected measurements for
#'   each elementwise combination of `y`, `gest_days`, `sex`, and `acronym`. The
#'   units of this vector will depend on each value in `acronym`.
#' @noRd
ig_nbs_msnt_p2v <- function(p, gest_days, sex, acronym) {
  msnt <- ig_nbs_msnt(gest_days = gest_days, sex = sex, acronym = acronym)
  y <- rep_len(x = NA_real_, length.out = length(p))
  # Remove NA p/mu/sigma/nu/tau values, or qST3C() will fail
  lgl_complete_msnt <- !is.na(p) & stats::complete.cases(as.data.frame(msnt))
  msnt_no_na <- lapply(X = msnt, \(coeff) coeff[lgl_complete_msnt])
  if (any(lgl_complete_msnt)) {
    y[lgl_complete_msnt] <- gamlss.dist::qST3C(p = p[lgl_complete_msnt],
                                               mu = msnt_no_na[[1]],
                                               sigma = msnt_no_na[[2]],
                                               nu = msnt_no_na[[3]],
                                               tau = msnt_no_na[[4]])
  }
  y
}

#' @rdname ig_nbs_msnt
#' @param y Numeric vector of same length as `gest_days`/`sex`/`acronym` with
#'   measurements which will be used to get centiles using
#'   `gamlss.dist::pST3C()`.
#' @returns Numeric vector the same length as `y` with centiles for each
#'   elementwise combination of `y`, `gest_days`, `sex`, and `acronym`.
#' @noRd
ig_nbs_msnt_v2p <- function(y, gest_days, sex, acronym) {
  msnt <- ig_nbs_msnt(gest_days = gest_days, sex = sex, acronym = acronym)
  p <- rep_len(x = NA_real_, length.out = length(y))
  # Remove NA y/mu/sigma/nu/tau values, or pST3() will fail
  lgl_complete_msnt <- !is.na(y) & stats::complete.cases(as.data.frame(msnt))
  msnt_no_na <- lapply(X = msnt, \(coeff) coeff[lgl_complete_msnt])
  if (any(lgl_complete_msnt)) {
    p[lgl_complete_msnt] <- gamlss.dist::pST3C(q =y[lgl_complete_msnt],
                                               mu = msnt_no_na[[1]],
                                               sigma = msnt_no_na[[2]],
                                               nu = msnt_no_na[[3]],
                                               tau = msnt_no_na[[4]])
  }
  p
}

# INTERNAL: INTERGROWTH-21st weight-length ratio standard ----------------------

#' INTERGROWTH-21<sup>st</sup> weight-to-length ratio means/standard
#' deviations (internal)
#'
#' @param ga_weeks Numeric vector of length one or more with gestational age(s)
#'   in weeks. Each element should be between `24` and `42.85714`
#'   (42 weeks and 6 days).
#' @param sex Character vector of the same length as `ga_weeks` with sex(es),
#'   either `"M"` (male) or `"F"` (female).
#' @return A named list with two vectors, named `mu` and `sigma`, each of which
#'   has the same length as `ga_weeks`.
#' @note These equations are not included in the referenced publication. Rather,
#'   they were taken from weight-to-length ratio calculating Excel files
#'   available on the [INTERGROWTH-21<sup>st</sup>
#'   website](https://intergrowth21.com/tools-resources/newborn-size).
#' @references
#' Villar J, Puglia FA, Fenton TR, Ismal LC, Staines-Urias E, Giuliani F, et al.
#' **Body composition at birth and its relationship with neonatal anthropometric
#' ratios: the newborn body composition study of the INTERGROWTH-21st project.**
#' *Pediatric Research* 2017, **82:305-316.** \doi{10.1038/pr.2017.52}
#' @noRd
ig_nbs_wlr_mu_sigma <- function(ga_weeks, sex) {
  sex_as_numeric <- sex == "M"
  lgl_very_preterm <- ga_weeks < 33
  mu <- ifelse(
    test = lgl_very_preterm,
    yes = 3.400617 + (-0.0103163 * ga_weeks^2) + (0.0003407 * ga_weeks^3) +
      (0.1382809 * sex_as_numeric),
    no = ifelse(
      test = sex == "M",
      yes = -17.84615 + (-3778.768 * (ga_weeks^-1)) +
        (1291.477 * ((ga_weeks^-1) * log(ga_weeks))),
      no = -5.542927 + (0.0018926 * (ga_weeks^3)) +
        (-0.0004614 * ((ga_weeks^3)* log(ga_weeks)))
    )
  )
  sigma <- ifelse(
    test = lgl_very_preterm,
    yes = sqrt(x = 0.3570057),
    no = ifelse(
      test = sex == "M",
      yes = 1.01047 + (-0.0080948 * ga_weeks),
      no = 0.6806229
    )
  )
  list(mu = mu, sigma = sigma)
}

#' Convert values to centiles for the INTERGROWTH-21st weight-to-length ratio
#' standard (internal)
#' @param y Numeric vector of length one or more with measurements.
#' @param gest_days Numeric vector of same length as `z` with gestational ages
#'   in days.
#' @param sex Character vector of the same length as `y` with sex(es), either
#'   `"M"` (male) or `"F"` (female).
#' @inheritParams ig_nbs_wlr_mu_sigma
#' @returns Numeric vector of centiles with same length as `y`.
#' @noRd
ig_nbs_wlrfga_v2p <- function(y, gest_days, sex) {
  wlr <- ig_nbs_wlr_mu_sigma(ga_weeks = gest_days / 7, sex = sex)
  with(wlr, mu_sigma_y2z(y = y, mu = mu, sigma = sigma)) |>
    pnorm()
}

#' Convert values to centiles for the INTERGROWTH-21st weight-to-length ratio
#' standard (internal)
#' @param p Numeric vector of length one or more with centiles.
#' @param gest_days Numeric vector of same length as `z` with gestational ages
#'   in days.
#' @param sex Character vector of the same length as `y` with sex(es), either
#'   `"M"` (male) or `"F"` (female).
#' @returns Numeric vector of expected measurements with same length as `p`.
#' @noRd
ig_nbs_wlrfga_p2v <- function(p, gest_days, sex) {
  wlr <- ig_nbs_wlr_mu_sigma(ga_weeks = gest_days / 7, sex = sex)
  with(wlr, mu_sigma_z2y(z = qnorm(p), mu = mu, sigma = sigma))
}

# INTERNAL: INTERGROWTH-21st normative body composition standards --------------

#' INTERGROWTH-21<sup>st</sup> normative body composition means/standard
#' deviations
#'
#' @param gest_days Numeric vector of length one or more with gestational age(s)
#'   in days at which to calculate mu (mean) and sigma (SD). Should be between
#'   `266` and `294`.
#' @param sex Character vector of same length as `gest_days` with sex(es),
#'   either `"M"` (male) or `"F"` (female).
#' @param acronym A single string denoting the INTERGROWTH-21<sup>st</sup>
#'   NBS normative body composition standard to use. Must be one of `"fmfga"`,
#'   `"bfpfga"`, or `"ffmfga"` and is case-sensitive.
#' @return A numeric matrix with two columns and same number of rows as
#'   the length of `gest_days`/`sex`/`acronym`. The columns contain the (1)
#'   mean and (2) standard deviation for each elementwise combination of
#'   `gest_days`, `sex`, and `acronym`.
#' @note These parameters are not included in the referenced publication, but
#'   the associated supplementary materials. We used tables S1, S2 and S3 and
#'   `lm()` to derive the equations. This process can be seen in the
#'   INTERGROWTH-21<sup>st</sup> body composition vignette or the source code of
#'   `data-raw/ig_nbs_bc.R`. As a result, z-scores/centiles derived from
#'   these models differ slightly from Villar *et al.*'s published values.
#' @references
#' Villar J, Puglia FA, Fenton TR, Ismal LC, Staines-Urias E, Giuliani F, et al.
#' **Body composition at birth and its relationship with neonatal anthropometric
#' ratios: the newborn body composition study of the INTERGROWTH-21st project.**
#' *Pediatric Research* 2017, **82:305-316.** \doi{10.1038/pr.2017.52}
#' @noRd
ig_nbs_bodycomp_mu_sigma <- function(gest_days, sex, acronym) {
  x <- gest_days
  acronym_sex <- paste0(acronym, "_", sex)
  # `ig_nbs_bc_li` is an internal list with regression equation parameters for
  # the normative body composition standards, which you can check out in
  # `data-raw/sysdata.R` or the INTERGROWTH-21st body composition vignette
  params_li <- ig_nbs_bc_li[acronym_sex]
  params_null <- vapply(params_li, is.null, FUN.VALUE = logical(length = 1L))
  if (any(params_null)) {
    params_li[params_null] <- rep(list(rep(NA, 5)), sum(params_null))
  }

  # Unlist is a relative bottleneck here, could refactor later *if* too slow
  params <- matrix(unlist(params_li, recursive = FALSE, use.names = FALSE),
                   ncol = length(params_li), nrow = 5)
  mu <- params[1,] + params[2,] * x + params[3,] * x^2 + params[4,] * x^3
  matrix(c(mu, params[5,]), nrow = length(mu), ncol = 2,
         dimnames = list(NULL, c("mu", "sigma")))
}

#' Convert values to centiles for the INTERGROWTH-21st normative body
#' composition standards (internal)
#' @param p Numeric vector of length one or more with centiles.
#' @param gest_days Numeric vector of same length as `z` with gestational ages
#'   in days.
#' @inheritParams ig_nbs_bodycomp_mu_sigma
#' @returns Numeric vector of expected measurements, with same length as `y`.
#' @noRd
ig_nbs_bodycomp_p2v <- function(p, gest_days, sex, acronym) {
  body_comp <- ig_nbs_bodycomp_mu_sigma(gest_days, sex, acronym)
  out <- mu_sigma_z2y(z = qnorm(p), mu = body_comp[,1], sigma = body_comp[,2])
  unname(replace(out, out <= 0, values = NA))
}

#' Convert values to centiles for the INTERGROWTH-21st weight-to-length ratio
#' standard (internal)
#' @param y Numeric vector of length one or more with measurements.
#' @param gest_days Numeric vector of same length as `z` with gestational ages
#'   in days.
#' @inheritParams ig_nbs_bodycomp_mu_sigma
#' @returns Numeric vector of expected measurements, with same length as `p`.
#' @noRd
ig_nbs_bodycomp_v2p <- function(y, gest_days, sex, acronym) {
  body_comp <- ig_nbs_bodycomp_mu_sigma(gest_days, sex, acronym)
  mu_sigma_y2z(y = y, mu = body_comp[,1], sigma = body_comp[, 2]) |>
    unname() |>
    pnorm()
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
