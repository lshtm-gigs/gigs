#' Convert z-scores/centiles to values in the INTERGROWTH-21<sup>st</sup>
#' Newborn Size Standards
#'
#' @param p,z Numeric vector of length one or more with centiles/z-scores to
#'   convert to values. For `p`, if an element of `p` is not between `0` and
#'   `1`, gigs will replace it with `NA` and warn you informatively. This
#'   behaviour can be customised using the functions in [gigs_options].
#' @param gest_days Numeric vector of length one or more with gestational ages
#'   in days. Elements should be between certain values depending on the Newborn
#'   Size standard in use (defined by `acronym`). These are:
#'   * Between 168 and 300 days for `"wfga"`, `"lfga"`, `"hcfga"`, and
#'     `"wlrfga"`.
#'   * Between 154 and 280 days for `"fmfga"`, `"bfpfga"`, and `"ffmfga"`.
#'
#'   By default, gigs will replace elements in `gest_days` that are out of
#'   bounds for the growth standard in use with `NA` and warn you. This
#'   behaviour can be customised using the functions in [gigs_options].
#' @param acronym Character vector of length one or more denoting the
#'   INTERGROWTH-21<sup>st</sup> Newborn Size standard to use for each
#'   observation. Each element should be one of:
#'   * `"wfga"` (weight-for-GA)
#'   * `"lfga"` (length-for-GA)
#'   * `"hcfga"` (head circumference-for-GA)
#'   * `"wlrfga"` (weight/length ratio-for-GA)
#'   * `"fmfga"` (fat mass-for-GA)
#'   * `"bfpfga"` (body fat %-for-GA)
#'   * `"ffmfga"` (fat-free mass-for-GA)
#'
#'   This argument is case-sensitive. By default, gigs will replace elements in
#'   `acronym` which are not one of the above values with `NA` and warn you.
#'   This behaviour can be customised using the functions in [gigs_options].
#' @srrstats {G2.3b} Explicit reference to `acronym` case-sensitivity.
#' @inherit shared_roxygen_params params note
#' @inherit shared_value2zscore_returns return
#' @references
#' Villar J, Cheikh Ismail L, Victora CG, Ohuma EO, Bertino E, Altman DG, et al.
#' **International standards for newborn weight, length, and head circumference
#' by gestational age and sex: the Newborn Cross-Sectional Study of the
#' INTERGROWTH-21st Project.** *Lancet* 2014, **384(9946):857-68.**
#' \doi{10.1016/S0140-6736(14)60932-6}
#'
#' Villar J, Giuliani F, Fenton TR, Ohuma EO, Ismail LC, Kennedy SH et al.
#' **INTERGROWTH-21st very preterm size at birth reference charts.** *Lancet*
#' 2016, **387(10021):844-45.** \doi{10.1016/S0140-6736(16)00384-6}
#'
#' Villar J, Puglia FA, Fenton TR, Ismal LC, Staines-Urias E, Giuliani F, et al.
#' **Body composition at birth and its relationship with neonatal anthropometric
#' ratios: the newborn body composition study of the INTERGROWTH-21st project.**
#' *Pediatric Research* 2017, **82:305-316.** \doi{10.1038/pr.2017.52}
#' @examples
#' # Convert centiles to values
#' p <- 0.25 # 25th centile
#' ig_nbs_centile2value(p = p, gest_days = 280, sex = "M", acronym = "wfga") |>
#'   round(digits = 2)
#'
#' # Or z-scores to values
#' z <- qnorm(p)
#' ig_nbs_zscore2value(z = z, gest_days = 280, sex = "M", acronym = "wfga") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' ig_nbs_zscore2value(z = z, gest_days = 280, sex = "M", acronym = "lfga") |>
#'   round(digits = 2)
#'
#' # ... or by using a standard-specific function
#' ig_nbs_lfga_zscore2value(z = z, gest_days = 280, sex = "M") |>
#'   round(digits = 2)
#'
#' # Inputs are recycled to the input of the longest length
#' ig_nbs_lfga_zscore2value(z = seq(0.1, 0.9, by = 0.2),
#'                          gest_days = 280,
#'                          sex = "M") |>
#'   round(digits = 2)
#'
#' # Bad inputs will not stop the function but will instead return NA - here 140
#' # days for gest_days is outside the bounds of the INTERGROWTH-21st newborn
#' # size standards
#' ig_nbs_hcfga_zscore2value(z = z,
#'                           gest_days = c(140, 182, 224, 266),
#'                           sex = "F") |>
#'   round(digits = 2)
#' @rdname ig_nbs_centile2value
#' @export
ig_nbs_centile2value <- function(p, gest_days, sex, acronym) {
  list(p = p, gest_days = gest_days, sex = sex, acronym = acronym) |>
    do.call(what = validate_ig_nbs) |>
    do.call(what = ig_nbs_c2v_internal)
}

#' @rdname ig_nbs_centile2value
#' @export
ig_nbs_wfga_centile2value <- function(p, gest_days, sex) {
  ig_nbs_centile2value(p, gest_days, sex, acronym = "wfga")
}

#' @rdname ig_nbs_centile2value
#' @export
ig_nbs_lfga_centile2value <- function(p, gest_days, sex) {
  ig_nbs_centile2value(p, gest_days, sex, acronym = "lfga")
}

#' @rdname ig_nbs_centile2value
#' @export
ig_nbs_hcfga_centile2value <- function(p, gest_days, sex) {
  ig_nbs_centile2value(p, gest_days, sex, acronym = "hcfga")
}

#' @rdname ig_nbs_centile2value
#' @export
ig_nbs_wlrfga_centile2value <- function(p, gest_days, sex) {
  ig_nbs_centile2value(p, gest_days, sex, acronym = "wlrfga")
}

#' @rdname ig_nbs_centile2value
#' @export
ig_nbs_fmfga_centile2value <- function(p, gest_days, sex) {
  ig_nbs_centile2value(p, gest_days, sex, acronym = "fmfga")
}

#' @rdname ig_nbs_centile2value
#' @export
ig_nbs_bfpfga_centile2value <- function(p, gest_days, sex) {
  ig_nbs_centile2value(p, gest_days, sex, acronym = "bfpfga")
}

#' @rdname ig_nbs_centile2value
#' @export
ig_nbs_ffmfga_centile2value <- function(p, gest_days, sex) {
  ig_nbs_centile2value(p, gest_days, sex, acronym = "ffmfga")
}

#' @importFrom stats pnorm
#' @rdname ig_nbs_centile2value
#' @export
ig_nbs_zscore2value <- function(z, gest_days, sex, acronym) {
  validated <- list(z = z, gest_days = gest_days, sex = sex,
                    acronym = acronym) |>
    do.call(what = validate_ig_nbs)
  with(validated, ig_nbs_c2v_internal(pnorm(z), gest_days, sex, acronym))
}

#' @rdname ig_nbs_centile2value
#' @export
ig_nbs_wfga_zscore2value <- function(z, gest_days, sex) {
  ig_nbs_zscore2value(z, gest_days, sex, acronym = "wfga")
}

#' @rdname ig_nbs_centile2value
#' @export
ig_nbs_lfga_zscore2value <- function(z, gest_days, sex) {
  ig_nbs_zscore2value(z, gest_days, sex, acronym = "lfga")
}

#' @rdname ig_nbs_centile2value
#' @export
ig_nbs_hcfga_zscore2value <- function(z, gest_days, sex) {
  ig_nbs_zscore2value(z, gest_days, sex, acronym = "hcfga")
}

#' @rdname ig_nbs_centile2value
#' @export
ig_nbs_wlrfga_zscore2value <- function(z, gest_days, sex) {
  ig_nbs_zscore2value(z, gest_days, sex, acronym = "wlrfga")
}

#' @rdname ig_nbs_centile2value
#' @export
ig_nbs_fmfga_zscore2value <- function(z, gest_days, sex) {
  ig_nbs_zscore2value(z, gest_days, sex, acronym = "fmfga")
}

#' @rdname ig_nbs_centile2value
#' @export
ig_nbs_bfpfga_zscore2value <- function(z, gest_days, sex) {
  ig_nbs_zscore2value(z, gest_days, sex, acronym = "bfpfga")
}

#' @rdname ig_nbs_centile2value
#' @export
ig_nbs_ffmfga_zscore2value <- function(z, gest_days, sex) {
  ig_nbs_zscore2value(z, gest_days, sex, acronym = "ffmfga")
}

#' Convert values to z-scores/centiles in the INTERGROWTH-21<sup>st</sup>
#' Newborn Size Standards
#'
#' @param weight_kg Numeric vector of length one or more with birth weight(s) in
#'   kg.
#' @param length_cm Numeric vector of length one or more with birth length(s) in
#'    cm.
#' @param headcirc_cm Numeric vector of length one or more with birth head
#'   circumference(s) in cm.
#' @param wei_len_ratio Numeric vector of length one or more with weight-length
#'   ratio(s) in kg per m.
#' @param fat_mass_g Numeric vector of length one or more with fat mass(es) in
#'   g.
#' @param body_fat_perc Numeric vector of length one or more with body fat
#'   percentage(s).
#' @param fatfree_mass_g Numeric vector of length one or more with fat-free
#'   mass(es) in g.
#' @inherit shared_roxygen_params params note
#' @inherit shared_zscore2value_returns return
#' @inherit ig_nbs_centile2value params references
#' @examples
#' # Convert values to centiles
#' ig_nbs_value2centile(y = 3.12, gest_days = 280, sex = "M", acronym = "wfga") |>
#'   round(digits = 2)
#'
#' # Or values to z-scores
#' ig_nbs_value2zscore(y = 3.12, gest_days = 280, sex = "M", acronym = "wfga") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' ig_nbs_value2zscore(y = 48.84, gest_days = 280, sex = "F", acronym = "lfga") |>
#'   round(digits = 2)
#'
#' # ... or by using a standard-specific function
#' ig_nbs_lfga_value2zscore(length_cm = 48.84, gest_days = 280, sex = "F") |>
#'   round(digits = 2)
#'
#' # Inputs are recycled to the input of the longest length
#' ig_nbs_wlrfga_value2zscore(wei_len_ratio = c(7.37, 6.47, 6.12, 6.86),
#'                            gest_days = 280,
#'                            sex = "M") |>
#'   round(digits = 2)
#'
#' # Bad inputs will not stop the function but will instead return NA - here 301
#' # days gest_days is outside the bounds of the INTERGROWTH-21st newborn size
#' # standards
#' ig_nbs_hcfga_value2centile(headcirc_cm = c(23.0, 28.0, 33.0, 35.0),
#'                               gest_days = c(168, 217, 266, 301),
#'                               sex = "F") |>
#'   round(digits = 2)
#' @rdname ig_nbs_value2centile
#' @export
ig_nbs_value2centile <- function(y, gest_days, sex, acronym) {
  list(y = y, gest_days = gest_days, sex = sex, acronym = acronym) |>
    do.call(what = validate_ig_nbs) |>
    do.call(what = ig_nbs_v2c_internal)
}

#' @rdname ig_nbs_value2centile
#' @export
ig_nbs_wfga_value2centile <- function(weight_kg, gest_days, sex) {
  ig_nbs_value2centile(weight_kg, gest_days, sex, acronym = "wfga")
}

#' @rdname ig_nbs_value2centile
#' @export
ig_nbs_lfga_value2centile <- function(length_cm, gest_days, sex) {
  ig_nbs_value2centile(length_cm, gest_days, sex, acronym = "lfga")
}

#' @rdname ig_nbs_value2centile
#' @export
ig_nbs_hcfga_value2centile <- function(headcirc_cm, gest_days, sex) {
  ig_nbs_value2centile(headcirc_cm, gest_days, sex, acronym = "hcfga")
}

#' @rdname ig_nbs_value2centile
#' @export
ig_nbs_wlrfga_value2centile <- function(wei_len_ratio, gest_days, sex) {
  ig_nbs_value2centile(wei_len_ratio, gest_days, sex, acronym = "wlrfga")
}

#' @rdname ig_nbs_value2centile
#' @export
ig_nbs_fmfga_value2centile <- function(fat_mass_g, gest_days, sex) {
  ig_nbs_value2centile(fat_mass_g, gest_days, sex, acronym = "fmfga")
}

#' @rdname ig_nbs_value2centile
#' @export
ig_nbs_bfpfga_value2centile <- function(body_fat_perc, gest_days, sex) {
  ig_nbs_value2centile(body_fat_perc, gest_days, sex, acronym = "bfpfga")
}

#' @rdname ig_nbs_value2centile
#' @export
ig_nbs_ffmfga_value2centile <- function(fatfree_mass_g, gest_days, sex) {
  ig_nbs_value2centile(fatfree_mass_g, gest_days, sex, acronym = "ffmfga")
}

#' @importFrom stats qnorm
#' @rdname ig_nbs_value2centile
#' @export
ig_nbs_value2zscore <- function(y, gest_days, sex, acronym) {
  validated <- list(y = y, gest_days = gest_days,
                    sex = sex, acronym = acronym) |>
    do.call(what = validate_ig_nbs)
  with(validated, ig_nbs_v2c_internal(y, gest_days, sex, acronym)) |>
    qnorm()
}

#' @rdname ig_nbs_value2centile
#' @export
ig_nbs_wfga_value2zscore <- function(weight_kg, gest_days, sex) {
  ig_nbs_value2zscore(weight_kg, gest_days, sex, acronym = "wfga")
}

#' @rdname ig_nbs_value2centile
#' @export
ig_nbs_lfga_value2zscore <- function(length_cm, gest_days, sex) {
  ig_nbs_value2zscore(length_cm, gest_days, sex, acronym = "lfga")
}

#' @rdname ig_nbs_value2centile
#' @export
ig_nbs_hcfga_value2zscore <- function(headcirc_cm, gest_days, sex) {
  ig_nbs_value2zscore(headcirc_cm, gest_days, sex, acronym = "hcfga")
}

#' @rdname ig_nbs_value2centile
#' @export
ig_nbs_wlrfga_value2zscore <- function(wei_len_ratio, gest_days, sex) {
  ig_nbs_value2zscore(wei_len_ratio, gest_days, sex, acronym = "wlrfga")
}

#' @rdname ig_nbs_value2centile
#' @export
ig_nbs_fmfga_value2zscore <- function(fat_mass_g, gest_days, sex) {
  ig_nbs_value2zscore(fat_mass_g, gest_days, sex, acronym = "fmfga")
}

#' @rdname ig_nbs_value2centile
#' @export
ig_nbs_bfpfga_value2zscore <- function(body_fat_perc, gest_days, sex) {
  ig_nbs_value2zscore(body_fat_perc, gest_days, sex, acronym = "bfpfga")
}

#' @rdname ig_nbs_value2centile
#' @export
ig_nbs_ffmfga_value2zscore <- function(fatfree_mass_g, gest_days, sex) {
  ig_nbs_value2zscore(fatfree_mass_g, gest_days, sex, acronym = "ffmfga")
}

# INTERNAL: INTERGROWTH-21st Newborn Size standards conversion logic -----------

#' Convert z-scores to values in the INTERGROWTH-21<sup>st</sup> Fetal standards
#' @inherit ig_fet_zscore2value params return
#' @note This function will fail if given inputs of different lengths.
#' @noRd
ig_nbs_c2v_internal <- function(p, gest_days, sex, acronym) {
  stop_if_lengths_unequal(p, gest_days, sex, acronym)
  vpns_lim <- 231
  ifelse(
    test = acronym == "wlrfga",
    yes = ig_nbs_wlrfga_p2v(p, gest_days, sex),
    no = ifelse(
      test = acronym %in% c("fmfga", "bfpfga", "ffmfga"),
      yes =  ig_nbs_bodycomp_p2v(p, gest_days, sex, acronym),
      no = ifelse(
        test = gest_days >= vpns_lim,
        yes = ig_nbs_msnt_p2v(p, gest_days, sex, acronym),
        no = ig_vpns_zscore2value(z = qnorm(p), gest_days, sex, acronym)
      )
    )
  )
}

#' Convert z-scores to values in the INTERGROWTH-21<sup>st</sup> Fetal standards
#' @inherit ig_fet_zscore2value params return
#' @note This function will fail if given inputs of different lengths.
#' @noRd
ig_nbs_v2c_internal <- function(y, gest_days, sex, acronym) {
  stop_if_lengths_unequal(y, gest_days, sex, acronym)
  vpns_lim <- 231
  ifelse(
    test = acronym == "wlrfga",
    yes = ig_nbs_wlrfga_v2p(y, gest_days, sex),
    no = ifelse(
      test = acronym %in% c("fmfga", "bfpfga", "ffmfga"),
      yes = ig_nbs_bodycomp_v2p(y, gest_days, sex, acronym),
      no = ifelse(
        test = gest_days >= vpns_lim,
        yes = ig_nbs_msnt_v2p(y, gest_days, sex, acronym),
        no = pnorm(ig_vpns_value2zscore(y, gest_days, sex, acronym))
      )
    )
  )
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
#' @param acronym Character vector of acronym(s) denoting which
#'   coefficient-based INTERGROWTH-21<sup>st</sup> standard to use. Elements
#'   which are not one of `"wfga"`,`"lfga"`, or `"hcfga"` will be return `NA`.
#' @return A list with names `"mu"`, `"sigma"`, `"nu"`, and `"tau"`, where each
#'   is a numeric vector with mu, sigma, nu and tau values for each elementwise
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
  retrieve_coefficients(gest_days, sex, acronym, gigs::ig_nbs_coeffs,
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
  y <- rep_len(x = NA, length.out = length(p))
  # Remove NA p/mu/sigma/nu/tau values, or qST3C() will fail
  lgl_complete_msnt <- stats::complete.cases(as.data.frame(msnt))
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
  p <- rep_len(x = NA, length.out = length(y))
  # Remove NA y/mu/sigma/nu/tau values, or pST3() will fail
  lgl_complete_msnt <- stats::complete.cases(as.data.frame(msnt))
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
#'   website](https://intergrowth21.tghn.org/newborn-size-birth/#c4).
#' @references
#' Villar J, Puglia FA, Fenton TR, Ismal LC, Staines-Urias E, Giuliani F, et al.
#' **Body composition at birth and its relationship with neonatal anthropometric
#' ratios: the newborn body composition study of the INTERGROWTH-21st project.**
#' *Pediatric Research* 2017, **82:305-316.** \doi{10.1038/pr.2017.52}
#' @noRd
ig_nbs_wlr_mu_sigma <- function(ga_weeks, sex) {
  sex_as_numeric <- ifelse(sex == "M", yes = 1, no = 0)
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
#' @param acronym Character vector of same length as `gest_days` with acronym(s)
#'   denoting the INTERGROWTH-21<sup>st</sup> NBS normative body composition
#'   standard to use. Must be one of `"fmfga"`, `"bfpfga"`, or `"ffmfga"`.
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
  replace(out, out <= 0, values = NA)
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
    pnorm()
}

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.0} Primary literature referenced for each exported function,
#'   and for internal functions.
#' @srrstats {G1.4, G1.4a} All functions in file documented using `{roxygen2}`.
#' @srrstats {G2.0a, G2.1a} Exported functions in this file document
#'   expectations on the length of inputs and their data types.
#' @srrstats {G2.0, G2.1, G2.2, G2.3, 2.3a, G2.6} These standards
#'   are met in all exported functions by passing inputs to [validate_ig_nbs()].
#'   All internal functions in this script are provided with vectors that have
#'   already been validated.
#' @srrstatsTODO {G2.13, G2.14, G2.14a, G2.14b, G2.16} These standards are met
#'   in all exported functions by passing inputs to [validate_ig_nbs()]. All
#'   internal functions in this script are provided with vectors that have
#'   already checked for missing/undefined/out-of-bounds data.
