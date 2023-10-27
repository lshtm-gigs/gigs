#' Convert z-scores/percentiles to values in the INTERGROWTH-21<sup>st</sup>
#' Newborn Size Standards
#'
#' @param p,z Percentile(s)/z-score(s) to convert to a value/values.
#' @param gest_age Gestational age(s) in days. Must be between `266` to `294`
#' for body composition equations (`"fmfga"`, `"bfpfga"`, or `"ffmfga"`), or
#' between `168` and `300` for the other standards.
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param acronym Acronym(s) denoting the INTERGROWTH-21<sup>st</sup> NBS
#' standard to use. Must be one of `"wfga"` (weight-for-GA), `"lfga"`
#' (length-for-GA), `"hcfga"` (head circumference-for-GA), `"wlrfga"`
#' (weight/length ratio-for-GA), `"fmfga"` (fat mass-for-GA), `"bfpfga"`
#' (body fat %-for-GA), or `"ffmfga"` (fat-free mass-for-GA).
#' @return Expected measurements for each combination of z-score/percentile,
#' gestational age, sex, and acronym provided to the function.
#' @note Input vectors will be recycled to the length of the longest vector
#' according to the rules of `vctrs::vec_recycle_common()`.
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
#' ratios: the newborn body composition study of the INTERGROWTH-21st
#' project.** *Pediatric Research* 2017, **82:305-316.**
#' \doi{10.1038/pr.2017.52}
#' @examples
#' # Convert percentiles to values
#' p <- 0.25 # 25th percentile
#' ig_nbs_percentile2value(p = p, gest_age = 280, sex = "M", acronym = "wfga") |>
#'   round(digits = 2)
#'
#' # Or z-scores to values
#' z <- qnorm(p)
#' ig_nbs_zscore2value(z = z, gest_age = 280, sex = "M", acronym = "wfga") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' ig_nbs_zscore2value(z = z, gest_age = 280, sex = "M", acronym = "lfga") |>
#'   round(digits = 2)
#'
#' # ... or by using a standard-specific function
#' ig_nbs_lfga_zscore2value(z = z, gest_age = 280, sex = "M") |>
#'   round(digits = 2)
#'
#' # Inputs are recycled to the input of the longest length
#' ig_nbs_lfga_zscore2value(z = seq(0.1, 0.9, by = 0.2),
#'                          gest_age = 280,
#'                          sex = "M") |>
#'   round(digits = 2)
#'
#' # Bad inputs will not stop the function but will instead return NA - here 140
#' # days for gest_age is outside the bounds of the INTERGROWTH-21st newborn
#' # size standards
#' ig_nbs_hcfga_zscore2value(z = z,
#'                           gest_age = c(140, 182, 224, 266),
#'                           sex = "F") |>
#'   round(digits = 2)
#' @importFrom vctrs vec_recycle_common
#' @importFrom gamlss.dist qST3
#' @rdname ig_nbs_percentile2value
#' @export
ig_nbs_percentile2value <- function(p, gest_age, sex, acronym) {
  max_len_vecs <- vctrs::vec_recycle_common(p = p,
                                            gest_age = gest_age,
                                            sex = sex,
                                            acronym = acronym)
  checked_params <- check_nbs_params(sex = max_len_vecs[["sex"]],
                                     gest_age = max_len_vecs[["gest_age"]],
                                     acronym = max_len_vecs[["acronym"]])
  checked_p <- max_len_vecs[["p"]]
  checked_p[which(abs(max_len_vecs[["p"]]) >= 1)] <- NA
  input <- list(p = checked_p,
                gest_age = checked_params[["age"]],
                sex = checked_params[["sex"]],
                acronym = checked_params[["acronym"]])

  fromMSNT_p2v <- function(max_len_vec_li) {
    msnt <- ig_nbs_msnt(gest_age = max_len_vec_li[["gest_age"]],
                        sex = max_len_vec_li[["sex"]],
                        acronym = max_len_vec_li[["acronym"]])
    # Remove incomplete cases or qST3() will fail
    lgl_complete <- stats::complete.cases(as.data.frame(msnt))
    msnt_no_na <- lapply(X = msnt, \(coeff) coeff[lgl_complete])
    # Initialise empty vector for y_out to go into
    y_out <- rep_len(x = NA, length.out = length(max_len_vec_li[["p"]]))
    # Calculate y values...
    y <- ifelse(
      test = max_len_vec_li[["sex"]] == "U",
      yes =  mean_if_sex_undefined(
        fn = ig_nbs_percentile2value,
        arg1 = max_len_vec_li[["p"]][lgl_complete],
        x_arg = max_len_vec_li[["gest_age"]][lgl_complete],
        acronym = max_len_vec_li[["acronym"]][lgl_complete]),
      no = gamlss.dist::qST3(max_len_vec_li[["p"]][lgl_complete],
                             mu = msnt_no_na[[1]],
                             sigma = msnt_no_na[[2]],
                             nu = msnt_no_na[[3]],
                             tau = msnt_no_na[[4]]))
    # ... then assign to indices in the vector of NAs
    suppressWarnings(y_out[lgl_complete] <- y)
    y_out
  }

  fromLM_p2v <- function(max_len_vec_li) {
    body_comp <- ig_nbs_bodycomp(x = max_len_vec_li[["gest_age"]],
                                 sex = max_len_vec_li[["sex"]],
                                 acronym = max_len_vec_li[["acronym"]])
    not_in_LM_bounds <- !inrange(max_len_vec_li[["gest_age"]], c(266, 294))
    max_len_vec_li[["p"]][not_in_LM_bounds] <- NA
    lm_out <- ifelse(
      max_len_vec_li[["sex"]] == "U",
      yes = mean_if_sex_undefined(fn = ig_nbs_percentile2value,
                                  arg1 = max_len_vec_li[["p"]],
                                  x_arg = max_len_vec_li[["gest_age"]],
                                  acronym = max_len_vec_li[["acronym"]]),
      no = mu_sigma_z2y(z = qnorm(max_len_vec_li[["p"]]),
                        mu = body_comp[,1],
                        sigma = body_comp[, 2]))
    ifelse(lm_out <= 0, yes = NA, no = lm_out)
  }
  
  fromWLR_p2v <- function(max_len_vec_li) {
    wlr <- ig_nbs_wlr(ga_weeks = max_len_vec_li[["gest_age"]] / 7,
                      sex = max_len_vec_li[["sex"]])
    ifelse(
      max_len_vec_li[["sex"]] == "U",
      yes = mean_if_sex_undefined(fn = ig_nbs_percentile2value,
                                  arg1 = max_len_vec_li[["p"]],
                                  x_arg = max_len_vec_li[["gest_age"]],
                                  acronym = max_len_vec_li[["acronym"]]),
      no = mu_sigma_z2y(z = qnorm(max_len_vec_li[["p"]]),
                        mu = wlr[["mu"]],
                        sigma = wlr[["sigma"]])
    )
  }

  vpns_lim <- 231
  out <- ifelse(
    test = input[["gest_age"]] >= vpns_lim,
    yes = ifelse(test = input[["acronym"]] %in% c("wfga", "lfga", "hcfga"),
                 yes = fromMSNT_p2v(input),
                 no = fromLM_p2v(input)),
    no = ig_vpns_zscore2value(z = qnorm(input[["p"]]),
                              gest_age = input[["gest_age"]],
                              sex = input[["sex"]],
                              acronym = input[["acronym"]]
    ))
  ifelse(test = input[["acronym"]] == "wlrfga",
         yes = fromWLR_p2v(input),
         no = out)
}

#' @rdname ig_nbs_percentile2value
#' @export
ig_nbs_wfga_percentile2value <- function(p, gest_age, sex) {
  ig_nbs_percentile2value(p = p, gest_age = gest_age, sex = sex, acronym = "wfga")
}

#' @rdname ig_nbs_percentile2value
#' @export
ig_nbs_lfga_percentile2value <- function(p, gest_age, sex) {
  ig_nbs_percentile2value(p = p, gest_age = gest_age, sex = sex, acronym = "lfga")
}

#' @rdname ig_nbs_percentile2value
#' @export
ig_nbs_hcfga_percentile2value <- function(p, gest_age, sex) {
  ig_nbs_percentile2value(p = p, gest_age = gest_age, sex = sex, acronym = "hcfga")
}

#' @rdname ig_nbs_percentile2value
#' @export
ig_nbs_wlrfga_percentile2value <- function(p, gest_age, sex) {
  ig_nbs_percentile2value(p = p, gest_age = gest_age, sex = sex, acronym = "wlrfga")
}

#' @rdname ig_nbs_percentile2value
#' @export
ig_nbs_fmfga_percentile2value <- function(p, gest_age, sex) {
  ig_nbs_percentile2value(p = p, gest_age = gest_age, sex = sex, acronym = "fmfga")
}

#' @rdname ig_nbs_percentile2value
#' @export
ig_nbs_bfpfga_percentile2value <- function(p, gest_age, sex) {
  ig_nbs_percentile2value(p = p, gest_age = gest_age, sex = sex, acronym = "bfpfga")
}

#' @rdname ig_nbs_percentile2value
#' @export
ig_nbs_ffmfga_percentile2value <- function(p, gest_age, sex) {
  ig_nbs_percentile2value(p = p, gest_age = gest_age, sex = sex, acronym = "ffmfga")
}

#' @importFrom stats pnorm
#' @rdname ig_nbs_percentile2value
#' @export
ig_nbs_zscore2value <- function(z, gest_age, sex, acronym) {
  ig_nbs_percentile2value(p = pnorm(z), gest_age = gest_age, sex = sex, acronym = acronym)
}

#' @rdname ig_nbs_percentile2value
#' @export
ig_nbs_wfga_zscore2value <- function(z, gest_age, sex) {
  ig_nbs_zscore2value(z = z, gest_age = gest_age, sex = sex, acronym = "wfga")
}

#' @rdname ig_nbs_percentile2value
#' @export
ig_nbs_lfga_zscore2value <- function(z, gest_age, sex) {
  ig_nbs_zscore2value(z = z, gest_age = gest_age, sex = sex, acronym = "lfga")
}

#' @rdname ig_nbs_percentile2value
#' @export
ig_nbs_hcfga_zscore2value <- function(z, gest_age, sex) {
  ig_nbs_zscore2value(z = z, gest_age = gest_age, sex = sex, acronym = "hcfga")
}

#' @rdname ig_nbs_percentile2value
#' @export
ig_nbs_wlrfga_zscore2value <- function(z, gest_age, sex) {
  ig_nbs_zscore2value(z = z, gest_age = gest_age, sex = sex, acronym = "wlrfga")
}

#' @rdname ig_nbs_percentile2value
#' @export
ig_nbs_fmfga_zscore2value <- function(z, gest_age, sex) {
  ig_nbs_zscore2value(z = z, gest_age = gest_age, sex = sex, acronym = "fmfga")
}

#' @rdname ig_nbs_percentile2value
#' @export
ig_nbs_bfpfga_zscore2value <- function(z, gest_age, sex) {
  ig_nbs_zscore2value(z = z, gest_age = gest_age, sex = sex, acronym = "bfpfga")
}

#' @rdname ig_nbs_percentile2value
#' @export
ig_nbs_ffmfga_zscore2value <- function(z, gest_age, sex) {
  ig_nbs_zscore2value(z = z, gest_age = gest_age, sex = sex, acronym = "ffmfga")
}

#' Convert values to z-scores/percentiles in the INTERGROWTH-21<sup>st</sup>
#' Newborn Size Standards
#'
#' @param y Value(s) to convert to either a z-score/z-scores or a
#' percentile/percentiles.
#' @param gest_age Gestational age(s) in days. Must be between `266` to `294`
#' for body composition equations (`"fmfga"`, `"bfpfga"`, or `"ffmfga"`), or
#' between `168` and `300` for the other standards.
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param acronym Acronym(s) denoting the INTERGROWTH-21<sup>st</sup> NBS
#' standard to use. Must be one of `"wfga"` (weight-for-GA), `"lfga"`
#' (length-for-GA), `"hcfga"` (head circumference-for-GA), `"wlrfga"`
#' (weight/length ratio-for-GA), `"fmfga"` (fat mass-for-GA), `"bfpfga"`
#' (body fat %-for-GA), or `"ffmfga"` (fat-free mass-for-GA).
#' @param weight_kg Birth weight(s) in kg
#' @param length_cm Birth length(s) in cm
#' @param headcirc_cm Birth head circumference(s) in cm
#' @param wei_len_ratio Weight-length ratio(s) in kg per m.
#' @param fat_mass_g Fat mass(es) in g
#' @param body_fat_perc Body fat percentage(s)
#' @param fatfree_mass_g Fat-free mass(es) in g
#' @note Input vectors will be recycled to the length of the longest vector
#' according to the rules of `vctrs::vec_recycle_common()`.
#' @return Z-scores/percentiles for each combination of measurement,
#' gestational age, sex, and acronym provided to the function.
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
#' ratios: the newborn body composition study of the INTERGROWTH-21st
#' project.** *Pediatric Research* 2017, **82:305-316.**
#' \doi{10.1038/pr.2017.52}
#' @examples
#' # Convert values to percentiles
#' ig_nbs_value2percentile(y = 3.12, gest_age = 280, sex = "M", acronym = "wfga") |>
#'   round(digits = 2)
#'
#' # Or values to z-scores
#' ig_nbs_value2zscore(y = 3.12, gest_age = 280, sex = "M", acronym = "wfga") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' ig_nbs_value2zscore(y = 48.84, gest_age = 280, sex = "F", acronym = "lfga") |>
#'   round(digits = 2)
#'
#' # ... or by using a standard-specific function
#' ig_nbs_lfga_value2zscore(length_cm = 48.84, gest_age = 280, sex = "F") |>
#'   round(digits = 2)
#'
#' # Inputs are recycled to the input of the longest length
#' ig_nbs_wlrfga_value2zscore(wei_len_ratio = c(7.37, 6.47, 6.12, 6.86),
#'                            gest_age = 280,
#'                            sex = "M") |>
#'   round(digits = 2)
#'
#' # Bad inputs will not stop the function but will instead return NA - here 301
#' # days gest_age is outside the bounds of the INTERGROWTH-21st newborn size
#' # standards
#' ig_nbs_hcfga_value2percentile(headcirc_cm = c(23.0, 28.0, 33.0, 35.0),
#'                               gest_age = c(168, 217, 266, 301),
#'                               sex = "F") |>
#'   round(digits = 2)
#' @rdname ig_nbs_value2percentile
#' @importFrom vctrs vec_recycle_common
#' @importFrom gamlss.dist pST3
#' @export
ig_nbs_value2percentile <- function(y, gest_age, sex, acronym) {
  max_len_vecs <- vctrs::vec_recycle_common(y = y,
                                            gest_age = gest_age,
                                            sex = sex,
                                            acronym = acronym)
  checked_params <- check_nbs_params(sex = max_len_vecs[["sex"]],
                                     gest_age = max_len_vecs[["gest_age"]],
                                     acronym = max_len_vecs[["acronym"]])
  input <- list(y = max_len_vecs[["y"]],
                gest_age = checked_params[["age"]],
                sex = checked_params[["sex"]],
                acronym = checked_params[["acronym"]])

  fromMSNT_v2p <- function(max_len_vec_li) {
    msnt <- ig_nbs_msnt(gest_age = max_len_vec_li[["gest_age"]],
                        sex = max_len_vec_li[["sex"]],
                        acronym = max_len_vec_li[["acronym"]])
    # Remove NA values for y or mu, or pST3() will fail
    lgl_complete <- stats::complete.cases(as.data.frame(msnt))
    msnt_no_na <- lapply(X = msnt, \(coeff) coeff[lgl_complete])
    # Initialise empty vector for p_out to go into
    p_out <- rep_len(x = NA, length.out = length(max_len_vec_li[["y"]]))
    # Calculate percentile values...
    p <- ifelse(
      test = max_len_vec_li[["sex"]] == "U",
      yes = mean_if_sex_undefined(
        fn = ig_nbs_value2percentile,
        arg1 = max_len_vec_li[["p"]][lgl_complete],
        x_arg = max_len_vec_li[["gest_age"]][lgl_complete],
        acronym = max_len_vec_li[["acronym"]][lgl_complete]),
      no = gamlss.dist::pST3(max_len_vec_li[["y"]][lgl_complete],
                             mu = msnt_no_na[[1]],
                             sigma = msnt_no_na[[2]],
                             nu = msnt_no_na[[3]],
                             tau = msnt_no_na[[4]])
    )
    # ... then assign to indices in the preallocated vector
    suppressWarnings(p_out[lgl_complete] <- p)
    p_out
  }

  fromLM_v2p <- function(max_len_vec_li) {
    body_comp <- ig_nbs_bodycomp(x = max_len_vec_li[["gest_age"]],
                                 sex = max_len_vec_li[["sex"]],
                                 acronym = max_len_vec_li[["acronym"]])
    not_in_LM_bounds <- !inrange(max_len_vec_li[["gest_age"]], c(266, 294))
    max_len_vec_li[["p"]][not_in_LM_bounds] <- NA

    ifelse(
      max_len_vec_li[["sex"]] == "U",
      yes = mean_if_sex_undefined(fn = ig_nbs_value2percentile,
                                  arg1 = max_len_vec_li[["y"]],
                                  x_arg = max_len_vec_li[["gest_age"]],
                                  acronym = max_len_vec_li[["acronym"]]),
      no = pnorm(mu_sigma_y2z(y = max_len_vec_li[["y"]],
                              mu = body_comp[,1],
                              sigma = body_comp[, 2]))
    )
  }

  fromWLR_v2p <- function(max_len_vec_li) {
    wlr <- ig_nbs_wlr(ga_weeks = max_len_vec_li[["gest_age"]] / 7,
                      sex = max_len_vec_li[["sex"]])
    wlr_out <- ifelse(
      max_len_vec_li[["sex"]] == "U",
      yes = mean_if_sex_undefined(fn = ig_nbs_percentile2value,
                                  arg1 = max_len_vec_li[["p"]],
                                  x_arg = max_len_vec_li[["gest_age"]],
                                  acronym = max_len_vec_li[["acronym"]]),
      no = pnorm(mu_sigma_y2z(y = max_len_vec_li[["y"]],
                              mu = wlr[["mu"]],
                              sigma = wlr[["sigma"]]))
    )
  }

  vpns_lim <- 231
  out <- ifelse(
    test = input[["gest_age"]] >= vpns_lim,
    yes = ifelse(test = input[["acronym"]] %in% c("wfga", "lfga", "hcfga"),
                 yes = fromMSNT_v2p(input),
                 no = fromLM_v2p(input)),
    no = pnorm(ig_vpns_value2zscore(y = input[["y"]],
                                    gest_age = input[["gest_age"]],
                                    sex = input[["sex"]],
                                    acronym = input[["acronym"]]))
  )
  ifelse(test = input[["acronym"]] == "wlrfga",
         yes = fromWLR_v2p(input),
         no = out)
}

#' @rdname ig_nbs_value2percentile
#' @export
ig_nbs_wfga_value2percentile <- function(weight_kg, gest_age, sex) {
  ig_nbs_value2percentile(y = weight_kg, gest_age = gest_age, sex = sex, acronym = "wfga")
}

#' @rdname ig_nbs_value2percentile
#' @export
ig_nbs_lfga_value2percentile <- function(length_cm, gest_age, sex) {
  ig_nbs_value2percentile(y = length_cm, gest_age = gest_age, sex = sex, acronym = "lfga")
}

#' @rdname ig_nbs_value2percentile
#' @export
ig_nbs_hcfga_value2percentile <- function(headcirc_cm, gest_age, sex) {
  ig_nbs_value2percentile(y = headcirc_cm, gest_age = gest_age, sex = sex, acronym = "hcfga")
}

#' @rdname ig_nbs_value2percentile
#' @export
ig_nbs_wlrfga_value2percentile <- function(wei_len_ratio, gest_age, sex) {
  ig_nbs_value2percentile(y = wei_len_ratio, gest_age = gest_age, sex = sex, acronym = "wlrfga")
}

#' @rdname ig_nbs_value2percentile
#' @export
ig_nbs_fmfga_value2percentile <- function(fat_mass_g, gest_age, sex) {
  ig_nbs_value2percentile(y = fat_mass_g, gest_age = gest_age, sex = sex, acronym = "fmfga")
}

#' @rdname ig_nbs_value2percentile
#' @export
ig_nbs_bfpfga_value2percentile <- function(body_fat_perc, gest_age, sex) {
  ig_nbs_value2percentile(y = body_fat_perc, gest_age = gest_age, sex = sex, acronym = "bfpfga")
}

#' @rdname ig_nbs_value2percentile
#' @export
ig_nbs_ffmfga_value2percentile <- function(fatfree_mass_g, gest_age, sex) {
  ig_nbs_value2percentile(y = fatfree_mass_g, gest_age = gest_age, sex = sex, acronym = "ffmfga")
}

#' @importFrom stats qnorm
#' @rdname ig_nbs_value2percentile
#' @export
ig_nbs_value2zscore <- function(y, gest_age, sex, acronym) {
  qnorm(ig_nbs_value2percentile(y = y, gest_age = gest_age, sex = sex, acronym = acronym))
}

#' @rdname ig_nbs_value2percentile
#' @export
ig_nbs_wfga_value2zscore <- function(weight_kg, gest_age, sex) {
  ig_nbs_value2zscore(y = weight_kg, gest_age = gest_age, sex = sex, acronym = "wfga")
}

#' @rdname ig_nbs_value2percentile
#' @export
ig_nbs_lfga_value2zscore <- function(length_cm, gest_age, sex) {
  ig_nbs_value2zscore(y = length_cm, gest_age = gest_age, sex = sex, acronym = "lfga")
}

#' @rdname ig_nbs_value2percentile
#' @export
ig_nbs_hcfga_value2zscore <- function(headcirc_cm, gest_age, sex) {
  ig_nbs_value2zscore(y = headcirc_cm, gest_age = gest_age, sex = sex, acronym = "hcfga")
}

#' @rdname ig_nbs_value2percentile
#' @export
ig_nbs_wlrfga_value2zscore <- function(wei_len_ratio, gest_age, sex) {
  ig_nbs_value2zscore(y = wei_len_ratio, gest_age = gest_age, sex = sex, acronym = "wlrfga")
}

#' @rdname ig_nbs_value2percentile
#' @export
ig_nbs_fmfga_value2zscore <- function(fat_mass_g, gest_age, sex) {
  ig_nbs_value2zscore(y = fat_mass_g, gest_age = gest_age, sex = sex, acronym = "fmfga")
}

#' @rdname ig_nbs_value2percentile
#' @export
ig_nbs_bfpfga_value2zscore <- function(body_fat_perc, gest_age, sex) {
  ig_nbs_value2zscore(y = body_fat_perc, gest_age = gest_age, sex = sex, acronym = "bfpfga")
}

#' @rdname ig_nbs_value2percentile
#' @export
ig_nbs_ffmfga_value2zscore <- function(fatfree_mass_g, gest_age, sex) {
  ig_nbs_value2zscore(y = fatfree_mass_g, gest_age = gest_age, sex = sex, acronym = "ffmfga")
}

#' Retrieve GAMLSS coefficients for INTERGROWTH-21<sup>st</sup> Newborn Size
#' standards
#'
#' @description Retrieves mu/sigma/nu/tau values for GAMLSS-based calculation of
#' z-scores/percentiles in the INTERGROWTH-21<sup>st</sup> Newborn Size
#' standards.
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param gest_age Gestational age(s) in days. Must be between `231` and `300`.
#' @param acronym Acronym(s) denoting which GAMLSS-based
#' INTERGROWTH-21<sup>st</sup> standard to use. Must be one of `"wfga"`,
#' `"lfga"`, or `"hcfga"`.
#' @return A dataframe containing mu, sigma, nu, and tau values for each
#' provided combination of sex, gestational age, and acronym.
#' @note These coefficients are not included in the referenced publication, and
#' were instead supplied directly by Eric Ohuma. However, Villar *et al.* used
#' these coefficients to construct the growth curves they described, and in
#' testing we found these coefficients output the INTERGROWTH-21<sup>st</sup>
#' standards exactly once rounded to the correct number of decimal places.
#' @references
#' Villar J, Cheikh Ismail L, Victora CG, Ohuma EO, Bertino E, Altman DG, et al.
#' **International standards for newborn weight, length, and head circumference
#' by gestational age and sex: the Newborn Cross-Sectional Study of the
#' INTERGROWTH-21st Project.** *Lancet* 2014, **384(9946):857-68.**
#' \doi{10.1016/S0140-6736(14)60932-6}
#' @examples
#' # Get mu/sigma/nu/tau coefficients for weight in a male of 40 weeks'
#' # gestational age
#' gigs:::ig_nbs_msnt(gest_age = 40 * 7, sex = "M", acronym = "wfga")
#' @rdname ig_nbs_msnt
#' @keywords internal
#' @noRd
ig_nbs_msnt <- function(gest_age, sex, acronym) {
  retrieve_coefficients(gest_age, sex, acronym, gigs::ig_nbs_coeffs,
                        c("mu", "sigma", "nu", "tau"))
}

#' INTERGROWTH-21<sup>st</sup> weight-to-length ratio means/standard
#' deviations
#'
#' @param ga_weeks Gestational age(s) in weeks. Must be between `24` and `42 +
#' 6/7`.
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @return Weight-to-length ratio medians and standard deviations for the given
#' `sex`/`gest_age` combinations.
#' @note These equations are not included in the referenced publication. Rather,
#' they were taken from weight-to-length ratio calculating Excel files available
#' on the
#' [INTERGROWTH-21<sup>st</sup> website](https://intergrowth21.tghn.org/newborn-size-birth/#c4).
#' @references
#' Villar J, Puglia FA, Fenton TR, Ismal LC, Staines-Urias E, Giuliani F, et al.
#' **Body composition at birth and its relationship with neonatal anthropometric
#' ratios: the newborn body composition study of the INTERGROWTH-21st project.**
#' *Pediatric Research* 2017, **82:305-316.**
#' \doi{10.1038/pr.2017.52}
#' @rdname ig_nbs_wlr
#' @keywords internal
#' @noRd
ig_nbs_wlr <- function(ga_weeks, sex) {
  sex_as_numeric <- ifelse(sex == "M", yes = 1, no = 0)
  mu <- ifelse(
    test = ga_weeks < 33,
    yes = 3.400617 + (-0.0103163 * ga_weeks ^ 2) + (0.0003407 * ga_weeks ^ 3) +
      (0.1382809 * sex_as_numeric),
    no = ifelse(
      test = sex == "M",
      yes = -17.84615 + (-3778.768 * (ga_weeks ^ -1)) +
        (1291.477 * ((ga_weeks ^ -1) * log(ga_weeks))),
      no = -5.542927 + (0.0018926 * (ga_weeks ^ 3)) +
        (-0.0004614 * ((ga_weeks ^ 3)* log(ga_weeks)))
    )
  )
  sigma <- ifelse(
    test = ga_weeks < 33,
    yes = sqrt(x = 0.3570057),
    no = ifelse(
      test = sex == "M",
      yes = 1.01047 + (-0.0080948 * ga_weeks),
      no = 0.6806229
    )
  )
  data.frame(gest_age = ga_weeks * 7, sex, mu = mu, sigma = sigma)
}

#' INTERGROWTH-21<sup>st</sup> normative body composition means/standard
#' deviations
#'
#' @param x Gestation age in days at which to calculate mu (mean) and sigma
#' (SD). Should be between `266` and `294`.
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param acronym Acronym(s) denoting the INTERGROWTH-21<sup>st</sup> NBS
#' normative body composition standard to use. Must be one of `"fmfga"`,
#' `"bfpfga"`, or `"ffmfga"`.
#' @return A matrix with means and standard deviations for each gestational
#' age/sex/acronym combination.
#' @note These parameters are not included in the referenced publication, but
#' the associated supplementary materials. We used tables S1, S2 and S3 and
#' `lm()` to derive the equations. This process can be seen in the
#' INTERGROWTH-21st body composition vignette or the source code of
#' `data-raw/ig_nbs_bc.R`. As a result, z-scores/percentiles derived from these
#' models differ slightly from Villar *et al.*'s published values.
#' @references
#' Villar J, Puglia FA, Fenton TR, Ismal LC, Staines-Urias E, Giuliani F, et al.
#' **Body composition at birth and its relationship with neonatal anthropometric
#' ratios: the newborn body composition study of the INTERGROWTH-21st
#' project.** *Pediatric Research* 2017, **82:305-316.**
#' \doi{10.1038/pr.2017.52}
#' @rdname ig_nbs_bodycomp
#' @keywords internal
#' @noRd
ig_nbs_bodycomp <- function(x, sex, acronym) {
  acronym_sex <- paste0(acronym, "_", sex)
  # `ig_nbs_bc_li` is an internal list with regression equation parameters for
  # the normative body composition standards, which you can check out in
  # `data-raw/ig_nbs_bc.R` or the INTERGROWTH-21st body composition vignette
  params_li <- ig_nbs_bc_li[acronym_sex]
  params_null <- vapply(params_li, is.null, FUN.VALUE = logical(length = 1L))
  params_li[params_null] <- rep(list(rep(NA, 5)), sum(params_null))

  # Unlist is a relative bottleneck here, could refactor later *if* too slow
  params <- matrix(unlist(params_li, recursive = FALSE, use.names = FALSE),
                   ncol = length(params_li), nrow = 5)
  mu <- params[1,] + params[2,] * x + params[3,] * x^2 + params[4,] * x^3
  matrix(c(mu, params[5,]), nrow = length(mu), ncol = 2,
         dimnames = list(NULL, c("mu", "sigma")))
}