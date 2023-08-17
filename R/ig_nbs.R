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
#' @importFrom gamlss.dist qST3
#' @importFrom vctrs vec_recycle_common
#' @rdname ig_nbs_percentile2value
#' @export
ig_nbs_percentile2value <- function(p, gest_age, sex, acronym) {
  max_len_vecs <- vctrs::vec_recycle_common(p = p,
                                            gest_age = gest_age,
                                            sex = sex,
                                            acronym = acronym)
  checked_params <- check_nbs_params(sex = max_len_vecs$sex, gest_age = max_len_vecs$gest_age, acronym = max_len_vecs$acronym)
  checked_p <- max_len_vecs$p
  checked_p[which(abs(max_len_vecs$p) >= 1)] <- NA
  input <- list(p = checked_p, gest_age = checked_params$age, sex = checked_params$sex, acronym = checked_params$acronym)

  fromMSNT_p2v <- function(max_len_vec_li) {
    msnt <- ig_nbs_msnt(sex = max_len_vec_li$sex, gest_age = max_len_vec_li$gest_age, acronym = max_len_vec_li$acronym)
    msnt <- cbind(p = max_len_vec_li$p, msnt, n_ = seq(1, nrow(msnt)))
    msnt_no_na <- msnt[which(!is.na(msnt$p) & !is.na(msnt$mu)), ]
    msnt_no_na$out <- ifelse(
      test = msnt_no_na$sex == "U",
      yes =  mean_if_sex_undefined(fn = ig_nbs_percentile2value,
                                   arg1 = msnt_no_na$p,
                                   x_arg = msnt_no_na$gest_age,
                                   acronym = msnt_no_na$acronym),
      no = gamlss.dist::qST3(msnt_no_na$p,
                             mu = msnt_no_na$mu,
                             sigma = msnt_no_na$sigma,
                             nu = msnt_no_na$nu,
                             tau = msnt_no_na$tau)
    )
    merged <- merge(msnt, msnt_no_na, all.x = TRUE)
    merged_ordered <- merged[order(merged$n_), ]
    msnt_out <- merged_ordered$out
    return(msnt_out)
  }

  fromLM_p2v <- function(max_len_vec_li) {
    body_comp <- ig_nbs_bodycomp(sex = max_len_vec_li$sex,
                                 acronym = max_len_vec_li$acronym)
    ga_in_range <- max_len_vec_li$gest_age < 266 | max_len_vec_li$gest_age > 294
    max_len_vec_li$p[ga_in_range] <- NA
    lm_out <- ifelse(
      max_len_vec_li$sex == "U",
      yes = mean_if_sex_undefined(fn = ig_nbs_percentile2value,
                                  arg1 = max_len_vec_li$p,
                                  x_arg = max_len_vec_li$gest_age,
                                  acronym = max_len_vec_li$acronym),
      no = body_comp$y_intercept + body_comp$ga_coeff * (max_len_vec_li$gest_age / 7) +
             qnorm(max_len_vec_li$p) * body_comp$std_dev)
    ifelse(lm_out <= 0, yes = NA, no = lm_out)
  }
  
  fromWLR_p2v <- function(max_len_vec_li) {
    wlr <- ig_nbs_wlr(ga_weeks = max_len_vec_li$gest_age / 7,
                      sex = max_len_vec_li$sex)
    wlr_out <- ifelse(
      max_len_vec_li$sex == "U",
      yes = mean_if_sex_undefined(fn = ig_nbs_percentile2value,
                                  arg1 = max_len_vec_li$p,
                                  x_arg = max_len_vec_li$gest_age,
                                  acronym = max_len_vec_li$acronym),
      no = qnorm(max_len_vec_li$p) * wlr$sigma + wlr$mu
    )
  }

  vpns_lim <- 231
  out <- ifelse(
    test = input$gest_age >= vpns_lim,
    yes = ifelse(test = input$acronym %in% c("wfga", "lfga", "hcfga"),
                 yes = fromMSNT_p2v(input),
                 no = fromLM_p2v(input)),
    no = ig_vpns_zscore2value(z = qnorm(input$p), gest_age = input$gest_age,
                              sex = input$sex, acronym = input$acronym
    ))
  ifelse(test = input$acronym == "wlrfga", yes = fromWLR_p2v(input), no = out)
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
#' @importFrom gamlss.dist pST3
#' @importFrom vctrs vec_recycle_common
#' @export
ig_nbs_value2percentile <- function(y, gest_age, sex, acronym) {
  max_len_vecs <- vctrs::vec_recycle_common(y = y, gest_age = gest_age, sex = sex, acronym = acronym)
  checked_params <- check_nbs_params(sex = max_len_vecs$sex, gest_age = max_len_vecs$gest_age, acronym = max_len_vecs$acronym)
  input <- list(y = y, gest_age = checked_params$age, sex = checked_params$sex, acronym = checked_params$acronym)

  fromMSNT_v2p <- function(max_len_vec_li) {
    msnt <- ig_nbs_msnt(sex = max_len_vec_li$sex, gest_age = max_len_vec_li$gest_age, acronym = max_len_vec_li$acronym)
    msnt <- cbind(y = max_len_vec_li$y, msnt, n_ = seq(1, nrow(msnt)))
    msnt_no_na <- msnt[which(!is.na(msnt$y) & !is.na(msnt$mu)), ]
    msnt_no_na$out <- ifelse(
      test = msnt_no_na$sex == "U",
      yes = mean_if_sex_undefined(fn = ig_nbs_value2percentile, arg1 = msnt_no_na$y,
                                  x_arg = msnt_no_na$gest_age, acronym = msnt_no_na$acronym),
      no = gamlss.dist::pST3(msnt_no_na$y, mu = msnt_no_na$mu, sigma = msnt_no_na$sigma, nu = msnt_no_na$nu, tau = msnt_no_na$tau)
    )
    merged <- merge(msnt, msnt_no_na, all.x = TRUE)
    merged_ordered <- merged[order(merged$n_), ]
    msnt_out <- merged_ordered$out
    return(msnt_out)
  }

  fromLM_v2p <- function(max_len_vec_li) {
    body_comp <- ig_nbs_bodycomp(sex = max_len_vec_li$sex, acronym = max_len_vec_li$acronym)
    max_len_vec_li$p[max_len_vec_li$gest_age < 266 | max_len_vec_li$gest_age > 294] <- NA
    ifelse(
      max_len_vec_li$sex == "U",
      yes = mean_if_sex_undefined(fn = ig_nbs_value2percentile, arg1 = max_len_vec_li$y,
                                  x_arg = max_len_vec_li$gest_age, acronym = max_len_vec_li$acronym),
      no = pnorm((max_len_vec_li$y - body_comp$y_intercept - body_comp$ga_coeff * (max_len_vec_li$gest_age / 7)) / body_comp$std_dev)
    )
  }
  
  fromWLR_v2p <- function(max_len_vec_li) {
    wlr <- ig_nbs_wlr(ga_weeks = max_len_vec_li$gest_age / 7, sex = max_len_vec_li$sex)
    wlr_out <- ifelse(
      max_len_vec_li$sex == "U",
      yes = mean_if_sex_undefined(fn = ig_nbs_percentile2value, arg1 = max_len_vec_li$p,
                                  x_arg = max_len_vec_li$gest_age, acronym = max_len_vec_li$acronym),
      no = pnorm((max_len_vec_li$y - wlr$mu) / wlr$sigma)
    )
  }

  vpns_lim <- 231
  out <- ifelse(test = input$gest_age >= vpns_lim,
         yes = ifelse(test = input$acronym %in% c("wfga", "lfga", "hcfga"),
                      yes = fromMSNT_v2p(input),
                      no = fromLM_v2p(input)),
         no = pnorm(
           ig_vpns_value2zscore(y = input$y, gest_age = input$gest_age, sex = input$sex, acronym = input$acronym)
         )
  )
  ifelse(test = input$acronym == "wlrfga", yes = fromWLR_v2p(input), no = out)
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
#' standards
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
  new_df <- data.frame(gest_age = gest_age,
                       sex = sex,
                       acronym = acronym)
  new_df$sort <- seq_along(new_df$gest_age)
  load_msnt_with_sex_acronym <- function(sex, acronym) {
    coeff_tbl <- gigs::ig_nbs_coeffs[[acronym]][[sex]]
    cbind(coeff_tbl,
          sex = rep(ifelse(sex == "male", yes = "M", no = "F"),
                    length(coeff_tbl)),
          acronym = rep(acronym, length(coeff_tbl)))
  }
  sexes_to_load <- intersect(c("male", "female"),
                             ifelse(sex == "M", yes = "male", no = "female"))
  acronyms_to_load <- intersect(names(gigs::ig_nbs_coeffs), unique(acronym))
  tot_len <- length(sexes_to_load) * length(acronyms_to_load)
  ig_nbs_coeffs_long <- do.call(
    rbind,
    mapply(x = rep_len(sexes_to_load, length.out = tot_len),
           y = rep_len(acronyms_to_load, length.out = tot_len),
           SIMPLIFY = F,
           FUN = function(x, y) load_msnt_with_sex_acronym(sex = x,
                                                           acronym = y)))

  not_in_coeff_tbl <- which(!gest_age %in% ig_nbs_coeffs_long$gest_age &
                              gest_age >= 231)
  if (length(not_in_coeff_tbl)) {
    in_coeff_tbl <- which(!seq_along(gest_age) %in% not_in_coeff_tbl)
    not_in_coeff_tbl_df <- new_df[not_in_coeff_tbl, ]
    coeffs_interpolated <- interpolate_coeffs(
      ig_nbs_coeffs_long,
      xvars = not_in_coeff_tbl_df$gest_age,
      sex = not_in_coeff_tbl_df$sex,
      acronym = not_in_coeff_tbl_df$acronym
    ) |>
      merge(not_in_coeff_tbl_df)
  } else {
    in_coeff_tbl <- seq_along(gest_age)
  }

  merge_df <- new_df[in_coeff_tbl, ]
  out <- merge(merge_df, ig_nbs_coeffs_long, all.x = TRUE, sort = FALSE)
  if (exists(x = "coeffs_interpolated")) {
    out <- rbind(out, coeffs_interpolated)
  }
  out <- out[order(out$sort), ]
  out[, -which(names(out) == "sort")]
}

#' INTERGROWTH-21<sup>st</sup> weight-to-length ratio medians/standard
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

#' INTERGROWTH-21<sup>st</sup> body composition equation parameters
#'
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param acronym Acronym(s) denoting the INTERGROWTH-21<sup>st</sup> NBS body
#' composition standard to use. Must be one of `"fmfga"`, `"bfpfga"`, or
#' `"ffmfga"`.
#' @return Body composition equation parameters for each provided
#' age/sex/acronym combination.
#' @note These parameters are not included in the referenced publication, but
#' the associated supplementary materials. We used tables S1, S2 and S3 and
#' linear models to derive the equations. As a result, z-scores/percentiles
#' derived from these parameters differ slightly from the Villar *et al.*'s
#' published values.
#' @references
#' Villar J, Puglia FA, Fenton TR, Ismal LC, Staines-Urias E, Giuliani F, et al.
#' **Body composition at birth and its relationship with neonatal anthropometric
#' ratios: the newborn body composition study of the INTERGROWTH-21st
#' project.** *Pediatric Research* 2017, **82:305-316.**
#' \doi{10.1038/pr.2017.52}
#' @rdname ig_nbs_bodycomp
#' @keywords internal
#' @noRd
ig_nbs_bodycomp <- function(sex, acronym) {
  new_df <- data.frame(sex = sex, acronym = acronym,
                       sort = seq(from = 1, to = length(sex)))
  bodycomp_params <- data.frame(
      sex = c(rep("M", 3), rep("F", 3)),
      acronym = c("fmfga", "bfpfga", "ffmfga"),
      y_intercept = c(-1134.2, -17.68, -2487.6, -840.2, -9.02, -1279),
      ga_coeff = c(37.2, 0.69, 139.9, 30.7, 0.51, 105.3),
      std_dev = c(152.1593, 3.6674, 276.2276, 156.8411, 3.9405, 260.621)
  )
  out <- merge(new_df, bodycomp_params, all.x = TRUE, sort = FALSE)
  out <- out[order(out$sort), ]
  out <- out[, -which(names(out) == "sort")]
  return(out)
}