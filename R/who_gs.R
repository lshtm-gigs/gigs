#' Convert z-score/percentiles to WHO growth standards values
#'
#' @param z,p Z-score(s)/percentile(s) to convert from.
#' @param x X value at which to convert z-score to a value. Must be within bounds of available x values for given
#' acronym.
#' @param sex Sex(es), either `"M"` (male) or `"F"`  (female).
#' @param acronym Valid acronym for WHO Growth Standards datasets: one of `"wfa"` (weight-for-age), `"bfa"`
#' (bmi-for-age), `"lhfa"` (length/height-for-age), `"wfl"` (weight-for-length), `"wfh"`
#' (weight-for-height), `"hcfa"` (head circumference-for-age), `"acfa"` (arm circumference-for-age),
#' `"ssfa"` (subscapular skinfold-for-age), or `"tsfa"` (triceps skinfold-for-age),
#'
#' @references
#' de Onis M, Garza C, Victora CG, Onyango AW, Frongillo EA, Martines J. **The WHO Multicentre Growth Reference
#' Study: planning, study design, and methodology** *Food Nutr Bull.* 2004, **25(1 Suppl):S15-26.**
#' doi: [10.1177/15648265040251s104](https://journals.sagepub.com/doi/10.1177/15648265040251S104)
#'
#' de Onis M *et al.* (2006) **'COMPUTATION OF CENTILES AND Z-SCORES FOR LENGTH/HEIGHT-FOR-AGE,
#' WEIGHT-FOR-AGE, WEIGHT-FOR-LENGTH, WEIGHT-FOR-HEIGHT AND BMI-FOR-AGE'** *in* World Health Organization (ed.)
#' *WHO Child Growth Standards: length/height-for-age, weight-for-age, weight-for-length, weight-for-height and
#' body mass index-for-age: methods and development*. Geneva: World Health Organization, pp. 301-304.
#'
#' Cole TJ **The LMS method for constructing normalized growth standards** *Eur J Clin Nutr.* 1990,
#' **44(1):45-60.** PMID: [2354692](https://pubmed.ncbi.nlm.nih.gov/2354692/)
#'
#' @examples
#' # Convert percentiles to values
#' p <- 0.25 # 25th percentile
#' who_gs_percentile2value(p = p, x = 501, sex = "M", acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Or z-scores to values
#' z <- qnorm(p)
#' who_gs_zscore2value(z = z, x = 501, sex = "M", acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' who_gs_zscore2value(z = z, x = 300, sex = "M", acronym = "lhfa") |>
#'   round(digits = 2)
#'
#' # ... or by using a standard-specific function
#' who_gs_lhfa_zscore2value(z = z, x = 300, sex = "M") |>
#'   round(digits = 2)
#'
#' # Inputs are recycled to the input of the longest length
#' who_gs_ssfa_percentile2value(p = seq(0.1, 0.9, by = 0.2),
#'                              x = 670,
#'                              sex = "M") |>
#'   round(digits = 2)
#'
#' # Bad inputs will not stop the function but will instead return NA - here 60 cm
#' # for height_cm is incompatible with the WHO Growth Standard for weight-for-height
#' who_gs_wfh_zscore2value(z = 0,
#'                         x = c(60, 85, 105, 120),
#'                         sex = "M") |>
#'   round(digits = 2)
#' @rdname who_gs_zscore2value
#' @export
who_gs_zscore2value <- function(z, x, sex, acronym) {
  max_len_vecs <- rep_to_longest(list(z = z, x = x, sex = sex, acronym = acronym))
  lms <- who_gs_lms(x = max_len_vecs$x, sex = max_len_vecs$sex, acronym = max_len_vecs$acronym)

  # from https://stackoverflow.com/questions/29920302/raising-vector-with-negative-numbers-to-a-fractional-exponent-in-r
  exponent <- function(a, pow) (abs(a) ^ pow) * sign(a)

  z_over_three <- function(l, m, s, z) {
    sd3pos <- who_gs_lms2sd(l = l, m = m, s = s, n_sd = 3)
    sd23pos <- sd3pos - who_gs_lms2sd(l = l, m = m, s = s, n_sd = 2)
    (z - 3) * sd23pos + sd3pos
  }

  z_under_minus_three <- function(l, m, s, z) {
    sd3neg <- who_gs_lms2sd(l = l, m = m, s = s, n_sd = -3)
    sd23neg <- who_gs_lms2sd(l = l, m = m, s = s, n_sd = -2) - sd3neg
    (z + 3) * sd23neg + sd3neg
  }

  y_from_LMS <- function(l, m, s, z, acronym) {
    ifelse(
      test = abs(z) <= 3 | acronym %in% c("hcfa", "lhfa"),
      yes = ifelse(
        test = l != 0,
        yes = exponent(z * s * l + 1, (1 / l)) * m,
        no = m * exp(s * z)),
      no = ifelse(
        test = z > 3,
        yes = z_over_three(l, m, s, z),
        no = z_under_minus_three(l, m, s, z))
    )
  }

  ifelse(max_len_vecs$sex == "U",
         yes = mean_if_sex_undefined(who_gs_zscore2value,
                                     arg1 = z,
                                     x_arg = max_len_vecs$x,
                                     acronym = max_len_vecs$acronym),
         no = y_from_LMS(lms$L, lms$M, lms$S, max_len_vecs$z, max_len_vecs$acronym))
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_wfa_zscore2value <- function(z, x, sex) {
  who_gs_zscore2value(z = z, x = x, sex = sex, acronym = "wfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_bfa_zscore2value <- function(z, x, sex) {
  who_gs_zscore2value(z = z, x = x, sex = sex, acronym = "bfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_lhfa_zscore2value <- function(z, x, sex) {
  who_gs_zscore2value(z = z, x = x, sex = sex, acronym = "lhfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_wfl_zscore2value <- function(z, x, sex) {
  who_gs_zscore2value(z = z, x = x, sex = sex, acronym = "wfl")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_wfh_zscore2value <- function(z, x, sex) {
  who_gs_zscore2value(z = z, x = x, sex = sex, acronym = "wfh")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_hcfa_zscore2value <- function(z, x, sex) {
  who_gs_zscore2value(z = z, x = x, sex = sex, acronym = "hcfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_acfa_zscore2value <- function(z, x, sex) {
  who_gs_zscore2value(z = z, x = x, sex = sex, acronym = "acfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_ssfa_zscore2value <- function(z, x, sex) {
  who_gs_zscore2value(z = z, x = x, sex = sex, acronym = "ssfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_tsfa_zscore2value <- function(z, x, sex) {
  who_gs_zscore2value(z = z, x = x, sex = sex, acronym = "tsfa")
}

#' @rdname who_gs_zscore2value
#' @importFrom stats qnorm
#' @export
who_gs_percentile2value <- function(p, x, sex, acronym) {
  who_gs_zscore2value(z = qnorm(p), x = x, sex = sex, acronym = acronym)
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_wfa_percentile2value <- function(p, x, sex) {
  who_gs_percentile2value(p = p, x = x, sex = sex, acronym = "wfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_bfa_percentile2value <- function(p, x, sex) {
  who_gs_percentile2value(p = p, x = x, sex = sex, acronym = "bfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_lhfa_percentile2value <- function(p, x, sex)  {
  who_gs_percentile2value(p = p, x = x, sex = sex, acronym = "lhfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_wfl_percentile2value <- function(p, x, sex) {
  who_gs_percentile2value(p = p, x = x, sex = sex, acronym = "wfl")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_wfh_percentile2value <- function(p, x, sex) {
  who_gs_percentile2value(p = p, x = x, sex = sex, acronym = "wfh")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_hcfa_percentile2value <- function(p, x, sex) {
  who_gs_percentile2value(p = p, x = x, sex = sex, acronym = "hcfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_acfa_percentile2value <- function(p, x, sex) {
  who_gs_percentile2value(p = p, x = x, sex = sex, acronym = "acfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_ssfa_percentile2value <- function(p, x, sex) {
  who_gs_percentile2value(p = p, x = x, sex = sex, acronym = "ssfa")
}

#' @rdname who_gs_zscore2value
#' @export
who_gs_tsfa_percentile2value <- function(p, x, sex) {
  who_gs_percentile2value(p = p, x = x, sex = sex, acronym = "tsfa")
}

#' Convert anthropometric measures to WHO Growth Standards z-scores
#'
#' @param y Y value(s) for growth standard.
#' @param x X value(s) for growth standard.
#' @param sex Sex(es), either `"M"` (male) or `"F"`  (female).
#' @param acronym Valid acronym for WHO Growth Standards datasets: one of `"wfa"` (weight-for-age), `"bfa"`
#' (bmi-for-age), `"lhfa"` (length/height-for-age), `"wfl"` (weight-for-length), or `"wfh"`
#' (weight-for-height), `"hcfa"` (head circumference-for-age), `"acfa"` (arm circumference-for-age),
#' `"ssfa"` (subscapular skinfold-for-age), or `"tsfa"` (triceps skinfold-for-age),
#' @param weight_kg Weight measurement(s) in kg.
#' @param age_days Age since birth in days.
#' @param bmi Body mass index measurement(s).
#' @param lenht_cm Length/height measurement(s) in cm.
#' @param length_cm Recumbent length measurement(s) in cm.
#' @param height_cm Standing height measurement(s) in cm.
#' @param headcirc_cm Head circumference measurement(s) in cm.
#' @param armcirc_cm Arm circumference measurement(s) in cm.
#' @param subscap_sf_mm Subscapular skinfold measurement(s) in mm.
#' @param triceps_sf_mm Triceps skinfold measurement(s) in mm.
#' @references
#' de Onis M, Garza C, Victora CG, Onyango AW, Frongillo EA, Martines J. **The WHO Multicentre Growth Reference
#' Study: planning, study design, and methodology** *Food Nutr Bull.* 2004, **25(1 Suppl):S15-26.**
#' doi: [10.1177/15648265040251s104](https://journals.sagepub.com/doi/10.1177/15648265040251S104)
#'
#' de Onis M *et al.* (2006) **'COMPUTATION OF CENTILES AND Z-SCORES FOR LENGTH/HEIGHT-FOR-AGE,
#' WEIGHT-FOR-AGE, WEIGHT-FOR-LENGTH, WEIGHT-FOR-HEIGHT AND BMI-FOR-AGE'** *in* World Health Organization (ed.)
#' *WHO Child Growth Standards: length/height-for-age, weight-for-age, weight-for-length, weight-for-height and
#' body mass index-for-age: methods and development*. Geneva: World Health Organization, pp. 301-304.
#'
#' Cole TJ **The LMS method for constructing normalized growth standards** *Eur J Clin Nutr.* 1990,
#' **44(1):45-60.** PMID: [2354692](https://pubmed.ncbi.nlm.nih.gov/2354692/)
#'
#' @examples
#' # Convert values to percentiles
#' who_gs_value2percentile(y = 10.1, x = 505, sex = "M", acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Or values to z-scores
#' who_gs_value2zscore(y = 10.1, x = 505, sex = "M", acronym = "wfa") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' who_gs_value2percentile(y = 75.2, x = 300, sex = "M", acronym = "lhfa") |>
#'   round(digits = 2)
#'
#' # ... or by using a standard-specific function
#' who_gs_lhfa_value2percentile(lenht_cm = 75.2, age_days = 300, sex = "M") |>
#'   round(digits = 2)
#'
#' # Inputs are recycled to the input of the longest length
#' who_gs_ssfa_value2percentile(subscap_sf_mm = 6.1,
#'                              age_days = seq(450, 650, by = 50),
#'                              sex = "M") |>
#'   round(digits = 2)
#'
#' # Bad inputs will not stop the function but will instead return NA - here 121 cm
#' # height_cm is incompatible with the WHO Growth Standard for weight-for-height
#' who_gs_wfh_value2zscore(weight_kg = c(7.4, 14.2, 21.8, 22.4),
#'                         height_cm = c(65, 95, 120, 121),
#'                         sex = "M") |>
#'   round(digits = 2)
#' @rdname who_gs_value2zscore
#' @export
who_gs_value2zscore <- function(y, x, sex, acronym) {
  max_len_vecs <- rep_to_longest(list(y = y, x = x, sex = sex, acronym = acronym))
  lms <- who_gs_lms(x = max_len_vecs$x, sex = max_len_vecs$sex, acronym = max_len_vecs$acronym)

  z_over_three <- function(l, m, s, y) {
    sd3pos <-  who_gs_lms2sd(l = l, m = m, s = s, n_sd = 3)
    sd23pos <- sd3pos - who_gs_lms2sd(l = l, m = m, s = s, n_sd = 2)
    3 + (y - sd3pos) / sd23pos
  }
  z_under_minus_three <- function(l, m, s, y) {
    sd3neg <-  who_gs_lms2sd(l = l, m = m, s = s, n_sd = -3)
    sd23neg <- who_gs_lms2sd(l = l, m = m, s = s, n_sd = -2) - sd3neg
    -3 + (y - sd3neg) / sd23neg
  }

  z_from_LMS <- function(l, m, s, y, acronym) {
    z <- ifelse(test = l != 0,
                yes = (abs((y / m) ^ l) - 1) / (s * l),
                no = log(y / m) / s)
    ifelse(
      test = abs(z) <= 3 | acronym %in% c("hcfa", "lhfa"),
      yes = z,
      no = ifelse(test = z > 3,
                  yes = z_over_three(l, m, s, y),
                  no =  z_under_minus_three(l, m, s, y))
    )
  }

  ifelse(max_len_vecs$sex == "U",
         yes = mean(c(
           who_gs_value2zscore(y = max_len_vecs$y, x = lms$x, sex = "M", acronym = max_len_vecs$acronym),
           who_gs_value2zscore(y = max_len_vecs$y, x = lms$x, sex = "F", acronym = max_len_vecs$acronym)
         )),
         no = z_from_LMS(lms$L, lms$M, lms$S, max_len_vecs$y, max_len_vecs$acronym))
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_wfa_value2zscore <- function(weight_kg, age_days, sex) {
  who_gs_value2zscore(y = weight_kg, x = age_days, sex = sex, acronym = "wfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_bfa_value2zscore <- function(bmi, age_days, sex) {
  who_gs_value2zscore(y = bmi, x = age_days, sex = sex, acronym = "bfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_lhfa_value2zscore <- function(lenht_cm, age_days, sex) {
  who_gs_value2zscore(y = lenht_cm, x = age_days, sex = sex, acronym = "lhfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_wfl_value2zscore <- function(weight_kg, length_cm, sex) {
  who_gs_value2zscore(y = weight_kg, x = length_cm, sex = sex, acronym = "wfl")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_wfh_value2zscore <- function(weight_kg, height_cm, sex) {
  who_gs_value2zscore(y = weight_kg, x = height_cm, sex = sex, acronym = "wfh")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_hcfa_value2zscore <- function(headcirc_cm, age_days, sex) {
  who_gs_value2zscore(y = headcirc_cm, x = age_days, sex = sex, acronym = "hcfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_acfa_value2zscore <- function(armcirc_cm, age_days, sex) {
  who_gs_value2zscore(y = armcirc_cm, x = age_days, sex = sex, acronym = "acfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_ssfa_value2zscore <- function(subscap_sf_mm, age_days, sex) {
  who_gs_value2zscore(y = subscap_sf_mm, x = age_days, sex = sex, acronym = "ssfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_tsfa_value2zscore <- function(triceps_sf_mm, age_days, sex) {
  who_gs_value2zscore(y = triceps_sf_mm, x = age_days, sex = sex, acronym = "tsfa")
}

#' @rdname who_gs_value2zscore
#' @importFrom stats pnorm
#' @export
who_gs_value2percentile <- function(y, x, sex, acronym) {
  pnorm(who_gs_value2zscore(y = y, x = x, sex = sex, acronym = acronym))
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_wfa_value2percentile <- function(weight_kg, age_days, sex) {
  who_gs_value2percentile(y = weight_kg, x = age_days, sex = sex, acronym = "wfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_bfa_value2percentile <- function(bmi, age_days, sex) {
  who_gs_value2percentile(y = bmi, x = age_days, sex = sex, acronym = "bfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_lhfa_value2percentile <- function(lenht_cm, age_days, sex) {
  who_gs_value2percentile(y = lenht_cm, x = age_days, sex = sex, acronym = "lhfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_wfl_value2percentile <- function(weight_kg, length_cm, sex) {
  who_gs_value2percentile(y = weight_kg, x = length_cm, sex = sex, acronym = "wfl")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_wfh_value2percentile <- function(weight_kg, height_cm, sex) {
  who_gs_value2percentile(y = weight_kg, x = height_cm, sex = sex, acronym = "wfh")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_hcfa_value2percentile <- function(headcirc_cm, age_days, sex) {
  who_gs_value2percentile(y = headcirc_cm, x = age_days, sex = sex, acronym = "hcfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_acfa_value2percentile <- function(armcirc_cm, age_days, sex) {
  who_gs_value2percentile(y = armcirc_cm, x = age_days, sex = sex, acronym = "acfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_ssfa_value2percentile <- function(subscap_sf_mm, age_days, sex) {
  who_gs_value2percentile(y = subscap_sf_mm, x = age_days, sex = sex, acronym = "ssfa")
}

#' @rdname who_gs_value2zscore
#' @export
who_gs_tsfa_value2percentile <- function(triceps_sf_mm, age_days, sex) {
  who_gs_value2percentile(y = triceps_sf_mm, x = age_days, sex = sex, acronym = "tsfa")
}

#' Get LMS values for WHO Growth Standards models
#'
#'
#' @param x X value(s) at which to retrieve LMS values. Must be within bounds of available x values for given acronym.
#' @param sex Sex(es), either `"M"` (male) or `"F"` (female).
#' @param acronym Valid acronym for WHO Growth Standards datasets: one of `"wfa"` (weight-for-age), `"bfa"`
#' (bmi-for-age), `"lhfa"` (length/height-for-age), `"wfl"` (weight-for-length), or `"wfh"`
#' (weight-for-height), `"hcfa"` (head circumference-for-age), `"acfa"` (arm circumference-for-age),
#' `"ssfa"` (subscapular skinfold-for-age), or `"tsfa"` (triceps skinfold-for-age),
#'
#' @returns A dataframe with lambda, mu and sigma values for the WHO growth standard(s) specified by the `acronym`
#' parameter, for the supplied `x` and `sex` values.
#'
#' @references
#' de Onis M, Garza C, Victora CG, Onyango AW, Frongillo EA, Martines J. **The WHO Multicentre Growth Reference
#' Study: planning, study design, and methodology** *Food Nutr Bull.* 2004, **25(1 Suppl):S15-26.**
#' doi: [10.1177/15648265040251s104](https://journals.sagepub.com/doi/10.1177/15648265040251S104)
#'
#' @rdname who_gs_lms
#' @keywords internal
who_gs_lms <- function(x, sex, acronym) {
  checked_params <- check_who_params(sex, acronym)
  x <- ifelse(test = acronym %in% c("wfl", "wfh"),
              yes = round2(x = x, digits = 1),
              no = x)

  new_df <- data.frame(x = x,
                       sex = checked_params$sex,
                       acronym = checked_params$acronym,
                       sort = seq(from = 1, to = length(x)))

  load_lms_tables <- function(sex, acronym) {
    coeff_tbl <- gigs::who_gs_coeffs[[acronym]][[sex]]
    coeff_tbl <- cbind(coeff_tbl,
          sex = rep_len(ifelse(sex == "male", yes = "M", no = "F"), length.out = nrow(coeff_tbl)),
          acronym = rep_len(acronym, length.out = nrow(coeff_tbl))) |>
      setNames(c("x", "L", "M", "S", "sex", "acronym" ))
  }

  sexes_to_load <- intersect(c("male", "female"), ifelse(sex == "M", yes = "male", no = "female"))
  acronyms_to_load <- intersect(names(gigs::who_gs_coeffs), unique(acronym))
  tot_len <- length(sexes_to_load) * length(acronyms_to_load)
  who_gs_coeffs_li <- mapply(x = rep_len(sexes_to_load, length.out = tot_len),
                             y = rep_len(acronyms_to_load, length.out = tot_len),
                             SIMPLIFY = FALSE,
                             FUN = \(x, y) load_lms_tables(sex = x, acronym = y))
  if (!length(who_gs_coeffs_li)) {
    out <- new_df
    out$L <- NA
    out$M <- NA
    out$S <- NA
    return(out)
  }

  sexes_to_load <- intersect(c("M", "F"), ifelse(sexes_to_load == "male",
                                                 yes = "M", no = "F"))
  who_gs_coeffs_long <- do.call(rbind, who_gs_coeffs_li)
  if (length(who_gs_coeffs_li)) {
    names(who_gs_coeffs_li) <- paste0(sexes_to_load, "_", acronyms_to_load)

    if (length(acronyms_to_load) == 1) {
      who_x <- who_gs_coeffs_li[[1]]$x
      within_coeff_lims <- inrange(new_df$x, who_x)
    } else {
      index_per_acronym <- sapply(acronyms_to_load,
                                  FUN = \(x) grep(pattern = x,
                                                  names(who_gs_coeffs_li))[[1]][1])
      who_gs_xlims <- lapply(index_per_acronym,
                             FUN = \(x) who_gs_coeffs_li[[x]][, 1] )
      within_coeff_lims <- sapply(seq_along(new_df$acronym),
                                  FUN = \(x) {
                                    temp_acro <- new_df$acronym[x]
                                    temp_x <- new_df$x[x]
                                    inrange(temp_x, who_gs_xlims[[temp_acro]])
                                  })
    }
    needs_lerp <- which(!x %in% who_gs_coeffs_long$x & within_coeff_lims)
  }
  # Linear interpolation for values not in table but within bounds for that
  # acronym
  if (exists(x = "needs_lerp") && length(needs_lerp)) {
    in_coeff_tbl <- which(!seq_along(x) %in% needs_lerp)
    not_in_coeff_tbl_df <- new_df[needs_lerp, ]
    coeffs_interpolated <- do.call(rbind,
            mapply(xvars = not_in_coeff_tbl_df$x,
                   sex = not_in_coeff_tbl_df$sex,
                   acronym = not_in_coeff_tbl_df$acronym,
                   SIMPLIFY = F,
                   FUN = \(xvars, sex, acronym) {
                     interpolate_coeffs(who_gs_coeffs_long, xvars, sex, acronym)
                   })) |>
      merge(not_in_coeff_tbl_df)
  } else {
    in_coeff_tbl <- seq_along(x)
  }

  merge_df <- new_df[in_coeff_tbl, ]
  out <- merge(merge_df, who_gs_coeffs_long, all.x = TRUE, sort = FALSE)
  if (exists(x = "coeffs_interpolated")) {
    out <- rbind(out, coeffs_interpolated)
  }
  out <- out[order(out$sort), ]
  out[, -which(names(out) == "sort")]
}

#' Get standard deviations for WHO Growth Standards data from LMS values
#' @param l Lambda value as provided by [who_gs_lms]
#' @param m Mu value as provided by [who_gs_lms]
#' @param s Sigma value as provided by [who_gs_lms]
#' @param n_sd Number of SD from the median to be computed
#' @references
#' de Onis M *et al.* (2006) **'COMPUTATION OF CENTILES AND Z-SCORES FOR LENGTH/HEIGHT-FOR-AGE,
#' WEIGHT-FOR-AGE, WEIGHT-FOR-LENGTH, WEIGHT-FOR-HEIGHT AND BMI-FOR-AGE'** *in* World Health Organization (ed.)
#' *WHO Child Growth Standards: length/height-for-age, weight-for-age, weight-for-length, weight-for-height and
#' body mass index-for-age: methods and development*. Geneva: World Health Organization, pp. 301-304.
#' @keywords internal
who_gs_lms2sd <- function(l, m, s, n_sd) {
  m * (1 + l * s * n_sd)^(1 / l)
}