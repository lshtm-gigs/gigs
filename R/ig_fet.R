#' Convert z-scores/centiles to values in the INTERGROWTH-21<sup>st</sup> Fetal
#' Growth standards
#'
#' @inheritParams shared_roxygen_params
#' @param gest_days Numeric vector of gestational age(s) in days. Elements
#'   should be between `98` to `280` for all standards other than estimated
#'   fetal weight-for-GA (`"efwfga"`), which takes values betweem `154` and
#'   `280`. Will return NA for each observation where GA is not within bounds.
#' @param acronym Acronym(s) denoting an INTERGROWTH-21<sup>st</sup> Fetal
#'   Growth standard. Each value should be one of `"hcfga"` (head
#'   circumference-for-GA), `"bpdfga"` (biparietal diameter-for-age), `"acfga"`
#'   (abdominal circumference-for-GA), `"flfga"` (femur length-for-GA),
#'   `"ofdfga"` (occipito-frontal diameter-for-GA), or `"efwfga"` (estimated
#'   fetal length-for-GA). If none of these, the function will return NA.
#' @references
#' Papageorghiou AT, Ohuma EO, Altman DG, Todros T, Cheikh Ismail L, Lambert A
#' et al. **International standards for fetal growth based on serial ultrasound
#' measurements: the Fetal Growth Longitudinal Study of the INTERGROWTH-21st
#' Project.** *Lancet* 2014, **384(9946):869-79.**
#' \doi{0.1016/S0140-6736(14)61490-2}
#'
#' Stirnemann J, Villar J, Salomon LJ, Ohuma EO, Lamber A, Victoria CG et al.
#' **International Estimated Fetal Weight Standards of the INTERGROWTH-21st
#' Project.** *Ultrasound Obstet Gynecol* 2016, **49:478-486**
#' \doi{10.1002/uog.17347}
#' @examples
#' # Convert centiles to values
#' p <- 0.25 # 25th centile
#' ig_fet_centile2value(p = p, gest_days = 280, acronym = "hcfga") |>
#'   round(digits = 2)
#'
#' # Or z-scores to values
#' z <- qnorm(p)
#' ig_fet_zscore2value(z = z, gest_days = 280, acronym = "hcfga") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' ig_fet_zscore2value(z = z, gest_days = 280, acronym = "acfga") |>
#'   round(digits = 2)
#'
#' # ... or by using a standard-specific function
#' ig_fet_acfga_zscore2value(z = z, gest_days = 280) |>
#'   round(digits = 2)
#'
#' # Inputs are recycled to the input of the longest length
#' ig_fet_efwfga_zscore2value(z = seq(0.1, 0.9, by = 0.2),
#'                            gest_days = 280) |>
#'   round(digits = 2)
#'
#' # Bad inputs will not stop the function but will instead return NA - here 91
#' # days for gest_days is outside the bounds of the INTERGROWTH-21st Fetal
#' # Growth standards
#' ig_fet_hcfga_zscore2value(z = z, gest_days = c(91, 98, 224, 266)) |>
#'   round(digits = 2)
#' @rdname ig_fet_zscore2value
#' @srrstats {G1.0} Primary literature referenced here.
#' @export
ig_fet_zscore2value <- function(z, gest_days, acronym) {
  validated <- vctrs::vec_recycle_common(z = z,
                                         gest_days = gest_days,
                                         acronym = acronym) |>
    do.call(what = validate_ig_fet)

  mu_sigma <- ig_fet_equations(gest_days = validated[["gest_days"]],
                               acronym = validated[["acronym"]])
  ifelse(
      test = validated[["acronym"]] == "efwfga",
      yes = ig_fet_efw_z2v(z = validated[["z"]],
                           gest_days = validated[["gest_days"]]),
      no = mu_sigma_z2y(z = validated[["z"]],
                        mu = mu_sigma[["mu"]],
                        sigma = mu_sigma[["sigma"]])
  )
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_hcfga_zscore2value <- function(z, gest_days) {
  ig_fet_zscore2value(z, gest_days, acronym = "hcfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_bpdfga_zscore2value <- function(z, gest_days) {
  ig_fet_zscore2value(z, gest_days, acronym = "bpdfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_acfga_zscore2value <- function(z, gest_days) {
  ig_fet_zscore2value(z, gest_days, acronym = "acfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_flfga_zscore2value <- function(z, gest_days) {
  ig_fet_zscore2value(z, gest_days, acronym = "flfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_ofdfga_zscore2value <- function(z, gest_days) {
  ig_fet_zscore2value(z, gest_days, acronym = "ofdfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_efwfga_zscore2value <- function(z, gest_days) {
  ig_fet_zscore2value(z, gest_days, acronym = "efwfga")
}

#' @rdname ig_fet_zscore2value
#' @importFrom stats qnorm
#' @export
ig_fet_centile2value <- function(p, gest_days, acronym) {
  ig_fet_zscore2value(qnorm(p), gest_days, acronym = acronym)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_hcfga_centile2value <- function(p, gest_days) {
  ig_fet_centile2value(p, gest_days, acronym = "hcfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_bpdfga_centile2value <- function(p, gest_days) {
  ig_fet_centile2value(p, gest_days, acronym = "bpdfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_acfga_centile2value <- function(p, gest_days) {
  ig_fet_centile2value(p, gest_days, acronym = "acfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_flfga_centile2value <- function(p, gest_days) {
  ig_fet_centile2value(p, gest_days, acronym = "flfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_ofdfga_centile2value <- function(p, gest_days) {
  ig_fet_centile2value(p, gest_days, acronym = "ofdfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_efwfga_centile2value <- function(p, gest_days) {
  ig_fet_centile2value(p, gest_days, acronym = "efwfga")
}

#' Convert values to z-scores/centiles in the INTERGROWTH-21<sup>st</sup> Fetal
#' Growth standards
#'
#' @inheritParams shared_roxygen_params
#' @inherit ig_fet_zscore2value params references
#' @examples
#' # TODO: update with a proper example
#' print("temp")
#' @rdname ig_fet_value2zscore
#' @export
ig_fet_value2zscore <- function(y, gest_days, acronym) {
  validated <- vctrs::vec_recycle_common(y = y,
                                         gest_days = gest_days,
                                         acronym = acronym) |>
    do.call(what = validate_ig_fet)
  mu_sigma <- ig_fet_equations(gest_days = validated[["gest_days"]],
                               acronym = validated[["acronym"]])

  ifelse(
      test = validated[["acronym"]] == "efwfga",
      yes = ig_fet_efw_v2z(efw_g = validated[["y"]],
                           gest_days = validated[["gest_days"]]),
      no = mu_sigma_y2z(y = validated[["y"]],
                        mu = mu_sigma[["mu"]],
                        sigma = mu_sigma[["sigma"]])
  )
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_hcfga_value2zscore <- function(headcirc_mm, gest_days) {
  ig_fet_value2zscore(headcirc_mm, gest_days, acronym = "hcfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_bpdfga_value2zscore <- function(bpd_mm, gest_days) {
  ig_fet_value2zscore(bpd_mm, gest_days, acronym = "bpdfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_acfga_value2zscore <- function(abdocirc_mm, gest_days) {
  ig_fet_value2zscore(abdocirc_mm, gest_days, acronym = "acfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_flfga_value2zscore <- function(femurlen_mm, gest_days) {
  ig_fet_value2zscore(femurlen_mm, gest_days, acronym = "flfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_ofdfga_value2zscore <- function(ofd_mm, gest_days) {
  ig_fet_value2zscore(ofd_mm, gest_days, acronym = "ofdfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_efwfga_value2zscore <- function(efw_g, gest_days) {
  ig_fet_value2zscore(efw_g, gest_days, acronym = "efwfga")
}

#' @rdname ig_fet_value2zscore
#' @importFrom stats pnorm
#' @export
ig_fet_value2centile <- function(headcirc_mm, gest_days, acronym) {
  pnorm(ig_fet_value2zscore(headcirc_mm, gest_days, acronym))
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_hcfga_value2centile <- function(headcirc_mm, gest_days) {
  ig_fet_value2centile(headcirc_mm, gest_days, acronym = "hcfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_bpdfga_value2centile <- function(bpd_mm, gest_days) {
  ig_fet_value2centile(bpd_mm, gest_days, acronym = "bpdfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_acfga_value2centile <- function(abdocirc_mm, gest_days) {
  ig_fet_value2centile(abdocirc_mm, gest_days, acronym = "acfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_flfga_value2centile <- function(femurlen_mm, gest_days) {
  ig_fet_value2centile(femurlen_mm, gest_days, acronym = "flfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_ofdfga_value2centile <- function(ofd_mm, gest_days) {
  ig_fet_value2centile(ofd_mm, gest_days, acronym = "ofdfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_efwfga_value2centile <- function(efw_g, gest_days) {
  ig_fet_value2centile(efw_g, gest_days, acronym = "efwfga")
}

#' INTERGROWTH-21<sup>st</sup> equations for fetal growth
#'
#' Estimates median and standard deviation for different measures of fetal
#' growth.
#'
#' @param gest_days Gestational age in days. Values not between `14` and `40`
#'   will be set to NA.
#' @param acronym Acronym(s) denoting an INTERGROWTH-21<sup>st</sup> Fetal
#'   Growth standard. Easy value should be one of `"hcfga"` (head
#'   circumference-for-GA), `"bpdfga"` (biparietal diameter-for-age), `"acfga"`
#'   (abdominal circumference-for-GA), `"flfga"` (femur length-for-GA),
#'   `"ofdfga"` (occipito-frontal diameter-for-GA), or `"efwfga"` (estimated
#'   fetal length-for-GA).
#' @return A list with with mean(s) and standard deviation(s) for each
#'   `gest_days`/`acronym` combination provided to the function.
#' @references
#' Papageorghiou AT, Ohuma EO, Altman DG, Todros T, Cheikh Ismail L, Lambert A
#' et al. **International standards for fetal growth based on serial ultrasound
#' measurements: the Fetal Growth Longitudinal Study of the INTERGROWTH-21st
#' Project.** *Lancet* 2014, **384(9946):869-79.**
#' \doi{0.1016/S0140-6736(14)61490-2}
#' @rdname ig_fet_equations
#' @srrstats {G1.0} Primary literature referenced here.
#' @noRd
ig_fet_equations <- function(gest_days, acronym) {
  mu_fns <- list(
    hcfga = \(GA) -28.2849 + 1.69267 * GA^2 - 0.397485 * GA^2 * log(GA),
    bpdfga = \(GA) 5.60878 + 0.158369 * GA^2 - 0.00256379 * GA^3,
    acfga = \(GA) -81.3243 + 11.6772 * GA - 0.000561865 * GA^3,
    flfga = \(GA) -39.9616 + 4.32298 * GA - 0.0380156 * GA^2,
    ofdfga = \(GA) -12.4097 + 0.626342 * GA^2 - 0.148075 * GA^2 * log(GA)
  )
  sigma_fns <- list(
    hcfga = \(GA) {
      1.98735 + 0.0136772 * GA^3 - 0.00726264 * GA^3 * log(GA) +
      0.000976253 * GA^3 * log(GA)^2
    },
    bpdfga = \(GA) {
      exp(0.101242 + 0.00150557 * GA^3 - 0.000771535 * GA^3 * log(GA) +
          0.0000999638 * GA^3 * log(GA)^2)
    },
    acfga = \(GA) {
      -4.36302 + 0.121445 * GA^2 - 0.0130256 * GA^3 +
      0.00282143 * GA^3 * log(GA)
    },
    flfga = \(GA) exp(0.605843 - 42.0014 * GA^-2 + 0.00000917972 * GA^3),
    ofdfga = \(GA) {
      exp(-0.880034 + 0.0631165 * GA^2 - 0.0317136 * GA^2 * log(GA) +
          0.00408302 * GA^2 * log(GA)^2)
    }
  )

  len_out <- length(gest_days)
  mu <- rep(NA_real_, len_out)
  sigma <- rep(NA_real_, len_out)
  gest_wks <- gest_days / 7
  for (idx in seq_along(mu)) {
    ga <- gest_wks[idx]
    try(mu[idx] <- mu_fns[[acronym[idx]]](GA = ga), silent = TRUE)
    try(sigma[idx] <- sigma_fns[[acronym[idx]]](GA = ga), silent = TRUE)
  }
  list(mu = mu, sigma = sigma)
}

#' Estimate fetal weight in grams using the INTERGROWTH-21<sup>st</sup>
#' predictive equation
#'
#' @param abdocirc_cm Numeric vector with abdominal circumference value(s) in
#'   cm. Should have length 1 or same length as `headcirc_mm`.
#' @param headcirc_cm Numeric vector with head circumference value(s) in cm.
#'   Should have length 1 or same length as `abdocirc_mm`.
#' @notes The inputs `abdocirc_mm` and `headcirc_mm` are recycled using
#'   [vctrs::vec_recycle_common()] if necessary.
#' @references
#' Stirnemann J, Villar J, Salomon LJ, Ohuma EO, Lamber A, Victoria CG et al.
#' **International Estimated Fetal Weight Standards of the INTERGROWTH-21st
#' Project.** *Ultrasound Obstet Gynecol* 2016, **49:478-486**
#' \doi{10.1002/uog.17347}
#' @export
ig_fet_estimate_fetal_weight <- function(abdocirc_cm, headcirc_cm) {
  checkmate::assert_numeric(abdocirc_cm, min.len = 1)
  checkmate::assert_numeric(headcirc_cm, min.len = 1)
  recycled <- vctrs::vec_recycle_common(ac = abdocirc_cm, hc = headcirc_cm)
  exp(
    5.084820 - 54.06633 * (recycled[["ac"]]/100)^3 -
      95.80076 * (recycled[["ac"]]/100)^3 * log(recycled[["ac"]]/100) +
      3.136370 * (recycled[["hc"]]/100)
  )
}

#' Get lambda/mu/sigma values for the INTERGROWTH-21st estimated fetal weight
#' standard (part of the INTERGROWTH-21st Fetal Growth standards)
#'
#' @param gest_days Gestational age in days, ranging from `154` to `280`.
#' @references
#' Stirnemann J, Villar J, Salomon LJ, Ohuma EO, Lamber A, Victoria CG et al.
#' **International Estimated Fetal Weight Standards of the INTERGROWTH-21st
#' Project.** *Ultrasound Obstet Gynecol* 2016, **49:478-486**
#' \doi{10.1002/uog.17347}
#' @return List of three numeric vectors named `l` (lambda), `m` (mu),
#'   and `s` (sigma), each of which has the same length as `gest_days`.
#' @noRd
ig_fet_efw_lms <- function(gest_days) {
  gest_wks <- gest_days / 7
  lambda <- \(GA) -4.257629-2162.234 * GA^-2 + 0.0002301829 * GA^3
  mu <- \(GA) 4.956737 + 0.0005019687 * GA^3 - 0.0001227065 * GA^3 * log(GA)
  sigma <- \(GA) {
    10^-4 * (-6.997171 + 0.057559 * GA^3 - 0.01493946 * GA^3 * log(GA))
  }
  list(l = lambda(gest_wks),
       m = mu(gest_wks),
       s = sigma(gest_wks))
}

#' Convert values to z-scores in the INTERGROWTH-21st estimated fetal weight
#' standard (internal)
#'
#' @param efw_g Numeric vector with estimated fetal weight in grams.
#' @param gest_days
#' @references
#' Stirnemann J, Villar J, Salomon LJ, Ohuma EO, Lamber A, Victoria CG et al.
#' **International Estimated Fetal Weight Standards of the INTERGROWTH-21st
#' Project.** *Ultrasound Obstet Gynecol* 2016, **49:478-486**
#' \doi{10.1002/uog.17347}
#' @returns Numeric vector with same length as `efw_g` and `gest_days`
#'   containing z-scores.
#' @noRd
ig_fet_efw_v2z <- function(efw_g, gest_days) {
  lms <- ig_fet_efw_lms(gest_days)

  log_efw <- log(efw_g)
  with(lms,
       ifelse(test = abs(l) < sqrt(.Machine$double.eps),
              yes = s^-1 * log(log_efw / m),
              no = (s * l)^-1 * ((log_efw / m)^l - 1))
  )
}

#' Convert z-scores to values in the INTERGROWTH-21st estimated fetal weight
#' standard (internal)
#'
#' @param z Numeric vector with estimated fetal weight in grams.
#' @param gest_days Numeric vector with gestational age in days.
#' @references
#' Stirnemann J, Villar J, Salomon LJ, Ohuma EO, Lamber A, Victoria CG et al.
#' **International Estimated Fetal Weight Standards of the INTERGROWTH-21st
#' Project.** *Ultrasound Obstet Gynecol* 2016, **49:478-486**
#' \doi{10.1002/uog.17347}
#' @returns Numeric vector with same length as `z` and `gest_days`
#'   containing expected estimated fetal weight values in grams.
#' @noRd
ig_fet_efw_z2v <- function(z, gest_days) {
  lms <- ig_fet_efw_lms(gest_days)
  log_efw <- with(lms,
                  ifelse(test = abs(l) < sqrt(.Machine$double.eps),
                         yes = m * exp(s * z),
                         no = m * (z * s * l + 1)^(1 / l)))
  exp(log_efw)
}
