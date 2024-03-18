# Fetal weight estimation ------------------------------------------------------

#' Estimate fetal weight in grams using the INTERGROWTH-21<sup>st</sup>
#' predictive equation
#'
#' @param abdocirc_mm Numeric vector with abdominal circumference value(s) in
#'   mm. Should have length one or same length as `headcirc_mm`.
#' @param headcirc_mm Numeric vector with head circumference value(s) in mm.
#'   Should have length one or same length as `abdocirc_mm`.
#' @note Inputs are recycled using [vctrs::vec_recycle_common()].
#' @examples
#' # Estimate fetal weight in grams
#' ig_fet_estimate_fetal_weight(abdocirc_mm = 31:33,
#'                              headcirc_mm = 25:27)
#'
#' # Input vectors are recycled using vctrs::vec_recycle_common
#' ig_fet_estimate_fetal_weight(abdocirc_mm = 25.0,
#'                              headcirc_mm = 24:26)
#' @returns Numeric vector with estimated fetal weight(s) in g, with the same
#'   length as the longest input vector.
#' @seealso Get z-scores/centiles for estimated fetal weights at known
#'   gestational ages using the [ig_fet_value2zscore()] or
#'   [ig_fet_value2centile()] functions, respectively.
#' @references
#' Stirnemann J, Villar J, Salomon LJ, Ohuma EO, Lamber A, Victoria CG et al.
#' **International Estimated Fetal Weight Standards of the INTERGROWTH-21st
#' Project.** *Ultrasound Obstet Gynecol* 2016, **49:478-486**
#' \doi{10.1002/uog.17347}
#' @export
ig_fet_estimate_fetal_weight <- function(abdocirc_mm, headcirc_mm) {
  recycled <- validate_parameter_lengths(abdocirc_mm = abdocirc_mm,
                                         headcirc_mm = headcirc_mm) |>
    mapply(FUN = validate_ig_fet_weight_estimation_param,
           SIMPLIFY = FALSE,
           varname = c("abdocirc_mm", "headcirc_mm")) |>
    do.call(what = vctrs::vec_recycle_common) |>
    lapply(FUN = \(x) x / 10) # Standard in paper is in centimetres
  with(
    recycled,
    exp(5.084820 - 54.06633 * (abdocirc_mm/100)^3 -
          95.80076 * (abdocirc_mm/100)^3 * log(abdocirc_mm/100) +
          3.136370 * (headcirc_mm/100))
  )
}

# Gestational age estimation ---------------------------------------------------

#' Estimate gestational age using INTERGROWTH-21<sup>st</sup> predictive
#' equations
#'
#' @param crl_mm Numeric vector with crown-rump length value(s) in mm taken
#'   from an ultrasound scan obtained 56 to 97 days since the last menstrual
#'   period. If not `NULL` (the default), should be of length one or the same
#'   length as other inputs. The function will return NA for all elements of
#'   `crl_mm` which are not between 15 and 95 mm and have no `headcirc_mm`
#'   or `abdocirc_mm` values.
#' @param headcirc_mm Numeric vector with head circumference value(s) in cm
#'   taken from an ultrasound scan obtained 98 to 181 days since the last
#'   menstrual period. If not `NULL` (the default), should have length one or
#'   same length as other inputs.
#' @param femurlen_mm Numeric vector with femur length value(s) in cm. If not
#'   `NULL` (the default), should have length one or same length as other
#'   arguments inputs.
#' @note Vector inputs to this function are recycled using
#'   [vctrs::vec_recycle_common()]. The function will attempt to estimate the
#'   gestational age (GA) in three steps, from highest to lowest accuracy.
#'   First, using crown-rump length measurements obtained in early pregnancy,
#'   then with both head circumference and femur length measurements, and
#'   finally with head circumference alone.
#' @examples
#' # Estimate gestational age in days using crown-rump length (most accurate)
#' ig_fet_estimate_ga(crl_mm = 40:45)
#'
#' # Estimate gestational age in days using head circumference and femur length
#' # (second-most accurate)
#' ig_fet_estimate_ga(headcirc_mm = 250:255, femurlen_mm = 55:60)
#'
#' # Estimate gestational age in days using head circumference only (least
#' # accurate)
#' ig_fet_estimate_ga(headcirc_mm = 250:255)
#'
#' # The function defaults to CRL if available
#' ig_fet_estimate_ga(crl_mm = 40:45,
#'                    headcirc_mm = 250:255,
#'                    femurlen_mm = 55:60)
#'
#' # Inputs are recycled using [vctrs::vec_recycle_common]
#' ig_fet_estimate_ga(headcirc_mm = 252,
#'                    femurlen_mm = 55:60)
#' @returns A numeric vector with estimated gestational ages in days. This
#'   vector will have a length equal to the outputs from
#'   [vctrs::vec_recycle_common()].
#' @references
#' Papageorghiou AT, Kennedy SH, Salomon LJ, Ohuma EO, Cheikh Ismail L, Barros
#' FC et al. **International standards for early fetal size and pregnancy dating
#' based on ultrasound measurement of crown-rump length in the first trimester
#' of pregnancy.** *Ultrasound Obstet Gynecol* 2014, **44(6):641-48**
#' \doi{10.1002/uog.13448}
#'
#' Papageorghiou AT, Kemp B, Stones W, Ohuma EO, Kennedy SH, Purwar M et al.
#' **Ultrasound-based gestational age estimation in late pregnancy.**
#' *Ultrasound Obstet Gynecol* 2016. **48(6):719-26** \doi{10.1002/uog.15894}
#' @export
ig_fet_estimate_ga <- function(crl_mm = NULL,
                               headcirc_mm = NULL,
                               femurlen_mm = NULL) {
  recycled <- validate_parameter_lengths(crl_mm = crl_mm,
                                         headcirc_mm = headcirc_mm,
                                         femurlen_mm = femurlen_mm) |>
    mapply(FUN = validate_ig_fet_estimation_param,
           SIMPLIFY = FALSE,
           varname = c("crl_mm", "headcirc_mm", "femurlen_mm")) |>
    do.call(what = vctrs::vec_recycle_common)
  with(recycled, {
    crl_is_null <- is.null(crl_mm)
    hc_is_null <- is.null(headcirc_mm)
    fl_is_null <- is.null(femurlen_mm)
    if (!crl_is_null) {
      ifelse(inrange(crl_mm, c(15, 95)),
             yes = ig_fet_estimate_ga_crl(crl_mm),
             no = rep(NA, length(crl_mm)))
    } else if (!hc_is_null) {
      if (!fl_is_null) {
        ig_fet_estimate_ga_hcfl(headcirc_mm, femurlen_mm)
      } else {
        ig_fet_estimate_ga_hc(headcirc_mm)
      }
    } else {
      stop(paste("At least one of `crl_mm` or `headcirc_mm` must not be `NULL`",
                 "for ig_fet_estimate_ga() to run."),
           call. = FALSE)
    }
  })
}

#' Estimate gestational age using crown-rump length in mm
#' @inheritParams ig_fet_estimate_ga
#' @return Numeric vector the same length as `crl_mm` with gestational age
#'   estimate(s) in days.
#' @references
#' Papageorghiou AT, Kennedy SH, Salomon LJ, Ohuma EO, Cheikh Ismail L, Barros
#' FC et al. **International standards for early fetal size and pregnancy dating
#' based on ultrasound measurement of crown-rump length in the first trimester
#' of pregnancy.** *Ultrasound Obstet Gynecol* 2014, **44(6):641-48**
#' \doi{10.1002/uog.13448}
#' @noRd
ig_fet_estimate_ga_crl <- function(crl_mm) {
  ig_fet_mu_sigma(crl_mm, acronym = "gafcrl")[["mu"]]
}

#' Estimate gestational age using head circumference only
#' @inheritParams ig_fet_estimate_ga
#' @return Numeric vector of the same length as `headcirc_mm` with gestational
#'   age estimations in days.
#' @references
#' Papageorghiou AT, Kemp B, Stones W, Ohuma EO, Kennedy SH, Purwar M et al.
#' **Ultrasound-based gestational age estimation in late pregnancy.**
#' *Ultrasound Obstet Gynecol* 2016. **48(6):719-26** \doi{10.1002/uog.15894}
#' @noRd
ig_fet_estimate_ga_hc <- function(headcirc_mm) {
  exp(0.05970 * log(headcirc_mm)^2 + 0.000000006409 * headcirc_mm^3 + 3.3258)
}

#' Estimate gestational age using head circumference and femur length
#' @inherit ig_fet_estimate_ga_hc params return references
#' @noRd
ig_fet_estimate_ga_hcfl <- function(headcirc_mm, femurlen_mm) {
  exp(0.03243 * log(headcirc_mm)^2 + 0.001644 * femurlen_mm *
    log(headcirc_mm) + 3.813)
}

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.0} Primary literature referenced for each exported function,
#'   and for internal functions.
#' @srrstats {G1.4, G1.4a} All functions in file documented using `{roxygen2}`.
#' @srrstats {G2.0a, G2.1a, EA1.3} Exported function in this file document
#'   expectations on the length of inputs and their data types.
#' @srrstats {G2.0, G2.1, G2.2, G2.3, G2.3a, G2.6} These standards
#'   are met in all exported functions by passing inputs to
#'   [validate_ig_fet_estimation_param()]. All internal functions in this script
#'   are provided with vectors that have already been validated.
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.16} These standards are met
#'   in all exported functions by passing inputs to
#'   [validate_ig_fet_estimation_param()]. All internal functions in this script
#'   are provided with vectors that have already been validated.
NULL
