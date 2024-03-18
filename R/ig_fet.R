#' Convert z-scores/centiles to values in the INTERGROWTH-21<sup>st</sup> Fetal
#' standards
#'
#' @inherit shared_roxygen_params params note
#' @param x Numeric vector of length one or more with x values. Elements should
#'   have specific units and be between certain values depending on the standard
#'   in use (defined by `acronym`). These are:
#'   * Between 98 and 280 days for `"hcfga"`, `"bpdfga"`, `"acfga"`, `"flfga"`,
#'     `"ofdfga"`, and `"tcdfga"`.
#'   * Between 154 and 280 days for `"efwfga"`.
#'   * Between 112 and 294 days for `"sfhfga"`.
#'   * Between 58 and 105 days for `"crlfga"`.
#'   * Between 19 and 95 mm for `"gafcrl"`.
#'   * Between 105 and 280 days for `"gwgfga"`.
#'   * Between 168 and 280 days for `"pifga"`, `"rifga"`, and `"sdrfga"` (the
#'     INTERGROWTH-21<sup>st</sup> Fetal Doppler standards).
#'   * Between 105 and 252 days for `"poffga"`, `"sffga"`, `"avfga"`, and
#'     `"pvfga"` (the INTERGROWTH-21<sup>st</sup> Fetal Brain Development
#'     standards).
#'
#'   By default, gigs will replace out-of-bounds elements in `x` with `NA` and
#'   warn you. You can customise this behaviour using the [GIGS package-level
#'   options][gigs_options].
#' @param acronym A single string denoting the INTERGROWTH-21<sup>st</sup> Fetal
#'   Growth standard to use. Each value should be one of:
#'   * `"hcfga"` (head circumference-for-GA)
#'   * `"bpdfga"` (biparietal diameter-for-GA)
#'   * `"acfga"` (abdominal circumference-for-GA)
#'   * `"flfga"` (femur length-for-GA)
#'   * `"ofdfga"` (occipito-frontal diameter-for-GA)
#'   * `"efwfga"` (estimated fetal weight-for-GA)
#'   * `"sfhfga"` (symphisis-fundal height-for-GA)
#'   * `"crlfga"` (crown-rump length-for-GA)
#'   * `"gafcrl"` (GA-for-crown-rump length)
#'   * `"gwgfga"` (gestational weight gain-for-GA)
#'   * `"pifga"` (pulsatility index-for-GA)
#'   * `"rifga"` (resistance index-for-GA)
#'   * `"sdrfga"` (systolic/diastolic ratio-for-GA)
#'   * `"tcdfga"` (transcerebellar diameter-for-GA)
#'   * `"poffga"` (parieto-occipital fissure-for-GA)
#'   * `"sffga"` (Sylvian fissure-for-GA)
#'   * `"avfga"` (anterior horn of lateral ventricle-for-GA)
#'   * `"pvfga"` (atrium of posterior horn of lateral ventricle-for-GA)
#'   * `"cmfga"` (cisterna magna-for-GA)
#'
#'   This argument is case-sensitive. If `acronym` is not one of the values
#'   list above, gigs will throw an error.
#' @srrstats {G2.3b} Explicit reference to `acronym` case-sensitivity.
#' @param gest_days,crl_mm,tcd_mm Numeric vector of length one or more with
#'   standard-specific `x` variables. See the documentation for `x` for
#'   information on how out-of-bounds elements will be handled.
#' @inherit shared_zscore2value_returns return
#' @references
#' Papageorghiou AT, Ohuma EO, Altman DG, Todros T, Cheikh Ismail L, Lambert A
#' et al. **International standards for fetal growth based on serial ultrasound
#' measurements: the Fetal Growth Longitudinal Study of the INTERGROWTH-21st
#' Project.** *Lancet* 2014, **384(9946):869-79.**
#' \doi{10.1016/S0140-6736(14)61490-2}
#'
#' Stirnemann J, Villar J, Salomon LJ, Ohuma EO, Lamber A, Victoria CG et al.
#' **International Estimated Fetal Weight Standards of the INTERGROWTH-21st
#' Project.** *Ultrasound Obstet Gynecol* 2016, **49:478-486**
#' \doi{10.1002/uog.17347}
#'
#' Papageorghiou AT, Ohuma EO, Gravett MG, Lamber A, Noble JA, Pang R et al.
#' **International standards for symphysis-fundal height based on serial
#' measurements from the Fetal Growth Longitudinal Study of the INTERGROWTH-21st
#' Project: prospective cohort study in eight countries.** *BMJ* 2016,
#' **355:i5662** \doi{10.1136/bmj.i5662}
#'
#' Papageorghiou AT, Kennedy SH, Salomon LJ, Ohuma EO, Cheikh Ismail L, Barros
#' FC et al. **International standards for early fetal size and pregnancy dating
#' based on ultrasound measurement of crown-rump length in the first trimester
#' of pregnancy.** *Ultrasound Obstet Gynecol* 2014, **44(6):641-48**
#' \doi{10.1002/uog.13448}
#'
#' Cheikh Ismail L, Bishop DC, Pang R, Ohuma EO, Kac G, Abrams B et al.
#' **Gestational weight gain standards based on women enrolled in the Fetal
#' Growth Longitudinal Study of the INTERGROWTH-21st Project: a prospective
#' longitudinal cohort study.** *BMJ* 2016, **352:i555** \doi{10.1136/bmj.i555}
#'
#' Drukker L, Staines-Urias E, Villar J, Barros FC, Carvalho M, Munim S et al.
#' **International gestational age-specific centiles for umbilical artery
#' Doppler indices: a longitudinal prospective cohort study of the
#' INTERGROWTH-21st Project.** *Am J Obstet Gynecol* 2021,
#' **222(6):602.e1-602.e15** \doi{10.1016/j.ajog.2020.01.012}
#'
#' Rodriguez-Sibaja MJ, Villar J, Ohuma EO, Napolitano R, Heyl S, Carvalho M et
#' al. **Fetal cerebellar growth and Sylvian fissure maturation: international
#' standards from Fetal Growth Longitudinal Study of INTERGROWTH-21st Project**
#' *Ultrasound Obstet Gynecol* 2021, **57(4):614-623** \doi{10.1002/uog.22017}
#'
#' Napolitano R, Molloholli M, Donadono V, Ohuma EO, Wanyonyi SZ, Kemp B et al.
#' **International standards for fetal brain structures based on serial
#' ultrasound measurements from Fetal Growth Longitudinal Study of
#' INTERGROWTH-21st Project** *Ultrasound Obstet Gynecol* 2020,
#' **56(3):359-370** \doi{10.1002/uog.21990}
#' @examples
#' # Convert centiles to values
#' ig_fet_centile2value(p = c(0.25, 0.5, 0.75), # 25th, 50th, 75th centile
#'                      x = 274:276,
#'                      acronym = "hcfga") |>
#'   round(digits = 2)
#'
#' # Or z-scores to values
#' ig_fet_zscore2value(z = qnorm(c(0.25, 0.5, 0.75)),
#'                     x = 274:276,
#'                     acronym = "hcfga") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' ig_fet_zscore2value(z = -1:1,
#'                     x = 274:276,
#'                     acronym = "acfga") |>
#'   round(digits = 2)
#'
#' # ... or by using a standard-specific function
#' ig_fet_acfga_zscore2value(z = -1:1,
#'                           gest_days = 274:276) |>
#'   round(digits = 2)
#'
#' # Inputs are recycled to the input of the longest length
#' ig_fet_acfga_zscore2value(z = 0,
#'                           gest_days = 274:276) |>
#'   round(digits = 2)
#'
#' # Bad inputs will not stop the function but will instead produce `NA`s in the
#' # output - by default gigs will issue useful warnings
#' ig_fet_hcfga_centile2value(p = c(-0.5, 0.15, 0.5, 0.85),
#'                            gest_days = c(91, 98, 224, NA)) |>
#'   round(digits = 2)
#' @rdname ig_fet_zscore2value
#' @export
ig_fet_zscore2value <- function(z, x, acronym) {
  validate_ig_fet(z = z, x = x, acronym = acronym) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_hcfga_zscore2value <- function(z, gest_days) {
  acronym <- "hcfga"
  validate_ig_fet(z = z,
                  x = gest_days,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_bpdfga_zscore2value <- function(z, gest_days) {
  acronym <- "bpdfga"
  validate_ig_fet(z = z,
                  x = gest_days,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_acfga_zscore2value <- function(z, gest_days) {
  acronym <- "acfga"
  validate_ig_fet(z = z,
                  x = gest_days,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_flfga_zscore2value <- function(z, gest_days) {
  acronym <- "flfga"
  validate_ig_fet(z = z,
                  x = gest_days,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_ofdfga_zscore2value <- function(z, gest_days) {
  acronym <- "ofdfga"
  validate_ig_fet(z = z,
                  x = gest_days,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_efwfga_zscore2value <- function(z, gest_days) {
  acronym <- "efwfga"
  validate_ig_fet(z = z,
                  x = gest_days,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_sfhfga_zscore2value <- function(z, gest_days) {
  acronym <- "sfhfga"
  validate_ig_fet(z = z,
                  x = gest_days,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_crlfga_zscore2value <- function(z, gest_days) {
  acronym <- "crlfga"
  validate_ig_fet(z = z,
                  x = gest_days,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_gafcrl_zscore2value <- function(z, crl_mm) {
  acronym <- "gafcrl"
  validate_ig_fet(z = z,
                  x = crl_mm,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_gwgfga_zscore2value <- function(z, gest_days) {
  acronym <- "gwgfga"
  validate_ig_fet(z = z,
                  x = gest_days,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_pifga_zscore2value <- function(z, gest_days) {
  acronym <- "pifga"
  validate_ig_fet(z = z,
                  x = gest_days,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_rifga_zscore2value <- function(z, gest_days) {
  acronym <- "rifga"
  validate_ig_fet(z = z,
                  x = gest_days,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_sdrfga_zscore2value <- function(z, gest_days) {
  acronym <- "sdrfga"
  validate_ig_fet(z = z,
                  x = gest_days,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_tcdfga_zscore2value <- function(z, gest_days) {
  acronym <- "tcdfga"
  validate_ig_fet(z = z,
                  x = gest_days,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_gaftcd_zscore2value <- function(z, tcd_mm) {
  acronym <- "gaftcd"
  validate_ig_fet(z = z,
                  x = tcd_mm,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_poffga_zscore2value <- function(z, gest_days) {
  acronym <- "poffga"
  validate_ig_fet(z = z,
                  x = gest_days,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_sffga_zscore2value <- function(z, gest_days) {
  acronym <- "sffga"
  validate_ig_fet(z = z,
                  x = gest_days,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_avfga_zscore2value <- function(z, gest_days) {
  acronym <- "avfga"
  validate_ig_fet(z = z,
                  x = gest_days,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_pvfga_zscore2value <- function(z, gest_days) {
  acronym <- "pvfga"
  validate_ig_fet(z = z,
                  x = gest_days,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_cmfga_zscore2value <- function(z, gest_days) {
  acronym <- "cmfga"
  validate_ig_fet(z = z,
                  x = gest_days,
                  acronym = acronym,
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_z2v_internal)
}

#' @rdname ig_fet_zscore2value
#' @importFrom stats qnorm
#' @export
ig_fet_centile2value <- function(p, x, acronym) {
  validated <- validate_ig_fet(p = p, x = x, acronym = acronym)
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_hcfga_centile2value <- function(p, gest_days) {
  acronym <- "hcfga"
  validated <- validate_ig_fet(p = p,
                               x = gest_days,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_bpdfga_centile2value <- function(p, gest_days) {
  acronym <- "bpdfga"
  validated <- validate_ig_fet(p = p,
                               x = gest_days,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_acfga_centile2value <- function(p, gest_days) {
  acronym <- "acfga"
  validated <- validate_ig_fet(p = p,
                               x = gest_days,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_flfga_centile2value <- function(p, gest_days) {
  acronym <- "flfga"
  validated <- validate_ig_fet(p = p,
                               x = gest_days,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_ofdfga_centile2value <- function(p, gest_days) {
  acronym <- "ofdfga"
  validated <- validate_ig_fet(p = p,
                               x = gest_days,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_efwfga_centile2value <- function(p, gest_days) {
  acronym <- "efwfga"
  validated <- validate_ig_fet(p = p,
                               x = gest_days,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_sfhfga_centile2value <- function(p, gest_days) {
  acronym <- "sfhfga"
  validated <- validate_ig_fet(p = p,
                               x = gest_days,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_crlfga_centile2value <- function(p, gest_days) {
  acronym <- "crlfga"
  validated <- validate_ig_fet(p = p,
                               x = gest_days,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_gafcrl_centile2value <- function(p, crl_mm) {
  acronym <- "gafcrl"
  validated <- validate_ig_fet(p = p,
                               x = crl_mm,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_gwgfga_centile2value <- function(p, gest_days) {
  acronym <- "gwgfga"
  validated <- validate_ig_fet(p = p,
                               x = gest_days,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_pifga_centile2value <- function(p, gest_days) {
  acronym <- "pifga"
  validated <- validate_ig_fet(p = p,
                               x = gest_days,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_rifga_centile2value <- function(p, gest_days) {
  acronym <- "rifga"
  validated <- validate_ig_fet(p = p,
                               x = gest_days,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_sdrfga_centile2value <- function(p, gest_days) {
  acronym <- "sdrfga"
  validated <- validate_ig_fet(p = p,
                               x = gest_days,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_tcdfga_centile2value <- function(p, gest_days) {
  acronym <- "tcdfga"
  validated <- validate_ig_fet(p = p,
                               x = gest_days,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_gaftcd_centile2value <- function(p, tcd_mm) {
  acronym <- "gaftcd"
  validated <- validate_ig_fet(p = p,
                               x = tcd_mm,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_poffga_centile2value <- function(p, gest_days) {
  acronym <- "poffga"
  validated <- validate_ig_fet(p = p,
                               x = gest_days,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_sffga_centile2value <- function(p, gest_days) {
  acronym <- "sffga"
  validated <- validate_ig_fet(p = p,
                               x = gest_days,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_avfga_centile2value <- function(p, gest_days) {
  acronym <- "avfga"
  validated <- validate_ig_fet(p = p,
                               x = gest_days,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_pvfga_centile2value <- function(p, gest_days) {
  acronym <- "pvfga"
  validated <- validate_ig_fet(p = p,
                               x = gest_days,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_cmfga_centile2value <- function(p, gest_days) {
  acronym <- "cmfga"
  validated <- validate_ig_fet(p = p,
                               x = gest_days,
                               acronym = acronym,
                               x_name = gigs::ig_fet[[acronym]][["x"]])
  with(validated, ig_fet_z2v_internal(qnorm(p), x, acronym))
}

#' Convert values to z-scores/centiles in the INTERGROWTH-21<sup>st</sup> Fetal
#' Growth standards
#'
#' @param gest_days Numeric vector of length one or more that is usually used
#'   as the x variable, but can also be a y variable for the `"gafcrl"`
#'   and `"gaftcd"` standards. When `gest_days` is being used as the x variable,
#'   out of bounds elements of `gest_days` will be handled according to the
#'   [GIGS package-level option][gigs_options] for `"handle_invalid_xvar"`.
#' @param headcirc_mm Numeric vector of length one or more with head
#'   circumference measurements in mm.
#' @param bpd_mm Numeric vector of length one or more with biparietal diameter
#'   measurement(s) in mm.
#' @param abdocirc_mm Numeric vector of length one or more with abdominal
#'   circumference measurement(s) in mm.
#' @param femurlen_mm Numeric vector of length one or more with femur length(s)
#'   in mm.
#' @param ofd_mm Numeric vector of length one or more with in occipito-frontal
#'   diameter(s) in mm.
#' @param efw_g Numeric vector of length one or more with estimated fetal
#'   weight(s) in g.
#' @param sfh_cm Numeric vector of length one or more with symphisis-fundal
#'   height(s) in mm.
#' @param crl_mm Numeric vector of length one or more with crown-rump length(s)
#'   in mm.
#' @param gest_wt_gain_kg Numeric vector of length one or more with gestational
#'   weight gain(s)/loss(es) in kg.
#' @param puls_idx Numeric vector of length one or more with pulsatility index
#'   value(s).
#' @param resist_idx Numeric vector of length one or more with resistance index
#'   value(s).
#' @param sys_dia_ratio Numeric vector of length one or more with
#'   systolic/diastolic ratio value(s).
#' @param tcd_mm Numeric vector of length one or more with transcerebellar
#'   diameter(s) in mm.
#' @param par_occ_fiss_mm Numeric vector of length one or more with
#'   parietal-occipital fissure measurement(s) in mm.
#' @param sylv_fiss_mm Numeric vector of length one or more with Sylvian fissure
#'    measurement(s) in mm.
#' @param ant_hlv_mm Numeric vector of length one or more with anterior horn of
#'   lateral ventricle measurements(s) in mm.
#' @param atr_phlv_mm Numeric vector of length one or more with atrium of
#'   posterior horn of lateral ventricle measurement(s) in mm.
#' @param cist_mag_mm Numeric vector of length one or more with cisterna magna
#'   measurement(s) in mm.
#' @inheritParams shared_roxygen_params
#' @inherit ig_fet_zscore2value params references
#' @inherit shared_value2zscore_returns return
#' @examples
#' # Convert values to centiles
#' ig_fet_value2centile(y = 335:345,
#'                      x = 260:270,
#'                      acronym = "hcfga") |>
#'   round(digits = 2)
#'
#' # Or values to z-scores
#' ig_fet_value2zscore(y = 335:345,
#'                     x = 260:270,
#'                     acronym = "hcfga") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' ig_fet_value2zscore(y = 335:345,
#'                     x = 260:270,
#'                     acronym = "acfga") |>
#'   round(digits = 2)
#'
#' # ... or by using a standard-specific function
#' ig_fet_acfga_value2zscore(abdocirc_mm = 335:345,
#'                           gest_days = 260:270) |>
#'   round(digits = 2)
#'
#' # Inputs are recycled to the input of the longest length
#' ig_fet_efwfga_value2zscore(efw_g = seq(3200, 3500, by = 50),
#'                            gest_days = 280) |>
#'   round(digits = 2)
#'
#' # Bad inputs will not stop the function but will instead produce `NA`s in the
#' # output - by default gigs will issue useful warnings
#' ig_fet_hcfga_value2zscore(headcirc_mm = c(90, 100, 300, NaN),
#'                           gest_days = c(91, 98, 224, 266)) |>
#'   round(digits = 2)
#' @rdname ig_fet_value2zscore
#' @export
ig_fet_value2zscore <- function(y, x, acronym) {
  validate_ig_fet(y = y, x = x, acronym = acronym) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_hcfga_value2zscore <- function(headcirc_mm, gest_days) {
  acronym <- "hcfga"
  validate_ig_fet(y = headcirc_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_bpdfga_value2zscore <- function(bpd_mm, gest_days) {
  acronym <- "bpdfga"
  validate_ig_fet(y = bpd_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_acfga_value2zscore <- function(abdocirc_mm, gest_days) {
  acronym <- "acfga"
  validate_ig_fet(y = abdocirc_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_flfga_value2zscore <- function(femurlen_mm, gest_days) {
  acronym <- "flfga"
  validate_ig_fet(y = femurlen_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_ofdfga_value2zscore <- function(ofd_mm, gest_days) {
  acronym <- "ofdfga"
  validate_ig_fet(y = ofd_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_efwfga_value2zscore <- function(efw_g, gest_days) {
  acronym <- "efwfga"
  validate_ig_fet(y = efw_g,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_sfhfga_value2zscore <- function(sfh_cm, gest_days) {
  acronym <- "sfhfga"
  validate_ig_fet(y = sfh_cm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_crlfga_value2zscore <- function(crl_mm, gest_days) {
  acronym <- "crlfga"
  validate_ig_fet(y = crl_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_gafcrl_value2zscore <- function(gest_days, crl_mm) {
  acronym <- "gafcrl"
  validate_ig_fet(y = gest_days,
                  x = crl_mm,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_gwgfga_value2zscore <- function(gest_wt_gain_kg, gest_days) {
  acronym <- "gwgfga"
  validate_ig_fet(y = gest_wt_gain_kg,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_pifga_value2zscore <- function(puls_idx, gest_days) {
  acronym <- "pifga"
  validate_ig_fet(y = puls_idx,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_rifga_value2zscore <- function(resist_idx, gest_days) {
  acronym <- "rifga"
  validate_ig_fet(y = resist_idx,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_sdrfga_value2zscore <- function(sys_dia_ratio, gest_days) {
  acronym <- "sdrfga"
  validate_ig_fet(y = sys_dia_ratio,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_tcdfga_value2zscore <- function(tcd_mm, gest_days) {
  acronym <- "tcdfga"
  validate_ig_fet(y = tcd_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_gaftcd_value2zscore <- function(gest_days, tcd_mm) {
  acronym <- "gaftcd"
  validate_ig_fet(y = gest_days,
                  x = tcd_mm,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_poffga_value2zscore <- function(par_occ_fiss_mm, gest_days) {
  acronym <- "poffga"
  validate_ig_fet(y = par_occ_fiss_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_sffga_value2zscore <- function(sylv_fiss_mm, gest_days) {
  acronym <- "sffga"
  validate_ig_fet(y = sylv_fiss_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_avfga_value2zscore <- function(ant_hlv_mm, gest_days) {
  acronym <- "avfga"
  validate_ig_fet(y = ant_hlv_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_pvfga_value2zscore <- function(atr_phlv_mm, gest_days) {
  acronym <- "pvfga"
  validate_ig_fet(y = atr_phlv_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_cmfga_value2zscore <- function(cist_mag_mm, gest_days) {
  acronym <- "cmfga"
  validate_ig_fet(y = cist_mag_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal)
}

#' @rdname ig_fet_value2zscore
#' @importFrom stats pnorm
#' @export
ig_fet_value2centile <- function(y, x, acronym) {
  validate_ig_fet(y = y, x = x, acronym = acronym) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_hcfga_value2centile <- function(headcirc_mm, gest_days) {
  acronym <- "hcfga"
  validate_ig_fet(y = headcirc_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_bpdfga_value2centile <- function(bpd_mm, gest_days) {
  acronym <- "bpdfga"
  validate_ig_fet(y = bpd_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_acfga_value2centile <- function(abdocirc_mm, gest_days) {
  acronym <- "acfga"
  validate_ig_fet(y = abdocirc_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_flfga_value2centile <- function(femurlen_mm, gest_days) {
  acronym <- "flfga"
  validate_ig_fet(y = femurlen_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_ofdfga_value2centile <- function(ofd_mm, gest_days) {
  acronym <- "ofdfga"
  validate_ig_fet(y = ofd_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_efwfga_value2centile <- function(efw_g, gest_days) {
  acronym <- "efwfga"
  validate_ig_fet(y = efw_g,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_sfhfga_value2centile <- function(sfh_cm, gest_days) {
  acronym <- "sfhfga"
  validate_ig_fet(y = sfh_cm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_crlfga_value2centile <- function(crl_mm, gest_days) {
  acronym <- "crlfga"
  validate_ig_fet(y = crl_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_gafcrl_value2centile <- function(gest_days, crl_mm) {
  acronym <- "gafcrl"
  validate_ig_fet(y = gest_days,
                  x = crl_mm,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_gwgfga_value2centile <- function(gest_wt_gain_kg, gest_days) {
  acronym <- "gwgfga"
  validate_ig_fet(y = gest_wt_gain_kg,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_pifga_value2centile <- function(puls_idx, gest_days) {
  acronym <- "pifga"
  validate_ig_fet(y = puls_idx,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_rifga_value2centile <- function(resist_idx, gest_days) {
  acronym <- "rifga"
  validate_ig_fet(y = resist_idx,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_sdrfga_value2centile <- function(sys_dia_ratio, gest_days) {
  acronym <- "sdrfga"
  validate_ig_fet(y = sys_dia_ratio,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_tcdfga_value2centile <- function(tcd_mm, gest_days) {
  acronym <- "tcdfga"
  validate_ig_fet(y = tcd_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_gaftcd_value2centile <- function(gest_days, tcd_mm) {
  acronym <- "gaftcd"
  validate_ig_fet(y = gest_days,
                  x = tcd_mm,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_poffga_value2centile <- function(par_occ_fiss_mm, gest_days) {
  acronym <- "poffga"
  validate_ig_fet(y = par_occ_fiss_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_sffga_value2centile <- function(sylv_fiss_mm, gest_days) {
  acronym <- "sffga"
  validate_ig_fet(y = sylv_fiss_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_avfga_value2centile <- function(ant_hlv_mm, gest_days) {
  acronym <- "avfga"
  validate_ig_fet(y = ant_hlv_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_pvfga_value2centile <- function(atr_phlv_mm, gest_days) {
  acronym <- "pvfga"
  validate_ig_fet(y = atr_phlv_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_cmfga_value2centile <- function(cist_mag_mm, gest_days) {
  acronym <- "cmfga"
  validate_ig_fet(y = cist_mag_mm,
                  x = gest_days,
                  acronym = acronym,
                  y_name = gigs::ig_fet[[acronym]][["y"]],
                  x_name = gigs::ig_fet[[acronym]][["x"]]) |>
    do.call(what = ig_fet_v2z_internal) |>
    pnorm()
}

# INTERNAL: INTERGROWTH-21st Fetal standards conversion logic ------------------

#' Convert z-scores to values in the INTERGROWTH-21<sup>st</sup> Fetal standards
#' @inherit ig_fet_zscore2value params return
#' @note This function will fail if given inputs of different lengths.
#' @noRd
ig_fet_z2v_internal <- function(z, x, acronym) {
  if (acronym %in% c("pifga", "rifga", "sdrfga")) {
    ig_fet_doppler_z2y(z = z, gest_days = x, acronym = acronym)
  } else if (acronym == "efwfga") {
    ig_fet_efw_z2y(z = z, gest_days = x)
  } else if (acronym == "gwgfga") {
    ig_fet_gwg_z2y(z = z, gest_days = x)
  } else {
    ig_fet_mu_sigma_z2y(z = z, x = x, acronym = acronym)
  }
}

#' Convert values to z-scores in the INTERGROWTH-21<sup>st</sup> Fetal standards
#' @inherit ig_fet_zscore2value params return
#' @note This function will fail if given inputs of different lengths.
#' @noRd
ig_fet_v2z_internal <- function(y, x, acronym) {
  if (acronym %in% c("pifga", "rifga", "sdrfga")) {
    ig_fet_doppler_y2z(y = y, gest_days = x, acronym = acronym)
  } else if (acronym == "efwfga") {
    ig_fet_efw_y2z(efw_g = y, gest_days = x)
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
#'   have the units/values described in the [ig_fet_value2zscore()]
#'   documentation.
#' @param acronym A single string denoting an INTERGROWTH-21<sup>st</sup> fetal
#'   standard. Each element should be one of the acronyms described in the
#'   [ig_fet_value2zscore()] documentation.
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

#' Get lambda/mu/sigma values for the INTERGROWTH-21st estimated fetal weight
#' standard (internal; part of the INTERGROWTH-21st Fetal Growth standards)
#'
#' @param gest_days Numeric vector of gestational age in days, ranging from
#'   `154` to `280`.
#' @references
#' Stirnemann J, Villar J, Salomon LJ, Ohuma EO, Lamber A, Victoria CG et al.
#' **International Estimated Fetal Weight Standards of the INTERGROWTH-21st
#' Project.** *Ultrasound Obstet Gynecol* 2016, **49:478-486**
#' \doi{10.1002/uog.17347}
#' @returns Named list with three elements `l` (lambda), `m` (mu),
#'   and `s` (sigma), each of which is the same length as `gest_days`.
#' @noRd
ig_fet_efw_lms <- function(gest_days) {
  GA <- gest_days / 7
  log_ga <- log(GA)
  list(l = -4.257629 - 2162.234 * GA^(-2) + 0.0002301829 * GA^3,
       m = 4.956737 + 0.0005019687 * GA^3 - 0.0001227065 * GA^3 * log_ga,
       s = 1e-04 * (-6.997171 + 0.057559 * GA^3 - 0.01493946 * GA^3 * log_ga))
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
#' @returns Numeric vector with same length as `z` and `gest_days` containing
#'   z-scores.
#' @srrstats {G3.0} Using `abs() < sqrt(.Machine$double.eps)` for floating point
#'   equality.
#' @noRd
ig_fet_efw_y2z <- function(efw_g, gest_days) {
  lms <- ig_fet_efw_lms(gest_days)
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
#' @returns Numeric vector with same length as `z` and `gest_days` containing
#'   expected estimated fetal weight values in grams.
#' @srrstats {G3.0} Using `abs() < sqrt(.Machine$double.eps)` for floating point
#'   equality.
#' @noRd
ig_fet_efw_z2y <- function(z, gest_days) {
  lms <- ig_fet_efw_lms(gest_days)
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
#' @srrstats {G1.0} Primary literature referenced for each exported function,
#'   and for internal functions.
#' @srrstats {G1.4, G1.4a} All functions in file documented using `{roxygen2}`.
#' @srrstats {G2.0a, G2.1a, EA1.3} Exported function in this file document
#'   expectations on the length of inputs and their data types.
#' @srrstats {G2.0, G2.1, G2.2, G2.3, G2.3a, G2.6} These standards
#'   are met in all exported functions by passing inputs to [validate_ig_fet()].
#'   All internal functions in this script are provided with vectors that have
#'   already been validated.
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.16} These standards are met
#'   in all exported functions by passing inputs to [validate_ig_fet()]. All
#'   internal functions in this script are provided with vectors that have
#'   already checked for missing/undefined/out-of-bounds data.
NULL
