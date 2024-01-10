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
#' @param x Numeric vector of x values of length 1 or more. Elements should have
#'   specific units and be between certain values depending on the standard in
#'   use (defined by `acronym`). These are:
#'   * Between 98 and 280 days for `"hcfga"`, `"bpdfga"`, `"acfga"`,
#'     `"flfga"`, `"ofdfga"`, and `"tcdfga"`.
#'   * Between 154 and 280 days for `"efwfga"`.
#'   * Between 112 and 294 days for `"sfhfga"`.
#'   * Between 58 and 105 days for `"crlfga"`.
#'   * Between 19 and 95 mm for `"gafcrl"`.
#'   * Between 105 and 280 days for `"gwgfga"`.
#'   * Between 168 and 280 days for `"pifga"`, `"rifga"`, and `"sdrfga"`
#'     (the INTERGROWTH-21st Fetal Doppler standards).
#'   * Between 105 and 252 days for `"poffga"`, `"sffga"`, `"avfga"`,
#'     and `"pvfga"` (the INTERGROWTH-21st Fetal Brain Development
#'     standards).
#'
#'   The function will return NA for each observation where `x` is not within
#'   these bounds.
#' @param acronym Character vector of length one or more denoting the
#'   INTERGROWTH-21<sup>st</sup> Fetal Growth standard(s) in use. Each value
#'   should be one of:
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
#'   This argument is case-sensitive. If `acronym` is not one of these values,
#'   the function will return NA.
#' @param gest_days,crl_mm Standard-specific x variables. These should be within
#'   the bounds given in the documentation for the `acronym` argument.
#' @inherit shared_roxygen_params params note
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
#' @srrstats {G1.0} Primary literature referenced here.
#' @examples
#' # Convert centiles to values
#' p <- 0.25 # 25th centile
#' ig_fet_centile2value(p = p, x = 280, acronym = "hcfga") |>
#'   round(digits = 2)
#'
#' # Or z-scores to values
#' z <- qnorm(p)
#' ig_fet_zscore2value(z = z, x = 280, acronym = "hcfga") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' ig_fet_zscore2value(z = z, x = 280, acronym = "acfga") |>
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
#' # Growth standard for head circumference-for-GA
#' ig_fet_hcfga_zscore2value(z = z, gest_days = c(91, 98, 224, 266)) |>
#'   round(digits = 2)
#' @rdname ig_fet_zscore2value
#' @export
ig_fet_zscore2value <- function(z, x, acronym) {
  validated <- vctrs::vec_recycle_common(z = z,
                                         x = x,
                                         acronym = acronym) |>
    do.call(what = validate_ig_fet)

  ifelse(
    test = validated[["acronym"]] %in% c("pifga", "rifga", "sdrfga"),
    yes = ig_fet_doppler_z2y(z = validated[["z"]],
                             gest_days = validated[["x"]],
                             acronym = validated[["acronym"]]),
    no = ifelse(
      test = validated[["acronym"]] == "efwfga",
      yes = ig_fet_efw_z2y(z = validated[["z"]],
                           gest_days = validated[["x"]]),
      no = ifelse(
        test = validated[["acronym"]] == "gwgfga",
        yes = ig_fet_gwg_z2y(z = validated[["z"]],
                             gest_days = validated[["x"]]),
        no = ig_fet_mu_sigma_z2y(z = validated[["z"]],
                                 x = validated[["x"]],
                                 acronym = validated[["acronym"]])
      )
    )
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
#' @export
ig_fet_sfhfga_zscore2value <- function(z, gest_days) {
  ig_fet_zscore2value(z, gest_days, acronym = "sfhfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_crlfga_zscore2value <- function(z, gest_days) {
  ig_fet_zscore2value(z, gest_days, acronym = "crlfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_gafcrl_zscore2value <- function(z, crl_mm) {
  ig_fet_zscore2value(z, crl_mm, acronym = "gafcrl")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_gwgfga_zscore2value <- function(z, gest_days) {
  ig_fet_zscore2value(z, gest_days, acronym = "gwgfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_pifga_zscore2value <- function(z, gest_days) {
  ig_fet_zscore2value(z, gest_days, acronym = "pifga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_rifga_zscore2value <- function(z, gest_days) {
  ig_fet_zscore2value(z, gest_days, acronym = "rifga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_sdrfga_zscore2value <- function(z, gest_days) {
  ig_fet_zscore2value(z, gest_days, acronym = "sdrfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_tcdfga_zscore2value <- function(z, gest_days) {
  ig_fet_zscore2value(z, gest_days, acronym = "tcdfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_poffga_zscore2value <- function(z, gest_days) {
  ig_fet_zscore2value(z, gest_days, acronym = "poffga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_sffga_zscore2value <- function(z, gest_days) {
  ig_fet_zscore2value(z, gest_days, acronym = "sffga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_avfga_zscore2value <- function(z, gest_days) {
  ig_fet_zscore2value(z, gest_days, acronym = "avfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_pvfga_zscore2value <- function(z, gest_days) {
  ig_fet_zscore2value(z, gest_days, acronym = "pvfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_cmfga_zscore2value <- function(z, gest_days) {
  ig_fet_zscore2value(z, gest_days, acronym = "cmfga")
}

#' @rdname ig_fet_zscore2value
#' @importFrom stats qnorm
#' @export
ig_fet_centile2value <- function(p, x, acronym) {
  ig_fet_zscore2value(qnorm(p), x, acronym = acronym)
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

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_sfhfga_centile2value <- function(p, gest_days) {
  ig_fet_centile2value(p, gest_days, acronym = "sfhfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_crlfga_centile2value <- function(p, gest_days) {
  ig_fet_centile2value(p, gest_days, acronym = "crlfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_gafcrl_centile2value <- function(p, crl_mm) {
  ig_fet_centile2value(p, crl_mm, acronym = "gafcrl")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_gwgfga_centile2value <- function(p, gest_days) {
  ig_fet_centile2value(p, gest_days, acronym = "gwgfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_pifga_centile2value <- function(p, gest_days) {
  ig_fet_centile2value(p, gest_days, acronym = "pifga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_rifga_centile2value <- function(p, gest_days) {
  ig_fet_centile2value(p, gest_days, acronym = "rifga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_sdrfga_centile2value <- function(p, gest_days) {
  ig_fet_centile2value(p, gest_days, acronym = "sdrfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_tcdfga_centile2value <- function(p, gest_days) {
  ig_fet_centile2value(p, gest_days, acronym = "tcdfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_poffga_centile2value <- function(p, gest_days) {
  ig_fet_centile2value(p, gest_days, acronym = "poffga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_sffga_centile2value <- function(p, gest_days) {
  ig_fet_centile2value(p, gest_days, acronym = "sffga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_avfga_centile2value <- function(p, gest_days) {
  ig_fet_centile2value(p, gest_days, acronym = "avfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_pvfga_centile2value <- function(p, gest_days) {
  ig_fet_centile2value(p, gest_days, acronym = "pvfga")
}

#' @rdname ig_fet_zscore2value
#' @export
ig_fet_cmfga_centile2value <- function(p, gest_days) {
  ig_fet_centile2value(p, gest_days, acronym = "cmfga")
}

#' Convert values to z-scores/centiles in the INTERGROWTH-21<sup>st</sup> Fetal
#' Growth standards
#'
#' @param gest_days Numeric vector of that is usually used as the x variable,
#'   but can also be the y variable for the `"gafcrl"` standard. When
#'   `gest_days` is the x variable, each element of `gest_days` should have
#'   specific units and be within the boulds defined in the description for the
#'   argument `x`.
#' @param headcirc_mm Numeric vector of head circumference measurements in mm.
#' @param bpd_mm Numeric vector of biparietal diameter measurements in mm.
#' @param abdocirc_mm Numeric vector of abdominal circumference measurements
#'   in mm.
#' @param femurlen_mm Numeric vector of femur length in mm.
#' @param ofd_mm Numeric vector of in occipito-frontal diameter in mm.
#' @param efw_g Numeric vector of estimated fetal weight in g.
#' @param sfh_cm Numeric vector of symphisis-fundal height in mm.
#' @param crl_mm Numeric vector of crown-rump length in mm.
#' @param gest_wt_gain_kg Numeric vector of gestational weight gain (or loss)
#'   in kg.
#' @param puls_idx Numeric vector of pulsatility index.
#' @param resist_idx Numeric vector of resistance index.
#' @param sys_dia_ratio Numeric vector of systolic/diastolic ratio.
#' @param tcd_mm Numeric vector of transcerebellar diameter in mm.
#' @param par_occ_fiss_mm Numeric vector of parietal-occipital fissure size in
#'   mm.
#' @param sylv_fiss_mm Numeric vector of Sylvian fissure size in mm.
#' @param ant_hlv_mm Numeric vector of anterior horn of lateral ventricle
#'   measurements(s) in mm.
#' @param atr_phlv_mm Numeric vector of atrium of posterior horn of lateral
#'   ventricle measurement(s) in mm.
#' @param cist_mag_mm Numeric vector of cisterna magna measurement(s) in mm.
#' @inheritParams shared_roxygen_params
#' @inherit ig_fet_zscore2value params references
#' @examples
#' # Convert values to centiles
#' y <- 335
#' ig_fet_value2centile(y = y, x = 280, acronym = "hcfga") |>
#'   round(digits = 2)
#'
#' # Or values to z-scores
#' ig_fet_value2zscore(y = y, x = 280, acronym = "hcfga") |>
#'   round(digits = 2)
#'
#' # Specify which standard to use with the acronym parameter...
#' ig_fet_value2zscore(y = 340, x = 280, acronym = "acfga") |>
#'   round(digits = 2)
#'
#' # ... or by using a standard-specific function
#' ig_fet_acfga_value2zscore(abdocirc_mm = 340, gest_days = 280) |>
#'   round(digits = 2)
#'
#' # Inputs are recycled to the input of the longest length
#' ig_fet_efwfga_value2zscore(efw_g = seq(3200, 3500, by = 50),
#'                            gest_days = 280) |>
#'   round(digits = 2)
#'
#' # Bad inputs will not stop the function but will instead return NA - here 90
#' # days for `gest_days` is outside the bounds of the INTERGROWTH-21st Fetal
#' # Growth standards
#' ig_fet_hcfga_value2zscore(headcirc_mm = c(90, 100, 300, 310),
#'                           gest_days = c(91, 98, 224, 266)) |>
#'   round(digits = 2)
#' @rdname ig_fet_value2zscore
#' @export
ig_fet_value2zscore <- function(y, x, acronym) {
  validated <- vctrs::vec_recycle_common(y = y,
                                         x = x,
                                         acronym = acronym) |>
    do.call(what = validate_ig_fet)

  ifelse(
    test = validated[["acronym"]] %in% c("pifga", "rifga", "sdrfga"),
    yes = ig_fet_doppler_y2z(y = validated[["y"]],
                             gest_days = validated[["x"]],
                             acronym = validated[["acronym"]]),
    no = ifelse(
      test = validated[["acronym"]] == "efwfga",
      yes = ig_fet_efw_y2z(efw_g = validated[["y"]],
                           gest_days = validated[["x"]]),
      no = ifelse(
        test = validated[["acronym"]] == "gwgfga",
        yes = ig_fet_gwg_y2z(gwg_kg = validated[["y"]],
                             gest_days = validated[["x"]]),
        no = ig_fet_mu_sigma_y2z(y = validated[["y"]],
                                 x = validated[["x"]],
                                 acronym = validated[["acronym"]])
      )
    )
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
#' @export
ig_fet_sfhfga_value2zscore <- function(sfh_cm, gest_days) {
  ig_fet_value2zscore(sfh_cm, gest_days, acronym = "sfhfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_crlfga_value2zscore <- function(crl_mm, gest_days) {
  ig_fet_value2zscore(crl_mm, gest_days, acronym = "crlfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_gafcrl_value2zscore <- function(gest_days, crl_mm) {
  ig_fet_value2zscore(gest_days, crl_mm, acronym = "gafcrl")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_gwgfga_value2zscore <- function(gest_wt_gain_kg, gest_days) {
  ig_fet_value2zscore(gest_wt_gain_kg, gest_days, acronym = "gwgfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_pifga_value2zscore <- function(puls_idx, gest_days) {
  ig_fet_value2zscore(puls_idx, gest_days, acronym = "pifga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_rifga_value2zscore <- function(resist_idx, gest_days) {
  ig_fet_value2zscore(resist_idx, gest_days, acronym = "rifga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_sdrfga_value2zscore <- function(sys_dia_ratio, gest_days) {
  ig_fet_value2zscore(sys_dia_ratio, gest_days, acronym = "sdrfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_tcdfga_value2zscore <- function(tcd_mm, gest_days) {
  ig_fet_value2zscore(tcd_mm, gest_days, acronym = "tcdfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_poffga_value2zscore <- function(par_occ_fiss_mm, gest_days) {
  ig_fet_value2zscore(par_occ_fiss_mm, gest_days, acronym = "poffga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_sffga_value2zscore <- function(sylv_fiss_mm, gest_days) {
  ig_fet_value2zscore(sylv_fiss_mm, gest_days, acronym = "sffga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_avfga_value2zscore <- function(ant_hlv_mm, gest_days) {
  ig_fet_value2zscore(ant_hlv_mm, gest_days, acronym = "avfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_pvfga_value2zscore <- function(atr_phlv_mm, gest_days) {
  ig_fet_value2zscore(atr_phlv_mm, gest_days, acronym = "pvfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_cmfga_value2zscore <- function(cist_mag_mm, gest_days) {
  ig_fet_value2zscore(cist_mag_mm, gest_days, acronym = "cmfga")
}

#' @rdname ig_fet_value2zscore
#' @importFrom stats pnorm
#' @export
ig_fet_value2centile <- function(y, x, acronym) {
  pnorm(ig_fet_value2zscore(y, x, acronym))
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

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_sfhfga_value2centile <- function(sfh_cm, gest_days) {
  ig_fet_value2centile(sfh_cm, gest_days, acronym = "sfhfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_crlfga_value2centile <- function(crl_mm, gest_days) {
  ig_fet_value2centile(crl_mm, gest_days, acronym = "crlfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_gafcrl_value2centile <- function(gest_days, crl_mm) {
  ig_fet_value2centile(gest_days, crl_mm, acronym = "gafcrl")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_gwgfga_value2centile <- function(gest_wt_gain_kg, gest_days) {
  ig_fet_value2centile(gest_wt_gain_kg, gest_days, acronym = "gwgfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_pifga_value2centile <- function(puls_idx, gest_days) {
  ig_fet_value2centile(puls_idx, gest_days, acronym = "pifga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_rifga_value2centile <- function(resist_idx, gest_days) {
  ig_fet_value2centile(resist_idx, gest_days, acronym = "rifga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_sdrfga_value2centile <- function(sys_dia_ratio, gest_days) {
  ig_fet_value2centile(sys_dia_ratio, gest_days, acronym = "sdrfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_tcdfga_value2centile <- function(tcd_mm, gest_days) {
  ig_fet_value2centile(tcd_mm, gest_days, acronym = "tcdfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_poffga_value2centile <- function(par_occ_fiss_mm, gest_days) {
  ig_fet_value2centile(par_occ_fiss_mm, gest_days, acronym = "poffga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_sffga_value2centile <- function(sylv_fiss_mm, gest_days) {
  ig_fet_value2centile(sylv_fiss_mm, gest_days, acronym = "sffga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_avfga_value2centile <- function(ant_hlv_mm, gest_days) {
  ig_fet_value2centile(ant_hlv_mm, gest_days, acronym = "avfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_pvfga_value2centile <- function(atr_phlv_mm, gest_days) {
  ig_fet_value2centile(atr_phlv_mm, gest_days, acronym = "pvfga")
}

#' @rdname ig_fet_value2zscore
#' @export
ig_fet_cmfga_value2centile <- function(cist_mag_mm, gest_days) {
  ig_fet_value2centile(cist_mag_mm, gest_days, acronym = "cmfga")
}

# INTERNAL: IG-21st Fetal standards: normally distributed ----------------------

#' INTERGROWTH-21<sup>st</sup> equations for normally distributed aspects of
#' fetal growth (internal)
#'
#' Estimates median and standard deviation for different measures of fetal
#' growth.
#'
#' @param x Numeric vector of x variable, which should have the units/values
#'   described in the [ig_fet_value2zscore()] documentation.
#' @param acronym Character vector of acronym(s) denoting an
#'   INTERGROWTH-21<sup>st</sup> fetal standard. Each element should be one of
#'   the acronyms described in the [ig_fet_value2zscore()] documentation.
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
#' @srrstats {G1.0} Primary literature referenced here.
#' @noRd
ig_fet_mu_sigma <- function(x, acronym) {
  mu_fns <- list(
    # Fetal Growth standards
    hcfga = \(GA) -28.2849 + 1.69267 * GA^2 - 0.397485 * GA^2 * log(GA),
    bpdfga = \(GA) 5.60878 + 0.158369 * GA^2 - 0.00256379 * GA^3,
    acfga = \(GA) -81.3243 + 11.6772 * GA - 0.000561865 * GA^3,
    flfga = \(GA) -39.9616 + 4.32298 * GA - 0.0380156 * GA^2,
    ofdfga = \(GA) -12.4097 + 0.626342 * GA^2 - 0.148075 * GA^2 * log(GA),
    # Symphisis-fundal height standard
    sfhfga = \(GA) 5.133374 + 0.1058353119 * GA^2 - 0.0231295 * GA^2 * log(GA),
    # CRL/early pregnancy dating standards --> uses GA in days
    crlfga = \(GA) -50.6562 + (0.815118 * GA * 7) + (0.00535302 * (GA * 7)^2),
    gafcrl = \(x) {
      CRL <- x * 7
      40.9041 + 3.21585 * (CRL)^0.5 + 0.348956 * (CRL)
    },
    # Cerebellar development
    tcdfga = \(GA) -13.10907 + 20.8941 * (GA/10)^0.5 + 0.5035914 * (GA/10)^3,
    ## 04/01/2024: `gaftcd` not included whilst equation is dubious
    ## gaftcd = \(TCD) 3.957113 + 8.154074 * TCD/10 - 0.076187 * (TCD/10)^3
    # Fetal brain development standards
    poffga = \(GA) 10.29428 - (122.8447 * GA^-1) + (0.00001038 * GA^3),
    sffga = \(GA) {
      80.27012 - (32.7877 * GA^-0.5) - (100.1593 * GA^-0.5 * log(GA))
    },
    avfga = \(GA) 6.396214 + (0.00006205 * GA^3),
    pvfga = \(GA) 4.389214 + (38.10015 * GA^-1) + (0.0000020063 * GA^3),
    # n.b. cmfga is LOGARITHMIC
    cmfga = \(GA) 2.098095 - (239.0659 * GA^-2) - 0.0000001547 * GA^3
  )
  sigma_fns <- list(
    # Fetal Growth standards
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
    },
    # Fetal Growth standards
    sfhfga = \(GA) 0.9922667 + 0.0258087 * GA,
    # CRL/early pregnancy dating standards --> GA in days
    crlfga = \(GA) -2.21626 + (0.0984894 * GA * 7),
    gafcrl = \(x) {
      CRL <- x * 7
      2.39102 + (0.0193474 * CRL)
    },
    # Fetal Growth standards
    tcdfga = \(GA) 0.4719837 + 0.0500382 * (GA/10)^3,
    ## 04/01/2024: `gaftcd` not included whilst equation is dubious
    ## gaftcd = \(TCD) 7 * (1.577198 -1.30374 * (TCD/10)^-0.5)
    # Fetal brain development standards
    poffga = \(GA) 1.596042 - (257.2297 * GA^-2),
    sffga = \(GA) 2.304501 - (353.814 * GA^-2),
    avfga = \(GA) 0 * GA + 1.204454,
    pvfga = \(GA) 0.6707227 + (0.034258 * GA),
    cmfga = \(GA) 0.2297936 + (8.1872 * GA^-2) # n.b. LOGARITHMIC
  )

  len_out <- length(x)
  mu <- rep(NA_real_, len_out)
  sigma <- rep(NA_real_, len_out)
  x <- x / 7

  is_na_acronym <- is.na(acronym)
  has_mu_sigma_acronym <- acronym %in% names(mu_fns) & !is_na_acronym
  acronyms <- unique(acronym[has_mu_sigma_acronym])
  for (curr_acronym in acronyms) {
    has_curr_acronym <- acronym == curr_acronym & !is_na_acronym
    current_x <- x[has_curr_acronym]
    mu[has_curr_acronym] <- mu_fns[[curr_acronym]](current_x)
    sigma[has_curr_acronym] <- sigma_fns[[curr_acronym]](current_x)
  }
  list(mu = mu, sigma = sigma)
}

#' Convert values to z-scores for the INTERGROWTH-21st Fetal standards based on
#' simple mu-sigma models (internal; covers most Fetal standards)
#' @param y Numeric vector of y values of length 1 or greater.
#' @inheritParams ig_fet_mu_sigma
#' @noRd
ig_fet_mu_sigma_y2z <- function(y, x, acronym) {
  mu_sigma <- ig_fet_mu_sigma(x = x, acronym = acronym)
  y[acronym == "cmfga"] <- log(y[acronym == "cmfga"])
  z <- with(mu_sigma, mu_sigma_y2z(y, mu, sigma))
}

#' Convert z-scores to values for the INTERGROWTH-21st Fetal standards based on
#' simple mu-sigma models (internal; covers most Fetal standards)
#' @param z Numeric vector of length 1 or greater with z-scores.
#' @inheritParams ig_fet_mu_sigma
#' @noRd
ig_fet_mu_sigma_z2y <- function(z, x, acronym) {
  mu_sigma <- ig_fet_mu_sigma(x = x, acronym = acronym)
  y <- with(mu_sigma, mu_sigma_z2y(z, mu, sigma))
  y <- ifelse(acronym != "cmfga", yes = y, no = exp(y))
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
  gest_wks <- gest_days / 7
  lambda <- \(GA) -4.257629 - 2162.234 * GA^-2 + 0.0002301829 * GA^3
  mu <- \(GA) 4.956737 + 0.0005019687 * GA^3 - 0.0001227065 * GA^3 * log(GA)
  sigma <- \(GA) {
    10^-4 * (-6.997171 + 0.057559 * GA^3 - 0.01493946 * GA^3 * log(GA))
  }
  list(l = lambda(gest_wks),
       m = mu(gest_wks),
       s = sigma(gest_wks))
}

#' Convert values to z-scores in the INTERGROWTH-21st estimated fetal weight
#' standard (internal; part of the INTERGROWTH-21st Fetal Growth standards)
#'
#' @param efw_g Numeric vector with estimated fetal weight(s) in grams.
#' @param gest_days Numeric vector with gestational age(s) in days.
#' @references
#' Stirnemann J, Villar J, Salomon LJ, Ohuma EO, Lamber A, Victoria CG et al.
#' **International Estimated Fetal Weight Standards of the INTERGROWTH-21st
#' Project.** *Ultrasound Obstet Gynecol* 2016, **49:478-486**
#' \doi{10.1002/uog.17347}
#' @returns Numeric vector with same length as `efw_g` and `gest_days`
#'   containing z-scores.
#' @srrstats {G3.0} Using `abs() < .Machine$double.eps` for floating point
#'   equality.
#' @noRd
ig_fet_efw_y2z <- function(efw_g, gest_days) {
  lms <- ig_fet_efw_lms(gest_days)
  efw_g[efw_g < 0] <- NA
  log_efw <- log(efw_g)
  with(lms,
       ifelse(test = abs(l) < sqrt(.Machine$double.eps),
               yes = s^-1 * log(log_efw / m),
               no = (s * l)^-1 * ((log_efw / m)^l - 1))
  )
}

#' Convert z-scores to values in the INTERGROWTH-21st estimated fetal weight
#' standard (internal; part of the INTERGROWTH-21st Fetal Growth standards)
#'
#' @param z Numeric vector with estimated fetal weight(s) in grams.
#' @param gest_days Numeric vector with gestational age(s) in days.
#' @references
#' Stirnemann J, Villar J, Salomon LJ, Ohuma EO, Lamber A, Victoria CG et al.
#' **International Estimated Fetal Weight Standards of the INTERGROWTH-21st
#' Project.** *Ultrasound Obstet Gynecol* 2016, **49:478-486**
#' \doi{10.1002/uog.17347}
#' @returns Numeric vector with same length as `z` and `gest_days`
#'   containing expected estimated fetal weight values in grams.
#' @srrstats {G3.0} Using `abs() < .Machine$double.eps` for floating point
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

#' INTERGROWTH-21<sup>st</sup> fetal doppler lambda/mu/sigma equations
#'   (internal; used for the INTERGROWTH-21st Fetal Doppler standards)
#'
#' @param gest_days Numeric vector with gestational age in days.
#' @param acronym Character vector with acronyms denoting INTERGROWTH-21st Fetal
#'   Doppler standard in use. Only `"pifga"`, `"rifga"` and `"sdrfga"` will
#'   return LMS values.
#' @note The LMS values produced here are not used in Royston's LMS format (like
#'   in the WHO Growth Standards), but instead in Royston & Wright's exponential
#'   normal (EN) model. See [ig_fet_doppler_y2z()] for the reference.
#' @references
#' Drukker L, Staines-Urias E, Villar J, Barros FC, Carvalho M, Munim S et al.
#' **International gestational age-specific centiles for umbilical artery
#' Doppler indices: a longitudinal prospective cohort study of the
#' INTERGROWTH-21st Project.** *Am J Obstet Gynecol* 2021,
#' **222(6):602.e1-602.e15** \doi{10.1016/j.ajog.2020.01.012}
#' @returns List of three numeric vectors equal in length to `gest_days`.
#' @noRd
ig_fet_doppler_lms <- function(gest_days, acronym) {
  ig_fet_doppler_L <- c(pifga = -0.0768617,
                        rifga = 0.0172944,
                        sdrfga = -0.2752483)
  ig_fet_doppler_M <- list(
    pifga = \(GA) 1.02944 + 77.7456 * GA^-2 - 0.000004455 * GA^3,
    rifga = \(GA) 0.674914 + 25.3909 * GA^-2 - 0.0000022523 * GA^3,
    sdrfga = \(GA) 2.60358 + 445.991 * GA^-2 - 0.0000108754 * GA^3
  )
  ig_fet_doppler_S <- list(
    pifga = \(GA) -0.00645693 + 254.885 * log(GA) * GA^-2 - 715.949 * GA^-2,
    rifga = \(GA) 0.0375921 + 60.7614 * log(GA) * GA^-2 - 183.336 * GA^-2,
    sdrfga = \(GA) -0.503202 + 1268.37 * log(GA) * GA^-2 - 3417.37 * GA^-2
  )

  # Initialise empty vectors
  len_out <- length(gest_days)
  lambda <- rep(NA_real_, len_out)
  mu <- rep(NA_real_, len_out)
  sigma <- rep(NA_real_, len_out)
  gest_wks <- gest_days / 7

  is_na_acronym <- is.na(acronym)
  has_doppler_acronym <- acronym %in% names(ig_fet_doppler_L) & !is_na_acronym
  acronyms <- unique(acronym[has_doppler_acronym])
  for (curr_acronym in acronyms) {
    has_curr_acronym <- acronym == curr_acronym & !is_na_acronym
    ga <- gest_wks[has_curr_acronym]
    lambda[has_curr_acronym] <- ig_fet_doppler_L[[curr_acronym]]
    mu[has_curr_acronym] <- ig_fet_doppler_M[[curr_acronym]](GA = ga)
    sigma[has_curr_acronym] <- ig_fet_doppler_S[[curr_acronym]](GA = ga)
  }
  list(l = lambda, m = mu, s = sigma)
}

#' Convert fetal doppler values to z-scores for specific gestational ages
#' (internal)
#'
#' @param y Numeric vector of length 1 or more with umibilical artery doppler
#'   values, specific to the acronym(s) in use.
#' @param gest_days Numeric vector of length 1 or `length(y)` with
#'   gestational ages in days.
#' @param acronym Character vector of length 1 or `length(y)` with
#'   INTERGROWTH-21<sup>st</sup> umbilical artery doppler standard(s) in use.
#' @note Uses an inverted form of the exponential normal model equation defined
#'   in section 3.1.6 of the attached reference. This model adjusts for skewness
#'   in the distribution of umbilical artery indices at different gestational
#'   ages.
#' @references
#' Royston P, Wright EM. **A Method for Estimating Age-Specific Reference
#' Intervals (‘Normal Ranges’) Based on Fractional Polynomials and Exponential
#' Transformation** *J R Statist Soc A* 1998, **161(Part 1):79-101**
#' \doi{10.1111/1467-985X.00091}
#' @noRd
ig_fet_doppler_y2z <- function(y, gest_days, acronym) {
  lms <- ig_fet_doppler_lms(gest_days, acronym)
  # Inversion of EN model equation from section 3.1.6 of referenced paper
  with(lms, (exp((y - m) * l * s^-1) - 1) / l)
}

#' Convert fetal doppler z-scores to values for specific gestational ages
#' (internal)
#'
#' @param y Numeric vector of length 1 or more with umibilical artery doppler
#'   values, specific to the acronym(s) in use.
#' @param gest_days Numeric vector of length 1 or more with gestational ages in
#'   days.
#' @param acronym Character vector of `length(y)` with
#'   INTERGROWTH-21<sup>st</sup> umbilical artery doppler standard(s) in use.
#' @note Uses the exponential normal model equation defined in section 3.1.6 of
#'   the attached reference. This model adjusts for skewness in the distribution
#'   of umbilical artery indices at different gestational ages.
#' @references
#' Royston P, Wright EM. **A Method for Estimating Age-Specific Reference
#' Intervals (‘Normal Ranges’) Based on Fractional Polynomials and Exponential
#' Transformation** *J R Statist Soc A* 1998, **161(Part 1):79-101**
#' \doi{10.1111/1467-985X.00091}
#' @noRd
ig_fet_doppler_z2y <- function(z, gest_days, acronym) {
  lms <- ig_fet_doppler_lms(gest_days, acronym)
  # EN model equation from section 3.1.6 of referenced paper
  with(lms, {
    log_val <- 1 + l * z
    out <- rep.int(x = NA_real_, length(z))
    use <- !is.na(log_val) & log_val > 0
    out[use] <- m[use] + s[use] * log(log_val[use]) / l[use]
    out
  })


}

# INTERNAL; IG-21st Gestational Weight Gain ------------------------------------

#' INTERGROWTH-21<sup>st</sup> gestational weight gain mu/sigma equations
#'   (internal; used for the INTERGROWTH-21st gestational weight gain standard)
#'
#' @param gest_days Numeric vector with gestational age in days.
#' @note The mu/sigma values returned by this equation have the units
#'   log(gestational weight gain).
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
  log_mu <- 1.382972 - 56.14743 * GA^-2 + 0.2787683 * GA^0.5
  log_sigma <- 0.2501993731 + 142.4297879 * GA^-2 - 61.45345 * GA^-2 * log(GA)
  list(log_mu = log_mu, log_sigma = log_sigma)
}

#' Convert gestational weight gain values to z-scores for specific
#'   gestational ages (internal)
#'
#' @param gwg_kg Numeric vector of length 1 or more with gestational weight gain
#'   values in kg.
#' @param gest_days Numeric vector of length 1 or `length(y)` with
#'   gestational ages in days.
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

#' Convert gestational weight gain z-scores to values for specific
#'   gestational ages (internal)
#'
#' @param z Numeric vector of length 1 or more containing z-scores.
#' @param gest_days Numeric vector of `length(y)` with gestational ages in days.
#' @note Uses the equations/method in Table 4 of the attached reference to
#'   calculate z-scores.
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