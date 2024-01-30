#' gigs: Assess Growth in Infants and Newborns
#'
#' Convert between anthropometric measures and z-scores/centiles using WHO
#' and INTERGROWTH-21<sup>st</sup> growth standards. This includes functions for
#' classification of newborn and infant growth, including size for gestational
#' age, stunting, wasting, and weight-for-age.
#'
#' @name gigs-package
#' @aliases gigs
#' @examples
#' \dontrun{
#' help(package = "gigs")
#' browseURL("https://www.github.com/lshtm-gigs/gigs")
#' }
#' @keywords internal
"_PACKAGE"

#' INTERGROWTH-21<sup>st</sup> Newborn Size Standards (including very preterm)
#' growth curve data
#'
#' @name ig_nbs
#' @description
#' A set of nested lists containing tables with reference values at different
#' z-scores/centiles for valid gestational ages in days. The list is ordered
#' by acronym first, then by sex and finally by z-score/centile.
#' @source
#' [INTERGROWTH-21<sup>st</sup> Newborn Size in Very Preterm
#'   Infants](https://intergrowth21.tghn.org/very-preterm-size-birth/#vp1)\cr
#' [INTERGROWTH-21<sup>st</sup> Newborn Size
#'   Standards](https://intergrowth21.tghn.org/newborn-size-birth/#ns1)\cr
#' [INTERGROWTH-21<sup>st</sup> Newborn Size Standards - Body
#'   Composition](https://www.nature.com/articles/pr201752)
#' @note
#' The tables in this package are combined versions of the tables published by
#' Villar *et al.* (2014) and Villar *et al.* (2016), so they cover `168` to
#' `300` days' gestational age. The body composition tables (`ffmfga`, `bfpfga`,
#' and `fmfga`) cover a smaller gestational age span, ranging from only `266` to
#' `294` days' (38 to 42 weeks') gestational age.
#' @references
#' Villar J, Giuliani F, Fenton TR, Ohuma EO, Ismail LC, Kennedy SH et al.
#' **INTERGROWTH-21st very preterm size at birth reference charts.** *Lancet*
#' 2016, **387(10021):844-45.** \doi{10.1016/S0140-6736(16)00384-6}
#'
#' Villar J, Cheikh Ismail L, Victora CG, Ohuma EO, Bertino E, Altman DG, et al.
#' **International standards for newborn weight, length, and head circumference
#' by gestational age and sex: the Newborn Cross-Sectional Study of the
#' INTERGROWTH-21st Project.** *Lancet* 2014, **384(9946):857-68.**
#' \doi{10.1016/S0140-6736(14)60932-6}
#'
#' Villar J, Puglia FA, Fenton TR, Ismal LC, Staines-Urias E, Giuliani F, et al.
#' **Body composition at birth and its relationship with neonatal anthropometric
#' ratios: the newborn body composition study of the INTERGROWTH-21st
#' project.** *Pediatric Research* 2017, **82:305-316.**
#' \doi{10.1038/pr.2017.52}
#' @srrstats {G1.0} Primary literature referenced here.
#' @examples
#' names(gigs::ig_nbs)
#' head(gigs::ig_nbs$wfga$male$zscores)
#' @docType data
#' @keywords data
NULL

#' INTERGROWTH-21<sup>st</sup> Newborn Size Standards GAMLSS coefficients
#'
#' @name ig_nbs_coeffs
#' @description
#' A set of nested lists containing mu, sigma, nu and tau values across
#' gestational ages for either sex, for the INTERGROWTH-21<sup>st</sup>
#' weight/length/head circumference-for-gestational age standards. The lists are
#' ordered by acronym, then sex.
#' @references
#' Villar J, Cheikh Ismail L, Victora CG, Ohuma EO, Bertino E, Altman DG, et al.
#' **International standards for newborn weight, length, and head circumference
#' by gestational age and sex: the Newborn Cross-Sectional Study of the
#' INTERGROWTH-21st Project.** *Lancet* 2014, **384(9946):857-68.**
#' \doi{10.1016/S0140-6736(14)60932-6}
#' @srrstats {G1.0} Primary literature referenced here.
#' @source
#' Mu/sigma/nu/tau values were provided by Dr Eric Ohuma.
#' @examples
#' names(gigs::ig_nbs_coeffs)
#' head(gigs::ig_nbs_coeffs$wfga$male)
#' @docType data
#' @keywords data
NULL

#' INTERGROWTH-21<sup>st</sup> Postnatal Growth Standards growth curve data
#'
#' @name ig_png
#' @description
#' A set of nested lists containing tables with reference values at different
#' z-scores/centiles for valid post-menstrual ages. The list is ordered by
#' acronym first, then by sex, and finally by z-score/centile.
#' @references
#' Villar J, Giuliani F, Bhutta ZA, Bertino E, Ohuma EO, Ismail LC et al.
#' **Postnatal growth standards for preterm infants: the Preterm Postnatal
#' Follow-up Study of the INTERGROWTH-21st Project.** *Lancet Glob Health* 2015,
#' *3(11):e681-e691.* \doi{10.1016/S2214-109X(15)00163-1}
#' @source
#' [INTERGROWTH-21<sup>st</sup> Postnatal Growth of Preterm
#'   Infants](https://intergrowth21.tghn.org/postnatal-growth-preterm-infants/)
#' @examples
#' names(gigs::ig_png)
#' head(gigs::ig_png$wfa$male$zscores)
#' @docType data
#' @keywords data
NULL

#' WHO Child Growth Standards growth curve data
#'
#' @name who_gs
#' @description
#' A set of nested lists containing tables with reference values at different
#' z-scores/centiles for valid x values (usually age in days, also
#' length or height in cm for weight-for-length (`wfl`) and weight-for-height
#' (`wfh`) standards, respectively). The list is ordered by acronym first, then
#' by sex and finally by z-score/centile.
#' @references
#' de Onis M, Garza C, Victora CG, Onyango AW, Frongillo EA, Martines J. **The
#' WHO Multicentre Growth Reference Study: planning, study design, and
#' methodology** *Food Nutr Bull.* 2004, **25(1 Suppl):S15-26.**
#' \doi{10.1177/15648265040251s104}
#'
#' World Health Organisation. **WHO child growth standards:
#' length/height-for-age, weight-for-age, weight-for-length, weight-for-height
#' and body mass index-for-age: methods and development.** *Technical report,
#' WHO, Geneva*, 2006.
#'
#' World Health Organisation. **WHO child growth standards: head
#' circumference-for-age, arm circumference-for-age, triceps skinfold-for-age
#' and subscapular skinfold-for-age: methods and development.** *Technical
#' report, WHO, Geneva*, 2007.
#' @srrstats {G1.0} Primary literature referenced here.
#' @source
#' [WHO Child Growth
#'   Standards](https://www.who.int/tools/child-growth-standards/standards)
#' @examples
#' names(gigs::who_gs)
#' head(gigs::who_gs$wfa$male$zscores)
#' @docType data
#' @keywords data
NULL

#' INTERGROWTH-21<sup>st</sup> Fetal Standards growth curve data
#'
#' @name ig_fet
#' @docType data
#' @description
#' A set of nested lists containing tables with reference values at different
#' z-scores/centiles for valid gestational ages in days. The list is ordered
#' by acronym first, then by z-score/centile (as available - some Fetal
#' standards only have centile tables).
#' @source
#' [INTERGROWTH-21<sup>st</sup> Pregnancy Dating
#'   (CRL)](https://intergrowth21.tghn.org/pregnancy-dating/)
#'
#' [INTERGROWTH-21<sup>st</sup> Symphysis-Fundal Height
#'   standard](https://intergrowth21.tghn.org/symphysis-fundal-height/)
#'
#' [INTERGROWTH-21<sup>st</sup> Fetal Growth
#'   standards](https://intergrowth21.tghn.org/fetal-growth/)
#'
#' [INTERGROWTH-21<sup>st</sup> Fetal Doppler
#'   standards](https://intergrowth21.tghn.org/fetal-doppler/)
#'
#' [INTERGROWTH-21<sup>st</sup> Gestational Weight Gain
#'   standard](https://intergrowth21.tghn.org/gestational-weight-gain/)
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
#' names(gigs::ig_fet)
#' head(gigs::ig_fet$hcfga$zscores)
#' @srrstats {G1.0} Primary literature referenced here.
#' @keywords data
NULL

#' WHO Child Growth Standards LMS coefficients
#'
#' @name who_gs_coeffs
#' @description
#' A set of nested lists containing tables with LMS values for each sex in
#' different combinations of age/length-height/BMI. The list is ordered by
#' acronym first, then by sex.
#' @references
#' de Onis M, Garza C, Victora CG, Onyango AW, Frongillo EA, Martines J. **The
#' WHO Multicentre Growth Reference Study: planning, study design, and
#' methodology** *Food Nutr Bull.* 2004, **25(1 Suppl):S15-26.**
#' \doi{10.1177/15648265040251s104}
#'
#' World Health Organisation. **WHO child growth standards:
#' length/height-for-age, weight-for-age, weight-for-length, weight-for-height
#' and body mass index-for-age: methods and development.** *Technical report,
#' WHO, Geneva*, 2006.
#'
#' World Health Organisation. **WHO child growth standards: head
#' circumference-for-age, arm circumference-for-age, triceps skinfold-for-age
#' and subscapular skinfold-for-age: methods and development.** *Technical
#' report, WHO, Geneva*, 2007.
#'
#' Cole TJ. **The LMS method for constructing normalized growth standards.**
#' *Eur J Clin Nutr.* 1990, **44(1):45-60.** PMID:
#' [2354692](https://pubmed.ncbi.nlm.nih.gov/2354692/)
#' @source
#' [WHO Child Growth
#'   Standards](https://www.who.int/tools/child-growth-standards/standards)
#' @examples
#' names(gigs::who_gs_coeffs)
#' head(gigs::who_gs_coeffs$lhfa$male)
#' @srrstats {G1.0} Primary literature referenced here.
#' @docType data
#' @keywords data
NULL

#' Data extract from the Low birthweight Infant Feeding Exploration (LIFE) study
#'
#' @name life6mo
#' @description A subset of anthropometric data for 300 singleton infants
#' enrolled in the Low birthweight Infant Feeding Exploration (LIFE) study. The
#' variables are as follows:
#' @format A data frame with 2,511 rows and 10 variables:
#' \itemize{
#'   \item{`id`:}{  Unique ID for each infant in the dataset (1--300).}
#'   \item{`gestage`:}{  Best obstetric estimate of gestational age in days
#'     (201--311).}
#'   \item{`sex`:}{  Sex of the infant as a factor (`"M"` = Male; `"F"` =
#'     Female).}
#'   \item{`visitweek`:}{  Chronological age in weeks (±1) when study visit
#'     occurred (0--26).}
#'   \item{`pma`:}{  Post-menstrual age in days (202--545).}
#'   \item{`age_days`:}{  Chronological age in days at each visit; equal to
#'     `pma - gestage` (0--245).}
#'   \item{`weight_g`:}{  Mean weight in g (1271.67--9050.00).}
#'   \item{`len_cm`:}{  Mean length in cm (35.03--72.93).}
#'   \item{`headcirc_cm`:}{  Mean head circumference in cm (23.20--45.17).}
#'   \item{`muac_cm`:}{  Mean mid-upper arm circumference in cm (6.13--17.03).}
#' }
#'
#' @note We subsetted the full LIFE 6 month dataset for [gigs]. As such, this
#' extract only includes data from 300 singleton pregnancies where the best
#' estimate of gestational age was >168 days. We also removed rows corresponding
#' to visit weeks where no measurement data was taken due to non-attendance of
#' the visit.
#' @references
#' Vesel L, Bellad RM, Manji K, Saidi F, Velasquez E, Sudfeld C, et al.
#' **Feeding practices and growth patterns of moderately low birthweight infants
#' in resource-limited settings: results from a multisite, longitudinal
#' observational study.** *BMJ Open* 2023, **13(2):e067316.**
#' \doi{10.1136/BMJOPEN-2022-067316}
#' @examples
#' head(gigs::life6mo)
#' @srrstats {G1.0} Primary literature referenced here.
#' @docType data
#' @keywords data
NULL

#' Shared roxygen parameters
#'
#' Many of the parameters in `[gigs]` are shared between different functions.
#' This roxygen block is designed to make inheriting these parameters as simple
#' as possible.
#'
#' @name shared_roxygen_params
#' @param y Numeric vector of length one or more with anthropometric
#'   measurement(s) to convert to centiles/z-scores. Units depend on which
#'   `acronym`(s) are in use.
#' @param z,p Numeric vector of length one or more with centiles/z-scores to
#'   convert to values. For `p`, if an element of `p` is not between `0` and
#'   `1`, gigs will replace it with `NA` and warn you. This behaviour can be
#'   customised using the functions in [gigs_options].
#' @param sex Character vector of length one or more with sex(es), either `"M"`
#'   (male) or `"F"` (female). This argument is case-sensitive. By default, gigs
#'   will replace elements of `sex` which are not `"M"` or `"F"` with `NA`
#'   and warn you. This behaviour can be customised using the functions in
#'   [gigs_options].
#' @note Input vectors are recycled by [vctrs::vec_recycle_common()].
#' @keywords internal
NULL

#' Shared z-score/centile roxygen parameters
#'
#' Much of the roxygen content in `[gigs]` is shared between different
#' functions. This roxygen block is designed to make inheriting these parameters
#' more simple.
#'
#' @name shared_zscore2value_returns
#' @returns Numeric vector of z-scores/centiles with length equal to the
#'   outputs from [vctrs::vec_recycle_common()].
#' @keywords internal
NULL

#' Shared z-score/centile to value roxygen parameters
#'
#' Much of the roxygen content in `[gigs]` is shared between different
#' functions. This roxygen block is designed to make inheriting these parameters
#' more simple.
#'
#' @name shared_value2zscore_returns
#' @returns Numeric vector of expected measurements with length equal to the
#'   outputs from [vctrs::vec_recycle_common()].
#' @keywords internal
NULL

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.4a} All package data is documented with `{roxygen2}`.