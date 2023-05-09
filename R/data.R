#' gigs: Assess Growth in Infants and Newborns
#'
#' Convert between anthropometric measures and z-scores/percentiles using WHO and INTERGROWTH-21st growth standards.
#' This package also enables conversion of z-scores/percentiles to newborn and infant size classifications, including
#' size for gestational age, stunting, and wasting.
#'
#' @docType package
#' @name gigs-package
#' @aliases gigs
#' @examples
#' \dontrun{
#' help(package = "gigs")
#' browseURL("https://www.github.com/simpar1471/gigs")
#' }
#' @keywords internal
NULL

#' INTERGROWTH-21st Newborn Standards (including very preterm) tables
#'
#' @name ig_nbs
#' @docType data
#' @description
#' A set of nested lists containing tables with reference values at different z-scores/percentiles for valid gestational
#' ages in days. The list is ordered by acronym first, then by sex and finally z-score/percentile.
#'
#' @source
#' [INTERGROWTH-21st Newborn Size in Very Preterm
#' Infants](https://intergrowth21.tghn.org/very-preterm-size-birth/#vp1)\cr
#' [INTERGROWTH-21st Newborn Size Standards](https://intergrowth21.tghn.org/newborn-size-birth/#ns1)\cr
#' [INTERGROWTH-21st Newborn Size Standards - Body Composition](https://doi.org/10.1038/pr.2017.52)
#'
#' @references
#' Villar J, Giuliani F, Fenton TR, Ohuma EO, Ismail LC, Kennedy SH et al. **INTERGROWTH-21st very
#' preterm size at birth reference charts.** *Lancet* 2016, **387(10021):844-45.**
#' \doi{10.1016/S0140-6736(16)00384-6}
#'
#' Villar J, Cheikh Ismail L, Victora CG, Ohuma EO, Bertino E, Altman DG, et al. **International
#' standards for newborn weight, length, and head circumference by gestational age and sex: the Newborn Cross-Sectional
#' Study of the INTERGROWTH-21st Project.** *Lancet* 2014, **384(9946):857-68.**
#' \doi{10.1016/S0140-6736(14)60932-6}
#'
#' Villar J, Puglia FA, Fenton TR, Ismal LC, Staines-Urias E, Giuliani F, et al. **Body composition at birth and
#' its relationship with neonatal anthropometric ratios: the newborn body composition study of the INTERGROWTH-21st
#' project** *Pediatric Research* 2017, **82:305-316.** \doi{10.1038/pr.2017.52}
#'
#' @examples
#' names(gigs::ig_nbs)
#' head(gigs::ig_nbs$wfga$male$zscores)
#' @keywords data
NULL

#' INTERGROWTH-21st Newborn Size Standards GAMLSS coefficients
#'
#' @name ig_nbs_coeffs
#' @docType data
#' @description
#' A set of nested lists containing mu, sigma, nu and tau values across gestational ages for either sex. The list is
#' ordered by acronym then sex.
#'
#' @references
#' Villar J, Cheikh Ismail L, Victora CG, Ohuma EO, Bertino E, Altman DG, et al. **International
#' standards for newborn weight, length, and head circumference by gestational age and sex: the Newborn Cross-Sectional
#' Study of the INTERGROWTH-21st Project.** *Lancet* 2014, **384(9946):857-68.**
#' \doi{10.1016/S0140-6736(14)60932-6}
#'
#' @source
#' Provided by Dr Eric Ohuma, produced for the provided reference.
#'
#' @examples
#' names(gigs::ig_nbs_coeffs)
#' head(gigs::ig_nbs_coeffs$wfga$male)
#' @docType data
#' @keywords data
NULL

#' INTERGROWTH-21st Post-natal Growth Standards tables
#'
#' @name ig_png
#' @docType data
#' @description
#' A set of nested lists containing tables with reference values at different z-scores/percentiles for valid
#' postmenstrual ages. The list is ordered by acronym first, then by sex and finally z-score/percentile.
#'
#' @references
#' Villar J, Cheikh Ismail L, Victora CG, Ohuma EO, Bertino E, Altman DG, et al. **International
#' standards for newborn weight, length, and head circumference by gestational age and sex: the Newborn Cross-Sectional
#' Study of the INTERGROWTH-21st Project.** *Lancet* 2014, **384(9946):857-68.**
#' \doi{10.1016/S0140-6736(14)60932-6}
#'
#' @source
#' [INTERGROWTH-21st Post-natal Growth of Preterm
#' Infants](https://intergrowth21.tghn.org/postnatal-growth-preterm-infants/#pg1)
#'
#' @examples
#' names(gigs::ig_png)
#' head(gigs::ig_png$wfa$male$zscores)
#' @docType data
#' @keywords data
NULL

#' WHO Growth Standards tables
#'
#' @name who_gs
#' @description
#' A set of nested lists containing tables with reference values at different z-scores/percentiles for valid
#' postmenstrual ages. The list is ordered by acronym first, then by sex and finally z-score/percentile.
#'
#' @references
#' de Onis M, Garza C, Victora CG, Onyango AW, Frongillo EA, Martines J. **The WHO Multicentre Growth Reference
#' Study: planning, study design, and methodology** *Food Nutr Bull.* 2004, **25(1 Suppl):S15-26.**
#' doi: [10.1177/15648265040251s104](https://journals.sagepub.com/doi/10.1177/15648265040251S104)
#'
#' @source
#' [WHO Child Growth Standards](https://www.who.int/tools/child-growth-standards/standards)
#'
#' @examples
#' names(gigs::who_gs)
#' head(gigs::who_gs$wfa$male$zscores)
#' @docType data
#' @keywords data
NULL

#' WHO Growth Standards LMS coefficients
#'
#' @name who_gs_coeffs
#' @description
#' A set of nested lists containing tables with LMS values for each sex in different combinations of
#' age/length-height/BMI. The list is ordered by acronym first, then by sex.
#'
#' @references
#' de Onis M, Garza C, Victora CG, Onyango AW, Frongillo EA, Martines J. **The WHO Multicentre Growth Reference
#' Study: planning, study design, and methodology** *Food Nutr Bull.* 2004, **25(1 Suppl):S15-26.**
#' doi: [10.1177/15648265040251s104](https://journals.sagepub.com/doi/10.1177/15648265040251S104)
#'
#' @source
#' [WHO Child Growth Standards](https://www.who.int/tools/child-growth-standards/standards)
#'
#' @examples
#' names(gigs::who_gs_coeffs)
#' head(gigs::who_gs_coeffs$lhfa$male)
#' @docType data
#' @keywords data
NULL