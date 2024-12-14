---
title: |
  'gigs: Assess Fetal, Newborn, and Child Growth with International Standards in R'
tags:
  - R
  - intergrowth
  - who child growth standards
  - growth standards
  - growth analysis
authors:
  - name: Simon R. Parker
    orcid: 0000-0003-0872-7098
    affiliation: '1'
  - name: Linda Vesel
    orcid: 0000-0003-3753-4172
    affiliation: '2'
  - name: Eric O. Ohuma
    orcid: 0000-0002-3116-2593
    affiliation: '1'
affiliations:
 - index: 1
   name: Maternal, Adolescent, Reproductive and Child Health Centre, London School of Hygiene & Tropical Medicine, London, UK
   ror: 00a0jsq62
 - index: 2
   name: Ariadne Labs, Brigham and Women’s Hospital, Harvard T.H. Chan School of Public Health, Boston, Massachusetts, U.S.A. 
   ror: 03vek6s52

date: 14 December 2024
bibliography: paper.bib
---

# Summary

Researchers studying newborn and child growth often want to assess how the size 
of infants relative to their peers. To do this, researchers can use a growth 
standard. A growth standard describes how infants *should* grow if provided
with the nutrition and support required to reach their growth potential. 
``gigs`` makes a range of growth standards available in one R package:
the INTERGROWTH-21^st^ Fetal Standards 
[@Papageorghiou2014AInternationalPregnancy; @Stirnemann2017InternationalProject;
@Papageorghiou2016InternationalCountries; 
@Papageorghiou2014BInternationalPregnancy; 
@Drukker2020InternationalProject; @Rodriguez-Sibaja2021FetalProject; 
@Stirnemann2020IntergrowthWeight]; INTERGROWTH-21^st^ Newborn Size 
and Very Preterm Newborn Size Standards [@Villar2014InternationalProject; 
@Villar2016INTERGROWTH-21stCharts; @Villar2017BodyProject]; INTERGROWTH-21^st^ Postnatal Growth
of Preterm Infants Standards [@Villar2015PostnatalProject]; and the WHO Child Growth Standards 
[@WHOMulticentreGrowthReferenceStudyGroup2006WHOAge; @WHO2006WHODevelopment; 
@WHO2007WHODevelopment]. Also included are functions to classify growth for 
indicators including size-for-gestational age, small vulnerable newborns 
[@Ashorn2023SmallImpact; @Lawn2023SmallCounting], stunting, wasting, 
weight-for-age (underweight), and micro/macrocephaly.

# Statement of need

Not all research groups have the ability to implement growth standards in 
software, and may make errors whilst doing so. Furthermore, researchers must 
pick and choose the right growth standards for their work - which is not always 
achieved in practice [@perumal_who_2015]. The ``gigs`` package makes these
issues less common. Firstly, extensive unit testing ensures that each growth 
standard in ``gigs`` is accurate to published charts, for all of the growth 
standards implemented within:

- The INTERGROWTH-21^st^ Fetal Standards 
  [@Papageorghiou2014AInternationalPregnancy; @Stirnemann2017InternationalProject;
@Papageorghiou2016InternationalCountries; 
@Papageorghiou2014BInternationalPregnancy; 
@Drukker2020InternationalProject; @Rodriguez-Sibaja2021FetalProject; 
@Stirnemann2020IntergrowthWeight]
- INTERGROWTH-21^st^ Newborn Size and Very Preterm Newborn Size Standards 
  [@Villar2014InternationalProject; @Villar2016INTERGROWTH-21stCharts; 
  @Villar2017BodyProject]
- INTERGROWTH-21^st^ Postnatal Growth of Preterm Infants Standards 
  [@Villar2015PostnatalProject]
- WHO Child Growth Standards 
  [@WHOMulticentreGrowthReferenceStudyGroup2006WHOAge; @WHO2006WHODevelopment; 
  @WHO2007WHODevelopment]

This is more growth standards than made available in other R packages designed 
to perform the same sorts of analyses. For example, the WHO Child Growth 
Standards are implemented in many other packages: the ``zanthro`` package for 
Stata [@vidmar_standardizing_2013], and the R packages ``anthro`` [@r_anthro], 
``AGD`` [@r_AGD], ``childsds`` [@r_childsds], ``growthstandards`` 
[@r_growthstandards], ``sitar`` [@r_sitar], and ``zscorer`` [@r_zscorer]. 
However, only ``gigs`` implements the full set of INTERGROWTH-21^st^ Fetal 
standards and INTERGROWTH-21^st^ Postnatal Growth of Preterm Infants standards.

When benchmarked against each other, ``gigs`` for R outperforms almost all 
the packages mentioned above, or is at most a few milliseconds slower. It does 
this whilst checking inputs to ensure they are formatted correctly, to make life
easier for ``gigs`` users. For example, it will detect missing (`NA`) or 
infinite (`-Inf`/`Inf`) numeric inputs, and flag these to the user at the 
console. Full benchmarking results are available on the [package documentation 
website](https://docs.ropensci.org/gigs/articles/benchmarking.html).

In addition to the R package, a 
[Stata package](https://www.github.com/lshtm-gigs/gigs-stata/) and 
[SAS package](https://www.github.com/SASPAC/gigs/) are available for users of 
these statistical softwares. All versions of ``gigs`` are already in use by 
researchers at LSHTM, Harvard, and an international collaboration studying 
**... requires elaboration**, led by Eric Ohuma. Thanks to its speed, input 
checking, and unit test-backed accuracy, we believe ``gigs`` will be used widely
by those looking to assess newborn, infant, or child growth in R.

# Acknowledgements

The development of this package was supported by the Bill & Melinda Gates 
Foundation (BMGF) under grant number 046290. BMGF had no specific input on the 
content of the package. We also want to thank our rOpenSci reviewers - Victor 
Ordu and Chitra Saraswati - and handling editor Rebecca Killick, whose feedback 
and guidance through the rOpenSci software review process has been extremely 
useful.

# References