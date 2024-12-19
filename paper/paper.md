---
title: |
  'gigs: A package for standardising fetal, neonatal, and child growth assessment'
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

There is a gap for clear guidance and open access tools for assessing nutrition 
and growth indicators as part of individual clinical care and population-based 
epidemiology. To enable better identification of at-risk infants, a unifying 
framework and clear guidance for how, when and for whom to use the existing 
international newborn and child growth standards developed by the WHO and 
INTERGROWTH-21^st^ is needed. Such guidance is essential to enable tracking of 
progress towards Sustainable Development Goals and the WHO Global 
Nutrition targets. This need is the motivation for ``gigs`` - a package for 
researchers and policymakers that facilitates the appropriate use of growth 
standards for the determination and assessment of growth outcomes at the 
population level (e.g., stunting, wasting, underweight and trajectories over 
time) among vulnerable infants. For clinicians, the ``gigs`` also provides 
support in choosing which standards to use when assessing growth of individual 
infants.

# Statement of need

Without accurate global data on newborn and child growth patterns using 
appropriate tools, true gaps and achievements in newborn and child health and 
survival will not be revealed. Those most vulnerable to poor growth outcomes 
will not be properly identified, tracking progress towards global targets (e.g. the 
Sustainable Development Goals) will be more difficult, and evaluating what 
changes are 'true progress' will be even more challenging. The ``gigs`` package 
allows researchers, policy-makers, and other key stakeholders to apply and 
correctly identify at-risk, vulnerable infants at a population level. This can 
then inform appropriate interventions to make progress towards global, national 
and local goals. Similarly, use of appropriate standards by clinicians will 
enable proper identification of at-risk infants and inform the clinical care 
management and decision-making.

Applying appropriate growth standards can be difficult - not all research groups
have the ability to implement growth standards in software, and may make errors 
whilst doing so. Furthermore, researchers must pick and choose the right growth 
standards for their work - which is not always achieved in practice 
[@perumal_who_2015]. The ``gigs`` package makes these issues less common. 
Firstly, extensive unit testing ensures that each growth standard in ``gigs`` is
accurate to published charts, for every growth standard we've implemented:

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

Next, ``gigs`` offers specific functions (``classify_growth()``, ``gigs_waz()``,
and friends) which select an appropriate growth standard to apply to each 
observation in flat, tabular dataset. Growth standards are selected based on the
chronological and gestational age for each observation, and the specific 
criteria can be found in the [package 
documentation](https://docs.ropensci.org/gigs/reference/gigs_zscoring.html). 
These functions can be used to investigate growth indicators including 
size-for-gestational age, small vulnerable newborns 
[@Ashorn2023SmallImpact; @Lawn2023SmallCounting], stunting, wasting, 
weight-for-age (underweight), and micro/macrocephaly.

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
researchers at LSHTM, Harvard, the [Small Vulnerable Newborn 
Collaboration](https://www.thelancet.com/series/small-vulnerable-newborns) and 
an international collaboration investigating stillbirths. Thanks to its speed, 
input checking, and unit test-backed accuracy, we believe ``gigs`` will be used 
widely by those looking to assess newborn, infant, or child growth in R.

# Acknowledgements

The development of this package was supported by the Bill & Melinda Gates 
Foundation (BMGF) under grant number 046290. BMGF had no specific input on the 
content of the package. We also want to thank our rOpenSci reviewers - Victor 
Ordu and Chitra Saraswati - and handling editor Rebecca Killick, whose feedback 
and guidance through the rOpenSci software review process has been extremely 
useful.

# References