
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gigs <img src="man/figures/logo.png" align="right" height="138" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/lshtm-gigs/gigs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lshtm-gigs/gigs/actions/workflows/R-CMD-check.yaml)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN
version](https://www.r-pkg.org/badges/version/gigs)](https://www.r-pkg.org/pkg/gigs)
[![pkgcheck](https://github.com/lshtm-gigs/gigs/workflows/pkgcheck/badge.svg)](https://github.com/lshtm-gigs/gigs/actions?query=workflow%3Apkgcheck)
[![test-coverage](https://github.com/lshtm-gigs/gigs/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/lshtm-gigs/gigs/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/github/lshtm-gigs/gigs/graph/badge.svg?token=G5BIYGV5JL)](https://codecov.io/github/lshtm-gigs/gigs)
<!-- badges: end -->

## Overview

Produced as part of the Guidance for International Growth Standards
project, **gigs** provides a single, simple interface for working with
the WHO Child Growth standards and outputs from the
INTERGROWTH-21<sup>st</sup> project. You will find functions for
converting from anthropometric measures (e.g. weight or length) to
z-scores and centiles, and the inverse. Also included are functions for
classifying newborn and infant growth according to literature-based
cut-offs.

**gigs** is be of use to anyone interested in fetal and child growth,
including child health researchers, policymakers, and clinicians. This
package is best suited to growth data where the gestational age of each
child is known, as the use of the growth standards included in **gigs**
is gestational age-dependent. We recommend you check out the [available
standards](#available-standards) section to see if your anthropometric
measurements can be converted to z-scores/centiles by **gigs**. We
recommend using **gigs** to generate continuous or categorical measures
of fetal/newborn/child growth, which can then be used in downstream
analyses.

## Installation

``` r
# You can install the development version from GitHub with `remotes`:
# install.packages("remotes")
remotes::install_github(repo = "lshtm-gigs/gigs")
```

## Available standards

- `ig_nbs` - INTERGROWTH-21<sup>st</sup> Newborn Size standards
  (including very preterm)
  <details>
  <summary>
  Component standards
  </summary>

  | Acronym  | Description                                | Unit  | `gest_days` range |
  |----------|--------------------------------------------|-------|-------------------|
  | `wfga`   | weight-or-gestational age                  | kg    | 168 to 300 days   |
  | `lfga`   | length-for-gestational age                 | cm    | 168 to 300 days   |
  | `hcfga`  | head circumference-for-gestational age     | cm    | 168 to 300 days   |
  | `wlrfga` | weight-to-length ratio-for-gestational age | kg/cm | 168 to 300 days   |
  | `ffmfga` | fat-free mass-for-gestational age          | kg    | 266 to 294 days   |
  | `bfpfga` | body fat percentage-for-gestational age    | %     | 266 to 294 days   |
  | `fmfga`  | fat mass-for-gestational age               | kg    | 266 to 294 days   |

  </details>
- `ig_png` - INTERGROWTH-21<sup>st</sup> Postnatal Growth of Preterm
  Infants standards
  <details>
  <summary>
  Component standards
  </summary>

  | Acronym | Description                | Unit | `x` range              |
  |---------|----------------------------|------|------------------------|
  | `wfa`   | weight-for-age             | kg   | 27 to \<64 exact weeks |
  | `lfa`   | length-for-age             | cm   | 27 to \<64 exact weeks |
  | `hcfa`  | head circumference-for-age | cm   | 27 to \<64 exact weeks |
  | `wfl`   | weight-for-length          | kg   | 35 to 65 cm            |

  </details>
- `ig_fet` - INTERGROWTH-21<sup>st</sup> Fetal standards
  <details>
  <summary>
  Component standards
  </summary>

  | Acronym  | Description                                                               | Unit | `x` range       |
  |----------|---------------------------------------------------------------------------|------|-----------------|
  | `hcfga`  | head circumference-for-gestational age                                    | mm   | 98 to 280 days  |
  | `bpdfga` | biparietal diameter-for-gestational age                                   | mm   | 98 to 280 days  |
  | `acfga`  | abdominal circumference-for-gestational age                               | mm   | 98 to 280 days  |
  | `flfga`  | femur length-for-gestational age                                          | mm   | 98 to 280 days  |
  | `ofdfga` | occipito-frontal diameter for-gestational age                             | mm   | 98 to 280 days  |
  | `efwfga` | estimated fetal weight-for-gestational age                                | g    | 154 to 280 days |
  | `sfhfga` | symphisis-fundal height-for-gestational age                               | mm   | 112 to 294 days |
  | `crlfga` | crown-rump length-for-gestational age                                     | mm   | 58 to 105 days  |
  | `gafcrl` | gestational age-for-crown-rump length                                     | days | 15 to 95 mm     |
  | `gwgfga` | gestational weight gain-for-gestational age                               | kg   | 98 to 280 days  |
  | `pifga`  | pulsatility index-for-gestational age                                     |      | 168 to 280 days |
  | `rifga`  | resistance index-for-gestational age                                      |      | 168 to 280 days |
  | `sdrfga` | systolic/diastolic ratio-for-gestational age                              |      | 168 to 280 days |
  | `tcdfga` | transcerebellar diameter-for-gestational age                              | mm   | 98 to 280 days  |
  | `tcdfga` | gestational age-for-transcerebellar diameter                              | mm   | 98 to 280 days  |
  | `poffga` | parietal-occipital fissure-for-gestational age                            | mm   | 105 to 252 days |
  | `sffga`  | Sylvian fissue-for-gestational age                                        | mm   | 105 to 252 days |
  | `avfga`  | anterior horn of the lateral ventricle-for-gestational age                | mm   | 105 to 252 days |
  | `pvfga`  | atrium of the posterior horn of the lateral ventricle-for-gestational age | mm   | 105 to 252 days |
  | `cmfga`  | cisterna magna-for-gestational age                                        | mm   | 105 to 252 days |

  </details>
- `who_gs` - WHO Child Growth Standards for term infants
  <details>
  <summary>
  Component standards
  </summary>

  | Acronym | Description                  | Unit             | `x` range       |
  |---------|------------------------------|------------------|-----------------|
  | `wfa`   | weight-for-age               | kg               | 0 to 1856 days  |
  | `bfa`   | BMI-for-age                  | kg/m<sup>2</sup> | 0 to 1856 days  |
  | `lhfa`  | length/height-for-age        | cm               | 0 to 1856 days  |
  | `hcfa`  | head circumference-for-age   | cm               | 0 to 1856 days  |
  | `wfl`   | weight-for-height            | kg               | 45 to 110 cm    |
  | `wfh`   | weight-for-length            | kg               | 65 to 120 cm    |
  | `acfa`  | arm circumference-for-age    | cm               | 91 to 1856 days |
  | `ssfa`  | subscapular skinfold-for-age | mm               | 91 to 1856 days |
  | `tsfa`  | triceps skinfold-for-age     | mm               | 91 to 1856 days |

  </details>

## Conversion functions

### Terminology

The package operates with on measured anthropometric values, and can
convert between these and *z-scores*/*centiles*. Z-scores and centiles
represent the location of a measurement within a normal distribution of
values, such that:

- A *z-score* is the number of standard deviations from the mean for a
  given anthropometric measurement (e.g. height or weight).
- A *centile* represents the proportion of measurements in some
  distribution which we would expect to be lower than a measurement
  we’ve taken, represented as a value between 0 and 1.

### Usage

Conversion functions are named according to the set of standards in use,
the component standard from that set, then the type of conversion. For
example, to convert *values to z-scores* in the *weight-for-GA* standard
from the *INTERGROWTH-21<sup>st</sup> Newborn Size Standards* would be:
`ig_nbs`/`_wfga`/`_value2zscore()`

Similarly, the conversion of length-for-age values to centiles in term
and preterm infants could be performed with the WHO Child Growth
standards and INTERGROWTH-21<sup>st</sup> Postnatal Growth of Preterm
Infants standards, respectively:

- Term infants: `who_gs`/`_lhfa`/`_value2zscore()`
- Preterm infants: `ig_png`/`_lfa`/`_value2centile()`

If the component standard is not included in the function call, it
should be passed to the `acronym` parameter of the general function
call. For example, these two function calls would behave in the same
way:

``` r
ig_nbs_value2zscore(y = 25.7, gest_days = 182, sex = "F", acronym = "hcfga") |>
  round(digits = 2)
#> [1] 1.18

ig_nbs_hcfga_value2zscore(headcirc_cm = 25.7, gest_days = 182, sex = "F") |>
  round(digits = 2)
#> [1] 1.18
```

A vector of `acronym` values can be used if you want to convert with
different standards for each input.

### Values to z-scores/centiles

These functions allow easy conversion from measured values to z-scores
or centiles for the standard used.

``` r
# Convert from z-scores for individual values...
ig_nbs_value2zscore(y = 0.785, gest_days = 182, sex = "F", acronym = "wfga") |>
  round(digits = 2)
#> [1] 0

# .. or for multiple inputs
ig_nbs_wfga_value2centile(weight_kg = 0.785,
                          gest_days = seq(175, 196, by = 7),
                          sex = "F") |>
  round(digits = 2)
#> [1] 0.75 0.50 0.25 0.09

# You can do the same for centiles
ig_png_wfa_value2centile(weight_kg = c(2.86, 3.12, 3.12, 3.43, 3.77, 4.10),
                         pma_weeks = 40,
                         sex = "M") |>
  round(digits = 2)
#> [1] 0.10 0.25 0.25 0.50 0.75 0.90
```

### Z-scores/centiles to values

These functions convert z-scores to expected anthropometric
measurements. They are mostly useful for the creation of reference
curves (see below).

``` r
# Convert from z-scores for individual values...
ig_nbs_zscore2value(z = 0, gest_days = 182, sex = "F", acronym = "wfga") |>
  round(digits = 3)
#> [1] 0.785

# .. or for multiple inputs
ig_nbs_wfga_zscore2value(z = 0, gest_days = seq(182, 204, by = 7), sex = "F") |>
  round(digits = 3)
#> [1] 0.785 0.893 1.013 1.147

# You can do the same for centiles
ig_png_wfa_centile2value(p = c(0.1, 0.25, 0.5, 0.75, 0.9),
                            pma_weeks = 40,
                            sex = "M") |>
  round(digits = 2)
#> [1] 2.87 3.12 3.43 3.77 4.11
```

#### Reference curves

We can use gigs to generate reference curves for the standards by
getting curves for the expected weight at multiple z-scores across
multiple gestational ages. We would usually recommend
[`ggplot2`](https://ggplot2.tidyverse.org/) for such visualisation, but
do not use it here to reduce our package’s dependencies.

``` r
z_score_range <- -2:2
gestage_range <- 168:230
ref <- mapply(z_score_range,
               FUN = function(z) {
                 gigs::ig_nbs_wfga_zscore2value(z = z,
                                                gest_days = gestage_range,
                                                sex = "F")
               })
matplot(ref, x = gestage_range, col = 1:5, type = "l", lty = 2:6,
        xlab = "Gestational age (days)",
        ylab = "Weight (kg)")
title(main = "Weight-for-GA in very preterm newborns")
legend(x = min(gestage_range) + 1, y = ref[length(ref)], legend = 2:-2,
       title = "Z-score", col = 5:1, lty = 2:6)
```

<img src="man/figures/README-example_zp2v_curves-1.png" width="100%" />

## Classification functions

These functions allow for quick identification of at-risk infants
through classification of suboptimal growth. The cut-offs used are
sourced from research literature; you can check the function
documentation to see these sources.

``` r
# Size for gestational age
classify_sfga(
  weight_kg = c(2.1, 3.2, 4.5),
  gest_days = c(252, 259, 290),
  sex = "M"
)
#> [1] SGA AGA LGA
#> Levels: SGA AGA LGA

# Small vulnerable newborns
classify_svn(
  weight_kg = c(2.1, 3.2, 4.5),
  gest_days = c(252, 259, 290),
  sex = "M"
)
#> [1] Preterm SGA Term AGA    Term LGA   
#> Levels: Preterm SGA Preterm AGA Preterm LGA Term SGA Term AGA Term LGA

# Stunting, i.e. low length increase relative to age
classify_stunting(
  lenht_cm = c(42.3, 75.4, 72.83),
  age_days = c(252, 525, 245),
  gest_days = c(238, 259, 266),
  sex = "M"
)
#> [1] stunting_severe stunting        not_stunting   
#> Levels: stunting_severe stunting not_stunting

# Wasting, i.e. low weight increase relative to length/height
classify_wasting(
  weight_kg = c(5.75, 2.18, 5.30, 6.75),
  lenht_cm = c(67.7, 46.6, 55.8, 80.1),
  gest_days = c(268, 247, 250, 278),
  age_days = c(45, 33, 230, 278),
  sex = c("F", "M", "F", "M"),
)
#> [1] wasting_severe wasting        not_wasting    wasting_severe
#> Levels: wasting_severe wasting not_wasting overweight

# Weight-for-age, i.e. low weight increase relative to age
classify_wfa(
  weight_kg = c(2.1, 7.2, 6.1, 9.1, 9.4),
  age_days = c(435, 501, 323, 201, 154),
  gest_days = c(36, 27, 37, 40, 28),
  sex = c("M", "M", "F", "F", "F")
)
#> [1] underweight_severe underweight_severe normal_weight      overweight        
#> [5] <NA>              
#> Levels: underweight_severe underweight normal_weight overweight
```

# Other packages

Other R packages can be used to analyse growth data with international
standards, but have limitations which are not present in gigs. There are
also software packages external to R which implement these standards.
The table below describes these packages, and to what extent they have
implemented functions that let users convert anthropometric measurements
to z-scores/centiles in each set of standards implemented in gigs - the
WHO Child Growth standards, INTERGROWTH-21<sup>st</sup> Newborn Size
standards (including Very Preterm), and the INTERGROWTH-21<sup>st</sup>
Postnatal Growth standards for preterm infants. A green tick (✅)
indicates that all possible standards are included in a package, a red
cross (❌) indicates that these standards are completely missing, and an
exclamation mark (❕) indicates that some of these standards are
implemented but not others.

| Software                                                                     | Platform | WHO (0-5 years) | IG-21<sup>st</sup> NBS | IG-21<sup>st</sup> PNG | IG-21<sup>st</sup> Fetal | Functionality              |
|------------------------------------------------------------------------------|----------|-----------------|------------------------|------------------------|--------------------------|----------------------------|
| [gigs](https://www.github.com/lshtm-gigs/gigs/)                              | R        | ✅              | ✅                     | ✅                     | ✅                       | Values ↔ z-scores/centiles |
| [anthro](https://cran.r-project.org/web/packages/anthro/index.html)          | R        | ✅              | ❌                     | ❌                     | ❌                       | Values → z-scores          |
| [childsds](https://cran.r-project.org/web/packages/childsds/index.html)      | R        | ✅              | ❌                     | ❌                     | ❌                       | Values → z-scores/centiles |
| [ki-tools/growthstandards](https://www.github.com/ki-tools/growthstandards/) | R        | ✅              | ✅                     | ❌                     | ❕                       | Values ↔ z-scores/centiles |
| [nutriverse/intergrowth](https://github.com/nutriverse/intergrowth/)         | R        | ❌              | ❌                     | ❌                     | ❕                       | NA                         |
| [gigs](https://www.github.com/lshtm-gigs/gigs-stata/)                        | Stata    | ✅              | ✅                     | ✅                     | ✅                       | Values ↔ z-scores/centiles |
| [zanthro](https://journals.sagepub.com/doi/epdf/10.1177/1536867X1301300211)  | Stata    | ✅              | ❌                     | ❌                     | ❌                       | Values → z-scores/centiles |

We have benchmarked some of these implementations against each other for
conversion of values to z-scores in the WHO Child Growth standards. At
100,000 inputs, each package took:

| Package                                                                      | Time taken (100,000 inputs) |
|------------------------------------------------------------------------------|-----------------------------|
| [ki-tools/growthstandards](https://www.github.com/ki-tools/growthstandards/) | 125 ms                      |
| [childsds](https://cran.r-project.org/web/packages/childsds/index.html)      | 126 ms                      |
| [gigs](https://www.github.com/lshtm-gigs/gigs/)                              | 128 ms                      |
| [gigs](https://www.github.com/lshtm-gigs/gigs-stata/)                        | 0.41 seconds                |
| [zanthro](https://journals.sagepub.com/doi/epdf/10.1177/1536867X1301300211)  | 2.05 seconds                |
| [anthro](https://cran.r-project.org/web/packages/anthro/index.html)          | 2.24 seconds                |
| [nutriverse/intergrowth](https://github.com/nutriverse/intergrowth/)         | NA (no WHO functions)       |

The results can be seen in the online benchmarking
[article](https://lshtm-gigs.github.io/gigs/articles/benchmarking.html).

The WHO and INTERGROWTH-21<sup>st</sup> standards are also available in
standalone form, available from the [WHO
website](https://www.who.int/tools/child-growth-standards/software) and
[INTERGROWTH-21<sup>st</sup>
website](https://intergrowth21.tghn.org/intergrowth-21st-applications/),
respectively. The INTERGROWTH-21<sup>st</sup> website also includes
download links for Excel-based calculators in some standards.

## Citation

Parker SR, Vesel L, Ohuma EO (2023). *gigs: Assess Fetal, Newborn, and Child Growth with International Standards*.
<https://github.com/lshtm-gigs/gigs/>.

## Code of Conduct

Please note that the gigs project is released with a [Contributor Code
of Conduct](https://ropensci.org/code-of-conduct/). By contributing to
this project you agree to abide by its terms.
