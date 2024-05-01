
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
project at the London School of Hygiene & Tropical Medicine, **gigs**
provides a single, simple interface for working with the WHO Child
Growth standards and outputs from the INTERGROWTH-21<sup>st</sup>
project. You will find functions for converting from anthropometric
measures (e.g. weight or length) to z-scores and centiles, and the
inverse. Also included are functions for classifying newborn and infant
growth according to literature-based cut-offs.

**gigs** is of use to anyone interested in fetal and child growth,
including child health researchers, policymakers, and clinicians. This
package is best suited to growth data where the gestational age (GA) of
each child is known, as the use of the growth standards included in
**gigs** is GA-dependent. We recommend you check out the [available
standards](#available-international-growth-standards) section to see if
your anthropometric measurements can be converted to z-scores/centiles
by **gigs**. We recommend using **gigs** to generate continuous or
categorical measures of fetal/newborn/child growth, which can then be
used in downstream analyses.

## Installation

``` r
# You can install the development version from GitHub with `remotes`:
# install.packages("remotes")
remotes::install_github(repo = "lshtm-gigs/gigs")
```

## Terminology

GIGS operates on anthropometric measurements, and can convert between
these and *z-scores*/*centiles*. Z-scores and centiles represent the
location of a measurement within a normal distribution of values, such
that:

- A *z-score* is the number of standard deviations from the mean for a
  given anthropometric measurement (e.g. height or weight).
- A *centile* represents the proportion of measurements in some
  distribution which we would expect to be lower than a measurement
  we’ve taken. In *gigs*, these are represented as a value between `0`
  and `1`. For example, `0.5` corresponds to the 50<sup>th</sup> centile
  (i.e. the mean), whereas `0.75` corresponds to the 75<sup>th</sup>
  centile.

In growth data, z-scores and centiles represent the size a fetus,
newborn, or child relative to its peers. Its size is considered relative
to some standardising variable, which is usually age but could also be
another variable such as their length. By tracking a child’s relative
size as they grow, you can see if they are achieving their growth
potential or not. If not, this may indicate underlying issues such as
ill health or undernutrition.

## Classification functions

GIGS includes a number of functions which permit fast identification of
at-risk infants through classification of suboptimal growth. The
cut-offs used are sourced from research literature; you can check the
function documentation to see these sources.

### Growth classification in `data.frame`-like objects

Use the `classify_growth()` function to quickly compute growth
indicators in `data.frame`-like objects. All `classify_*()`-style
functions in GIGS use
[data-masking](https://rlang.r-lib.org/reference/args_data_masking.html),
so you provide a `data.frame`-like object in the `.data` argument and
then refer to your column names directly. In `classify_growth()`, you
can also use the `.analyses` argument to specify which growth indicators
you want to classify.

``` r
life6mo_newborns <- gigs::life6mo[life6mo$age_days == 0, ]

# Use classify_growth() to get multiple growth indicators at once
life6mo_classified <- classify_growth(
  .data = life6mo_newborns,
  weight_kg = weight_g / 1000,
  lenht_cm = len_cm,
  gest_days = gestage,
  age_days = age_days,
  sex = as.character(sex),
  .analyses = c("svn", "stunting")
)
#> `gigs::classify_growth()`
#> ✔ Small vulnerable newborns: Success
#> ✔ Stunting: Success

head(life6mo_classified, n = 4)
#>    id gestage sex visitweek pma age_days weight_g   len_cm headcirc_cm  muac_cm
#> 1   1     273   M         0 273        0     2300 42.06667    33.26667 9.433333
#> 28  4     250   F         0 250        0     1500 42.03333    30.03333 8.066667
#> 36  5     238   F         0 238        0     2390 43.46667    33.63333 9.166667
#> 56  8     240   F         0 240        0     1800 41.73333    31.46667 8.033334
#>    birthweight_centile         svn       lhaz        stunting stunting_outliers
#> 1          0.010765424    Term SGA -3.5406446 stunting_severe   stunting_severe
#> 28         0.002833163 Preterm SGA -2.2854751        stunting          stunting
#> 36         0.756367868 Preterm AGA -0.6087434    not_stunting      not_stunting
#> 56         0.126440075 Preterm AGA -1.6989568    not_stunting      not_stunting
```

When using `classify_growth()`, you will be informed which of the
analyses you wanted to run were successful. In the example below,
because `lenht_cm` is not specified, stunting indicators cannot be
computed.

``` r
life6mo_classified <- classify_growth(
  .data = life6mo_newborns,
  weight_kg = weight_g / 1000,
  gest_days = gestage,
  age_days = age_days,
  sex = as.character(sex),
  .analyses = c("svn", "stunting")
)
#> `gigs::classify_growth()`
#> ✔ Small vulnerable newborns: Success
#> ✔ Stunting: Success

head(life6mo_classified, n = 4)
#>    id gestage sex visitweek pma age_days weight_g   len_cm headcirc_cm  muac_cm
#> 1   1     273   M         0 273        0     2300 42.06667    33.26667 9.433333
#> 28  4     250   F         0 250        0     1500 42.03333    30.03333 8.066667
#> 36  5     238   F         0 238        0     2390 43.46667    33.63333 9.166667
#> 56  8     240   F         0 240        0     1800 41.73333    31.46667 8.033334
#>    birthweight_centile         svn
#> 1          0.010765424    Term SGA
#> 28         0.002833163 Preterm SGA
#> 36         0.756367868 Preterm AGA
#> 56         0.126440075 Preterm AGA
```

You can also use `classify_*()` functions which are specific to the
growth indicator you’d like to calculate, for example `classify_svn()`:

``` r
# Small vulnerable newborns
life6mo_svn <- classify_svn(
  .data = life6mo_newborns,
  weight_kg = weight_g / 1000,
  gest_days = gestage,
  sex = as.character(sex)
)

head(life6mo_svn, n = 4)
#>    id gestage sex visitweek pma age_days weight_g   len_cm headcirc_cm  muac_cm
#> 1   1     273   M         0 273        0     2300 42.06667    33.26667 9.433333
#> 28  4     250   F         0 250        0     1500 42.03333    30.03333 8.066667
#> 36  5     238   F         0 238        0     2390 43.46667    33.63333 9.166667
#> 56  8     240   F         0 240        0     1800 41.73333    31.46667 8.033334
#>    birthweight_centile         svn
#> 1          0.010765424    Term SGA
#> 28         0.002833163 Preterm SGA
#> 36         0.756367868 Preterm AGA
#> 56         0.126440075 Preterm AGA
```

## Conversion functions

### Available international growth standards

GIGS facilitates the proper use of international growth standards, which
are growth charts developed using international samples of healthy
singleton children born to mothers that had their health needs met
during pregnancy. They represent an international standard of ‘optimal’
growth. GIGS implements international growth standards from the WHO and
INTERGROWTH-21<sup>st</sup> project:

- `ig_nbs` - INTERGROWTH-21<sup>st</sup> Newborn Size standards
  (including very preterm)
  <details>
  <summary>
  Component standards
  </summary>

  | Acronym  | Description                   | Unit  | `gest_days` range |
  |----------|-------------------------------|-------|-------------------|
  | `wfga`   | weight-for-GA                 | kg    | 168 to 300 days   |
  | `lfga`   | length-for-GA                 | cm    | 168 to 300 days   |
  | `hcfga`  | head circumference-for-GA     | cm    | 168 to 300 days   |
  | `wlrfga` | weight-to-length ratio-for-GA | kg/cm | 168 to 300 days   |
  | `ffmfga` | fat-free mass-for-GA          | kg    | 266 to 294 days   |
  | `bfpfga` | body fat percentage-for-GA    | %     | 266 to 294 days   |
  | `fmfga`  | fat mass-for-GA               | kg    | 266 to 294 days   |

  </details>
- `ig_png` - INTERGROWTH-21<sup>st</sup> Postnatal Growth of Preterm
  Infants standards
  <details>
  <summary>
  Component standards
  </summary>

  | Acronym | Description                | Unit | `x` range             |
  |---------|----------------------------|------|-----------------------|
  | `wfa`   | weight-for-age             | kg   | 27 to ≤64 exact weeks |
  | `lfa`   | length-for-age             | cm   | 27 to ≤64 exact weeks |
  | `hcfa`  | head circumference-for-age | cm   | 27 to ≤64 exact weeks |
  | `wfl`   | weight-for-length          | kg   | 35 to 65 cm           |

  </details>
- `ig_fet` - INTERGROWTH-21<sup>st</sup> Fetal standards
  <details>
  <summary>
  Component standards
  </summary>

  | Acronym  | Description                                                  | Unit | `x` range       |
  |----------|--------------------------------------------------------------|------|-----------------|
  | `hcfga`  | head circumference-for-GA                                    | mm   | 98 to 280 days  |
  | `bpdfga` | biparietal diameter-for-GA                                   | mm   | 98 to 280 days  |
  | `acfga`  | abdominal circumference-for-GA                               | mm   | 98 to 280 days  |
  | `flfga`  | femur length-for-GA                                          | mm   | 98 to 280 days  |
  | `ofdfga` | occipito-frontal diameter for-GA                             | mm   | 98 to 280 days  |
  | `efwfga` | estimated fetal weight-for-GA                                | g    | 154 to 280 days |
  | `sfhfga` | symphisis-fundal height-for-GA                               | mm   | 112 to 294 days |
  | `crlfga` | crown-rump length-for-GA                                     | mm   | 58 to 105 days  |
  | `gafcrl` | GA-for-crown-rump length                                     | days | 15 to 95 mm     |
  | `gwgfga` | gestational weight gain-for-GA                               | kg   | 98 to 280 days  |
  | `pifga`  | pulsatility index-for-GA                                     |      | 168 to 280 days |
  | `rifga`  | resistance index-for-GA                                      |      | 168 to 280 days |
  | `sdrfga` | systolic/diastolic ratio-for-GA                              |      | 168 to 280 days |
  | `tcdfga` | transcerebellar diameter-for-GA                              | mm   | 98 to 280 days  |
  | `tcdfga` | GA-for-transcerebellar diameter                              | mm   | 98 to 280 days  |
  | `poffga` | parietal-occipital fissure-for-GA                            | mm   | 105 to 252 days |
  | `sffga`  | Sylvian fissue-for-GA                                        | mm   | 105 to 252 days |
  | `avfga`  | anterior horn of the lateral ventricle-for-GA                | mm   | 105 to 252 days |
  | `pvfga`  | atrium of the posterior horn of the lateral ventricle-for-GA | mm   | 105 to 252 days |
  | `cmfga`  | cisterna magna-for-GA                                        | mm   | 105 to 252 days |
  | `efwfga` | Hadlock estimated fetal weight-for-GA                        | g    | 126 to 287 days |

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

| Software                                                                            | Platform | WHO (0-5 years) | IG-21<sup>st</sup> NBS | IG-21<sup>st</sup> PNG | IG-21<sup>st</sup> Fetal | Functionality              |
|-------------------------------------------------------------------------------------|----------|-----------------|------------------------|------------------------|--------------------------|----------------------------|
| [gigs](https://www.github.com/lshtm-gigs/gigs/)                                     | R        | ✅              | ✅                     | ✅                     | ✅                       | Values ↔ z-scores/centiles |
| [anthro](https://cran.r-project.org/web/packages/anthro/index.html)                 | R        | ✅              | ❌                     | ❌                     | ❌                       | Values → z-scores          |
| [childsds](https://cran.r-project.org/web/packages/childsds/index.html)             | R        | ✅              | ❌                     | ❌                     | ❌                       | Values → z-scores/centiles |
| [ki-tools/growthstandards](https://www.github.com/ki-tools/growthstandards/)        | R        | ✅              | ✅                     | ❕                     | ❕                       | Values ↔ z-scores/centiles |
| [nutriverse/intergrowth](https://github.com/nutriverse/intergrowth/)                | R        | ❌              | ❌                     | ❌                     | ❕                       | Values → z-scores/centiles |
| [gigs](https://www.github.com/lshtm-gigs/gigs-stata/) (Stata)                       | Stata    | ✅              | ✅                     | ✅                     | ✅                       | Values ↔ z-scores/centiles |
| [zanthro](https://journals.sagepub.com/doi/epdf/10.1177/1536867X1301300211) (Stata) | Stata    | ✅              | ❌                     | ❌                     | ❌                       | Values → z-scores/centiles |

We have benchmarked some of these implementations against each other for
conversion of values to z-scores in the WHO Child Growth standards. The
table below shows relative speed of each software package to process
100,000 inputs. The code used to generate these timings can be seen
online in the GIGS benchmarking
[article](https://lshtm-gigs.github.io/gigs/articles/benchmarking.html).

| Software                                                                            | Platform | WHO (0-5 years) (ms) | IG-21<sup>st</sup> NBS (ms) | IG-21<sup>st</sup> PNG (ms) | IG-21<sup>st</sup> Fetal (ms) |
|-------------------------------------------------------------------------------------|----------|----------------------|-----------------------------|-----------------------------|-------------------------------|
| [gigs](https://www.github.com/lshtm-gigs/gigs/)                                     | R        | 96                   | 76                          | 20                          | 9                             |
| [anthro](https://cran.r-project.org/web/packages/anthro/index.html)                 | R        | 2132                 | ❌                          | ❌                          | ❌                            |
| [childsds](https://cran.r-project.org/web/packages/childsds/index.html)             | R        | 123                  | ❌                          | ❌                          | ❌                            |
| [ki-tools/growthstandards](https://www.github.com/ki-tools/growthstandards/)        | R        | 89                   | 69                          | 39                          | 10                            |
| [nutriverse/intergrowth](https://github.com/nutriverse/intergrowth/)                | R        | ❌                   | ❌                          | ❌                          | 16                            |
| [gigs](https://www.github.com/lshtm-gigs/gigs-stata/) (Stata)                       | Stata    | 405                  | 471                         | 164                         | 93                            |
| [zanthro](https://journals.sagepub.com/doi/epdf/10.1177/1536867X1301300211) (Stata) | Stata    | 2046                 | ❌                          | ❌                          | ❌                            |

The WHO and INTERGROWTH-21<sup>st</sup> standards are also available in
standalone form, available from the [WHO
website](https://www.who.int/tools/child-growth-standards/software) and
[INTERGROWTH-21<sup>st</sup>
website](https://intergrowth21.tghn.org/intergrowth-21st-applications/),
respectively. The INTERGROWTH-21<sup>st</sup> website also includes
download links for Excel-based calculators in some standards.

## Citation

Parker S (2023). *gigs: Assess Fetal, Newborn, and Child Growth*.
<https://github.com/lshtm-gigs/gigs/>.

## Code of Conduct

Please note that the gigs project is released with a [Contributor Code
of Conduct](https://ropensci.org/code-of-conduct/). By contributing to
this project you agree to abide by its terms.
