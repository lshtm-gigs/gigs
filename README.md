
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gigs <img src="man/figures/logo.png" align="right" height="138" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/simpar1471/gigs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/simpar1471/gigs/actions/workflows/R-CMD-check.yaml)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![test-coverage](https://github.com/simpar1471/gigs/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/simpar1471/gigs/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

## Overview

gigs provides a single, simple interface for working with the WHO Child
Growth Standards and outputs from the INTERGROWTH-21<sup>st</sup>
project. You will find functions for converting between anthropometric
measures (e.g. weight or length) to z-scores and percentiles, and the
inverse. Also included are functions for classifying newborn growth
according to DHS guidelines.

## Installation

``` r
# Install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("lshtm_mnhg/gigs")
```

## Available standards

- `ig_nbs` - INTERGROWTH-21<sup>st</sup> standards for newborn size
  (including very preterm)
  <details>
  <summary>
  Component standards
  </summary>

  - `wfga` - Weight (kg) for gestational age
  - `lfga` - Length (cm) for gestational age
  - `hcfga` - Head circumference (cm) for gestational age
  - `wlrfga` - Weight-to-length ratio for gestational age
  - `fmfga` - Fat mass (g) for gestational age
  - `bfpfga` - Body fat percentage for gestational age
  - `ffmfga` - Fat-free mass (g) for gestational age

  </details>
- `ig_png` - INTERGROWTH-21<sup>st</sup> standards for post-natal growth
  in preterm infants
  <details>
  <summary>
  Component standards
  </summary>

  - `wfa` - Weight (kg) for age (weeks)
  - `lfa` - Length (cm) for age (weeks)
  - `hcfa` - Head circumference (cm) for age (weeks)

  </details>
- `who_gs` - WHO Child Growth Standards for term infants
  <details>
  <summary>
  Component standards
  </summary>

  - `wfa` Weight (kg) for age (days)
  - `bfa` Body mass index for age (days)
  - `lhfa` Length/height (cm) for age (days)
  - `wfl` Weight (kg) for recumbent length (cm)
  - `wfh` Weight (kg) for standing height (cm)
  - `hcfa` Head circumference (mm) for age (days)
  - `acfa` Arm circumference (mm) for age (days)
  - `ssfa` Subscapular skinfold (mm) for age (days)
  - `tsfa` Triceps skinfold (mm) for age (days)

  </details>

## Conversion functions

n.b. functions are named according to the main standard in use, the
component standard, then the type of conversion, e.g.:
`ig_nbs`/`_wfga`/`_zscore2percentile()`

### Z-scores/percentiles to values

``` r
# Convert from z-scores for individual values...
ig_nbs_zscore2value(z = 0, gest_age = 26, sex = "F", acronym = "wfga") |>
        round(digits = 3)
#> [1] 0.785

# .. or for multiple inputs
ig_nbs_wfga_zscore2value(z = 0, gest_age = 26:29, sex = "F") |>
        round(digits = 3)
#> [1] 0.785 0.893 1.013 1.147

# You can do the same for percentiles
ig_png_wfa_percentile2value(p = c(0.1, 0.25, 0.5, 0.75, 0.9),
                            pma_weeks = 40,
                            sex = "M") |>
        round(digits = 2)
#> [1] 2.87 3.12 3.43 3.77 4.11
```

### Values to z-scores/percentiles

These functions allow easy from measured values to z-scores and
percentiles for the standard used.

``` r
# Convert from z-scores for individual values...
ig_nbs_value2zscore(y = 0.785, gest_age = 26, sex = "F", acronym = "wfga") |>
        round(digits = 2)
#> [1] 0

# .. or for multiple inputs
ig_nbs_wfga_value2percentile(weight_kg = 0.785, gest_age = 25:28, sex = "F") |>
        round(digits = 2)
#> [1] 0.75 0.50 0.25 0.09

# You can do the same for percentiles
ig_png_wfa_value2percentile(weight_kg = c(2.86, 3.12, 3.12, 3.43, 3.77, 4.10),
                            pma_weeks = 40,
                            sex = "M") |>
        round(digits = 2)
#> [1] 0.10 0.25 0.25 0.50 0.75 0.90
```

## Classification functions

These functions apply DHS guidelines to z-scores/percentiles generated
by the conversion functions. This allows for quick identification of
at-risk infants through classification of suboptimal growth.

``` r
# Size for gestational age
classify_sga(
        y = c(32.6, 34.2, 36.0),
        gest_age = 39 + 4/7,
        sex = "M",
        acronym = "hcfga"
)
#> [1] SGA AGA LGA
#> Levels: AGA LGA SGA

# Stunting, i.e. low length increase relative to age
classify_stunting(
        lenht_cm = c(42.3, 75.4, 72.83),
        age_days = c(252, 525, 245),
        ga_at_birth = c(34, 37, 38),
        sex = "M",
        lenht_method = "H"
)
#> [1] normal   stunting normal  
#> Levels: normal stunting

# Wasting, i.e. low weight increase relative to length/height
classify_wasting(
        weight_kg = c(5.75, 2.18, 5.30, 6.75),
        lenht_cm = c(67.7, 46.6, 55.8, 80.1),
        sex = c("F", "M", "F", "M"),
        lenht_method = c("H", "L", "L", "H")
)
#> [1] wasting_severe wasting        normal         implausible   
#> Levels: implausible normal wasting wasting_severe

# Weight-for-age, i.e. low weight increase relative to age
classify_wfa(
        weight_kg = c(2.1, 7.2, 6.1, 9.1, 9.4),
        age_days = c(435, 501, 323, 201, 154),
        ga_at_birth = c(36, 27, 37, 40, 28),
        sex = c("M", "M", "F", "F", "F")
)
#> [1] implausible        underweight_severe underweight        normal            
#> [5] overweight        
#> Levels: implausible normal overweight underweight underweight_severe
```
