# gigs 0.4.1.9002

-   The new version of the package includes many changes recommended by the good folks at [rOpenSci](https://ropensci.org/), who are reviewing the package.
-   **BREAKING CHANGES**:
    -   Conversion between anthropometric values and z-scores/centiles is now done with `value2zscore()`, `value2centile()`, `zscore2value()`, and `centile2value()`.
        -   The `family` and `acronym` arguments are used to select a specific growth standard.
    -   The `classify_*()` family of functions no longer take vectors as input, but instead take `data.frame`-like objects.
-   **NEW FEATURES**:
    -   Use `classify_growth()` to get data on multiple growth indicators at the same time. It will try to compute as many growth outcomes (i.e. size-for-GA, SVN, stunting, wasting, weight-for-age and head size) as possible based on the data you provide, and will tell you which analyses were performed.
    -   The GIGS z-scoring functions are now explicitly user-facing. Check out `gigs_waz()` and friends for easy z-scoring using growth standards suggested by GIGS.
    -   An `id` parameter for `classify_growth()` and friends, as well as the gigs z-scoring functions. This parameter informs the functions about *who* each data point is from, so that birth measurements for each individual can be identified and assessed appropriately.
    -   The `compute_*()` functions for growth analysis. These functions take several vectors containing anthropometric measurements, age, and sex; they return a factor of growth categories. They are equivalent to the `classify_*()` functions from v0.4.1 and earlier.
    -   The `categorise_*()` functions for growth analysis. These functions take vectors of growth centiles/z-scores as input, and return a factor of growth categories.
    -   Added the INTERGROWTH-21st standard for estimated fetal weight based on the Hadlock formula. The is available through `value2zscore()` and friends provided that `family = "ig_fet"` and `acronym = "hefwfga"`.
    -   Added a new package-level option - `"handle_unused_levels"`. This option matters when you're categorising growth data, but want gigs to drop unused levels in your newly-made factors.
        -   By default, `.gigs_options$handle_unused_levels` is `"keep_warn"`. So gigs will tell you that there are unused factor levels, but not drop them. Change it to `"drop_warn"` or `"drop_silent"`to drop the unused factor levels automatically.
    -   You can now set all gigs input-checking options at once with `gigs_input_options_set()`.
-   **FIXES**:
    -   Z-scoring based on GIGS-recommended standards now accounts for measurements from different individuals, whereas in earlier versions the z-scoring logic treated data as if it all came from the same individual.
    -   `.gigs_options` is now actively exported by gigs and available to end-users.
-   **INTERNAL/DOCS CHANGES**:
    -   Errors, warnings, and messages from GIGS are now handled with `rlang::abort()` and friends, and look much prettier.
    -   Updates to the 'Getting Started' vignette and 'Benchmarking' article.
    -   Other non-breaking changes to documentation and test suite.

# gigs 0.4.1

-   Changes to documentation to facilitate `autotest::autotest_package()` pass.
-   Minor changes to vignettes for new patched version.

# gigs 0.4.0

-   **BREAKING CHANGES:**
    -   Changed name of `classify_sga()` to `classify_sfga()`
-   **NEW FEATURES:**
    -   Added INTERGROWTH-21<sup>st</sup> Fetal standards, including the Fetal Growth standards, Fetal Doppler standards, Brain Development, and more!
    -   The INTERGROWTH-21<sup>st</sup> Postnatal Growth (`ig_png`) conversion functions now accept post-menstrual age in weeks (`pma_weeks`) as either whole weeks or decimals, e.g. `37` or `37.142857`, where `37.142857` is equal to `37` weeks + `1` day, so `37 + 1/7` weeks.
    -   Added `checkmate`-based verification of inputs, with customised `warning()` and `stop()` mechanisms to exported functions. See documentation for allowed data types/lengths in each function argument.
    -   gigs package options via `.gigs_options`, which lets you customise warning and error behaviour as you see fit.
-   **INTERNAL/DOCS CHANGES:**
    -   Added [statistical review roclets](https://docs.ropensci.org/srr/) tags.
    -   Better documentation, for compliance with SRR tags.
    -   Internal, non-breaking changes to test suite and documentation.

# gigs 0.3.1

-   Internal, non-breaking changes to test suite and documentation.
-   Classification functions now use `gigs_**z()` functions internally for z-scoring. These z-scoring functions, found in `gigs-zscoring.R`, will be made user-facing once the GIGS guidance documentation is made public.

# gigs 0.3.0

-   This update includes several large changes to the GIGS interface, including changed argument names for some classification and conversion functions. We have only not used a major release as the package may still change with the release of the GIGS guidance document.
-   **BREAKING CHANGES:**
    -   Replaced `gest_age` parameter with `gest_days` in all circumstances, including `ig_nbs` growth curve/coefficient tables.
    -   `classify_wasting()` now takes in gestational age/age data, and uses this to apply the INTERGROWTH-21<sup>st</sup> Postnatal Growth standard for weight-for-length where appropriate.
    -   Changed out `percentile` for `centile` in function names and documentation. Argument names left unchanged, so centiles are still passed into functions as `p`.
-   **NEW FEATURES:**
    -   Added `classify_svn()` function, which classifies newborns into the categories used in the Lancet's 2023 Small Vulnerable Newborn (SVN) Series.
-   **INTERNAL/DOCS CHANGES:**
    -   Updated coefficient retrieval functions to run \~ 2 times as fast as in version 0.2.3.
    -   Added type checking functions to conversion functions, along with additional unit tests to make sure they work.
    -   Extensive changes to documentation structure. These changes appear internally, where the roxygen documentation uses more `@inherit`/`@inheritParams` tags.
    -   Added vignette displaying performance differences between gigs and other, similar R packages, as well as Stata implementations.

# gigs 0.2.3

-   Updated regression equations for the normative body composition standards in `ig_nbs`, namely `fmfga`, `ffmfga`, and `bfpfga`. These functions give outputs much closer to the published centiles, though a degree of error is still present.
-   Updated and improved body composition vignette to include the new derivation process.
-   The coefficient-based `ig_nbs`/`who_gs` functions are now 30 to 10x quicker depending on the length of the function inputs (more inputs = slower).
    -   This comes from removal of the `data.table` dependency - all coefficient retrieval/interpolation now occurs using matrices. The move from using `data.table.merge()` to subsetting operators has sped up the code significantly.
-   More consistent subsetting operators and terminology in package source code.

# gigs 0.2.2

-   Optimised interpolation for coefficient-based `who_gs_...()`/`ig_nbs_...()` functions. No more for-loops with rbind - instead, the `approx()` function for interpolation is called on vectors.
-   Began using `data.table` for retrieving/merging coefficient tables, in a new helper function named `retrive_coefficients()`. This approach is slower for smaller inputs than the old `data.frame` approach, but maintains a much lower time as the number of inputs are increased. It also concentrates all the coefficient-retrieval logic in one group of common functions.
-   Minor changes to `README.md` and small refactors for readability.

# gigs 0.2.1

-   Renamed `coarse` parameter in `classify_sga()` to `severe` to better reflect its function; now setting `severe` to `FALSE` will cause `classify_sga()` to specify which SGA cases are below the 3rd percentile.
-   Documentation updates:
    -   Full use of \<sup\> tags for pkgdown website
    -   Standardised titles in documentation for conversion + classification functions
    -   Updated "Introduction to gigs" article to include all available classification functions
    -   Extra docs changes which can be viewed in the diffs for this commit (e.g., replacing all `@returns` Roxygen tags with `@return`)

# gigs 0.2.0

-   Added INTERGROWTH-21st Postnatal Growth tables + functions for a new weight-for-length standard. These include `ig_png_wfl_value2zscore()` and `ig_png_wfl_percentile2value()`.
-   Refactored `check_params.R` to be more standard-specific. Added some comments as the purpose of this code was unclear. May refactor later to make more self-evidently important.

# gigs 0.1.2

-   `classify_sga()` now uses `<` and `>`, not `<=` and `>=` to determine LGA/SGA cut-offs. This aligns this function with the 'correct' methodology for calculating size for GA.

# gigs 0.1.1

-   Major documentation updates
-   Added `life6mo` dataset
-   Added 'Getting Started' vignette

# gigs 0.1.0

-   Created package
-   Added a `NEWS.md` file to track changes to the package.
-   Added conversion functions (e.g. `who_gs_value2zscore()`/`ig_png_value2percentile()`)
-   Added classification functions (e.g. `classify_sga()`)
-   Added reference datasets (e.g. `ig_nbs`, `who_gs`, `who_gs_coeffs`)
