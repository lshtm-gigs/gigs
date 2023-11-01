# gigs 0.2.3.9000
* Updated coefficient retrieval functions to run ~ 2 times as fast as in version
  0.2.3.
* Added `classify_svn()` function, which classifies newborns into the categories
  used in the Lancet's 2023 Small Vulnerable Newborn (SVN) Series. 
* Added type checking functions to conversion functions, along with additional
  unit tests to make sure they work.
* Extensive changes to documentation. These should mostly appear internally, 
  where the roxygen documentation uses more `@inherit`/`@inheritParams` tags. 

# gigs 0.2.3
* Updated regression equations for the normative body composition standards in 
  `ig_nbs`, namely `fmfga`, `ffmfga`, and `bfpfga`. These functions give outputs
  much closer to the published centiles, though a degree of error is still
  present.
* Updated and improved body composition vignette to include the new derivation 
  process.
* The coefficient-based `ig_nbs`/`who_gs` functions are now 30 to 10x quicker 
  depending on the length of the function inputs (more inputs = slower).
  * This comes from removal of the `data.table` dependency - all coefficient 
    retrieval/interpolation now occurs using matrices. The move from using 
    `data.table.merge()` to subsetting operators has sped up the code 
    significantly.
* More consistent subsetting operators and terminology in package source code.

# gigs 0.2.2

* Optimised interpolation for coefficient-based `who_gs_...()`/`ig_nbs_...()` 
  functions. No more for-loops with rbind - instead, the `approx()` function for
  interpolation is called on vectors.
* Began using `data.table` for retrieving/merging coefficient tables, in a new 
  helper function named `retrive_coefficients()`. This approach is slower for 
  smaller inputs than the old `data.frame` approach, but maintains a much lower
  time as the number of inputs are increased. It also concentrates all the 
  coefficient-retrieval logic in one group of common functions.
* Minor changes to `README.md` and small refactors for readability.

# gigs 0.2.1

* Renamed `coarse` parameter in `classify_sga()` to `severe` to better reflect 
  its function; now setting `severe` to `FALSE` will cause `classify_sga()` to
  specify which SGA cases are below the 3rd percentile.
* Documentation updates:
  * Full use of \<sup\> tags for pkgdown website
  * Standardised titles in documentation for conversion + classification 
    functions
  * Updated "Introduction to gigs" article to include all available 
    classification functions
  * Extra docs changes which can be viewed in the diffs for this commit (e.g.,
    replacing all `@returns` Roxygen tags with `@return`)

# gigs 0.2.0

* Added INTERGROWTH-21st Postnatal Growth tables + functions for a new 
  weight-for-length standard. These include `ig_png_wfl_value2zscore()` and 
  `ig_png_wfl_percentile2value()`.
* Refactored `check_params.R` to be more standard-specific. Added some comments
  as the purpose of this code was unclear. May refactor later to make more
  self-evidently important.

# gigs 0.1.2

* `classify_sga()` now uses `<` and `>`, not `<=` and `>=` to determine LGA/SGA
  cut-offs. This aligns this function with the 'correct' methodology for 
  calculating size for GA.

# gigs 0.1.1

* Major documentation updates
* Added `life6mo` dataset
* Added 'Getting Started' vignette

# gigs 0.1.0

* Created package
* Added a `NEWS.md` file to track changes to the package.
* Added conversion functions (e.g. `who_gs_value2zscore()`/`ig_png_value2percentile()`) 
* Added classification functions (e.g. `classify_sga()`)
* Added reference datasets (e.g. `ig_nbs`, `who_gs`, `who_gs_coeffs`)
