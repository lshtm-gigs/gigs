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
