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