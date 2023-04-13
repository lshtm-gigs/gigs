make_ig_png_list <- function(raw_stem, x, y) {
  data_dir <- file.path("data-raw", "tables", "ig_png")
  make_percentile_tbl <- function(sex) {
    tbl <- read.csv(file = file.path(data_dir, paste0(raw_stem, sex, "_percentiles.csv")), fileEncoding = "UTF-8-BOM",
                    header = F, sep = " ") |>
      as.data.frame() |>
      dplyr::rename(pma_weeks = V1, P03 = V2, P05 = V3, P10 = V4, P50 = V5, P90 = V6, P95 = V7, P97 = V8) |>
      dplyr::mutate(pma_weeks = as.numeric(pma_weeks))
    return(tbl)
  }

  make_zscore_tbl <- function(sex) {
    tbl <- read.csv(file = file.path(data_dir, paste0(raw_stem, sex, "_zscores.csv")), fileEncoding = "UTF-8-BOM",
                    header = F, sep = " ") |>
      as.data.frame() |>
      dplyr::rename(pma_weeks = V1, SD3neg = V2, SD2neg = V3, SD1neg = V4, SD0 = V5, SD1 = V6, SD2 = V7, SD3 = V8) |>
      dplyr::mutate(pma_weeks = as.numeric(pma_weeks))
  }

  return(list(male = list(zscores = make_zscore_tbl(sex = "male"),
                          percentiles = make_percentile_tbl(sex = "male")),
              female = list(zscores = make_zscore_tbl(sex = "female"),
                            percentiles = make_percentile_tbl(sex = "female")),
              x = x,
              y = y))
}

ig_nbs_weight <- make_ig_png_list(raw_stem = "ig_png_weight_", x = "pma_weeks", y = "weight_kg")
ig_nbs_length <- make_ig_png_list(raw_stem = "ig_png_len_", x = "pma_weeks", y = "length_cm")
ig_nbs_headcirc <- make_ig_png_list(raw_stem = "ig_png_hc_", x = "pma_weeks", y = "headcirc_cm")

ig_png <- list(
  wfa = ig_nbs_weight,
  lfa = ig_nbs_length,
  hcfa = ig_nbs_headcirc
)
usethis::use_data(ig_png, overwrite = TRUE)