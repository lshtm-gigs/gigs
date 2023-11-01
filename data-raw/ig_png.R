make_ig_png_list <- function(raw_stem, x, y) {
  data_dir <- file.path("data-raw", "tables", "ig_png")
  make_percentile_tbl <- function(sex, x) {
    tbl <- read.csv(file = file.path(data_dir,
                                     paste0(raw_stem, sex, "_percentiles.csv")),
                    fileEncoding = "UTF-8-BOM",
                    header = F, sep = " ") |>
      as.data.frame() |>
      dplyr::mutate(V1 = as.numeric(V1)) |>
      purrr::set_names(c(x, "P03", "P05", "P10", "P50", "P90", "P95", "P97"))
  }

  make_zscore_tbl <- function(sex, x) {
    tbl <- read.csv(file = file.path(data_dir,
                                     paste0(raw_stem, sex, "_zscores.csv")),
                    fileEncoding = "UTF-8-BOM",
                    header = F, sep = " ") |>
      as.data.frame() |>
      dplyr::mutate(V1 = as.numeric(V1)) |>
      purrr::set_names(c(x, "SD3neg", "SD2neg", "SD1neg", "SD0", "SD1", "SD2",
                         "SD3"))
  }

  list(male = list(zscores = make_zscore_tbl(sex = "male", x = x),
                   percentiles = make_percentile_tbl(sex = "male", x = x)),
       female = list(zscores = make_zscore_tbl(sex = "female", x = x),
                     percentiles = make_percentile_tbl(sex = "female", x = x)),
              x = x,
              y = y)
}

ig_nbs_weight <- make_ig_png_list(raw_stem = "ig_png_weight_",
                                  x = "pma_weeks",
                                  y = "weight_kg")
ig_nbs_length <- make_ig_png_list(raw_stem = "ig_png_len_",
                                  x = "pma_weeks",
                                  y = "length_cm")
ig_nbs_headcirc <- make_ig_png_list(raw_stem = "ig_png_hc_",
                                    x = "pma_weeks",
                                    y = "headcirc_cm")
ig_nbs_wfl <- make_ig_png_list(raw_stem = "ig_png_wfl_",
                               x = "length_cm",
                               y = "weight_kg")

ig_png <- list(
  wfa = ig_nbs_weight,
  lfa = ig_nbs_length,
  hcfa = ig_nbs_headcirc,
  wfl = ig_nbs_wfl
)
usethis::use_data(ig_png, overwrite = TRUE)
