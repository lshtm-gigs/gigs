make_ig_png_list <- function(raw_stem, x, y) {
  data_dir <- file.path("data-raw", "tables", "ig_png")
  make_percentile_tbl <- function(sex, x) {
    tbl <- read.csv(file = file.path(data_dir, paste0(raw_stem, sex, "_percentiles.csv")), fileEncoding = "UTF-8-BOM",
                    header = F, sep = " ") |>
      as.data.frame() |>
      dplyr::mutate(V1 = as.numeric(V1)) |>
      purrr::set_names(c(x, "P03", "P05", "P10", "P50", "P90", "P95", "P97"))
  }

  make_zscore_tbl <- function(sex, x) {
    tbl <- read.csv(file = file.path(data_dir, paste0(raw_stem, sex, "_zscores.csv")), fileEncoding = "UTF-8-BOM",
                    header = F, sep = " ") |>
      as.data.frame() |>
      dplyr::mutate(V1 = as.numeric(V1)) |>
      purrr::set_names(c(x, "SD3neg", "SD2neg", "SD1neg", "SD0", "SD1", "SD2", "SD3"))
  }

  return(list(male = list(zscores = make_zscore_tbl(sex = "male", x = x),
                          percentiles = make_percentile_tbl(sex = "male", x = x)),
              female = list(zscores = make_zscore_tbl(sex = "female", x = x),
                            percentiles = make_percentile_tbl(sex = "female", x = x)),
              x = x,
              y = y))
}

ig_nbs_weight <- make_ig_png_list(raw_stem = "ig_png_weight_", x = "pma_weeks", y = "weight_kg")
ig_nbs_length <- make_ig_png_list(raw_stem = "ig_png_len_", x = "pma_weeks", y = "length_cm")
ig_nbs_headcirc <- make_ig_png_list(raw_stem = "ig_png_hc_", x = "pma_weeks", y = "headcirc_cm")
ig_nbs_wfl <- make_ig_png_list(raw_stem = "ig_png_wfl_", x = "length_cm", y = "weight_kg")

ig_png <- list(
  wfa = ig_nbs_weight,
  lfa = ig_nbs_length,
  hcfa = ig_nbs_headcirc,
  wfl = ig_nbs_wfl
)
usethis::use_data(ig_png, overwrite = TRUE)

# median <- function(length_cm, sex) {
#    ifelse(sex == "M",
#           yes = 13.98383 + 203.5677 * (length_cm / 10) ^ -2 - 291.114 * ((length_cm / 10)^ -2 * log(length_cm/10)),
#           no = 50.32492 + 140.8019 * (length_cm / 10) ^ -1 - 167.906 * (length_cm / 10) ^ -0.5)
# }
#
# stddev <- function(length_cm, sex) {
#   ifelse(sex == "M",
#          yes = exp(-1.830098 + 0.0049708 * (length_cm / 10)^3),
#          no = 0.2195888 -0.0046046 * (length_cm / 10) ^ 3 + 0.0033017 * (length_cm / 10) ^ 3 * log(length_cm/10))
# }
#
# wfl_value2percentile <- function(weight_kg, length_cm, sex) {
#   pnorm(wfl_value2zscore(weight_kg, length_cm, sex))
# }
#
# wfl_value2zscore <- function(weight_kg, length_cm, sex) {
#   max_len_vecs <- vctrs::vec_recycle_common(weight_kg = weight_kg, length_cm = length_cm, sex = sex)
#   max_len_vecs$median <- median(max_len_vecs$length_cm, max_len_vecs$sex)
#   max_len_vecs$stdev <- stddev(max_len_vecs$length_cm, max_len_vecs$sex)
#   (max_len_vecs$weight_kg - max_len_vecs$median) / max_len_vecs$stdev
# }
#
# wfl_percentile2value <- function(p, length_cm, sex) {
#   wfl_zscore2value(qnorm(p), length_cm, sex)
# }
#
# wfl_zscore2value <- function(z, length_cm, sex) {
#   max_len_vecs <- vctrs::vec_recycle_common(z = z, length_cm = length_cm, sex = sex)
#   max_len_vecs$median <- median(max_len_vecs$length_cm, max_len_vecs$sex)
#   max_len_vecs$stddev <- stddev(max_len_vecs$length_cm, max_len_vecs$sex)
#   max_len_vecs$median + max_len_vecs$z * max_len_vecs$stddev
# }
# len_range <- seq(35, 65, by = 0.1)
# z_tbl <- purrr::map(
#   .x = -3:3,
#   .f = ~ round(wfl_zscore2value(z = .x,
#                                 length_cm = len_range,
#                                 sex = "F"),
#                digits = 2)) |>
#     do.call(what = cbind) |>
#     as.data.frame() |>
#     purrr::set_names(c("SD3neg", "SD2neg", "SD1neg", "SD0", "SD1", "SD2", "SD3")) |>
#   dplyr::mutate(length_cm = len_range,
#                 .before = tidyselect::everything()) |>
#   readr::write_delim(file = "data-raw/tables/ig_png/ig_png_wfl_female_zscores.csv",
#                      col_names = FALSE, delim = " ")
#
# purrr::map(
#   .x = c(0.03, 0.05, 0.1, 0.5, 0.9, 0.95, 0.97),
#   .f = ~ round(wfl_percentile2value(p = .x,
#                                     length_cm = len_range,
#                                     sex = "M"),
#                digits = 2)) |>
#     do.call(what = cbind) |>
#     as.data.frame() |>
#     dplyr::mutate(length_cm = len_range,
#                   .before = tidyselect::everything()) |>
#   readr::write_delim(file = "data-raw/tables/ig_png/ig_png_wfl_male_percentiles.csv",
#                      col_names = FALSE, delim = " ")