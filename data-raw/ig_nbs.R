make_ig_nbs_list <- function(raw_stem, x, y) {
  data_dir <- file.path("data-raw", "tables", "ig_nbs")
  make_zscore_tbl <- function(sex) {
    tbl <- read.csv(file = file.path(data_dir, paste0(raw_stem, sex,
                                                      "_zscores.csv")),
                    fileEncoding = "UTF-8-BOM",
                    header = F, sep = " ") |>
      as.data.frame() |>
      dplyr::rename(gest_days = V1, SD3neg = V2, SD2neg = V3, SD1neg = V4,
                    SD0 = V5, SD1 = V6, SD2 = V7, SD3 = V8) |>
      dplyr::mutate(gest_days = 168:300)
    return(tbl)
  }

  make_centile_tbl <- function(sex) {
    tbl <- read.csv(file = file.path(data_dir,
                                     paste0(raw_stem, sex, "_percentiles.csv")),
                    fileEncoding = "UTF-8-BOM",
                    header = F,
                    sep = " ") |>
      as.data.frame() |>
      dplyr::rename(gest_days = V1, P03 = V2, P05 = V3, P10 = V4, P50 = V5,
                    P90 = V6, P95 = V7, P97 = V8) |>
      dplyr::mutate(gest_days = 168:300)
    return(tbl)
  }
  return(list(male = list(zscores = make_zscore_tbl(sex = "male"),
                          centiles = make_centile_tbl(sex = "male")),
              female = list(zscores = make_zscore_tbl(sex = "female"),
                            centiles = make_centile_tbl(sex = "female")),
              x = x,
              y = y))
}


ig_nbs_wfga <- make_ig_nbs_list(raw_stem = "ig_nbs_weight_",
                                x = "gest_days",
                                y = "weight_kg")
ig_nbs_lfga <- make_ig_nbs_list(raw_stem = "ig_nbs_len_",
                                x = "gest_days",
                                y = "length_cm")
ig_nbs_hcfga <- make_ig_nbs_list(raw_stem = "ig_nbs_hc_",
                                 x = "gest_days",
                                 y = "headcirc_cm")
ig_nbs_wlrfga <- make_ig_nbs_list(raw_stem = "ig_nbs_weightlenratio_",
                                  x = "gest_days",
                                  y = "wei_len_ratio")


make_ig_nbs_bodycomp_list <- function(raw_stem, x, y) {
  data_dir <- file.path("data-raw", "tables", "ig_nbs")
  make_centile_tbl <- function(sex) {
    tbl <- read.csv(file = file.path(data_dir,
                                     paste0(raw_stem, sex, "_percentiles.csv")),
                    fileEncoding = "UTF-8-BOM",
                         header = F,
                    sep = ",") |>
      as.data.frame() |>
      dplyr::rename(gest_days = V1, P03 = V2, P10 = V3, P50 = V4, P90 = V5,
                    P97 = V6)
   return(tbl)
  }
  return(list(male = list(centiles = make_centile_tbl(sex = "male")),
              female = list(centiles = make_centile_tbl(sex = "female")),
              x = x,
              y = y))
}
ig_nbs_ffmfga <- make_ig_nbs_bodycomp_list(raw_stem = "ig_nbs_ffmfga_",
                                           x = "gest_days",
                                           y = "fatfree_mass_g")
ig_nbs_bfpfga <- make_ig_nbs_bodycomp_list(raw_stem = "ig_nbs_bfpfga_",
                                           x = "gest_days",
                                           y = "body_fat_perc")
ig_nbs_fmfga <- make_ig_nbs_bodycomp_list(raw_stem = "ig_nbs_fmfga_",
                                          x = "gest_days",
                                          y = "fat_mass_g")

ig_nbs <- list(
  wfga = ig_nbs_wfga,
  lfga = ig_nbs_lfga,
  hcfga = ig_nbs_hcfga,
  wlrfga = ig_nbs_wlrfga,
  ffmfga = ig_nbs_ffmfga,
  bfpfga = ig_nbs_bfpfga,
  fmfga = ig_nbs_fmfga
)

usethis::use_data(ig_nbs, overwrite = TRUE)

ig_coeffs_readxl_wrap <- function(path) {
  tbl <- readxl::read_excel(path = path, sheet = 2) |>
    dplyr::relocate(sex, GA, mu, sigma, nu, tau) |>
    dplyr::select(!anthro) |>
    dplyr::rename(gest_days = GA) |>
    as.data.frame()
  return(list(
    male = dplyr::filter(tbl, sex == "Male") |> dplyr::select(!sex),
    female = dplyr::filter(tbl, sex == "Female") |> dplyr::select(!sex)
  ))
}

# Using tables provided by Eric Ohuma
ig_nbs_coeffs <- list(
  wfga = ig_coeffs_readxl_wrap(
    path = "data-raw/tables/ig_nbs/Newborn standards parameters (BW).xlsx"),
  lfga = ig_coeffs_readxl_wrap(
    path = "data-raw/tables/ig_nbs/Newborn standards parameters (BL).xlsx"),
  hcfga = ig_coeffs_readxl_wrap(
    path = "data-raw/tables/ig_nbs/Newborn standards parameters (HC).xlsx")
)
usethis::use_data(ig_nbs_coeffs, overwrite = TRUE)
