get_time <- function(ymd_hms) {
  ymd_hms2 <- lubridate::ymd_hms(ymd_hms)
  hms <- paste(lubridate::hour(ymd_hms2),
               lubridate::minute(ymd_hms2),
               stringr::str_pad(lubridate::second(ymd_hms2), width = 2, pad = "0", side = "right"),
               sep = ":")
  lubridate::hms(hms, quiet = TRUE)
}

life6mo <- readxl::read_xls(file.path("data-raw", "tables", "life6mo", "LIFE6mo_growth dataset.xls")) |>
  dplyr::mutate(sex = factor(dplyr::case_when(sex == 1 ~ "M", sex == 2 ~ "F", .default = NA))) |>
  dplyr::mutate(age_days = as.integer(pma - gestage), .after = pma) |>
  dplyr::mutate(temp = stringr::str_extract(string = motherid, pattern = "^[:digit:]*")) |>
  dplyr::mutate(site = as.integer(stringr::str_extract(string = temp, pattern = "^[:digit:]")),
                facility = as.integer(stringr::str_extract(string = temp, pattern = "[:digit:]$")),
                .before = "birthdate", .keep = "unused") |>
  dplyr::mutate(visittime = get_time(visittime)) |>
  dplyr::mutate(meaninfwgt = ifelse(meaninfwgt == 0, yes = NA, meaninfwgt)) |>
  dplyr::select(!tidyselect::contains(c("waz", "laz", "haz", "wfl", "wlz", "grp"))) |>
  as.data.frame()

usethis::use_data(life6mo, overwrite = TRUE)