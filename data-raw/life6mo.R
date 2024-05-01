get_time <- function(ymd_hms) {
  ymd_hms2 <- lubridate::ymd_hms(ymd_hms)
  hms <- paste(lubridate::hour(ymd_hms2),
               lubridate::minute(ymd_hms2),
               stringr::str_pad(lubridate::second(ymd_hms2), width = 2,
                                pad = "0", side = "right"),
               sep = ":")
  lubridate::hms(hms, quiet = TRUE) |> as.character()
}

life6mo <- readxl::read_xls(file.path("data-raw", "tables", "life6mo",
                                      "LIFE6mo_growth dataset.xls")) |>
  # Make new ID column
  dplyr::mutate(id = dplyr::consecutive_id(infantid),
                .before = tidyselect::everything(),
                .keep = "unused") |>
  # Remove existing z-scores
  dplyr::select(
    !tidyselect::contains(c("waz", "laz", "haz", "wfl", "wlz", "grp"))
  ) |>
  # Remove study variables which aren't useful for GIGS
  dplyr::select(!(motherid | sizega | LBWtype4 | withdrawalrsn | deliverymode2 |
    preterm)) |>
  # Remove non-singleton pregnancies; then drop this and `sibling` cols
  dplyr::filter(birthcount == 1) |>
  dplyr::select(!(birthcount | sibling)) |>
  # Remove rows where visit wasn't attended
  dplyr::filter(visitattend == 1) |>
  dplyr::select(!visitattend) |>
  # Remove rows where gestational age is not within INTERGROWTH-21st NBS bounds
  dplyr::filter(gestage > 24 * 7 & gestage < 300) |>
  # Re-generate IDs |>
  dplyr::mutate(id = as.integer(dplyr::consecutive_id(id))) |>
  # Remove rows with all missing measurement data --> not useful
  ## Start by converting 0 meaninfwgt to NA
  dplyr::mutate(meaninfwgt = ifelse(meaninfwgt == 0, yes = NA, meaninfwgt)) |>
  ## Then do a complete.cases drop
  dplyr::filter(complete.cases(meaninfwgt, meaninflen, meanhead, meanmuac)) |>
  # Convert sex to "M"/"F"
  dplyr::mutate(sex = factor(dplyr::case_when(sex == 1 ~ "M", sex == 2 ~ "F",
                                              .default = NA),
                             levels = c("M", "F"))) |>
  # Add age_days col
  dplyr::mutate(age_days = as.integer(pma - gestage), .after = pma) |>
  # Convert gestage/pma to integer cols
  dplyr::mutate(dplyr::across(.cols = tidyselect::any_of(c("gestage", "pma")),
                              .fns = as.integer)) |>
  # Rename measurement columns
  dplyr::rename(weight_g = meaninfwgt, len_cm = meaninflen,
                headcirc_cm = meanhead, muac_cm = meanmuac) |>
  # Reorder columns
  dplyr::select(id, gestage, sex, visitweek, pma, age_days, weight_g, len_cm,
                headcirc_cm, muac_cm)

# Select subset of IDs:
#   * Keep all IDs which have a birthweight and are not term AGA
#   * Keep enough other IDs at random without replacement to have 300 IDs
IDs_with_birthweight_and_not_term_AGA <- life6mo |>
  dplyr::filter(visitweek == 0 & age_days < 0.5) |>
  dplyr::mutate(svn = gigs::compute_svn(weight_g / 1000, gestage, as.character(sex))) |>
  dplyr::filter(svn != "Term AGA") |>
  dplyr::pull(id) |>
  unique()
lgl_IDs_to_sample <- !unique(life6mo$id) %in% IDs_with_birthweight_and_not_term_AGA
IDs_to_sample <- unique(life6mo$id)[lgl_IDs_to_sample]
IDs_to_use <- c(
  IDs_with_birthweight_and_not_term_AGA,
  withr::with_seed(seed = 34789, {
    sample(x = IDs_to_sample,
           size = 300 - length(IDs_with_birthweight_and_not_term_AGA),
           replace = FALSE)
  }))

life6mo <- life6mo |>
  dplyr::filter(id %in% IDs_to_use) |>
  dplyr::mutate(id = dplyr::consecutive_id(id)) |>
  # Set to data frame (i.e. not tibble)
  as.data.frame()

# Save to gigs
usethis::use_data(life6mo, overwrite = TRUE)
