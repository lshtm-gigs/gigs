# INTERGROWTH-21st fetal growth standards --------------------------------------

#' Construct URLs to PDFs of INTERGROWTH-21st fetal growth standard tables
ig_fet_url <- function(acronym, zscores_or_centiles) {
  if (acronym == "efwfga") {
    url_stem <- "https://media.tghn.org/medialibrary/2017/"
    if (zscores_or_centiles == "zscores") {
      efw_url <- paste0(url_stem, "03/GROW_EFW_zs_Table.pdf")
    } else {
      efw_url <- paste0(url_stem, "12/GROW_EFW_ct_Table_values.pdf")
    }
    return(efw_url)
  }
  url_stem <- "https://media.tghn.org/medialibrary/2017/03/GROW_Fetal-"
  url_end <- "_Table.pdf"
  cent_or_zs <- c(zscores = "zs", centiles = "ct")[[zscores_or_centiles]]
  url_acronym <- gsub(x = acronym, pattern = "fga$", replacement = "")
  paste0(url_stem, cent_or_zs, "_", url_acronym, url_end)
}

#' Construct R data frame from INTERRGOWTH-21st Fetal Growth standard PDFs
fetal_growth_chart_from_pdf <- function(file) {
  text <- pdftools::pdf_text(file) |>
    strsplit(text, split = "\n") |>
    unlist()
  which_is_chart <- stringr::str_which(
    string = text,
    pattern = "^[:space:]*[:digit:][:digit:]"
  )
  chart_start <- head(which_is_chart, n = 1)
  chart_end <- tail(which_is_chart, n = 1)
  chart_text <- text[chart_start:chart_end]
  chart_headers <- paste("gest_days", text[chart_start - 1])

  chart_content <- c(chart_headers, chart_text) |>
    trimws(which = "left")
  read.table(textConnection(chart_content), header = TRUE)
}

#' Download INTERGROWTH-21st Fetal Growth standards and convert to data frame
download_extract_chart <- function(pdf_url, acronym, z_or_c) {
  pdf_path <- file.path(data_dir, paste0("ig_fet_", acronym, "_", z_or_c,
                                         ".pdf"))
  if (!file.exists(pdf_path)) {
    download.file(url = pdf_url, destfile = pdf_path,
                  mode = "wb")
  }
  chart_names <- if (z_or_c == "zscores") {
    c("SD3neg", "SD2neg", "SD1neg", "SD0", "SD1", "SD2", "SD3")
  } else {
    c("P03", "P05", "P10", "P50", "P90", "P95", "P97")
  }
  fetal_growth_chart_from_pdf(file = pdf_path) |>
    dplyr::mutate(gest_days = 7 * gest_days) |>
    setNames(c("gest_days", chart_names))
}

# Making ig_fet list -----------------------------------------------------------

## Set up
### Make data dir if doesn't exist already
data_dir <- "data-raw/tables/ig_fet"
if (!dir.exists(data_dir)) dir.create(data_dir)

### Set up names for nested list
ig_fet_acronyms <- c("hcfga", "bpdfga", "acfga", "flfga", "ofdfga", "efwfga")
chr_zscores_centiles <- c("zscores", "centiles")

ig_fet <- purrr::map(
  .x = ig_fet_acronyms,
  .f = \(acronym) {
    tbls <- purrr::map2(
      .x = chr_zscores_centiles,
      .y = acronym,
      .f = \(z_or_c, acronym) {
        download_extract_chart(pdf_url = ig_fet_url(acronym, z_or_c),
                               acronym = acronym, z_or_c = z_or_c)
      }
    ) |>
      setNames(chr_zscores_centiles)
    s <- list(zscores = tbls[[1]], centiles = tbls[[2]],
              x = "gest_days",
              y = switch(acronym,
                         hcfga = "headcirc_mm",
                         bpdfga = "bpd_mm",
                         acfga = "abdocirc_mm",
                         flfga = "femurlen_mm",
                         ofdfga = "ofd_mm",
                         efwfga = "efw_g"))
  }
) |>
  setNames(ig_fet_acronyms)

# Add symphysis-fundal height standard -----------------------------------------

li_sfhfga <- list()
for (tbl_type in c("zscores", "centiles")) {
  file <- paste0("data-raw/tables/ig_fet/ig_fet_sfhfga_", tbl_type, ".txt")
  centile_tbl <- read.table(file, header = TRUE) |>
    dplyr::rename(gest_days = gest_wks) |>
    dplyr::mutate(gest_days = gest_days * 7)
  li_sfhfga[[tbl_type]] <- centile_tbl
}
ig_fet[["sfhfga"]] <- li_sfhfga
ig_fet[["sfhfga"]][["x"]] <- "gest_days"
ig_fet[["sfhfga"]][["y"]] <- "sfh_cm"
rm(li_sfhfga, file, centile_tbl)

# IG-21st CRL standards --------------------------------------------------------

#' Convert gestational age as "weeks+days" to number of days
#' @param str Character vector with gestational ages represented as
#'   "weeks+days", e.g. `"40+1"` or `"23+5"`.
conv_ga_plus_days <- function(str) {
  wks <- as.numeric(stringr::str_extract(str, pattern = "^\\d*"))
  days <- as.numeric(stringr::str_extract(str, pattern = "\\d*$"))
  as.integer(7 * wks + days)
}

## CRL for gestational age
li_crlfga <- list()
for (tbl_type in c("zscores", "centiles")) {
  file <- paste0("data-raw/tables/ig_fet/ig_fet_crlfga_", tbl_type, ".txt")
  centile_tbl <- read.table(file, header = TRUE) |>
    dplyr::rename(gest_days = gest_wks) |>
    dplyr::mutate(gest_days = conv_ga_plus_days(gest_days))
  li_crlfga[[tbl_type]] <- centile_tbl
}
li_crlfga[["x"]] <- "gest_days"
li_crlfga[["y"]] <- "crl_mm"
ig_fet[["crlfga"]] <- li_crlfga
rm(li_crlfga, file, centile_tbl)

## Gestational age for CRL
ig_fet[["gafcrl"]] <- list(
  centiles = read.table(
    file = "data-raw/tables/ig_fet/ig_fet_gafcrl_centiles.txt",
    header = TRUE
  ) |>
    dplyr::mutate(dplyr::across(.cols = tidyselect::starts_with(match = "P"),
                                .f = \(x) conv_ga_plus_days(x))
    ),
  x = "crl_mm",
  y = "gest_days")

# IG-21st gestational weight gain standard -------------------------------------

# Centiles: https://media.tghn.org/medialibrary/2017/05/GROW_GWG-nw-ct_Table.pdf
# Z-scores: https://media.tghn.org/medialibrary/2017/05/GROW_GWG-nw-zs_Table.pdf
ig_fet[["gwgfga"]] <- list(
  centiles = read.table(
    file = "data-raw/tables/ig_fet/ig_fet_gwgfga_centiles.txt",
    header = TRUE
  ) |>
    dplyr::mutate(gest_days = gest_wks * 7, .keep = "unused",
                  .before = tidyselect::everything()),
  x = "gest_wt_gain_kg",
  y = "gest_days")

# IG-21st fetal doppler standards ----------------------------------------------

doppler_acronyms <- c("pifga", "rifga", "sdrfga")
for (acronym in doppler_acronyms) {
  file <- paste0("data-raw/tables/ig_fet/ig_fet_", acronym, "_centiles.txt")
  centile_tbl <- read.table(file, header = TRUE)
  names(centile_tbl)[1] <- "gest_days"
  centile_tbl[["gest_days"]] <- conv_ga_plus_days(centile_tbl[["gest_days"]])
  li_new <- list(centiles = centile_tbl,
                 x = "gest_days", y = switch(acronym,
                                             pifga = "puls_idx",
                                             rifga = "resist_idx",
                                             sdrfga = "sys_dia_ratio"))
  ig_fet[[acronym]] <- li_new
}
rm(file, centile_tbl, li_new, acronym, doppler_acronyms)

# IG-21st Fetal trans-cerebellar diameter (TCD) standards ----------------------
## TCD for GA --> from suppl. table 1 of https://dx.doi.org/10.1002/uog.22017
ig_fet[["tcdfga"]] <- list(
  centiles = read.table(
    file = "data-raw/tables/ig_fet/ig_fet_tcdfga_centiles.txt",
    header = TRUE
  ) |>
    dplyr::mutate(gest_days = gest_wks * 7, .keep = "unused",
                  .before = tidyselect::everything()) ,
  x = "gest_days",
  y = "tcd_mm")

## GA for TCD --> suppl. table 2 of https://dx.doi.org/10.1002/uog.22017 has
##     incorrect values, needs correction - using tabulated outputs from
#      equation for now
ig_fet[["gaftcd"]] <- list(
  centiles = read.table(
    skip = 2,
    file = "data-raw/tables/ig_fet/ig_fet_gaftcd_centiles.txt",
    header = TRUE),
  x = "tcd_mm",
  y = "gest_days")


# IG-21st Fetal Brain Development standards ------------------------------------
ig_fet_neural_acronyms <- c("poffga", "sffga", "avfga", "pvfga", "cmfga")
for (acronym in ig_fet_neural_acronyms) {
  ig_fet[[acronym]] <- list(
    centiles = read.table(
      file = glue::glue("data-raw/tables/ig_fet/ig_fet_{acronym}_centiles.txt"),
      header = TRUE
    ) |>
      dplyr::mutate(gest_days = conv_ga_plus_days(gest_wks), .keep = "unused",
                    .before = tidyselect::everything()) |>
      dplyr::select(!n),
    x = "gest_days",
    y = switch(acronym,
               poffga = "par_occ_fiss_mm",
               sffga = "sylv_fiss_mm",
               avfga = "ant_hlv_mm",
               pvfga = "atr_phlv_mm",
               cmfga = "cist_mag_mm"))
}
rm(acronym, ig_fet_neural_acronyms)

usethis::use_data(ig_fet, overwrite = TRUE)