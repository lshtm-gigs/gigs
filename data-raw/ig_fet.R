# Functions for downloading/parsing INTERGROWTH-21st fetal growth charts -------

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

fetal_growth_chart_from_pdf <- function(file) {
  text <- pdftools::pdf_text(file) |>
    strsplit(text, split = "\n") |>
    unlist()
  which_is_chart <- stringr::str_which(string = text,
                                       pattern = "^[:space:]*[:digit:][:digit:]")
  chart_start <- head(which_is_chart, n = 1)
  chart_end <- tail(which_is_chart, n = 1)
  chart_text <- text[chart_start:chart_end]
  chart_headers <- paste("gest_days", text[chart_start - 1])

  chart_content <- c(chart_headers, chart_text) |>
    trimws(which = "left")
  read.table(textConnection(chart_content), header = TRUE)
}

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

# Set up -----------------------------------------------------------------------

# Make data dir if doesn't exist already
data_dir <- "data-raw/tables/ig_fet"
if (!dir.exists(data_dir)) dir.create(data_dir)

# Set up names for nested list
ig_fet_acronyms <- c("hcfga", "bpdfga", "acfga", "flfga", "ofdfga", "efwfga")
chr_zscores_centiles <- c("zscores", "centiles")

# Making growth chart list; save to package ------------------------------------

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

usethis::use_data(ig_fet, overwrite = TRUE)