# PREPARE DATA FOR GIGS (SAS) PACKAGE ------------------------------------------

library(gigs)

#' Write a data frame to SAS-readable .txt format with `write.table()`
#' @param x A data frame to write to disk
#' @param file Name for saved file.
#' @param sep Separator for .txt file. Default = `","`.
write.table_sas <- function(x, file, sep = ",") {
  x[is.na(x)] <- " "
  write.table(x,
              file = file,
              sep = sep,
              row.names = FALSE,
              col.names = FALSE,
              quote = FALSE)
  x
}

ig_fet_zscores <- list()
for (standard in names(gigs::ig_fet)) {
  tbl_zscores <- gigs::ig_fet[[standard]][["zscores"]]
  if (is.null(tbl_zscores)) {
    next
  } else {
    ig_fet_zscores[[standard]] <- gigs::ig_fet[[standard]][["zscores"]] |>
      dplyr::mutate(acronym = standard,
                    x_unit = gigs::ig_fet[[standard]][["x"]],
                    y_unit = gigs::ig_fet[[standard]][["y"]])
  }
}
ig_fet_zscores <- ig_fet_zscores |>
  dplyr::bind_rows() |>
  dplyr::relocate(acronym, x_unit, y_unit, .before = tidyselect::everything()) |>
  write.table_sas(file = "exclude/r2sas/ig_fet_zscores.txt", sep = " ")

ig_fet_centiles <- list()
for (standard in names(gigs::ig_fet)) {
  tbl_zscores <- gigs::ig_fet[[standard]][["centiles"]]
  if (is.null(tbl_zscores)) {
    next
  } else {
    ig_fet_centiles[[standard]] <- gigs::ig_fet[[standard]][["centiles"]] |>
      dplyr::mutate(acronym = standard,
                    x_unit = gigs::ig_fet[[standard]][["x"]],
                    y_unit = gigs::ig_fet[[standard]][["y"]])
    names(ig_fet_centiles[[standard]])[1] <- "x"
  }
}
ig_fet_centiles <- ig_fet_centiles |>
  dplyr::bind_rows() |>
  dplyr::relocate(P25, .before = P50) |>
  dplyr::relocate(P75, .after = P50) |>
  dplyr::relocate(acronym, x_unit, y_unit, .before = tidyselect::everything()) |>
  write.table_sas(file = "exclude/r2sas/ig_fet_centiles.txt", sep = " ")


ig_nbs_zscores <- list()
for (standard in names(gigs::ig_nbs)) {
  li_zscores <- list()
  for (sex in c("male", "female")) {
    li_zscores[[sex]] <- gigs::ig_nbs[[standard]][[sex]][["zscores"]]
  }
  df_zscores <- dplyr::bind_rows(li_zscores, .id = "sex") |>
    dplyr::mutate(sex = ifelse(sex == "male", yes = "M", no = "F"))
  ig_nbs_zscores[[standard]] <- df_zscores |>
    dplyr::mutate(acronym = standard,
                  y_unit = gigs::ig_nbs[[standard]][["y"]])
}
ig_nbs_zscores <- ig_nbs_zscores |>
  dplyr::bind_rows() |>
  dplyr::relocate(acronym, y_unit, sex, .before = tidyselect::everything()) |>
  write.table_sas(file = "exclude/r2sas/ig_nbs_zscores.txt", sep = " ")

ig_nbs_centiles <- list()
for (standard in names(gigs::ig_nbs)) {
  li_centiles <- list()
  for (sex in c("male", "female")) {
    li_centiles[[sex]] <- gigs::ig_nbs[[standard]][[sex]][["centiles"]]
  }
  df_centiles <- dplyr::bind_rows(li_centiles, .id = "sex") |>
    dplyr::mutate(sex = ifelse(sex == "male", yes = "M", no = "F"))
  ig_nbs_centiles[[standard]] <- df_centiles |>
    dplyr::mutate(acronym = standard,
                  y_unit = gigs::ig_nbs[[standard]][["y"]])
}
ig_nbs_centiles <- ig_nbs_centiles |>
  dplyr::bind_rows() |>
  dplyr::relocate(acronym, y_unit, sex, .before = tidyselect::everything()) |>
  write.table_sas(file = "exclude/r2sas/ig_nbs_centiles.txt", sep = " ")

ig_png_zscores <- list()
for (standard in names(gigs::ig_png)) {
  li_zscores <- list()
  for (sex in c("male", "female")) {
    li_zscores[[sex]] <- gigs::ig_png[[standard]][[sex]][["zscores"]]
  }
  df_zscores <- dplyr::bind_rows(li_zscores, .id = "sex") |>
    dplyr::mutate(sex = ifelse(sex == "male", yes = "M", no = "F"))
  ig_png_zscores[[standard]] <- df_zscores |>
    dplyr::mutate(acronym = standard,
                  x_unit = gigs::ig_png[[standard]][["x"]],
                  y_unit = gigs::ig_png[[standard]][["y"]])
  names(ig_png_zscores[[standard]])[2] <- "x"
}
ig_png_zscores <- ig_png_zscores |>
  dplyr::bind_rows() |>
  dplyr::relocate(acronym, x_unit, y_unit, sex, .before = tidyselect::everything()) |>
  write.table_sas(file = "exclude/r2sas/ig_png_zscores.txt", sep = " ")

ig_png_centiles <- list()
for (standard in names(gigs::ig_png)) {
  li_centiles <- list()
  for (sex in c("male", "female")) {
    li_centiles[[sex]] <- gigs::ig_png[[standard]][[sex]][["centiles"]]
  }
  df_centiles <- dplyr::bind_rows(li_centiles, .id = "sex") |>
    dplyr::mutate(sex = ifelse(sex == "male", yes = "M", no = "F"))
  ig_png_centiles[[standard]] <- df_centiles |>
    dplyr::mutate(acronym = standard,
                  x_unit = gigs::ig_png[[standard]][["x"]],
                  y_unit = gigs::ig_png[[standard]][["y"]])
  names(ig_png_centiles[[standard]])[2] <- "x"
}
ig_png_centiles <- ig_png_centiles |>
  dplyr::bind_rows() |>
  dplyr::relocate(acronym, x_unit, y_unit, sex, .before = tidyselect::everything()) |>
  write.table_sas(file = "exclude/r2sas/ig_png_centiles.txt", sep = " ")

gigs::gigs_options_set(new_value = "quiet")
write.table_sas(gigs::life6mo, file = "exclude/r2sas/life6mo.txt")

life6mo_comp <- gigs::life6mo |>
  dplyr::mutate(
    weight_kg = weight_g / 1000,
    sex = as.character(sex),
    gest_days = pma - age_days) |>
  dplyr::select(!muac_cm) |>
  gigs::classify_growth(weight_kg = weight_kg,
                        lenht_cm = len_cm,
                        headcirc_cm = headcirc_cm,
                        sex = sex,
                        age_days = age_days,
                        gest_days = gest_days) |>
  dplyr::select(id, visitweek,
                birthweight_centile, sfga, sfga_severe, svn,
                lhaz, stunting, stunting_outliers,
                wlz, wasting, wasting_outliers,
                waz, wfa, wfa_outliers,
                hcaz, headsize)
write.table_sas(life6mo_comp, file = "exclude/r2sas/life6mo_comparison.txt")