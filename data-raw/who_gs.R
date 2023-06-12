construct_who_url <- function(indicator, expand_var, acronym, sex, tbl_type, tbls_var) {
  if(!expand_var %in% c("expanded", "expandable") )
  if (!sex %in% c("male", "female")) stop("Bad sex var", call. = F)
  sex <- ifelse(sex == "male", yes = "boys", no = "girls")
  if (!tbl_type %in% c("zscore", "percentiles")) stop("Bad zscores/percentiles var", call. = F)
  if (!tbls_var %in% c("tables", "table")) stop("Bad tables/table var", call. = F)
  file.path("https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators",
            indicator, glue::glue("{expand_var}-tables"),
            glue::glue("{acronym}-{sex}-{tbl_type}-expanded-{tbls_var}.xlsx"))
}

read_who_data <- function(indicator, expand_var, acronym, sex, tbl_type, tbls_var) {
  local_file <- file.path("data-raw", "tables", "who_gs", paste0("who_gs_", acronym, "_", sex, "_", tbl_type, ".xlsx"))
  who_url <- construct_who_url(indicator = indicator,
                               expand_var = expand_var,
                               acronym = acronym,
                               sex = sex,
                               tbl_type = tbl_type,
                               tbls_var = tbls_var)
  if (!file.exists(local_file)) {
    download.file(url = who_url, destfile = local_file, mode = "wb")
  }
  tbl <- readxl::read_xlsx(local_file) |>
    as.data.frame()
  colnames(tbl)[1] <- switch(colnames(tbl)[1],
                             "Day" = "age_days",
                             "Age" = "age_days",
                             "Length" = "length_cm",
                             "Height" = "height_cm")
  return(tbl)
}

get_who_data <- memoise::memoise(function(indicator, expand_var = "expanded", acronym, tbls_vars = rep("tables", 2), x_y) {
  tables <- tbls_vars
  xy_vars <- x_y
  c("male", "female", "x", "y") |>
    purrr::set_names() |>
    purrr::map(.f = ~ {
      if (.x %in% c("x", "y")) {
        switch(.x, "x" = xy_vars[1], "y" = xy_vars[2])
      } else {
        zscores <- read_who_data(indicator = indicator,
                                 expand_var = expand_var,
                                 acronym = acronym,
                                 sex = .x,
                                 tbl_type = "zscore",
                                 tbls_var = tables[1])
        percentiles <- read_who_data(indicator = indicator,
                                     expand_var = expand_var,
                                     acronym = acronym,
                                     sex = .x,
                                     tbl_type = "percentiles",
                                     tbls_var = tables[2])
        list(zscores = zscores, percentiles = percentiles)
      }
    })
})

who_gs <- list(
  wfa = get_who_data(indicator = "weight-for-age", acronym = "wfa", x_y = c("age_days", "weight_kg")),
  bfa = get_who_data(indicator = "body-mass-index-for-age", acronym = "bfa", x_y = c("age_days", "bmi")),
  lhfa = get_who_data(indicator = "length-height-for-age", expand_var = "expandable", acronym = "lhfa", x_y = c("age_days", "lenht_cm")),
  wfl = get_who_data(indicator = "weight-for-length-height", acronym = "wfl", tbls_vars = c("table", "tables"), x_y = c("len_cm", "weight_kg")),
  wfh = get_who_data(indicator = "weight-for-length-height", acronym = "wfh", x_y = c("height_cm", "weight_kg")),
  hcfa = get_who_data(indicator = "head-circumference-for-age", acronym = "hcfa", x_y = c("headcirc_cm", "age_days")),
  acfa = get_who_data(indicator = "arm-circumference-for-age", acronym = "acfa", x_y = c("armcirc_cm", "age_days")),
  ssfa = get_who_data(indicator = "subscapular-skinfold-for-age", acronym = "ssfa", tbls_vars = c("table", "tables"), x_y = c("subscap_skinfold_mm", "age_days")),
  tsfa = get_who_data(indicator = "triceps-skinfold-for-age", acronym = "tsfa", x_y = c("triceps_skinfold_mm", "age_days"))
)

who_gs_coeffs <- who_gs |>
  purrr::map(.f = ~ {
    list(male = .x[['male']]$zscores |> dplyr::select(1:4),
         female = .x[['female']]$zscores |> dplyr::select(1:4))
  })
usethis::use_data(who_gs_coeffs, overwrite = TRUE)

who_gs <- who_gs |>
  purrr::map(.f = ~ {
    .x[['male']]$zscores <- .x[['male']]$zscores |> dplyr::select(!(L|M|S))
    .x[['female']]$zscores <- .x[['female']]$zscores |> dplyr::select(!(L|M|S))
    .x[['male']]$percentiles <- .x[['male']]$percentiles |> dplyr::select(!(L|M|S))
    .x[['female']]$percentiles <- .x[['female']]$percentiles |> dplyr::select(!(L|M|S))
    return(.x)
  })
usethis::use_data(who_gs, overwrite = TRUE)


write_dta_lmsfiles <- function(who_gs_lms) {
  dta_dir <- file.path("data-raw", "tables", "who_gs", "stata")
  if (!dir.exists(dta_dir)) dir.create(path = dta_dir)
  purrr::walk2(
    .x = who_gs_lms,
    .y = names(who_gs_lms),
    .f = ~ {
      sub_li <- .x
      m_f <- names(.x)
      name <- .y
      full_tbl <- purrr::map2(.x = sub_li,
                              .y = m_f,
                              .f = ~ {
                                dplyr::mutate(.x, sex = ifelse(.y == "male", yes = 1, no = 0))
                              }) |>
        purrr::list_rbind() |>
        dplyr::rename(whoLMS_xvar = 1, whoLMS_L = L, whoLMS_M = M, whoLMS_S = S, whoLMS_sex = sex)
      haven::write_dta(data = full_tbl, path = file.path(dta_dir, paste0("whoLMS_", name, ".dta")))
    }
  )
}
write_dta_lmsfiles(who_gs_lms = who_gs_coeffs)