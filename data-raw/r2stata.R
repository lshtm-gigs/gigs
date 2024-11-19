# PREPARE DATA FOR STATA PACKAGE -----------------------------------------------

library(gigs)

# GAMLSS coefficients ----------------------------------------------------------
write_dta_gamlssfiles <- function(gamlss_tbls, filestem) {
  dta_dir <- file.path("exclude", "r2stata", "coeffs")
  if (!dir.exists(dta_dir)) dir.create(dta_dir)
  purrr::map2(
    .x = gamlss_tbls,
    .y = names(gamlss_tbls),
    .f = ~ {
      tbls <- .x
      acronym <- .y
      out <- purrr::map2_dfr(
        .x = tbls,
        .y = names(tbls),
        .f = ~ {
          tbl <- .x
          sex <- .y
          sex_col <- ifelse(sex == "male", 1, 0)
          tbl |>
            dplyr::mutate(nbsMSNT_sex = as.integer(sex_col)) |>
            dplyr::rename(nbsMSNT_gest_age = gest_days,
                          nbsMSNT_mu = mu,
                          nbsMSNT_sigma = sigma,
                          nbsMSNT_nu = nu,
                          nbsMSNT_tau = tau)
        })
      haven::write_dta(data = out,
                       path = file.path(dta_dir,
                                        paste0(filestem, acronym, ".dta")))
    })
}
write_dta_gamlssfiles(gigs::ig_nbs_coeffs, filestem = "ig_nbsGAMLSS_")
write_dta_gamlssfiles(gigs::ig_nbs_ext_coeffs, filestem = "ig_nbs_extGAMLSS_")

# LMS coefficients -------------------------------------------------------------
write_dta_lmsfiles <- function(who_gs_lms) {
  dta_dir <- file.path("exclude", "r2stata", "coeffs")
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
                                dplyr::mutate(.x, sex = ifelse(.y == "male",
                                                               yes = 1, no = 0))
                              }) |>
        purrr::list_rbind() |>
        dplyr::rename(whoLMS_xvar = 1, whoLMS_sex = sex,
                      whoLMS_L = L, whoLMS_M = M, whoLMS_S = S)
      haven::write_dta(data = full_tbl,
                        path = file.path(dta_dir,
                                         paste0("whoLMS_", name, ".dta")))
    }
  )
}
write_dta_lmsfiles(who_gs_lms = gigs::who_gs_coeffs)

# LIFE dataset -----------------------------------------------------------------

life6mo_stata <- gigs::life6mo |>
  dplyr::mutate(
    id = haven::labelled(
      x = as.integer(id),
      label = "Unique ID for each infant in the dataset (1--300)"
    ),
    visitweek = haven::labelled(
      x = as.integer(visitweek),
      label = "Chronological age in weeks (+/-1 week) when study visit occurred"
    ),
    sex = haven::labelled(
      x = as.integer(dplyr::if_else(sex == "M", true = 1, false = 2)),
      label = "Sex of each infant (male = 1; female = 2)."
    ),
    gestage = haven::labelled(
      x = as.integer(gestage),
      label = "Best obstetric estimate of gestational age in days (181--291)"
    ),
    age_days = haven::labelled(
      x = as.integer(age_days),
      label = "Chronological age in days at each visit (0--242)."
    ),
    pma = haven::labelled(
      x = as.integer(pma),
      label = "Post-menstrual age in days at each visit (182--528)."
    ),
    wt_kg = haven::labelled(wt_kg,
      label = "Mean weight at each visit in kg (1.24--9.40667)."
    ),
    len_cm = haven::labelled(len_cm,
      label = "Mean length at each visit in cm (37.37--72.93)."
    ),
    headcirc_cm = haven::labelled(headcirc_cm,
      label = "Mean head circumference at each visit in cm (23.20--44.87)."
    ),
    muac_cm = haven::labelled(muac_cm,
      label = "Mean mid-upper arm circumference at each visit in cm (6.30--16.83)."
    )
  ) |>
  dplyr::relocate(id, visitweek, sex, gestage, age_days, pma) |>
  haven::write_dta(path = "exclude/r2stata/life6mo.dta", version = 13)
rlang::warn(c(
  "",
  "i" = "You generated a new version of `life6mo.dta`.",
  "!" = "Run Stata's `compress' function to make the dataset smaller!"
))