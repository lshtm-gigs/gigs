# PREPARE DATA FOR STATA PACKAGE -----------------------------------------------

library(gigs)

# GAMLSS coefficients ----------------------------------------------------------
write_dta_gamlssfiles <- function(gamlss_tbls) {
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
                                        paste0("ig_nbsGAMLSS_",
                                               acronym, ".dta")))
    })
}
write_dta_gamlssfiles(gigs::ig_nbs_coeffs)

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
    sex = haven::labelled(
      x = as.integer(dplyr::if_else(sex == "M", true = 1, false = 2)),
      labels = c("Male" = 1, "Female" = 2),
      label = "Sex of each infant (male = 1; female = 2)."
    ),
    gestage = as.integer(gestage),
    age_days = as.integer(age_days),
    pma = as.integer(pma),
  ) |>
  dplyr::relocate(id, visitweek, sex, gestage, visitweek, age_days, pma) |>
  haven::write_dta(path = "exclude/r2stata/life6mo.dta", version = 13)
rlang::warn(c(
  "",
  "i" = "You generated a new version of `life6mo.dta`.",
  "!" = "Run Stata's `compress' function to make the dataset smaller!"
))