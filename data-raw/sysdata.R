# Normative body composition standards -----------------------------------------

#' Extract standard deviation from centiles
#' @param P50 Value of y at mean
#' @param PX Value of y at centile specified by `p`
#' @param p Centile at which y = PX
std_deviation <- function(P50, PX, p) (PX - P50) / qnorm(p = p)

run_LMs <- function(df) {
  model_linear <- lm(P50 ~ gest_days, data = df)
  model_poly1 <- lm(P50 ~ poly(gest_days, degree = 1, raw = TRUE), data = df)
  model_poly2 <- lm(P50 ~ poly(gest_days, degree = 2, raw = TRUE), data = df)
  model_poly3 <- lm(P50 ~ poly(gest_days, degree = 3, raw = TRUE), data = df)
  list(model_linear, model_poly1, model_poly2, model_poly3)
}

extract_SDs <- function(df) {
  stddev_03 <- std_deviation(df$P50, df$P03, p = 0.03)
  stddev_10 <- std_deviation(df$P50, df$P10, p = 0.10)
  stddev_90 <- std_deviation(df$P50, df$P90, p = 0.90)
  stddev_97 <- std_deviation(df$P50, df$P97, p = 0.97)
  stddevs <- c(stddev_03, stddev_10, stddev_90, stddev_97)
  mean(stddevs, na.rm = TRUE)
}

extract_eqn_params <- function(df) {
  li_models <- run_LMs(df)
  # Select 'best' model based on largest adjusted R squared value
  r_sq_adj <- vapply(X = li_models,
                     FUN = \(x) summary(x)$adj.r.squared,
                     FUN.VALUE = numeric(length = 1L))
  chosen_model <- li_models[[which(r_sq_adj == max(r_sq_adj))]]
  model_coeffs <- coef(chosen_model) |> unname()
  SD <- extract_SDs(df)
  c(intercept = model_coeffs[1],
    x = model_coeffs[2],
    "x^2" = model_coeffs[3],
    "x^3" = if (length(model_coeffs) == 4) model_coeffs[4] else 0,
    sigma = SD)
}

## Model extraction
### Cache data in list
bodycomp_tables <- list(fmfga_M = gigs::ig_nbs$fmfga$male$centiles,
                        fmfga_F = gigs::ig_nbs$fmfga$female$centiles,
                        bfpfga_M = gigs::ig_nbs$bfpfga$male$centiles,
                        bfpfga_F = gigs::ig_nbs$bfpfga$female$centiles,
                        ffmfga_M = gigs::ig_nbs$ffmfga$male$centiles,
                        ffmfga_F = gigs::ig_nbs$ffmfga$female$centiles)


# Valid x-ranges for each acronym ----------------------------------------------
xvar_ranges <- list()
for (chr_standard in c("ig_fet", "ig_nbs", "ig_png", "who_gs")) {
  li_ranges <- list()
  li_standard <- getExportedValue(ns = "gigs", chr_standard)
  for (chr_acronym in names(li_standard)) {
    curr_xvar <- switch(chr_standard,
                        ig_fet = li_standard[[chr_acronym]][[1]][[1]],
                        li_standard[[chr_acronym]][[1]][[1]][[1]])
    li_ranges[[chr_acronym]] <- range(curr_xvar)
  }
  names(li_ranges) <- names(li_standard)
  xvar_ranges[[chr_standard]] <- li_ranges
}

rm(li_ranges, li_standard, curr_xvar, chr_acronym, chr_standard)

# Save data to sysdata.rda with usethis ----------------------------------------
usethis::use_data(ig_nbs_bc_li, xvar_ranges, internal = TRUE, overwrite = TRUE)

# Save body composition data as .dta file for use in stata-gigs ----------------
dta_dir <- file.path("data-raw", "tables", "ig_nbs", "stata")
if (!dir.exists(dta_dir)) dir.create(dta_dir)
ig_nbs_bc_df <- as.data.frame(ig_nbs_bc_li) |>
  t() |>
  as.data.frame() |>
  tibble::rownames_to_column( var = "nbsBC_sexacronym") |>
  dplyr::rename(nbsBC_intercept = intercept,
                nbsBC_x = `x`,
                nbsBC_x2 = `x^2`,
                nbsBC_x3 = `x^3`,
                nbsBC_sigma = sigma) |>
  `rownames<-`(value = NULL)
ig_nbs_bc_df |>
  haven::write_dta(path = file.path(dta_dir, "ig_nbsNORMBODYCOMP.dta"))