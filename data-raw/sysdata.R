# LMS/GAMLSS coefficient rownames, see load_coefficient_matrix() ---------------
who_gs_coeff_rownames <- purrr::map2(
  .x = rep_len(c("M", "F"), length.out = 18),
  .y = rep_len(names(gigs::who_gs_coeffs), length.out = 18),
  .f = \(sex, acronym) {
    sex_acronym <- paste0(sex, "_",  acronym)
    sex_long <- if (sex == "M") "male" else "female"
    x_indices <- unlist(gigs::who_gs_coeffs[[acronym]][[sex_long]][,1])
    paste0(sex_acronym, "_", x_indices)
  }) |>
  purrr::set_names(paste0(rep_len(c("M", "F"), length.out = 18), "_",
                          rep_len(names(gigs::who_gs_coeffs), length.out = 18)))

ig_nbs_coeff_rownames <- purrr::map2(
  .x = rep_len(c("M", "F"), length.out = 6),
  .y = rep_len(names(gigs::ig_nbs_coeffs), length.out = 6),
  .f = \(sex, acronym) {
    sex_acronym <- paste0(sex, "_",  acronym)
    sex_long <- if (sex == "M") "male" else "female"
    x_indices <- unlist(gigs::ig_nbs_coeffs[[acronym]][[sex_long]][,1])
    paste0(sex_acronym, "_", x_indices)
  }) |>
  purrr::set_names(paste0(rep_len(c("M", "F"), length.out = 6), "_",
                          rep_len(names(gigs::ig_nbs_coeffs), length.out = 6)))

coeff_rownames <- c(who_gs_coeff_rownames, ig_nbs_coeff_rownames)

# Normative body composition standards -----------------------------------------

#' Extract standard deviation from percentiles
#' @param P50 Value of y at mean
#' @param PX Value of y at percentile specified by p
#' @param p Percentile at which y = PX
std_deviation <- function(P50, PX, p) (PX - P50) / qnorm(p = p)

run_LMs <- function(df) {
  model_linear <- lm(P50 ~ gest_age, data = df)
  model_poly1 <- lm(P50 ~ poly(gest_age, degree = 1, raw = TRUE), data = df)
  model_poly2 <- lm(P50 ~ poly(gest_age, degree = 2, raw = TRUE), data = df)
  model_poly3 <- lm(P50 ~ poly(gest_age, degree = 3, raw = TRUE), data = df)
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
  r_sq_adj <- sapply(X = li_models, FUN = \(x) summary(x)$adj.r.squared)
  chosen_model <- li_models[[which(r_sq_adj == max(r_sq_adj))]]
  model_coeffs <- coef(chosen_model) |> unname()
  SD <- extract_SDs(df)
  c(intercept = model_coeffs[1],
    x = model_coeffs[2],
    "x^2" = model_coeffs[3],
    "x^3" = if (length(model_coeffs) == 4) model_coeffs[4]  else 0,
    sigma = SD)
}

## Model extraction
### Cache data in list
bodycomp_tables <- list(fmfga_M = gigs::ig_nbs$fmfga$male$percentiles,
                        fmfga_F = gigs::ig_nbs$fmfga$female$percentiles,
                        bfpfga_M = gigs::ig_nbs$bfpfga$male$percentiles,
                        bfpfga_F = gigs::ig_nbs$bfpfga$female$percentiles,
                        ffmfga_M = gigs::ig_nbs$ffmfga$male$percentiles,
                        ffmfga_F = gigs::ig_nbs$ffmfga$female$percentiles)

## Save as list of coefficients
ig_nbs_bc_li <- lapply(bodycomp_tables, extract_eqn_params)

# Save data to sysdata.rda with usethis ----------------------------------------
usethis::use_data(ig_nbs_bc_li, coeff_rownames,
                  internal = TRUE, overwrite = TRUE)
