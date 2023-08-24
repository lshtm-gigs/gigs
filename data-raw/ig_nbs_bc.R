# Normative body composition standards

## Custom model extraction functions -------------------------------------------
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

#' Extract equation and standard deviation; print to console
#' @param df A data.frame from `gigs` for male/female `fmfga`, `bfpfga` or
#' `ffmfga`.
extract_eqn_params <- function(df) {
  li_models <- run_LMs(df)
  # Select 'best' model based on largest adjusted R squared value
  r_sq_adj <- sapply(X = li_models, FUN = \(x) summary(x)$adj.r.squared)
  chosen_model <- li_models[[which(r_sq_adj == max(r_sq_adj))]]
  #
  model_coeffs <- coef(chosen_model) |> unname()
  SD <- extract_SDs(df)
  c(intercept = model_coeffs[1],
    x = model_coeffs[2],
    "x^2" = model_coeffs[3],
    "x^3" = if (length(model_coeffs) == 4) model_coeffs[4]  else 0,
    sigma = SD)
}


## Model extraction ------------------------------------------------------------
### Cache data in list
bodycomp_tables <- list(fmfga_M = gigs::ig_nbs$fmfga$male$percentiles,
                        fmfga_F = gigs::ig_nbs$fmfga$female$percentiles,
                        bfpfga_M = gigs::ig_nbs$bfpfga$male$percentiles,
                        bfpfga_F = gigs::ig_nbs$bfpfga$female$percentiles,
                        ffmfga_M = gigs::ig_nbs$ffmfga$male$percentiles,
                        ffmfga_F = gigs::ig_nbs$ffmfga$female$percentiles)

## Save as list of coefficients ------------------------------------------------
ig_nbs_bc_li <- lapply(bodycomp_tables, extract_eqn_params)
ig_nbs_bc_df <- do.call("rbind", ig_nbs_bc_li) |> t() |> as.data.frame()
ig_nbs_bc_mat <- matrix(unlist(ig_nbs_bc_li, recursive =  FALSE, use.names = FALSE),
                        ncol = length(ig_nbs_bc_li), byrow = TRUE,
                        dimnames = list(c("intercept", "x", "x2", "x3", "sigma"),
                                        names(ig_nbs_bc_li)))
usethis::use_data(ig_nbs_bc_li, internal = TRUE, overwrite = TRUE)