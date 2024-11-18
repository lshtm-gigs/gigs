# Extended INTERGROWTH-21st newborn size standards -----------------------------
library(mfp)

ig_nbs_coeffs <- gigs::ig_nbs_coeffs
ig_nbs_ext_coeffs <- list(wfga = list(male = NULL, female = NULL),
                          lfga = list(male = NULL, female = NULL),
                          hcfga = list(male = NULL, female = NULL))

plot_extrapolated <- function(plot_data, coefficient) {
    acronym_str <- switch(acronym,
                          wfga = "weight-for-GA",
                          lfga = "length-for-GA",
                          hcfga = "head circumference-for-GA")
    plot <- ggplot2::ggplot(
      plot_data, ggplot2::aes(x = gest_days, y = {{ coefficient }},
                              colour = source)) +
      ggplot2::geom_line() +
      ggplot2::geom_vline(xintercept = 300, colour = "grey", linetype = "dashed") +
      ggplot2::geom_vline(xintercept = modelling_cutoff, colour = "grey20", linetype = "dashed") +
      ggplot2::scale_x_continuous(breaks = seq(231, 314, 14),
                                  labels = seq(231, 314, 14)) +
      ggplot2::labs(
        title = glue::glue("Predicted vs. original coefficients for<br>**{sex}s** in **{acronym_str}**"),
        x = "Gestational age (days)",
        colour = "Coefficient source") +
      ggpubr::theme_pubr() +
      ggplot2::theme(plot.title = ggtext::element_markdown())
}

for (acronym in c("wfga", "lfga", "hcfga")) {
  for (sex in c("male", "female")) {
    coeffs <- gigs::ig_nbs_coeffs[[acronym]][[sex]]

    modelling_cutoff <- 280
    alpha <- 0.15 # Increase from 0.05 to make fp 'take' to some standards
    modelling_coeffs <- dplyr::filter(coeffs, gest_days >= modelling_cutoff)
    mfp_mu <- mfp::mfp(mu ~ fp(gest_days, df = 4), data = modelling_coeffs,
                       maxits = 200, alpha = alpha)

    coeffs_ext <- data.frame(gest_days = 231:314)
    coeffs_ext$mu <- predict(mfp_mu, newdata = coeffs_ext)

    mfp_sigma <- mfp::mfp(sigma ~ fp(gest_days, df = 4),
                          data = modelling_coeffs, maxits = 200, alpha = alpha)
    coeffs_ext$sigma <- predict(mfp_sigma, newdata = coeffs_ext)

    coeffs_ext$nu <- coeffs$nu[2]
    coeffs_ext$tau <- coeffs$tau[2]

    plot_df <- dplyr::bind_rows(
      dplyr::mutate(coeffs, source = "Original coeffs"),
      dplyr::mutate(coeffs_ext, source = "Extrapolated coeffs") |>
        dplyr::filter(gest_days >= modelling_cutoff)) |>
      dplyr::select(!(nu | tau))

    cowplot::plot_grid(
      plotlist = list(plot_extrapolated(plot_df, coefficient = mu),
                      plot_extrapolated(plot_df, coefficient = sigma))
    ) |>
      print()

    # Replace predicted coeffs with old coeffs for old GA range
    coeffs_ext$mu[seq_along(231:300)] <- coeffs$mu
    coeffs_ext$sigma[seq_along(231:300)] <- coeffs$sigma
    ig_nbs_ext_coeffs[[acronym]][[sex]] <- coeffs_ext
  }
}

generate_extrapolated_ig_nbs <- function(acronym, sex, mode) {
  curve <- purrr::map2(
    .x = switch(mode,
                plot = c(0.03, 0.10, 0.25, 0.50, 0.75, 0.90, 0.97),
                centiles = c(0.03, 0.05, 0.10, 0.50, 0.90, 0.95, 0.97),
                zscores = pnorm(-3:3)),
    .y = switch(mode,
                plot = c("P03", "P10", "P25", "P50", "P75", "P90", "P97"),
                centiles = c("P03", "P05", "P10", "P50", "P90", "P95", "P97"),
                zscores = c("SD3neg", "SD2neg", "SD1neg", "SD0", "SD1", "SD2",
                            "SD3"))
      ,
    .f = function(centile, name) {
      vpns_lim <- 231
      gest_days <- 154:314
      y_nbs_ext <- with(ig_nbs_ext_coeffs[[acronym]][[sex]],
                        gamlss.dist::qST3C(p = centile,
                                           mu = mu,
                                           sigma = sigma,
                                           nu = nu,
                                           tau = tau))
      y_vpns_ext <- gigs:::ig_vpns_zscore2value(z = qnorm(centile),
                                                154:230,
                                                switch(sex,
                                                       male = "M",
                                                       female = "F"),
                                                acronym)
      standard <- ifelse(
        test = gest_days >= vpns_lim,
        yes = "NS",
        no = "Very Preterm NS"
      )
      x <- data.frame(gest_days = gest_days,
                      Centile = name,
                      y = c(y_vpns_ext, y_nbs_ext),
                      standard = standard)
    }) |>
    purrr::list_rbind()
}

plot_extrapolated_chart <- function(acronym, sex) {
  curve_data <- generate_extrapolated_ig_nbs(acronym, sex, mode = "plot")
  key_colours <- c("P03" = "#fa2c2e",
                   "P10" = "black",
                   "P25" = "#ff7d2b",
                   "P50" = "#09843b",
                   "P75" = "#ff7d2b",
                   "P90" = "black",
                   "P97" = "#fa2c2e")
  acronym_str <- switch(acronym,
                        wfga = "weight-for-GA",
                        lfga = "length-for-GA",
                        hcfga = "head circumference-for-GA")
  curve <- ggplot2::ggplot(
    curve_data, ggplot2::aes(x = gest_days, y = y, colour = Centile,
                             id = standard)) +
    ggplot2::geom_vline(xintercept = 168, colour = "grey",
                        linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 300, colour = "grey",
                        linetype = "dashed") +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = seq(154, 314, 14),
                                labels = seq(154, 314, 14)) +
    ggplot2::scale_colour_manual(values = key_colours) +
    ggplot2::labs(
      title = glue::glue("Extrapolated growth curve for **{sex}s** in **{acronym_str}**"),
      x = "Gestational age (days)") +
    ggpubr::theme_pubr() +
    ggplot2::theme(plot.title = ggtext::element_markdown(size = 9))
}

ig_nbs_ext <- list(wfga = list(male = list(zscores = NULL, centiles = NULL),
                               female = list(zscores = NULL, centiles = NULL),
                               x = NULL, y = NULL),
                   lfga = list(male = list(zscores = NULL, centiles = NULL),
                               female = list(zscores = NULL, centiles = NULL),
                               x = NULL, y = NULL),
                   hcfga = list(male = list(zscores = NULL, centiles = NULL),
                                female = list(zscores = NULL, centiles = NULL),
                                x = NULL, y = NULL))

for (acronym in c("wfga", "lfga", "hcfga")) {
  for (sex in c("male", "female")) {
    plt <- plot_extrapolated_chart(acronym, sex)
    print(plt)
    cowplot::ggsave2(
      plot = plt, width = 5, height = 4, units = "in",
      filename = glue::glue("exclude/extending_ig_nbs/plot_{acronym}_{sex}.png")
    )

    # Store centiles
    centile_data <- generate_extrapolated_ig_nbs(acronym, sex,
                                                 mode = "centiles")
    ig_nbs_ext[[acronym]][[sex]][["centiles"]] <- centile_data |>
      dplyr::select(!standard) |>
      tidyr::pivot_wider(names_from = "Centile", values_from = "y") |>
      `colnames<-`(colnames(gigs::ig_nbs[[acronym]][[sex]][["centiles"]])) |>
      as.data.frame() |>
      round(digits = 2)

    # Store z-scores
    zscore_data <- generate_extrapolated_ig_nbs(acronym, sex, mode = "zscores")
    ig_nbs_ext[[acronym]][[sex]][["zscores"]] <- zscore_data |>
      dplyr::select(!standard) |>
      tidyr::pivot_wider(names_from = "Centile", values_from = "y") |>
      `colnames<-`(colnames(gigs::ig_nbs[[acronym]][[sex]][["zscores"]])) |>
      as.data.frame() |>
      round(digits = 2)
  }
  ig_nbs_ext[[acronym]][["x"]] <- gigs::ig_nbs[[acronym]]$x
  ig_nbs_ext[[acronym]][["y"]] <- gigs::ig_nbs[[acronym]]$y
}

usethis::use_data(ig_nbs_ext, overwrite = TRUE)
usethis::use_data(ig_nbs_ext_coeffs, overwrite = TRUE)

# Save extended INTERGROWTH-21st NBS coeffs .dta file for use in stata-gigs ----
dta_dir <- file.path("data-raw", "tables", "ig_nbs_ext")
if (!dir.exists(dta_dir)) dir.create(dta_dir)
dta_dir <- file.path(dta_dir, "stata")
if (!dir.exists(dta_dir)) dir.create(dta_dir)

for (acronym in names(gigs::ig_nbs_ext_coeffs)) {
  male <- gigs::ig_nbs_ext_coeffs[[acronym]]$male |>
    dplyr::rename(gest_age = gest_days) |>
    dplyr::mutate(sex = 1)
  colnames(male) <- paste0("nbs_extMSNT_", colnames(male))
  female <- gigs::ig_nbs_ext_coeffs[[acronym]]$female |>
    dplyr::rename(gest_age = gest_days) |>
    dplyr::mutate(sex = 0)
  colnames(female) <- paste0("nbs_extMSNT_", colnames(female))
  dplyr::bind_rows(male, female) |>
    haven::write_dta(
      path = file.path(dta_dir, paste0("ig_nbs_extGAMLSS_", acronym, ".dta")),
      version = 15
    )
}