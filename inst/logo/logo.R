# Make plot of HCFA in very-preterm newborn infants
load("data/ig_nbs.rda")
long_tbl <- ig_nbs$wfga$female$zscores |>
  dplyr::filter(gest_days > 231)
tidy_tbl <- tidyr::pivot_longer(long_tbl,
                                cols = tidyselect::starts_with("SD"),
                                names_to = "SD",
                                values_to = "weight_kg")
sm <- function(x) predict(smooth.spline(x = x))$y
curve <- ggplot2::ggplot() +
  ggplot2::geom_ribbon(data = long_tbl,
                       mapping = ggplot2::aes(x = gest_days, ymax = sm(SD3),
                                              ymin = sm(SD2)), alpha = 0.6,
                       fill = "#ffdab9") +
  ggplot2::geom_ribbon(data = long_tbl,
                       mapping = ggplot2::aes(x = gest_days, ymax = sm(SD2),
                                              ymin = sm(SD1)), alpha = 0.6,
                       fill = "#ff8c00") +
  ggplot2::geom_ribbon(data = long_tbl,
                       mapping = ggplot2::aes(x = gest_days, ymax = sm(SD1),
                                              ymin = sm(SD0)), alpha = 0.8,
                       fill = "#fe5a1d") +
  ggplot2::geom_ribbon(data = long_tbl,
                       mapping = ggplot2::aes(x = gest_days, ymax = sm(SD0),
                                              ymin = sm(SD1neg)), alpha = 0.8,
                       fill = "#fe5a1d") +
  ggplot2::geom_ribbon(data = long_tbl,
                       mapping = ggplot2::aes(x = gest_days, ymax = sm(SD1neg),
                                              ymin = sm(SD2neg)), alpha = 0.6,
                       fill = "#ff8c00") +
  ggplot2::geom_ribbon(data = long_tbl,
                       mapping = ggplot2::aes(x = gest_days, ymax = sm(SD2neg),
                                              ymin = sm(SD3neg)), alpha = 0.6,
                       fill = "#ffdab9") +
  ggplot2::geom_smooth(data = dplyr::filter(tidy_tbl, SD == "SD0"),
                       mapping = ggplot2::aes(x = gest_days, y = weight_kg),
                       colour = "#7f4029", weight = 0.5, alpha = 0.5) +
  ggplot2::theme_void()

title_font <- "Merriweather" # Added to R using extrafont package
url_font <- "Open Sans" # Added to R using extrafont package
temp <- "inst/logo/temp_sticker.png"
name_size <- 10.5
url_size <- 1.15
sticker <- hexSticker::sticker(
  filename = temp,

  subplot = curve,
  s_x = 1, s_y = 1, s_height = 1.09, s_width = 1.9,

  package = "gigs",
  p_size = name_size, p_x = 0.85, p_y = 1.425,  p_color = "#000000",
  p_family = title_font,

  h_fill = "#FFFFFF", h_color = "#fe7b3b",

  url = "https://www.github.com/ropensci/gigs",
  u_x = 1.05, u_y = 0.085, u_size = url_size, u_angle = 30,
  u_family = url_font) +
  cowplot::draw_image(image = "inst/logo/temp2.png", x = 0.625, y = 0.190,
                      width = 1, height = 1, scale = 0.925)
unlink(temp)
ggplot2::ggsave(sticker,
                filename = "man/figures/logo.png",
                height = 5.08, width = 4.39, units = "cm", type = "cairo")
usethis::use_logo(img = "man/figures/logo.png", geometry = "400x400")
