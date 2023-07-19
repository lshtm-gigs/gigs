#' Easily add growth standards to plots
GeomGrowthStandard <- ggplot2::ggproto(
  "GeomGrowthStandard",
  ggplot2::Geom,
  required_aes = c("x", "y", "growthstandard"),
  default_aes = ggplot2::aes(
    palette = "Oranges",
    colour = "grey60",
    alpha = 0.6,
    linetype = ggplot2::GeomPath$default_aes$linetype,
    linewidth = ggplot2::GeomPath$default_aes$linewidth
  ),

  # TODO: Write draw_key_growthstandard?
  draw_key = ggplot2::draw_key_point,

  draw_panel = function(data, panel_params, coord) {
    sm <- function(x) predict(smooth.spline(x = x, all.knots = F))$y

    growthstandard <- unique(data$growthstandard)
    gs_id <- strsplit(growthstandard, split = "-")[[1]]
    call_as_str <- paste0("gigs::", gs_id[1], "$", gs_id[2], "$", gs_id[3],
                          "$", gs_id[4])

    growthstandard <- rlang::eval_tidy(rlang::parse_expr(call_as_str)) |>
      dplyr::mutate(dplyr::across(.cols = tidyselect::matches(match = "^P|^SD"),
                                  .fns = ~ sm(.x))) |>
      gigsgrowthstandard(standard = "who_gs",
                         acronym = "wfa",
                         sex = "male",
                         zc = ifelse(gs_id[4] == "zscores",
                                     yes = "zscores",
                                     no = "centiles"),
                         xvar = "age_days",
                         yvar = "weight_kg")

    if (!inherits(growthstandard, what = "gigsgrowthstandard")) {
      stop("The object looked up by geom_growthstandard() is not valid")
    }
    coords <- coord$transform(data, panel_params)
    n_colours <- length(names(growthstandard)) / 2
    colours <- RColorBrewer::brewer.pal(n_colours, name = unique(data$palette))

    gs_median <- median_growthstandard(growthstandard) |>
      dplyr::filter(x >= panel_params$x.range[1],
                    x <= panel_params$x.range[2]) |>
      dplyr::mutate(
        PANEL = unique(data$panel),
        group = 1,
        alpha = 1,
        color = colours[n_colours],
        colour = colours[n_colours],
        linetype = unique(data$linetype),
        linewidth = unique(data$linewidth) * 1.25,
      )
    mid <- ggplot2::GeomPath$draw_panel(
      gs_median,
      panel_params = panel_params,
      coord = coord)
    gs_ribbon <- long_growthstandard(growthstandard) |>
      dplyr::filter(x >= panel_params$x.range[1],
                    x <= panel_params$x.range[2]) |>
      dplyr::mutate(group = paste0(maxband, "-", minband)) |>
      dplyr::mutate(ymin = sm(ymin), ymax = sm(ymax), .by = group)

    gs_ribbon <- gs_ribbon |>
      dplyr::mutate(
        PANEL = data$panel,
        fill = rep(colours[-n_colours], length.out = dplyr::n()),
        alpha = unique(data$alpha),
        color = unique(data$color),
        colour = unique(data$colour),
        linewidth = unique(data$linewidth),
        .keep = "unused")

    ribbons <- purrr::map(.x = unique(gs_ribbon$group),
                          .f = ~ {
                            ggplot2::GeomRibbon$draw_group(
                              data = dplyr::filter(gs_ribbon, group == .x),
                              panel_params, coord)
                          })
    do.call(grid::gList, c(ribbons, list(mid)))
  }
)

median_growthstandard <- function(gs) {
  yonly <- gs[, -1]
  name_median <- names(yonly)[ceiling(length(yonly) / 2)]
  dplyr::select(gs, 1, {{ name_median }}) |>
  dplyr::rename(x = 1, y = 2)
}

long_growthstandard <- function(gs) {
  yonly <- gs[, -1]
  yonly_len <- length(yonly)
  names_ymin <- names(yonly)[1:ceiling(yonly_len / 2) - 1]
  name_median <- names(yonly)[ceiling(yonly_len / 2)]
  names_ymax <- names(yonly)[(ceiling(yonly_len / 2) + 1): yonly_len]

  gs_long <- gs |>
    dplyr::select(!{{ name_median }}) |>
    tidyr::pivot_longer(cols = tidyselect::any_of(names_ymin),
                        names_to = "minband",
                        values_to = "ymin") |>
    tidyr::pivot_longer(cols = tidyselect::any_of(names_ymax),
                        names_to = "maxband",
                        values_to = "ymax") |>
    dplyr::rename(x = 1)
  if (attr(gs, which = "zc") == "zscores") {
    dplyr::filter(gs_long,
                  stringr::str_extract(minband, pattern = "^SD\\d") == maxband)
  } else if (attr(gs, which = "zc") == "centiles") {
    check_p <- function(pmin, pmax) {
      nchar_bool <- nchar(pmin) != nchar(pmax)
      min_num <- gsub(pattern = "P", x = pmin, replacement = "")
      max_num <- gsub(pattern = "P", x = pmax, replacement = "")

      summed <- as.numeric(min_num) + as.numeric(max_num)
      divisor <- 10 ^ nchar(max_num)
      modulo_bool <- summed %% divisor == 0
      nchar_bool | modulo_bool
    }
    dplyr::filter(gs_long, check_p(minband, maxband))
  }
}

geom_growthstandard <- function(mapping = NULL, data = NULL,
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = FALSE, ...) {
  ggplot2::layer(
    geom = GeomGrowthStandard, stat = "identity",
    data = data, mapping = mapping, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# Test geom for WHO GS
# indiv_who <- gigs::life6mo |>
#   dplyr::filter(visitweek != 0,
#                 sex == "M",
#                 gestage > 37 * 7,
#                 stringr::str_detect(infantid, pattern = "101")) |>
#   dplyr::mutate(meaninfwgt = meaninfwgt / 1000,
#                 gest_age = gestage)
#
# ggplot2::ggplot(indiv_who, ggplot2::aes(x = age_days, y = meaninfwgt, group = infantid)) +
#   geom_growthstandard(ggplot2::aes(x = age_days,
#                                    y = meaninfwgt,
#                                    growthstandard = "who_gs-wfa-male-zscores",
#                                    palette = "Blues"),
#                       na.rm = TRUE) +
#   ggplot2::geom_line(na.rm = TRUE) +
#   ggplot2::geom_point(na.rm = TRUE) +
#   ggplot2::theme_bw()
#
# # Test geom for IG NBS
# indiv_ig_nbs <- gigs::life6mo |>
#   dplyr::filter(visitweek == 0,
#                 sex == "M",
#                 gestage > 24 * 7,
#                 stringr::str_detect(infantid, pattern = "101")) |>
#   dplyr::mutate(meaninfwgt = meaninfwgt / 1000,
#                 gest_age = gestage)
#
# ggplot2::ggplot(indiv_ig_nbs, ggplot2::aes(x = gestage, y = meaninfwgt, group = infantid), na.rm = TRUE) +
#   geom_growthstandard(ggplot2::aes(x = gestage, y = meaninfwgt, growthstandard = "ig_nbs-wfga-male-percentiles",
#                                    palette = "Blues")) +
#   ggplot2::geom_point() +
#   ggplot2::theme_bw()
#
# plot(gigs::ig_png$hcfa$male$zscores |>
#        new_gigsgrowthstandard(standard = "ig_nbs",
#                               acronym = "wlrfga",
#                               sex = "male",
#                               zc = "zscores",
#                               xvar = "pma_weeks",
#                               yvar = "headcirc_cm"),
#      palette = "Oranges")