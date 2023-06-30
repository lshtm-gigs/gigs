gigsgrowthstandard_yvars <- list(
  weight_kg = "Weight (kg)",
  length_cm = "Length (cm)",
  headcirc_cm = "Head circumference (cm)",
  wei_len_ratio = "Weight-to-length ratio (kg/m)",
  fat_mass_g = "Fat mass (g)",
  body_fat_perc = "Body fat percentage",
  fatfree_mass_g = "Fat-free mass (g)",
  bmi = "Body mass index (kg/m^2)",
  lenht_cm = "Length/height (cm)",
  armcirc_cm = "Arm circumference (cm)",
  subscap_sf_mm = "Subscapular skinfold (mm)",
  triceps_sf_mm = "Triceps skinfold (mm)"
)

gigsgrowthstandard_xvars <- list(
  gest_age = "Gestational age (days)",
  pma_weeks = "Post-menstrual age (weeks)",
  age_days = "Age (days)",
  length_cm = "Length (cm)",
  height_cm = "Height (cm)"
)

new_gigsgrowthstandard <- function(object = data.frame(),
                                   standard = character(),
                                   acronym = character(),
                                   sex = character(),
                                   zc = character(),
                                   xvar = character(),
                                   yvar = character()) {
  if (!is.data.frame(object)) stop("`object` must be a dataframe")
  attrs <- c(standard, acronym, sex, zc, xvar, yvar) |>
    setNames(c("standard", "acronym", "sex", "zc", "xvar", "yvar"))
  nullattrs <- is.null(attrs)
  if (any(nullattrs)) {
    stop(sum(nullattrs), " essential parameters were not provided: `",
         paste(attrs[nullattrs], collapse = ", `"), "`.")
  }

  gs <- object |>
    structure(class = c("gigsgrowthstandard", "data.frame"),
              standard = standard,
              acronym = acronym,
              sex = sex,
              zc = zc,
              xvar = xvar,
              yvar = yvar)
  gs
}

validate_gigsgrowthstandard <- function(gs) {
  check_gs_cols <- function(gs, required) {
    present <- required %in% names(gs)
    if (!sum(present) == length(required)) {
      missing_SDs <- required[!present]
      stop("Growth standard missing required columns: `",
           paste(missing_SDs, collapse = "`, `"), "`.")
    }
  }

  check_gs_xyvars <- function(gs) {
    if (!attr(gs, which = "xvar") %in% names(gigsgrowthstandard_xvars)) {
      stop("Attribute  `xvar` should be a name in gigsgrowthstandard_xvars")
    }
    if (!attr(gs, which = "yvar") %in% names(gigsgrowthstandard_yvars)) {
      stop("Attribute  `yvar` should be a name in gigsgrowthstandard_yvars")
    }
    gs
  }

  essential_attr_names <- c("standard", "acronym", "sex", "zc", "xvar", "yvar")
  null_attr <- sapply(essential_attr_names, FUN = \(x) is.null(attr(gs, x)))
  if (any(null_attr)) {
    stop(sum(null_attr), " necessary attributes were left as null: \`",
         paste(essential_attr_names[null_attr], collapse = ", "), "\`.")
  }

  check_gs_cols(gs, ifelse(attr(gs, which = "zc") == "zscores",
                           yes = c("SD3neg", "SD2neg", "SD1neg", "SD0", "SD1",
                                   "SD2", "SD3"),
                           no = c("P03", "P10", "P50", "P90", "P97")))
  check_gs_xyvars(gs)
  invisible(gs)
}

gigsgrowthstandard <- function(object, standard, acronym, sex, zc, xvar, yvar) {
  gs <- new_gigsgrowthstandard(object,
                               as.character(standard),
                               as.character(acronym),
                               as.character(sex),
                               as.character(zc),
                               as.character(xvar),
                               as.character(yvar))
    validate_gigsgrowthstandard(gs)
}

#' @importFrom graphics plot
plot.gigsgrowthstandard <- function(x, ...) {
  print(ggplot2::autoplot(x, ...))
}

#' @importFrom ggplot2 autoplot
#' @importFrom RColorBrewer brewer.pal
autoplot.gigsgrowthstandard <- function(object, ...) {
  gs_attributes <- attributes(object)
  zc <- gs_attributes$zc
  xvar <- gs_attributes$xvar
  yvar <- gs_attributes$yvar

  if (zc == "zscores" ) {
    namesto <- "SD"
  } else if (zc == "centiles" ) {
    namesto <- "P"
  } else stop("zc attribute not defined as 'zscores' or `centiles`.")

  obj_long <- object |>
    tidyr::pivot_longer(cols = tidyselect::starts_with(match = namesto,
                                                       ignore.case = FALSE),
                        names_to = namesto,
                        values_to = yvar)

  ribbon <- names(object)[-1]
  median <- ceiling(length(ribbon) / 2 )
  median_str <- ribbon[ceiling(length(ribbon) / 2 )]
  ribbon_min <- ribbon[1:median-1]
  ribbon_max <- ribbon[length(ribbon):(median+1)]

  sm <- function(x) predict(smooth.spline(x = x, all.knots = F))$y

  if (is.null(list(...)$palette)) {
    colours <- RColorBrewer::brewer.pal(name = "Oranges", length(ribbon_min) + 1)
  } else {
    colours <- RColorBrewer::brewer.pal(name = list(...)$palette, length(ribbon_min) + 1)
  }

  plot <- ggplot2::ggplot(object)
  for (i in seq_along(ribbon_min)) {
    fill_lab <- paste(ribbon_min[i], "-", ribbon_max[i])
    plot <- plot +
        ggplot2::geom_ribbon(
          show.legend = TRUE,
          mapping = ggplot2::aes(
            x = .data[[xvar]],
            ymin = sm(.data[[ribbon_max[i]]]),
            ymax = sm(.data[[ribbon_min[i]]]),
            fill = {{ fill_lab }}),
          colour = "grey30")
  }

  plot +
    ggplot2::geom_line(data = dplyr::filter(obj_long, .data[[namesto]] == median_str),
                         mapping = ggplot2::aes(x = .data[[xvar]],
                                                y = sm(.data[[yvar]])),
                         colour = "black", linewidth = 1) +
    ggplot2::labs(x = gigsgrowthstandard_xvars[[xvar]],
                  y = gigsgrowthstandard_yvars[[yvar]]) +
    ggplot2::scale_fill_manual(
       name = paste0(ifelse(zc == "zscores",
                            yes = "Z-score",
                            no = "Centile"), " band"),
       values = setNames(colours,
                         c(paste(ribbon_min, "-", ribbon_max), "Median")),
       guide = "legend") +
    ggplot2::theme_bw()
}

# zscores <- gigs::ig_nbs$wfga$female$zscores |>
#   dplyr::filter(gest_age > 231) |>
#   new_gigsgrowthstandard(standard = "ig_nbs",
#                          acronym = "wfga",
#                          sex = "female",
#                          zc = "zscores",
#                          xvar = "gest_age",
#                          yvar = "weight_kg")
#
# zscores2 <- gigs::who_gs$wfa$male$zscores |>
#   new_gigsgrowthstandard(standard = "who_gs",
#                          acronym = "wfa",
#                          sex = "male",
#                          zc = "zscores",
#                          xvar = "age_days",
#                          yvar = "weight_kg")
#
# zscores3 <- gigs::ig_png$hcfa$female$zscores |>
#   new_gigsgrowthstandard(standard = "ig_png",
#                          acronym = "hcfa",
#                          sex = "female",
#                          zc = "zscores",
#                          xvar = "pma_weeks",
#                          yvar = "headcirc_cm")
#
# centiles <- gigs::ig_nbs$wfga$female$percentiles |>
#   dplyr::filter(gest_age > 231) |>
#   new_gigsgrowthstandard(standard = "ig_nbs",
#                          acronym = "wfga",
#                          sex = "female",
#                          zc = "centiles",
#                          xvar = "gest_age",
#                          yvar = "weight_kg")
#
# centiles2 <- gigs::who_gs$hcfa$male$percentiles |>
#   dplyr::rename(P001 = "P01", P01 = "P1", P03 = "P3", P05 = "P5") |>
#   new_gigsgrowthstandard(standard = "who_gs",
#                          acronym = "hcfa",
#                          sex = "male",
#                          zc = "centiles",
#                          xvar = "age_days",
#                          yvar = "headcirc_cm")
#
# centiles3 <- gigs::ig_png$wfa$male$percentiles |>
#   new_gigsgrowthstandard(standard = "ig_png",
#                          acronym = "wfa",
#                          sex = "male",
#                          zc = "centiles",
#                          xvar = "pma_weeks",
#                          yvar = "weight_kg")
#
# plot(zscores2, palette = "OrRd")
# plot(zscores3, palette = "Greys")
# plot(centiles, palette = "Blues")
# plot(centiles2, palette = "Greens")
# plot(centiles3, palette = "Oranges")
#
#
# # And for plotting individual data on top?
# indiv <- gigs::life6mo |>
#   dplyr::filter(visitweek != 0,
#                 sex == "M",
#                 gestage > 37 * 7,
#                 stringr::str_detect(infantid, pattern = "101")) |>
#   dplyr::mutate(meaninfwgt = meaninfwgt / 1000,
#                 gest_age = gestage)
#
# s <- zscores2 |>
#    dplyr::filter(age_days < (max(indiv$age_days, na.rm = T) + 5)) |>
#   ggplot2::autoplot(palette = "Reds") +
#   ggplot2::geom_line(data = indiv, ggplot2::aes(x = age_days, y = meaninfwgt, group = infantid)) +
#   ggplot2::geom_point(data = indiv, ggplot2::aes(x = age_days, y = meaninfwgt, group = infantid))