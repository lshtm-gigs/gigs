#' Generate growth data for testing
#' @param n Integer > 10; number of rows in the returned data frame. Must be
#'   divisible by 5.
#' @param seed Integer; seed used to generate GA, age, and z-score data.
#' @param restrict Logical; Should the function automatically remove rows in the
#'   output where one of `weight_kg`, `length_cm`, or `headcirc_cm` are `NA`?
#'   Default = `TRUE`.
#' @note This function produces data which is always in the GA range of the
#'   INTERGROWTH-21st Newborn Size standards and age range of the WHO standards.
#'   Whether observations can be used with the INTERGROWTH-21st PNG standards
#'   depends on the random seed used.
#' @returns A data frame with `n` rows and 11 columns containing:
#'   * `gest_age`: Gestational age in days
#'   * `age_days`: Chronological age in days
#'   * `sex`: Sex (`"M"` = male; `"F"` = female)
#'   * `waz`: Weight-for-age z-score
#'   * `lhaz`: Length/height-for-age z-score
#'   * `wlz`: Weight-for-length/height z-score
#'   * `hcaz`: Head circumference-for-age z-score
#'   * `weight_kg`: Expected weight, given the value of `waz` and age/sex
#'   * `lenht_cm`: Expected length/height, given the value of `lhaz` and age/sex
#'   * `weight_kg_from_wlz`: Expected weight, given the value of `wlz` and
#'        length/height/sex
#'   * `headcirc_cm`: Expected head circumference, given the value of `hcaz` and
#'        age/sex
#' @note This is **not** suitable for simulating growth data accurately. The
#'   z-score sampling procedure does **not** account for within-individual
#'   correlation in growth measures over time).
#' @noRd
gigs_random_growth_dataset <- function(n = 10L,
                                       seed = 472346,
                                       restrict = TRUE) {
  checkmate::assert_int(n, lower = 10)
  checkmate::assert_integerish(n / 5, lower = 1)
  checkmate::assert_int(seed)
  who_lim <- 1856L

  data <- withr::with_seed(seed = seed, {
    data.frame(
      id = cut(1:n, breaks = n / 5, labels = 1:(n/5))
    ) |>
      dplyr::mutate(
        gest_age = 24 * 7 + base::sample(x = 0:32, size = 1, replace = TRUE),
        age_days = base::sample.int(
          size = 5,
          n = who_lim,
          replace = FALSE,
          prob = c(0.04, 0.04, 0.04, 0.04, rep.int(x = 0.0005, times = who_lim - 4))
        ),
        sex = base::sample(x = c("M", "F"), size = 1, replace = FALSE),
        waz_exp = stats::rnorm(n = 5),
        lhaz_exp = stats::rnorm(n = 5),
        wlz_exp = stats::rnorm(n = 5),
        hcaz_exp = stats::rnorm(n = 5),
        .by = id
      ) |>
    dplyr::arrange(id, age_days)
  })
  gigs_input_options_set("quiet", TRUE)

  gigs_lgls <- with(
    data, suppressWarnings(gigs_zscoring_lgls(age_days, gest_age, id))
  )

  out <- data |>
    dplyr::mutate(
      id = droplevels(id),
      weight_kg = weight_from_waz(waz_exp,
                                  gest_days = gest_age,
                                  age_days = age_days,
                                  sex = sex,
                                  id = id,
                                  gigs_lgls = gigs_lgls),
      lenht_cm = lenht_from_lhaz(lhaz_exp,
                                 gest_days = gest_age,
                                 age_days = age_days,
                                 sex = sex,
                                 id = id,
                                 gigs_lgls = gigs_lgls),
      weight_kg_from_wlz = weight_from_wlz(wlz_exp,
                                           lenht_cm = lenht_cm,
                                           gest_days = gest_age,
                                           age_days = age_days,
                                           sex = sex,
                                           id = id,
                                           gigs_lgls = gigs_lgls),
      headcirc_cm = headcirc_from_hcaz(hcaz_exp,
                                       gest_days = gest_age,
                                       age_days = age_days,
                                       sex = sex,
                                       id = id,
                                       gigs_lgls = gigs_lgls)
    ) |>
    dplyr::mutate(pma_weeks = (age_days + gest_age) / 7, .after = age_days) |>
    dplyr::mutate(
      bweight_centile_exp = ifelse(age_days == min(age_days, na.rm = TRUE) & age_days < 3,
                                   yes = pnorm(waz_exp),
                                   no = NA),
      .after = waz_exp, .by = id)
  
  gigs_input_options_set("warn", TRUE)
  if (restrict) {
    out <- out |>
      dplyr::filter(complete.cases(weight_kg, lenht_cm, headcirc_cm))
  }
  out
}

#' Get anthropometric measures from z-scores using gigs criteria
#' @param waz,lhaz,wlz,hcaz Numeric vector of length one or more with z-scores
#' @param gest_days Numeric vector of length one or more with gestational age in
#'   days. Used with `age_days` to determine post-menstrual age and therefore
#'   which standards to use.
#' @param age_days Numeric vector of length one or more with age in days. Used
#'   with `gest_days` to determine post-menstrual age and therefore which
#'   standard(s) to use.
#' @param sex Character vector of length one or more with sexes. Values should
#'   be either `"M"` (male) or `"F"` (female).
#' @param gigs_lgls A list with three equal-length logical vectors named
#'   `c("ig_nbs", "ig_png", "who_gs")`. This can be pre-computed and provided to
#'   reduce the amount of computation, if you're using these functions one after
#'   the other. If you're using these functions as a one off, you can leave
#'   this argument as `NULL` (the default value).
#' @note These are functions used to simulate data for unit tests. You can take
#'   them and use them for your own work if you want, but without any guaruntee
#'   that the simulated data are "true to life".
#' @returns A vector of length one or more with expected weight/length/head
#'   circumferences for the z-scores and standards used in the GIGS appraoch.
#' @rdname unit-test-data-simulation
#' @noRd
weight_from_waz <- function(waz,
                            gest_days,
                            age_days,
                            sex,
                            id,
                            gigs_lgls = NULL) {
  if (is.null(gigs_lgls)) {
    gigs_lgls <- gigs_zscoring_lgls(age_days = age_days, gest_days = gest_days,
                                    id = id)
  }
  pma_weeks <- (gest_days + age_days) / 7
  weights <- with(gigs_lgls, expr = {
    weight_ig_nbs <- gigs:::fn_on_subset(ig_nbs_c2v_internal, ig_nbs,
                                         pnorm(waz), gest_days, sex, "wfga")
    weight_ig_png <- gigs:::fn_on_subset(ig_png_z2v_internal, ig_png,
                                             waz, pma_weeks, sex, "wfa")
    weight_who_gs <- gigs:::fn_on_subset(who_gs_z2v_internal, who_gs,
                                             waz, age_days, sex, "wfa")
    list(ig_nbs = weight_ig_nbs,
         ig_png = weight_ig_png,
         who_gs = weight_who_gs)
  })

  weight_kg <- rep_len(x = NA_real_, length.out = length(waz))
  weight_kg[gigs_lgls[["ig_nbs"]]] <- weights[["ig_nbs"]]
  weight_kg[gigs_lgls[["ig_png"]]] <- weights[["ig_png"]]
  weight_kg[gigs_lgls[["who_gs"]]] <- weights[["who_gs"]]
  weight_kg
}

#' @rdname unit-test-data-simulation
#' @noRd
lenht_from_lhaz <- function(lhaz,
                            gest_days,
                            age_days,
                            sex,
                            id,
                            gigs_lgls = NULL) {
  if (is.null(gigs_lgls)) {
    gigs_lgls <- gigs_zscoring_lgls(age_days = age_days, gest_days = gest_days,
                                    id = id)
  }
  pma_weeks <- (gest_days + age_days) / 7
  lengths <- with(gigs_lgls, expr = {
    lenht_ig_nbs <- gigs:::fn_on_subset(ig_nbs_c2v_internal, ig_nbs,
                                        pnorm(lhaz), gest_days, sex, "lfga")
    lenht_ig_png <- gigs:::fn_on_subset(ig_png_z2v_internal, ig_png,
                                        lhaz, pma_weeks, sex, "lfa")
    lenht_who_gs <- gigs:::fn_on_subset(who_gs_z2v_internal, who_gs,
                                        lhaz, age_days, sex, "lhfa")
    list(ig_nbs = lenht_ig_nbs,
         ig_png = lenht_ig_png,
         who_gs = lenht_who_gs)
  })

  lenht_cm <- rep_len(x = NA_real_, length = length(lhaz))
  lenht_cm[gigs_lgls[["ig_nbs"]]] <- lengths[["ig_nbs"]]
  lenht_cm[gigs_lgls[["ig_png"]]] <- lengths[["ig_png"]]
  lenht_cm[gigs_lgls[["who_gs"]]] <- lengths[["who_gs"]]
  lenht_cm
}

#' @rdname unit-test-data-simulation
#' @noRd
weight_from_wlz <- function(wlz,
                            lenht_cm,
                            gest_days,
                            age_days,
                            sex,
                            id,
                            gigs_lgls = NULL) {
  if (is.null(gigs_lgls)) {
    gigs_lgls <- gigs_zscoring_lgls(age_days = age_days, gest_days = gest_days,
                                    id = id)
  }
  gigs_lgls$ig_png <- vctrs::vec_assign(x = gigs_lgls$ig_png,
                                        i = !inrange(lenht_cm, c(35, 65)),
                                        value = FALSE)
  pma_weeks <- (gest_days + age_days) / 7
  lengths <- with(gigs_lgls, expr = {
    lenht_ig_png <- gigs:::fn_on_subset(ig_png_z2v_internal,
                                        ig_png & inrange(pma_weeks, c(27, 64)),
                                        wlz, lenht_cm, sex, "wfl")
    lenht_who_gs_wfl <- gigs:::fn_on_subset(
      who_gs_z2v_internal, who_gs & age_days < 731, wlz, lenht_cm, sex, "wfl"
    )
    lenht_who_gs_wfh <- gigs:::fn_on_subset(
      who_gs_z2v_internal, who_gs & age_days >= 731, wlz, lenht_cm, sex, "wfh"
    )
    list(ig_png = lenht_ig_png,
         who_gs_wfl = lenht_who_gs_wfl,
         who_gs_wfh = lenht_who_gs_wfh)
  })

  lenht_cm <- rep_len(x = NA_real_, length = length(wlz))
  lenht_cm[gigs_lgls[["ig_png"]]] <- lengths[["ig_png"]]
  lenht_cm[gigs_lgls[["who_gs"]] & age_days  < 731] <- lengths[["who_gs_wfl"]]
  lenht_cm[gigs_lgls[["who_gs"]] & age_days >= 731] <- lengths[["who_gs_wfh"]]
  lenht_cm
}

#' @rdname unit-test-data-simulation
#' @noRd
headcirc_from_hcaz <- function(hcaz,
                               gest_days,
                               age_days,
                               sex,
                               id,
                               gigs_lgls = NULL) {
  if (is.null(gigs_lgls)) {
    gigs_lgls <- gigs_zscoring_lgls(age_days = age_days, gest_days = gest_days,
                                    id = id)
  }
  pma_weeks <- (gest_days + age_days) / 7

  headcircs <- with(gigs_lgls, expr = {
    headcirc_ig_nbs <- gigs:::fn_on_subset(ig_nbs_c2v_internal, ig_nbs,
                                           pnorm(hcaz), gest_days, sex, "hcfga")
    headcirc_ig_png <- gigs:::fn_on_subset(ig_png_z2v_internal, ig_png,
                                            hcaz, pma_weeks, sex, "hcfa")
    headcirc_who_gs <- gigs:::fn_on_subset(who_gs_z2v_internal, who_gs,
                                            hcaz, age_days, sex, "hcfa")
    list(ig_nbs = headcirc_ig_nbs,
         ig_png = headcirc_ig_png,
         who_gs = headcirc_who_gs)
  })

  headcirc <- rep_len(x = NA_real_, length = length(hcaz))
  headcirc[gigs_lgls[["ig_nbs"]]] <- headcircs[["ig_nbs"]]
  headcirc[gigs_lgls[["ig_png"]]] <- headcircs[["ig_png"]]
  headcirc[gigs_lgls[["who_gs"]]] <- headcircs[["who_gs"]]
  headcirc
}