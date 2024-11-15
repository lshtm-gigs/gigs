#' Calculate z-scores for anthropometric measures according to GIGS guidance
#'
#' @description These functions calculate z-scores for weight-for-age (WAZs),
#'   length/height-for-age (LHAZs), weight-for-length/height (WLZs), or
#'   head circumference-for-age (HCAZs).
#'
#'   The growth standard used for each observation differs based on the
#'   gestational and post-menstrual age of the infant being analysed. The
#'   different procedures are based on the advised use of each growth standard,
#'   where a measurement is considered a 'birth measurement' if it is the first
#'   measurement taken from an infant and was taken <3 days after birth:
#'   * `gest_days` < 24 weeks (168 days):
#'     - Birth: No standard available
#'     - Postnatal: IG-21<sup>st</sup> Postnatal Growth standards from 27 to 64
#'       weeks' PMA
#'     - Postnatal: Uncorrected WHO standard after 64 weeks' PMA
#'   * `gest_days` >= 24 weeks (168 days):
#'     - Birth: IG-21<sup>st</sup> Newborn Size standards at birth
#'       (including Very Preterm Newborn Size standards if born before 33
#'       weeks).
#'     - Postnatal: IG-21<sup>st</sup> Postnatal Growth standards from 27 to 64
#'       weeks' PMA
#'     - Postnatal: Uncorrected WHO Child Growth standards after 64 weeks' PMA
#'   * `gest_days` >= 37 weeks (259 days):
#'     - Birth: IG-21<sup>st</sup> Newborn Size standards
#'     - Postnatal: Uncorrected WHO Child Growth standards
#'   * `gest_days` >= 43 weeks (301 days):
#'     - Birth: No standard available
#'     - Postnatal: Uncorrected WHO Child Growth standards
#'
#'   For `gigs_wlz()`, two WHO standards are available:
#'   * The weight-for-length standard, which is applied when `age_days < 731`.
#'   * The weight-for-height standard, which is applied when `age_days >= 731`.
#' @param weight_kg A numeric vector of length one or more with weight in kg.
#' @param age_days A numeric vector of length one or more with ages in days.
#' @param gest_days A numeric vector of length one or more with gestational ages
#'   in days.
#' @param id A factor of length one or more with an ID for each observation,
#'   either ordered or unordered, containing no missing (`NA`) values. When
#'   supplied, `id` is used to ensure that only the earliest measurement for
#'   each individual is used as a birth measure. Leave this argument as `NULL`
#'   if all your data comes from the same individual. Default = `NULL`.
#' @inheritParams shared_roxygen_params
#' @examples
#' gest_days <- c(rep(35 * 7, 3), rep(35 * 7, 3))
#' age_days <- c(0, 100, 500, 2, 100, 500)
#' sex <- rep.int(c("M", "F"), c(3, 3))
#' wt_kg <- c(3, 6, 9, 3, 6, 9)
#' len_cm <- rep.int(c(52.2, 60.4, 75), 2)
#' head_cm <- rep.int(c(30, 40, 49), 2)
#' ids <- factor(rep.int(c("A", "B"), c(3, 3)))
#'
#' # Weight-for-age z-score (WAZ)
#' waz <- gigs_waz(weight_kg = wt_kg,
#'                 age_days = age_days,
#'                 gest_days = gest_days,
#'                 sex = sex,
#'                 id = ids)
#' print(waz)
#'
#' # Note - if you don't specify 'id' you'll get different results!
#' waz_no_id <- gigs_waz(weight_kg = wt_kg,
#'                       age_days = age_days,
#'                       gest_days = gest_days,
#'                       sex = sex)
#' print(waz == waz_no_id)
#'
#' # Length/height-for-age z-score (LHAZ)
#' lhaz <- gigs_lhaz(lenht_cm = len_cm,
#'                   age_days = age_days,
#'                   gest_days = gest_days,
#'                   sex = sex,
#'                   id = ids)
#' print(lhaz)
#'
#' # Weight-for-length/height z-score (WLZ)
#' #   Note: There's no at-birth standards for weight-for-length, so elements 1
#' #   and 4 have `NA` values
#' wlz <- gigs_wlz(weight_kg = wt_kg,
#'                 lenht_cm = len_cm,
#'                 age_days = age_days,
#'                 gest_days = gest_days,
#'                 sex = sex,
#'                 id = ids)
#' print(wlz)
#'
#' # Head circumference-for-age z-score (HCAZ)
#' hcaz <- gigs_hcaz(headcirc_cm = head_cm,
#'                   age_days = age_days,
#'                   gest_days = gest_days,
#'                   sex = sex,
#'                   id = ids)
#' print(hcaz)
#' @returns A numeric vector of z-scores, derived using the appropriate growth
#'   standard for each element-wise combination of `gest_days` and `age_days`.
#'   For `gigs_wlz()`, all birth WLZs will be missing (`NA`), as there is no
#'   INTERGROWTH-21<sup>st</sup> Newborn standard for weight-for-length.
#' @note These functions expect vectors which are recyclable with
#'   [vctrs::vec_recycle_common()].
#' @rdname gigs_zscoring
#' @export
gigs_waz <- function(weight_kg, age_days, gest_days, sex, id = NULL) {
  validate_waz_params(weight_kg = weight_kg,
                      age_days = age_days,
                      gest_days = gest_days,
                      sex = sex,
                      id = id) |>
    do.call(what = gigs_waz_internal)
}

#' @rdname gigs_zscoring
#' @param lenht_cm A numeric vector of length one or more with length or height
#'   in cm. Should be recumbent length when `age_days < 731`, and standing
#'   height when `age_days >= 731`.
#' @export
gigs_lhaz <- function(lenht_cm, age_days, gest_days, sex, id = NULL) {
  validate_lhaz_params(lenht_cm = lenht_cm,
                       age_days = age_days,
                       gest_days = gest_days,
                       sex = sex,
                       id = id) |>
    do.call(what = gigs_lhaz_internal)
}

#' @rdname gigs_zscoring
#' @export
gigs_wlz <- function(weight_kg, lenht_cm, age_days, gest_days, sex, id = NULL) {
  validate_wlz_params(weight_kg = weight_kg,
                      lenht_cm = lenht_cm,
                      age_days = age_days,
                      gest_days = gest_days,
                      sex = sex,
                      id = id) |>
    do.call(what = gigs_wlz_internal)
}

#' @rdname gigs_zscoring
#' @param headcirc_cm Numeric vector of length one or more with head
#'   circumference in cm.
#' @export
gigs_hcaz <- function(headcirc_cm, age_days, gest_days, sex, id = NULL) {
  validate_hcaz_params(headcirc_cm = headcirc_cm,
                       age_days = age_days,
                       gest_days = gest_days,
                       sex = sex,
                       id = id) |>
    do.call(what = gigs_hcaz_internal)
}

# GIGS z-scoring functions (INTERNAL) ------------------------------------------

#' @inheritParams gigs_waz
#' @noRd
gigs_waz_internal <- function(weight_kg,
                              age_days,
                              gest_days,
                              sex,
                              id = NULL,
                              gigs_lgls = NULL) {
  # Set up PMA
  pma_days <- gest_days + age_days
  pma_weeks <- pma_days / 7
  pma_weeks[!inrange(x = pma_weeks, vec = c(27, 64))] <- NA

  if (is.null(gigs_lgls)) {
    gigs_lgls <- gigs_zscoring_lgls(gest_days = gest_days,
                                    age_days = age_days,
                                    id = id, 
                                    call = rlang::caller_call())
  }
  gigs_lgls <- lapply(X = gigs_lgls, FUN = \(lgl) lgl & !is.na(weight_kg))

  z_ig_nbs <- qnorm(fn_on_subset(ig_nbs_v2c_internal, gigs_lgls[["ig_nbs"]],
                                 weight_kg, gest_days, sex, acronym = "wfga"))
  z_ig_png <- fn_on_subset(ig_png_v2z_internal, gigs_lgls[["ig_png"]],
                           weight_kg, pma_weeks, sex, acronym = "wfa")
  z_who_gs <- fn_on_subset(who_gs_v2z_internal, gigs_lgls[["who_gs"]],
                           weight_kg, age_days, sex, acronym = "wfa")

  z_out <- rep(NA_real_, length(weight_kg))
  z_out[gigs_lgls[["ig_nbs"]]] <- z_ig_nbs
  z_out[gigs_lgls[["ig_png"]]] <- z_ig_png
  z_out[gigs_lgls[["who_gs"]]] <- z_who_gs
  z_out
}

#' @inheritParams gigs_lhaz
#' @noRd
gigs_lhaz_internal <- function(lenht_cm,
                               age_days,
                               gest_days,
                               sex,
                               id = NULL,
                               gigs_lgls = NULL) {
  # Set up PMA
  pma_days <- gest_days + age_days
  pma_weeks <- pma_days / 7
  pma_weeks[!inrange(x = pma_weeks, vec = c(27, 64))] <- NA

  if (is.null(gigs_lgls)) {
    gigs_lgls <- gigs_zscoring_lgls(gest_days = gest_days,
                                    age_days = age_days,
                                    id = id)
  }
  gigs_lgls <- lapply(X = gigs_lgls, FUN = \(lgl) lgl & !is.na(lenht_cm))

  z_ig_nbs <- qnorm(fn_on_subset(ig_nbs_v2c_internal, gigs_lgls[["ig_nbs"]],
                                 lenht_cm, gest_days, sex, acronym = "lfga"))
  z_ig_png <- fn_on_subset(ig_png_v2z_internal, gigs_lgls[["ig_png"]],
                           lenht_cm, pma_weeks, sex, acronym = "lfa")
  z_who_gs <- fn_on_subset(who_gs_v2z_internal, gigs_lgls[["who_gs"]],
                           lenht_cm, age_days, sex, acronym = "lhfa")

  z_out <- rep(NA_real_, length(lenht_cm))
  z_out[gigs_lgls[["ig_nbs"]]] <- z_ig_nbs
  z_out[gigs_lgls[["ig_png"]]] <- z_ig_png
  z_out[gigs_lgls[["who_gs"]]] <- z_who_gs
  z_out
}

#' @inheritParams gigs_wlz
#' @noRd
gigs_wlz_internal <- function(weight_kg,
                              lenht_cm,
                              age_days,
                              gest_days,
                              sex,
                              id = NULL,
                              gigs_lgls = NULL) {
  # Set up PMA
  pma_days <- gest_days + age_days
  pma_weeks <- pma_days / 7
  pma_weeks[!inrange(x = pma_weeks, vec = c(27, 64))] <- NA_real_

  if (is.null(gigs_lgls)) {
    gigs_lgls <- gigs_zscoring_lgls(gest_days = gest_days,
                                    age_days = age_days,
                                    id = id)
  }
  gigs_lgls <- lapply(X = gigs_lgls,
                      FUN = \(lgl) lgl & !(is.na(weight_kg) | is.na(lenht_cm)))


  use_ig_png <- gigs_lgls[["ig_png"]] & inrange(lenht_cm, c(35, 65))
  use_who_wfl <- gigs_lgls[["who_gs"]] & age_days  < 731
  use_who_wfh <- gigs_lgls[["who_gs"]] & age_days >= 731

  z_ig_png <- fn_on_subset(ig_png_v2z_internal,
                           use_ig_png,
                           weight_kg,
                           lenht_cm,
                           sex,
                           acronym = "wfl")
  z_who_gs_wfl <- fn_on_subset(who_gs_v2z_internal,
                               use_who_wfl,
                               weight_kg,
                               lenht_cm,
                               sex,
                               acronym = "wfl")
  z_who_gs_wfh <- fn_on_subset(who_gs_v2z_internal,
                               use_who_wfh,
                               weight_kg,
                               lenht_cm,
                               sex,
                               acronym = "wfh")

  z_out <- rep(NA_real_, length(weight_kg))
  z_out[use_ig_png] <- z_ig_png
  z_out[use_who_wfl] <- z_who_gs_wfl
  z_out[use_who_wfh] <- z_who_gs_wfh
  z_out
}

#' @inheritParams gigs_hcaz
#' @noRd
gigs_hcaz_internal <- function(headcirc_cm,
                               age_days,
                               gest_days,
                               sex,
                               id = NULL,
                               gigs_lgls = NULL) {
  # Set up PMA
  pma_days <- gest_days + age_days
  pma_weeks <- pma_days / 7
  pma_weeks[!inrange(x = pma_weeks, vec = c(27, 64))] <- NA

  if (is.null(gigs_lgls)) {
    gigs_lgls <- gigs_zscoring_lgls(gest_days = gest_days,
                                    age_days = age_days,
                                    id = id)
  }
  gigs_lgls <- lapply(X = gigs_lgls, FUN = \(lgl) lgl & !is.na(headcirc_cm))

  z_ig_nbs <- qnorm(
    fn_on_subset(ig_nbs_v2c_internal, gigs_lgls[["ig_nbs"]], headcirc_cm,
                 gest_days, sex, acronym = "hcfga")
  )
  z_ig_png <- fn_on_subset(ig_png_v2z_internal,
                           gigs_lgls[["ig_png"]],
                           headcirc_cm,
                           pma_weeks,
                           sex,
                           acronym = "hcfa")
  z_who_gs <- fn_on_subset(who_gs_v2z_internal,
                           gigs_lgls[["who_gs"]],
                           headcirc_cm,
                           age_days,
                           sex,
                           acronym = "hcfa")

  z_out <- rep(NA_real_, length(headcirc_cm))
  z_out[gigs_lgls[["ig_nbs"]]] <- z_ig_nbs
  z_out[gigs_lgls[["ig_png"]]] <- z_ig_png
  z_out[gigs_lgls[["who_gs"]]] <- z_who_gs
  z_out
}

# GIGS z-scoring logic ---------------------------------------------------------

#' Get logical vectors describing which standards to apply for an observation
#' using GIGS cut-off logic
#'
#' @description Applies gestational age/PMA cutoffs for term and preterm infants
#'   to generate logical vectors describing where to apply GIGS-compliant
#'   growth standards.
#' @param gest_days Numeric vector of length one or more with gestational ages
#'   in days.
#' @param age_days Numeric vector with of length one or more with ages in days.
#' @param id A factor variable with an ID for each observation. When not `NULL`,
#'   this variable is used to ensure that only the earliest measurement for each
#'   individual is used as a birth measure. If `NULL` (the default), the
#'   function will assume the input data comes from the one individual.
#' @param call A calling environment (default = `rlang::caller_env()`) used to
#'   improve formatting of warnings/errors.  
#' @note The logical vectors returned by this function will have `FALSE`
#'   elements where either `gest_days` or `age_days` is `NA`.
#' @returns Named list with three logical vectors `ig_nbs`, `ig_png`, and
#'   `who_gs`. Where these logical vectors are `TRUE`, then the growth standard
#'   from that named list should be used.
#' @rdname gigs_xaz
#' @noRd
gigs_zscoring_lgls <- function(age_days, 
                               gest_days, 
                               id = NULL, 
                               call = rlang::caller_env()) {
  len_age_days <- length(age_days)
  len_gest_days <- length(gest_days)
  if (len_age_days != len_gest_days) {
    rlang::abort(
      message = c("`age_days` and `gest_days` must have the same length.",
                  "!" = paste0("`age_days` had length ", len_age_days,
                               "; `gest_days` had length ", len_gest_days,
                               ".")),
      class = "gigs_zscoring_inconsistent_lengths",
      .internal = TRUE
    )
  }

  id_provided <- TRUE
  if (is.null(id)) {
    id_provided <- FALSE
    id <- factor(x = rep.int(x = 1, times = len_age_days))
  } else if (!checkmate::test_factor(id)) {
    cli::cli_abort(
      c("Non-factor `id` variable used in `gigs_zscoring_lgls`.",
        "i" = "`id` had class {.cls {class(id)}}."),
      class = "gigs_zscoring_id_not_factor",
      .internal = TRUE
    )
  }

  # Set up vars for use later
  term_cutoff_days <- 37 * 7 # i.e. 37 weeks = term baby
  is_term <- gest_days >= term_cutoff_days
  pma_days <- gest_days + age_days
  pma_weeks <- pma_days / 7

  # Determine which is the `first` measure per ID
  is_first_measurement <- logical(length = len_gest_days)
  for (level in levels(id)) {
    is_curr_level <- id == level
    age_days_subset <- age_days[is_curr_level]
    is_first_measure <- suppressWarnings(
      age_days_subset == min(age_days_subset, na.rm = TRUE)
    )
    is_first_measurement[is_curr_level] <- is_first_measure
  }
  is_first_measurement[is.na(is_first_measurement)] <- FALSE

  is_birth_measure <- is_first_measurement & age_days < 3
  is_inrange_ig_nbs <- inrange(gest_days, c(168, 300))
  is_inrange_ig_png <- inrange(pma_weeks, c(27, 64))
  is_birth_with_large_ga <- is_birth_measure & gest_days > 300

  use_ig_nbs <- is_birth_measure & is_inrange_ig_nbs
  use_ig_png <- !use_ig_nbs & !is_term & is_inrange_ig_png
  use_who_gs <- !use_ig_nbs & is_term | (!is_term & pma_weeks > 64) |
    is_birth_with_large_ga

  is_older_birth <- use_ig_nbs & age_days > 0.5
  if (any(is_older_birth, na.rm = FALSE)) {
    qty_is_older_birth <- sum(is_older_birth)
    msg <- c(paste0("There {cli::qty(qty_is_older_birth)} {?was/were} ",
                    "{.val {qty_is_older_birth}} 'at birth' observation{?s} ",
                    "where {.var age_days} > {.val {0.5}}."))
    if (id_provided) {
      ids_older_birth <- cli::cli_vec(unique(id[is_older_birth]),
                                      style = list("vec-trunc" = 15))
      msg <- c(msg,
               "i" = paste("This occurred for{cli::qty(ids_older_birth)}",
                           "ID{?s} {.val {ids_older_birth}}."))
    }
    cli::cli_warn(msg, class = "gigs_zscoring_old_birth_obs", call = call)
  }
  if (any(is_birth_with_large_ga, na.rm = TRUE)) {
    qty_birth_w_large_ga <- sum(is_birth_with_large_ga)
    msg <- c(paste("There {cli::qty(qty_birth_w_large_ga)}{?was/were}",
                   "{.val {qty_birth_w_large_ga}} birth observation{?s} where ",
                   "{.var gest_age} > {.val {300L}}. The WHO Growth standards ",
                   "were used for {cli::qty(qty_birth_w_large_ga)}",
                   "{?this/these}{cli::qty(qty_birth_w_large_ga)} ",
                   "observation{?s} instead of the INTERGROWTH-21st Newborn ",
                   "Size standards."))
    if (id_provided) {
      ids_birth_w_large_ga <- cli::cli_vec(unique(id[is_birth_with_large_ga]),
                                           style = list("vec-trunc" = 15))
      msg <- c(msg,
               "i" = paste("This occurred for{cli::qty(ids_birth_w_large_ga)}",
                           "ID{?s} {.val {ids_birth_w_large_ga}}."))
    }
    cli::cli_warn(msg, class = "gigs_zscoring_birth_w_large_ga", call = call)
  }

  # Prevents `NAs are not allowed in subscripted assignments` error
  is_na_input <- is.na(age_days) | is.na(gest_days)
  use_ig_nbs[is_na_input] <- FALSE
  use_ig_png[is_na_input] <- FALSE
  use_who_gs[is_na_input] <- FALSE
  is_birth_measure[is_na_input] <- FALSE
  list(birth = is_birth_measure,
       ig_nbs = use_ig_nbs,
       ig_png = use_ig_png,
       who_gs = use_who_gs)
}

# Parameter validation ---------------------------------------------------------

#' Validate inputs to the INTERGROWTH-21st weight-for-age z-scoring function
#' @inheritParams gigs_waz
#' @seealso [classify_wfa()]
#' @noRd
validate_waz_params <- function(weight_kg, age_days, gest_days, sex, id) {
  if (is.null(id)) {
    validate_parameter_lengths(
      weight_kg = weight_kg, age_days = age_days, gest_days = gest_days,
      sex = sex, call = rlang::caller_env()
    )
  } else {
    validate_parameter_lengths(
      weight_kg = weight_kg, age_days = age_days, gest_days = gest_days,
      sex = sex, id = id, call = rlang::caller_env()
    )
  }
  catch_and_throw_validate_issues(expr = {
    id <- validate_id(id)
    sex <- validate_sex(sex)
    gest_days <- validate_numeric(gest_days, varname = "gest_days")
    age_days <- validate_numeric(age_days, varname = "age_days")
    weight_kg <- validate_numeric(weight_kg, varname = "weight_kg")
  }, call = rlang::caller_env())
  vctrs::vec_recycle_common(
    weight_kg = weight_kg,age_days = age_days, gest_days = gest_days,
    sex = sex, id = id
  )
}

#' Validate inputs to the GIGS length-for-age z-scoring function
#' @inheritParams gigs_laz
#' @seealso [classify_stunting()]
#' @noRd
validate_lhaz_params <- function(lenht_cm, age_days, gest_days, sex, id) {
  validate_parameter_lengths(
    lenht_cm = lenht_cm, age_days = age_days, gest_days = gest_days, sex = sex,
    id = id, call = rlang::caller_env()
  )
  catch_and_throw_validate_issues(expr = {
    id <- validate_id(id)
    sex <- validate_sex(sex)
    gest_days <- validate_numeric(gest_days, varname = "gest_days")
    age_days <- validate_numeric(age_days, varname = "age_days")
    lenht_cm <- validate_numeric(lenht_cm, varname = "lenht_cm")
  }, call = rlang::caller_env())
  vctrs::vec_recycle_common(
    lenht_cm = lenht_cm, age_days = age_days, gest_days = gest_days, sex = sex,
    id = id
  )
}

#' Validate inputs to the `gigs_hcaz()`
#' @inheritParams gigs_hcaz
#' @seealso [gigs_hcaz()]
#' @noRd
validate_hcaz_params <- function(headcirc_cm, age_days, gest_days, sex, id) {
  validate_parameter_lengths(headcirc_cm = headcirc_cm, age_days = age_days,
                             gest_days = gest_days, sex = sex, id = id,
                             call = rlang::caller_env())
  catch_and_throw_validate_issues(expr = {
    id <- validate_id(id)
    sex <- validate_sex(sex)
    gest_days <- validate_numeric(gest_days, varname = "gest_days")
    age_days <- validate_numeric(age_days, varname = "age_days")
    headcirc_cm <- validate_numeric(headcirc_cm, varname = "headcirc_cm")
  }, call = rlang::caller_env())
  vctrs::vec_recycle_common(headcirc_cm = headcirc_cm, age_days = age_days,
                            gest_days = gest_days, sex = sex, id = id)
}

#' Validate inputs to the GIGS weight-for-length z-scoring function
#' @inheritParams gigs_wlz
#' @seealso [classify_wasting()]
#' @noRd
validate_wlz_params <- function(weight_kg,
                                lenht_cm,
                                age_days,
                                gest_days,
                                sex,
                                id) {
  validate_parameter_lengths(weight_kg = weight_kg, lenht_cm = lenht_cm,
                             age_days = age_days, gest_days = gest_days,
                             sex = sex, id = id, call = rlang::caller_env())
  catch_and_throw_validate_issues(expr = {
    id <- validate_id(id)
    sex <- validate_sex(sex)
    gest_days <- validate_numeric(gest_days, varname = "gest_days")
    age_days <- validate_numeric(age_days, varname = "age_days")
    lenht_cm <- validate_numeric(lenht_cm, varname = "lenht_cm")
    weight_kg <- validate_numeric(weight_kg, varname = "weight_kg")
  }, call = rlang::caller_env())
  vctrs::vec_recycle_common(weight_kg = weight_kg, lenht_cm = lenht_cm,
                            age_days = age_days, gest_days = gest_days,
                            sex = sex, id = id)
}

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.4a} This file's functions are all documented with `{roxygen2}`.
#' @srrstats {G2.5} It is specifically noted that input factors can be ordered
#'   or unordered.
NULL
