# GROWTH CLASSIFICATION --------------------------------------------------------
#   Take a `data.frame` or similar tabular object, assigns new columns with
#   growth categories, then returns the extended table to the user

#' Classify size-for-gestational age in `data.frame`-like objects with the
#' INTERGROWTH-21<sup>st</sup> weight-for-gestational age standard
#'
#' @param .data A `data.frame`-like tabular object with one or more rows. Must
#'   be permissible by [checkmate::assert_data_frame()], so you can also supply
#'   a `tibble`, `data.table`, or similar.
#' @param weight_kg <[`data-masking`][rlang::args_data_masking]> The name of a
#'   column in `.data` which is a numeric vector of birth weight values in kg.
#'   It is assumed that weight measurements provided to this function are
#'   birth weights recorded <12 hours after an infant's birth.
#' @param gest_days <[`data-masking`][rlang::args_data_masking]> The name of a
#'   column in `.data` which is a numeric vector of gestational age(s) at birth
#'   in days between `168` and `300`. By default, gigs will warn you about
#'   elements of this vector which are outside these bounds, are `NA`, or `Inf`.
#'   You can customise this behaviour using the [GIGS package-level
#'   options][gigs_options].
#' @param sex <[`data-masking`][rlang::args_data_masking]> The name of a
#'   column in `.data` which is a case-sensitive character vector of sexes,
#'   either `"M"` (male) or `"F"` (female). By default, gigs will warn you if
#'   any elements of `sex` are not `"M"` or `"F"`, or are missing (`NA`). You
#'   can customise this behaviour using the [GIGS package-level
#'   options][gigs_options].
#' @param .new A three-length character vector with names for the output
#'   columns. These inputs will be repaired if necessary using
#'   [vctrs::vec_as_names()], which will print any changes to the console. If
#'   any elements in `.new` are the same as elements in `colnames(.data)`, the
#'   function will throw an error. Default = `c("birthweight_centile", "sfga",
#'   "sfga_severe")`.
#' @returns A tabular object of the same class that was provided as `.data`,
#'   with three new columns named according to `.new`. These columns will be
#'   (from left to right):
#'
#'   * `birthweight_centile` - Numeric vector of birthweight centiles from the
#'     INTERGROWTH-21<sup>st</sup> Newborn Size standard for
#'     weight-for-GA
#'   * `sfga` - Factor of size-for-GA categories without severe small-for-GA
#'     classification
#'   * `sfga_severe` - Factor of size-for-GA categories with severe small-for-GA
#'     classification
#' @examples
#' data <- data.frame(
#'  wt_kg = c(2.2, 2.5, 3.3, 4.0),
#'  gestage = 266:269,
#'  sex = c("F", "M", "F", "M")
#' )
#'
#' data |>
#'   classify_sfga(weight_kg = wt_kg,
#'                 gest_days = gestage,
#'                 sex = sex)
#' @note Categorical (factor) columns produced here may contain unused factor
#'   levels. By default, gigs will inform you if these columns have unused 
#'   factor levels. You can change this behaviour using the 
#'   [GIGS package-level option][gigs_options] 
#'   `.gigs_options$handle_unused_levels`.
#' @seealso See [classify_svn()] for size-for-GA classifications which are
#'   stratified by whether a newborn is term. See [classify_growth()] to run
#'   this analysis and others at the same time.
#' @inherit categorise_sfga details references
#' @export
classify_sfga <- function(
    .data,
    weight_kg,
    gest_days,
    sex,
    .new = c("birthweight_centile", "sfga", "sfga_severe")) {
  checkmate::assert_data_frame(.data, min.rows = 1)
  err_if_.new_in_.data(.new = .new, .data_colnames = colnames(.data))
  checkmate::qassert(.new, rules = "S3")

  inputs <- validate_inputs(
    y = eval_tidy(enquo(weight_kg), .data),
    x = eval_tidy(enquo(gest_days), .data),
    sex = eval_tidy(enquo(sex), .data),
    family = "ig_nbs",
    acronym = "wfga",
    yzp_name = "weight_kg",
    x_name = "gest_days"
  )
  p <- do.call(what = ig_nbs_v2c_internal,
                args = inputs[!names(inputs) == "family"])

  .new <- repair_.new_names(.new = list("sfga" = .new), mode = "specific")
  .data[[.new[1]]] <- p
  .data[[.new[2]]] <- categorise_sfga_internal(p, severe = FALSE)
  .data[[.new[3]]] <- categorise_sfga_internal(p, severe = TRUE)
  .data
}

#' Classify small vulnerable newborns in `data.frame`-like objects with the
#' INTERGROWTH-21<sup>st</sup> weight-for-gestational age standard
#'
#' @inherit classify_sfga params note
#' @param .new A two-length character vector with names for the output
#'   columns. These inputs will be repaired if necessary using
#'   [vctrs::vec_as_names()], which will print any changes to the console. If
#'   any elements in `.new` are the same as elements in `colnames(.data)`, the
#'   function will throw an error. Default = `c("birthweight_centile", "svn")`.
#' @returns A tabular object of the same class that was provided as `.data`,
#'   with two new columns named according to `.new`. These columns will be
#'   (from left to right):
#'
#'   * `birthweight_centile` - Numeric vector of birthweight centiles from the
#'     INTERGROWTH-21<sup>st</sup> Newborn Size standard for
#'     weight-for-GA
#'   * `svn` - Factor of small vulnerable newborn (SVN) categories
#' @inherit categorise_svn details
#' @examples
#' data <- data.frame(
#'   wt_kg = c(1.5, 2.6, 2.6, 3.5),
#'   gestage = c(235, 257, 275, 295),
#'   sex = c("F", "M", "F", "M")
#' )
#'
#' data |>
#'   classify_svn(weight_kg = wt_kg,
#'                gest_days = gestage,
#'                sex = sex)
#' @inherit classify_sfga references
#' @seealso See [classify_sfga()] for size-for-GA classifications which are not
#'   stratified by whether a newborn is term. See [classify_growth()] to run
#'   this analysis and others at the same time.
#' @export
classify_svn <- function(.data,
                          weight_kg,
                          gest_days,
                          sex,
                          .new = c("birthweight_centile", "svn")) {
  checkmate::assert_data_frame(.data, min.rows = 1)
  checkmate::qassert(.new, rules = "S2")
  err_if_.new_in_.data(.new = .new, .data_colnames = colnames(.data))

  inputs <- validate_inputs(
    y = eval_tidy(enquo(weight_kg), .data),
    x = eval_tidy(enquo(gest_days), .data),
    sex = eval_tidy(enquo(sex), .data),
    family = "ig_nbs",
    acronym = "wfga",
    yzp_name = "weight_kg",
    x_name = "gest_days"
  )
  p <- do.call(what = ig_nbs_v2c_internal,
                args = inputs[!names(inputs) == "family"])

  .new <- repair_.new_names(.new = list("svn" = .new), mode = "specific")
  .data[[.new[1]]] <- p
  .data[[.new[2]]] <- categorise_svn_internal(
    p, eval_tidy(expr = enquo(gest_days), data = .data)
  )
  .data
}

#' Classify stunting in `data.frame`-like objects with GIGS-recommended growth
#' standards
#'
#' @inherit classify_sfga params note
#' @param lenht_cm <[`data-masking`][rlang::args_data_masking]> The name of a
#'   column in `.data` which is a numeric vector of length/height values in cm.
#' @param age_days <[`data-masking`][rlang::args_data_masking]> The name of a
#'   column in `.data` which is a numeric vector of age values in days.
#' @param gest_days <[`data-masking`][rlang::args_data_masking]> The name of a
#'   column in `.data` which is a numeric vector of gestational age(s) at birth
#'   in days. This column, in conjunction with the column referred to by
#'   `age_days`, is used to select which growth standard to use for each
#'   observation.
#' @param id <[`data-masking`][rlang::args_data_masking]> The name of a
#'   column in `.data` which is a factor variable with IDs for each observation.
#'   When not `NULL`, this variable is used to ensure that only the first
#'   measurement taken from each infant is used as a birth measure. If all your
#'   data is from one individual, leave this parameter as `NULL`. Default =
#'   `NULL`.
#' @param .new A three-length character vector with names for the output
#'   columns. These inputs will be repaired if necessary using
#'   [vctrs::vec_as_names()], which will print any changes to the console. If
#'   any elements in `.new` are the same as elements in `colnames(.data)`, the
#'   function will throw an error. Default = `c("lhaz", "stunting",
#'   "stunting_outliers")`.
#' @returns A tabular object of the same class that was provided as `.data`,
#'   with three new columns named according to `.new`. These columns will be
#'   (from left to right):
#'
#'   * `lhaz` - Numeric vector of length/height-for-age zscores
#'   * `stunting` - Factor of stunting categories without outlier flagging
#'   * `stunting_outliers` - Factor of stunting categories with outlier flagging
#' @inherit categorise_stunting details references
#' @examples
#' # This dummy dataset contains data from two people, from birth (<3 days) to
#' # 500 days of age.
#' data <- data.frame(
#'   child = factor(rep.int(c("A", "B"), c(3, 3))),
#'   agedays = c(0, 100, 500, 2, 100, 500),
#'   gestage  = c(rep(35 * 7, 3), rep(40 * 7, 3)),
#'   sex = rep.int(c("M", "F"), c(3, 3)),
#'   length_cm = rep.int(c(52.2, 60.4, 75), 2)
#' )
#'
#' # Use the `id` argument to ensure that `classify_stunting` uses the correct
#' # standard for each observation
#' data |>
#'   classify_stunting(lenht_cm = length_cm,
#'                     age_days = agedays,
#'                     gest_days = gestage,
#'                     sex = sex,
#'                     id = child)
#'
#' # If you don't specify `id`, `classify_stunting` will assume data is from one
#' # child only
#' data |>
#'   classify_stunting(lenht_cm = length_cm,
#'                     age_days = agedays,
#'                     gest_days = gestage,
#'                     sex = sex)
#' @seealso See [classify_growth()] to run this analysis and others at the same
#'   time. See [gigs_waz()] to learn which growth standard will be used for 
#'   different combinations of gestational/chronological age.
#' @export
classify_stunting <- function(
    .data,
    lenht_cm,
    age_days,
    gest_days,
    sex,
    id = NULL,
    .new = c("lhaz", "stunting", "stunting_outliers")) {
  checkmate::assert_data_frame(.data, min.rows = 1)
  checkmate::qassert(.new, rules = "S3")
  err_if_.new_in_.data(.new = .new, .data_colnames = colnames(.data))

  if (!rlang::quo_is_null(enquo(id))) {
    id <- eval_tidy(enquo(id), data = .data)
  }

  lhaz <- validate_lhaz_params(
    lenht_cm = eval_tidy(enquo(lenht_cm), data = .data),
    age_days = eval_tidy(enquo(age_days), data = .data),
    gest_days = eval_tidy(enquo(gest_days), data = .data),
    sex = eval_tidy(enquo(sex), data = .data),
    id = id
  ) |>
    do.call(what = gigs_lhaz_internal)

  .new <- repair_.new_names(.new = list("stunting" = .new), mode = "specific")
  .data[[.new[1]]] <- lhaz
  .data[[.new[2]]] <- categorise_stunting_internal(lhaz, outliers = FALSE)
  .data[[.new[3]]] <- categorise_stunting_internal(lhaz, outliers = TRUE)
  .data
}

#' Classify wasting in `data.frame`-like objects with GIGS-recommended growth
#' standards
#'
#' @inherit classify_stunting params note
#' @inheritParams classify_sfga
#' @param .new A three-length character vector with names for the output
#'   columns. These inputs will be repaired if necessary using
#'   [vctrs::vec_as_names()], which will print any changes to the console. If
#'   any elements in `.new` are the same as elements in `colnames(.data)`, the
#'   function will throw an error. Default = `c("wlz", "wasting",
#'   "wasting_outliers")`.
#' @returns A tabular object of the same class that was provided as `.data`,
#'   with three new columns named according to `.new`. These columns will be
#'   (from left to right):
#'
#'   * `wlz` - Numeric vector of weight-for-length/height zscores
#'   * `wasting` - Factor of wasting categories without outlier flagging
#'   * `wasting_outliers` - Factor of wasting categories with outlier flagging
#' @inherit categorise_wasting details
#' @inherit categorise_stunting references
#' @inherit classify_stunting seealso
#' @examples
#' # This dummy dataset contains data from two people, from birth (<3 days) to
#' # 500 days of age.
#' data <- data.frame(
#'   child = factor(rep.int(c("A", "B"), c(3, 3))),
#'   agedays = c(0, 100, 500, 2, 100, 500),
#'   gestage  = c(rep(35 * 7, 3), rep(35 * 7, 3)),
#'   sex = rep.int(c("M", "F"), c(3, 3)),
#'   weight_kg = c(3, 6, 9, 3, 6, 9),
#'   length_cm = rep.int(c(52.2, 60.4, 75), 2)
#' )
#'
#' # Use the `id` argument to ensure that `classify_wasting` uses the correct
#' # standard for each observation
#' data |>
#'   classify_wasting(weight_kg = weight_kg,
#'                    lenht_cm = length_cm,
#'                    age_days = agedays,
#'                    gest_days = gestage,
#'                    sex = sex,
#'                    id = child)
#'
#' # If you don't specify `id`, `classify_wasting` will assume data is from one
#' # child only
#' data |>
#'   classify_wasting(weight_kg = weight_kg,
#'                    lenht_cm = length_cm,
#'                    age_days = agedays,
#'                    gest_days = gestage,
#'                    sex = sex)
#' @export
classify_wasting <- function(.data,
                             weight_kg,
                             lenht_cm,
                             age_days,
                             gest_days,
                             sex,
                             id = NULL,
                             .new = c("wlz", "wasting", "wasting_outliers")) {
  checkmate::assert_data_frame(.data, min.rows = 1)
  checkmate::qassert(.new, rules = "S3")
  err_if_.new_in_.data(.new = .new, .data_colnames = colnames(.data))
  
  if (!rlang::quo_is_null(enquo(id))) {
    id <- eval_tidy(enquo(id), .data)
  }
  
  wlz <- validate_wlz_params(
    weight_kg = eval_tidy(enquo(weight_kg), data = .data),
    lenht_cm = eval_tidy(enquo(lenht_cm), data = .data),
    age_days = eval_tidy(enquo(age_days), data = .data),
    gest_days = eval_tidy(enquo(gest_days), data = .data),
    sex = eval_tidy(enquo(sex), data = .data),
    id = id
  ) |>
    do.call(what = gigs_wlz_internal)
  
  .new <- repair_.new_names(.new = list("wasting" = .new), mode = "specific")
  .data[[.new[1]]] <- wlz
  .data[[.new[2]]] <- categorise_wasting_internal(wlz, outliers = FALSE)
  .data[[.new[3]]] <- categorise_wasting_internal(wlz, outliers = TRUE)
  .data
}

#' Classify weight-for-age in `data.frame`-like objects with GIGS-recommended
#' growth standards
#'
#' @inherit classify_stunting params note
#' @inheritParams classify_sfga
#' @param .new A three-length character vector with names for the output
#'   columns. These inputs will be repaired if necessary using
#'   [vctrs::vec_as_names()], which will print any changes to the console. If
#'   any elements in `.new` are the same as elements in `colnames(.data)`, the
#'   function will throw an error. Default = `c("waz", "wfa", "wfa_outliers")`.
#' @returns A tabular object of the same class that was provided as `.data`,
#'   with three new columns named according to `.new`. These columns will be
#'   (from left to right):
#'
#'   * `wlz` - Numeric vector of weight-for-length/height zscores
#'   * `wfa` - Factor of weight-for-age categories without outlier flagging
#'   * `wfa_outliers` - Factor of weight-for-age categories with outlier
#'     flagging
#' @inherit categorise_wfa details
#' @inherit categorise_stunting references
#' @inherit classify_stunting seealso
#' @examples
#' # This dummy dataset contains data from two people, from birth (<3 days) to
#' # 500 days of age.
#' data <- data.frame(
#'   child = factor(rep.int(c("A", "B"), c(3, 3))),
#'   agedays = c(0, 100, 500, 2, 100, 500),
#'   gestage  = c(rep(35 * 7, 3), rep(35 * 7, 3)),
#'   sex = rep.int(c("M", "F"), c(3, 3)),
#'   weight_kg = c(3, 6, 9, 3, 6, 9)
#' )
#'
#' # Use the `id` argument to ensure that `classify_wfa()` uses the correct
#' # standard for each observation
#' data |>
#'   classify_wfa(weight_kg = weight_kg,
#'                age_days = agedays,
#'                gest_days = gestage,
#'                sex = sex,
#'                id = child)
#'
#' # If you don't specify `id`, `classify_wfa()` will assume data is from one
#' # child only
#' data |>
#'   classify_wfa(weight_kg = weight_kg,
#'                age_days = agedays,
#'                gest_days = gestage,
#'                sex = sex)
#' @export
classify_wfa <- function(.data,
                         weight_kg,
                         age_days,
                         gest_days,
                         sex,
                         id = NULL,
                         .new = c("waz", "wfa", "wfa_outliers")) {
  checkmate::assert_data_frame(.data, min.rows = 1)
  checkmate::qassert(.new, rules = "S3")
  err_if_.new_in_.data(.new = .new, .data_colnames = colnames(.data))

  if (!rlang::quo_is_null(enquo(id))) {
    id <- eval_tidy(enquo(id), data = .data)
  }

  waz <- validate_waz_params(
    weight_kg = eval_tidy(enquo(weight_kg), data = .data),
    age_days = eval_tidy(enquo(age_days), data = .data),
    gest_days = eval_tidy(enquo(gest_days), data = .data),
    sex = eval_tidy(enquo(sex), data = .data),
    id = id
  ) |>
    do.call(what = gigs_waz_internal)

  .new <- repair_.new_names(.new = list("wfa" = .new), mode = "specific")
  .data[[.new[1]]] <- waz
  .data[[.new[2]]] <- categorise_wfa_internal(waz, outliers = FALSE)
  .data[[.new[3]]] <- categorise_wfa_internal(waz, outliers = TRUE)
  .data
}

#' Classify head size in `data.frame`-like objects with GIGS-recommended
#' growth standards
#'
#' @inherit classify_stunting params note
#' @inheritParams classify_sfga
#' @param headcirc_cm <[`data-masking`][rlang::args_data_masking]> The name of a
#'   column in `.data` which is a numeric vector of head circumference values in
#'   cm.
#' @param .new A three-length character vector with names for the output
#'   columns. These inputs will be repaired if necessary using
#'   [vctrs::vec_as_names()], which will print any changes to the console. If
#'   any elements in `.new` are the same as elements in `colnames(.data)`, the
#'   function will throw an error. Default = `c("hcaz", "headsize")`.
#' @returns A tabular object of the same class that was provided as `.data`,
#'   with three new columns named according to `.new`. These columns will be
#'   (from left to right):
#'
#'   * `hcaz` - Numeric vector of weight-for-length/height zscores
#'   * `headsize` - Factor of head size categories
#' @inherit categorise_headsize details references
#' @inherit classify_stunting seealso
#' @examples
#' # This dummy dataset contains data from two people, from birth (<3 days) to
#' # 500 days of age.
#' data <- data.frame(
#'   child = factor(rep.int(c("A", "B"), c(3, 3))),
#'   agedays = c(0, 100, 500, 2, 100, 500),
#'   gestage  = c(rep(35 * 7, 3), rep(35 * 7, 3)),
#'   sex = rep.int(c("M", "F"), c(3, 3)),
#'   headcirc_cm = rep.int(c(40, 50, 60), 2)
#' )
#'
#' # Use the `id` argument to ensure that `classify_headsize()` uses the correct
#' # standard for each observation
#' data |>
#'   classify_headsize(headcirc_cm = headcirc_cm,
#'                     age_days = agedays,
#'                     gest_days = gestage,
#'                     sex = sex,
#'                     id = child)
#'
#' # If you don't specify `id`, `classify_headsize()` will assume data is from
#' # one child only
#' data |>
#'   classify_headsize(headcirc_cm = headcirc_cm,
#'                     age_days = agedays,
#'                     gest_days = gestage,
#'                     sex = sex)
#' @export
classify_headsize <- function(.data,
                              headcirc_cm,
                              age_days,
                              gest_days,
                              sex,
                              id = NULL,
                              .new = c("hcaz", "headsize")) {
  checkmate::assert_data_frame(.data, min.rows = 1)
  checkmate::qassert(.new, rules = "S2")
  err_if_.new_in_.data(.new = .new, .data_colnames = colnames(.data))

  if (!rlang::quo_is_null(enquo(id))) {
    id <- eval_tidy(enquo(id), data = .data)
  }

  hcaz <- validate_hcaz_params(
    headcirc_cm = eval_tidy(enquo(headcirc_cm), .data),
    age_days = eval_tidy(enquo(age_days), .data),
    gest_days = eval_tidy(enquo(gest_days), .data),
    sex = eval_tidy(enquo(sex), .data),
    id = id
  ) |>
    do.call(what = gigs_hcaz_internal)

  .new <- repair_.new_names(.new = list("headsize" = .new), mode = "specific")
  .data[[.new[1]]] <- hcaz
  .data[[.new[2]]] <- categorise_headsize_internal(hcaz)
  .data
}

#' Classify multiple growth indicators at the same time using GIGS-recommended
#' growth standards
#'
#' This function permits classification of multiple growth indicators (stunting,
#' wasting, weight-for-age, and more) at once in `data.frame`-like objects.
#'
#' @param weight_kg <[`data-masking`][rlang::args_data_masking]> The name of a
#'   column in `.data` which is a numeric vector of weight values in kg. When
#'   performing size-for-GA and small vulnerable newborn classifications,
#'   centiles and classifications will only be provided where measurements were
#'   taken <12hrs after birth, i.e. `age_days < 0.5`.
#' @inheritParams classify_wasting
#' @inheritParams classify_headsize
#' @param .outcomes A character vector of up to six elements in length
#'   describing which growth outcomes you want to assess. Use this when you
#'   supply data that can be used to generate multiple growth indicators, but
#'   only want to run a specific set. This argument is case-sensitive, and an
#'   error will be thrown if you supply a string in `.analyses` which is either
#'   `NA` or is not a member of this argument's default. Default = `c("sfga",
#'   "svn", "stunting", "wasting", "wfa", "headsize")`.
#' @param .new A list with names corresponding to `.analyses`, which describes
#'   the names of new columns to be added to `.data`. If any elements of the
#'   vectors in `.new` are equal to existing elements of `colnames(.data)`, the
#'   function will throw an error. Excluding the birthweight centiles produced
#'   when getting size-for-gestational age (`"sfga"`) and small vulnerable
#'   newborn (`"svn"`) classifications, all strings in `.new` must be unique.
#'   Each character vector in `.new` is repaired with `vctrs::as_names()`, which
#'   will issue messages if any elements in `.new` are changed.
#'
#'   \describe{
#'     \item{`"sfga"`:}{  `c("birthweight_centile", "sfga", "sfga_severe")`}
#'     \item{`"svn"`:}{  `c("birthweight_centile", "svn")`}
#'     \item{`"stunting"`:}{  `c("lhaz", "stunting", "stunting_outliers")`}
#'     \item{`"wasting"`:}{  `c("wlz", "wasting", "wasting_outliers")`}
#'     \item{`"wfa"`:}{  `c("waz", "wfa", "wfa_outliers")`}
#'     \item{`"headsize"`:}{  `c("hcaz", "headsize")`}
#'   }
#' @param .verbose A single logical value. When `TRUE` (the default),
#'   messages about which analyses were requested versus which were performed
#'   will be printed to the console. Warnings from `classify_growth()` will
#'   still be printed even if `.verbose` is `FALSE`.
#' @note For size-for-GA and small vulnerable newborn analyses, centiles and
#'   categorisations will only be applied for birth measurements. These are
#'   considered to be the observation per level of `id` where `age_days` is
#'   smallest, provided that `age_days` is `<3`.
#'
#'   Categorical (factor) columns produced here may contain unused factor
#'   levels. By default, gigs will inform you if these columns have unused
#'   factor levels. You can change this behaviour using the 
#'   [GIGS package-level option][gigs_options] 
#'   `.gigs_options$handle_unused_levels`.
#' @seealso See [gigs_waz()] to learn which growth standard will be used for 
#'   different combinations of gestational/chronological age.
#' @examples
#' # This dummy dataset contains data from two people, from birth (<3 days) to
#' # 500 days of age.
#' data <- data.frame(
#'   child = factor(rep.int(c("A", "B"), c(3, 3))),
#'   agedays = c(0, 100, 500, 2, 100, 500),
#'   gestage  = c(rep(35 * 7, 3), rep(35 * 7, 3)),
#'   sex = rep.int(c("M", "F"), c(3, 3)),
#'   wt_kg = c(3, 6, 9, 3, 6, 9),
#'   len_cm = rep.int(c(52.2, 60.4, 75), 2),
#'   head_cm = rep.int(c(30, 40, 49), 2)
#' )
#'
#' data_classified <- classify_growth(data,
#'                                    age_days = agedays,
#'                                    gest_days = gestage,
#'                                    weight_kg = wt_kg,
#'                                    lenht_cm = len_cm,
#'                                    headcirc_cm = head_cm,
#'                                    sex = sex,
#'                                    id = child)
#'
#' data_classified
#'
#' # Use `.outcomes` to set which growth indicators will be computed
#' data_svn <- classify_growth(data,
#'                             age_days = agedays,
#'                             gest_days = gestage,
#'                             sex = sex,
#'                             weight_kg = wt_kg,
#'                             id = child,
#'                             .outcomes = "svn")
#'
#' data_svn
#'
#' # Use `.new` to specify new column names
#' data_svn <- classify_growth(data,
#'                             age_days = agedays,
#'                             gest_days = gestage,
#'                             sex = sex,
#'                             weight_kg = wt_kg,
#'                             id = child,
#'                             .outcomes = "svn",
#'                             .new = list("svn" = c("ig_nbs_centile",
#'                                                   "SVN_Category")))
#' data_svn
#'
#' # The function will warn you if you don't provide data for your outcomes
#' data_svn_stunting <- classify_growth(data,
#'                                      age_days = agedays,
#'                                      gest_days = gestage,
#'                                      sex = sex,
#'                                      weight_kg = wt_kg,
#'                                      id = child,
#'                                      .outcomes = c("svn", "stunting"))
#' data_svn_stunting
#' @references
#' WHO. **Physical status: the use and interpretation of anthropometry. Report
#' of a WHO Expert Committee.** *World Health Organisation Technical Report
#' Series 1995,* **854: 1–452**
#'
#' Royal College of Obstetricians and Gynaecologists. **The Investigation and
#' Management of the Small-for-Gestational-Age Fetus: Green-top Guideline No.
#' 31.** *Technical report, Royal College of Obstetricians and Gynaecologists,
#' London, 2013.*
#'
#' Lawn JE, Ohuma EO, Bradley E, Idueta LS, Hazel E, Okwaraji YB et al.
#' **Small babies, big risks: global estimates of prevalence and mortality for
#' vulnerable newborns to accelerate change and improve counting.** *The Lancet*
#' 2023, *401(10389):1707-1719.* \doi{10.1016/S0140-6736(23)00522-6}
#'
#' **'Implausible z-score values'** *in* World Health Organization (ed.)
#' *Recommendations for data collection, analysis and reporting on
#' anthropometric indicators in children under 5 years old*. Geneva: World
#' Health Organization and the United Nations Children's Fund UNICEF, (2019).
#' pp. 64-65.
#'
#' **'Percentage of children stunted, wasted, and underweight, and mean z-scores
#' for stunting, wasting and underweight'** *in* *Guide to DHS Statistics DHS-7*
#' Rockville, Maryland, USA: ICF (2020). pp. 431-435.
#' <https://dhsprogram.com/data/Guide-to-DHS-Statistics/Nutritional_Status.htm>
#'
#' Victora CG, Schuler-Faccini L, Matijasevich A, Ribeiro E, Pessoa A,
#' Barros FC. **Microcephaly in Brazil: how to interpret reported numbers?**
#' *The Lancet* 2016, *387(10019):621-624* \doi{10.1016/S0140-6736(16)00273-7}
#'
#' Accogli A, Geraldo AF, Piccolo G, Riva A, Scala M, Balagura G, et al.
#' **Diagnostic Approach to Macrocephaly in Children**. *Frontiers in
#' Paediatrics* 2022, *9:794069* \doi{10.3389/fped.2021.794069}
#' @returns A tabular object of the same class that was provided as `.data`,
#'   with new columns named according to `.new`. The new columns will depend
#'   on the values supplied for `.outcomes`.
#' @importFrom rlang eval_tidy enquo
#' @export
classify_growth <- function(
  .data,
  gest_days,
  age_days,
  sex,
  weight_kg = NULL,
  lenht_cm = NULL,
  headcirc_cm = NULL,
  id = NULL,
  .outcomes = c("sfga", "svn", "stunting", "wasting", "wfa", "headsize"),
  .new = list(
      sfga = c("birthweight_centile", "sfga", "sfga_severe"),
      svn = c("birthweight_centile", "svn"),
      stunting = c("lhaz", "stunting", "stunting_outliers"),
      wasting = c("wlz", "wasting", "wasting_outliers"),
      wfa = c("waz", "wfa", "wfa_outliers"),
      headsize = c("hcaz", "headsize")
    ),
  .verbose = TRUE) {

  # All possible analyses
  all_analyses <- c("sfga", "svn", "stunting", "wasting", "wfa", "headsize")
  all_analyses_len <- length(all_analyses)
  # Which analyses have been run
  outcomes_run <- logical(length = all_analyses_len) |>
    setNames(all_analyses)

  # Variable checking - basic assertions on length/type
  checkmate::assert_data_frame(.data, min.rows = 1)
  checkmate::assert_character(.outcomes, any.missing = FALSE, min.len = 1,
                              max.len = all_analyses_len)
  checkmate::assert_subset(.outcomes, choices = all_analyses)
  checkmate::assert_list(.new, any.missing = FALSE, min.len = 1,
                         max.len = all_analyses_len)
  checkmate::qassert(x = .verbose, rules = "B1")

  # Sanity checks: Integrity of `.data`, `.outcomes`, and `.new`
  err_if_.new_in_.data(.new = unique(unlist(.new)),
                       .data_colnames = colnames(.data))
  check_all_.new_names_valid(.new = .new, all = all_analyses)
  check_all_.outcomes_in_.new(.new = .new, .outcomes = .outcomes)
  .new <- .new[.outcomes]
  check_.new_vector_lengths(.new)
  .new <- check_sfga_svn_centile_colname(.new)
  check_if_.new_elements_unique(.new)
  .new <- repair_.new_names(.new)

  if (rlang::quo_is_null(enquo(id))) {
    id <- factor(rep.int("A", nrow(.data)))
  } else {
    id <- eval_tidy(enquo(id), .data)
  }

  # Move passed-in data to new `growth_data` object - and throw warnings/errors
  # out based on users' GIGS options
  catch_and_throw_validate_issues(expr = {
    growth_data <- data.frame(
      "gest_days" = validate_numeric(eval_tidy(enquo(gest_days), .data),
                                     varname = "gest_days"),
      "age_days" = validate_numeric(eval_tidy(enquo(age_days), .data),
                                    varname = "age_days"),
      "sex" = validate_sex(eval_tidy(enquo(sex), .data)),
      "id" = validate_id(id)
    )

    # What data was provided? Store data in `growth_data` where possible
    missing_weight <- rlang::quo_is_null(enquo(weight_kg))
    missing_lenht <- rlang::quo_is_null(enquo(lenht_cm))
    missing_headcirc <- rlang::quo_is_null(enquo(headcirc_cm))
    if (!missing_weight) {
      growth_data[["weight_kg"]] <- validate_numeric(
        eval_tidy(enquo(weight_kg), .data), varname = "weight_kg"
      )
    }
    if (!missing_lenht) {
      growth_data[["lenht_cm"]] <- validate_numeric(
        eval_tidy(enquo(lenht_cm), .data), varname = "lenht_cm"
      )
    }
    if (!missing_headcirc) {
      growth_data[["headcirc_cm"]] <- validate_numeric(
        eval_tidy(enquo(headcirc_cm), .data), varname = "headcirc_cm"
      )
    }
  }, call = rlang::current_env())

  # Cache gigs logicals --> saves a fair chunk of time as the input dataset gets
  # bigger
  cached_gigs_lgls <- with(growth_data,
                           gigs_zscoring_lgls(age_days = age_days,
                                              gest_days = gest_days,
                                              id = id, 
                                              call = rlang::current_env()))

  # Compute outcomes as provided data allows
  for (outcome in .outcomes) {
    if (outcome == "sfga" | outcome == "svn") {
      if (missing_weight) next
      bweight_centile_not_calculated <- !.new[[outcome]][1] %in% names(.data)
      if (bweight_centile_not_calculated) {
        bw_centile <- rep(NA_real_, nrow(.data))
        bw_centile[cached_gigs_lgls$ig_nbs] <- fn_on_subset(
          fn = ig_nbs_v2c_internal,
          lgl = cached_gigs_lgls$ig_nbs,
          growth_data[["weight_kg"]],
          growth_data[["gest_days"]],
          growth_data[["sex"]],
          acronym = "wfga"
        )
        is_birth_who_gs <- cached_gigs_lgls$birth & cached_gigs_lgls$who_gs
        bw_centile[is_birth_who_gs] <- pnorm(fn_on_subset(
          fn = who_gs_v2z_internal,
          lgl = is_birth_who_gs,
          growth_data[["weight_kg"]],
          growth_data[["gest_days"]],
          growth_data[["sex"]],
          acronym = "wfga"
        ))
        .data[[.new[[outcome]][1]]] <- bw_centile
      }
      if (outcome == "sfga") {
        .data[[.new[[outcome]][2]]] <- categorise_sfga_internal(
          p = bw_centile, severe = FALSE
        )
        .data[[.new[[outcome]][3]]] <- categorise_sfga_internal(
          p = bw_centile, severe = TRUE
        )
      } else {
        .data[[.new[[outcome]][2]]] <- categorise_svn_internal(
          p = bw_centile, gest_days = growth_data[["gest_days"]]
        )
      }
    }

    if (outcome == "stunting") {
      if (missing_lenht) next
      lhaz <- with(growth_data,
                   gigs_lhaz_internal(lenht_cm = lenht_cm, age_days = age_days,
                                      gest_days = gest_days, sex = sex,
                                      id = id, gigs_lgls = cached_gigs_lgls))
      .data[[.new[[outcome]][1]]] <- lhaz
      .data[[.new[[outcome]][2]]] <- categorise_stunting_internal(lhaz, FALSE)
      .data[[.new[[outcome]][3]]] <- categorise_stunting_internal(lhaz, TRUE)
    }

    if (outcome == "wasting") {
      if (missing_weight | missing_lenht) next
      wlz <- with(growth_data,
                   gigs_wlz_internal(weight_kg = weight_kg,
                                     lenht_cm = lenht_cm, age_days = age_days,
                                     gest_days = gest_days, sex = sex,
                                     id = id, gigs_lgls = cached_gigs_lgls))
      .data[[.new[[outcome]][1]]] <- wlz
      .data[[.new[[outcome]][2]]] <- categorise_wasting_internal(wlz, FALSE)
      .data[[.new[[outcome]][3]]] <- categorise_wasting_internal(wlz, TRUE)
    }

    if (outcome == "wfa") {
      if (missing_weight) next
      waz <- with(growth_data,
                  gigs_waz_internal(weight_kg = weight_kg, age_days = age_days,
                                    gest_days = gest_days, sex = sex,
                                    id = id, gigs_lgls = cached_gigs_lgls))
      .data[[.new[[outcome]][1]]] <- waz
      .data[[.new[[outcome]][2]]] <- categorise_wfa_internal(waz, FALSE)
      .data[[.new[[outcome]][3]]] <- categorise_wfa_internal(waz, TRUE)
    }

    if (outcome == "headsize") {
      if (missing_headcirc) next
      hcaz <- with(growth_data,
                   gigs_hcaz_internal(headcirc_cm = headcirc_cm,
                                      age_days = age_days,
                                      gest_days = gest_days,
                                      sex = sex, id = id,
                                      gigs_lgls = cached_gigs_lgls))
      .data[[.new[[outcome]][1]]] <- hcaz
      .data[[.new[[outcome]][2]]] <- categorise_headsize_internal(hcaz)
    }

    outcomes_run[outcome] <- TRUE
  }

 if (.verbose) {
    msg <- msg_classify_growth(all = all_analyses,
                               requested = .outcomes,
                               run = outcomes_run,
                               missing = c(weight_kg = missing_weight,
                                           lenht_cm = missing_lenht,
                                           headcirc_cm = missing_headcirc))
   cli::cli_h1(text = "{.fun gigs::classify_growth}")
   cli::cli_inform(msg, class = "gigs_classify_growth_msg")
  }
  .data
}

# INTERNAL; checkers for all growth_classify.R functions -----------------------

#' For `classify_*() functions`:Throw an error if elements of vectors in `.new`
#' are in `.data`.
#' @param .new A character vector of length one or more with new column names to
#'   check against `.data`.
#' @param .data_colnames A character vector of length one or more with column
#'   names to be checked against `.new`.
#' @return Returns `.new` invisibly if there are no matches between `new` and
#'   `existing`, else throws an error.
#' @noRd
err_if_.new_in_.data <- function(.new, .data_colnames) {
  matches <- .new %in% .data_colnames
  any_matches <- any(matches)
  if (any_matches) {
    matched_names <- paste0("`", .new[matches], "`") |>
      setNames(nm = rep.int("!", length(.new[matches])))
    rlang::abort(
      message =
        c(paste0("Column names requested in `.new` already exist in `.data`. ",
                 "These are:"),
          matched_names
        ),
      call = rlang::env_parent(),
      class = "gigs_classify_.new_in_.data")
  }
  invisible(.new)
}



# INTERNAL; checkers for classify_growth() -------------------------------------

#' For `classify_growth()`: Check that the names of vectors in `.new` map onto
#' the range of available analyses (i.e. `all`).
#' @inheritParams classify_growth
#' @returns Invisibly returns `.new`, unchanged.
#' @noRd
check_all_.new_names_valid <- function(.new, all) {
  .new_names <- names(.new)
  lgl_invalid_.new_name <- !.new_names %in% all
  if (any(lgl_invalid_.new_name)) {
    rlang::abort(
      c(
        "Some elements in `names(.new)` are not valid.",
        "!" = paste0("Bad names in `.new`: ", paste_sep_commas_quoted(
          .new_names[lgl_invalid_.new_name])
        ),
        "i" = paste0("Each name in `.new` must be one of: ",
                     paste_sep_commas_quoted(all), ".")
      ),
      call = rlang::expr(gigs::classify_growth()),
      class = "gigs_classify_growth_invalid_.new_names"
    )
  }
  invisible(.new)
}

#' For `classify_growth()`: check that all requested outcomes in `.outcomes`
#' have named vectors with column names in `.new`.
#' @inheritParams classify_growth
#' @returns @returns Invisibly returns `.new`, unchanged.
#' @noRd
check_all_.outcomes_in_.new <- function(.outcomes, .new) {
  lgl_.analyses_in_.new <- .outcomes %in% names(.new)
  if (!all(lgl_.analyses_in_.new)) {
    rlang::abort(
      c("Elements of `.outcomes` are missing new column names in `.new`:",
        "!" = paste0("`.new` must have column names for ",
                     paste_sep_commas(.outcomes[!lgl_.analyses_in_.new]))),
      call = rlang::expr(gigs::classify_growth()),
      class = "gigs_classify_growth_.outcomes_not_in_.new"
    )
  }
  invisible(.new)
}

#' Check whether vectors in `.new` are the correct length, given the number of
#' new columns to be generated
#' @inheritParams classify_growth
#' @returns Invisibly returns `.new`, unchanged.
#' @note This function is a sanity check - for `classify_growth()`, for example,
#'   running a head size analysis should only add two columns - one for HCAZ and
#'   one with head size categories. Users can specify column names, but should
#'   supply one name for each new column - which this function enforces.
#' @noRd
check_.new_vector_lengths <- function(.new) {
  .new_names <- names(.new)
  msgs <- vector(length = length(.new), mode = "list") |>
    setNames(.new_names)
  for (idx in seq_along(.new)) {
    curr_vec <- .new[[idx]]
    curr_name <- .new_names[idx]
    expected_len <- switch(curr_name, sfga = 3, svn = 2, wasting = 3,
                           stunting = 3, wfa = 3, headsize = 2)
    check <- checkmate::check_character(
      curr_vec,
      any.missing = FALSE,
      len = expected_len
    )
    if (is.character(check)) {
      msgs[[curr_name]] <- check
    }
  }
  msgs <- vctrs::list_drop_empty(msgs)
  if (length(msgs) > 0) {
    msg <- setNames(paste0("`.new[[\"", names(msgs), "\"]]`: ", msgs, "."),
                    rep.int("i", length(msgs)))
    rlang::abort(
      message = c("Some vectors in `.new` have the wrong length:", msg),
      call = rlang::expr(gigs::classify_growth()),
      class = "gigs_classify_growth_.new_lengths_incorrect"
    )
  }
  invisible(.new)
}

#' Check whether the 'birthweight centile' column name is the same for
#' size-for-GA and small vulnerable newborns
#' @inheritParams classify_growth
#' @returns Returns `.new` unchanged if the column name for birthweight centiles
#'   is the same in `.new[["sfga"]]` and `.new[["svn"]]`, else returns `.new`
#'   with the column name for birthweight centiles the same as in
#'   `.new[["sfga"]]`.
#' @noRd
check_sfga_svn_centile_colname <- function(.new) {
  if (all(c("sfga", "svn") %in% names(.new))) {
    bweight_centiles_sfga <- .new[["sfga"]][1]
    bweight_centiles_svn <- .new[["svn"]][1]
    if (bweight_centiles_sfga != bweight_centiles_svn) {
      rlang::warn(
        message = c(paste0("User-supplied column names for birthweight ",
                           "centiles in size-for-GA and SVN categorisations ",
                           "did not match."),
                    "*" = paste0("Size-for-GA: `", bweight_centiles_sfga, "`."),
                    "*" = paste0("SVN:         `", bweight_centiles_svn, "`."),
                    "i" = paste0("`classify_growth()` will use the name ",
                                 "provided for size-for-GA (`\"",
                                 bweight_centiles_sfga, "\"`).")),
        call = rlang::expr(gigs::classify_growth()),
        class = "gigs_classify_growth_bweight_centile_not_identical"
      )
      .new[["svn"]][1] <- bweight_centiles_sfga
    }
  }
  .new
}

#' Check if elements of `.new` are unique, and abort if not
#'
#' @inheritParams classify_growth
#' @details This function is used to ensure that all new column names requested
#'   by a user in `classify_growth()` are unique.
#' @return Return its input (`.new`) unchanged, unless any of the elements of
#'   its vectors were non-unique.
#' @noRd
check_if_.new_elements_unique <- function(.new) {
  .new_names <- names(.new)
  chr_unlisted <- unlist(.new)
  lgl_duplicated <- duplicated(chr_unlisted)
  limit <- if (all(c("sfga", "svn") %in% .new_names)) 1L else 0L
  if (sum(as.integer(lgl_duplicated)) <= limit) {
    return(invisible(.new))
  }
  chr_duplicated <- chr_unlisted[lgl_duplicated]
  # Exclude sfga/svn bweight centile name from the set of bad duplicates
  if (all(c("sfga", "svn") %in% .new_names)) {
    chr_duplicated <- chr_duplicated[chr_duplicated != .new[["sfga"]][1]]
  }

  msgs <- vapply(
    X = chr_duplicated,
    FUN.VALUE = character(length = 1L),
    FUN = \(current_duplicate) {
      present_in <- vapply(X = .new_names,
                           FUN.VALUE = logical(length = 1),
                           FUN = \(name) {
                             current_duplicate %in% .new[[name]]
                           })
      present_in <- paste0("new[[\"", .new_names[present_in], "\"]]")
      paste0("`\"", current_duplicate, "\"`: Present in ",
             paste_sep_commas(present_in), ".")
    }) |>
    setNames(rep.int("!", length(chr_duplicated)))

  rlang::abort(
    c("Some elements in `.new` are not unique:", msgs),
    call = rlang::caller_env(),
    class = "gigs_classify_growth_.new_elements_not_unique"
  )
}

#' Repair new names requested by users, and inform them if they need repair
#'
#' @inheritParams classify_growth
#' @param mode A single-length character vector specifying how the function is
#'   being called. If called for `"classify_growth"` (the default), repaired
#'   names will be reported as part of a list. If called for a `"specific"`
#'   growth function (e.g. `classify_stunting()`), the print output will be more
#'   suitable to a vector of inputs.
#' @details This function is used to ensure that user-requested column names
#'   by a user in `classify_*()` functions are valid (i.e. syntactic, minimal,
#'   and unique).
#' @return Return its input (`.new`) unchanged, unless any of the elements of
#'   its vectors were not syntactic, in which case these elements are replaced
#'   with the output from `vctrs::vec_as_names()`.
#' @returns A list vector the same length as `.new`, repaired if necessary with
#'   `vctrs::vec_as_names()`.
#' @noRd
repair_.new_names <- function(.new, mode = "classify_growth") {
  if (!mode %in% c("classify_growth", "specific")) {
    rlang::abort(
      c("!" = paste0("{.arg mode} should be one of `\"classify_growth\"`",
                     "or `\"specific\".`")),
      call = rlang::caller_env(),
      class = "gigs_internal_repair_mode_incorrect",
      .internal = TRUE)
  }
  .new_names <- names(.new)
  chr_OLD <- unlist(.new)
  chr_NEW <- vctrs::vec_as_names(chr_OLD, repair = "universal_quiet")
  if (all(c("svn", "sfga") %in% .new_names)) {
    chr_NEW["sfga1"] <- chr_OLD["sfga1"]
    chr_NEW["svn1"] <- chr_OLD["sfga1"]
  }
  renaming_info <- vector(mode = "list", length(.new_names))
  for (idx in seq_along(.new)) {
    analysis <- .new_names[idx]
    old_names <- .new[[idx]]
    names_len <- length(old_names)
    new_names <- rep_len(x = NA_character_, length.out = names_len)
    for (i in seq_len(names_len)) {
      new_names[i] <- chr_NEW[sprintf("%s%i", analysis, i)]
    }
    was_renamed <- old_names != new_names
    if (!any(was_renamed)) {
      next
    }
    .new[[idx]] <- new_names
    bullets <- paste0("`", old_names[was_renamed], "` -> `",
                      new_names[was_renamed], "`") |>
      setNames(rep_len("i", length.out = sum(was_renamed)))
    renaming_info[[idx]] <- c(
      "*" = sprintf(fmt = "In `.new[[\"%s\"]]`:", analysis), bullets
    )
  }
  li_renamed <- vctrs::list_drop_empty(renaming_info)
  if (length(li_renamed) != 0) {
    renamed <- unlist(li_renamed)
    if (mode == "specific") {
      renamed <- renamed[-1]
    }
    rlang::warn(
      c("Column names in `.new` repaired by `rlang::vec_as_names()`:", renamed),
      class = "gigs_repaired_names_in_.new",
      call = rlang::caller_env()
    )
  }

  if (mode == "specific") {
    .new[[1]]
  } else {
    .new
  }
}

#' Build a character vector describing which outcomes in `classify_growth()`
#' were computed, formatted for `rlang::inform()`
#'
#' @param all A character vector denoting all growth analyses offered by GIGS.
#'   Equivalent to the default argument of `.analyses` in [classify_growth()].
#' @param requested A character vector of up to six elements describing which
#'   growth analyses a user wanted to run.
#' @param run A character vector of up to six elements describing which growth
#'   analyses indicated by `requested` were run.
#' @return A character vector the same length as `requested` describing which
#'   growth indicators were (not) computed.
#' @noRd
msg_classify_growth <- function(all, requested, run, missing) {
  analysis_msg_strings <- c("Size-for-gestational age",
                            "Small vulnerable newborns", "Stunting", "Wasting",
                            "Weight-for-age", "Head size") |>
    setNames(all)
  analysis_msg_strings <- analysis_msg_strings[requested]
  msg_strings <- rep_len(x = NA_character_, length(requested))
  for (idx in seq_along(requested)) {
    chr_analysis <- requested[idx]
    lgl_was_run <- run[chr_analysis]
    if (lgl_was_run) {
      chr_was_run <- "Success"
    } else {
      if (chr_analysis != "wasting") {
        chr_was_run <- switch(
          chr_analysis,
          stunting = "Not computed (`lenht_cm` not supplied)",
          headsize = "Not computed (`headcirc_cm` not supplied)",
          "Not computed (`weight_kg` not supplied)")
      } else {
        if (!missing["weight_kg"]) {
          chr_was_run <- "Not computed (`lenht_cm` not supplied)"
        } else if (!missing["lenht_cm"]) {
          chr_was_run <- "Not computed (`weight_kg` not supplied)"
        } else {
          chr_was_run <- "Not computed (`weight_kg`, `lenht_cm` not supplied)"
        }
      }
    }
    names(msg_strings)[idx] <- if (lgl_was_run) "v" else "!"
    msg_strings[idx] <- paste0(analysis_msg_strings[chr_analysis], ": ",
                               chr_was_run)
  }
  msg_strings
}

# SRR tags ---------------------------------------------------------------------
#' @srrstats {G1.0} Primary literature referenced where relevant for each
#'   function in this script.
#' @srrstats {G1.4} This file's functions are all documented with `{roxygen2}`.
#' @srrstats {G2.0} Each exported function passes its inputs to
#'   `validate_*()` functions, which enforce specific input lengths.
#' @srrstats {G2.0a, EA1.3} Documentation in this script explicitly references
#'   length of inputs. Input lengths are explicitly referenced in the
#'   documentation and checked by `validate_*()` functions for each exported
#'   function.
#' @srrstats {G2.1, G2.1a, G2.2} Documentation explicitly notes expected data
#'   types, lengths, and expected values for univariate input.
#' @srrstats {G2.3, G2.3a, G2.3b} Univariate character inputs are restricted to
#'   specific inputs by `{checkmate}` calls; these are case-sensitive and
#'   documented as such.
#' @srrstats {G2.7} We use `checkmate::assert_data_frame()` to check that the
#'   supplied `.data` objects inherit from `data.frame`, allowing the use of
#'   `tibble`s, `data.table`s, and more.
#' @srrstats {G2.8, G2.12} The package passes columns to various GIGS-specific
#'   `validate_*()` functions. These ensure that passed columns are atomic and
#'   have the right type. This behaviour inherently restricts a user from
#'   supplying list-columns as input to the function.
#' @srrstats {G2.9} Users can select names to apply to new columns, which are
#'   fixed by `vctrs::as_names()`. This function prints messages to the console
#'   if any of the names supplied by the user require changing.
#' @srrstats {G2.11} In this script, all vector columns are passed to functions
#'   which strip attributes - this lets `units`-like non-standard vector columns
#'   work in these functions.
#' @srrstats {G2.14, G2.14a, G2.14b} Missing/undefined data checks are
#'   performed and their behaviour can be customised using [gigs_option_set()].
#' @srrstats {EA3.0, EA3.1} The `classify_growth()` function automates growth
#'   standard selection for end users, which (as far as we have found) was not
#'   previously available to end-users.
NULL
