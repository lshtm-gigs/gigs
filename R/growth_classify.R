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
#' @seealso [ig_nbs_wfga_value2centile()], which this function uses to get
#'   a centile for each observation.
#' @examples
#' data <- data.frame(
#'  wtkg = c(2.2, 2.5, 3.3, 4.0),
#'  gestage = 266:269,
#'  sex = c("F", "M", "F", "M")
#' )
#'
#' data |>
#'   classify_sfga(weight_kg = wtkg,
#'                 gest_days = gestage,
#'                 sex = sex)
#' @inherit categorise_sfga details references
#' @export
classify_sfga <- function(
    .data,
    weight_kg,
    gest_days,
    sex,
    .new = c("birthweight_centile", "sfga", "sfga_severe")) {
  checkmate::assert_data_frame(.data, min.rows = 1)
  err_if_.new_in_.data(new = .new, .data_colnames = colnames(.data))
  checkmate::qassert(.new, rules = "S3")

  p <- ig_nbs_wfga_value2centile(
    weight_kg = eval_tidy(enquo(weight_kg), .data),
    gest_days = eval_tidy(enquo(gest_days), .data),
    sex = eval_tidy(enquo(sex), .data)
  )

  .new <- vctrs::vec_as_names(.new, repair = "universal")
  .data[[.new[1]]] <- p
  .data[[.new[2]]] <- categorise_sfga_internal(p, severe = FALSE)
  .data[[.new[3]]] <- categorise_sfga_internal(p, severe = TRUE)
  .data
}

#' Classify small vulnerable newborns in `data.frame`-like objects with the
#' INTERGROWTH-21<sup>st</sup> weight-for-gestational age standard
#'
#' @inherit classify_sfga params
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
#'   wtkg = c(1.5, 2.6, 2.6, 3.5),
#'   gestage = c(235, 257, 275, 295),
#'   sex = c("F", "M", "F", "M")
#' )
#'
#' data |>
#'   classify_svn(weight_kg = wtkg,
#'                gest_days = gestage,
#'                sex = sex)
#' @seealso [ig_nbs_wfga_value2centile()], which this function uses to get
#'   a centile for each observation.
#' @inherit classify_sfga references
#' @export
classify_svn <- function(.data,
                          weight_kg,
                          gest_days,
                          sex,
                          .new = c("birthweight_centile", "svn")) {
  checkmate::assert_data_frame(.data, min.rows = 1)
  checkmate::qassert(.new, rules = "S2")
  err_if_.new_in_.data(new = .new, .data_colnames = colnames(.data))
  p <- ig_nbs_wfga_value2centile(
    weight_kg = eval_tidy(enquo(weight_kg), .data),
    gest_days = eval_tidy(enquo(gest_days), .data),
    sex = eval_tidy(enquo(sex), .data)
  )

  .new <- vctrs::vec_as_names(.new, repair = "universal")
  .data[[.new[1]]] <- p
  .data[[.new[2]]] <- categorise_svn_internal(
    p, eval_tidy(expr = enquo(gest_days), data = .data)
  )
  return(.data)
}

#' Classify stunting in `data.frame`-like objects with GIGS-recommended growth
#' standards
#'
#' @param weight_kg <[`data-masking`][rlang::args_data_masking]> The name of a
#'   column in `.data` which is a numeric vector of weight values in kg.
#' @param lenht_cm <[`data-masking`][rlang::args_data_masking]> The name of a
#'   column in `.data` which is a numeric vector of length/height values in cm.
#' @param age_days <[`data-masking`][rlang::args_data_masking]> The name of a
#'   column in `.data` which is a numeric vector of age values in days.
#' @param gest_days <[`data-masking`][rlang::args_data_masking]> The name of a
#'   column in `.data` which is a numeric vector of gestational age(s) at birth
#'   in days. This column, in conjunction with the column referred to by
#'   `age_days`, is used to select which growth standard to use for each
#'   observation.
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
#' # The first observation uses the INTERGROWTH-21st Postnatal Growth standards;
#' # the next two use the WHO Child Growth Standards.
#' data <- data.frame(
#'   length_cm = c(52.2, 75.4, 63.1),
#'   agedays = c(357, 375, 250),
#'   gestage  = c(196, 287, 266),
#'   sex = c("M", "M", "F")
#' )
#'
#' data |>
#'   classify_stunting(lenht_cm = length_cm,
#'                     age_days = agedays,
#'                     gest_days = gestage,
#'                     sex = sex)
#' @export
classify_stunting <- function(
    .data,
    lenht_cm,
    age_days,
    gest_days,
    sex,
    .new = c("lhaz", "stunting", "stunting_outliers")) {
  checkmate::assert_data_frame(.data, min.rows = 1)
  checkmate::qassert(.new, rules = "S3")
  err_if_.new_in_.data(new = .new, .data_colnames = colnames(.data))

  lhaz <- gigs_lhaz(
    lenht_cm = eval_tidy(enquo(lenht_cm), .data),
    age_days = eval_tidy(enquo(age_days), .data),
    gest_days = eval_tidy(enquo(gest_days), .data),
    sex = eval_tidy(enquo(sex), .data)
  )

  .new <- vctrs::vec_as_names(.new, repair = "universal")
  .data[[.new[1]]] <- lhaz
  .data[[.new[2]]] <- categorise_stunting_internal(lhaz, outliers = FALSE)
  .data[[.new[3]]] <- categorise_stunting_internal(lhaz, outliers = TRUE)
  .data
}

#' Classify wasting in `data.frame`-like objects with GIGS-recommended growth
#' standards
#'
#' @inheritParams classify_stunting
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
#' @examples
#' # Returns factor with stunting classifications
#' data <- data.frame(
#'   wtkg = c(5.75, 2.18, 3.00, 6.75),
#'   length_height = c(67.7, 46.6, 50.0, 80.1),
#'   gestage = c(251, 197, 225, 243),
#'   age = c(251, 197, 225, 243),
#'   sex =  c("F", "M", "F", "M")
#' )
#' data |>
#'   classify_wasting(weight_kg = wtkg,
#'                    lenht_cm = length_height,
#'                    gest_days = gestage,
#'                    age_days = age,
#'                    sex = sex)
#' @export
classify_wasting <- function(.data,
                             weight_kg,
                             lenht_cm,
                             age_days,
                             gest_days,
                             sex,
                             .new = c("wlz", "wasting", "wasting_outliers")) {
  checkmate::assert_data_frame(.data, min.rows = 1)
  checkmate::qassert(.new, rules = "S3")
  err_if_.new_in_.data(new = .new, .data_colnames = colnames(.data))

  wlz <- gigs_wlz(
    weight_kg = eval_tidy(enquo(weight_kg), .data),
    lenht_cm = eval_tidy(enquo(lenht_cm), .data),
    age_days = eval_tidy(enquo(age_days), .data),
    gest_days = eval_tidy(enquo(gest_days), .data),
    sex = eval_tidy(enquo(sex), .data)
  )

  .new <- vctrs::vec_as_names(.new, repair = "universal")
  .data[[.new[1]]] <- wlz
  .data[[.new[2]]] <- categorise_wasting_internal(wlz, outliers = FALSE)
  .data[[.new[3]]] <- categorise_wasting_internal(wlz, outliers = TRUE)
  .data
}

#' Classify weight-for-age in `data.frame`-like objects with GIGS-recommended
#' growth standards
#'
#' @inheritParams classify_stunting
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
#' @examples
#' data <- data.frame(
#'   wtkg = c(7.2, 4.5, 9.1, 24),
#'   age = c(401, 185, 101, 607),
#'   gestage = 7 * c(27, 36, 40, 41),
#'   sex = c("F", "M", "F", "M")
#' )
#' data |>
#'   classify_wfa(weight_kg = wtkg,
#'                age_days = age,
#'                gest_days = gestage,
#'                sex = sex)
#' @export
classify_wfa <- function(.data,
                         weight_kg,
                         age_days,
                         gest_days,
                         sex,
                         .new = c("waz", "wfa", "wfa_outliers")) {
  checkmate::assert_data_frame(.data, min.rows = 1)
  checkmate::qassert(.new, rules = "S3")
  err_if_.new_in_.data(new = .new, .data_colnames = colnames(.data))

  waz <- gigs_waz(
    weight_kg = eval_tidy(enquo(weight_kg), data = .data),
    age_days = eval_tidy(enquo(age_days), data = .data),
    gest_days = eval_tidy(enquo(gest_days), data = .data),
    sex = eval_tidy(enquo(sex), data = .data)
  )

  .new <- vctrs::vec_as_names(.new, repair = "universal")
  .data[[.new[1]]] <- waz
  .data[[.new[2]]] <- categorise_wfa_internal(waz, outliers = FALSE)
  .data[[.new[3]]] <- categorise_wfa_internal(waz, outliers = TRUE)
  .data
}

#' Classify head size in `data.frame`-like objects with GIGS-recommended
#' growth standards
#'
#' @inheritParams classify_stunting
#' @inheritParams classify_sfga
#' @param headcirc_cm <[`data-masking`][rlang::args_data_masking]> The name of a
#'   column in `.data` which is a numeric vector of head circumference values in
#'   cm.
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
#' @inherit categorise_headsize details references
#' @examples
#' data <- data.frame(
#'   head_cm = c(41, 40, 41, 51),
#'   age = c(401, 185, 101, 607),
#'   gestage = c(189, 252, 280, 287),
#'   sex = c("F", "M", "F", "M")
#' )
#'
#' data |>
#'   classify_headsize(
#'     headcirc_cm = head_cm,
#'     age_days = age,
#'     gest_days = gestage,
#'     sex = sex
#'   )
#' @export
classify_headsize <- function(.data,
                              headcirc_cm,
                              age_days,
                              gest_days,
                              sex,
                              .new = c("hcaz", "headsize")) {
  checkmate::assert_data_frame(.data, min.rows = 1)
  checkmate::qassert(.new, rules = "S2")
  err_if_.new_in_.data(new = .new, .data_colnames = colnames(.data))

  hcaz <- gigs_hcaz(
    headcirc_cm = eval_tidy(enquo(headcirc_cm), .data),
    age_days = eval_tidy(enquo(age_days), .data),
    gest_days = eval_tidy(enquo(gest_days), .data),
    sex = eval_tidy(enquo(sex), .data)
  )

  .new <- vctrs::vec_as_names(.new, repair = "universal")
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
#' @param .analyses A character vector of up to six elements in length
#'   describing which growth analyses you want to run. Use this when you supply
#'   data that can be used to generate multiple growth indicators, but only want
#'   to run a specific set. This argument is case-sensitive, and an error will
#'   be thrown if you supply a string in `.analyses` which is either `NA` or is
#'   not a member of this argument's default. Default = `c("sfga", "svn",
#'   "stunting", "wasting", "wfa", "headsize")`.
#' @param .new A list with names corresponding to `.analyses`, which describes
#'   the names of new columns to be added to `.data`. If any elements of the
#'   vectors in `.new` are equal to existing elements of `colnames(.data)`, the
#'   function will throw an error. Excluding the birthweight centiles produced
#'   when getting size-for-gestational age (`"sfga"`) and small vulnerable
#'   newborn (`"svn"`) classifications, all strings in `.new` must be unique.
#'   Each character vector in `.new` is repaired with `vctrs::as_names()`, which
#'   will issue messages if any elements in `.new` are changed.
#'
#'   \itemize{
#'    \item{`"sfga"`:}{  `c("birthweight_centile", "sfga", "sfga_severe")`}
#'    \item{`"svn"`:}{  `c("birthweight_centile", "svn")`}
#'    \item{`"stunting"`:}{  `c("lhaz", "stunting", "stunting_outliers")`}
#'    \item{`"wasting"`:}{  `c("wlz", "wasting", "wasting_outliers")`}
#'    \item{`"wfa"`:}{  `c("waz", "wfa", "wfa_outliers")`}
#'    \item{`"headsize"`:}{  `c("hcaz", "headsize")`}
#'   }
#' @param .verbose A single logical value. When `TRUE` (the default),
#'   messages about which analyses were requested versus which were performed
#'   will be printed to the console. Warnings from `classify_growth()` will
#'   still be printed even if `.verbose` is `FALSE`.
#' @note For size-for-GA and small vulnerable newborn analyses, centiles and
#'   categorisations will only be applied on birthweights, i.e. rows where
#'   the column referred to by `weight_kg` has an age between `0` and `0.5`.
#' @examples
#' data <- data.frame(
#'   agedays = c(0, 100, 100),
#'   gestage = c(270, 270, 270),
#'   wtkg = c(2.5, 7.5, 7.5),
#'   len_cm = c(45, 60, 60),
#'   head_cm = c(36, 40.2, 40.2),
#'   sex = c("M", "M", "F")
#' )
#'
#' data_classified <- classify_growth(data,
#'                                    weight_kg = wtkg,
#'                                    age_days = agedays,
#'                                    gest_days = gestage,
#'                                    sex = sex)
#'
#' data_classified
#'
#' # Use `.analyses` to set which growth indicators will be computed
#' data_svn <- classify_growth(data,
#'                             weight_kg = wtkg,
#'                             age_days = agedays,
#'                             gest_days = gestage,
#'                             sex = sex,
#'                             .analyses = "svn")
#'
#' data_svn
#'
#' # Use `.new` to set new column names
#' data_svn <- classify_growth(data,
#'                             weight_kg = wtkg,
#'                             age_days = agedays,
#'                             gest_days = gestage,
#'                             sex = sex,
#'                             .analyses = "svn",
#'                             .new = list("svn" = c("ig_nbs_centile",
#'                                                   "SVN_Category")))
#' data_svn
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
    .analyses = c("sfga", "svn", "stunting", "wasting", "wfa", "headsize"),
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
  analyses_run <- rep_len(x = FALSE, length.out = all_analyses_len) |>
    setNames(all_analyses)

  # Variable checking - basic assertions on length/type
  checkmate::assert_data_frame(.data, min.rows = 1)
  checkmate::assert_character(.analyses, any.missing = FALSE, min.len = 1,
                              max.len = all_analyses_len)
  checkmate::assert_subset(.analyses, choices = all_analyses)
  checkmate::assert_list(.new, any.missing = FALSE, min.len = 1,
                         max.len = all_analyses_len)
  checkmate::qassert(x = .verbose, rules = "B1")

  # Ascertain integrity of `.data`, `.analyses`, and `.new`
  err_if_.new_in_.data(new = unique(unlist(.new)),
                       .data_colnames = colnames(.data))
  check_all_.new_names_valid(.new = .new, all = all_analyses)
  check_all_.analyses_in_.new(.new = .new, .analyses = .analyses)
  .new <- .new[.analyses]
  check_.new_vector_lengths(.new)
  .new <- check_sfga_svn_centile_colname(.new)
  check_if_.new_elements_unique(.new)
  .new <- repair_.new_names(.new)

  catch_and_throw_validate_issues(expr = {
    growth_data <- data.frame(
      "gest_days" = validate_numeric(eval_tidy(enquo(gest_days), .data),
                                     varname = "gest_days"),
      "age_days" = validate_numeric(eval_tidy(enquo(age_days), .data),
                                    varname = "age_days"),
      "sex" = validate_sex(eval_tidy(enquo(sex), .data))
    )
  }, call = rlang::current_env())

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

  # Run analyses if data allows
  for (analysis in .analyses) {
    if (analysis == "sfga" | analysis == "svn" & !missing_weight) {
      bweight_centile_not_calculated <- !.new[[analysis]][1] %in% names(.data)
      if (bweight_centile_not_calculated) {
        is_birthweight <-
          growth_data[["age_days"]] > -sqrt(.Machine$double.eps) &
          growth_data[["age_days"]] < 0.5
        is_birthweight[is.na(is_birthweight)] <- FALSE
        is_calculable <- is_birthweight &
          inrange(growth_data[["gest_days"]], vec = c(168, 300))
        ig_nbs_wfga_p <- rep(NA_real_, nrow(.data))
        ig_nbs_wfga_p[is_calculable] <- fn_on_subset(
          fn = ig_nbs_v2c_internal,
          lgl = is_calculable,
          growth_data[["weight_kg"]],
          growth_data[["gest_days"]],
          growth_data[["sex"]],
          acronym = "wfga"
        )
        .data[[.new[[analysis]][1]]] <- ig_nbs_wfga_p
      }
      if (analysis == "sfga") {
        .data[[.new[[analysis]][2]]] <- categorise_sfga_internal(
          p = ig_nbs_wfga_p, severe = FALSE
        )
        .data[[.new[[analysis]][3]]] <- categorise_sfga_internal(
          p = ig_nbs_wfga_p, severe = TRUE
        )
      } else {
        .data[[.new[[analysis]][2]]] <- categorise_svn_internal(
          p = ig_nbs_wfga_p, gest_days = growth_data[["gest_days"]]
        )
      }
    }

    if (analysis == "stunting" & !missing_lenht) {
      lhaz <- with(growth_data,
                   gigs_lhaz_internal(lenht_cm = lenht_cm, age_days = age_days,
                                      gest_days = gest_days, sex = sex))
      .data[[.new[[analysis]][1]]] <- lhaz
      .data[[.new[[analysis]][2]]] <- categorise_stunting_internal(lhaz, FALSE)
      .data[[.new[[analysis]][3]]] <- categorise_stunting_internal(lhaz, TRUE)
    }

    if (analysis == "wasting" & !(missing_weight | missing_lenht)) {
      wlz <- with(growth_data,
                   gigs_wlz_internal(weight_kg = weight_kg,
                                     lenht_cm = lenht_cm, age_days = age_days,
                                     gest_days = gest_days, sex = sex))
      .data[[.new[[analysis]][1]]] <- wlz
      .data[[.new[[analysis]][2]]] <- categorise_wasting_internal(wlz, FALSE)
      .data[[.new[[analysis]][3]]] <- categorise_wasting_internal(wlz, TRUE)
    }

    if (analysis == "wfa" & !missing_weight) {
      waz <- with(growth_data,
                  gigs_waz_internal(weight_kg = weight_kg, age_days = age_days,
                                    gest_days = gest_days, sex = sex))
      .data[[.new[[analysis]][1]]] <- waz
      .data[[.new[[analysis]][2]]] <- categorise_wfa_internal(waz, FALSE)
      .data[[.new[[analysis]][3]]] <- categorise_wfa_internal(waz, TRUE)
    }

    if (analysis == "headsize" & !missing_headcirc) {
      hcaz <- with(growth_data,
                   gigs_hcaz_internal(headcirc_cm = headcirc_cm,
                                      age_days = age_days, gest_days = gest_days,
                                      sex = sex))
      .data[[.new[[analysis]][1]]] <- hcaz
      .data[[.new[[analysis]][2]]] <- categorise_headsize_internal(hcaz)
    }
    analyses_run[analysis] <- TRUE
  }
  if (.verbose) {
    rlang::inform(message = msg_classify_growth(all = all_analyses,
                                                requested = .analyses,
                                                run = analyses_run))
  }

  .data
}

# INTERNAL ---------------------------------------------------------------------

#' @noRd
check_.new_vector_lengths <- function(.new) {
  .new_names <- names(.new)
  for (idx in seq_along(.new)) {
    curr_vec <- .new[[idx]]
    curr_name <- .new_names[idx]
    expected_len <- switch(curr_name, sfga = 3, svn = 2, wasting = 3,
                           stunting = 3, wfa = 3, headsize = 2)
    checkmate::assert_character(
      curr_vec,
      any.missing = FALSE,
      len = expected_len,
      .var.name = paste0(".new[[\"", curr_name, "\"]]")
    )
  }
}

check_all_.new_names_valid <- function(.new, all) {
  .new_names <- names(.new)
  lgl_invalid_.new_name <- !.new_names %in% all
  if (any(lgl_invalid_.new_name)) {
    rlang::abort(c(
      "Some elements in `names(.new)` are not valid.",
      "!" = paste0("Bad names in `.new`: ",
                   paste_sep_commas_quoted(.new_names[lgl_invalid_.new_name])),
      "i" = paste0("Each name in `.new` must be one of: ",
                   paste_sep_commas_quoted(all))
    ), call = rlang::expr(gigs::classify_growth()))
  }
  return(invisible(.new))
}

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
                    "i" = paste0("GIGS will use the name provided for ",
                                 "size-for-GA (", bweight_centiles_sfga, ")."))
      )
      .new[["svn"]][1] <- bweight_centiles_sfga
    }
  }
  .new
}

#' Check dot
#' @noRd
check_all_.analyses_in_.new <- function(.analyses, .new) {
  lgl_.analyses_in_.new <- .analyses %in% names(.new)
  if (!all(lgl_.analyses_in_.new)) {
    rlang::abort(
      c("Elements of `.analyses` are missing new column names in `.new`:",
        "!" = paste0("`.new` must have column names for",
                     paste_sep_commas(.new[!lgl_.analyses_in_.new]))),
      class = "gigs_classify_growth_.analyses_not_in_.new"
    )
  }
}

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
  rlang::abort(
    c("Some elements in `.new` are not unique.",
      setNames(sprintf(fmt = "`\"%s\"`", chr_duplicated),
               rep("!", length(chr_duplicated)))),
    call = rlang::caller_env()
  )
}

#' @noRd
repair_.new_names <- function(.new) {
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
    rlang::warn(
      c("Column names in `.new` repaired by `rlang::vec_as_names()`:",
        unlist(li_renamed)),
      call = caller_env()
    )
  }
  .new
}

#' Check whether a character vector has any elements in another, and issue an
#' @param new A character vector of length one or more with new column names to
#'   check against `existing`.
#' @param .data_colnames A character vector of length one or more with column names
#'   to be checked against with `new`.
#' @return Returns `new` invisibly if there are no matches between `new` and
#'   `existing`, else throws an error.
#' @noRd
err_if_.new_in_.data <- function(new, .data_colnames) {
  matches <- new %in% .data_colnames
  any_matches <- any(matches)
  if (any_matches) {
    matched_names <- paste0("`", new[matches], "`") |>
      setNames(nm = "!")
    rlang::abort(
      message =
        c(paste0("Column names requested in `.new` already exist in `.data`. ",
                 "These are:"),
          matched_names
        ),
      call = rlang::env_parent(),
      class = "gigs_classify_.new_in_.data")
  }
  invisible(new)
}

#' Build a character vector describing which analyses in `classify_growth()`
#' were performed, formatted for `rlang::inform()`
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
msg_classify_growth <- function(all, requested, run) {
  analysis_msg_strings <- c("Size-for-gestational age",
                            "Small vulnerable newborns", "Stunting", "Wasting",
                            "Weight-for-age", "Head size") |>
    setNames(all)
  analysis_msg_strings <- analysis_msg_strings[requested]
  msg_strings <- rep_len(x = NA_character_, length(requested))
  for (idx in seq_along(requested)) {
    chr_analysis <- requested[idx]
    lgl_was_run <- run[chr_analysis]
    chr_was_run <- if (lgl_was_run) "Success" else "Not computed"
    names(msg_strings)[idx] <- if (lgl_was_run) "v" else "!"
    msg_strings[idx] <- paste0(analysis_msg_strings[chr_analysis], ": ",
                               chr_was_run)
  }
  c("`gigs::classify_growth()`", msg_strings)
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
NULL

# @srrstats {G2.7} We use `checkmate::assert_data_frame()` to check that the
#   supplied `.data` objects inherit from `data.frame`, allowing the use of at
#   `tibble`s, `data.table`s, and more.
# @srrstats {G2.8} The package passes columns to various GIGS-specific
#   `validate_*()` functions. These ensure that passed columns are atomic and
#   have the right type.
# @srrstats {G2.9} Users can select names to apply to new columns, which are
#   fixed by `vctrs::as_names()`. This function prints messages to the console
#   if any of the names supplied by the user require changing.
# @srrstats {G2.11} In this script, all vector columns are passed to functions
#   which strip attributes - this lets `units`-like non-standard vector columns
#   work in these functions.


# #' @srrstats {G2.12} For this script
# #' @srrstats {G2.14, G2.14a, G2.14b, G2.14c} Missing/undefined data checks are
# #'   performed and their behaviour can be customised using [gigs_option_set()].