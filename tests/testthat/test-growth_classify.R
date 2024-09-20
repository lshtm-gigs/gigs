#' @srrstats {G5.1} Users can run this function to make their own data set for
#'   test confirmation
test_data <- gigs_random_growth_dataset(10000, seed = 11, restrict = TRUE)

test_data_birth <- test_data[with(test_data, !is.na(bweight_centile_exp)),]
test_data_postnatal <- test_data[with(test_data, age_days > 3), ]
test_data_postnatal_wlz <- test_data_postnatal[
    !is.na(with(test_data_postnatal, weight_kg_from_wlz)),
]

# Testing `classify_sfga()` ----------------------------------------------------

#' @srrstats {G5.4, G5.4c} Tests to ensure that `classify_sfga()` reproduces
#'   expected z-scores from randomly generated growth data, and the appropriate
#'   categorical data.
test_that(desc = "classify_sfga() reproduces expected z-scores", {
  sfga <- classify_sfga(
    .data = test_data_birth,
    weight_kg = weight_kg,
    gest_days = gest_age,
    sex = sex,
    .new = c("bweight_centile_test", "sfga", "sfga_severe")
  )
  sfga_temp <- dplyr::select(sfga, id, tidyselect::starts_with("bweight"), tidyselect::starts_with("sfga"))
  testthat::expect_equal(
    sfga$bweight_centile_test, sfga$bweight_centile_exp
  )
  testthat::expect_equal(
    categorise_sfga(p = sfga$bweight_centile_test, severe = FALSE),
    sfga$sfga
  )
  testthat::expect_equal(
    categorise_sfga(p = sfga$bweight_centile_test, severe = TRUE),
    sfga$sfga_severe
  )
})

#' @srrstats {G5.2, G5.2a, G5.2b} Tests of message/warning/error behaviour
#'   in `classify_sfga()`.
test_that(desc = "classify_sfga() throws appropriate errors/warnings", {
  testthat::expect_error(
    classify_sfga(
      .data = test_data_birth,
      weight_kg = weight_kg,
      gest_days = gest_age,
      sex = sex,
      .new = c("bweight_centile_exp", "waz_exp", "sfga_severe")
    ),
    class = "gigs_classify_.new_in_.data"
  )

  testthat::expect_warning(
    classify_sfga(
      .data = test_data_birth,
      weight_kg = weight_kg,
      gest_days = gest_age,
      sex = sex,
      .new = c(".3415789.", "sfga", "sfga")
    ),
    class = "gigs_repaired_names_in_.new"
  )
})

#' @srrstats {G5.8, G5.8a, G5.8b} Testing bad data inputs to `classify_sfga()`.
test_that(desc = "classify_sfga() errors on bad data types", {

  # Not using a data frame for .data
  testthat::expect_error(
    classify_sfga(
      .data = as.list(test_data_birth),
      weight_kg = weight_kg,
      gest_days = gest_age,
      sex = sex,
      .new = c("bweight_centile_exp", "waz_exp", "sfga_severe")
    ),
    regexp = test_error_wrong_type(".data", "data.frame", "list")
  )

  # Using the wrong data types
  testthat::expect_error(
    classify_sfga(
      .data = test_data_birth,
      weight_kg = as.complex(weight_kg),
      gest_days = gest_age,
      sex = sex,
      .new = c("bweight_centile_test", "sfga", "sfga_severe")
    ),
    regexp = test_error_wrong_type("weight_kg", "numeric", "complex")
  )
  testthat::expect_error(
    classify_sfga(
      .data = test_data_birth,
      weight_kg = weight_kg,
      gest_days = as.character(gest_age),
      sex = sex,
      .new = c("bweight_centile_test", "sfga", "sfga_severe")
    ),
    regexp = test_error_wrong_type("gest_days", "numeric", "character")
  )
  testthat::expect_error(
    classify_sfga(
      .data = test_data_birth,
      weight_kg = weight_kg,
      gest_days = as.character(gest_age),
      sex = sex,
      .new = 1:3
    ),
    regexp = test_error_wrong_type(".new", "string", "integer")
  )
  testthat::expect_error(
    classify_sfga(
      .data = test_data_birth,
      weight_kg = weight_kg,
      gest_days = gest_age,
      sex = sex,
      .new = c("bweight_centile_test", "sfga_severe")
    ),
    regexp = regex_error_wrong_length(varname = ".new",
                                      length_wanted = 3,
                                      length_got = 2)
  )
})

# Testing `classify_svn()` -----------------------------------------------------

#' @srrstats {G5.4, G5.4c} Tests to ensure that `classify_svn()` reproduces
#'   expected z-scores from randomly generated growth data, and the appropriate
#'   categorical data.
test_that(desc = "classify_svn() reproduces expected z-scores", {
  svn <- classify_svn(
    .data = test_data_birth,
    weight_kg = weight_kg,
    gest_days = gest_age,
    sex = sex,
    .new = c("bweight_centile_test", "svn")
  )
  testthat::expect_equal(
    svn$bweight_centile_exp, svn$bweight_centile_test
  )
  testthat::expect_equal(
    categorise_svn(p = svn$bweight_centile_test, gest_days = svn$gest_age),
    svn$svn
  )
})

#' @srrstats {G5.2, G5.2a, G5.2b} Tests of message/warning/error behaviour
#'   in `classify_svn()`.
test_that(desc = "classify_svn() throws appropriate errors/warnings", {
  testthat::expect_error(
    classify_svn(
      .data = test_data_birth,
      weight_kg = weight_kg,
      gest_days = gest_age,
      sex = sex,
      .new = c("bweight_centile_exp", "svn")
    ),
    class = "gigs_classify_.new_in_.data"
  )

  testthat::expect_warning(
    classify_svn(
      .data = test_data_birth,
      weight_kg = weight_kg,
      gest_days = gest_age,
      sex = sex,
      .new = c(".23456987", "svn")
    ),
    class = "gigs_repaired_names_in_.new"
  )
})

#' @srrstats {G5.8, G5.8a, G5.8b} Testing bad data inputs to
#'   `classify_svn()`.
test_that(desc = "classify_svn() errors on bad data types", {

  # Not using a data frame for .data
  testthat::expect_error(
    classify_svn(
      .data = as.list(test_data_birth),
      weight_kg = weight_kg,
      gest_days = gest_age,
      sex = sex,
      .new = c("bweight_centile_test", "svn")
    ),
    regexp = test_error_wrong_type(".data", "data.frame", "list")
  )

  # Using the wrong data types
  testthat::expect_error(
    classify_svn(
      .data = test_data_birth,
      weight_kg = as.complex(weight_kg),
      gest_days = gest_age,
      sex = sex,
      .new = c("bweight_centile_test", "svn")
    ),
    regexp = test_error_wrong_type("weight_kg", "numeric", "complex")
  )
  testthat::expect_error(
    classify_svn(
      .data = test_data_birth,
      weight_kg = weight_kg,
      gest_days = as.character(gest_age),
      sex = sex,
      .new = c("bweight_centile_test", "svn")
    ),
    regexp = test_error_wrong_type("gest_days", "numeric", "character")
  )
  testthat::expect_error(
    classify_svn(
      .data = test_data_birth,
      weight_kg = weight_kg,
      gest_days = gest_age,
      sex = sex,
      .new = 5:6
    ),
    regexp = test_error_wrong_type(".new", "string", "integer")
  )
  testthat::expect_error(
    classify_svn(
      .data = test_data_birth,
      weight_kg = weight_kg,
      gest_days = gest_age,
      sex = sex,
      .new = "bweight_centile_test"
    ),
    regexp = regex_error_wrong_length(varname = ".new",
                                      length_wanted = 2,
                                      length_got = 1)
  )
})

# Testing `classify_stunting()` ------------------------------------------------

#' @srrstats {G5.4, G5.4c} Tests to ensure that `classify_stunting()` reproduces
#'   expected z-scores from randomly generated growth data, and the appropriate
#'   categorical data.
test_that(desc = "classify_stunting() reproduces expected z-scores", {
  stunting <- classify_stunting(
    .data = test_data[with(test_data, !is.na(lenht_cm)), ],
    lenht_cm = lenht_cm,
    gest_days = gest_age,
    age_days = age_days,
    sex = sex,
    id = id,
    .new = c("lhaz_test", "stunting", "stunting_outliers")
  )
  testthat::expect_equal(
    stunting$lhaz_test, stunting$lhaz_exp
  )
  testthat::expect_equal(
    categorise_stunting(lhaz = stunting$lhaz_test, outliers = FALSE),
    stunting$stunting
  )
  testthat::expect_equal(
    categorise_stunting(lhaz = stunting$lhaz_test, outliers = TRUE),
    stunting$stunting_outliers
  )
})

#' @srrstats {G5.2, G5.2a, G5.2b} Tests of message/warning/error behaviour
#'   in `classify_stunting()`.
test_that(desc = "classify_stunting() throws appropriate errors/warnings", {
  testthat::expect_error(
    classify_stunting(
      .data = test_data,
      lenht_cm = lenht_cm,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c("lhaz_exp", "stunting", "stunting_outliers")
    ),
    class = "gigs_classify_.new_in_.data"
  )

  testthat::expect_warning(
    classify_stunting(
      .data = test_data,
      lenht_cm = lenht_cm,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c(".3415789.", "stunting", "stunting")
    ),
    class = "gigs_repaired_names_in_.new"
  )
})

#' @srrstats {G5.8, G5.8a, G5.8b} Testing bad data inputs to
#'   `classify_stunting()`.
test_that(desc = "classify_stunting() errors on bad data", {

  # Not using a data frame for .data
  testthat::expect_error(
    classify_stunting(
      .data = as.list(test_data),
      lenht_cm = lenht_cm,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c("lhaz_test", "stunting", "stunting_outliers")
    ),
    regexp = test_error_wrong_type(".data", "data.frame", "list")
  )

  # Using the wrong data types
  testthat::expect_error(
    classify_stunting(
      .data = test_data,
      lenht_cm = as.complex(lenht_cm),
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c("lhaz_test", "stunting", "stunting_outliers")
    ),
    regexp = test_error_wrong_type("lenht_cm", "numeric", "complex")
  )
  testthat::expect_error(
    classify_stunting(
      .data = test_data,
      lenht_cm = lenht_cm,
      age_days = as.character(age_days),
      gest_days = gest_age,
      sex = sex,
      .new = c("lhaz_test", "stunting", "stunting_outliers")
    ),
    regexp = test_error_wrong_type("age_days", "numeric", "character")
  )
  testthat::expect_error(
    classify_stunting(
      .data = test_data,
      lenht_cm = lenht_cm,
      age_days = age_days,
      gest_days = as.character(gest_age),
      sex = sex,
      .new = c("lhaz_test", "stunting", "stunting_outliers")
    ),
    regexp = test_error_wrong_type("gest_days", "numeric", "character")
  )
  testthat::expect_error(
    classify_stunting(
      .data = test_data,
      lenht_cm = lenht_cm,
      age_days = age_days,
      gest_days = as.character(gest_age),
      sex = sex,
      .new = 1:3
    ),
    regexp = test_error_wrong_type(".new", "string", "integer")
  )
  testthat::expect_error(
    classify_stunting(
      .data = test_data,
      lenht_cm = lenht_cm,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c("lhaz_test", "stunting_outliers")
    ),
    regexp = regex_error_wrong_length(varname = ".new",
                                      length_wanted = 3,
                                      length_got = 2)
  )
})

# Testing `classify_wasting()` -------------------------------------------------

#' @srrstats {G5.4, G5.4c} Tests to ensure that `classify_wasting()` reproduces
#'   expected z-scores from randomly generated growth data, and the appropriate
#'   categorical data.
test_that(desc = "classify_wasting() reproduces expected z-scores", {
  wasting <- classify_wasting(
    .data = test_data_postnatal_wlz,
    weight_kg = weight_kg_from_wlz,
    lenht_cm = lenht_cm,
    gest_days = gest_age,
    age_days = age_days,
    sex = sex,
    id = id,
    .new = c("wlz_test", "wasting", "wasting_outliers")
  )
  testthat::expect_equal(
    wasting$wlz_exp, wasting$wlz_test, tolerance = sqrt(.Machine$double.eps)
  )
  testthat::expect_equal(
    categorise_wasting(wlz = wasting$wlz_test, outliers = FALSE),
    wasting$wasting
  )
  testthat::expect_equal(
    categorise_wasting(wlz = wasting$wlz_test, outliers = TRUE),
    wasting$wasting_outliers
  )
})

#' @srrstats {G5.2, G5.2a, G5.2b} Tests of message/warning/error behaviour
#'   in `classify_wasting()`.
test_that(desc = "classify_wasting() throws appropriate errors/warnings", {
  testthat::expect_error(
    classify_wasting(
      .data = test_data_postnatal_wlz,
      weight_kg = weight_kg_from_wlz,
      lenht_cm = lenht_cm,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c("lhaz_exp", "wasting", "wasting_outliers")
    ),
    class = "gigs_classify_.new_in_.data"
  )

  testthat::expect_warning(
    classify_wasting(
      .data = test_data_postnatal_wlz,
      weight_kg = weight_kg_from_wlz,
      lenht_cm = lenht_cm,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c(".3415789.", "wasting", "wasting")
    ),
    class = "gigs_repaired_names_in_.new"
  )
})

#' @srrstats {G5.8, G5.8a, G5.8b} Testing bad data inputs to
#'   `classify_wasting()`.
test_that(desc = "classify_wasting() errors on bad data", {

  # Not using a data frame for .data
  testthat::expect_error(
    classify_wasting(
      .data = as.list(test_data_postnatal_wlz),
      weight_kg = weight_kg_from_wlz,
      lenht_cm = lenht_cm,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c("lhaz_test", "wasting", "wasting_outliers")
    ),
    regexp = test_error_wrong_type(".data", "data.frame", "list")
  )

  # Using the wrong data types
  testthat::expect_error(
    classify_wasting(
      .data = test_data_postnatal_wlz,
      weight_kg = as.character(weight_kg_from_wlz),
      lenht_cm = lenht_cm,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c("lhaz_test", "wasting", "wasting_outliers")
    ),
    regexp = test_error_wrong_type("weight_kg", "numeric", "character")
  )
  testthat::expect_error(
    classify_wasting(
      .data = test_data_postnatal_wlz,
      weight_kg = weight_kg_from_wlz,
      lenht_cm = as.complex(lenht_cm),
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c("lhaz_test", "wasting", "wasting_outliers")
    ),
    regexp = test_error_wrong_type("lenht_cm", "numeric", "complex")
  )
  testthat::expect_error(
    classify_wasting(
      .data = test_data_postnatal_wlz,
      weight_kg = weight_kg_from_wlz,
      lenht_cm = lenht_cm,
      age_days = as.character(age_days),
      gest_days = gest_age,
      sex = sex,
      .new = c("lhaz_test", "wasting", "wasting_outliers")
    ),
    regexp = test_error_wrong_type("age_days", "numeric", "character")
  )
  testthat::expect_error(
    classify_wasting(
      .data = test_data_postnatal_wlz,
      weight_kg = weight_kg_from_wlz,
      lenht_cm = lenht_cm,
      age_days = age_days,
      gest_days = as.factor(gest_age),
      sex = sex,
      .new = c("lhaz_test", "wasting", "wasting_outliers")
    ),
    regexp = test_error_wrong_type("gest_days", "numeric", "factor")
  )
  testthat::expect_error(
    classify_wasting(
      .data = test_data_postnatal_wlz,
      weight_kg = weight_kg_from_wlz,
      lenht_cm = lenht_cm,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c("lhaz_test", "wasting_outliers")
    ),
    regexp = regex_error_wrong_length(varname = ".new",
                                      length_wanted = 3,
                                      length_got = 2)
  )
})

# Testing `classify_wfa()` -----------------------------------------------------

#' @srrstats {G5.4, G5.4c} Tests to ensure that `classify_wfa()` reproduces
#'   expected z-scores from randomly generated growth data, and the appropriate
#'   categorical data.
test_that(desc = "classify_wfa() reproduces expected z-scores", {
  wfa <- classify_wfa(
    .data = test_data,
    weight_kg = weight_kg,
    gest_days = gest_age,
    age_days = age_days,
    sex = sex,
    id = id,
    .new = c("waz_test", "wfa", "wfa_outliers")
  )
  testthat::expect_equal(
    wfa$waz_exp, wfa$waz_test, tolerance = sqrt(.Machine$double.eps)
  )
  testthat::expect_equal(
    categorise_wfa(waz = wfa$waz_test, outliers = FALSE),
    wfa$wfa
  )
  testthat::expect_equal(
    categorise_wfa(waz = wfa$waz_test, outliers = TRUE),
    wfa$wfa_outliers
  )
})

#' @srrstats {G5.2, G5.2a, G5.2b} Tests of message/warning/error behaviour
#'   in `classify_wfa()`.
test_that(desc = "classify_wfa() throws appropriate errors/warnings", {
  testthat::expect_error(
    classify_wfa(
      .data = test_data,
      weight_kg = weight_kg,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c("waz_exp", "wfa", "wfa_outliers")
    ),
    class = "gigs_classify_.new_in_.data"
  )

  testthat::expect_warning(
    classify_wfa(
      .data = test_data,
      weight_kg = weight_kg,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c(".3415789.", "wfa", "wfa_outliers")
    ),
    class = "gigs_repaired_names_in_.new"
  )
})

#' @srrstats {G5.8, G5.8a, G5.8b} Testing bad data inputs to
#'   `classify_wfa()`.
test_that(desc = "classify_wfa() errors on bad data", {
  # Not using a data frame for .data
  testthat::expect_error(
    classify_wfa(
      .data = as.list(test_data),
      weight_kg = weight_kg,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c("waz_test", "wfa", "wfa_outliers")
    ),
    regexp = test_error_wrong_type(".data", "data.frame", "list")
  )

  # Using the wrong data types
  testthat::expect_error(
    classify_wfa(
      .data = test_data,
      weight_kg = as.complex(weight_kg),
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c("waz_test", "wfa", "wfa_outliers")
    ),
    regexp = test_error_wrong_type("weight_kg", "numeric", "complex")
  )
  testthat::expect_error(
    classify_wfa(
      .data = test_data,
      weight_kg = weight_kg,
      age_days = as.character(age_days),
      gest_days = gest_age,
      sex = sex,
      .new = c("waz_test", "wfa", "wfa_outliers")
    ),
    regexp = test_error_wrong_type("age_days", "numeric", "character")
  )
  testthat::expect_error(
    classify_wfa(
      .data = test_data,
      weight_kg = weight_kg,
      age_days = age_days,
      gest_days = as.character(gest_age),
      sex = sex,
      .new = c("waz_test", "wfa", "wfa_outliers")
    ),
    regexp = test_error_wrong_type("gest_days", "numeric", "character")
  )
  testthat::expect_error(
    classify_wfa(
      .data = test_data,
      weight_kg = weight_kg,
      age_days = age_days,
      gest_days = as.character(gest_age),
      sex = sex,
      .new = 1:3
    ),
    regexp = test_error_wrong_type(".new", "string", "integer")
  )
  testthat::expect_error(
    classify_wfa(
      .data = test_data,
      weight_kg = weight_kg,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = "waz_test"
    ),
    regexp = regex_error_wrong_length(varname = ".new",
                                      length_wanted = 3,
                                      length_got = 1)
  )
})

# Testing `classify_headsize()` ------------------------------------------------

#' @srrstats {G5.4, G5.4c} Tests to ensure that `classify_headsize()` reproduces
#'   expected z-scores from randomly generated growth data, and the appropriate
#'   categorical data.
test_that(desc = "classify_headsize() reproduces expected z-scores", {
  headsize <- classify_headsize(
    .data = test_data,
    headcirc_cm = headcirc_cm,
    gest_days = gest_age,
    age_days = age_days,
    sex = sex,
    id = id,
    .new = c("hcaz_test", "headsize")
  )
  testthat::expect_equal(
    headsize$hcaz_exp, headsize$hcaz_test, tolerance = sqrt(.Machine$double.eps)
  )
  testthat::expect_equal(
    categorise_headsize(hcaz = headsize$hcaz_test),
    headsize$headsize
  )
})

#' @srrstats {G5.2, G5.2a, G5.2b} Tests of message/warning/error behaviour
#'   in `classify_stunting()`.
test_that(desc = "classify_headsize() throws appropriate errors/warnings", {
  testthat::expect_error(
    classify_headsize(
      .data = test_data,
      headcirc_cm = headcirc_cm,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c("hcaz_exp", "headsize")
    ),
    class = "gigs_classify_.new_in_.data"
  )

  testthat::expect_warning(
    classify_headsize(
      .data = test_data,
      headcirc_cm = headcirc_cm,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c(".3415789.", "headsize")
    ),
    class = "gigs_repaired_names_in_.new"
  )
})

#' @srrstats {G5.8, G5.8a, G5.8b} Testing bad data inputs to
#'   `classify_headsize()`.
test_that(desc = "classify_headsize() errors on bad data", {
  # Not using a data frame for .data
  testthat::expect_error(
    classify_headsize(
      .data = as.list(test_data),
      headcirc_cm = headcirc_cm,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c("hcaz_test", "headsize")
    ),
    regexp = test_error_wrong_type(".data", "data.frame", "list")
  )

  # Using the wrong data types
  testthat::expect_error(
    classify_headsize(
      .data = test_data,
      headcirc_cm = as.complex(headcirc_cm),
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c("hcaz_test", "headsize")
    ),
    regexp = test_error_wrong_type("headcirc_cm", "numeric", "complex")
  )
  testthat::expect_error(
    classify_headsize(
      .data = test_data,
      headcirc_cm = headcirc_cm,
      age_days = as.character(age_days),
      gest_days = gest_age,
      sex = sex,
      .new = c("hcaz_test", "headsize")
    ),
    regexp = test_error_wrong_type("age_days", "numeric", "character")
  )
  testthat::expect_error(
    classify_headsize(
      .data = test_data,
      headcirc_cm = headcirc_cm,
      age_days = age_days,
      gest_days = as.character(gest_age),
      sex = sex,
      .new = c("hcaz_test", "headsize")
    ),
    regexp = test_error_wrong_type("gest_days", "numeric", "character")
  )
  testthat::expect_error(
    classify_headsize(
      .data = test_data,
      headcirc_cm = headcirc_cm,
      age_days = age_days,
      gest_days = as.character(gest_age),
      sex = sex,
      .new = 1:3
    ),
    regexp = test_error_wrong_type(".new", "string", "integer")
  )
  testthat::expect_error(
    classify_headsize(
      .data = test_data,
      headcirc_cm = headcirc_cm,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      .new = c("hcaz_test", "headsize", "headsize2")
    ),
    regexp = regex_error_wrong_length(varname = ".new",
                                      length_wanted = 2,
                                      length_got = 3)
  )
})

# Testing `classify_growth()` --------------------------------------------------

#' @srrstats {G5.4, G5.4c} Tests to ensure that `classify_growth()` reproduces
#'   expected z-scores from randomly generated growth data.
test_that(desc = "`classify_growth()` reproduces expected z-scores", {
  growth <- classify_growth(
    .data = test_data,
    weight_kg = weight_kg,
    lenht_cm = lenht_cm,
    headcirc_cm = headcirc_cm,
    gest_days = gest_age,
    age_days = age_days,
    sex = sex,
    id = id,
    .outcomes = c("sfga", "svn", "stunting", "wfa", "headsize"),
    # Also demonstrates column renaming with .new
    .new = list(
      sfga = c("bweight_centile_test", "sfga", "sfga_severe"),
      svn = c("bweight_centile_test", "svn"),
      stunting = c("lhaz_test", "stunting", "stunting_outliers"),
      wfa = c("waz_test", "wfa", "wfa_outliers"),
      headsize = c("hcaz_test", "headsize")
    ),
    .verbose = FALSE # Silence message for this test
  )

  # Subset for only birth obs
  growth_birth <- growth[with(growth, !is.na(bweight_centile_test)), ]

  ## Size-for-GA
  testthat::expect_equal(
    growth_birth$bweight_centile_exp, growth_birth$bweight_centile_test
  )
  testthat::expect_equal(
    categorise_sfga(p = growth_birth$bweight_centile_test, severe = FALSE),
    growth_birth$sfga
  )
  testthat::expect_equal(
    categorise_sfga(p = growth_birth$bweight_centile_test, severe = TRUE),
    growth_birth$sfga_severe
  )

  ## SVN
  testthat::expect_equal(
    growth_birth$bweight_centile_exp, growth_birth$bweight_centile_test
  )
  testthat::expect_equal(
    categorise_svn(p = growth_birth$bweight_centile_test,
                   gest_days = growth_birth$gest_age),
    growth_birth$svn
  )

  # Stunting
  testthat::expect_equal(growth$lhaz_test[1:10], growth$lhaz_exp[1:10])
  testthat::expect_equal(
    categorise_stunting(lhaz = growth$lhaz_test, outliers = FALSE),
    growth$stunting
  )
  testthat::expect_equal(
    categorise_stunting(lhaz = growth$lhaz_test, outliers = TRUE),
    growth$stunting_outliers
  )

  # Weight-for-age
  testthat::expect_equal(
    growth$waz_exp, growth$waz_test, tolerance = sqrt(.Machine$double.eps)
  )
  testthat::expect_equal(
    categorise_wfa(waz = growth$waz_test, outliers = FALSE),
    growth$wfa
  )
  testthat::expect_equal(
    categorise_wfa(waz = growth$waz_test, outliers = TRUE),
    growth$wfa_outliers
  )

  # Head size
  testthat::expect_equal(
    growth$hcaz_exp, growth$hcaz_test, tolerance = sqrt(.Machine$double.eps)
  )
  testthat::expect_equal(
    categorise_headsize(hcaz = growth$hcaz_test),
    growth$headsize
  )

  # For wasting, use weight from WLZ to weight function
  growth_wlz <- classify_growth(
    .data = test_data_postnatal_wlz,
    weight_kg = weight_kg_from_wlz,
    lenht_cm = lenht_cm,
    gest_days = gest_age,
    age_days = age_days,
    sex = sex,
    .outcomes = "wasting",
    # Also demonstrates column renaming with .new
    .new = list(
      wasting = c("wlz_test", "wasting", "wasting_outliers")
    ),
    .verbose = FALSE # Silence message for this test
  )
  testthat::expect_equal(
    growth$waz_exp, growth$waz_test, tolerance = sqrt(.Machine$double.eps)
  )
  testthat::expect_equal(
    categorise_wfa(waz = growth$waz_test, outliers = FALSE),
    growth$wfa
  )
  testthat::expect_equal(
    categorise_wfa(waz = growth$waz_test, outliers = TRUE),
    growth$wfa_outliers
  )
})

#' @srrstats {G5.2, G5.2a, G5.2b} Demonstrating some errors + messages from
#'   `classify_growth()`.
test_that(desc = "`classify_growth()` displays the right errors/messages", {
  # Throw error when names in .new correspond to existing columns in .data
  testthat::expect_error(
    classify_growth(
      .data = test_data,
      weight_kg = weight_kg,
      gest_days = gest_age,
      age_days = age_days,
      sex = sex,
      .outcomes = c("sfga", "svn"),
      .new = list(
        sfga = c("bweight_centile_exp", "sfga", "sfga_severe"),
        svn = c("bweight_centile_exp", "svn")
      ),
      .verbose = FALSE # Silence message for this test
    ),
    class = "gigs_classify_.new_in_.data"
  )

  # Throw error if .new has names() which aren't valid
  testthat::expect_error(
    classify_growth(
      .data = test_data,
      weight_kg = weight_kg,
      gest_days = gest_age,
      age_days = age_days,
      sex = sex,
      .outcomes = c("sfga", "svn"),
      .new = list(
        sfga_WRONG = c("bweight_centile_test", "sfga", "sfga_severe"),
        svn_BAD = c("bweight_centile_test", "svn")
      ),
      .verbose = FALSE # Silence message for this test
    ),
    class = "gigs_classify_growth_invalid_.new_names"
  )

  # Throw error if members of `.outcomes` aren't in `names(.new)`
  testthat::expect_error(
    classify_growth(
      .data = test_data,
      weight_kg = weight_kg,
      gest_days = gest_age,
      age_days = age_days,
      sex = sex,
      .outcomes = c("sfga", "svn", "stunting"),
      .new = list(
        sfga = c("bweight_centile_test", "sfga", "sfga_severe"),
        svn = c("bweight_centile_test", "svn")
      ),
      .verbose = FALSE # Silence message for this test
    ),
    class = "gigs_classify_growth_.outcomes_not_in_.new"
  )

  # Throw error if vectors in `.new` are the wrong length
  testthat::expect_error(
    classify_growth(
      .data = test_data,
      weight_kg = weight_kg,
      gest_days = gest_age,
      age_days = age_days,
      sex = sex,
      .outcomes = c("sfga", "svn", "stunting"),
      .new = list(
        sfga = c("bweight_centile_test", "sfga"),
        svn = "bweight_centile_test",
        stunting = c("lhaz", "stunting", "stunting_outliers", "stunting3")
      ),
      .verbose = TRUE # Silence message for this test
    ),
    class = "gigs_classify_growth_.new_lengths_incorrect"
  )

  # Display warning if names in `.new` for birth weight centile don't match
  testthat::expect_warning(
    classify_growth(
      .data = test_data,
      weight_kg = weight_kg,
      gest_days = gest_age,
      age_days = age_days,
      sex = sex,
      .outcomes = c("sfga", "svn"),
      .new = list(
        sfga = c("bweight_centile_1", "sfga", "sfga_severe"),
        svn = c("bweight_centile_2", "svn")
      ),
      .verbose = FALSE # Silence message for this test
    ),
    class = "gigs_classify_growth_bweight_centile_not_identical"
  )

  # Throw an error if elements of `.new` vectors are not unique
  testthat::expect_error(
    classify_growth(
      .data = test_data,
      weight_kg = weight_kg,
      gest_days = gest_age,
      age_days = age_days,
      sex = sex,
      .outcomes = c("sfga", "svn"),
      .new = list(
        sfga = c("bweight_centile1", "TEST", "sfga_severe"),
        svn = c("bweight_centile1", "TEST")
      ),
      .verbose = FALSE # Silence message for this test
    ),
    class = "gigs_classify_growth_.new_elements_not_unique"
  )

  # Throw a warning if new column names in .new need to be repaired
  testthat::expect_warning(
    classify_growth(
      .data = test_data,
      weight_kg = weight_kg,
      gest_days = gest_age,
      age_days = age_days,
      sex = sex,
      .outcomes = c("sfga", "svn", "stunting", "wfa", "headsize"),
      .new = list(
        sfga = c("bweight_centile_test", "sfga", "sfga_severe"),
        svn = c("bweight_centile_test", "svn"),
        stunting = c("4795865", "stunting", "stunting_outliers"),
        wfa = c("waz_test", "wfa", "087912342"),
        headsize = c("hcaz_test", "headsize")
      ),
      .verbose = FALSE # Silence message for this test
    ),
    class = "gigs_repaired_names_in_.new"
  )

  # Issue messages telling the user which outcomes were calculated

  ## All successful
  testthat::expect_message(
    classify_growth(
      .data = test_data,
      weight_kg = weight_kg,
      lenht_cm = lenht_cm,
      headcirc_cm = headcirc_cm,
      gest_days = gest_age,
      age_days = age_days,
      sex = sex,
      .outcomes = c("sfga", "svn", "stunting", "wfa", "headsize"),
      .new = list(
          sfga = c("birthweight_centile", "sfga", "sfga_severe"),
          svn = c("birthweight_centile", "svn"),
          stunting = c("lhaz", "stunting", "stunting_outliers"),
          wfa = c("waz", "wfa", "wfa_outliers"),
          headsize = c("hcaz", "headsize")
      ),
      .verbose = TRUE # Silence message for this test
    ),
    class = "gigs_classify_growth_msg"
  )

  ## Some unsuccessful
  testthat::expect_message(
    classify_growth(
      .data = test_data,
      weight_kg = weight_kg,
      lenht_cm = lenht_cm,
      gest_days = gest_age,
      age_days = age_days,
      sex = sex,
      .outcomes = c("sfga", "svn", "stunting", "wfa", "headsize"),
      .new = list(
          sfga = c("birthweight_centile", "sfga", "sfga_severe"),
          svn = c("birthweight_centile", "svn"),
          stunting = c("lhaz", "stunting", "stunting_outliers"),
          wfa = c("waz", "wfa", "wfa_outliers"),
          headsize = c("hcaz", "headsize")
      ),
      .verbose = TRUE # Silence message for this test
    ),
    regexp = "Head size: Not computed \\(`headcirc_cm` not supplied\\)",
    class = "gigs_classify_growth_msg"
  )

  ## Wasting not successful; because weight not supplied
  testthat::expect_message(
    classify_growth(
      .data = test_data,
      lenht_cm = lenht_cm,
      gest_days = gest_age,
      age_days = age_days,
      sex = sex,
      .outcomes = "wasting",
      .new = list(
          wasting = c("wlz", "wasting", "wasting_outliers")
      ),
      .verbose = TRUE # Silence message for this test
    ),
    regexp = "Wasting: Not computed \\(`weight_kg` not supplied\\)",
    class = "gigs_classify_growth_msg"
  )

  ## Wasting not successful; because length/height not supplied
  testthat::expect_message(
    classify_growth(
      .data = test_data,
      weight_kg = weight_kg,
      gest_days = gest_age,
      age_days = age_days,
      sex = sex,
      .outcomes = "wasting",
      .verbose = TRUE # Silence message for this test
    ),
    regexp = "Wasting: Not computed \\(`lenht_cm` not supplied\\)",
    class = "gigs_classify_growth_msg"
  )

  ## All unsuccessful
  testthat::expect_message(
    classify_growth(
      .data = test_data,
      gest_days = gest_age,
      age_days = age_days,
      sex = sex,
      .outcomes = c("sfga", "svn", "stunting", "wasting", "wfa", "headsize"),
      .verbose = TRUE # Silence message for this test
    ),
    class = "gigs_classify_growth_msg"
  )
})
