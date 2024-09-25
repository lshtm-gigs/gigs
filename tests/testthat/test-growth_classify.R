#' @srrstats {G5.1} Users can run this function to make their own data set for
#'   test confirmation
test_data <- gigs_random_growth_dataset(10000, seed = 11, restrict = TRUE)

test_data_birth <- test_data[with(test_data, !is.na(bweight_centile_exp)),]
test_data_postnatal <- test_data[with(test_data, age_days > 3), ]
test_data_postnatal_wlz <- test_data_postnatal[
    !is.na(with(test_data_postnatal, weight_kg_from_wlz)),
]

#' @srrstats {EA6.0, EA6.0a, EA6.0b, EA6.0c, EA6.0d} This function checks the
#' output type, output dimensions, column names, and the types of the new
#' columns in tabular output.
check_classify_fn_output <- function(old, new, new_names) {
  checkmate::expect_data_frame(
    new, nrows = nrow(old), ncols = ncol(old) + length(new_names)
  )
  testthat::expect_equal(colnames(new), c(colnames(old), new_names))

  checkmate::expect_numeric(new[[new_names[1]]]) # First new col is continuous
  for (name in new_names[2:length(new_names)]) {
    checkmate::expect_factor(new[[name]]) # All other new cols are factors
  }
}

# Testing `classify_sfga()` ----------------------------------------------------

#' @srrstats {G5.4, G5.4c} Tests to ensure that `classify_sfga()` reproduces
#'   expected z-scores from randomly generated growth data, and the appropriate
#'   categorical data.
test_that(desc = "classify_sfga() reproduces expected z-scores", {
  new_names <- c("bweight_centile_test", "sfga", "sfga_severe")
  sfga <- classify_sfga(
    .data = test_data_birth,
    weight_kg = weight_kg,
    gest_days = gest_age,
    sex = sex,
    .new = new_names
  )
  check_classify_fn_output(test_data_birth, sfga, new_names)

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
  new_names <- c("bweight_centile_test", "svn")
  svn <- classify_svn(
    .data = test_data_birth,
    weight_kg = weight_kg,
    gest_days = gest_age,
    sex = sex,
    .new = new_names
  )
  check_classify_fn_output(test_data_birth, svn, new_names)

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
  new_names <- c("lhaz_test", "stunting", "stunting_outliers")
  stunting <- classify_stunting(
    .data = test_data[with(test_data, !is.na(lenht_cm)), ],
    lenht_cm = lenht_cm,
    gest_days = gest_age,
    age_days = age_days,
    sex = sex,
    id = id,
    .new = new_names
  )
  check_classify_fn_output(test_data[with(test_data, !is.na(lenht_cm)), ],
                           stunting, new_names)

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
  new_names <- c("wlz_test", "wasting", "wasting_outliers")
  wasting <- classify_wasting(
    .data = test_data_postnatal_wlz,
    weight_kg = weight_kg_from_wlz,
    lenht_cm = lenht_cm,
    gest_days = gest_age,
    age_days = age_days,
    sex = sex,
    id = id,
    .new = new_names
  )
  check_classify_fn_output(test_data_postnatal_wlz, wasting, new_names)

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
  new_names <- c("waz_test", "wfa", "wfa_outliers")
  wfa <- classify_wfa(
    .data = test_data,
    weight_kg = weight_kg,
    gest_days = gest_age,
    age_days = age_days,
    sex = sex,
    id = id,
    .new = new_names
  )
  check_classify_fn_output(test_data, wfa, new_names)

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
  new_names <- c("hcaz_test", "headsize")
  headsize <- classify_headsize(
    .data = test_data,
    headcirc_cm = headcirc_cm,
    gest_days = gest_age,
    age_days = age_days,
    sex = sex,
    id = id,
    .new = c("hcaz_test", "headsize")
  )
  check_classify_fn_output(test_data, headsize, new_names)


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

#' @srrstats {EA6.0, EA6.0a, EA6.0b, EA6.0c, EA6.0d} This function checks the
#' output type, output dimensions, column names, and the types of the new
#' columns in tabular output - but is specific to classify_growth().
check_classify_growth_output <- function(old, new, .outcomes, .new) {
  new_names <- .new[.outcomes]
  if ("sfga" %in% .outcomes & "svn" %in% .outcomes) {
    new_names[["svn"]][1] <- new_names[["sfga"]][1]
    continuous_cols <- vapply(X = unname(new_names[names(new_names) != "svn"]),
                            FUN = \(x) x[1],
                            FUN.VALUE = character(1L))
  } else {
    continuous_cols <- vapply(X = unname(new_names),
                              FUN = \(x) x[1],
                              FUN.VALUE = character(1L))
  }
  continuous_cols <- continuous_cols |>
    vctrs::vec_as_names(repair = "universal_quiet")
  categorical_cols <- lapply(X = unname(new_names),
                             FUN = \(x) x[2:length(x)]) |>
    unlist() |>
    vctrs::vec_as_names(repair = "universal_quiet")
  for (col_name in continuous_cols) {
    checkmate::expect_numeric(new[[col_name]])
  }
  for (col_name in categorical_cols) {
    checkmate::expect_factor(new[[col_name]])
  }
  n_new_cols <- length(c(continuous_cols, categorical_cols))
  checkmate::expect_data_frame(
    new, nrows = nrow(old), ncols = ncol(old) + n_new_cols
  )
  if ("sfga" %in% .outcomes & "svn" %in% .outcomes) {
    new_names[["svn"]] <- new_names[["svn"]][-1]
  }
  testthat::expect_equal(colnames(new), c(colnames(old), unlist(new_names)),
                         ignore_attr = TRUE)
}

#' @srrstats {G5.4, G5.4c} Tests to ensure that `classify_growth()` reproduces
#'   expected z-scores from randomly generated growth data.
test_that(desc = "`classify_growth()` reproduces expected z-scores", {
  outcomes <- c("sfga", "svn", "stunting", "wfa", "headsize")
  new <- list(
      sfga = c("bweight_centile_test", "sfga", "sfga_severe"),
      svn = c("bweight_centile_test", "svn"),
      stunting = c("lhaz_test", "stunting", "stunting_outliers"),
      wfa = c("waz_test", "wfa", "wfa_outliers"),
      headsize = c("hcaz_test", "headsize")
    )
  growth <- classify_growth(
    .data = test_data,
    weight_kg = weight_kg,
    lenht_cm = lenht_cm,
    headcirc_cm = headcirc_cm,
    gest_days = gest_age,
    age_days = age_days,
    sex = sex,
    id = id,
    .outcomes = outcomes,
    # Also demonstrates column renaming with .new
    .new = new,
    .verbose = FALSE # Silence message for this test
  )

  check_classify_growth_output(old = test_data,
                               new = growth,
                               .outcomes = outcomes,
                               .new = new)

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


# Ensure full coverage on `repair_.new_names()` --------------------------------

test_that(desc = "`repair_.new_names()` can throw an internal error", {
  ## Wasting not successful; because length/height not supplied
  testthat::expect_error(
    repair_.new_names(.new = list("sfga" = c("test1", "test2", "test3")),
                      mode = "gorp"),
    class = "gigs_internal_repair_mode_incorrect"
  )
})