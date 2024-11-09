#' @srrstats {G5.1} Users can run this function to make their own data set for
#'   test confirmation
test_data <- gigs_random_growth_dataset(10000, seed = 11, restrict = TRUE) |>
  # We then add categorical vars using clasisify_growth(), whose function is
  # validated in test-growth_classify.R
  classify_growth(
    gest_days = gest_age,
    age_days = age_days,
    sex = sex,
    weight_kg = weight_kg,
    lenht_cm = lenht_cm,
    headcirc_cm = headcirc_cm,
    id = id,
    .outcomes = c("sfga", "svn", "stunting", "wasting", "wfa", "headsize"),
    .new = list(
      sfga = c("DROP1", "sfga_exp", "sfga_sev_exp"),
      svn = c("DROP1", "svn_exp"),
      stunting = c("DROP2", "stunting_exp", "stunting_out_exp"),
      wasting = c("DROP3", "wasting_exp", "wasting_out_exp"),
      wfa = c("DROP4", "wfa_exp", "wfa_out_exp"),
      headsize = c("DROP5", "headsize_exp")
    ),
    .verbose = FALSE # Silence message for this test
  ) |>
  suppressWarnings() # Used to suppress "gigs_zscoring_old_birth_obs" warning
test_data <- test_data[ , !grepl("^DROP|(ile|z)_exp$", colnames(test_data))]

test_data_birth <- test_data[with(test_data, !is.na(sfga_exp)), ]
test_data_postnatal <- test_data[with(test_data, age_days > 3), ]
test_data_postnatal_wlz <- test_data_postnatal[
    !is.na(with(test_data_postnatal, weight_kg_from_wlz)),
]

# Testing `compute_sfga()` -----------------------------------------------------

#' @srrstats {G5.4, G5.4c} Tests to ensure that `compute_sfga()` reproduces
#'   expected appropriate categorical data.
test_that(desc = "compute_sfga() reproduces expected growth categories", {
  sfga_test <- with(test_data_birth, {
    compute_sfga(
      weight_kg = weight_kg,
      gest_days = gest_age,
      sex = sex
    )
  })

  sfga_sev_test <- with(test_data_birth, {
    compute_sfga(
      weight_kg = weight_kg,
      gest_days = gest_age,
      sex = sex,
      severe = TRUE
    )
  })

  testthat::expect_equal(sfga_test, test_data_birth$sfga_exp)
  testthat::expect_equal(sfga_sev_test, test_data_birth$sfga_sev_exp)
})

#' @srrstats {G5.2, G5.2a, G5.2b} Tests of message/warning/error behaviour
#'   in `compute_sfga()`.
test_that(desc = "compute_sfga() throws appropriate errors/warnings", {
  expected_length <- nrow(test_data_birth)
  withr::with_seed(seed = 452789, code = {
    n_missing_1 <- sample(1:(expected_length - 5), size = 1)
    n_missing_2 <- sample(1:(expected_length - 5), size = 1)
    n_missing_3 <- sample(1:(expected_length - 5), size = 1)
  })

  # For missing data (i.e. `NA`)
  testthat::expect_warning(
    with(test_data_birth, {
      compute_sfga(
        weight_kg = replace(weight_kg, 1:n_missing_1, values = NA),
        gest_days = replace(gest_age, 1:n_missing_2, values = NA),
        sex = replace(sex, 1:n_missing_3, values = NA)
      )
    }),
    regexp = test_msg_missing("weight_kg", expected_length, n_missing_1),
    fixed = TRUE
  )
  testthat::expect_warning(
    with(test_data_birth, {
      compute_sfga(
        weight_kg = replace(weight_kg, 1:n_missing_1, values = NA),
        gest_days = replace(gest_age, 1:n_missing_2, values = NA),
        sex = replace(sex, 1:n_missing_3, values = NA),
        severe = TRUE
      )
    }),
    regexp = test_msg_missing("gest_days", expected_length, n_missing_2),
    fixed = TRUE
  )
  testthat::expect_warning(
    with(test_data_birth, {
      compute_sfga(
        weight_kg = replace(weight_kg, 1:n_missing_1, values = NA),
        gest_days = replace(gest_age, 1:n_missing_2, values = NA),
        sex = replace(sex, 1:n_missing_3, values = NA),
      )
    }),
    regexp = test_msg_missing("sex", expected_length, n_missing_3),
    fixed = TRUE
  )

  # For bad sex data (i.e. not "M" or "F")
  testthat::expect_warning(
    with(test_data_birth, {
      compute_sfga(
        weight_kg = weight_kg,
        gest_days = gest_age,
        sex = replace(sex, 1:n_missing_2, values = "X")
      )
    }),
    regexp = test_msg_sex_invalid(expected_length, n_missing_2)
  )

  # For undefined input data (i.e. `NaN` or Inf)
  testthat::expect_warning(
    with(test_data_birth, {
      compute_sfga(
        weight_kg = weight_kg,
        gest_days = replace(gest_age, 1:n_missing_3, NaN),
        sex = sex
      )
    }),
    regexp = test_msg_undefined("gest_days", expected_length, n_missing_3)
  )
})

#' @srrstats {G5.8, G5.8a, G5.8b} Testing bad data types/zero-length input to
#'   `compute_sfga()`.
test_that(desc = "compute_sfga() errors on bad data types/zero-length input", {

  # Using the wrong data types
  testthat::expect_error(
    with(test_data_birth, {
      compute_sfga(
        weight_kg = as.complex(weight_kg),
        gest_days = gest_age,
        sex = sex,
        severe = TRUE
      )
    }),
    regexp = test_error_wrong_type("weight_kg", "numeric", "complex")
  )

  testthat::expect_error(
    with(test_data_birth, {
      compute_sfga(
        weight_kg = weight_kg,
        gest_days = as.character(gest_age),
        sex = sex,
        severe = FALSE
      )
    }),
    regexp = test_error_wrong_type("gest_days", "numeric", "character")
  )

  testthat::expect_error(
    with(test_data_birth, {
      compute_sfga(
        weight_kg = weight_kg,
        gest_days = gest_age,
        sex = as.factor(sex),
        severe = FALSE
      )
    }),
    regexp = test_error_wrong_type("sex", "character", "factor")
  )

  testthat::expect_error(
    with(test_data_birth, {
      compute_sfga(
        weight_kg = as.complex(weight_kg),
        gest_days = gest_age,
        sex = sex,
        severe = as.numeric(TRUE)
      )
    }),
    regexp = test_error_wrong_type("severe", "logical", "double")
  )

  # Zero-length data
  testthat::expect_error(
    with(test_data_birth, {
      compute_sfga(
        weight_kg = numeric(),
        gest_days = numeric(),
        sex = character()
      )
    }),
    class = "gigs_err_zero_length"
  )
})

# Testing `compute_svn()` ------------------------------------------------------

#' @srrstats {G5.4, G5.4c} Tests to ensure that `compute_svn()` reproduces
#'   expected appropriate categorical data.
test_that(desc = "compute_svn() reproduces expected growth categories", {
  svn_test <- with(test_data_birth, {
    compute_svn(
      weight_kg = weight_kg,
      gest_days = gest_age,
      sex = sex
    )
  })

  testthat::expect_equal(svn_test, test_data_birth$svn_exp)
})

#' @srrstats {G5.2, G5.2a, G5.2b} Tests of message/warning/error behaviour
#'   in `compute_svn()`.
test_that(desc = "compute_svn() throws appropriate errors/warnings", {
  expected_length <- nrow(test_data_birth)
  withr::with_seed(seed = 567865, code = {
    n_missing_1 <- sample(1:(expected_length - 5), size = 1)
    n_missing_2 <- sample(1:(expected_length - 5), size = 1)
    n_missing_3 <- sample(1:(expected_length - 5), size = 1)
  })

  # For missing data (i.e. `NA`)
  testthat::expect_warning(
    with(test_data_birth, {
      compute_svn(
        weight_kg = replace(weight_kg, 1:n_missing_1, values = NA),
        gest_days = replace(gest_age, 1:n_missing_2, values = NA),
        sex = replace(sex, 1:n_missing_3, values = NA)
      )
    }),
    regexp = test_msg_missing("weight_kg", expected_length, n_missing_1),
    fixed = TRUE
  )
  testthat::expect_warning(
    with(test_data_birth, {
      compute_svn(
        weight_kg = replace(weight_kg, 1:n_missing_1, values = NA),
        gest_days = replace(gest_age, 1:n_missing_2, values = NA),
        sex = replace(sex, 1:n_missing_3, values = NA)
      )
    }),
    regexp = test_msg_missing("gest_days", expected_length, n_missing_2),
    fixed = TRUE
  )
  testthat::expect_warning(
    with(test_data_birth, {
      compute_svn(
        weight_kg = replace(weight_kg, 1:n_missing_1, values = NA),
        gest_days = replace(gest_age, 1:n_missing_2, values = NA),
        sex = replace(sex, 1:n_missing_3, values = NA)
      )
    }),
    regexp = test_msg_missing("sex", expected_length, n_missing_3),
    fixed = TRUE
  )

  # For bad sex data (i.e. not "M" or "F")
  testthat::expect_warning(
    with(test_data_birth, {
      compute_svn(
        weight_kg = weight_kg,
        gest_days = gest_age,
        sex = replace(sex, 1:n_missing_2, values = "X")
      )
    }),
    regexp = test_msg_sex_invalid(expected_length, n_missing_2)
  )

  # For undefined input data (i.e. `NaN` or Inf)
  testthat::expect_warning(
    with(test_data_birth, {
      compute_svn(
        weight_kg = weight_kg,
        gest_days = replace(gest_age, 1:n_missing_3, NaN),
        sex = sex
      )
    }),
    regexp = test_msg_undefined("gest_days", expected_length, n_missing_3)
  )
})

#' @srrstats {G5.8, G5.8a, G5.8b} Testing bad data inputs to `compute_svn()`.
test_that(desc = "compute_svn() errors on bad data types", {
  # Using the wrong data types
  testthat::expect_error(
    with(test_data_birth, {
      compute_svn(
        weight_kg = as.complex(weight_kg),
        gest_days = gest_age,
        sex = sex
      )
    }),
    regexp = test_error_wrong_type("weight_kg", "numeric", "complex")
  )

  testthat::expect_error(
    with(test_data_birth, {
      compute_svn(
        weight_kg = weight_kg,
        gest_days = as.character(gest_age),
        sex = sex
      )
    }),
    regexp = test_error_wrong_type("gest_days", "numeric", "character")
  )

  testthat::expect_error(
    with(test_data_birth, {
      compute_svn(
        weight_kg = weight_kg,
        gest_days = gest_age,
        sex = as.factor(sex)
      )
    }),
    regexp = test_error_wrong_type("sex", "character", "factor")
  )

  # Zero-length data
  testthat::expect_error(
    with(test_data_birth, {
      compute_svn(
        weight_kg = numeric(),
        gest_days = gest_age,
        sex = character()
      )
    }),
    class = "gigs_err_zero_length"
  )
})

# Testing `compute_stunting()` -------------------------------------------------

#' @srrstats {G5.4, G5.4c} Tests to ensure that `compute_stunting()` reproduces
#'   expected appropriate categorical data.
test_that(desc = "compute_stunting() reproduces expected growth categories", {
  stunting_test <- with(test_data_postnatal, {
    compute_stunting(
      lenht_cm = lenht_cm,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      id = id
    )
  })

  stunting_out_test <- with(test_data_postnatal, {
    compute_stunting(
      lenht_cm = lenht_cm,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      id = id,
      outliers = TRUE
    )
  })

  testthat::expect_equal(stunting_test, test_data_postnatal$stunting_exp)
  testthat::expect_equal(stunting_out_test, test_data_postnatal$stunting_out_exp)
})

#' @srrstats {G5.2, G5.2a, G5.2b} Tests of message/warning/error behaviour
#'   in `compute_stunting()`.
test_that(desc = "compute_stunting() throws appropriate errors/warnings", {
  expected_length <- nrow(test_data_postnatal)
  withr::with_seed(seed = 904813, code = {
    n_missing_1 <- sample(1:(expected_length - 5), size = 1)
    n_missing_2 <- sample(1:(expected_length - 5), size = 1)
    n_missing_3 <- sample(1:(expected_length - 5), size = 1)
    n_missing_4 <- sample(1:(expected_length - 5), size = 1)
    n_missing_5 <- sample(1:(expected_length - 5), size = 1)
  })

  # For missing data (i.e. `NA`)
  testthat::expect_warning(
    with(test_data_postnatal, {
      compute_stunting(
        lenht_cm = replace(lenht_cm, 1:n_missing_4, values = NA),
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = id,
        outliers = TRUE
      )
    }),
    regexp = test_msg_missing("lenht_cm", expected_length, n_missing_4),
    fixed = TRUE
  )
  testthat::expect_warning(
    with(test_data_postnatal, {
      compute_stunting(
        lenht_cm = lenht_cm,
        age_days = age_days,
        gest_days = gest_age,
        sex = replace(sex, 1:n_missing_1, values = NA),
        id = id,
        outliers = TRUE
      )
    }),
    regexp = test_msg_missing("sex", expected_length, n_missing_1),
    fixed = TRUE
  )

  # For ID, including `NA` values causes an error.
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_stunting(
        lenht_cm = lenht_cm,
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = replace(id, 1:n_missing_4, values = NA),
        outliers = TRUE
      )
    }),
    regexp = test_msg_missing("id", expected_length, n_missing_4),
    fixed = TRUE
  )

  # For bad sex data (i.e. not "M" or "F")
  testthat::expect_warning(
    with(test_data_postnatal, {
      compute_stunting(
        lenht_cm = lenht_cm,
        age_days = age_days,
        gest_days = gest_age,
        sex = replace(sex, 1:n_missing_3, values = "X"),
        id = id,
        outliers = TRUE
      )
    }),
    regexp = test_msg_sex_invalid(expected_length, n_missing_3)
  )

  # For undefined input data (i.e. `NaN` or Inf)
  testthat::expect_warning(
    with(test_data_postnatal, {
      compute_stunting(
        lenht_cm = lenht_cm,
        age_days = age_days,
        gest_days = replace(gest_age, 1:n_missing_5, values = Inf),
        sex = sex,
        id = id,
        outliers = TRUE
      )
    }),
    regexp = test_msg_undefined("gest_days", expected_length, n_missing_5)
  )
})

#' @srrstats {G5.8, G5.8a, G5.8b} Testing bad data inputs to `compute_stunting()`.
test_that(desc = "compute_stunting() errors on bad data types", {
  # Using the wrong data types
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_stunting(
        lenht_cm = as.complex(lenht_cm),
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = id,
        outliers = TRUE
      )
    }),
    regexp = test_error_wrong_type("lenht_cm", "numeric", "complex")
  )
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_stunting(
        lenht_cm = lenht_cm,
        age_days = as.character(age_days),
        gest_days = gest_age,
        sex = sex,
        id = id,
        outliers = TRUE
      )
    }),
    regexp = test_error_wrong_type("age_days", "numeric", "character")
  )
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_stunting(
        lenht_cm = lenht_cm,
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = 1L,
        outliers = TRUE
      )
    }),
    regexp = test_error_wrong_type("id", "factor", "integer")
  )
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_stunting(
        lenht_cm = lenht_cm,
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = id,
        outliers = 1L
      )
    }),
    regexp = test_error_wrong_type("outliers", "logical", "integer")
  )

  # Zero-length data
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_stunting(
        lenht_cm = numeric(),
        age_days = age_days,
        gest_days = numeric(),
        sex = sex,
        id = factor(),
        outliers = TRUE
      )
    }),
    class = "gigs_err_zero_length"
  )
})

# Testing `compute_wasting()` --------------------------------------------------

#' @srrstats {G5.4, G5.4c} Tests to ensure that `compute_wasting()` reproduces
#'   expected appropriate categorical data.
test_that(desc = "compute_wasting() reproduces expected growth categories", {
  wasting_test <- with(test_data_postnatal, {
    compute_wasting(
      weight_kg = weight_kg,
      lenht_cm = lenht_cm,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      id = id
    )
  })

  wasting_out_test <- with(test_data_postnatal, {
    compute_wasting(
      weight_kg = weight_kg,
      lenht_cm = lenht_cm,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      id = id,
      outliers = TRUE
    )
  })

  testthat::expect_equal(wasting_test, test_data_postnatal$wasting_exp)
  testthat::expect_equal(wasting_out_test, test_data_postnatal$wasting_out_exp)
})

#' @srrstats {G5.2, G5.2a, G5.2b} Tests of message/warning/error behaviour
#'   in `compute_wasting()`.
test_that(desc = "compute_wasting() throws appropriate errors/warnings", {
  expected_length <- nrow(test_data_postnatal)
  withr::with_seed(seed = 367495, code = {
    n_missing_1 <- sample(1:(expected_length - 5), size = 1)
    n_missing_2 <- sample(1:(expected_length - 5), size = 1)
    n_missing_3 <- sample(1:(expected_length - 5), size = 1)
    n_missing_4 <- sample(1:(expected_length - 5), size = 1)
    n_missing_5 <- sample(1:(expected_length - 5), size = 1)
  })

  # For missing data (i.e. `NA`)
  testthat::expect_warning(
    with(test_data_postnatal, {
      compute_wasting(
        weight_kg = replace(weight_kg, 1:n_missing_5, values = NA),
        lenht_cm = lenht_cm,
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = id,
        outliers = TRUE
      )
    }),
    regexp = test_msg_missing("weight_kg", expected_length, n_missing_5),
    fixed = TRUE
  )
  testthat::expect_warning(
    with(test_data_postnatal, {
      compute_wasting(
        weight_kg = weight_kg,
        lenht_cm = lenht_cm,
        age_days = age_days,
        gest_days = gest_age,
        sex = replace(sex, 1:n_missing_4, values = NA),
        id = id,
        outliers = TRUE
      )
    }),
    regexp = test_msg_missing("sex", expected_length, n_missing_4),
    fixed = TRUE
  )

  # For ID, including `NA` values causes an error.
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_wasting(
        weight_kg = weight_kg,
        lenht_cm = lenht_cm,
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = replace(id, 1:n_missing_4, values = NA),
        outliers = TRUE
      )
    }),
    regexp = test_msg_missing("id", expected_length, n_missing_4),
    fixed = TRUE
  )

  # For bad sex data (i.e. not "M" or "F")
  testthat::expect_warning(
    with(test_data_postnatal, {
      compute_wasting(
        weight_kg = weight_kg,
        lenht_cm = lenht_cm,
        age_days = age_days,
        gest_days = gest_age,
        sex = replace(sex, 1:n_missing_2, values = "X"),
        id = id,
        outliers = TRUE
      )
    }),
    regexp = test_msg_sex_invalid(expected_length, n_missing_2)
  )

  # For undefined input data (i.e. `NaN` or Inf)
  testthat::expect_warning(
    with(test_data_postnatal, {
      compute_wasting(
        weight_kg = weight_kg,
        lenht_cm = lenht_cm,
        age_days = age_days,
        gest_days = replace(gest_age, 1:n_missing_2, values = Inf),
        sex = sex,
        id = id,
        outliers = TRUE
      )
    }),
    regexp = test_msg_undefined("gest_days", expected_length, n_missing_2)
  )
})

#' @srrstats {G5.8, G5.8a, G5.8b} Testing bad data inputs to `compute_wasting()`.
test_that(desc = "compute_wasting() errors on bad data types", {
  # Using the wrong data types
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_wasting(
        weight_kg = as.complex(weight_kg),
        lenht_cm = lenht_cm,
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = id,
        outliers = TRUE
      )
    }),
    regexp = test_error_wrong_type("weight_kg", "numeric", "complex")
  )
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_wasting(
        weight_kg = weight_kg,
        lenht_cm = lenht_cm,
        age_days = as.character(age_days),
        gest_days = gest_age,
        sex = sex,
        id = id,
        outliers = TRUE
      )
    }),
    regexp = test_error_wrong_type("age_days", "numeric", "character")
  )
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_wasting(
        weight_kg = weight_kg,
        lenht_cm = lenht_cm,
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = 1L,
        outliers = TRUE
      )
    }),
    regexp = test_error_wrong_type("id", "factor", "integer")
  )
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_wasting(
        weight_kg = weight_kg,
        lenht_cm = lenht_cm,
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = id,
        outliers = 1L
      )
    }),
    regexp = test_error_wrong_type("outliers", "logical", "integer")
  )

  # Zero-length data
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_wasting(
        weight_kg = weight_kg,
        lenht_cm = numeric(),
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = factor(),
        outliers = TRUE
      )
    }),
    class = "gigs_err_zero_length"
  )
})

# Testing `compute_wfa()` ------------------------------------------------------

#' @srrstats {G5.4, G5.4c} Tests to ensure that `compute_wfa()` reproduces
#'   expected appropriate categorical data.
test_that(desc = "compute_wfa() reproduces expected growth categories", {
  wfa_test <- with(test_data_postnatal, {
    compute_wfa(
      weight_kg = weight_kg,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      id = id
    )
  })

  wfa_out_test <- with(test_data_postnatal, {
    compute_wfa(
      weight_kg = weight_kg,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      id = id,
      outliers = TRUE
    )
  })

  testthat::expect_equal(wfa_test, test_data_postnatal$wfa_exp)
  testthat::expect_equal(wfa_out_test, test_data_postnatal$wfa_out_exp)
})

#' @srrstats {G5.2, G5.2a, G5.2b} Tests of message/warning/error behaviour
#'   in `compute_wfa()`.
test_that(desc = "compute_wfa() throws appropriate errors/warnings", {
  expected_length <- nrow(test_data_postnatal)
  withr::with_seed(seed = 346633, code = {
    n_missing_1 <- sample(1:(expected_length - 5), size = 1)
    n_missing_2 <- sample(1:(expected_length - 5), size = 1)
    n_missing_3 <- sample(1:(expected_length - 5), size = 1)
    n_missing_4 <- sample(1:(expected_length - 5), size = 1)
    n_missing_5 <- sample(1:(expected_length - 5), size = 1)
  })

  # For missing data (i.e. `NA`)
  testthat::expect_warning(
    with(test_data_postnatal, {
      compute_wfa(
        weight_kg = replace(weight_kg, 1:n_missing_1, values = NA),
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = id,
        outliers = TRUE
      )
    }),
    regexp = test_msg_missing("weight_kg", expected_length, n_missing_1),
    fixed = TRUE
  )
  testthat::expect_warning(
    with(test_data_postnatal, {
      compute_wfa(
        weight_kg = weight_kg,
        age_days = age_days,
        gest_days = gest_age,
        sex = replace(sex, 1:n_missing_3, values = NA),
        id = id,
        outliers = TRUE
      )
    }),
    regexp = test_msg_missing("sex", expected_length, n_missing_3),
    fixed = TRUE
  )

  # For ID, including `NA` values causes an error.
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_wfa(
        weight_kg = weight_kg,
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = replace(id, 1:n_missing_2, values = NA),
        outliers = TRUE
      )
    }),
    regexp = test_msg_missing("id", expected_length, n_missing_2),
    fixed = TRUE
  )

  # For bad sex data (i.e. not "M" or "F")
  testthat::expect_warning(
    with(test_data_postnatal, {
      compute_wfa(
        weight_kg = weight_kg,
        age_days = age_days,
        gest_days = gest_age,
        sex = replace(sex, 1:n_missing_2, values = "X"),
        id = id,
        outliers = TRUE
      )
    }),
    regexp = test_msg_sex_invalid(expected_length, n_missing_2)
  )

  # For undefined input data (i.e. `NaN` or Inf)
  testthat::expect_warning(
    with(test_data_postnatal, {
      compute_wfa(
        weight_kg = weight_kg,
        age_days = age_days,
        gest_days = replace(gest_age, 1:n_missing_2, values = Inf),
        sex = sex,
        id = id,
        outliers = TRUE
      )
    }),
    regexp = test_msg_undefined("gest_days", expected_length, n_missing_2)
  )
})

#' @srrstats {G5.8, G5.8a, G5.8b} Testing bad data inputs to `compute_wfa()`.
test_that(desc = "compute_wfa() errors on bad data types", {
  # Using the wrong data types
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_wfa(
        weight_kg = as.complex(weight_kg),
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = id,
        outliers = TRUE
      )
    }),
    regexp = test_error_wrong_type("weight_kg", "numeric", "complex")
  )
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_wfa(
        weight_kg = weight_kg,
        age_days = as.character(age_days),
        gest_days = gest_age,
        sex = sex,
        id = id,
        outliers = TRUE
      )
    }),
    regexp = test_error_wrong_type("age_days", "numeric", "character")
  )
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_wfa(
        weight_kg = weight_kg,
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = 1L,
        outliers = TRUE
      )
    }),
    regexp = test_error_wrong_type("id", "factor", "integer")
  )
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_wfa(
        weight_kg = weight_kg,
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = id,
        outliers = 1L
      )
    }),
    regexp = test_error_wrong_type("outliers", "logical", "integer")
  )

  # Zero-length data
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_wfa(
        weight_kg = weight_kg,
        age_days = age_days,
        gest_days = numeric(),
        sex = sex,
        id = factor(),
        outliers = TRUE
      )
    }),
    class = "gigs_err_zero_length"
  )
})

# Testing `compute_headsize()` -------------------------------------------------

#' @srrstats {G5.4, G5.4c} Tests to ensure that `compute_headsize()` reproduces
#'   expected appropriate categorical data.
test_that(desc = "compute_headsize() reproduces expected growth categories", {
  headsize_test <- with(test_data_postnatal, {
    compute_headsize(
      headcirc_cm = headcirc_cm,
      age_days = age_days,
      gest_days = gest_age,
      sex = sex,
      id = id
    )
  })

  testthat::expect_equal(headsize_test, test_data_postnatal$headsize_exp)
})

#' @srrstats {G5.2, G5.2a, G5.2b} Tests of message/warning/error behaviour
#'   in `compute_headsize()`.
test_that(desc = "compute_headsize() throws appropriate errors/warnings", {
  expected_length <- nrow(test_data_postnatal)
  withr::with_seed(seed = 278564, code = {
    n_missing_1 <- sample(1:(expected_length - 5), size = 1)
    n_missing_2 <- sample(1:(expected_length - 5), size = 1)
    n_missing_3 <- sample(1:(expected_length - 5), size = 1)
    n_missing_4 <- sample(1:(expected_length - 5), size = 1)
    n_missing_5 <- sample(1:(expected_length - 5), size = 1)
  })

  # For missing data (i.e. `NA`)
  testthat::expect_warning(
    with(test_data_postnatal, {
      compute_headsize(
        headcirc_cm = replace(headcirc_cm, 1:n_missing_1, values = NA),
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = id
      )
    }),
    regexp = test_msg_missing("headcirc_cm", expected_length, n_missing_1),
    fixed = TRUE
  )
  testthat::expect_warning(
    with(test_data_postnatal, {
      compute_headsize(
        headcirc_cm = headcirc_cm,
        age_days = age_days,
        gest_days = gest_age,
        sex = replace(sex, 1:n_missing_3, values = NA),
        id = id
      )
    }),
    regexp = test_msg_missing("sex", expected_length, n_missing_3),
    fixed = TRUE
  )

  # For ID, including `NA` values causes an error.
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_headsize(
        headcirc_cm = headcirc_cm,
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = replace(id, 1:n_missing_2, values = NA)
      )
    }),
    regexp = test_msg_missing("id", expected_length, n_missing_2),
    fixed = TRUE
  )

  # For bad sex data (i.e. not "M" or "F")
  testthat::expect_warning(
    with(test_data_postnatal, {
      compute_headsize(
        headcirc_cm = headcirc_cm,
        age_days = age_days,
        gest_days = gest_age,
        sex = replace(sex, 1:n_missing_2, values = "X"),
        id = id
      )
    }),
    regexp = test_msg_sex_invalid(expected_length, n_missing_2)
  )

  # For undefined input data (i.e. `NaN` or Inf)
  testthat::expect_warning(
    with(test_data_postnatal, {
      compute_headsize(
        headcirc_cm = headcirc_cm,
        age_days = age_days,
        gest_days = replace(gest_age, 1:n_missing_2, values = Inf),
        sex = sex,
        id = id
      )
    }),
    regexp = test_msg_undefined("gest_days", expected_length, n_missing_2)
  )
})

#' @srrstats {G5.8, G5.8a, G5.8b} Testing bad data inputs to `compute_headsize()`.
test_that(desc = "compute_headsize() errors on bad data types", {
  # Using the wrong data types
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_headsize(
        headcirc_cm = as.complex(headcirc_cm),
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = id
      )
    }),
    regexp = test_error_wrong_type("headcirc_cm", "numeric", "complex")
  )
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_headsize(
        headcirc_cm = headcirc_cm,
        age_days = as.character(age_days),
        gest_days = gest_age,
        sex = sex,
        id = id
      )
    }),
    regexp = test_error_wrong_type("age_days", "numeric", "character")
  )
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_headsize(
        headcirc_cm = headcirc_cm,
        age_days = age_days,
        gest_days = gest_age,
        sex = sex,
        id = 1L
      )
    }),
    regexp = test_error_wrong_type("id", "factor", "integer")
  )
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_headsize(
        headcirc_cm = headcirc_cm,
        age_days = age_days,
        gest_days = gest_age,
        sex = as.factor(sex),
        id = id
      )
    }),
    regexp = test_error_wrong_type("sex", "character", "factor")
  )

  # Zero-length data
  testthat::expect_error(
    with(test_data_postnatal, {
      compute_headsize(
        headcirc_cm = headcirc_cm,
        age_days = numeric(),
        gest_days = numeric(),
        sex = sex,
        id = factor()
      )
    }),
    class = "gigs_err_zero_length"
  )
})
