# Correctness tests ------------------------------------------------------------

#' @srrstats {G5.4, G5.6, G5.9, G5.9a} Test correctness of SfGA classifications
#'   against `life6mo` dataset (incl. fixed seeds + trivial noise).
test_that(
  desc = "Size for GA classification works - with and without `severe` flag",
  code = {
    sfga_df <- life6mo[life6mo$age_days == 0, ]
    sfga_exp <- factor(c("SGA", "SGA", "SGA", "LGA", "AGA", "SGA", "AGA",
                         "AGA", "AGA", "SGA", "SGA", "SGA", "AGA", "LGA",
                         "SGA", "AGA", "SGA", "SGA", "SGA", "SGA", "AGA",
                         "AGA", "AGA", "SGA", "SGA", "SGA", "AGA", "AGA",
                         "SGA", "SGA", "SGA", "SGA", "SGA", "SGA", "SGA",
                         "SGA", "SGA", "SGA", "AGA", "SGA", "AGA", "SGA",
                         "SGA", "SGA", "SGA", "SGA", "SGA", "SGA", "AGA",
                         "SGA", "AGA", "SGA", "AGA", "SGA", "SGA", "SGA",
                         "SGA"),
                       levels = c("SGA", "AGA",  "LGA"))
    sfga_severe_exp <- factor(c("SGA(<3)", "SGA", "SGA(<3)", "LGA", "AGA",
                                "SGA", "AGA", "AGA", "AGA", "SGA", "SGA(<3)",
                                "SGA(<3)", "AGA", "LGA", "SGA(<3)", "AGA",
                                "SGA", "SGA", "SGA", "SGA", "AGA", "AGA", "AGA",
                                "SGA", "SGA", "SGA(<3)", "AGA", "AGA",
                                "SGA(<3)", "SGA", "SGA", "SGA", "SGA(<3)",
                                "SGA(<3)", "SGA(<3)", "SGA", "SGA", "SGA",
                                "AGA", "SGA(<3)", "AGA", "SGA", "SGA",
                                "SGA(<3)", "SGA(<3)", "SGA", "SGA(<3)", "SGA",
                                "AGA", "SGA(<3)", "AGA", "SGA(<3)", "AGA",
                                "SGA(<3)", "SGA(<3)", "SGA(<3)", "SGA(<3)"),
                        levels = c("SGA(<3)", "SGA", "AGA", "LGA"))

    # Without noise; with consistent seeds
    withr::with_seed(
      seed = 200,
      expect_equal(
        object = with(sfga_df,
                      classify_sfga(weight_kg = weight_g / 1000,
                                    sex = as.character(sex),
                                    gest_days = gestage,
                                    severe = FALSE)),
        expected = sfga_exp)
    )
    withr::with_seed(
      seed = 200,
      expect_equal(
        object = with(sfga_df,
                      classify_sfga(weight_kg = weight_g / 1000,
                                    sex = as.character(sex),
                                    gest_days = gestage,
                                    severe = TRUE)),
        expected = sfga_severe_exp)
    )

    # With noise
    noise <- .Machine$double.eps
    expect_equal(
      object = with(sfga_df,
                    classify_sfga(weight_kg = (weight_g / 1000) + noise,
                                  sex = as.character(sex),
                                  gest_days = gestage + noise,
                                  severe = FALSE)),
      expected = sfga_exp)
    expect_equal(
      object = with(sfga_df,
                     classify_sfga(weight_kg = weight_g / 1000 - noise,
                                   sex = as.character(sex),
                                   gest_days = gestage - noise,
                                   severe = TRUE)),
      expected = sfga_severe_exp)
  }
)


#' @srrstats {G5.4, G5.6, G5.9, G5.9a} Test correctness of SVN classifications
#'   against `life6mo` dataset (incl. fixed seeds + trivial noise).
test_that(
  desc = "SVN classification works - with and without `severe` flag",
  code = {
    svn_df <- life6mo[life6mo$age_days == 0, ]
    svn <- with(svn_df, classify_svn(weight_kg = weight_g / 1000,
                                     gest_days = gestage,
                                     sex = as.character(sex)))

    # Get SGA categories --> already tested above so will be accurate
    sfga <- with(svn_df, classify_sfga(weight_kg = weight_g / 1000,
                                       sex = as.character(sex),
                                       gest_days = gestage,
                                       severe = FALSE))

    svn_lvls <- c("Preterm SGA", "Preterm AGA", "Preterm LGA", "Term SGA",
                  "Term AGA", "Term LGA")
    expected_svn <- factor(ifelse(svn_df$gestage < 259,
                                  paste0("Preterm ", sfga),
                                  paste0("Term ", sfga)),
                           levels = svn_lvls)

    # Correctness test, with fixed seed
    withr::with_seed(250, code = {
      expect_equal(object = svn,
                   expected = factor(expected_svn, levels = levels(svn)))
    })

    # Correctness test, with trivial noise
    noise <- .Machine$double.eps
    svn_with_noise <- with(svn_df,
                           classify_svn(weight_kg = (weight_g / 1000) + noise,
                                        gest_days = gestage + noise,
                                        sex = as.character(sex)))

    withr::with_seed(250, code = {
      expect_equal(object = svn_with_noise,
                   expected = factor(expected_svn, levels = svn_lvls))
    })
  }
)

#' @srrstats {G5.4, G5.6, G5.9, G5.9a} Test correctness of stunting
#'   classification against temporary dataset, with .
# test_that(
#   desc = "Stunting classification works",
#   code = {
#     # CASE: male; pre-term, below age cutoff, z = ~ -3.29
#     # CASE: female; pre-term, above age cutoff, z = ~ -1.40
#     # CASE: male; pre-term, below age cutoff, z = ~ -12.60
#     # CASE: female; term, z = ~ -1.58
#     # CASE: male; term, z = ~ 0.98
#     # CASE: female --> missing data
#     stunting_df <- data.frame(lenht = c(57.5, 73.6, 44.1, 72.4, 72.83, 87.4),
#                               ga_at_birth = c(238, 245, 252, 259, 266, 285),
#                               age_days = c(180, 455, 294, 525, 245, NA),
#                               psex = c("M", "F", "M", "F", "M", "F"))
#     stunting <- with(stunting_df,
#         classify_stunting(lenht_cm = lenht, age_days = age_days,
#                           gest_days = ga_at_birth, sex = psex)
#     )
#     stunting_outliers <- with(
#       stunting_df,
#       classify_stunting(lenht_cm = lenht, age_days = age_days,
#                         gest_days = ga_at_birth, sex = psex, outliers = TRUE)
#     )
#
#     stunting_exp <- c("stunting_severe", "not_stunting", "stunting_severe",
#                       "stunting", "not_stunting", NA_character_)
#     stunting_outliers_exp <- c("stunting_severe", "not_stunting", "outlier",
#                                "stunting", "not_stunting", NA_character_)
#     expect_equal(
#       object = stunting,
#       expected = factor(stunting_exp,
#                         levels = c("stunting_severe", "stunting",
#                                    "not_stunting")
#       ))
#
#     expect_equal(
#       object = stunting_outliers,
#       expected = factor(stunting_outliers_exp,
#                         levels = c("stunting_severe", "stunting",
#                                    "not_stunting", "outlier")
#     ))
# })

#
#
# test_that("Wasting classification works", code = {
#   wasting_df <- data.frame(wght_kg = c(0.45, 1.4, 2.68, 3.94, 7.09, 2.24, 7.48,
#                                        6.75, 16.86, 7.68, 30.53),
#                            lenht =   c(43.85, 34.5, 43.85, 59.76, 67.7, NA,
#                                        65.4, 93.52, 100.38, 91.41, 110.39),
#                            psex = rep_len(c("F", "M"), length.out = 11),
#                            ga_days = c(251, 197, 225, 243, 277, 236, 283,
#                                        NA_integer_, 293, 298, 287),
#                            age = c(1, 2, 76, 120, 2, 267, 154, 980, 1343, 892,
#                                    1850))
#   wasting <- with(
#     wasting_df,
#     classify_wasting(weight_kg = wght_kg, lenht_cm = lenht, sex = psex,
#                      gest_days = ga_days, age_days = age))
#   wasting_outliers <- with(
#     wasting_df,
#     classify_wasting(weight_kg = wght_kg, lenht_cm = lenht, sex = psex,
#                      gest_days = ga_days, age_days = age, outliers = TRUE))
#   expect_equal(
#     object = wasting,
#     expected = factor(c("wasting_severe", NA, "not_wasting", "wasting",
#                         "not_wasting", NA, "not_wasting", NA, "not_wasting",
#                         "wasting_severe",
#                         "overweight"),
#                       levels =  c("wasting_severe", "wasting", "not_wasting",
#                                   "overweight")))
#   expect_equal(
#     object = wasting_outliers,
#     expected = factor(c("outlier", NA, "not_wasting", "wasting", "not_wasting",
#                         NA, "not_wasting", NA, "not_wasting", "outlier",
#                         "outlier"),
#                       levels =  c("wasting_severe", "wasting", "not_wasting",
#                                   "overweight", "outlier")))
# })
#
# test_that("Weight-for-age classification works", {
#   # AGE,  SEX  , GA_WKS, PREM?,   ZSCORE , CLASS
#   # 501, FEMALE,   27  ,   Y  , <2 ZSCORE, UNDER_W
#   # 323, MALE  ,   37  ,   N  , <3 ZSCORE, U_SEVERE
#   # 435, FEMALE,   36  ,   Y  , <6 ZSCORE, IMPLAUS
#   # 201, MALE  ,   40  ,   N  , -1 ZSCORE, NORMAL
#   # 707, FEMALE,   41  ,   N  , >6 ZSCORE, IMPLAUS
#   # 154, MALE  ,   28  ,   Y  , >2 ZSCORE, OVER_W
#   # 496, NA    ,   42  ,   N  , ~0 ZSCORE, NA
#   wfa_df <- data.frame(wght_kg =     c(7.2, 6.1, 2.1, 9.1, 24 , 9.4, 10.8),
#                        days_old =    c(501, 323, 435, 201, 707, 154, 496),
#                        ga_at_birth = c(27,  37,  36,  40,  41,  28,  42) * 7,
#                        psex = c(rep(c("F", "M"), 3), NA_character_))
#   wfa <- with(wfa_df,
#                classify_wfa(weight_kg = wght_kg,
#                             age_days = days_old,
#                             gest_days = ga_at_birth,
#                             sex = psex))
#   wfa_outliers <- with(wfa_df,
#                        classify_wfa(weight_kg = wght_kg,
#                                     age_days = days_old,
#                                     gest_days = ga_at_birth,
#                                     sex = psex,
#                                     outliers = TRUE))
#   expect_equal(
#     object = wfa,
#     expected = factor(c("underweight", "underweight_severe",
#                         "underweight_severe", "normal_weight", "overweight",
#                         "overweight", NA),
#                       levels =  c("underweight_severe", "underweight",
#                                   "normal_weight", "overweight")))
#   expect_equal(
#     object = wfa_outliers,
#     expected = factor(c("underweight", "underweight_severe",
#                         "outlier", "normal_weight", "outlier",
#                         "overweight", NA),
#                       levels =  c("underweight_severe", "underweight",
#                                   "normal_weight", "overweight", "outlier")))
# })

# Test error/warning behaviour with wrong input types or lengths ---------------

#' Generate an expected bad data type error from `{checkmate}`
#' @param name Single-length character vector with name of input.
#' @param wanted Single-length character vector with name of expected data
#'   type.
#' @param got Single-length character vector with name of received data
#'   type.
#' @description Used to test `{checkmate}` error messages. These errors are
#'   given by checkmate::assert_*()-style functions in check_params.R.
error_msg_wrong_type <- function(name, wanted, got) {
  paste0("Assertion on '", name, "' failed: Must be of type '", wanted,
         "', not '", got, "'.")
}

#' Generate an expected zero-length error from [validate_parameter_lengths]
#' @param name Name of input vector.
error_msg_zero_length <- function(names) {
  count <- length(names)
  var_str <- if(count > 1) "Variables" else "Variable"
  input_str <- if(count > 1) "Inputs" else "Input"
  varnames_str <- paste0(names, collapse = "', '")
  paste0(var_str, " '", varnames_str, "': ", input_str, " had length 0, ",
         "but must have length 1 or greater.")
}

#' Generate an expected zero-length error from `{checkmate}`
#' @param name Name of input vector.
error_msg_unrecyclable <- function(names) {
  count <- length(names)
  var_str <- if(count > 1) "Variables" else "Variable"
  input_str <- if(count > 1) "These inputs" else "This input"
  varname_str <- paste0(names, collapse = "', '")
  paste0(var_str, " '", varname_str, "': ", input_str, " cannot be ",
         "recycled with `vctrs\\:\\:vec_recycle_common\\(\\)`.")
}

#' @srrstats {G5.2, G5.2a, G5.2b, G5.8, G5.8a, G5.8b} Data length/type errors
#'   with classification functions
test_that(
  desc = "classify_sfga() displays appropriate error behaviour",
  code = {
    sfga_df <- life6mo[life6mo$age_days == 0, ]

    # Set up 'good' params
    sfga_weight <- sfga_df$weight_g / 1000
    sfga_gest <- sfga_df$gestage
    sfga_sex <- as.character(sfga_df$sex)
    sfga_severe <- TRUE

    # G5.8a --> zero-length data inputs cause errors
    expect_error(
      classify_sfga(double(), sfga_gest, sfga_sex, sfga_severe),
      error_msg_zero_length("weight_kg")
    )
    expect_error(
      classify_sfga(sfga_weight, double(), sfga_sex, sfga_severe),
      error_msg_zero_length("gest_days")
    )
    expect_error(
      classify_sfga(sfga_weight, sfga_gest, character(), sfga_severe),
      error_msg_zero_length("sex")
    )
    expect_error(
      classify_sfga(sfga_weight, sfga_gest, sfga_sex, logical()),
      "Assertion on 'severe' failed. Must be of length == 1, but has length 0."
    )
    expect_error(
      classify_sfga(sfga_weight, sfga_gest, sfga_sex, logical(3)),

      "Assertion on 'severe' failed. Must be of length == 1, but has length 3."
    )

    # G5.8b --> incorrect data types
    expect_error(
      classify_sfga(character(1), sfga_gest, sfga_sex, sfga_severe),
      error_msg_wrong_type("weight_kg", wanted = "numeric", got = "character")
    )
    expect_error(
      classify_sfga(sfga_weight, logical(1), sfga_sex, sfga_severe),
      error_msg_wrong_type("gest_days", wanted = "numeric", got = "logical")
    )
    expect_error(
      classify_sfga(sfga_weight, sfga_gest, integer(1), sfga_severe),
      error_msg_wrong_type("sex", wanted = "character", got = "integer")
    )
    expect_error(
      classify_sfga(sfga_weight, sfga_gest, sfga_sex, complex(1)),
      regexp = "Assertion on 'severe' failed. Must be of class 'logical'"
    )

    # G5.2, --> errors if vector inputs cannot be recycled
    expect_error(
      object = classify_sfga(character(2), sfga_gest, sfga_sex, sfga_severe),
      regexp = error_msg_unrecyclable(names = "weight_kg")
    )
    expect_error(
      object = classify_sfga(sfga_weight, numeric(3), sfga_sex, sfga_severe),
      regexp = error_msg_unrecyclable(names = "gest_days")
    )
    expect_error(
      object = classify_sfga(sfga_weight, sfga_gest, character(5), sfga_severe),
      regexp = error_msg_unrecyclable("sex")
    )
  }
)

test_that(
  desc = "classify_sfga() displays appropriate error behaviour",
  code = {
    svn_df <- life6mo[life6mo$age_days == 0, ]

    # Set up 'good' params
    svn_weight <- svn_df$weight_g / 1000
    svn_gest <- svn_df$gestage
    svn_sex <- as.character(svn_df$sex)

    # G5.8a --> zero-length data inputs cause errors
    expect_error(
      classify_svn(double(), svn_gest, svn_sex),
      error_msg_zero_length("weight_kg")
    )
    expect_error(
      classify_svn(svn_weight, double(), svn_sex),
      error_msg_zero_length("gest_days")
    )
    expect_error(
      classify_svn(svn_weight, svn_gest, character()),
      error_msg_zero_length("sex")
    )

    # G5.8b --> incorrect data types
    expect_error(
      classify_svn(character(1), svn_gest, svn_sex),
      error_msg_wrong_type("y", wanted = "numeric", got = "character")
    )
    expect_error(
      classify_svn(svn_weight, logical(1), svn_sex),
      error_msg_wrong_type("x", wanted = "numeric", got = "logical")
    )
    expect_error(
      classify_svn(svn_weight, svn_gest, integer(1)),
      error_msg_wrong_type("sex", wanted = "character", got = "integer")
    )

    # G5.2, --> errors if vector inputs cannot be recycled
    expect_error(
      object = classify_svn(character(2), svn_gest, svn_sex),
      regexp = error_msg_unrecyclable(names = "weight_kg")
    )
    expect_error(
      object = classify_svn(svn_weight, numeric(3), svn_sex),
      regexp = error_msg_unrecyclable(names = "gest_days")
    )
    expect_error(
      object = classify_svn(svn_weight, svn_gest, character(5)),
      regexp = error_msg_unrecyclable("sex")
    )
  }
)

# Test error/warning behaviour with edge-case inputs ---------------------------

# The full complement of error/warnings can be found in









# SRR tags ---------------------------------------------------------------------
#' @srrstats {G5.1} `life6mo` dataset used in this script is exported with
#'   package
NULL
