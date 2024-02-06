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
#'   classification against temporary dataset (incl. trivial noise)
test_that(
  desc = "Stunting classification works", code = {
    # CASE: male; pre-term, below age cutoff, z = ~ -3.45
    # CASE: female; pre-term, above age cutoff, z = ~ -1.40
    # CASE: male; pre-term, below age cutoff, z = ~ -11.88
    # CASE: female; term, z = ~ -2.63
    # CASE: male; term, z = ~ 0.98
    # CASE: female; term, z = ~ -0.09
    stunting_df <- data.frame(lenht = c(57.5, 73.6, 44.1, 72.4, 72.83, 87.4),
                              ga_at_birth = c(238, 245, 252, 259, 266, 285),
                              age_days = c(180, 455, 195, 525, 245, 800),
                              psex = c("M", "F", "M", "F", "M", "F"))
    stunting <- with(stunting_df,
        classify_stunting(lenht_cm = lenht, age_days = age_days,
                          gest_days = ga_at_birth, sex = psex)
    )
    stunting_outliers <- with(
      stunting_df,
      classify_stunting(lenht_cm = lenht, age_days = age_days,
                        gest_days = ga_at_birth, sex = psex, outliers = TRUE)
    )
    stunting_exp <- c("stunting_severe", "not_stunting", "stunting_severe",
                      "stunting", "not_stunting", "not_stunting")
    stunting_outliers_exp <- c("stunting_severe", "not_stunting", "outlier",
                                 "stunting", "not_stunting", "not_stunting")
    stunting_lvls <- c("stunting_severe", "stunting", "not_stunting")
    stunting_lvls_out <- c(stunting_lvls, "outlier")

    expect_equal(
      object = stunting,
      expected = factor(stunting_exp, levels = stunting_lvls)
    )
    expect_equal(
      object = stunting_outliers,
      expected = factor(stunting_outliers_exp, stunting_lvls_out)
    )
    noise <- sqrt(.Machine$double.eps)
    stunting_noisy <- with(stunting_df,
      classify_stunting(lenht_cm = lenht - noise,
                        age_days = age_days + noise,
                        gest_days = ga_at_birth + noise,
                        sex = psex)
    )
    stunting_outliers_noisy <- with(
      stunting_df,
      classify_stunting(lenht_cm = lenht + noise,
                        age_days = age_days - noise,
                        gest_days = ga_at_birth - noise,
                        sex = psex,
                        outliers = TRUE)
    )
    expect_equal(
      object = stunting_noisy,
      expected = factor(stunting_exp, levels = stunting_lvls)
    )
    expect_equal(
      object = stunting_outliers_noisy,
      expected = factor(stunting_outliers_exp, stunting_lvls_out)
    )
})

#' @srrstats {G5.4, G5.6, G5.9, G5.9a} Test correctness of stunting
#'   classification against temporary dataset (incl. trivial noise)
test_that(
  desc = "Wasting classification works", code = {
    standard <- c("who_gs", "ig_png", "ig_png", "who_gs", "who_gs", "who_gs")
    term <- c("term", "preterm", "preterm", "preterm", "term", "term")
    sex <- withr::with_seed(seed = 8, code = {
      sample(c("M", "F"), size = 6, replace = TRUE)
    })
    len_cm <- c(47.56, 38.5, 43.85, 59.76, 67.7, 65.4)
    ga_days <- c(266, 238, 245, 252, 273, 287)
    age_days <- c(46, 2, 140, 450, 1500, 1850)
    pma_weeks <- (ga_days + age_days) / 7
    z <- c(-6.25, -3.5, -2.1, -1, 1.5, 3.0)
    y <- vapply(X = seq_along(standard),
                FUN.VALUE = numeric(1),
                FUN = \(idx) {
                  gigs_z2v_fn <- get(paste0(standard[idx], "_wfl_zscore2value"))
                  xvar <- len_cm[idx]
                  gigs_z2v_fn(z[idx], xvar, sex[idx])
                })
    wasting_df <- data.frame(wght_kg = y,
                             lenht = len_cm,
                             psex = sex,
                             ga_days = ga_days,
                             age = age_days)
    rm(standard, term, sex, len_cm, ga_days, age_days, pma_weeks, z, y)

    wasting <- with(
      wasting_df,
      classify_wasting(weight_kg = wght_kg, lenht_cm = lenht, sex = psex,
                       gest_days = ga_days, age_days = age)
    )
    wasting_outliers <- with(
      wasting_df,
      classify_wasting(weight_kg = wght_kg, lenht_cm = lenht, sex = psex,
                       gest_days = ga_days, age_days = age, outliers = TRUE))

    wasting_lvls <- c("wasting_severe", "wasting", "not_wasting", "overweight")
    wasting_exp <- factor(c("wasting_severe", "wasting_severe", "wasting",
                            "not_wasting", "not_wasting", "overweight"),
                          wasting_lvls)
    wasting_lvls_outlier <- c(wasting_lvls, "outlier")
    wasting_exp_outliers <- factor(c("outlier", "wasting_severe",
                                     "wasting", "not_wasting", "not_wasting",
                                     "overweight"),
                                   wasting_lvls_outlier)

    expect_equal(wasting, wasting_exp)
    expect_equal(wasting_outliers, wasting_exp_outliers)

    noise <- sqrt(.Machine$double.eps)
    wasting_noisy <- with(
      wasting_df,
      classify_wasting(weight_kg = wght_kg - noise,
                       lenht_cm = lenht + noise,
                       sex = psex,
                       gest_days = ga_days + noise,
                       age_days = age + noise)
    )
    wasting_outliers_noisy <- with(
      wasting_df,
      classify_wasting(weight_kg = wght_kg + noise,
                       lenht_cm = lenht - noise,
                       sex = psex,
                       gest_days = ga_days - noise,
                       age_days = age - noise,
                       outliers = TRUE))
    expect_equal(wasting_noisy, wasting_exp)
    expect_equal(wasting_outliers_noisy, wasting_exp_outliers)
})

#' @srrstats {G5.4, G5.6, G5.9, G5.9a} Test correctness of weight-for-age
#'   classification against temporary dataset (incl. trivial noise)
test_that("Weight-for-age classification works", {
  # AGE,  SEX  , GA_WKS, PREM?,   ZSCORE , CLASS
  # 501, FEMALE,   27  ,   Y  , <2 ZSCORE, UNDER_W
  # 323, MALE  ,   37  ,   N  , <3 ZSCORE, U_SEVERE
  # 435, FEMALE,   36  ,   Y  , <6 ZSCORE, U_SEVERE/IMPLAUS
  # 201, MALE  ,   40  ,   N  , -1 ZSCORE, NORMAL
  # 707, FEMALE,   41  ,   N  , >6 ZSCORE, OVER_W/IMPLAUS
  # 154, MALE  ,   28  ,   Y  , >2 ZSCORE, OVER_W
  # 496, MALE  ,   42  ,   N  , ~0.2 ZSCR, NORMAL
  wfa_df <- data.frame(wght_kg =     c(7.2, 6.1, 2.1, 9.1, 24 , 9.4, 10.8),
                       days_old =    c(501, 323, 435, 201, 707, 154, 496),
                       ga_at_birth = c(27,  37,  36,  40,  41,  28,  42) * 7,
                       psex = c(rep(c("F", "M"), 3), "M"))
  wfa <- with(wfa_df,
               classify_wfa(weight_kg = wght_kg,
                            age_days = days_old,
                            gest_days = ga_at_birth,
                            sex = psex))
  wfa_outliers <- with(wfa_df,
                       classify_wfa(weight_kg = wght_kg,
                                    age_days = days_old,
                                    gest_days = ga_at_birth,
                                    sex = psex,
                                    outliers = TRUE))
  wfa_lvls <- c("underweight_severe", "underweight", "normal_weight",
                "overweight")
  wfa_exp <- factor(c("underweight", "underweight_severe", "underweight_severe",
                      "normal_weight", "overweight", "overweight",
                      "normal_weight"),
                    levels =  wfa_lvls)
  wfa_lvls_out <- c(wfa_lvls, "outlier")
  wfa_exp_out <- factor(c("underweight", "underweight_severe", "outlier",
                          "normal_weight", "outlier", "overweight",
                          "normal_weight"),
                        levels =  wfa_lvls_out)

  expect_equal(wfa, wfa_exp)
  expect_equal(wfa_outliers, wfa_exp_out)

  noise <- sqrt(.Machine$double.eps)
  wfa_noisy <- with(
    wfa_df,
    classify_wfa(weight_kg = wght_kg - noise,
                 age_days = days_old + noise,
                 gest_days = ga_at_birth + noise,
                 sex = psex)
  )
  wfa_outliers_noisy <- with(
    wfa_df,
    classify_wfa(weight_kg = wght_kg + noise,
                 age_days = days_old - noise,
                 gest_days = ga_at_birth - noise,
                 sex = psex,
                 outliers = TRUE))
  expect_equal(wfa_noisy, wfa_exp)
  expect_equal(wfa_outliers_noisy, wfa_exp_out)
})

# Test error/warning behaviour with wrong input types or lengths ---------------

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
      test_error_zero_length("weight_kg")
    )
    expect_error(
      classify_sfga(sfga_weight, double(), sfga_sex, sfga_severe),
      test_error_zero_length("gest_days")
    )
    expect_error(
      classify_sfga(sfga_weight, sfga_gest, character(), sfga_severe),
      test_error_zero_length("sex")
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
      test_error_wrong_type("weight_kg", wanted = "numeric", got = "character")
    )
    expect_error(
      classify_sfga(sfga_weight, logical(1), sfga_sex, sfga_severe),
      test_error_wrong_type("gest_days", wanted = "numeric", got = "logical")
    )
    expect_error(
      classify_sfga(sfga_weight, sfga_gest, integer(1), sfga_severe),
      test_error_wrong_type("sex", wanted = "character", got = "integer")
    )
    expect_error(
      classify_sfga(sfga_weight, sfga_gest, sfga_sex, complex(1)),
      regexp = "Assertion on 'severe' failed. Must be of class 'logical'"
    )

    # G5.2, --> errors if vector inputs cannot be recycled
    expect_error(
      object = classify_sfga(character(2), sfga_gest, sfga_sex, sfga_severe),
      regexp = test_error_unrecyclable(names = "weight_kg")
    )
    expect_error(
      object = classify_sfga(sfga_weight, numeric(3), sfga_sex, sfga_severe),
      regexp = test_error_unrecyclable(names = "gest_days")
    )
    expect_error(
      object = classify_sfga(sfga_weight, sfga_gest, character(5), sfga_severe),
      regexp = test_error_unrecyclable("sex")
    )
  }
)

test_that(
  desc = "classify_svn() displays appropriate error behaviour",
  code = {
    svn_df <- life6mo[life6mo$age_days == 0, ]

    # Set up 'good' params
    svn_weight <- svn_df$weight_g / 1000
    svn_gest <- svn_df$gestage
    svn_sex <- as.character(svn_df$sex)

    # G5.8a --> zero-length data inputs cause errors
    expect_error(
      classify_svn(double(), svn_gest, svn_sex),
      test_error_zero_length("weight_kg")
    )
    expect_error(
      classify_svn(svn_weight, double(), svn_sex),
      test_error_zero_length("gest_days")
    )
    expect_error(
      classify_svn(svn_weight, svn_gest, character()),
      test_error_zero_length("sex")
    )

    # G5.8b --> incorrect data types
    expect_error(
      classify_svn(character(1), svn_gest, svn_sex),
      test_error_wrong_type("weight_kg", wanted = "numeric", got = "character")
    )
    expect_error(
      classify_svn(svn_weight, logical(1), svn_sex),
      test_error_wrong_type("gest_days", wanted = "numeric", got = "logical")
    )
    expect_error(
      classify_svn(svn_weight, svn_gest, integer(1)),
      test_error_wrong_type("sex", wanted = "character", got = "integer")
    )

    # G5.2, --> errors if vector inputs cannot be recycled
    expect_error(
      object = classify_svn(character(2), svn_gest, svn_sex),
      regexp = test_error_unrecyclable(names = "weight_kg")
    )
    expect_error(
      object = classify_svn(svn_weight, numeric(3), svn_sex),
      regexp = test_error_unrecyclable(names = "gest_days")
    )
    expect_error(
      object = classify_svn(svn_weight, svn_gest, character(5)),
      regexp = test_error_unrecyclable("sex")
    )
  }
)

# Test error/warning behaviour with edge-case inputs ---------------------------









# SRR tags ---------------------------------------------------------------------
#' @srrstats {G5.1} `life6mo` dataset used in this script is exported with
#'   package
NULL
