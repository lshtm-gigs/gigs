test_that(
  desc = "Size for GA classification works - with and without `severe` flag",
  code = {
    weight_df <- data.frame(weight = c(0.811730, 0.191618, NA, 1.14117, 1.21483,
                                       0.841630, 1.05958, 1.10986, 0.570546,
                                       0.606674, 0.453822, 0.974013, 0.993918,
                                       0.987215, 0.736120, 1.21361, 0.555279,
                                       0.951882, 0.284086, 0.609805, 0.803476,
                                       0.696198),
                            psex = "M", gestage = seq(24, 27, by = 1/7) * 7)
    sga <- with(weight_df, classify_sga(weight_kg = weight, sex = psex,
                                        gest_days = gestage, severe = FALSE))
    sga_severe <- with(weight_df, classify_sga(weight_kg = weight, sex = psex,
                                               gest_days = gestage,
                                               severe = TRUE))
    expect_equal(
      object = sga,
      expected = factor(c("AGA", "SGA", NA, "LGA", "LGA", "AGA", "LGA", "LGA",
                          "SGA", "AGA", "SGA", "AGA", "AGA", "AGA", "AGA",
                          "LGA", "SGA", "AGA", "SGA", "SGA", "AGA", "SGA"),
                        levels = c("SGA", "AGA",  "LGA")))

    expect_equal(
      object = sga_severe,
      expected = factor(c("AGA", "SGA(<3)", NA, "LGA", "LGA", "AGA", "LGA",
                          "LGA", "SGA", "AGA", "SGA(<3)", "AGA", "AGA", "AGA",
                          "AGA", "LGA", "SGA(<3)", "AGA", "SGA(<3)", "SGA(<3)",
                          "AGA", "SGA"),
                        levels = c("SGA(<3)", "SGA", "AGA",  "LGA")))
  }
)

test_that(
  desc = "Small vulnerable newborn classification works",
  code = {
    centiles <- c(0.07, 0.15, 0.02, 0.91, 0.75)
    expected_sga <- c("SGA", "AGA", "SGA", "LGA", "AGA")
    na_indices <- c(4, 17)
    params <- list(
      p = rep(centiles, 4),
      gest_days = c(rep(36 * 7, 10), rep(39 * 7, 10)),
      sex = rep(c(rep("M", 5), rep("F", 5)), 2)
    )
    params$p[na_indices] <- NA
    params$weight_kg <- do.call(what = ig_nbs_wfga_centile2value, params)
    params$p <- NULL
    svn <- do.call(what = classify_svn, params)

    expected_svn <- c(paste("Preterm", rep(expected_sga, 2)),
                      paste("Term", rep(expected_sga, 2)))
    expected_svn[na_indices] <- NA
    expect_equal(object = svn,
                 expected = factor(expected_svn, levels = levels(svn)))
  }
)

test_that(
  desc = "Stunting classification works",
  code = {
    # CASE: male; pre-term, below age cutoff, z = ~ -3.29
    # CASE: female; pre-term, above age cutoff, z = ~ -1.40
    # CASE: male; pre-term, below age cutoff, z = ~ -12.60
    # CASE: female; term, z = ~ -1.58
    # CASE: male; term, z = ~ 0.98
    # CASE: female --> missing data
    stunting_df <- data.frame(lenht = c(57.5, 73.6, 44.1, 72.4, 72.83, 87.4),
                              ga_at_birth = c(238, 245, 252, 259, 266, 285),
                              age_days = c(180, 455, 294, 525, 245, NA),
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

    stunting_exp <- c("stunting_severe", "normal", "stunting_severe",
                      "stunting", "normal", NA_character_)
    stunting_outliers_exp <- c("stunting_severe", "normal", "outlier",
                               "stunting", "normal", NA_character_)
    expect_equal(
      object = stunting,
      expected = factor(stunting_exp,
                        levels = c("stunting_severe", "stunting", "normal")
      ))

    expect_equal(
      object = stunting_outliers,
      expected = factor(stunting_outliers_exp,
                        levels = c("stunting_severe", "stunting", "normal",
                                   "outlier")
    ))
})


test_that("Wasting classification works", code = {
  wasting_df <- data.frame(wght_kg = c(0.45, 1.4, 2.68, 3.94, 7.09, 2.24, 7.48,
                                       6.75, 16.86, 7.68, 30.53),
                           lenht =   c(43.85, 34.5, 43.85, 59.76, 67.7, NA,
                                       65.4, 93.52, 100.38, 91.41, 110.39),
                           psex = rep_len(c("F", "M"), length.out = 11),
                           ga_days = c(251, 197, 225, 243, 277, 236, 283,
                                       NA_integer_, 293, 298, 287),
                           age = c(1, 2, 76, 120, 2, 267, 154, 980, 1343, 892,
                                   1850))
  wasting <- with(
    wasting_df,
    classify_wasting(weight_kg = wght_kg, lenht_cm = lenht, sex = psex,
                     gest_days = ga_days, age_days = age))
  wasting_outliers <- with(
    wasting_df,
    classify_wasting(weight_kg = wght_kg, lenht_cm = lenht, sex = psex,
                     gest_days = ga_days, age_days = age, outliers = TRUE))
  expect_equal(
    object = wasting,
    expected = factor(c("wasting_severe", NA, "normal", "wasting", "normal", NA,
                        "normal", NA, "normal", "wasting_severe",
                        "overweight"),
                      levels =  c("wasting_severe", "wasting", "normal",
                                  "overweight")))
  expect_equal(
    object = wasting_outliers,
    expected = factor(c("outlier", NA, "normal", "wasting", "normal", NA,
                        "normal", NA, "normal", "outlier", "outlier"),
                      levels =  c("wasting_severe", "wasting", "normal",
                                  "overweight", "outlier")))
})

test_that("Weight-for-age classification works", {
  # AGE,  SEX  , GA_WKS, PREM?,   ZSCORE , CLASS
  # 501, FEMALE,   27  ,   Y  , <2 ZSCORE, UNDER_W
  # 323, MALE  ,   37  ,   N  , <3 ZSCORE, U_SEVERE
  # 435, FEMALE,   36  ,   Y  , <6 ZSCORE, IMPLAUS
  # 201, MALE  ,   40  ,   N  , -1 ZSCORE, NORMAL
  # 707, FEMALE,   41  ,   N  , >6 ZSCORE, IMPLAUS
  # 154, MALE  ,   28  ,   Y  , >2 ZSCORE, OVER_W
  # 496, NA    ,   42  ,   N  , ~0 ZSCORE, NA
  wfa_df <- data.frame(wght_kg =     c(7.2, 6.1, 2.1, 9.1, 24 , 9.4, 10.8),
                       days_old =    c(501, 323, 435, 201, 707, 154, 496),
                       ga_at_birth = c(27,  37,  36,  40,  41,  28,  42) * 7,
                       psex = c(rep(c("F", "M"), 3), NA_character_))
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
  expect_equal(
    object = wfa,
    expected = factor(c("underweight", "underweight_severe",
                        "underweight_severe", "normal", "overweight",
                        "overweight", NA),
                      levels =  c("underweight_severe", "underweight", "normal",
                                  "overweight")))
  expect_equal(
    object = wfa_outliers,
    expected = factor(c("underweight", "underweight_severe",
                        "outlier", "normal", "outlier",
                        "overweight", NA),
                      levels =  c("underweight_severe", "underweight", "normal",
                                  "overweight", "outlier")))
})
