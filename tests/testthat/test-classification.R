test_that(
  desc = "Size for GA classification works - with and without `severe` flag",
  code = {
    sfga_df <- life6mo[life6mo$age_days == 0, ]
    sfga <- with(sfga_df, classify_sfga(weight_kg = weight_g / 1000,
                                        sex = as.character(sex),
                                        gest_days = gestage,
                                        severe = FALSE))
    sfga_severe <- with(sfga_df, classify_sfga(weight_kg = weight_g / 1000,
                                               sex = as.character(sex),
                                               gest_days = gestage,
                                               severe = TRUE))
    expect_equal(
      object = sfga,
      expected = factor(c("SGA", "SGA", "SGA", "LGA", "AGA", "SGA", "AGA",
                          "AGA", "AGA", "SGA", "SGA", "SGA", "AGA", "LGA",
                          "SGA", "AGA", "SGA", "SGA", "SGA", "SGA", "AGA",
                          "AGA", "AGA", "SGA", "SGA", "SGA", "AGA", "AGA",
                          "SGA", "SGA", "SGA", "SGA", "SGA", "SGA", "SGA",
                          "SGA", "SGA", "SGA", "AGA", "SGA", "AGA", "SGA",
                          "SGA", "SGA", "SGA", "SGA", "SGA", "SGA", "AGA",
                          "SGA", "AGA", "SGA", "AGA", "SGA", "SGA", "SGA",
                          "SGA"),
                        levels = c("SGA", "AGA",  "LGA")))

    expect_equal(
      object = sfga_severe,
      expected = factor(c("SGA(<3)", "SGA", "SGA(<3)", "LGA", "AGA", "SGA",
                          "AGA", "AGA", "AGA", "SGA", "SGA(<3)", "SGA(<3)",
                          "AGA", "LGA", "SGA(<3)", "AGA", "SGA", "SGA", "SGA",
                          "SGA", "AGA", "AGA", "AGA", "SGA", "SGA", "SGA(<3)",
                          "AGA", "AGA", "SGA(<3)", "SGA", "SGA", "SGA",
                          "SGA(<3)", "SGA(<3)", "SGA(<3)", "SGA", "SGA", "SGA",
                          "AGA", "SGA(<3)", "AGA", "SGA", "SGA", "SGA(<3)",
                          "SGA(<3)", "SGA", "SGA(<3)", "SGA", "AGA", "SGA(<3)",
                          "AGA", "SGA(<3)", "AGA", "SGA(<3)", "SGA(<3)",
                          "SGA(<3)", "SGA(<3)"),
                        levels = c("SGA(<3)", "SGA", "AGA", "LGA")))
  }
)

test_that(
  desc = "Small vulnerable newborn classification works",
  code = {
    svn_df <- life6mo[life6mo$age_days == 0, ]
    svn <- with(svn_df, classify_svn(weight_kg = weight_g / 1000,
                                     gest_days = gestage,
                                     sex = as.character(sex)))

    # Get SGA categories --> already tested above so should be accurate
    sfga <- with(svn_df, classify_sfga(weight_kg = weight_g / 1000,
                                       sex = as.character(sex),
                                       gest_days = gestage,
                                       severe = FALSE))

    expected_svn <- factor(ifelse(svn_df$gestage < 259,
                                  paste0("Preterm ", sfga),
                                  paste0("Term ", sfga)),
                           levels = levels(svn))

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

    stunting_exp <- c("stunting_severe", "not_stunting", "stunting_severe",
                      "stunting", "not_stunting", NA_character_)
    stunting_outliers_exp <- c("stunting_severe", "not_stunting", "outlier",
                               "stunting", "not_stunting", NA_character_)
    expect_equal(
      object = stunting,
      expected = factor(stunting_exp,
                        levels = c("stunting_severe", "stunting",
                                   "not_stunting")
      ))

    expect_equal(
      object = stunting_outliers,
      expected = factor(stunting_outliers_exp,
                        levels = c("stunting_severe", "stunting",
                                   "not_stunting", "outlier")
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
    expected = factor(c("wasting_severe", NA, "not_wasting", "wasting", "not_wasting", NA,
                        "not_wasting", NA, "not_wasting", "wasting_severe",
                        "overweight"),
                      levels =  c("wasting_severe", "wasting", "not_wasting",
                                  "overweight")))
  expect_equal(
    object = wasting_outliers,
    expected = factor(c("outlier", NA, "not_wasting", "wasting", "not_wasting", NA,
                        "not_wasting", NA, "not_wasting", "outlier", "outlier"),
                      levels =  c("wasting_severe", "wasting", "not_wasting",
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
                        "underweight_severe", "normal_weight", "overweight",
                        "overweight", NA),
                      levels =  c("underweight_severe", "underweight",
                                  "normal_weight", "overweight")))
  expect_equal(
    object = wfa_outliers,
    expected = factor(c("underweight", "underweight_severe",
                        "outlier", "normal_weight", "outlier",
                        "overweight", NA),
                      levels =  c("underweight_severe", "underweight",
                                  "normal_weight", "overweight", "outlier")))
})
