test_that(
  desc = "Size for GA classification works - with and without `severe` flag",
  code = {
    # pre-term, below age cutoff, <-2 on IG
    # pre-term, above age cutoff, <-2 on WHO
    # pre-term, below age cutoff, < normal on IG
    # term, age, <-2
    # term, age, healthy
    ## ALL MALE ALL "H"
    weight_df <- data.frame(weight = c(0.811730, 0.191618, NA, 1.14117, 1.21483,
                                       0.841630, 1.05958, 1.10986, 0.570546,
                                       0.606674, 0.453822, 0.974013, 0.993918,
                                       0.987215, 0.736120, 1.21361, 0.555279,
                                       0.951882, 0.284086, 0.609805, 0.803476,
                                       0.696198),
                            psex = "M", gestage = seq(24, 27, by = 1/7) * 7)
    expect_equal(
      object = with(weight_df,
                    classify_sga(weight_kg = weight, sex = psex,
                                 gest_age = gestage, severe = FALSE)),
      expected = factor(c("AGA", "SGA", NA, "LGA", "LGA", "AGA", "LGA", "LGA",
                          "SGA", "AGA", "SGA", "AGA", "AGA", "AGA", "AGA", "LGA",
                          "SGA", "AGA", "SGA", "SGA", "AGA", "SGA"),
                        levels = c("SGA", "AGA",  "LGA")))

    expect_equal(
      object =
        with(weight_df,
             classify_sga(weight_kg = weight, sex = psex, gest_age = gestage,
                          severe = TRUE)),
      expected = factor(c("AGA", "SGA(<3)", NA, "LGA", "LGA", "AGA", "LGA", "LGA",
                          "SGA", "AGA", "SGA(<3)", "AGA", "AGA", "AGA", "AGA",
                          "LGA", "SGA(<3)", "AGA", "SGA(<3)", "SGA(<3)", "AGA",
                          "SGA"), levels = c("SGA(<3)", "SGA", "AGA",  "LGA")))
  }
)

test_that(
  desc = "Small vulnerable newborn classification works",
  code = {
    params <- list(
      p = rep(c(0.07, 0.15, 0.03, 0.25, 0.75), 4),
      gest_age = c(rep(36 * 7, 10), rep(39 * 7, 10)),
      sex = rep(c(rep("M", 5), rep("F", 5)), 2)
    )
    params$weight_kg <- do.call(what = "ig_nbs_wfga_percentile2value", params)
    params$p <- NULL
    svn <- do.call(what = "classify_svn", params)

    expect_equal(svn,
                 factor(c("Preterm SGA", "Preterm non-SGA", "Preterm SGA",
                          "Preterm non-SGA", "Preterm non-SGA",
                          "Preterm SGA", "Preterm non-SGA", "Preterm SGA",
                          "Preterm non-SGA", "Preterm non-SGA", "Term SGA",
                          "Term non-SGA", "Term SGA", "Term non-SGA",
                          "Term non-SGA", "Term SGA", "Term non-SGA",
                          "Term SGA", "Term non-SGA", "Term non-SGA"),
                 levels = levels(svn)))

    na_indices <- sample(x = seq_along(params$weight_kg),
                         size = 6,
                         replace = FALSE)
    params$weight_kg[na_indices] <- NA_real_
    svn_NAs <- do.call(what = "classify_svn", params)

    # All NA weight_kg lead to NA SVN classifications
    expect_true(all(is.na(svn_NAs[na_indices])))

    # All non-NA weight_kg lead to non-NA SVN classifications
    expect_false(any(is.na(svn_NAs[-na_indices])))
  }
)

test_that(desc = "Stunting classification works", {
  expect_equal(object =
                 # pre-term, below age cutoff, <-2 on IG
                 # pre-term, above age cutoff, <-2 on WHO
                 # pre-term, below age cutoff, < normal on IG
                 # term, age, <-2
                 # term, age, healthy
                 ## ALL MALE ALL "H"
                 with(data.frame(lenht = c(57.5, 73.6, 44.1, 75.4, 72.83),
                                 ga_at_birth = c(34, 35, 36, 37, 38) * 7,
                                 age_days = c(180, 455, 294, 525, 245),
                                 psex = "M", len_method = "H"),
                      classify_stunting(lenht_cm = lenht,
                                        age_days = age_days,
                                        ga_at_birth = ga_at_birth,
                                        sex = psex,
                                        lenht_method = len_method)),
               expected = factor(
                 c("stunting_severe", "normal", "implausible", "stunting", "normal"),
                 levels = c("implausible", "stunting_severe", "stunting", "normal")
                 ))
})

test_that("classify_stunting() fails with inputs of incorrect length", {
  expect_error(object =
                  classify_stunting(lenht_cm = c(57.5, 73.6, 44.1, 75.4, 72.83),
                                    age_days = c(34, 35, 36, 37, 38) * 7,
                                    ga_at_birth = c(180, 455, 294, 525, 245),
                                    sex = "M",
                                    lenht_method = c("H", "L")),
               regexp = "lenht_method should be as long as the input vectors or length 1.")
})

test_that("classify_stunting() fails with inputs of incorrect length", {
  expect_equal(object =
                  classify_stunting(lenht_cm = c(57.5, 73.6, 44.1, 75.4, 72.83),
                                    age_days = c(34, 35, 36, 37, 38) * 7,
                                    ga_at_birth = c(180, 455, 294, 525, 245),
                                    sex = "M",
                                    lenht_method = "H"),
               expected = factor(
                 c("stunting_severe", "normal", "implausible", "normal", "normal"),
                 levels = c("implausible", "stunting_severe", "stunting", "normal")
                 ))
})

test_that("Wasting classification works", {
  expect_equal(
      # H or L,  SEX  ,   ZSCORE   CLASS
      # HEIGHT, FEMALE, <3 ZSCORE W_SEVERE
      # LENGTH, MALE,   <2 ZSCORE W
      # NA    , FEMALE, <3 ZSCORE NA
      # HEIGHT, MALE,   >6 ZSCORE IMPLAUS
      # LENGTH, FEMALE, ~1 ZSCORE NORMAL
      # X     , MALE,   <3 ZSCORE NA
      with(data.frame(wght_kg = c(5.75, 2.18, 3.00, 6.75, 5.30, 2.70),
                      lenht =   c(67.7, 46.6, 50.0, 80.1, 55.8, 46.4),
                      psex = rep(c("F", "M"), 3),
                      lenht_meth = c("H", "L", NA_character_, "H", "L", "X")),
           classify_wasting(weight_kg = wght_kg,
                            lenht_cm = lenht,
                            sex = psex,
                            lenht_method = lenht_meth)),
    expected = factor(
      c("wasting_severe", "wasting",  NA_character_, "implausible", "normal", NA_character_),
      levels =  c("implausible", "wasting_severe", "wasting", "normal", "overweight"))
    )
})

test_that("Weight-for-age classification works", {
  expect_equal(
      # AGE,  SEX  , GA_WKS, PREM?,   ZSCORE , CLASS
      # 501, FEMALE,   27  ,   Y  , <2 ZSCORE, UNDER_W
      # 323, MALE  ,   37  ,   N  , <3 ZSCORE, U_SEVERE
      # 435, FEMALE,   36  ,   Y  , <6 ZSCORE, IMPLAUS
      # 201, MALE  ,   40  ,   N  , -1 ZSCORE, NORMAL
      # 707, FEMALE,   41  ,   N  , >6 ZSCORE, IMPLAUS
      # 154, MALE  ,   28  ,   Y  , >2 ZSCORE, OVER_W
      # 496, NA    ,   42  ,   N  , ~0 ZSCORE, NA
      with(data.frame(wght_kg =     c(7.2, 6.1, 2.1, 9.1, 24 , 9.4, 10.8),
                      days_old =    c(501, 323, 435, 201, 707, 154, 496),
                      ga_at_birth = c(27,  37,  36,  40,  41,  28,  42) * 7,
                      psex = c(rep(c("F", "M"), 3), NA_character_)),
           classify_wfa(weight_kg = wght_kg,
                        age_days = days_old,
                        ga_at_birth = ga_at_birth,
                        sex = psex)),
    expected = factor(
      c("underweight", "underweight_severe", "implausible", "normal", "implausible", "overweight",
        NA_character_),
      levels = c("implausible", "underweight_severe", "underweight", "normal", "overweight"))
    )
  
  expect_error(
    object = classify_wfa(weight_kg = c(7.2, 6.1, 2.1, 9.1),
                          age_days = c(501, 323, 435, 201),
                          ga_at_birth = 37,
                          sex = c("F", "M")),
    regexp = "^Can't recycle"
  )
})