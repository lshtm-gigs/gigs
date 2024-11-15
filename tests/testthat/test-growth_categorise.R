# Correctness testing ----------------------------------------------------------

#' @srrstats {G5.4, G5.4c} Tests to ensure that `categorise_sfga()` reproduces
#'   expected categories around the categorisation thresholds
test_that("Size-for-GA categorisation works as expected", {
  centile <- c(0.01, 0.029, 0.031, 0.09, 0.10, 0.11, 0.89, 0.91, 0.999)
  sfga <- categorise_sfga(centile, severe = FALSE)
  sfga_severe <- categorise_sfga(centile, severe = TRUE)
  
  sfga_lvls <- c("SGA", "AGA", "LGA")
  sfga_expected <- factor(
    c("SGA", "SGA", "SGA", "SGA", "AGA", "AGA", "AGA", "LGA", "LGA"),
    levels = sfga_lvls
  )
  sfga_severe_expected <- factor(
    c("SGA(<3)", "SGA(<3)", "SGA", "SGA", "AGA", "AGA", "AGA", "LGA", "LGA"),
    levels = c("SGA(<3)", sfga_lvls)
  )

  expect_equal(sfga, sfga_expected)
  expect_equal(sfga_severe, sfga_severe_expected)
})

test_that("SVN categorisation works as expected", {
  centile <- c(0.01, 0.029, 0.031, 0.09, 0.10, 0.11, 0.89, 0.91, 0.999)
  gest_days <- seq(250, 290, 5)
  svn <- categorise_svn(centile, gest_days = gest_days)

  svn_lvls <- paste(c(rep_len("Preterm", 3), rep_len("Term", 3)),
                    rep.int(c("SGA", "AGA", "LGA"), times = 2))
  svn_chr <- paste0(
    c(rep.int("Preterm ", 2), rep.int("Term ", 7)),
    c("SGA", "SGA", "SGA", "SGA", "AGA", "AGA", "AGA", "LGA", "LGA")
  )
  expect_equal(svn, expected = factor(svn_chr, levels = svn_lvls))
})

test_that("Stunting categorisation works as expected", {
  lhaz <- c(-6.5, -6, -3.1, -3, -2.9, -2, -1.9, -0.5, -.5, 3, 4.999, 6, 6.5)
  stunting_lvls <- c("stunting_severe", "stunting", "not_stunting")
  stunting_outlier_lvls <- c(stunting_lvls, "outlier")

  stunting_expected <- factor(
    c(rep(stunting_lvls[1], 4),
      rep(stunting_lvls[2], 2),
      rep(stunting_lvls[3], 7)), levels = stunting_lvls)
  stunting_outliers_expected <- factor(
    c(rep(stunting_outlier_lvls[4], 1),
      rep(stunting_outlier_lvls[1], 3),
      rep(stunting_outlier_lvls[2], 2),
      rep(stunting_outlier_lvls[3], 6),
      rep(stunting_outlier_lvls[4], 1)), levels = stunting_outlier_lvls)


  stunting <- categorise_stunting(lhaz, outliers = FALSE)
  stunting_outliers <- categorise_stunting(lhaz, outliers = TRUE)
  expect_equal(stunting, stunting_expected)
  expect_equal(stunting_outliers, stunting_outliers_expected)
})

test_that("Wasting categorisation works as expected", {
  wlz <- c(-5.5, -5, -3.1, -3, -2.9, -2, -1.9, -0.5, -1.9, 2, 4.999, 5, 5.5)
  wasting_lvls <- c("wasting_severe", "wasting", "not_wasting", "overweight")
  wasting_outlier_lvls <- c(wasting_lvls, "outlier")

  wasting_expected <- factor(
    c(rep(wasting_lvls[1], 4),
      rep(wasting_lvls[2], 2),
      rep(wasting_lvls[3], 3),
      rep(wasting_lvls[4], 4)), levels = wasting_lvls)
  wasting_outliers_expected <- factor(
    c(rep(wasting_outlier_lvls[5], 1),
      rep(wasting_outlier_lvls[1], 3),
      rep(wasting_outlier_lvls[2], 2),
      rep(wasting_outlier_lvls[3], 3),
      rep(wasting_outlier_lvls[4], 3),
      rep(wasting_outlier_lvls[5], 1)), levels = wasting_outlier_lvls)

  wasting <- categorise_wasting(wlz, outliers = FALSE)
  wasting_outliers <- categorise_wasting(wlz, outliers = TRUE)
  expect_equal(wasting, wasting_expected)
  expect_equal(wasting_outliers, wasting_outliers_expected)
})

test_that("Wasting categorisation works as expected", {
  waz <- c(-6.5, -6, -3.1, -3, -2.9, -2, -1.9, -0.5, -1.9, 2, 4.999, 5, 5.5)
  wfa_lvls <- c("underweight_severe", "underweight", "normal_weight",
                "overweight")
  wfa_outlier_lvls <- c(wfa_lvls, "outlier")

  wfa_expected <- factor(
    c(rep(wfa_lvls[1], 4),
      rep(wfa_lvls[2], 2),
      rep(wfa_lvls[3], 3),
      rep(wfa_lvls[4], 4)), levels = wfa_lvls)
  wfa_outliers_expected <- factor(
    c(rep(wfa_outlier_lvls[5], 1),
      rep(wfa_outlier_lvls[1], 3),
      rep(wfa_outlier_lvls[2], 2),
      rep(wfa_outlier_lvls[3], 3),
      rep(wfa_outlier_lvls[4], 3),
      rep(wfa_outlier_lvls[5], 1)), levels = wfa_outlier_lvls)

  wfa <- categorise_wfa(waz, outliers = FALSE)
  wfa_outliers <- categorise_wfa(waz, outliers = TRUE)
  expect_equal(wfa, wfa_expected)
  expect_equal(wfa_outliers, wfa_outliers_expected)
})

test_that("Head size categorisation works as expected", {
  hcaz <- c(-3.5, -3, -2.999,  -2.001, -2, -1.999, 0 , 1.999, 2, 2.001, 2.999,
            3, 3.5)
  headsize_lvls <- c("microcephaly_severe", "microcephaly", "normal_headcirc",
                     "macrocephaly", "macrocephaly_severe")

  headsize_expected <- factor(
    c(rep(headsize_lvls[1], 2),
      rep(headsize_lvls[2], 3),
      rep(headsize_lvls[3], 3),
      rep(headsize_lvls[4], 3),
      rep(headsize_lvls[5], 2)), levels = headsize_lvls)

  headsize <- categorise_headsize(hcaz)
  expect_equal(headsize, headsize_expected)
})

# Test handling of unused factor levels ----------------------------------------

# There are no associated srr tags specifically for handling unused factor
# levels in package outputs.
test_that("After categoristaion, unused factor levels can be flagged", {
  run_categorise_fn <- function(name, cat_fn) {
    keep_warn <- if (name != "svn") {
      cat_fn(0.5)
    } else {
      cat_fn(0.5, 260)
    }
  }

  #' Set off internal error in [handle_factor_levels()], for coverage reasons
  expect_error(
    handle_factor_levels(fct = categorise_sfga(0.95, FALSE),
                         outcome = "BAD OUTCOME"),
    class = "gigs_handle_fctr_lvls_bad_outcome_str",
    regexp = "`outcome` must be one of"
  )

  for (name in c("sfga", "svn", "stunting", "wasting", "wfa", "headsize")) {
    categorise_fn <- getFromNamespace(paste("categorise", name, sep = "_"),
                                      ns = "gigs")

    gigs_option_set(option = "handle_unused_levels",
                    new_value = "keep_warn", silent = TRUE)
    expect_message(keep_warn <- run_categorise_fn(name, categorise_fn),
                   regexp = "Unused factor levels kept")

    gigs_option_set(option = "handle_unused_levels",
                    new_value = "keep_silent", silent = TRUE)
    expect_no_message(keep_silent <- run_categorise_fn(name, categorise_fn))
    expect_equal(keep_warn, keep_silent)

    gigs_option_set(option = "handle_unused_levels",
                    new_value = "drop_warn", silent = TRUE)
    expect_message(drop_warn <- run_categorise_fn(name, categorise_fn),
                   regexp = "Unused factor levels dropped")

    gigs_option_set(option = "handle_unused_levels",
                    new_value = "drop_silent", silent = TRUE)
    expect_no_message(drop_silent <- run_categorise_fn(name, categorise_fn))

    expect_equal(drop_warn, drop_silent)
    expect_false(all(levels(keep_silent) == levels(drop_silent)))
  }

  # Set back to default value for other tests
  gigs_option_set(option = "handle_unused_levels", new_value = "keep_warn",
                  silent = TRUE)
})