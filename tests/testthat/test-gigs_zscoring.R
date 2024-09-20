#' @srrstats {G5.2, G5.2a, G5.2b} Test out error behaviour for this *internal*
#'   function - to ensure it functions correctly, even if the user should never
#'   see the error.
testthat::test_that("Test that gigs_zscoring_lgls() throws errors", {
  for (seed in seq(1000, 5000, 1000)) {
    withr::with_seed(seed, code = {
      n_age_days <- sample.int(n = 1000, size = 1)
      age_days <- sample.int(n = 1000, size = n_age_days, replace = TRUE)
      n_gest_days <- sample.int(n = 1000, size = 1)
      gest_days <- sample(x = 250:280, size = n_gest_days, replace = TRUE)
    })

    testthat::expect_error(
      gigs_zscoring_lgls(gest_days = gest_days, age_days = age_days),
      regexp = paste0("`age_days` had length ", n_age_days,
                      "; `gest_days` had length ", n_gest_days, ".")
    )
  }
})
