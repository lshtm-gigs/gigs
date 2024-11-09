#' @srrstats {G5.2, G5.2a, G5.2b} Test out warnings for this *internal*
#'   function - these will be printed to the console when users supply specific
#'   data.
test_that("gigs_zscoring_lgls() produces expected values", {
  single_chr_gigs_zscoring_lgls <- function(gest_days, age_days, id) {
    gigs_lgls <- gigs_zscoring_lgls(gest_days = gest_days,
                                    age_days = age_days,
                                    id = as.factor(id)) |>
      unlist()
    if (sum(gigs_lgls) == 0) {
      return("none")
    }
    c("ig_nbs", "ig_png", "who_gs")[gigs_lgls[2:4]]
  }

  # Should issue a warning if an 'at birth' obs is older than 12hrs --> choosing
  # not to let the user silence this
  expect_warning(
    gigs_zscoring_lgls(2, 279, as.factor("TEST1")),
    class = "gigs_zscoring_old_birth_obs"
  )

  # Should issue a warning if an 'at birth' obs has a GA so old it is out of
  # range of the IG-21st NBS --> choosing not to let the user silence this
  expect_warning(
    gigs_zscoring_lgls(0, 301, as.factor("TEST2")),
    class = "gigs_zscoring_birth_w_large_ga"
  )

  # Now get both warnings at once
  warnings <- capture_warnings(
    gigs_zscoring_lgls(age_days = c(2, 0),
                       gest_days = c(279, 301),
                       id = as.factor(c("TEST1", "TEST2")))
  )
})


#' @srrstats {G5.2, G5.2a, G5.2b} Test out error behaviour for
#'   gigs_zscoring_lgls() - to ensure it functions correctly, even if the user
#'   should never see the error.
test_that("gigs_zscoring_lgls() throws errors", {
  for (seed in seq(1000, 5000, 1000)) {
    withr::with_seed(seed, code = {
      n_age_days <- sample.int(n = 1000, size = 1)
      age_days <- sample.int(n = 1000, size = n_age_days, replace = TRUE)
      n_gest_days <- sample.int(n = 1000, size = 1)
      gest_days <- sample(x = 250:280, size = n_gest_days, replace = TRUE)
    })

    expect_error(
      gigs_zscoring_lgls(gest_days = gest_days, age_days = age_days),
      regexp = paste0("`age_days` had length ", n_age_days,
                      "; `gest_days` had length ", n_gest_days, "."),
      class = "gigs_zscoring_inconsistent_lengths"
    )
  }

  expect_error(
    gigs_zscoring_lgls(277, 2, "TEST1"),
    class = "gigs_zscoring_id_not_factor"
  )
})
