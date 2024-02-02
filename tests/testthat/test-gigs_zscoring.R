#' {}
test_that(
  desc = "GIGS head circumference-for-age z-scoring works",
  code = {
      for (seed in 4) {
          withr::with_seed(4, code = {
              z <- rnorm(n = 15)
              gest_days <- round(
                jitter(sample((25:41) * 7, 15, replace = TRUE))
              )
              age_days <- round(
                c(0,
                  jitter(sample((0:120) * 7, 7, replace = TRUE)),
                  0,
                  jitter(sample((0:120) * 7, 6, replace = TRUE)))
              )
              sex <- sample(c("M", "F"), size = 15, replace = TRUE)

              pma_days <- gest_days + age_days
              pma_wks <- pma_days / 7

              gigs_lgls <- gigs_xaz_lgls(gest_days = gest_days,
                                         age_days = age_days)
              headcirc <- rep(NA_real_, length(z))
              headcirc[gigs_lgls$ig_nbs] <-
                with(gigs_lgls, fn_on_subset(ig_nbs_hcfga_zscore2value, ig_nbs,
                                             z, gest_days, sex))
              headcirc[gigs_lgls$ig_png] <-
                with(gigs_lgls, fn_on_subset(ig_png_hcfa_zscore2value, ig_png,
                                             z, pma_wks, sex))
              headcirc[gigs_lgls$who_gs] <-
                with(gigs_lgls, fn_on_subset(who_gs_hcfa_zscore2value, who_gs,
                                             z, age_days, sex))

              gigs_z <- gigs_hcaz(headcirc_cm = headcirc, gest_days = gest_days,
                                  age_days = age_days, sex = sex)
          })
          expect_equal(z, gigs_z)
      }
  }
)

test_that(
  desc = "GIGS z-scoring functions fail if inputs have different length",
  code = {
    expect_error(object = gigs_hcaz(headcirc_cm = 50,
                                    gest_days = 232,
                                    age_days = 150,
                                    sex = c("M", "F")),
                 regexp = "Your inputs had different lengths.")
  }
)
