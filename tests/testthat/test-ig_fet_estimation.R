# Test EFW estimation

test_that(desc = "EFW function gives correct outputs", {
  # Example from https://dx.doi.org/10.1002/uog.17347
  expect_equal(object = round(ig_fet_estimate_fetal_weight(abdocirc_mm = 260,
                                                           headcirc_mm = 290)),
               expected = 1499)
})

# Test gestational age estimation

test_that(desc = "GA estimation function gives correct outputs", {
  # From GA for CRL standard
  gafcrl_tbl <- gigs::ig_fet[["gafcrl"]][[1]]
  crl_mm <- gafcrl_tbl[[1]]
  P50_table <- gafcrl_tbl[["P50"]]
  GA_estimations <- round(ig_fet_estimate_ga(crl_mm = crl_mm))
  expect_equal(object = GA_estimations, expected = P50_table)

  # Examples taken from https://dx.doi.org/10.1002/uog.15894
  expect_equal(object = round(ig_fet_estimate_ga(headcirc_mm = 250),
                              digits = 1), expected = 189.8)
  expect_equal(object = round(ig_fet_estimate_ga(headcirc_mm = 250,
                                                 femurlen_mm = 55),
                              digits = 1),
               expected = 200.5)
})

## Errors
test_that("Bad GA estimation calls give proper error messages", {
  # Non-numeric input errors
  expect_error(
    ig_fet_estimate_ga(crl_mm = "25", headcirc_mm = 250, femurlen_mm = 50),
    regexp = paste("Assertion on 'crl_mm' failed: Must be of type 'numeric',",
                   "not 'character'.")
  )
  expect_error(
    ig_fet_estimate_ga(crl_mm = 25, headcirc_mm = "250", femurlen_mm = 50),
    regexp = paste("Assertion on 'headcirc_mm' failed: Must be of type",
                   "'numeric', not 'character'.")
  )
  expect_error(
    ig_fet_estimate_ga(crl_mm = 25, headcirc_mm = 250, femurlen_mm = TRUE),
    regexp = paste("Assertion on 'femurlen_mm' failed: Must be of type",
                   "'numeric', not 'logical'.")
  )

  # Errors based on input length
  expect_error(
    ig_fet_estimate_ga(crl_mm = integer(), headcirc_mm = 250, femurlen_mm = 50),
    regexp = paste("Assertion on 'crl_mm' failed: Must have length >= 1, but",
                   "has length 0.")
  )
  expect_error(
    ig_fet_estimate_ga(crl_mm = 25, headcirc_mm = integer(), femurlen_mm = 50),
    regexp = paste("Assertion on 'headcirc_mm' failed: Must have length >= 1,",
                   "but has length 0.")
  )
  expect_error(
    ig_fet_estimate_ga(crl_mm = 25, headcirc_mm = 50, femurlen_mm = integer()),
    regexp = paste("Assertion on 'femurlen_mm' failed: Must have length >= 1,",
                   "but has length 0.")
  )

  # NULL for crl_mm + headcirc_mm inputs
  expect_error(
    ig_fet_estimate_ga(crl_mm = NULL, headcirc_mm = NULL, femurlen_mm = 50),
    regexp = "At least one of `crl_mm` or `headcirc_mm` must not be `NULL`"
  )

  # NULL for all inputs
  expect_error(
    ig_fet_estimate_ga(crl_mm = NULL, headcirc_mm = NULL, femurlen_mm = NULL),
    regexp = "At least one of `crl_mm` or `headcirc_mm` must not be `NULL`"
  )
})


# Test against other R implementations -----------------------------------------

#' @srrstats {G5.4a, G5.4b} Test correctness against other R
#'   implementations.
test_that(
  desc = "GIGS aligns with `intergrowth` package",
  code = {
    skip_if_not_installed(pkg = "intergrowth")

    # Estimation of fetal weight
    efw_GIGS <- ig_fet_estimate_fetal_weight(abdocirc_mm = 250,
                                             headcirc_mm = 290)
    efw_IG <- intergrowth::calculate_efw(ac = 25, hc =  29)
    expect_equal(object = efw_GIGS, expected = efw_IG)

    # GA estimation
    hc_mm <- 230:260
    GA_from_hc_GIGS <- ig_fet_estimate_ga(headcirc_mm = hc_mm)
    suppressMessages(suppressWarnings(
      GA_from_hc_IG <- intergrowth::calculate_gestage_hcfl(hc = hc_mm)[[1]]
    ))
    expect_equal(object = round(GA_from_hc_GIGS / 7, digits = 1),
                 expected = round(GA_from_hc_IG, digits = 1))

    fl_mm <- 30:60
    GA_from_hcfl_GIGS <- ig_fet_estimate_ga(headcirc_mm = hc_mm,
                                            femurlen_mm = fl_mm)
    suppressMessages(suppressWarnings(
      GA_from_hcfl_IG <- intergrowth::calculate_gestage_hcfl(hc = hc_mm,
                                                             fl = fl_mm)[[1]]
    ))
    # We use a tolerance here as the FL/HC equation in `intergrowth` pkg is
    # slightly incorrect - `intergrowth` uses 0.03242 as first coefficient
    # instead of the correct value of 0.03243
    expect_equal(object = round(GA_from_hcfl_GIGS / 7, digits = 7),
                 expected = round(GA_from_hcfl_IG, digits = 7),
                 tolerance = 10e-4)

    crl_mm <- 30:50
    # The `intergrowth` function fails for inputs w/ length > 1, so working out
    # whether outputs are equal on one-by-one basis in `vapply()`
    gigs_ig_equality <- vapply(
      X = crl_mm, FUN.VALUE = logical(length = 1),
      FUN = \(crl) {
        gigs <- ig_fet_estimate_ga(crl_mm = crl)
        ig <- suppressMessages(suppressWarnings(
          intergrowth::calculate_gestage_crl(crl = crl)[[1]]
        ))
        gigs / 7 - ig < .Machine$double.eps
      })
    expect_true(all(gigs_ig_equality))
  }
)

# Functions still work with odd class structures -------------------------------

#' @srrstats {G2.6, EA2.6} INTERGROWTH-21st Fetal Growth functions still
#'   operate even when univariate inputs have unusual class structures, and
#'   output correct results.
test_that(
  desc = "Test that univariate inputs with alternate class structures work",
  code = {
    skip_on_cran()

    # Estimation of fetal weight
    headcirc <- units::set_units(x = 29L, cm)
    abdocirc <- units::set_units(x = 26L, cm)
    efw <- ig_fet_estimate_fetal_weight(headcirc_mm = headcirc,
                                        abdocirc_mm = abdocirc)
    expect_vector(efw, ptype = double(), size = 1)
    expect_equal(
      efw, ig_fet_estimate_fetal_weight(headcirc_mm = 29L, abdocirc_mm = 26L)
    )

    # Estimation of gestational age
    crl <- units::set_units(x = 30:60, mm)
    femurlen <- units::set_units(x = 30:60, mm)
    headcirc <- units::set_units(x = 230:260, mm)
    ga <- ig_fet_estimate_ga(
      crl_mm = crl,
      headcirc_mm = femurlen,
      femurlen_mm = headcirc
    )
    expect_vector(ga, ptype = double(), size = 31)
    expect_equal(ga,
                 ig_fet_estimate_ga(crl_mm = 30:60, headcirc_mm = 30:60,
                                    femurlen_mm = 230:260))
  }
)