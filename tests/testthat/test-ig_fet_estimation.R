# Correctness tests ------------------------------------------------------------

#' @srrstats {G5.4, G5.4c} Tests ensure that `gigs` functions can be used to
#'   replicate published growth charts, within a tolerance. Tests use expected
#'   values from examples in linked papers.
test_that(desc = "EFW function gives correct outputs", {
  # Example from https://dx.doi.org/10.1002/uog.17347
  expect_equal(object = round(ig_fet_estimate_fetal_weight(abdocirc_mm = 260,
                                                           headcirc_mm = 290)),
               expected = 1499)
})
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

#' @srrstats {G5.9, G5.9a} Trivial noise does not meaningfully alter results.
test_that(desc = "EFW function gives correct outputs", {
  # Example from https://dx.doi.org/10.1002/uog.17347
  expect_equal(
    object = round(
      ig_fet_estimate_fetal_weight(
        abdocirc_mm = 260 + sqrt(.Machine$double.eps),
        headcirc_mm = 290 + sqrt(.Machine$double.eps)
      )),
    expected = 1499)
})
test_that(desc = "GA estimation function tolerates some noise", {
  # From GA for CRL standard
  gafcrl_tbl <- gigs::ig_fet[["gafcrl"]][[1]]
  crl_mm <- gafcrl_tbl[[1]][2:40]
  P50_table <- gafcrl_tbl[["P50"]][2:40]
  GA_estimations <- round(
    ig_fet_estimate_ga(crl_mm = crl_mm + sqrt(.Machine$double.eps))
  )
  expect_equal(object = GA_estimations, expected = P50_table)

  # Examples taken from https://dx.doi.org/10.1002/uog.15894
  expect_equal(
    object = round(
      ig_fet_estimate_ga(headcirc_mm = 250 + sqrt(.Machine$double.eps)),
      digits = 1),
    expected = 189.8)
  expect_equal(
    object = round(
      ig_fet_estimate_ga(headcirc_mm = 250 + sqrt(.Machine$double.eps),
                         femurlen_mm = 55 + sqrt(.Machine$double.eps)),
      digits = 1),
    expected = 200.5)
})

# Appropriate errors for zero-length data + data of wrong type -----------------
#
# #' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
# #' @srrstats {G5.8, G5.8a, G5.8b} Including errors for zero-length inputs and
# #'   incorrect data types.
# test_that("Bad GA estimation calls give good errors", {
#   # Non-numeric input errors
#   expect_error(
#     ig_fet_estimate_ga(crl_mm = character(1), headcirc_mm = 250, femurlen_mm = 50),
#     test_error_wrong_type("crl_mm", "numeric", "character")
#   )
#   expect_error(
#     ig_fet_estimate_ga(crl_mm = 25, headcirc_mm = complex(1), femurlen_mm = 50),
#     test_error_wrong_type("headcirc_mm", "numeric", "complex")
#   )
#   expect_error(
#     ig_fet_estimate_ga(crl_mm = 25, headcirc_mm = 250, femurlen_mm = logical(1)),
#     test_error_wrong_type("femurlen_mm", "numeric", "logical")
#   )
#
#   # Errors based on input length
#   expect_error(
#     ig_fet_estimate_ga(crl_mm = integer(), headcirc_mm = 250, femurlen_mm = 50),
#     test_error_zero_length("crl_mm")
#   )
#   expect_error(
#     ig_fet_estimate_ga(crl_mm = 25, headcirc_mm = double(), femurlen_mm = 50),
#     test_error_zero_length("headcirc_mm")
#   )
#   expect_error(
#     ig_fet_estimate_ga(crl_mm = integer(), headcirc_mm = 50, femurlen_mm = numeric()),
#     test_error_zero_length(c("crl_mm", "femurlen_mm"))
#   )
#
#   # NULL for crl_mm + headcirc_mm inputs
#   expect_error(
#     ig_fet_estimate_ga(crl_mm = NULL, headcirc_mm = NULL, femurlen_mm = 50),
#     regexp = "At least one of `crl_mm` or `headcirc_mm` must not be `NULL`"
#   )
#
#   # NULL for all inputs
#   expect_error(
#     ig_fet_estimate_ga(crl_mm = NULL, headcirc_mm = NULL, femurlen_mm = NULL),
#     paste0("All inputs were `NULL`. Ensure some inputs are not `NULL`, then ",
#            "try again.")
#   )
# })
#
# test_that(
#   desc = "Bad fetal weight estimation calls give good errors", code = {
#   # Non-numeric input errors
#   expect_error(
#     ig_fet_estimate_fetal_weight(abdocirc_mm = character(1), headcirc_mm = 250),
#     test_error_wrong_type("abdocirc_mm", "numeric", "character")
#   )
#   expect_error(
#     ig_fet_estimate_fetal_weight(abdocirc_mm = 250, headcirc_mm = logical(1)),
#     test_error_wrong_type("headcirc_mm", "numeric", "logical")
#   )
#
#   # Errors based on input length
#   expect_error(
#     ig_fet_estimate_fetal_weight(abdocirc_mm = numeric(), headcirc_mm = 250),
#     test_error_zero_length("abdocirc_mm")
#   )
#   expect_error(
#     ig_fet_estimate_fetal_weight(abdocirc_mm = 250, headcirc_mm = double()),
#     test_error_zero_length("headcirc_mm")
#   )
#   expect_error(
#     ig_fet_estimate_fetal_weight(abdocirc_mm = numeric(), headcirc_mm = double()),
#     test_error_zero_length(c("abdocirc_mm", "headcirc_mm"))
#   )
#
#   # NULL inputs
#   expect_error(
#     ig_fet_estimate_fetal_weight(abdocirc_mm = NULL, headcirc_mm = 50),
#     "Variable 'abdocirc_mm' was `NULL`, but should have a value."
#   )
#   expect_error(
#     ig_fet_estimate_fetal_weight(abdocirc_mm = NULL, headcirc_mm = 50),
#     "Variable 'abdocirc_mm' was `NULL`, but should have a value."
#   )
# })
#
# # Appropriate handling of non-fatal issues with input data ---------------------
#
# #' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
# #' @srrstats{G5.8, G5.8d} Show that gigs can handle data outside the
# #'   scope of its algorithms by replacing bad inputs with `NA`, and simply
# #'   outputting `NA` for the output elements which map onto `NA` input
# #'   elements.
# #' @srrstats {EA6.0, EA6.0a} Also checks that classes/types of returned objects
# #'   are correct using [checkmate::expect_numeric()]
# test_that(
#   desc = "Invalid values can be replaced with `NA` quietly",
#   code = {
#     # Make gigs *quietly* replace bad data with NA
#     for (option in names(.gigs_options)) {
#       gigs_option_set(option, new_value = "quiet", silent = TRUE)
#     }
#
#     input_len <- 40
#     headcirc_mm <- rep_len(285:294, length.out = input_len)
#     abdocirc_mm <- rep_len(255:264, length.out = input_len)
#     crl_mm <- rep_len(40:49, length.out = input_len)
#     femurlen_mm <- rep_len(50:59, length.out = input_len)
#
#     # Random set of indices to replace with bad data in each case
#     num_to_replace1 <- 7
#     withr::with_seed(465, code = {
#       replace_ints_1 <- sample(1L:input_len, size = num_to_replace1)
#     })
#     num_to_replace2 <- 8
#     withr::with_seed(420, code = {
#       replace_ints_2 <- sample(1L:input_len, size = num_to_replace2)
#     })
#     replace_ints_3 <- union(replace_ints_1, replace_ints_2)
#     num_to_replace3 <- length(union(replace_ints_1, replace_ints_2))
#
#     # Bad input 1: Undefined data (NaN, Inf, -Inf) -----------------------------
#     for (undefined_val in c(NaN, Inf, -Inf)) {
#       ## Do for ig_fet_estimate_fetal_weight
#       out <- ig_fet_estimate_fetal_weight(
#         abdocirc_mm,
#         replace(headcirc_mm, replace_ints_2, undefined_val)
#       )
#       checkmate::expect_numeric(out, len = input_len)
#       expect_true(all(is.na(out[replace_ints_2])))
#
#       ## Do for ig_fet_estimate_fetal_weight
#       out <- ig_fet_estimate_fetal_weight(
#         replace(abdocirc_mm, replace_ints_1, undefined_val),
#         replace(headcirc_mm, replace_ints_2, undefined_val)
#       )
#       checkmate::expect_numeric(out, len = input_len)
#       expect_true(all(is.na(out[replace_ints_3])))
#
#       ## Do for ig_fet_estimate_ga
#       out <- ig_fet_estimate_ga(
#         NULL,
#         replace(femurlen_mm, replace_ints_2, undefined_val),
#         replace(femurlen_mm, replace_ints_1, undefined_val)
#       )
#       checkmate::expect_numeric(out, len = input_len)
#       expect_true(all(is.na(out[replace_ints_3])))
#       out <- ig_fet_estimate_ga(
#         replace(crl_mm, replace_ints_1, undefined_val),
#         abdocirc_mm,
#         femurlen_mm
#       )
#       checkmate::expect_numeric(out, len = input_len)
#       expect_true(all(is.na(out[replace_ints_1])))
#     }
#
#     # Bad input 2: Missing data (NA) -------------------------------------------
#     ## Do for ig_fet_estimate_fetal_weight
#     out <- ig_fet_estimate_fetal_weight(
#       abdocirc_mm,
#       replace(headcirc_mm, replace_ints_2, NA)
#     )
#     checkmate::expect_numeric(out, len = input_len)
#     expect_true(all(is.na(out[replace_ints_2])))
#
#     ## Do for ig_fet_estimate_fetal_weight
#     out <- ig_fet_estimate_fetal_weight(
#       replace(abdocirc_mm, replace_ints_1, NA),
#       replace(headcirc_mm, replace_ints_2, NA)
#     )
#     checkmate::expect_numeric(out, len = input_len)
#     expect_true(all(is.na(out[replace_ints_3])))
#
#     ## Do for ig_fet_estimate_ga
#     out <- ig_fet_estimate_ga(
#       NULL,
#       replace(femurlen_mm, replace_ints_2, NA),
#       replace(femurlen_mm, replace_ints_1, NA)
#     )
#     checkmate::expect_numeric(out, len = input_len)
#     expect_true(all(is.na(out[replace_ints_3])))
#     out <- ig_fet_estimate_ga(
#       replace(crl_mm, replace_ints_1, NA),
#       abdocirc_mm,
#       femurlen_mm
#     )
#     checkmate::expect_numeric(out, len = input_len)
#     expect_true(all(is.na(out[replace_ints_1])))
#   }
# )

# Appropriate warnings/errors with bad input data ------------------------------

# #' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
# #' @srrstats{G5.8, G5.8d} Show that gigs can handle data outside the
# #'   scope of its algorithms by replacing bad inputs with `NA` and giving clear
# #'   warnings.
# test_that(
#   desc = "Invalid values can be replaced with `NA` with a warning",
#   code = {
#     # Make gigs replace bad data with `NA` and warn
#     for (option in names(.gigs_options)) {
#       gigs_option_set(option, new_value = "warn", silent = TRUE)
#     }
#
#     input_len <- 40
#     headcirc_mm <- rep_len(285:294, length.out = input_len)
#     abdocirc_mm <- rep_len(255:264, length.out = input_len)
#     crl_mm <- rep_len(40:49, length.out = input_len)
#     femurlen_mm <- rep_len(50:59, length.out = input_len)
#
#     # Random set of indices to replace with bad data in each case
#     num_to_replace1 <- 7
#     withr::with_seed(465, code = {
#       replace_ints_1 <- sample(1L:input_len, size = num_to_replace1)
#     })
#     num_to_replace2 <- 8
#     withr::with_seed(420, code = {
#       replace_ints_2 <- sample(1L:input_len, size = num_to_replace2)
#     })
#     replace_ints_3 <- union(replace_ints_1, replace_ints_2)
#     num_to_replace3 <- length(union(replace_ints_1, replace_ints_2))
#
#     # Bad input 1: Undefined data (NaN, Inf, -Inf) -----------------------------
#     for (undefined_val in c(NaN, Inf, -Inf)) {
#       ## Do for ig_fet_estimate_fetal_weight
#       expect_warning(
#         ig_fet_estimate_fetal_weight(
#           abdocirc_mm,
#           replace(headcirc_mm, replace_ints_2, undefined_val)
#         ),
#         test_msg_undefined("headcirc_mm", input_len, num_to_replace2)
#       )
#
#       ## Do for ig_fet_estimate_fetal_weight
#       warnings <- capture_warnings(
#         ig_fet_estimate_fetal_weight(
#           replace(abdocirc_mm, replace_ints_1, undefined_val),
#           replace(headcirc_mm, replace_ints_3, undefined_val)
#         ))
#       expect_match(
#         warnings[[1]],
#         test_msg_undefined("abdocirc_mm", input_len, num_to_replace1)
#       )
#       expect_match(
#         warnings[[2]],
#         test_msg_undefined("headcirc_mm", input_len, num_to_replace3)
#       )
#
#       ## Do for ig_fet_estimate_ga
#       warnings <- capture_warnings(
#         ig_fet_estimate_ga(
#           NULL,
#           replace(headcirc_mm, replace_ints_2, undefined_val),
#           replace(femurlen_mm, replace_ints_1, undefined_val)
#         ))
#       expect_match(
#         warnings[[1]],
#         test_msg_undefined("headcirc_mm", input_len, num_to_replace2)
#       )
#       expect_match(
#         warnings[[2]],
#         test_msg_undefined("femurlen_mm", input_len, num_to_replace1)
#       )
#
#       expect_warning(
#         ig_fet_estimate_ga(
#           replace(crl_mm, replace_ints_1, undefined_val),
#           abdocirc_mm,
#           femurlen_mm),
#         test_msg_undefined("crl_mm", input_len, num_to_replace1)
#       )
#     }
#
#     # Bad input 2: Missing data (NA) -------------------------------------------
#     ## Do for ig_fet_estimate_fetal_weight
#     expect_warning(
#       ig_fet_estimate_fetal_weight(
#         abdocirc_mm,
#         replace(headcirc_mm, replace_ints_1, NA)
#       ),
#       test_msg_missing("headcirc_mm", input_len, num_to_replace1)
#     )
#
#     ## Do for ig_fet_estimate_fetal_weight
#     warnings <- capture_warnings(
#       ig_fet_estimate_fetal_weight(
#         replace(abdocirc_mm, replace_ints_2, NA),
#         replace(headcirc_mm, replace_ints_3, NA)
#       ))
#     expect_match(
#       warnings[[1]],
#       test_msg_missing("abdocirc_mm", input_len, num_to_replace2)
#     )
#     expect_match(
#       warnings[[2]],
#       test_msg_missing("headcirc_mm", input_len, num_to_replace3)
#     )
#
#     ## Do for ig_fet_estimate_ga
#     warnings <- capture_warnings(
#       ig_fet_estimate_ga(
#         NULL,
#         replace(headcirc_mm, replace_ints_1, NA),
#         replace(femurlen_mm, replace_ints_2, NA)
#       ))
#     expect_match(
#       warnings[[1]],
#       test_msg_missing("headcirc_mm", input_len, num_to_replace1)
#     )
#     expect_match(
#       warnings[[2]],
#       test_msg_missing("femurlen_mm", input_len, num_to_replace2)
#     )
#
#     expect_warning(
#       ig_fet_estimate_ga(
#         replace(crl_mm, replace_ints_3, NA),
#         abdocirc_mm,
#         femurlen_mm),
#       test_msg_missing("crl_mm", input_len, num_to_replace3)
#     )
#   }
# )
#
# #' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
# #' @srrstats{G5.8, G5.8d} Show that gigs can handle data outside the
# #'   scope of its algorithms by replacing bad inputs with `NA` and giving clear
# #'   errors.
# #' @srrstats {G5.2, G5.2a, G5.2b} Explicit tests of error and warning behaviour.
# #' @srrstats{G5.8, G5.8d} Show that gigs can handle data outside the
# #'   scope of its algorithms by replacing bad inputs with `NA` and giving clear
# #'   warnings.
# test_that(
#   desc = "Invalid values can be replaced with `NA` with errors",
#   code = {
#     # Make gigs *quietly* replace bad data with NA
#     for (option in names(.gigs_options)) {
#       gigs_option_set(option, new_value = "error", silent = TRUE)
#     }
#
#         input_len <- 40
#     headcirc_mm <- rep_len(285:294, length.out = input_len)
#     abdocirc_mm <- rep_len(255:264, length.out = input_len)
#     crl_mm <- rep_len(40:49, length.out = input_len)
#     femurlen_mm <- rep_len(50:59, length.out = input_len)
#
#     # Random set of indices to replace with bad data in each case
#     num_to_replace1 <- 7
#     withr::with_seed(465, code = {
#       replace_ints_1 <- sample(1L:input_len, size = num_to_replace1)
#     })
#     num_to_replace2 <- 8
#     withr::with_seed(420, code = {
#       replace_ints_2 <- sample(1L:input_len, size = num_to_replace2)
#     })
#     replace_ints_3 <- union(replace_ints_1, replace_ints_2)
#     num_to_replace3 <- length(union(replace_ints_1, replace_ints_2))
#
#     # Bad input 1: Undefined data (NaN, Inf, -Inf) -----------------------------
#     for (undefined_val in c(NaN, Inf, -Inf)) {
#       ## Do for ig_fet_estimate_fetal_weight
#       expect_error(
#         ig_fet_estimate_fetal_weight(
#           abdocirc_mm,
#           replace(headcirc_mm, replace_ints_2, undefined_val)
#         ),
#         test_msg_undefined("headcirc_mm", input_len, num_to_replace2)
#       )
#
#       ## Do for ig_fet_estimate_fetal_weight
#       expect_error(
#         ig_fet_estimate_fetal_weight(
#           replace(abdocirc_mm, replace_ints_1, undefined_val),
#           replace(headcirc_mm, replace_ints_3, undefined_val)
#         ),
#         test_msg_undefined("abdocirc_mm", input_len, num_to_replace1)
#       )
#
#       ## Do for ig_fet_estimate_ga
#      expect_error(
#         ig_fet_estimate_ga(
#           NULL,
#           replace(headcirc_mm, replace_ints_1, undefined_val),
#           replace(femurlen_mm, replace_ints_2, undefined_val)
#         ),
#         test_msg_undefined("headcirc_mm", input_len, num_to_replace1)
#       )
#
#       expect_error(
#         ig_fet_estimate_ga(
#           replace(crl_mm, replace_ints_3, undefined_val),
#           abdocirc_mm,
#           femurlen_mm),
#         test_msg_undefined("crl_mm", input_len, num_to_replace3)
#       )
#     }
#
#     # Bad input 2: Missing data (NA) -------------------------------------------
#     ## Do for ig_fet_estimate_fetal_weight
#     expect_error(
#       ig_fet_estimate_fetal_weight(
#         abdocirc_mm,
#         replace(headcirc_mm, replace_ints_2, NA)
#       ),
#       test_msg_missing("headcirc_mm", input_len, num_to_replace2)
#     )
#
#     ## Do for ig_fet_estimate_fetal_weight
#     expect_error(
#       ig_fet_estimate_fetal_weight(
#         replace(abdocirc_mm, replace_ints_1, NA),
#         replace(headcirc_mm, replace_ints_2, NA)),
#       test_msg_missing("abdocirc_mm", input_len, num_to_replace1)
#     )
#
#     ## Do for ig_fet_estimate_ga
#     expect_error(
#       ig_fet_estimate_ga(
#         NULL,
#         replace(headcirc_mm, replace_ints_3, NA),
#         replace(femurlen_mm, replace_ints_2, NA)
#       ),
#       test_msg_missing("headcirc_mm", input_len, num_to_replace3)
#     )
#
#     expect_error(
#       ig_fet_estimate_ga(
#         replace(crl_mm, replace_ints_3, NA),
#         abdocirc_mm,
#         femurlen_mm),
#       test_msg_missing("crl_mm", input_len, num_to_replace3)
#     )
#   }
# )

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
        (gigs / 7 - ig) < .Machine$double.eps
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