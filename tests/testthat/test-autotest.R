# Autotest infrastructure for package tests ------------------------------------

test_that(
  desc = "Relevant `autotest` checks pass",
  code = {
    skip_on_ci()
    skip_on_cran()
    skip(message = paste0(
      "There is a bug which causes errors in autotest when all tests with ",
      "issues are not supposed to be tested. For now, I am skipping instead ",
      "of having testthat fail.")
    )
    at_table <- autotest::autotest_package(test = FALSE)
    at_table$note <- rep_len(x = NA_character_, length.out = nrow(at_table))

    # Turn off some tests for gigs_options_get/set which are failing, but have
    # a justification
    is_gigs_option_get <- at_table$fn_name == "gigs_option_get"
    is_gigs_option_set <- at_table$fn_name == "gigs_option_set"
    is_gigs_option_fn <- is_gigs_option_get | is_gigs_option_set

    is_single_char_case <- at_table$test_name == "single_char_case"
    at_table$test[is_gigs_option_fn & is_single_char_case] <- FALSE

    at_table$note[is_gigs_option_fn & is_single_char_case] <-
      paste0("These tests are not applicable as they fail even though the ",
             "function behaves as expected. The tests expect no warnings with ",
             "lower-case input, but warnings can still occur if the `silent` ",
             "parameter is set to `TRUE`. With upper-case input, the function ",
             "fails due to case-sensitivity. This is documented in the ",
             "roxygen tags, but is being intercepted as a mistake by autotest.")

    is_silent_param <- at_table$parameter == "silent"
    is_negate_logical <- at_table$test_name == "negate_logical"
    at_table$test[is_gigs_option_fn & is_silent_param & is_negate_logical] <-
      FALSE
    at_table$note[is_gigs_option_fn & is_silent_param & is_negate_logical] <-
      paste0("These tests expect that changing this logical parameter will ",
             "not result in any messages/warnings/errors. As this parameter ",
             "directly controls whether messages are printed to the console ",
             "these tests cannot pass - even though expected and documented ",
             "behaviour is occurring.")

    at_results <- autotest::autotest_package(test_data = at_table, test = TRUE,
                                             functions = "gigs_option_get")
    at_results2 <- autotest::autotest_package(test_data = at_table,
                                              test = TRUE)
    expect_success(expect_autotest_testdata(at_table))
  }
)