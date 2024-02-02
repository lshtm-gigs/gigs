library(testthat)

#' @srrstats {G5.2a} The `message()` output of `gigs_options_get()`'s is checked
#'   here.
#' @srrstats {EA6.0e} `gigs_options_get()` checked for single-length character
#'   output.
test_that(
  desc= "Getting options works",
  code = {
    for (option in names(.gigs_options)) {

      # Should print out nice messages
      expect_message(
        gigs_option_get(option),
        paste0("gigs options: `", option, "` is currently set to \"",
               "[quiet|warn|error]*\".")
      )

      # Should not do that if silent = TRUE, but return a single string value
      checkmate::expect_string(gigs_option_get(option, silent = TRUE))
    }

  }
)

#' @srrstats {G5.2a} The `message()` output of `gigs_options_get()`'s is checked
#'   here.
#' @srrstats {EA6.0e} `gigs_options_get()` checked for single-length character
#'   output.
test_that(
  desc= "Setting options works",
  code = {
    for (option in names(.gigs_options)) {
      for (value in c("quiet", "warn", "error")) {
        # Should print out nice messages
        expect_message(
          gigs_option_set(option, value),
          paste0("gigs options: `",  option, "` is now set to \"", value, "\".")
        )
      }

      # Should not do that if silent = TRUE, but return a single string value
      checkmate::expect_string(gigs_option_set(option, value, silent = TRUE))
    }
  }
)
