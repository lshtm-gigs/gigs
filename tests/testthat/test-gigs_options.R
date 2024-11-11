#' @srrstats {G5.2a} The `message()` output of `gigs_options_get()`'s is checked
#'   here.
#' @srrstats {EA6.0e} `gigs_option_get()` checked for single-length character
#'   output.
test_that("Getting options works", {
  for (option in names(.gigs_options)) {
    # Should print out nice messages
    expect_message(
      gigs_option_get(option, silent = FALSE),
      paste0("gigs options: '", option, "' is currently set to \"",
             "[quiet|warn|error|keep_quiet|keep_warn|drop_quiet|drop_warn]",
             "*\".")
    )
    # Should not do that if silent = TRUE, but return a single string value
    checkmate::expect_string(gigs_option_get(option, silent = TRUE))
  }
})

#' @srrstats {G5.2a} The `message()` output of `gigs_options_get()`'s is checked
#'   here.
#' @srrstats {EA6.0e} `gigs_options_get()` checked for single-length character
#'   output.
test_that(
  desc = "Setting options works",
  code = {
    for (option in names(.gigs_options)) {
      if (option != "handle_unused_levels") {
        values <- c("quiet", "warn", "error")
      } else {
        values <- c("keep_silent", "keep_warn", "drop_silent", "drop_warn")
      }
      for (value in values) {
        # Should print out nice messages
        expect_message(
          gigs_option_set(option, value, silent = FALSE),
          paste0("gigs options: '",  option, "' is now set to \"", value, "\".")
        )
      }

      # Should not do that if silent = TRUE, but will return a string
      checkmate::expect_string(gigs_option_set(option, value, silent = TRUE))
    }
  }
)

# Reset package-level options back to default; keeps test behaviour consistent
gigs_input_options_set("warn", silent = TRUE)
gigs_option_set("handle_unused_levels", "keep_warn", TRUE)
