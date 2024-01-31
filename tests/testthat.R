# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(gigs)

test_check("gigs")

#' @srrstats {G5.0, G5.1} gigs conversion functions are validated against growth
#'   curve tables published by the World Health Organisation and
#'   INTERGROWTH-21st Project. These tables are exported within the package as
#'   `gigs::ig_nbs`, `gigs::ig_png`, `gigs::ig_fet`, and `gigs::who_gs`.