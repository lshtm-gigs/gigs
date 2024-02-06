#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#'
#' @srrstatsVerbose TRUE
#' @noRd
NULL

#' fulfilled_standards
#'
#' This block describes any SRR standards which are met, but do not have the
#' `@srrstats` tag placed next to all examples of the standard being met.
#'
#' @srrstats {G1.2} `.github/CONTRIBUTING.md` contains a lifecycle
#'   statement/roadmap.
#' @srrstats {G5.1} Datasets used to check gigs package are exported.
#' @srrstats {G2.15} No exported gigs functions assume non-missingness.
#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
#' @srrstatsNA {G2.14c} Imputation of values is not something we want to include
#'   in gigs, so we haven't included an option for it.
#' @srrstatsNA {G2.4, G2.4a, G2.4b, G2.4c ,G2.4d, G2.4e} No gigs functions use
#'   `as.*` functions to convert between data types.
#' @srrstatsNA {G2.5} No gigs functions request a factor variable as input.
#' @srrstatsNA {G2.7, G2.8, G2.9, G2.10, G2.11, G2.12} No gigs functions take
#'   tabular input.
#' @srrstatsNA {G3.1, G3.1a} No gigs functions use covariance calculations.
#' @srrstatsNA {G4.0} No gigs functions write to disk.
#' @srrstatsNA {G5.3} No gigs functions are expected to return strictly non-`NA`
#'   or non-`Inf`/non-`-Inf`/non-`NaN` values.
#' @srrstatsNA {G5.7} We don't have specific expectations for how our
#'   implementation will perform as the properties of our data change.
#' @srrstatsNA {G5.10, G5.11, G5.11a, G5.12} Presently, the gigs test suite
#'   runs fast enough that extended tests are not required.
#' @srrstatsNA {EA2.0, EA2.1, EA2.2, EA2.2a, EA2.2b, EA2.3, EA2.4, EA2.5} No
#'   gigs functions accept tabular/multi-tabular input, so no index column
#'   standards need to be met.
#' @srrstatsNA {EA3.0, EA3.1} We are still working on exact guidelines for which
#'   growth standards to use when several are available, and do not want to
#'   explicitly implement an algorithm that selects the 'best' growth standards
#'   before this guidance is written. An early implementation of this reasoning
#'   can be found in `gigs_zscoring.R`. Once we have published our guidelines,
#'   we will update the package code and documentation to reflect this, and move
#'   these standards to an `srrstats` tag at that point.
#' @srrstatsNA {EA4.0} Adherence to this standard is not suitable given the
#'   nature of analyses facilitated by gigs. For example, a user wanting
#'   centiles from anthropometric measurements encoded as integer values would
#'   not want to receive their centiles as integers. Similarly, limiting
#'   z-scores or predicted values based on the precision of units inputted by a
#'   user would not be appropriate.
#' @srrstatsNA {EA4.1} We would rather output double precision floats and let
#'   users round them, instead of implementing methods to artificially restrict
#'   gigs outputs to a set number of decimal places. This is for reasons similar
#'   to our reasoning for not fulfilling **EA4.0**.
#' @srrstatsNA {EA4.2} The objects returned by gigs are numeric vectors and
#'   factors, so do not need custom `print()` behaviour.
#' @srrstatsNA {EA5.0, EA5.0a, EA5.0b, EA5.1, EA5.2, EA5.3, EA5.4, EA5.5,
#'   EA5.6} The gigs package does not currently offer visualisation functions.
#' @srrstatsNA {EA6.0b, EA6.0c, EA6.0d} No exported gigs functions return
#'   tabular data.
#' @srrstatsNA {EA6.1} The gigs package does not currently offer visualisation
#'   functions.
#' @noRd
NULL
