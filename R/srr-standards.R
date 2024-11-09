#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#' (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.2} CONTRIBUTING.md has a 'planned development' and scope
#'   section.
#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' @srrstatsNA {G2.4, G2.4a, G2.4b, G2.4c, G2.4d, G2.4e} This package does not
#'   perform any of these conversions.
#' @srrstatsNA {G2.10} Column extraction is performed with rlang data-masking,
#'   so should be flexible for different `data.frame`-based objects.
#' @srrstatsNA {G2.14c} The package does not perform data imputation.
#' @srrstatsNA {G2.15} The package checks for missingness and informs the user
#'   throughout. Where base functions with `na.rm = TRUE` are used, this is done
#'   after checking for missingness, or otherwise handled deliberately.
#' @srrstatsNA {G3.1, G3.1a} This package does not perform covariance
#'   calculations.
#' @srrstatsNA {G4.0} This package does not write to local files.
#' @srrstatsNA {G5.3} This package does not return objects which explicitly
#'   contains no NA values.
#' @srrstatsNA {G5.7} This algorithms implemented in this package do not change
#'   in accuracy as input sizes change, and the time changes are marked in our
#'   benchmarking article.
#' @srrstatsNA {G5.10, G5.11, G5.11a, G5.12} This package has no extended tests.
#' @srrstatsNA {EA2.0, EA2.1, EA2.2, EA2.2a, EA2.2b, EA2.3, EA2.4, EA2.5} This
#'   package does not perform any extensive table filter/join operations.
#' @srrstatsNA {EA4.0} This package returns objects which are sufficiently
#'   different from input data types to make this requirement inapplicable.
#' @srrstatsNA {EA4.1, EA5.2} Users can choose to round or alter the continuous
#'   data this package calculates, but the package itself wouldn't benefit from
#'   this being implemented e.g. as a parameter.
#' @srrstatsNA {EA4.2} The objects returned by this package are simple vectors
#'   or data.frames, so we do not think they require custom print methods.
#' @srrstatsNA {EA5.0, EA5.0a, EA5.0b, EA5.1, EA5.4, EA5.5, EA5.6, EA6.1} This
#'   package does not produce graphical outputs.
#' @srrstatsNA {EA5.3} This package does not produce column-level summaries.
#' @noRd
NULL
