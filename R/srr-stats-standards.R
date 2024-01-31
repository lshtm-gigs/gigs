#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#'
#' @srrstatsVerbose TRUE

## @srrstatsTODO {G5.2} *Appropriate error and warning behaviour of all functions should be explicitly demonstrated through tests. In particular,*
## @srrstatsTODO {G5.2a} *Every message produced within R code by `stop()`, `warning()`, `message()`, or equivalent should be unique*
## @srrstatsTODO {G5.2b} *Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values.*
## @srrstatsTODO {G5.4} **Correctness tests** *to test that statistical algorithms produce expected results to some fixed test data sets (potentially through comparisons using binding frameworks such as [RStata](https://github.com/lbraglia/RStata)).*
## @srrstatsTODO {G5.4a} *For new methods, it can be difficult to separate out correctness of the method from the correctness of the implementation, as there may not be reference for comparison. In this case, testing may be implemented against simple, trivial cases or against multiple implementations such as an initial R implementation compared with results from a C/C++ implementation.*
## @srrstatsTODO {G5.4b} *For new implementations of existing methods, correctness tests should include tests against previous implementations. Such testing may explicitly call those implementations in testing, preferably from fixed-versions of other software, or use stored outputs from those where that is not possible.*
## @srrstatsTODO {G5.4c} *Where applicable, stored values may be drawn from published paper outputs when applicable and where code from original implementations is not available*

## @srrstatsTODO {G5.6} **Parameter recovery tests** *to test that the implementation produce expected results given data with known properties. For instance, a linear regression algorithm should return expected coefficient values for a simulated data set generated from a linear model.*
## @srrstatsTODO {G5.6a} *Parameter recovery tests should generally be expected to succeed within a defined tolerance rather than recovering exact values.*
#' @srrstatsTODO {G5.8} **Edge condition tests** *to test that these conditions produce expected behaviour such as clear warnings or errors when confronted with data with extreme properties including but not limited to:*
#' @srrstatsTODO {G5.8a} *Zero-length data*
#' @srrstatsTODO {G5.8b} *Data of unsupported types (e.g., character or complex numbers in for functions designed only for numeric data)*
#' @srrstatsTODO {G5.8c} *Data with all-`NA` fields or columns or all identical fields or columns*
#' @srrstatsTODO {G5.8d} *Data outside the scope of the algorithm (for example, data with more fields (columns) than observations (rows) for some regression algorithms)*
## #' @srrstatsTODO {G5.9} **Noise susceptibility tests** *Packages should test for expected stochastic behaviour, such as through the following conditions:*
## #' @srrstatsTODO {G5.9a} *Adding trivial noise (for example, at the scale of `.Machine$double.eps`) to data does not meaningfully change results*
## #' @srrstatsTODO {G5.9b} *Running under different random seeds or initial conditions does not meaningfully change results*
#' @srrstatsTODO {EA3.0} *The algorithmic components of EDA Software should enable automated extraction and/or reporting of statistics as some sufficiently "meta" level (such as variable or model selection), for which previous or reference implementations require manual intervention.*
#' @srrstatsTODO {EA3.1} *EDA software should enable standardised comparison of inputs, processes, models, or outputs which previous or reference implementations otherwise only enable in some comparably unstandardised form.*
#' @srrstatsTODO {EA6.0} *Return values from all functions should be tested, including tests for the following characteristics:*
#' @srrstatsTODO {EA6.0a} *Classes and types of objects*
#' @srrstatsTODO {EA6.0e} *Values of single-valued objects; for `numeric` values either using `testthat::expect_equal()` or equivalent with a defined value for the `tolerance` parameter, or using `round(..., digits = x)` with some defined value of `x` prior to testing equality.* 
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
## #' @srrstatsNA {G5.6b} No gigs functions contain random components.
#' @srrstatsNA {G5.7} We don't have specific expectations for how our
#'   implementation will perform as the properties of our data change.
#' @srrstatsNA {G5.10, G5.11, G5.11a, G5.12} Presently,  the gigs test suite
#'   runs fast enough that extended tests are not required.
#' @srrstatsNA {EA2.0, EA2.1, EA2.2, EA2.2a, EA2.2b, EA2.3, EA2.4, EA2.5} No
#'   gigs functions accept tabular/multi-tabular input, so no index column
#'   standards need to be met.
#' @srrstatsNA {EA4.0} Adherence to this standard is not suitable given the
#'   nature of analyses facilitated by gigs. For example, a user wanting
#'   centiles from anthropometric measurements encoded as integer values would
#'   not want to receive their centiles as integers. Similarly, limiting
#'   z-scores or predicted values based on the precision of units inputted by a
#'   user would not be appropriate.
#' @srrstatsNA {EA4.1} We would rather output double precision floats and let
#'   users round them, instead of implementing methods to artificially restrict
#'   gigs outputs to a set number of decimal places. This is for reasons similar
#'   to our request not to adhere to **EA4.0**.
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
