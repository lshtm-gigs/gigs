% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/growth_categorise.R
\name{categorise_wasting}
\alias{categorise_wasting}
\title{Categorise weight-for-length/height z-scores into wasting strata}
\usage{
categorise_wasting(wlz, outliers = FALSE)
}
\arguments{
\item{wlz}{A numeric vector of length one or more containing
weight-for-length/height z-scores (WLZs).}

\item{outliers}{A single \code{TRUE} or \code{FALSE} value specifying whether
implausible z-score thresholds should be applied. Default = \code{FALSE}.}
}
\value{
An object of class factor with the same length as \code{wlz}, containing
wasting classifications. Its levels are \code{c("wasting_severe", "wasting", "not_wasting", "overweight")} if \code{outliers = FALSE} (the default), else
\code{c("wasting_severe", "wasting", "not_wasting", "overweight", "outlier")}.
By default, gigs will inform you this object contains unused factor levels.
You can change this behaviour using the
\link[=gigs_options]{GIGS package-level option}
\code{.gigs_options$handle_unused_levels}.
}
\description{
Categorise weight-for-length/height z-scores into wasting strata
}
\details{
Cut-offs for wasting categories are:

\tabular{lll}{
\strong{Category} \tab \strong{Factor level} \tab
\strong{Z-score bounds} \cr
Severe wasting \tab \code{"wasting_severe"}       \tab \code{wlz} =< -3      \cr
Wasting        \tab \code{"wasting"}              \tab -3 < \code{wlz} =< -2 \cr
No wasting     \tab \code{"not_wasting"}          \tab \code{abs(wlz)} < 2  \cr
Overweight     \tab \code{"overweight"}           \tab \code{wlz} >= 2      \cr
Outlier        \tab \code{"outlier"}              \tab \code{abs(wlz)} > 5
}
}
\note{
This function assumes that your measurements were taken according to
WHO guidelines, which stipulate that recumbent length should not be
measured after 730 days. Instead, standing height should be used.
Implausible z-score bounds are sourced from the referenced WHO report, and
classification cut-offs from the DHS manual.
}
\examples{
wlz <- c(-5.5, -3, 0, 3, 5.5)
categorise_wasting(wlz, outliers = FALSE)
categorise_wasting(wlz, outliers = TRUE)
}
\references{
\strong{'Implausible z-score values'} \emph{in} World Health Organization (ed.)
\emph{Recommendations for data collection, analysis and reporting on
anthropometric indicators in children under 5 years old}. Geneva: World
Health Organization and the United Nations Children's Fund UNICEF, (2019).
pp. 64-65.

\strong{'Percentage of children stunted, wasted, and underweight, and mean z-scores
for stunting, wasting and underweight'} \emph{in} \emph{Guide to DHS Statistics DHS-7}
Rockville, Maryland, USA: ICF (2020). pp. 431-435.
\url{https://dhsprogram.com/data/Guide-to-DHS-Statistics/Nutritional_Status.htm}
}
