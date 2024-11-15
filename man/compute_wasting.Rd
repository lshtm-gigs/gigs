% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/growth_compute.R
\name{compute_wasting}
\alias{compute_wasting}
\title{Get wasting categories using multiple vectors and GIGS-recommended growth
standards}
\usage{
compute_wasting(
  weight_kg,
  lenht_cm,
  age_days,
  gest_days,
  sex,
  id = NULL,
  outliers = FALSE
)
}
\arguments{
\item{weight_kg}{Numeric vector of length one or more with weight
measurement(s) in kg.}

\item{lenht_cm}{Numeric vector of length one or more with length/height
measurement(s) in cm.}

\item{age_days}{Numeric vector of length one or more with age(s) in days
for each child. Should be between \code{0} to \code{1856} days. By default, gigs will
replace out-of-bounds elements in \code{age_days} with \code{NA} and warn you. This
behaviour can be customised using the functions in \link{gigs_options}.}

\item{gest_days}{Numeric vector with gestational age(s) at birth in days.}

\item{sex}{Character vector of length one or more with sex(es), either \code{"M"}
(male) or \code{"F"} (female). This argument is case-sensitive. By default, gigs
will replace elements of \code{sex} which are not \code{"M"} or \code{"F"} with \code{NA}
and warn you. You can customise this behaviour using the \link[=gigs_options]{GIGS package-level options}.}

\item{id}{A factor of length one or more with an ID for each observation,
either ordered or unordered, containing no missing (\code{NA}) values. When
supplied, \code{id} is used to ensure that only the earliest measurement for
each individual is used as a birth measure. Leave this argument as \code{NULL}
if all your data comes from the same individual. Default = \code{NULL}.}

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
Get wasting categories using multiple vectors and GIGS-recommended growth
standards
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
# The first observation for each infant in `id` uses the INTERGROWTH-21st
#  Newborn Size standards; the next two use either the INTERGROWTH-21st
# Postnatal Growth standards or WHO Child Growth Standards.
compute_wasting(
  weight_kg = c(3, 6, 11, 3, 6, 11),
  lenht_cm = c(52.2, 60.4, 61, 52.2, 60.4, 61),
  age_days = c(0, 100, 500, 2, 100, 500),
  gest_days = c(245, 245, 245, 280, 280, 280),
  sex = c("M", "M", "M", "F", "F", "F"),
  id = factor(c("A", "A", "A", "B", "B", "B"))
)

# With outlier flagging:
compute_wasting(
  weight_kg = c(3, 6, 11, 3, 6, 11),
  lenht_cm = c(52.2, 60.4, 61, 52.2, 60.4, 61),
  age_days = c(0, 100, 500, 2, 100, 500),
  gest_days = c(245, 245, 245, 280, 280, 280),
  sex = c("M", "M", "M", "F", "F", "F"),
  id = factor(c("A", "A", "A", "B", "B", "B")),
  outliers = TRUE
)

# If you don't specify `id`, the function will not identify that the fourth
# data point is a birth measurement. This means a weight-for-length z-score
# (and resulting wasting classification) will be calculated for the fourth
# data point, which is wrong here
compute_wasting(
  weight_kg = c(3, 6, 11, 3, 6, 11),
  lenht_cm = c(52.2, 60.4, 61, 52.2, 60.4, 61),
  age_days = c(0, 100, 500, 2, 100, 500),
  gest_days = c(245, 245, 245, 280, 280, 280),
  sex = c("M", "M", "M", "F", "F", "F")
)
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
