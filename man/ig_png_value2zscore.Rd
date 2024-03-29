% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ig_png.R
\name{ig_png_value2zscore}
\alias{ig_png_value2zscore}
\alias{ig_png_wfa_value2zscore}
\alias{ig_png_lfa_value2zscore}
\alias{ig_png_hcfa_value2zscore}
\alias{ig_png_wfl_value2zscore}
\alias{ig_png_value2centile}
\alias{ig_png_wfa_value2centile}
\alias{ig_png_lfa_value2centile}
\alias{ig_png_hcfa_value2centile}
\alias{ig_png_wfl_value2centile}
\title{Convert values to z-scores/centiles in the INTERGROWTH-21\if{html}{\out{<sup>}}st\if{html}{\out{</sup>}}
Postnatal Growth Standards for preterm infants}
\usage{
ig_png_value2zscore(y, x, sex, acronym)

ig_png_wfa_value2zscore(weight_kg, pma_weeks, sex)

ig_png_lfa_value2zscore(length_cm, pma_weeks, sex)

ig_png_hcfa_value2zscore(headcirc_cm, pma_weeks, sex)

ig_png_wfl_value2zscore(weight_kg, length_cm, sex)

ig_png_value2centile(y, x, sex, acronym)

ig_png_wfa_value2centile(weight_kg, pma_weeks, sex)

ig_png_lfa_value2centile(length_cm, pma_weeks, sex)

ig_png_hcfa_value2centile(headcirc_cm, pma_weeks, sex)

ig_png_wfl_value2centile(weight_kg, length_cm, sex)
}
\arguments{
\item{y}{Numeric vector of length one or more with anthropometric
measurement(s) to convert to centiles/z-scores. Units depend on which
\code{acronym}(s) are in use.}

\item{x}{Numeric vector of length one or more with x values. Elements of \code{x}
or its standard-specific equivalents (\code{pma_weeks}, \code{length_cm}) should have
specific units and be between certain values depending on the standard in
use (defined by \code{acronym}). These are:
\itemize{
\item Between 27 and 64 weeks for \code{"wfa"}, \code{"lfa"}, and \code{"hcfa"}.
\item Between 35 and 65 days for \code{"wfl"}.
}

By default, gigs will replace out-of-bounds elements in \code{x} with \code{NA} and
warn you. This behaviour can be customised using the functions in
\link{gigs_options}.}

\item{sex}{Character vector of length one or more with sex(es), either \code{"M"}
(male) or \code{"F"} (female). This argument is case-sensitive. By default, gigs
will replace elements of \code{sex} which are not \code{"M"} or \code{"F"} with \code{NA}
and warn you. This behaviour can be customised using the functions in
\link{gigs_options}.}

\item{acronym}{Character vector of length one or more denoting the
INTERGROWTH-21\if{html}{\out{<sup>}}st\if{html}{\out{</sup>}} Postnatal Growth standard(s) in use. Each
element should be one of:
\itemize{
\item \code{"wfa"} (weight-for-age)
\item \code{"lfa"} (length-for-age)
\item \code{"hcfa"} (head circumference-for-age)
\item \code{"wfl"} (weight-for-length)
}

This argument is case-sensitive. By default, gigs will replace elements in
\code{acronym} which are not one of the above values with \code{NA} and warn you.
This behaviour can be customised using the functions in \link{gigs_options}. If
all elements in \code{acronym} are not one of the above values, gigs will throw
an error.}

\item{weight_kg}{Numeric vector of length one or more with weight
measurement(s) in kg.}

\item{pma_weeks}{Numeric vector of length one or more with post-menstrual
age(s) in weeks. Values should be within the bounds defined in the
documentation for \code{x}.}

\item{length_cm}{Numeric vector of length one or more with recumbent length
measurement(s) in cm. This argument can be either an \code{x} variable when
using the length-for-age standard (\code{"lfa"}), or a \code{y} variable when using
the weight-for-length (\code{"wfl"}) standard.}

\item{headcirc_cm}{Numeric vector of length one or more with head
circumference measurement(s) in cm.}
}
\value{
Numeric vector of z-scores/centiles with length equal to the longest
input vector.
}
\description{
Convert values to z-scores/centiles in the INTERGROWTH-21\if{html}{\out{<sup>}}st\if{html}{\out{</sup>}}
Postnatal Growth Standards for preterm infants
}
\note{
Input vectors are recycled by \code{\link[vctrs:vec_recycle]{vctrs::vec_recycle_common()}}, and must
adhere to the \link{vctrs} recycling rules.
}
\examples{
# Convert values to centiles
ig_png_value2centile(y = c(5.79, 5.92, 7.25),
                     x = 54:56,
                     sex = c("M", "F", "M"),
                     acronym = "wfa") |>
  round(digits = 2)

# Or values to z-scores
ig_png_value2zscore(y = c(5.79, 5.92, 7.25),
                    x = 54:56,
                    sex = c("M", "F", "M"),
                    acronym = "wfa") |>
  round(digits = 2)

# Specify which standard to use with the acronym parameter...
ig_png_value2zscore(y = c(60.2, 60.3, 64.1),
                    x = 54:56,
                    sex = c("M", "F", "M"),
                    acronym = "lfa") |>
  round(digits = 2)

# ... or by using a standard-specific function
ig_png_lfa_value2zscore(length_cm = c(60.2, 60.3, 64.1),
                        pma_weeks = 54:56,
                        sex = c("M", "F", "M")) |>
  round(digits = 2)

# Inputs are recycled to the input of the longest length
ig_png_lfa_value2zscore(length_cm = c(51.10, 51.48, 51.86, 52.24, 52.63),
                        pma_weeks = 40,
                        sex = "M") |>
  round(digits = 2)

# Bad inputs will not stop the function but will instead produce `NA`s in the
# output - by default gigs will issue useful warnings
ig_png_hcfa_value2centile(headcirc_cm = c(20.6, NA, 38.2, 42.8),
                          pma_weeks = c(25, 27, 46, 64),
                          sex = "M") |>
  round(digits = 2)
}
\references{
Villar J, Giuliani F, Bhutta ZA, Bertino E, Ohuma EO, Ismail LC et al.
\strong{Postnatal growth standards for preterm infants: the Preterm Postnatal
Follow-up Study of the INTERGROWTH-21st Project.} \emph{Lancet Glob Health} 2015,
\emph{3(11):e681-e691.} \doi{10.1016/S2214-109X(15)00163-1}
}
