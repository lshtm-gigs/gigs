% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/growth_classify.R
\name{classify_sfga}
\alias{classify_sfga}
\title{Classify size-for-gestational age in \code{data.frame}-like objects with the
INTERGROWTH-21\if{html}{\out{<sup>}}st\if{html}{\out{</sup>}} weight-for-gestational age standard}
\usage{
classify_sfga(
  .data,
  weight_kg,
  gest_days,
  sex,
  .new = c("birthweight_centile", "sfga", "sfga_severe")
)
}
\arguments{
\item{.data}{A \code{data.frame}-like tabular object with one or more rows. Must
be permissible by \code{\link[checkmate:checkDataFrame]{checkmate::assert_data_frame()}}, so you can also supply
a \code{tibble}, \code{data.table}, or similar.}

\item{weight_kg}{<\code{\link[rlang:args_data_masking]{data-masking}}> The name of a
column in \code{.data} which is a numeric vector of birth weight values in kg.
It is assumed that weight measurements provided to this function are
birth weights recorded <12 hours after an infant's birth.}

\item{gest_days}{<\code{\link[rlang:args_data_masking]{data-masking}}> The name of a
column in \code{.data} which is a numeric vector of gestational age(s) at birth
in days between \code{168} and \code{300}. By default, gigs will warn you about
elements of this vector which are outside these bounds, are \code{NA}, or \code{Inf}.
You can customise this behaviour using the \link[=gigs_options]{GIGS package-level options}.}

\item{sex}{<\code{\link[rlang:args_data_masking]{data-masking}}> The name of a
column in \code{.data} which is a case-sensitive character vector of sexes,
either \code{"M"} (male) or \code{"F"} (female). By default, gigs will warn you if
any elements of \code{sex} are not \code{"M"} or \code{"F"}, or are missing (\code{NA}). You
can customise this behaviour using the \link[=gigs_options]{GIGS package-level options}.}

\item{.new}{A three-length character vector with names for the output
columns. These inputs will be repaired if necessary using
\code{\link[vctrs:vec_as_names]{vctrs::vec_as_names()}}, which will print any changes to the console. If
any elements in \code{.new} are the same as elements in \code{colnames(.data)}, the
function will throw an error. Default = \code{c("birthweight_centile", "sfga", "sfga_severe")}.}
}
\value{
A tabular object of the same class that was provided as \code{.data},
with three new columns named according to \code{.new}. These columns will be
(from left to right):
\itemize{
\item \code{birthweight_centile} - Numeric vector of birthweight centiles from the
INTERGROWTH-21\if{html}{\out{<sup>}}st\if{html}{\out{</sup>}} Newborn Size standard for
weight-for-GA
\item \code{sfga} - Factor of size-for-GA categories without severe small-for-GA
classification
\item \code{sfga_severe} - Factor of size-for-GA categories with severe small-for-GA
classification
}
}
\description{
Classify size-for-gestational age in \code{data.frame}-like objects with the
INTERGROWTH-21\if{html}{\out{<sup>}}st\if{html}{\out{</sup>}} weight-for-gestational age standard
}
\details{
Cut-offs for size-for-gestational age categorisations are:

\tabular{lll}{
\strong{Category} \tab \strong{Factor level} \tab
\strong{Centile bounds} \cr
Severely small-for-GA \tab \code{"SGA(<3)"} \tab \code{p} < 0.03      \cr
Small-for-GA          \tab \code{"SGA"}     \tab \code{p} < 0.1       \cr
Appropriate-for-GA    \tab \code{"AGA"}     \tab 0.1 =< \code{p} =< 0.9 \cr
Large-for-GA          \tab \code{"LGA"}     \tab \code{p} > 0.9
}
}
\note{
Categorical (factor) columns produced here may contain unused factor
levels. By default, gigs will inform you if these columns have unused
factor levels. You can change this behaviour using the
\link[=gigs_options]{GIGS package-level option}
\code{.gigs_options$handle_unused_levels}.
}
\examples{
data <- data.frame(
 wt_kg = c(2.2, 2.5, 3.3, 4.0),
 gestage = 266:269,
 sex = c("F", "M", "F", "M")
)

data |>
  classify_sfga(weight_kg = wt_kg,
                gest_days = gestage,
                sex = sex)
}
\references{
WHO. \strong{Physical status: the use and interpretation of anthropometry. Report
of a WHO Expert Committee.} \emph{World Health Organisation Technical Report
Series 1995,} \strong{854: 1–452}

Royal College of Obstetricians and Gynaecologists. \strong{The Investigation and
Management of the Small-for-Gestational-Age Fetus: Green-top Guideline No.
31.} \emph{Technical report, Royal College of Obstetricians and Gynaecologists,
London, 2013.}
}
\seealso{
See \code{\link[=classify_svn]{classify_svn()}} for size-for-GA classifications which are
stratified by whether a newborn is term. See \code{\link[=classify_growth]{classify_growth()}} to run
this analysis and others at the same time.
}
