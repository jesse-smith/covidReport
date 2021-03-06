% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inspections_table.R
\name{insp_replace_year_long}
\alias{insp_replace_year_long}
\title{Replace Malformed Years in Inspections Data}
\usage{
insp_replace_year_long(x, quiet = FALSE)
}
\arguments{
\item{x}{A vector of dates}

\item{quiet}{Should a warning be issued when malformed dates are detected?}
}
\value{
The corrected date vector
}
\description{
\code{insp_replace_year_long()} replaces numeric years which only have
three digits, rather than the expected four (i.e. \code{202} instead of \code{2020}).
The replacement value is the current year. This function is intended for
use inside
\code{
\link[covidReport:insp_cast_chr_date]{insp_cast_chr_date()}
}.
}
\seealso{
The calling function
\code{
  \link[covidReport:insp_cast_chr_date]{insp_cast_chr_date()}
  }, as well as higher-level functions
\code{\link[janitor:convert_to_date]{convert_to_date()}} and
\code{
  \link[covidReport:insp_prep_data]{insp_prep_data()}
  }
}
\keyword{internal}
