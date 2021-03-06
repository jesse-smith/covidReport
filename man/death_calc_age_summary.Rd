% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/death_table_age_summary.R
\name{death_calc_age_summary}
\alias{death_calc_age_summary}
\title{Calculate Age Minimum, Median, and Maximum for COVID-19 Deaths}
\usage{
death_calc_age_summary(
  data = pos(process_inv(read_inv(date = date))),
  date = NULL
)
}
\arguments{
\item{data}{NBS case data, as returned by
\code{\link[coviData:read-nbs]{pos(process_inv())}}}

\item{date}{The download date of the data; defaults to most recent}
}
\value{
A \code{tibble}
}
\description{
Calculate Age Minimum, Median, and Maximum for COVID-19 Deaths
}
\keyword{internal}
