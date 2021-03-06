% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/active_ethnicity.R
\name{active_calc_ethnicity}
\alias{active_calc_ethnicity}
\title{Calculate Active Case Rates and Percentages by Ethnicity}
\usage{
active_calc_ethnicity(data = pos(process_inv(read_inv(date))), date = NULL)
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
Calculate Active Case Rates and Percentages by Ethnicity
}
\keyword{internal}
