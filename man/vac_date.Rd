% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R
\name{vac_date}
\alias{vac_date}
\title{Get date of a TennIIS vaccination file}
\usage{
vac_date(date = NULL)
}
\arguments{
\item{date}{A date (as a \code{Date} or \code{"\%Y-\%m-\%d"} string)}
}
\value{
A \code{Date} (or errors if data does not exist)
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
}
\details{
This function is deprecated. Please use the
\code{\link[coviData:date-data]{date_vac()}} function, which is now the
backend for this function.
}
\keyword{internal}
