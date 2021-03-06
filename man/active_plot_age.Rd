% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/active_age.R
\name{active_plot_age}
\alias{active_plot_age}
\title{Plot Active Case Rates by Age}
\usage{
active_plot_age(data = pos(process_inv(read_inv(date))), date = NULL)
}
\arguments{
\item{data}{NBS case data, as returned by
\code{\link[coviData:read-nbs]{pos(process_inv())}}}

\item{date}{The download date of the data; defaults to most recent}
}
\value{
A \code{ggplot}
}
\description{
Plot Active Case Rates by Age
}
