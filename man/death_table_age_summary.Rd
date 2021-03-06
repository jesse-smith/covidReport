% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/death_table_age_summary.R
\name{death_table_age_summary}
\alias{death_table_age_summary}
\title{Tabulate Age Median and Range for COVID-19 Deaths}
\usage{
death_table_age_summary(
  data = pos(process_inv(read_inv(date = date))),
  date = NULL
)
}
\arguments{
\item{data}{Case data, as output by
\code{\link[coviData:process-nbs]{pos(process_inv())}}}

\item{date}{The download date of the data; defaults to most recent}
}
\value{
A \code{flextable}
}
\description{
Tabulate Age Median and Range for COVID-19 Deaths
}
