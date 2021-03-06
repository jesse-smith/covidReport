% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inspections_table.R
\name{insp_table_pipeline}
\alias{insp_table_pipeline}
\title{Run the Full Inspections Table Pipeline}
\usage{
insp_table_pipeline(
  data_path = coviData::path_create("V:/Compliance/Inspection Data for Publishing/",
    "Grand List of Inspections", ext = "xlsx"),
 
    table_path = coviData::path_create("V:/Compliance/Inspection Data for Publishing/Table/",
    paste0("insp_table_", Sys.Date()), ext = "html"),
  archive_dir = "V:/Compliance/Inspection Data for Publishing/Table/Archive/",
  force = FALSE
)
}
\arguments{
\item{data_path}{The path to the excel workbook containing inspections data}

\item{table_path}{The location to save the inspections table}

\item{archive_dir}{The path to the archive directory for inspections tables}

\item{force}{Should existing files at \code{table_path} or \code{archive_dir} be
overwritten if a conflict occurs?}
}
\value{
The inspections table as a \code{datatables} object (invisibly)
}
\description{
This is a simplified wrapper for the inspections table pipeline that makes
it easier to run. This takes inspections data from load to finished product
and cleans up after itself. See the component functions for details on the
pipeline.
}
\seealso{
The component functions of the inspections table pipeline:
\code{
  \link[covidReport:insp_load_data]{insp_load_data()}
  },
\code{
  \link[covidReport:insp_prep_data]{insp_prep_data()}
  },
\code{
  \link[covidReport:insp_create_table]{insp_create_table()}
  },
\code{
  \link[covidReport:insp_save_table]{insp_save_table()}
  },
\code{
  \link[covidReport:insp_archive_table]{insp_archive_table()}
  }
}
