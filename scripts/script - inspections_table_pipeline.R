run_script <- function() {

  # Print time to log
  message("\n", Sys.time())

  in_file <- "in 'inspections_table_pipeline.R'"

  # Set handling options
  tryCatch(
    options(rlang_backtrace_on_error = "branch", error = rlang::entrace),
    error = coviData::error_notify(paste0("`options(...)`", in_file))
  )
  tryCatch(
    covidReport::inspections_table_pipeline(),
    error = coviData::error_notify(
      paste0("inspections_table_pipeline()", in_file)
    )
  )
  to <- c("austen.onek@shelbycountytn.gov", "jesse.smith@shelbycountytn.gov")

  # if (weekdays(Sys.Date()) %in% c("Monday", "Friday")) {
  #   cc <- c(
  #     "jennifer.sharp@shelbycountytn.gov",
  #     "chip.washington@shelbycountytn.gov",
  #     "joan.e.carr@shelbycountytn.gov",
  #     "david.sweat@shelbycountytn.gov",
  #     "jennifer.kmet@shelbycountytn.gov",
  #     "chaitra.subramanya@shelbycountytn.gov"
  #   )
  # } else {
  #   cc <- NULL
  # }
  cc <- NULL

  br <- "<br>"

  subject <- paste0("COVID-19 Business Inspections Table: ", Sys.Date())

  table_dir <- paste0(
    "<a href='file://V:/Compliance/Inspection Data for Publishing/Table'>",
    "V:/Compliance/Inspection Data for Publishing/Table</a>"
  )

  table_file_here <- paste0(
    "<a href='file://V:/Compliance/Inspection Data for Publishing/Table/",
    "inspections_table_", Sys.Date(), ".html'>here</a>"
  )

  body <- paste0(
    "The <b>COVID-19 Business Inspections</b> table has been updated (on <i>",
    Sys.Date(), "</i> at <i>", format(Sys.time(), '%H:%M %p'), "</i>. ",
    "Please find the most recent version at the location below:",
    "<br><br>",
    table_dir,
    "<br><br>",
    "If you are on a Shelby County computer with access to the 'V:' drive, ",
    "you can view the table in your browser by clicking ", table_file_here, ".",
    "<br><br>",
    "Thanks!",
    "<br><br>",
    "Jesse Smith",
    "<br><br>",
    "<i>Note: This email was generated automatically. ",
    "If I am not in the office, it may take time for me to see any responses.",
    "</i>"
  )

  coviData::notify(
    to = c("austen.onek@shelbycountytn.gov", "jesse.smith@shelbycountytn.gov"),
    cc = cc,
    subject = subject,
    body = body,
    html = TRUE
  )

}

# Run the script with stderr output piped to log file
capture.output(
  run_script(),
  file = paste0(
    "C:/Users/Jesse.Smith/Documents/jobs/log/inspections_table_pipeline.log"
  ),
  append = TRUE,
  type = "message"
)
