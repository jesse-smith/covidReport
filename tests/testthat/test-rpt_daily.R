test_that("`rpt_daily_pptx()` works", {
  skip_on_ci()
  skip_reports()
  tmp_dir <- fs::dir_create(fs::file_temp())
  on.exit(fs::dir_delete(tmp_dir), add = TRUE)

  pptx <- suppressMessages(rpt_daily_pptx(date = Sys.Date()-1L, dir = tmp_dir))

  pptx_path_attr <- attr(pptx, "path")
  expect_s3_class(pptx, "rpptx")
  expect_true(fs::file_exists(pptx_path_attr))
})

test_that("`rpt_daily_mail()` works", {
  skip_on_ci()
  skip_reports()
  rpt_daily_mail(to = "Jesse.Smith@shelbycountytn.gov")
  rpt_sent <- TRUE
  expect_true(rpt_sent)
})
