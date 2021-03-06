test_that("`rpt_demog_pptx()` works", {
  skip_on_ci()
  skip_reports()
  tmp_dir <- fs::dir_create(fs::file_temp())
  on.exit(fs::dir_delete(tmp_dir), add = TRUE)

  pptx <- suppressMessages(rpt_demog_pptx(date = Sys.Date()-1L, dir = tmp_dir))

  pptx_path_attr <- attr(pptx, "path")
  expect_s3_class(pptx, "rpptx")
  expect_true(fs::file_exists(pptx_path_attr))
})
