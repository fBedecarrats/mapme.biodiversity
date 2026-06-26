test_that(".get_ghsl works", {
  skip_on_cran()
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  # Normally we would copy, but we don't have ghsl in inst/res yet
  # .copy_resource_dir(outdir)
  dir.create(file.path(outdir, "ghsl"), showWarnings = FALSE, recursive = TRUE)
  
  mapme_options(outdir = outdir, verbose = FALSE)

  expect_error(get_ghsl(years = 1974))
  gghsl <- get_ghsl(years = 2020)
  expect_silent(.check_resource_fun(gghsl))
  
  # Mock the file existing to avoid download in tests
  tif_name <- "GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0.tif"
  file.create(file.path(outdir, "ghsl", tif_name))
  
  fps <- gghsl(outdir = outdir)
  expect_silent(.check_footprints(fps))
  expect_equal(fps$filename, tif_name)
})
