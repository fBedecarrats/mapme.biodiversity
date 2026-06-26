test_that(".calc_ghsl_population works", {
  x <- read_sf(
    system.file("extdata", "sierra_de_neiba_478140.gpkg",
      package = "mapme.biodiversity"
    )
  )
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  .copy_resource_dir(outdir)
  mapme_options(outdir = outdir, verbose = FALSE)
  get_resources(x, get_ghsl_pop(years = c(2000, 2010, 2020)))
  ghsl_pop <- prep_resources(x)[["ghsl_pop"]]

  cgp <- calc_ghsl_population()
  result <- cgp(x, ghsl_pop)
  cgp <- calc_ghsl_population(stats = c("mean", "median", "sd"))
  result_multi_stat <- cgp(x, ghsl_pop)
  cgp <- calc_ghsl_population(engine = "zonal")
  result_zonal <- cgp(x, ghsl_pop)
  cgp <- calc_ghsl_population(engine = "extract")
  result_extract <- cgp(x, ghsl_pop)
  cgp <- calc_ghsl_population(engine = "exactextract")
  result_exact <- cgp(x, ghsl_pop)

  expect_silent(.check_single_asset(result))
  expect_silent(.check_single_asset(result_multi_stat))
  expect_silent(.check_single_asset(result_zonal))
  expect_silent(.check_single_asset(result_extract))
  expect_silent(.check_single_asset(result_exact))

  vars <- c("population_mean", "population_median", "population_sd")
  expect_equal(unique(result_multi_stat$variable), vars)
  expect_equal(result_zonal$value, result_extract$value, tolerance = 1e-4)
  expect_snapshot(result_exact$value)
})
