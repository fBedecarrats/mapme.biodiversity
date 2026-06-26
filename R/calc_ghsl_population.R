#' Calculate GHSL population count statistics
#'
#' The Global Human Settlement Layer (GHSL) provides population grids derived
#' from CIESIN GPWv4.11, disaggregated using built-up surface and volume
#' information. This function allows to efficiently calculate population count
#' statistics (e.g. total number of population) for polygons. For each polygon,
#' the desired statistic/s (min, max, sum, mean, median, sd or var) is/are
#' returned.
#'
#' The required resources for this indicator are:
#'  - [ghsl_pop]
#'
#' @name ghsl_population
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param stats Function to be applied to compute statistics for polygons
#'    either one or multiple inputs as character "min", "max", "sum", "mean",
#'    "median" "sd" or "var".
#' @keywords indicator
#' @returns A function that returns an indicator tibble with the specified
#'   population statistics as variable and the corresponding values as value.
#' @include register.R
#' @export
#' @examples
#' \dontshow{
#' mapme.biodiversity:::.copy_resource_dir(file.path(tempdir(), "mapme-data"))
#' }
#' \dontrun{
#' library(sf)
#' library(mapme.biodiversity)
#'
#' outdir <- file.path(tempdir(), "mapme-data")
#' dir.create(outdir, showWarnings = FALSE)
#'
#' mapme_options(
#'   outdir = outdir,
#'   verbose = FALSE
#' )
#'
#' aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
#'   package = "mapme.biodiversity"
#' ) %>%
#'   read_sf() %>%
#'   get_resources(get_ghsl_pop(years = c(2000, 2010, 2020))) %>%
#'   calc_indicators(
#'     calc_ghsl_population(engine = "extract", stats = c("sum", "median"))
#'   ) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_ghsl_population <- function(engine = "extract", stats = "sum") {
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(x,
           ghsl_pop = NULL,
           name = "ghsl_population",
           mode = "asset",
           aggregation = "stat",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(ghsl_pop)) {
      return(NULL)
    }

    results <- select_engine(
      x = x,
      raster = ghsl_pop,
      stats = stats,
      engine = engine,
      name = "population",
      mode = "asset"
    )

    # Extract epoch year from GHSL layer names
    # Layer names follow pattern: GHS_POP_E{YEAR}_GLOBE_R2023A_...
    years <- gsub(".*_E(\\d{4})_.*", "\\1", names(ghsl_pop))
    results[["datetime"]] <- as.POSIXct(paste0(years, "-01-01T00:00:00Z"))
    results[["unit"]] <- "count"
    results %>%
      tidyr::pivot_longer(-c(datetime, unit), names_to = "variable", values_to = "value") %>%
      dplyr::select(datetime, variable, unit, value)
  }
}

register_indicator(
  name = "ghsl_population",
  description = "Statistic of population counts based on GHSL",
  resources = "ghsl_pop"
)
