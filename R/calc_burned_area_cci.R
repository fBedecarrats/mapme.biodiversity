#' Calculate Annual Burned Area based on FireCCI51
#'
#' Calculates Annual Burned Area based on the FireCCI51 product.
#'
#' The required resources for this indicator are:
#'  - [firecci51]
#'
#' @name burned_area_cci
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @keywords indicator
#' @returns A function that returns an indicator tibble with variable burned
#'   area and corresponding area (in ha) as values.
#' @references Chuvieco, E., et al. ESA CCI ECV Fire Disturbance: DLR-CCI-FIRE-MODIS-BA-AREA_2-v5.1
#'   \doi{https://doi.org/10.5285/528ca4a4-9b42-4b9a-995b-1c9f6a6c2bce}
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
#'   get_resources(get_firecci51(years = 2021)) %>%
#'   calc_indicators(calc_burned_area_cci(engine = "extract")) %>%
#'   portfolio_long()
#'
#' aoi
#' }
calc_burned_area_cci <- function(engine = "extract") {
  engine <- check_engine(engine)

  function(x,
           firecci51 = NULL,
           name = "burned_area_cci",
           mode = "asset",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(firecci51)) {
      return(NULL)
    }

    # Transform AOI to match CRS of firecci51 data
    x_proj <- st_transform(x, st_crs(firecci51))

    # Mask and process firecci51 data with AOI
    firecci51 <- terra::mask(firecci51, x_proj)
    firecci51 <- terra::subst(firecci51, from = c(-2, -1, 0, NA), to = 0, others = 1)
    arearaster <- cellSize(firecci51, mask = FALSE, unit = "ha")
    firecci51 <- firecci51 * arearaster

    # Calculate statistics
    stats <- select_engine(
      x = x_proj,
      raster = firecci51,
      stats = "sum",
      engine = engine,
      name = "burned_area_cci",
      mode = "asset"
    )

    # Process and format output
    names(stats) <- "value"
    dates <- gsub("^.*?\\.A([0-9]+)\\..*$", "\\1", names(firecci51))
    dates <- as.POSIXct(paste0(as.Date(dates, "%Y%j"), "T00:00:00Z"))
    stats[["datetime"]] <- dates
    stats[["unit"]] <- "ha"
    stats[["variable"]] <- "burned_area_cci"
    stats[, c("datetime", "variable", "unit", "value")]
  }
}

register_indicator(
  name = "burned_area_cci",
  description = "Annual burned area detected by FireCCI51",
  resources = "firecci51"
)
