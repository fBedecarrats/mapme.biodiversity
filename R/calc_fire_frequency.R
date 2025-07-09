#' Calculate number of fire events per year
#'
#' Counts distinct fire events (separated in time) detected by MODIS MOD14A2/MYD14A2.
#'
#' @name fire_frequency
#' @param engine The extraction engine, one of "extract", "zonal", or "exactextract".
#' @keywords indicator
#' @returns A function that returns the number of distinct fire events per year and polygon.
#' @export
calc_fire_frequency <- function(engine = "extract") {
  engine <- check_engine(engine)

  function(x, mod14a2 = NULL, name = "fire_frequency", mode = "asset",
           aggregation = "sum", verbose = mapme_options()[["verbose"]]) {

    if (is.null(mod14a2)) return(NULL)

    # Project AOI to match raster
    x_proj <- st_transform(x, st_crs(mod14a2))
    mod14a2 <- terra::mask(mod14a2, x_proj)

    # Binary fire detection: MODIS values 7, 8, 9 = low/nominal/high confidence
    fire_mask <- mod14a2 %in% c(7, 8, 9)

    # Count distinct fire events per pixel using a transition-aware function
    count_fire_events <- function(x) {
      if (all(is.na(x))) return(NA_integer_)
      x <- as.integer(x == 1)
      rle_fire <- rle(x)
      sum(rle_fire$values == 1)
    }

    fire_event_raster <- terra::app(fire_mask, fun = count_fire_events)

    # Extract per polygon
    stats <- select_engine(
      x = x_proj,
      raster = fire_event_raster,
      stats = aggregation,
      engine = engine,
      name = "fire_frequency",
      mode = "asset"
    )

    names(stats) <- "value"

    # Derive year from raster names
    dates <- gsub("^.*A([0-9]{4}).*$", "\\1", names(mod14a2))
    years <- unique(as.integer(dates))
    stats[["datetime"]] <- as.POSIXct(paste0(years, "-01-01T00:00:00Z"))
    stats[["unit"]] <- "events"
    stats[["variable"]] <- "fire_frequency"

    stats[, c("datetime", "variable", "unit", "value")]
  }
}
