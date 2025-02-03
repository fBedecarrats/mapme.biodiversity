#' Calculate Burned Area (JD channel) from FireCCI51
#'
#' This function sums the area (ha) of pixels with Julian Day > 0 (burned),
#' for each month (layer) in the firecci51_jd SpatRaster.
#' The required resource is "firecci51_jd".
#'
#' @name calc_burned_area_cci_jd
#' @param engine A character, one of "zonal", "extract", or "exactextract".
#' @keywords indicator
#' @returns A closure that returns a tibble with columns `datetime, variable, unit, value`
#' @include register.R
#' @export
calc_burned_area_cci_jd <- function(engine = "extract") {
  engine <- check_engine(engine)

  function(x,
           firecci51_jd = NULL,
           name = "burned_area_cci_jd",
           mode = "asset",
           aggregation = "sum",
           verbose = mapme_options()[["verbose"]]) {

    # If no overlap or resource not fetched, return NULL
    if (is.null(firecci51_jd)) {
      return(NULL)
    }

    # Remove negative JD
    firecci51_jd <- terra::clamp(firecci51_jd, lower = 0, upper = Inf, values = FALSE)

    # Convert to 1 or 0
    firecci51_jd <- terra::ifel(firecci51_jd > 0, 1, 0)

    # Convert to area (ha)
    firecci51_jd <- terra::cellSize(firecci51_jd, mask = TRUE, unit = "ha")

    # Parse monthly layer names -> a date
    layer_names  <- names(firecci51_jd)
    # e.g. "20010101-ESACCI-L3S_FIRE-BA-MODIS-AREA_1-fv5.1-JD"
    # first 8 chars: "20010101" => as.Date("2001-01-01")
    dates <- as.Date(substr(layer_names, 1, 8), format = "%Y%m%d")
    datetimes <- as.POSIXct(paste0(dates, "T00:00:00Z"), tz = "UTC")

    # Sum area with select_engine
    results <- select_engine(
      x      = x,
      raster = firecci51_jd,
      stats  = "sum",
      engine = engine,
      mode   = mode
    )

    # Tidy up into columns [datetime, variable, unit, value]
    if (mode == "portfolio") {
      # One data.frame per row in x
      purrr::map(results, function(df_per_poly) {
        df_per_poly %>%
          tidyr::pivot_longer(everything(), names_to = "layer", values_to = "value") %>%
          dplyr::mutate(
            datetime = datetimes[dplyr::row_number()],
            variable = name,
            unit     = "ha"
          ) %>%
          dplyr::select(datetime, variable, unit, value)
      })
    } else {
      # mode = "asset" => single asset (should be a single data frame)
      df_1 <- results[[1]]
      df_1 %>%
        tidyr::pivot_longer(everything(), names_to = "layer", values_to = "value") %>%
        dplyr::mutate(
          datetime = datetimes[dplyr::row_number()],
          variable = name,
          unit     = "ha"
        ) %>%
        dplyr::select(datetime, variable, unit, value)
    }
  }
}

register_indicator(
  name        = "burned_area_cci_jd",
  description = "Sums area of positive JD pixels in FireCCI51 for each month",
  resources   = "firecci51_jd"
)
