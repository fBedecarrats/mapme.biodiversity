#' Calculate population count statistics based on GHSL
#'
#' The Global Human Settlement Layer (GHSL) provides a set of global spatial
#' information, evidence-based analytics, and knowledge about the human
#' presence on the planet. This function allows to efficiently
#' calculate population count statistics (e.g. total number of population) for
#' polygons based on the GHS-POP dataset. For each polygon, the desired
#' statistic/s (min, max, sum, mean, median, sd or var) is/are returned.
#'
#' The required resources for this indicator are:
#'  - [ghsl]
#'
#' @name population_count_ghsl
#' @param engine The preferred processing functions from either one of "zonal",
#'   "extract" or "exactextract" as character.
#' @param stats Function to be applied to compute statistics for polygons
#'    either one or multiple inputs as character "min", "max", "sum", "mean", "median"
#'    "sd" or "var".
#' @keywords indicator
#' @returns A function that returns an indicator tibble with the specified
#'   populations statistics as variable and the corresponding values as value.
#' @include register.R
#' @export
calc_population_count_ghsl <- function(engine = "extract", stats = "sum") {
  engine <- check_engine(engine)
  stats <- check_stats(stats)

  function(x,
           ghsl = NULL,
           name = "population_count_ghsl",
           mode = "asset",
           aggregation = "stat",
           verbose = mapme_options()[["verbose"]]) {
    if (is.null(ghsl)) {
      return(NULL)
    }

    # GHSL-POP R2023A uses -200 as No Data value
    ghsl <- clamp(
      ghsl,
      lower = 0,
      upper = Inf,
      values = FALSE
    )

    results <- select_engine(
      x = x,
      raster = ghsl,
      stats = stats,
      engine = engine,
      name = "population",
      mode = "asset"
    )

    # Filename: GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0
    # Year is in the 3rd element prefixed with 'E'
    years <- unlist(lapply(names(ghsl), function(n) {
        parts <- strsplit(n, "_")[[1]]
        year_part <- parts[match("E", substr(parts, 1, 1))]
        if (length(year_part) == 0) year_part <- parts[3] # Fallback
        gsub("E", "", year_part)
    }))
    
    results[["datetime"]] <- as.POSIXct(paste0(years, "-01-01T00:00:00Z"))
    results[["unit"]] <- "count"
    results %>%
      tidyr::pivot_longer(-c(datetime, unit), names_to = "variable", values_to = "value") %>%
      dplyr::select(datetime, variable, unit, value)
  }
}

register_indicator(
  name = "population_count_ghsl",
  description = "Statistic of population counts based on GHSL",
  resources = "ghsl"
)
