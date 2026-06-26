#' Population Count layer (GHSL)
#'
#' The Global Human Settlement Layer (GHSL) provides a set of global spatial
#' information, evidence-based analytics, and knowledge about the human
#' presence on the planet. This resource represents the population count,
#' 30 arcsec (~1 km) spatial resolution layers available to download for
#' the years 1975 to 2030 in 5-year intervals.
#' The dataset used is the GHS-POP R2023A version in WGS84 (EPSG:4326).
#'
#' @name ghsl
#' @param years A numeric vector indicating the years for which to make the
#'   resource available. Supported years are: 1975, 1980, 1985, 1990, 1995,
#'   2000, 2005, 2010, 2015, 2020, 2025, 2030.
#' @docType data
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @source \url{https://ghsl.jrc.ec.europa.eu/ghs_pop2023.php}
#' @include register.R
#' @export
get_ghsl <- function(years = 2020) {
  available_years <- c(1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025, 2030)
  years <- check_available_years(years, available_years, "ghsl")

  function(x,
           name = "ghsl",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    srcs <- unlist(sapply(years, function(year) .get_ghsl_url(year)))
    tifs <- unlist(sapply(years, function(year) .get_ghsl_tif_name(year)))
    
    if (is.null(outdir)) {
        # If no outdir, we use the remote URLs directly via vsizip/vsicurl
        # This is consistent with how accessibility_2000 works
        urls <- paste0("/vsizip//vsicurl/", srcs, "/", tifs)
        bbox <- c(xmin = -180.0, ymin = -90.0, xmax = 180.0, ymax = 90.0)
        tile <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(bbox, crs = "EPSG:4326")))
        fps <- do.call(rbind, lapply(urls, function(url) {
            tile[["source"]] <- url
            tile
        }))
        return(make_footprints(fps, filenames = tifs, what = "raster",
                               co = c("-co", "COMPRESS=LZW", "-co", "TILED=YES")))
    }
    
    dsts <- file.path(outdir, tifs)
    is_available <- purrr::map_lgl(dsts, spds_exists, what = "raster")
    
    if (all(is_available)) {
      return(make_footprints(dsts, what = "raster"))
    }

    purrr::walk2(srcs, dsts, function(src, dst) {
      if (!spds_exists(dst, what = "raster")) {
        if (verbose) message("Downloading GHSL-POP for year ", years[which(srcs == src)])
        
        # We can use gdal_utils translate directly with vsizip/vsicurl to avoid manual download/unzip
        vsi_path <- paste0("/vsizip//vsicurl/", src, "/", basename(dst))
        
        sf::gdal_utils(
          util = "translate",
          source = vsi_path,
          destination = dst,
          options = c(
            "-co", "COMPRESS=LZW",
            "-co", "PREDICTOR=2",
            "-co", "TILED=YES",
            "-ot", "Float32"
          )
        )
      }
    })

    make_footprints(dsts, what = "raster")
  }
}

#' Helper function to construct GHSL download urls
#'
#' @param target_year A numeric indicating the target year
#' @return A character vector.
#' @keywords internal
#' @noRd
.get_ghsl_url <- function(target_year) {
  sprintf(
    "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E%d_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E%d_GLOBE_R2023A_4326_30ss_V1_0.zip",
    target_year, target_year
  )
}

#' Helper function to construct GHSL tif names
#'
#' @param target_year A numeric indicating the target year
#' @return A character vector.
#' @keywords internal
#' @noRd
.get_ghsl_tif_name <- function(target_year) {
  sprintf("GHS_POP_E%d_GLOBE_R2023A_4326_30ss_V1_0.tif", target_year)
}

register_resource(
  name = "ghsl",
  description = "Global Human Settlement Layer (GHSL) Population Count (R2023A)",
  licence = "CC-BY 4.0",
  source = "https://ghsl.jrc.ec.europa.eu/ghs_pop2023.php",
  type = "raster"
)

register_resource(
  name = "ghsl",
  description = "Global Human Settlement Layer (GHSL) Population Count R2023A",
  licence = "CC-BY 4.0",
  source = "https://ghsl.jrc.ec.europa.eu/ghs_pop2023.php",
  type = "raster"
)
