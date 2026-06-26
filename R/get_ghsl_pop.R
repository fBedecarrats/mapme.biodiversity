#' GHS-POP Population Grid for 1975-2030
#'
#' This resource is published by the European Commission Joint Research Centre
#' (JRC) as part of the Global Human Settlement Layer (GHSL) project. It
#' represents the distribution of resident population, expressed as the number
#' of people per cell, at 30 arc-second (~1 km) spatial resolution in WGS84
#' (EPSG:4326). Population estimates are derived from CIESIN GPWv4.11 and
#' disaggregated using the built-up surface and volume information from the
#' GHSL. Data are available at 5-year intervals from 1975 to 2030.
#'
#' It may be required to increase the timeout option to successfully download
#' these layers from their source location via e.g.
#' `options(timeout = 600)`.
#'
#' @name ghsl_pop
#' @param years A numeric vector indicating the years for which to make the
#'   resource available. Must be multiples of 5 between 1975 and 2030.
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @references Pesaresi, M. et al. (2024). Advances on the Global Human
#'   Settlement Layer by joint assessment of Earth Observation and population
#'   survey data. International Journal of Digital Earth, 17(1).
#'   \doi{doi:10.1080/17538947.2024.2390454}
#' @source \url{https://human-settlement.emergency.copernicus.eu/ghs_pop2023.php}
#' @include register.R
#' @export
get_ghsl_pop <- function(years = 2020) {
  avail_years <- seq(1975, 2030, 5)
  years <- check_available_years(years, avail_years, "ghsl_pop")

  if (is.null(mapme_options()[["outdir"]])) {
    warning(paste(
      "GHSL population layers must be downloaded from the source location",
      "irrespective if `outdir` was specified or not."
    ))
  }

  function(x,
           name = "ghsl_pop",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    srcs <- unlist(sapply(years, function(year) .get_ghsl_pop_url(year)))
    has_outdir <- !is.null(outdir)

    if (has_outdir) {
      dsts <- file.path(outdir, .get_ghsl_pop_filename(years))
    } else {
      tmpdir <- tempfile()
      dir.create(tmpdir)
      dsts <- file.path(tmpdir, .get_ghsl_pop_filename(years))
    }

    is_available <- purrr::map_lgl(dsts, spds_exists, what = "raster")
    if (all(is_available)) {
      return(make_footprints(dsts, what = "raster"))
    }

    purrr::walk2(srcs, dsts, function(src, dst) {
      if (!spds_exists(dst, what = "raster")) {
        tmp_zip <- tempfile(fileext = ".zip")
        download.file(src, tmp_zip, mode = ifelse(
          Sys.info()["sysname"] == "Windows", "wb", "w"
        ))

        tmp_dir <- tempfile()
        dir.create(tmp_dir)
        utils::unzip(tmp_zip, exdir = tmp_dir)
        tif_file <- list.files(
          tmp_dir,
          pattern = "\\.tif$",
          full.names = TRUE,
          recursive = TRUE
        )[1]

        sf::gdal_utils(
          util = "translate",
          source = tif_file,
          destination = dst,
          options = c(
            "-co", "COMPRESS=DEFLATE",
            "-ot", "Float32"
          )
        )

        file.remove(tmp_zip)
        unlink(tmp_dir, recursive = TRUE)
      }
    })

    make_footprints(dsts, what = "raster")
  }
}

#' Helper function to construct GHSL population layer URLs
#'
#' @param target_year A numeric indicating the target year
#'
#' @return A character vector.
#' @keywords internal
#' @noRd
.get_ghsl_pop_url <- function(target_year) {
  layer_name <- sprintf("GHS_POP_E%d_GLOBE_R2023A_4326_30ss", target_year)
  filename <- sprintf("%s_V1_0", layer_name)
  paste0(
    "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/",
    "GHS_POP_GLOBE_R2023A/",
    layer_name, "/V1-0/",
    filename, ".zip"
  )
}

#' Helper function to construct GHSL population layer filenames
#'
#' @param years A numeric vector of target years
#'
#' @return A character vector of filenames.
#' @keywords internal
#' @noRd
.get_ghsl_pop_filename <- function(years) {
  sprintf("GHS_POP_E%d_GLOBE_R2023A_4326_30ss_V1_0.tif", years)
}


register_resource(
  name = "ghsl_pop",
  description = "GHS-POP R2023A - GHSL Population Grid 1975-2030",
  licence = "https://commission.europa.eu/legal-notice_en",
  source = "https://human-settlement.emergency.copernicus.eu/ghs_pop2023.php",
  type = "raster"
)
