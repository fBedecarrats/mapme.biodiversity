#' Active Fire Polygon
#'
#' This resource is published by Fire Information for Resource Management System
#' (FIRMS) from NASA as Near Real Time (NRT) active fire data. The data is
#' collected from Moderate Resolution Imaging Spectroradiometer (MODIS) and the
#' Visible Infrared Imaging Radiometer Suite (VIIRS). The resource represents the
#' fire hotspot with lat/lon coordinates along with information on fire pixel brightness
#' temperature, and fire radiative power (frp). The data from MODIS is available from
#' 2000 to 2021 and that from VIIRS is only available for 2012-2021 year range.
#'
#' The data from the following instruments are available:
#' - "MODIS"
#' - "VIIRS"
#'
#' @name nasa_firms
#' @param years A numeric vector indicating the years for which to make the
#'   resource available.
#' @param instrument A character vector specifying the
#'   data collection instrument.
#' @keywords resource
#' @returns A function that returns a character of file paths.
#' @references NRT VIIRS 375 m Active Fire product VNP14IMGT distributed
#' from NASA FIRMS. Available on-line https://earthdata.nasa.gov/firms.
#' doi:10.5067/FIRMS/VIIRS/VNP14IMGT_NRT.002.
#' @source \url{https://firms.modaps.eosdis.nasa.gov/download/}
#' @include register.R
#' @export
get_nasa_firms <- function(years = 2012:2021,
                           instrument = "VIIRS") {
  if (!any(instrument %in% c("MODIS", "VIIRS"))) {
    stop(
      paste(
        "The selected instrument",
        instrument[which(!instrument %in% c("MODIS", "VIIRS"))],
        "is not available. Please choose between MODIS and VIIRS"
      )
    )
  }

  if (any(!years %in% 2000:2021)) {
    warning("NASA FIRMS is only available for years between 2000 and 2021.")
  }

  function(x,
           name = "nasa_firms",
           type = "vector",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]],
           testing = mapme_options()[["testing"]]) {
    org_target_years <- years

    filenames <- c()
    for (sensor in instrument) {
      if (sensor == "VIIRS") {
        available_years <- c(2012:2021)
        target_years <- check_available_years(
          org_target_years, available_years, "nasa_firms"
        )
      } else {
        available_years <- c(2000:2021)
        target_years <- check_available_years(
          org_target_years, available_years, "nasa_firms"
        )
      }

      urls <- unlist(sapply(
        target_years,
        function(year) .get_firms_url(year, sensor)
      ))
      filename <- file.path(
        outdir,
        basename(paste0(sensor, "_", target_years, ".zip"))
      )
      filenames <- c(filenames, filename)

      if (testing) {
        next()
      }

      # start download in a temporal directory within tmpdir
      download_or_skip(urls, filename)
      # unzip zip files
      sapply(filename, function(zip) .unzip_firms(zip, outdir, sensor))
      # remove unneeded files
      unlink(
        grep("*.gpkg$|*.zip$",
          list.files(outdir, full.names = T),
          value = T, invert = T
        ),
        recursive = T, force = T
      )
    }
    # return filenames if testing is activated
    if (testing) {
      return(basename(filenames))
    }
    # return paths to the gpkg for target years
    grep(paste(org_target_years, collapse = "|"),
      list.files(outdir, full.names = T, pattern = ".gpkg$"),
      value = TRUE
    )
  }
}


#' A helper function to construct correct FIRMS layer urls
#'
#' @param target_year A numeric indicating the target year
#' @param instrument A character vector specifying the
#'   data collection instrument.
#'
#' @return A character vector with the URL to the zip file of the respective year.
#' @keywords internal
#' @noRd
.get_firms_url <- function(target_year, instrument) {
  if (instrument == "VIIRS") {
    paste0("https://firms.modaps.eosdis.nasa.gov/data/country/zips/viirs-snpp_", target_year, "_all_countries.zip")
  } else {
    paste0("https://firms.modaps.eosdis.nasa.gov/data/country/zips/modis_", target_year, "_all_countries.zip")
  }
}


#' A helper function to unzip firms global zip files
#'
#' @param zip A character vector with potentially multiple zip files
#' @param dir The directory to where the files are unzipped
#' @param instrument A character vector specifying the
#'   data collection instrument.
#' @return Nothing, its called for the side effect of unzipping.
#' @keywords internal
#' @noRd
.unzip_firms <- function(zip, dir, instrument) {
  bn <- basename(zip)
  year <- gsub(".*?([0-9]+).*", "\\1", bn)

  gpkg <- file.path(dir, paste0("active_fire_", tolower(instrument), "_", year, ".gpkg"))

  if (file.exists(gpkg)) {
    return()
  }

  utils::unzip(
    zipfile = file.path(
      dir,
      basename(paste0(instrument, "_", year, ".zip"))
    ),
    exdir = dir
  )

  # bind all the CSVs and convert to gpkg
  .convert_csv_to_gpkg(
    gpkg = gpkg,
    dir = dir,
    instrument = instrument,
    year = year
  )
}


#' Helper function to convert CSVs to geopackage
#'
#' @param gpkg A character vector indicating the filename for the geopackage
#' @param dir A directory where intermediate files are written to.
#' @param instrument A character vector specifying the
#'   data collection instrument.
#' @param year An integer indicating the year of observation
#'
#' @importFrom utils read.csv
#' @importFrom purrr walk
#' @return Nothing, its called for the side effect to create a gpkg.
#' @keywords internal
#' @noRd
#'
.convert_csv_to_gpkg <- function(gpkg, dir, instrument, year) {
  if (instrument == "VIIRS") {
    loc <- file.path(dir, "viirs-snpp", year)
  } else {
    loc <- file.path(dir, "modis", year)
  }

  csv_files <- list.files(loc, pattern = "*.csv", full.names = TRUE)

  walk(csv_files, function(file) {
    data <- read.csv(file)
    data <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
    write_sf(data, dsn = gpkg, append = TRUE)
  })
}

register_resource(
  name = "nasa_firms",
  description = "NASA Fire Information for Resource Management System (FIRMS) - Global fire map data archive",
  licence = "https://www.earthdata.nasa.gov/learn/find-data/near-real-time/citation",
  source = "https://firms.modaps.eosdis.nasa.gov",
  type = "vector"
)
