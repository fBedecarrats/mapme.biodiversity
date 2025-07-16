#' MODIS Thermal Anomalies & Fire 8-Day (MOD14A2 and MYD14A2)
#'
#' The MOD14A2 and MYD14A2 Version 6.1 products provide an 8-day composite of global thermal
#' anomalies and fire pixels derived from the MODIS sensors aboard the Terra (MOD14A2) and Aqua (MYD14A2) satellites.
#' The Planetary Computer exposes both datasets in a single STAC collection (`modis-14A2-061`), and this function retrieves both.
#'
#' The FireMask layer includes values indicating fire detection confidence,
#' no fire, cloud, water, and other classes, with fire classes starting at value 7.
#'
#' @name mod14a2
#' @param years Numeric vector of years (>=2000) for which the product should be fetched.
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @references Giglio, L., et al. MODIS/Terra+Aqua Thermal Anomalies/Fire 8-Day L3 Global 1km SIN Grid V061. 2021.
#'   \doi{https://doi.org/10.5067/MODIS/MOD14A2.061}
#' @source \url{https://planetarycomputer.microsoft.com/dataset/modis-14A2-061}
#' @include register.R
#' @export
get_mod14a2 <- function(years = 2000:2025) {
  check_namespace("rstac")
  now <- as.numeric(format(Sys.Date(), "%Y"))
  years <- check_available_years(years, 2000:now, "mod14a2")

  function(x,
           name = "mod14a2",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {

    dt <- paste0(min(years), "-01-01/", max(years), "-12-31")

    items <- try(
      rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1/") %>%
        rstac::stac_search(
          collection = "modis-14A2-061",
          bbox = as.numeric(sf::st_bbox(x)),
          datetime = dt,
          limit = NULL
        ) %>%
        rstac::post_request() %>%
        rstac::items_fetch()
    )

    if (inherits(items, "try-error")) {
      stop("Download for MOD14A2 resource was unsuccessful")
    }

    urls <- rstac::assets_url(items, asset_names = "FireMask")
    if (length(urls) == 0) {
      stop("The extent of the portfolio does not intersect with the MOD14A2 tiles.")
    }

    bboxs <- rstac::items_bbox(items)
    fps <- purrr::map(bboxs, function(b) {
      names(b) <- c("xmin", "ymin", "xmax", "ymax")
      bbox <- sf::st_bbox(b, crs = "EPSG:4326")
      sf::st_as_sf(sf::st_as_sfc(bbox))
    })
    fps <- sf::st_as_sf(purrr::list_rbind(fps))
    fps[["source"]] <- paste0("/vsicurl?pc_url_signing=yes&url=", urls)
    fps <- sf::st_transform(fps, "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")

    make_footprints(
      fps,
      what = "raster",
      co = c("-co", "COMPRESS=DEFLATE", "-of", "COG"),
      precision = 1e3
    )
  }
}

register_resource(
  name = "mod14a2",
  description = "MODIS 8-Day Thermal Anomalies and Fire Product (Terra + Aqua)",
  licence = "https://lpdaac.usgs.gov/data/data-citation-and-policies/",
  source = "https://planetarycomputer.microsoft.com/dataset/modis-14A2-061",
  type = "raster"
)
