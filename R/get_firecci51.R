#' FireCCI51 Burned Area Product
#'
#' The FireCCI51 product provides global burned area maps at 250m resolution from 2001 onwards.
#' The data is produced by the European Space Agency (ESA) and is available on the CEDA platform.
#'
#' @name firecci51
#' @param years Numeric vector of years to make the FireCCI51 product available for. Must be greater than the year 2001.
#' @param months Numeric vector of months to make the FireCCI51 product available for. Defaults to all 12 months.
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @references Alonso-Canas, I. and Chuvieco, E. (2015). Global Burned Area Mapping from MODIS (2001-2016). \doi{10.1016/j.rse.2015.08.009}
#' @references Resource Guide: \url{https://climate.esa.int/media/documents/Fire_cci_D4.2.1_PUG-MODIS_v1.1.pdf}
#' @source \url{https://data.ceda.ac.uk/neodc/esacci/fire/data/burned_area/MODIS/pixel/v5.1/}
#' @include register.R
#' @export

get_firecci51 <- function(years = 2001:2020, months = 1:12) {
  now <- as.numeric(format(Sys.Date(), "%Y"))
  years <- check_available_years(years, 2001:now, "firecci51")

  # Define bounding boxes for each AREA_*
  area_bboxes <- list(
    AREA_1 = st_as_sf(st_as_sfc(st_bbox(c(xmin = -180, ymin = 19, xmax = -50, ymax = 83), crs = st_crs("EPSG:4326")))),
    AREA_2 = st_as_sf(st_as_sfc(st_bbox(c(xmin = -105, ymin = -57, xmax = -34, ymax = 19), crs = st_crs("EPSG:4326")))),
    AREA_3 = st_as_sf(st_as_sfc(st_bbox(c(xmin = -26, ymin = 25, xmax = 53, ymax = 83), crs = st_crs("EPSG:4326")))),
    AREA_4 = st_as_sf(st_as_sfc(st_bbox(c(xmin = 53, ymin = 0, xmax = 180, ymax = 83), crs = st_crs("EPSG:4326")))),
    AREA_5 = st_as_sf(st_as_sfc(st_bbox(c(xmin = -26, ymin = -40, xmax = 53, ymax = 25), crs = st_crs("EPSG:4326")))),
    AREA_6 = st_as_sf(st_as_sfc(st_bbox(c(xmin = 95, ymin = -53, xmax = 180, ymax = 0), crs = st_crs("EPSG:4326"))))
  )

  function(x,
           name = "firecci51",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {

    # Check which AREA_* files intersect with the AOI
    intersecting_areas <- purrr::keep(names(area_bboxes), function(area) {
      st_intersects(x, area_bboxes[[area]], sparse = FALSE)
    })

    if (length(intersecting_areas) == 0) {
      stop("The extent of the portfolio does not intersect with any FireCCI51 areas.", call. = FALSE)
    }

    base_url <- "https://dap.ceda.ac.uk/neodc/esacci/fire/data/burned_area/MODIS/pixel/v5.1/compressed"
    urls <- purrr::map(years, function(year) {
      purrr::map(months, function(month) {
        purrr::map(intersecting_areas, function(area) {
          sprintf("%s/%d/%d%02d01-ESACCI-L3S_FIRE-BA-MODIS-%s-fv5.1.tar.gz", base_url, year, year, month, area)
        })
      })
    }) %>% unlist()

    bboxs <- purrr::map(intersecting_areas, function(area) {
      st_bbox(area_bboxes[[area]])
    })

    # Expand bounding boxes to match the number of URLs
    expanded_bboxs <- rep(bboxs, each = length(years) * length(months))

    # Create initial fps object
    fps <- purrr::map_dfr(expanded_bboxs, function(x) {
      names(x) <- c("xmin", "ymin", "xmax", "ymax")
      bbox <- st_bbox(x, crs = "EPSG:4326")
      st_as_sf(st_as_sfc(bbox))
    })

    # Create a new data frame to store all fps entries
    all_fps <- list()

    # Add the source field with paths to the TIFF files within the TAR archives
    for (i in seq_along(urls)) {
      url <- urls[i]
      bbox <- fps[i, ]
      tiff_files <- c("-JD.tif", "-CL.tif", "-LC.tif")
      for (tiff_ext in tiff_files) {
        tiff_file <- sub("\\.tar\\.gz$", tiff_ext, basename(url))
        tar_path <- paste0("/vsitar//vsicurl/", url, "/", tiff_file)
        bbox_copy <- bbox
        bbox_copy[["source"]] <- tar_path
        all_fps <- append(all_fps, list(bbox_copy))
      }
    }

    # Combine all fps entries into a single data frame
    fps <- bind_rows(all_fps)
    fps <- make_footprints(
      fps,
      what = "raster",
      co = c("-co", "COMPRESS=DEFLATE")
    )
  # Subset rows based on the filename pattern
  jd_fps <- fps[grepl("-JD\\.tif$", fps$filename), ]
  cl_fps <- fps[grepl("-CL\\.tif$", fps$filename), ]
  lc_fps <- fps[grepl("-LC\\.tif$", fps$filename), ]

  # Register subsets independently
  .add_resource(list("firecci51_jd" = jd_fps))
  .add_resource(list("firecci51_cl" = cl_fps))
  .add_resource(list("firecci51_lc" = lc_fps))

  # Also register resources together
  return(fps)

  }
}


# Register the resources
register_resource(
  name = "firecci51",
  description = "ESA FireCCI51 Burned Area Product",
  licence = "CC-BY-4.0",
  source = "https://data.ceda.ac.uk/neodc/esacci/fire/data/burned_area/MODIS/pixel/v5.1/",
  type = "raster"
)

register_resource(
  name = "firecci51_jd",
  description = "ESA FireCCI51 Julian Day of first burned area detection",
  licence = "CC-BY-4.0",
  source = "https://data.ceda.ac.uk/neodc/esacci/fire/data/burned_area/MODIS/pixel/v5.1/",
  type = "raster"
)

register_resource(
  name = "firecci51_cl",
  description = "ESA FireCCI51 Confidence Level of burned area detection",
  licence = "CC-BY-4.0",
  source = "https://data.ceda.ac.uk/neodc/esacci/fire/data/burned_area/MODIS/pixel/v5.1/",
  type = "raster"
)

register_resource(
  name = "firecci51_lc",
  description = "ESA FireCCI51 Land Cover of detected burned area",
  licence = "CC-BY-4.0",
  source = "https://data.ceda.ac.uk/neodc/esacci/fire/data/burned_area/MODIS/pixel/v5.1/",
  type = "raster"
)
