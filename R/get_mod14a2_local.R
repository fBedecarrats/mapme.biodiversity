#' Local MODIS Thermal Anomalies & Fire 8-Day (MOD14A2 and MYD14A2)
#'
#' Version locale de la ressource MOD14A2+MYD14A2 en utilisant des fichiers GeoTIFF déjà disponibles localement.
#'
#' @param folder Dossier contenant les fichiers MOD14A2 au format `.tif`.
#' @returns Une fonction retournant les footprints nécessaires à mapme.
#' @export
get_mod14a2_local <- function(folder) {
  function(x,
           name = "mod14a2",
           type = "raster",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {

    check_namespace("terra")
    tif_files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
    if (length(tif_files) == 0) stop("Aucun fichier .tif trouvé dans ", folder)

    # Lire les emprises
    bboxes <- lapply(tif_files, function(f) {
      r <- terra::rast(f)
      st_as_sf(st_as_sfc(st_bbox(r)))
    })

    fps <- sf::st_as_sf(do.call(rbind, bboxes))
    fps[["source"]] <- tif_files
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
  name = "mod14a2_local",
  description = "MODIS 8-Day Thermal Anomalies and Fire Product (Terra + Aqua, fichiers locaux)",
  licence = "https://lpdaac.usgs.gov/data/data-citation-and-policies/",
  source = "Fichiers locaux MOD14A2/MYD14A2",
  type = "raster"
)
