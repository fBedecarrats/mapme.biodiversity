devtools::load_all()
library(sf)
library(dplyr)
library(tidyr)

aoi_url <- "https://raw.githubusercontent.com/BETSAKA/Tools/main/data/AP_Vahatra.geojson"
aoi <- st_read(aoi_url, quiet = TRUE) %>%
  filter(nom == "Ankarafantsika")


outdir <- "mapme-resources"
dir.create(outdir, showWarnings = TRUE)

mapme_options(
  outdir = outdir,
  verbose = TRUE
)
<<<<<<< HEAD

aoi_path <- system.file("extdata", "gfw_sample.gpkg", package = "mapme.biodiversity")
aoi <- st_read(aoi_path, quiet = TRUE)


aoi <- get_resources(aoi, get_mod14a2())

list.files("mapme-resources/mod14a2")
# character(0)

library(tmap)
tmap_mode("view")
tm_shape(aoi) +
  tm_borders()
=======


mod14a2_dir <- "mapme-data/mod14a2"  # chemin local contenant des .tif de Madagascar

aoi <- get_resources(aoi, get_mod14a2_local(folder = mod14a2_dir))

# 4. Appliquer la fonction d’indicateur
aoi <- calc_indicators(aoi, calc_fire_frequency(engine = "extract"))
>>>>>>> f84cb89b4ea65eb23bcd7139bacf883a9a482048
