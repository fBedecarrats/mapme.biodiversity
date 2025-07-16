devtools::load_all()
library(sf)
library(dplyr)
library(tidyr)

aoi_path <- system.file("extdata", "gfw_sample.gpkg", package = "mapme.biodiversity")
aoi <- st_read(aoi_path, quiet = TRUE)

outdir <- "mapme-resources"
dir.create(outdir, showWarnings = TRUE)

mapme_options(
  outdir = outdir,
  verbose = TRUE
)

aoi_path <- system.file("extdata", "gfw_sample.gpkg", package = "mapme.biodiversity")
aoi <- st_read(aoi_path, quiet = TRUE)


aoi <- get_resources(aoi, get_mod14a2())

list.files("mapme-resources/mod14a2")
# character(0)

library(tmap)
tmap_mode("view")
tm_shape(aoi) +
  tm_borders()
