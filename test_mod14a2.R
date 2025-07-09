devtools::load_all()
library(sf)
library(dplyr)
library(tidyr)

aoi_path <- system.file("extdata", "gfw_sample.gpkg", package = "mapme.biodiversity")
aoi <- st_read(aoi_path, quiet = TRUE)

outdir <- "mapme-resources"
dir.create(outdir, showWarnings = FALSE)

mapme_options(
  outdir = outdir,
  verbose = TRUE
)

#  Appeler get_resources() pour ta nouvelle ressource
aoi <- get_resources(aoi, get_mod14a2(years = 2017))
# list.files("mapme-resources/mod14a2")

# Appliquer la fonction d’indicateur
aoi <- calc_indicators(aoi, calc_fire_frequency(engine = "extract"))



