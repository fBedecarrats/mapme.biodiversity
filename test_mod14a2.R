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
# 1. Charger un AOI exemple
aoi_path <- system.file("extdata", "gfw_sample.gpkg", package = "mapme.biodiversity")
aoi <- st_read(aoi_path, quiet = TRUE)

# 3. Appeler get_resources() pour ta nouvelle ressource
aoi <- get_resources(aoi, get_mod14a2(years = 2020))

# 4. Vérifie que la colonne .footprints contient bien des chemins de fichiers
aoi$.footprints$mod14a2
