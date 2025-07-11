devtools::load_all()
library(sf)
library(dplyr)
library(tidyr)

aoi_url <- "https://raw.githubusercontent.com/BETSAKA/Tools/main/data/AP_Vahatra.geojson"
aoi <- st_read(aoi_url, quiet = TRUE) %>%
  filter(nom == "Ankarafantsika")


outdir <- "mapme-resources"
dir.create(outdir, showWarnings = FALSE)

mapme_options(
  outdir = outdir,
  verbose = TRUE
)


mod14a2_dir <- "mapme-data/mod14a2"  # chemin local contenant des .tif de Madagascar

aoi <- get_resources(aoi, get_mod14a2_local(folder = mod14a2_dir))

# 4. Appliquer la fonction d’indicateur
aoi <- calc_indicators(aoi, calc_fire_frequency(engine = "extract"))
