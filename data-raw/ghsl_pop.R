## Code to prepare `ghsl_pop` sample dataset
library(terra)
library(sf)

aoi <- system.file("extdata", "sierra_de_neiba_478140.gpkg",
  package = "mapme.biodiversity"
) %>% st_read()

# Read GHSL population data remotely via GDAL vsicurl and crop to AOI
# Data is in WGS84 (EPSG:4326) at 30 arc-second resolution (~1km)
epochs <- c(2000, 2005, 2010, 2015, 2020)
dir.create("inst/res/ghsl_pop", showWarnings = FALSE)

for (epoch in epochs) {
  layer_name <- sprintf("GHS_POP_E%d_GLOBE_R2023A_4326_30ss", epoch)
  filename <- sprintf("%s_V1_0.tif", layer_name)
  zip_url <- sprintf(
    "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/%s/V1-0/%s_V1_0.zip",
    layer_name, layer_name
  )
  vsicurl_path <- sprintf("/vsizip/vsicurl/%s/%s", zip_url, filename)

  r <- rast(vsicurl_path)
  r_crop <- crop(r, vect(aoi))

  writeRaster(
    r_crop,
    file.path("inst/res/ghsl_pop", filename),
    overwrite = TRUE,
    gdal = c("COMPRESS=DEFLATE"),
    datatype = "FLT4S"
  )
}
