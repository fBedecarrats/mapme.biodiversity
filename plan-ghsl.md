# Plan: Add GHSL Population Resource and Indicator to mapme.biodiversity

## Overview

Add GHS-POP R2023A (GHSL population grid) as a new resource and a corresponding `ghsl_population` indicator, following the same patterns as the existing `worldpop` / `population_count` implementation.

## Data Source

**GHS-POP R2023A** — Global population grid from the European Commission Joint Research Centre.

- **Resolution**: 30 arc-seconds (~1km), WGS84 (EPSG:4326) — matches WorldPop's resolution
- **Epochs**: 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025, 2030
- **Format**: Zip archives containing GeoTIFF files
- **Licence**: European Commission reuse policy (free and open)
- **URL pattern**:
  ```
  https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/
    GHS_POP_GLOBE_R2023A/
    GHS_POP_E{epoch}_GLOBE_R2023A_4326_30ss/V1-0/
    GHS_POP_E{epoch}_GLOBE_R2023A_4326_30ss_V1_0.zip
  ```
- **File size**: ~300 MB per epoch (compressed zip)
- **Reference**: Pesaresi et al. (2024), *International Journal of Digital Earth*, 17(1). doi:10.1080/17538947.2024.2390454

## Files to Create/Modify

### 1. `R/get_ghsl_pop.R` — Resource function

Factory function `get_ghsl_pop(years = 2020)` returning a closure with standard signature.

- Validate `years` against available epochs: `seq(1975, 2030, 5)`
- Warn about needing `outdir` (like worldpop — files must be downloaded)
- Inner function downloads zip archives, extracts the TIF, optionally compresses with DEFLATE
- Uses `make_footprints()` to return sf footprint object
- Caches: checks `spds_exists()` before downloading
- Register via `register_resource()`

### 2. `R/calc_ghsl_population.R` — Indicator function

Factory function `calc_ghsl_population(engine = "extract", stats = "sum")` returning a closure.

- Validate engine and stats via `check_engine()` / `check_stats()`
- Inner function takes `ghsl_pop` resource parameter
- Extracts year from raster layer names to produce `datetime` column
- Returns standard tibble: `datetime`, `variable`, `unit`, `value`
- Variable naming: `population_{stat}` (e.g., `population_sum`)
- Unit: `"count"`
- Register via `register_indicator()`

### 3. `tests/testthat/test-get_ghsl_pop.R` — Resource tests

- Test year validation (invalid year errors)
- Test `get_ghsl_pop()` returns valid function via `.check_resource_fun()`
- Test footprints via `.check_footprints()` using sample data from `inst/res/ghsl_pop/`

### 4. `tests/testthat/test-calc_ghsl_population.R` — Indicator tests

- Test default stats (sum)
- Test multiple stats (mean, median, sd)
- Test all three engines (zonal, extract, exactextract)
- Validate output structure via `.check_single_asset()`
- Snapshot test for exactextract values

### 5. `inst/res/ghsl_pop/` — Sample data

- Small cropped GeoTIFF(s) covering the `sierra_de_neiba_478140.gpkg` test AOI
- Compressed (DEFLATE), converted to reasonable data type

### 6. `data-raw/ghsl_pop.R` — Data generation script

- Script to download, crop, and compress the sample data
- Reproduces the files in `inst/res/ghsl_pop/`

## Implementation Approach

Follow the worldpop pattern closely:

1. **Download strategy**: Download full zip → extract TIF → compress with DEFLATE → store locally. Similar to worldpop's `download.file()` + `gdal_utils("translate")` pattern.
2. **No new dependencies**: Uses only existing imports (`sf::gdal_utils`, `utils::download.file`, `utils::unzip`).
3. **Naming conventions**: `ghsl_pop` (resource), `ghsl_population` (indicator) — avoids collision with existing `worldpop`/`population_count`.

## Step-by-step Execution Order

1. Create sample data script `data-raw/ghsl_pop.R` and generate `inst/res/ghsl_pop/` files
2. Create `R/get_ghsl_pop.R` with resource function and registration
3. Create `R/calc_ghsl_population.R` with indicator function and registration  
4. Create `tests/testthat/test-get_ghsl_pop.R`
5. Create `tests/testthat/test-calc_ghsl_population.R`
6. Run `devtools::document()` and `devtools::check()` to verify integration
