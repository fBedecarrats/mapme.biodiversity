#' Climate Hazards Group InfraRed Precipitation with Station data (CHIRPS)
#'
#' This resource is published by Funk et al. (2015) and represents a quasi-global
#' (50°S-50°S) rainfall estimation at a monthly resolution starting with the year
#' 1981 to the near-present. It has a spatial resolution of 0.05°. The data can
#' be used to retrieve information on the amount of rainfall. Due to the availability
#' of +30 years, anomaly detection and long-term average analysis is also possible.
#' The routine will download the complete archive in order to support long-term
#' average and anomaly calculations with respect to the 1981 - 2010 climate normal
#' period. Thus no additionall arguments need to be specified.
#'
#'
#' @name chirps
#' @docType data
#' @keywords resource
#' @format Global raster layers available for years 1981 to near-present.
#' @source \url{https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/cogs/}
#' @references Funk, C., Peterson, P., Landsfeld, M. et al. The climate hazards
#' infrared precipitation with stations—a new environmental record for
#' monitoring extremes. Sci Data 2, 150066 (2015).
#' \url{https://doi.org/10.1038/sdata.2015.66}
NULL



.get_chirps <- function(x,
                        rundir = tempdir(),
                        verbose = TRUE) {
  chirps_url <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/cogs/"
  chirps_list <- RCurl::getURL(chirps_url, dirlistonly = TRUE, ftp.use.epsv = FALSE)
  chirps_list <- unique(stringr::str_extract_all(chirps_list, stringr::regex("chirps-\\s*(.*?)\\s*.cog"))[[1]])
  urls <- paste(chirps_url, chirps_list, sep = "")
  filenames <- file.path(rundir, basename(urls))

  aria_bin <- attributes(x)$aria_bin
  .download_or_skip(urls,
    filenames,
    verbose = verbose,
    check_existence = TRUE,
    aria_bin = aria_bin
  )

  filenames
}