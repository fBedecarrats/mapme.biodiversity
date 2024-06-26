% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_tri.R
\name{tri}
\alias{tri}
\alias{calc_tri}
\title{Calculate Terrain Ruggedness Index (TRI) statistics}
\usage{
calc_tri(engine = "extract", stats = "mean")
}
\arguments{
\item{engine}{The preferred processing functions from either one of "zonal",
"extract" or "exactextract" as character.}

\item{stats}{Function to be applied to compute statistics for polygons either
single or multiple inputs as character. Supported statistics are: "mean",
"median", "sd", "min", "max", "sum" "var".}
}
\value{
A function that returns a tibble with a column for terrain
ruggedness index statistics (in meters).
The range of index values and corresponding meaning:
(1) 0 - 80 m  :- level surface
(2) 81-116 m  :- nearly level surface
(3) 117-161 m :- slightly rugged surface
(4) 162-239 m :- intermediately rugged surface
(5) 240-497 m :- moderately rugged surface
(6) 498-958 m :- highly rugged surface
(7) 959-4367 m:- extremely rugged surface
}
\description{
Terrain Ruggedness Index is a measurement developed by Riley, et al. (1999).
The elevation difference between the centre pixel and its eight immediate
pixels are squared and then averaged and its square root is taken to get
the TRI value. This function allows to calculate terrain ruggedness
index (tri) statistics for polygons. For each polygon, the desired statistic(s)
are returned.
}
\details{
The required resources for this indicator are:
\itemize{
\item \link{nasa_srtm}
}
}
\examples{
\dontshow{
mapme.biodiversity:::.copy_resource_dir(file.path(tempdir(), "mapme-data"))
}
\dontrun{
library(sf)
library(mapme.biodiversity)

outdir <- file.path(tempdir(), "mapme-data")
dir.create(outdir, showWarnings = FALSE)

mapme_options(
  outdir = outdir,
  verbose = FALSE
)

aoi <- system.file("extdata", "sierra_de_neiba_478140_2.gpkg",
  package = "mapme.biodiversity"
) \%>\%
  read_sf() \%>\%
  get_resources(get_nasa_srtm()) \%>\%
  calc_indicators(
    calc_tri(stats = c("mean", "median", "sd", "var"), engine = "extract")
  ) \%>\%
  portfolio_long()

aoi
}
}
\references{
Riley, S. J., DeGloria, S. D., & Elliot, R. (1999). Index that quantifies
topographic heterogeneity. Intermountain Journal of Sciences, 5(1-4), 23-27.
}
\keyword{indicator}
