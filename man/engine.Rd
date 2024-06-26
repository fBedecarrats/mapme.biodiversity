% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/engines.R
\name{engine}
\alias{engine}
\alias{check_engine}
\alias{check_stats}
\alias{select_engine}
\title{Function to select processing engines}
\usage{
check_engine(queried_engine)

check_stats(queried_stats)

select_engine(x, raster, stats, engine, name = NULL, mode = "asset")
}
\arguments{
\item{queried_engine}{A character vector of length one indicating the engine
to check for.}

\item{queried_stats}{A character vector with statistic names to be checked
if they are supported by the backend}

\item{x}{An sf object representing a portfolio.}

\item{raster}{An terra SpatRaster from which values are to be extracted.}

\item{stats}{A character vector of statistics to aggregate the raster values
with.}

\item{engine}{A character vector of length one specifying the engine to be
used for the extraction.}

\item{name}{A character vector indicating the name to append to the columns
names.}

\item{mode}{A character vector indicating in which mode to conduct the
extraction (e.g. \code{asset}-wise or for the whole \code{portfolio} at once).}
}
\value{
\code{check_engine()} returns the character of the queried engine,
if supported. Throws an error otherwise.

\code{check_stats} returns a character vector of supported statistics.
Throws an error if any of the queried statistics is not supported.

\code{select_engine} returns a tibble.
}
\description{
\code{check_engine()} checks if an extraction engine for zonal vector-raster
operations is supported by the backend.

\code{check_stats} checks if one or multiple statistics are supported
for zonal vector-raster extraction by the backend.

\code{select_engine} extracts zonal vector-raster statistics for supported engine
and for one or more statistics. Columns are named according to the argument
\code{name} plus the respective stat. Both \code{portfolio} and \code{asset} modes are supported.
}
\keyword{utils}
