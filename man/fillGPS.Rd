% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fillGPS.R
\name{fillGPS}
\alias{fillGPS}
\title{\code{fillGPS} Fill in missing GPS positions after filtering}
\usage{
fillGPS(data.list = midnat, replace.all = F)
}
\arguments{
\item{data.list}{List of data frames created using \code{readLogger}}

\item{replace.all}{Logical, if TRUE, then all positions are replaced by new ones,
otherwise only missing positions are filled in (default).}
}
\value{
Returns the same list as in the input, but with missing GPS positions interpolated in all tables.
}
\description{
\code{fillGPS} Fill in missing GPS positions after filtering
}
\details{
Based on the filtered GPS points, empty GPS points in other tables are
filled in by interpolation on time
}
\seealso{
\code{\link{readLoggerTable}} to read single table from sightings database,
  \code{\link{readLogger}} to read entire sightings database,
  \code{\link{correctEffort}} to correct inconsistencies in effort table,
  \code{\link{filterGPS}} to flag bad GPS positions,
  \code{\link{mkLeaflet}} to make interactive Leaflet map of sightings,
  \code{\link{convert2spatial}} to convert selected parts of sightings data list to shapefiles.

Other Logger sightings database functions: 
\code{\link{convert2spatial}()},
\code{\link{correctEffort}()},
\code{\link{filterGPS}()},
\code{\link{mkLeaflet}()},
\code{\link{readLoggerTable}()},
\code{\link{readLogger}()}
}
\author{
Martin Biuw
}
\concept{Logger sightings database functions}
