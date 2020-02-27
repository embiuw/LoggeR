#' \code{fillGPS} Fill in missing GPS positions after filtering
#'
#' @param data.list List of data frames created using \code{readLogger}
#' @return Returns the same list as in the input, but with missing GPS positions interpolated in all tables.
#' @details Based on the filtered GPS points, empty GPS points in other tables are
#' filled in by interpolation on time
#' @family Logger sightings database functions
#' @seealso \code{\link{readLoggerTable}} to read single table from sightings database,
#'   \code{\link{readLogger}} to read entire sightings database,
#'   \code{\link{correctEffort}} tto correct inconsistencies in effort table,
#'   \code{\link{filterGPS}} to flag bad GPS positions,
#'   \code{\link{mkLeaflet}} to make interactive Leaflet map of sightings.
#' @author Martin Biuw
#' @example
#' midnat <- fillGPS()
#' @import rgdal
#' @export
#'


fillGPS <- function(data.list=midnat) {
  require(rgdal)

  gps <- data.list$gps[which(data.list$gps$ok),]

  coordinates(gps) <- c('Longitude', 'Latitude')
  proj4string(gps) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
  gps <- spTransform(gps, CRS("+proj=utm +zone=22 +south ellps=WGS84"))

  sight.na <- which(is.na(data.list$sightings$Longitude) | is.na(data.list$sightings$Latitude))
  times.na <- data.list$sightings$PCTime[sight.na]
  coord.na <- data.frame(Lon=approx(gps$PCTime, coordinates(gps)[,1], times.na)$y,
                    Lat=approx(gps$PCTime, coordinates(gps)[,2], times.na)$y)
  coordinates(coord.na) <- c('Lon', 'Lat')
  proj4string(coord.na) <- proj4string(gps)
  coord.na <- spTransform(coord.na, CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

  data.list$sightings$Longitude[sight.na] <- coordinates(coord.na)[,1]
  data.list$sightings$Latitude[sight.na] <- coordinates(coord.na)[,2]


  eff.na <- which(is.na(data.list$eff$Longitude) | is.na(data.list$eff$Latitude))
  times.na <- data.list$effort$PCTime[eff.na]
  coord.na <- data.frame(Lon=approx(gps$PCTime, coordinates(gps)[,1], times.na)$y,
                         Lat=approx(gps$PCTime, coordinates(gps)[,2], times.na)$y)
  coordinates(coord.na) <- c('Lon', 'Lat')
  proj4string(coord.na) <- proj4string(gps)
  coord.na <- spTransform(coord.na, CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

  data.list$effort$Longitude[eff.na] <- coordinates(coord.na)[,1]
  data.list$effort$Latitude[eff.na] <- coordinates(coord.na)[,2]


  if("bsight" %in% names(data.list)) {
    bsight.na <- which(is.na(data.list$bsight$Longitude) | is.na(data.list$bsight$Latitude))
    times.na <- data.list$bsight$PCTime[bsight.na]
    coord.na <- data.frame(Lon=approx(gps$PCTime, coordinates(gps)[,1], times.na)$y,
                           Lat=approx(gps$PCTime, coordinates(gps)[,2], times.na)$y)
    coordinates(coord.na) <- c('Lon', 'Lat')
    proj4string(coord.na) <- proj4string(gps)
    coord.na <- spTransform(coord.na, CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

    data.list$Longitude[bsight.na] <- coordinates(coord.na)[,1]
    data.list$Latitude[bsight.na] <- coordinates(coord.na)[,2]
  }

  if("snapshot" %in% names(data.list)) {
    bsnap.na <- which(is.na(data.list$bsnap$Longitude) | is.na(data.list$bsnap$Latitude))
    times.na <- data.list$bsnap$PCTime[bsnap.na]
    coord.na <- data.frame(Lon=approx(gps$PCTime, coordinates(gps)[,1], times.na)$y,
                           Lat=approx(gps$PCTime, coordinates(gps)[,2], times.na)$y)
    coordinates(coord.na) <- c('Lon', 'Lat')
    proj4string(coord.na) <- proj4string(gps)
    coord.na <- spTransform(coord.na, CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

    data.list$snapshot$Longitude[bsnap.na] <- coordinates(coord.na)[,1]
    data.list$snapshot$Latitude[bsnap.na] <- coordinates(coord.na)[,2]
  }
  data.list
}
