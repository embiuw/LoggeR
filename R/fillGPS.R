#' \code{fillGPS} Fill in missing GPS positions after filtering
#'
#' @param data.list List of data frames created using \code{readLogger}
#' @param replace.all Logical, if TRUE, then all positions are replaced by new ones,
#' otherwise only missing positions are filled in (default).
#' @return Returns the same list as in the input, but with missing GPS positions interpolated in all tables.
#' @details Based on the filtered GPS points, empty GPS points in other tables are
#' filled in by interpolation on time
#' @family Logger sightings database functions
#' @seealso \code{\link{readLoggerTable}} to read single table from sightings database,
#'   \code{\link{readLogger}} to read entire sightings database,
#'   \code{\link{correctEffort}} to correct inconsistencies in effort table,
#'   \code{\link{filterGPS}} to flag bad GPS positions,
#'   \code{\link{mkLeaflet}} to make interactive Leaflet map of sightings,
#'   \code{\link{convert2spatial}} to convert selected parts of sightings data list to shapefiles.
#' @author Martin Biuw
#' @example
#' midnat <- fillGPS()
#' @import rgdal
#' @export
#'


fillGPS <- function(data.list=midnat, replace.all=F) {
  require(rgdal)

  gps <- data.list$gps[which(data.list$gps$ok),]

  coordinates(gps) <- c('Longitude', 'Latitude')
  proj4string(gps) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
  gps <- spTransform(gps, CRS('+proj=utm +zone=22 +south ellps=WGS84'))

  if(replace.all) {
    sight.na <- c(1:nrow(data.list$sightings))
  } else {
    sight.na <- which(is.na(data.list$sightings$Longitude) | is.na(data.list$sightings$Latitude))
  }
  if(length(sight.na)>0) {
    times.na <- data.list$sightings$PCTime[sight.na]
    coord.na <- data.frame(Lon=approx(gps$PCTime, coordinates(gps)[,1], times.na)$y,
                      Lat=approx(gps$PCTime, coordinates(gps)[,2], times.na)$y)
    coordinates(coord.na) <- c('Lon', 'Lat')
    proj4string(coord.na) <- proj4string(gps)
    coord.na <- spTransform(coord.na, CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

    data.list$sightings$Longitude[sight.na] <- coordinates(coord.na)[,1]
    data.list$sightings$Latitude[sight.na] <- coordinates(coord.na)[,2]
  }


  if(replace.all) {
    eff.na <- c(1:nrow(data.list$effort))
  } else {
    eff.na <- which(is.na(data.list$effort$Longitude) | is.na(data.list$effort$Latitude))
  }

  if(length(eff.na)>0) {
    times.na <- data.list$effort$GpsTime[eff.na]
    coord.na <- data.frame(Lon=approx(gps$PCTime, coordinates(gps)[,1], times.na)$y,
                           Lat=approx(gps$PCTime, coordinates(gps)[,2], times.na)$y)
    coordinates(coord.na) <- c('Lon', 'Lat')
    proj4string(coord.na) <- proj4string(gps)
    coord.na <- spTransform(coord.na, CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

    data.list$effort$Longitude[eff.na] <- coordinates(coord.na)[,1]
    data.list$effort$Latitude[eff.na] <- coordinates(coord.na)[,2]
  }

  if("bsight" %in% names(data.list)) {
    if(replace.all) {
      bsight.na <- c(1:nrow(data.list$bsight))
    } else {
      bsight.na <- which(is.na(data.list$bsight$Longitude) | is.na(data.list$bsight$Latitude))
    }

    if(length(bsight.na)>0) {
      times.na <- data.list$bsight$GpsTime[bsight.na]
      if(class(times.na)=='character') {
        data.list$bsight$GpsTime <- as.POSIXct(strptime(data.list$bsight$GpsTime,
                                                        '%Y-%m-%d %H:%M:%S'), tz='UTC')
        times.na <- data.list$bsight$GpsTime[bsight.na]
      }
      coord.na <- data.frame(Lon=approx(gps$PCTime, coordinates(gps)[,1], times.na)$y,
                             Lat=approx(gps$PCTime, coordinates(gps)[,2], times.na)$y)
      coordinates(coord.na) <- c('Lon', 'Lat')
      proj4string(coord.na) <- proj4string(gps)
      coord.na <- spTransform(coord.na, CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

      data.list$bsight$Longitude[bsight.na] <- coordinates(coord.na)[,1]
      data.list$bsight$Latitude[bsight.na] <- coordinates(coord.na)[,2]
    }
  }

  if("snapshot" %in% names(data.list)) {
    if(replace.all) {
      bsnap.na <- c(1:nrow(data.list$snapshot))
    } else {
      bsnap.na <- which(is.na(data.list$snapshot$Longitude) | is.na(data.list$snapshot$Latitude))
    }

    if(length(bsnap.na)>0) {
      times.na <- data.list$snapshot$GpsTime[bsnap.na]
      if(class(times.na)=='character') {
        data.list$snapshot$GpsTime <- as.POSIXct(strptime(data.list$snapshot$GpsTime,
                                                          '%Y-%m-%d %H:%M:%S'), tz='UTC')
        times.na <- data.list$snapshot$GpsTime[bsnap.na]
      }

      coord.na <- data.frame(Lon=approx(gps$PCTime, coordinates(gps)[,1], times.na)$y,
                             Lat=approx(gps$PCTime, coordinates(gps)[,2], times.na)$y)
      coordinates(coord.na) <- c('Lon', 'Lat')
      proj4string(coord.na) <- proj4string(gps)
      coord.na <- spTransform(coord.na, CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

      data.list$snapshot$Longitude[bsnap.na] <- coordinates(coord.na)[,1]
      data.list$snapshot$Latitude[bsnap.na] <- coordinates(coord.na)[,2]
    }
  }
  data.list
}
