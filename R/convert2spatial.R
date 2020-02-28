#' \code{convert2spatial} Convert selected parts of sightings data list to shapefiles.
#'
#' @param data.list List of data frames created using \code{readLogger}
#' @param zone Which UTM zone should data be converted to.
#' If this is NA (default), then shapefiles are in geographic (Lat/Lon) projection.
#' To convert to UTM, set this argument to desired zone (22 for the Scotia Sea).
#' @param hemisphere Hemisphere where data were collected.
#' @param whales Logical, indicating if whale sightings table should be read
#' @param birds Logical, indicating if bird sightings tables should be read
#' @param shp.dir Directory to which output shapefiles should be written.
#' @return Writes shapefiles to specified directory.
#' @details Writes ESRI shapefiles that can be read by standard GIS software.
#' @family Logger sightings database functions
#' @seealso \code{\link{readLoggerTable}} to read single table from sightings database,
#'   \code{\link{readLogger}} to read entire sightings database,
#'   \code{\link{correctEffort}} tto correct inconsistencies in effort table,
#'   \code{\link{filterGPS}} to flag bad GPS positions,
#'   \code{\link{fillGPS}} To fill in missing GPS positions after filtering,
#'   \code{\link{mkLeaflet}} to make interactive Leaflet map of sightings.
#' @author Martin Biuw
#' @example
#' midnat <- fillGPS()
#' @import rgdal
#' @export
#'

convert2spatial <- function(data.list=midnat, zone=NA, hemisphere='south',
                            whales=T, birds=T, shp.dir='./shapefiles') {
  require(rgdal)

  shp.dir <- paste0(getwd(), gsub('.', '', shp.dir, fixed=T))
  if(!dir.exists(shp.dir)) dir.create(shp.dir)

  p4sLL <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  if(!is.na(zone)) p4sUTM <- paste0('+proj=utm +zone=', zone, ' +south ellps=WGS84')

  gps <- data.list$gps
  coordinates(gps) <- c('Longitude', 'Latitude')
  proj4string(gps) <- CRS(p4sLL)
  if(!is.na(zone)) gps <- spTransform(gps, CRS(p4sUTM))
  writeOGR(gps, paste0(shp.dir, '/gps.shp'),
           layer='PCTime', driver='ESRI Shapefile')

  effort <- data.list$effort
  coordinates(effort) <- c('Longitude', 'Latitude')
  proj4string(effort) <- CRS(p4sLL)
  if(!is.na(zone)) effort <- spTransform(effort, CRS(p4sUTM))
  writeOGR(effort, paste0(shp.dir, '/effort.shp'),
           layer='GpsTime', driver='ESRI Shapefile')

  if(whales) {
    sightings <- data.list$sightings
    coordinates(sightings) <- c('Longitude', 'Latitude')
    proj4string(sightings) <- CRS(p4sLL)
    if(!is.na(zone)) sightings <- spTransform(sightings, CRS(p4sUTM))
    writeOGR(sightings, paste0(shp.dir, '/sightings.shp'),
             layer='GpsTime', driver='ESRI Shapefile')
  }

  if(birds) {
    if('bsight' %in% names(data.list)) {
      bsight <- data.list$bsight
      coordinates(bsight) <- c('Longitude', 'Latitude')
      proj4string(bsight) <- CRS(p4sLL)
      if(!is.na(zone)) bsight <- spTransform(bsight, CRS(p4sUTM))
      writeOGR(bsight, paste0(shp.dir, '/bsight.shp'),
               layer='GpsTime', driver='ESRI Shapefile')
    }
    if('snapshot' %in% names(data.list)) {
      bsnap <- data.list$snapshot
      coordinates(bsnap) <- c('Longitude', 'Latitude')
      proj4string(bsnap) <- CRS(p4sLL)
      if(!is.na(zone)) bsnap <- spTransform(bsnap, CRS(p4sUTM))
      writeOGR(bsnap, paste0(shp.dir, '/snapshot.shp'),
               layer='GpsTime', driver='ESRI Shapefile')
    }
  }
  cat('\nShapefiles written to', shp.dir, '\n')
  flush.console()
}
