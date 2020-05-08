#' \code{coast.dist} Find distance to coastline along the extended line of a sighting
#' @param data Data list created from Access sightings database.
#' @param max.distance Maximum distance (in meters) from the vessel within which to detect a coastline.
#' This should be set to the distance to the horizon based on the height of the observation platform
#' @param coast.data Path to a valid OGR data source representing coastline.
#' This should be the \code{'ADD_DerivedLowresBasemap.shp'} in the \code{SimpleBasemap}
#' subdirectory of the \code{Quantarctica} package. Change the path to the correct path on your system.
#' @details Finds the closest coastline along the extended bearing to a detection.
#' @return The entire original database, with a new variable, \code{land}, in the sightings table
#' @export

coast.dist <- function(data=fram, max.distance=10000,
                       coast.data='ADD_DerivedLowresBasemap.shp') {
  require(rgdal)
  coast <- readOGR(coast.data)
  crs <- proj4string(coast)

  sightings <- data$sightings
  gps <- data$gps
  which.valid <- which(!is.na(sightings$Longitude) &
                       !is.na(sightings$Latitude) &
                       !is.na(sightings$`Ret-Distance`))
  sight <- sightings[which.valid,]
  coordinates(sight) <- c('Longitude', 'Latitude')
  proj4string(sight) <- '+proj=longlat +ellps=WGS84'
  sight <- spTransform(sight, CRS(crs))

  land <- rep(NA, nrow(sight))

  pb <- txtProgressBar(title = "Calculating distance to coast", min = 0,
                       max = nrow(sight), style=3)

  for(i in c(1:nrow(sight))) {
    setTxtProgressBar(pb, i, title=paste("Calculating distance to coast",
                                         round(i/nrow(sight)*100, 0),
                                         "% done"))

    which.gps <- which.min(abs(sight$PCTime[i]-gps$PCTime))
    hdg <- gps$Heading[which.gps]
    angle <- sight$Angle[i]
    if(angle>180) {
      angle <- angle-360
    }
    brg <- hdg + angle
    if(brg>360) brg <- brg-360
    crd <- coordinates(sight)[i,]
    x <- crd[1]+(seq(0,max.distance,by=10)*sin(rad(brg)))
    y <- crd[2]+(seq(0,max.distance,by=10)*cos(rad(brg)))
    crds <- data.frame(x=x, y=y)
    coordinates(crds) <- c('x', 'y')
    proj4string(crds) <- proj4string(coast)
    cl <- over(crds, coast)
    land[i] <- seq(0,max.distance,by=10)[which(cl!='Ocean')[1]]
  }
  close(pb)
  sightings$land <- rep(NA, nrow(sightings))
  sigtings$land[which.valid] <- land
  data$sightings <- sightings
  data
}
