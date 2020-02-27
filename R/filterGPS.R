#' \code{filterGPS} Filter out bad GPS position estimates
#'
#' @param data.list List of data frames created using \code{readLogger}
#' @param maxSpeed Maximum speed allowed between points, MORE DETAILS NEEDED HERE!!
#' @return Returns the same list as in the input, but with flagged GPS positions in the gps table,
#' @details Creates a quality 'flag' variable in the gps table, indicating which points have passed the
#' quality criterion and should be considered for further processing and analyses.
#' @family Logger sightings database functions
#' @seealso \code{\link{readLoggerTable}} to read single table from sightings database,
#'   \code{\link{readLogger}} to read entire sightings database,
#'   \code{\link{correctEffort}} tto correct inconsistencies in effort table,
#'   \code{\link{fillGPS}} To fill in missing GPS positions after filtering
#'   \code{\link{mkLeaflet}} to make interactive Leaflet map of sightings.
#' @author Martin Biuw
#' @example
#' midnat <- correctEffort()
#' @importFrom geosphere distHaversine
#' @import leaflet
#' @import htmlwidgets
#' @importFrom RColorBrewer brewer.pal
#' @export
#'

filterGPS <- function(data.list=midnat, maxSpeed=300) {
  require(geosphere)
  require(leaflet)
  require(htmlwidgets)
  require(RColorBrewer)

  gps <- data.list$gps
  dists <- distHaversine(cbind(gps$Longitude, gps$Latitude))/1852
  tDiff <- (as.numeric(gps$PCTime[-1])-as.numeric(gps$PCTime[-nrow(gps)]))/3600
  speeds <- dists/tDiff

  which.over <- which(speeds>maxSpeed)

  which.over <- which.over[order(speeds[which.over], decreasing=T)]

  n.deleted <- 0
  while(length(which.over)>0) {
    sumDist <- sum(dists)
    newDists <- c(sum(dists[-which.over[1]]),
                  sum(dists[-(which.over[1]+1)]))
    if(which.min(newDists)==1) {
      which.delete <- which.over[1]
    } else {
      which.delete <- which.over[1]+1
    }

    if(length(which.delete)>0) {
      gps <- gps[-which.delete,]
      n.deleted <- n.deleted+1
      cat('Number of positions filtered:', n.deleted, '                 \r')
      flush.console()
      dists <- distHaversine(cbind(gps$Longitude, gps$Latitude))/1852
      tDiff <- (as.numeric(gps$PCTime[-1])-as.numeric(gps$PCTime[-nrow(gps)]))/3600
      speeds <- dists/tDiff
      which.over <- which(speeds>maxSpeed)
    }
  }

  cat('\n\nDone! Total number of positions filtered:', n.deleted, '\n\n')
  flush.console()

  orig.gps <- data.list$gps

  orig.gps$ok <- rep(F, nrow(orig.gps))

  orig.gps$ok[match(gps$Index, orig.gps$Index)] <- T

  m <- leaflet()
  m <- addTiles(m, 'http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}')
##  m <- addTiles(m)
  m <- setView(m, mean(orig.gps$Longitude),
               mean(orig.gps$Latitude),
               zoom=5)

  m <- addPolylines(m, orig.gps$Longitude[which(orig.gps$ok)],
                    orig.gps$Latitude[which(orig.gps$ok)],
                    col=brewer.pal(12, 'Set3')[12], weight=2,
                    opacity=0.8)
  m <- addPolylines(m, orig.gps$Longitude, orig.gps$Latitude, weight=1, col='grey')
  m <- addMarkers(m, orig.gps$Longitude[which(!orig.gps$ok)],
                  orig.gps$Latitude[which(!orig.gps$ok)],
                  icon=list(iconUrl='https://cdn4.iconfinder.com/data/icons/social-messaging-ui-coloricon-1/21/39-512.png',
                            iconSize=c(10,10)))
  saveWidget(m, "map.html", selfcontained=F)
  viewer <- getOption("viewer")
  viewer('map.html')

  data.list$gps <- orig.gps
  data.list
}
