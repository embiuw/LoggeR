#' \code{defineSegments} Define segments based on distance along transect line
#'
#' @param data.list List of data frames created using \code{readLogger}
#' @param seglen Integer of desired segment length
#' @param units Units of seglen Can be 'm', 'km' or 'nm' (default) 
#' @return Returns the same list as in the input, but with variable 'Segment.Label' in all data frames
#' @details
#' @family Logger sightings database functions
#' @seealso \code{\link{readLoggerTable}} to read single table from sightings database,
#'   \code{\link{readLogger}} to read entire sightings database,
#'   \code{\link{filterGPS}} to flag bad GPS positions,
#'   \code{\link{fillGPS}} to fill in missing GPS positions after filtering
#'   \code{\link{mkLeaflet}} to make interactive Leaflet map of sightings,
#'   \code{\link{convert2spatial}} to convert selected parts of sightings data list to shapefiles.
#' @author Martin Biuw
#' @example
#' midnat <- correctEffort()
#' @import dygraphs
#' @import xts
#' @import ggplot2
#' @import ggthemes
#' @import xts
#' @import plotly
#' @import RColorBrewer
#' @export
#'

defineSegments <- function(data.list=fram, seglen=1, units='nm') {
  require(geosphere)
  
  data.list$gps <- data.list$gps[order(data.list$gps$PCTime),]
  cumdist <- c(0, cumsum(distHaversine(cbind(data.list$gps$Longitude, data.list$gps$Latitude))))
  
  if(units=='nm') {
    seglen <- seglen * 1852
  }
  
  if(units=='km') {
    seglen <- seglen * 1000
  }
  
  segs <- as.numeric(cut(cumdist, seq(0, max(cumdist)+seglen, by=seglen)))
  if(is.na(segs[1])) segs[1] <- 1
  
  data.list$gps$Segment.Label <- segs
  seg.start <- data.list$gps$PCTime[match(unique(segs), segs)]
  
  data.list$effort$Segment.Label <- rep(NA, nrow(data.list$effort))
  data.list$sightings$Segment.Label <- rep(NA, nrow(data.list$sightings))
  
  if('bsight' %in% names(data.list)) {
    data.list$bsight$Segment.Label <- rep(NA, nrow(data.list$bsight))
  }
  if('snapshot' %in% names(data.list)) {
    if(class(data.list$snapshot)!='character') {
      data.list$snapshot$Segment.Label <- rep(NA, nrow(data.list$snapshot))
    }
  }  
  
  pb <- txtProgressBar(title = "Defining trip segments", min = 0,
                       max = length(seg.start), style=3)
  
  
  for(i in 2:length(seg.start)) {
    setTxtProgressBar(pb, i, title=paste("Defining trip segments",
                                         round(i/length(seg.start)*100, 0),
                                         "% done"))
    eff.within <- which(data.list$effort$PCTime>=seg.start[i-1] &
                          data.list$effort$PCTime<seg.start[i]) 
    data.list$effort$Segment.Label[eff.within] <- i-1
    
    sight.within <- which(data.list$sightings$PCTime>=seg.start[i-1] &
                          data.list$sightings$PCTime<seg.start[i]) 
    data.list$sightings$Segment.Label[sight.within] <- i-1

    if('bsight' %in% names(data.list)) {
      bsight.within <- which(data.list$bsight$PCTime>=seg.start[i-1] &
                            data.list$bsight$PCTime<seg.start[i]) 
      data.list$bsight$Segment.Label[bsight.within] <- i-1
    }
    
    if('snapshot' %in% names(data.list)) {
      if(class(data.list$snapshot)!='character') {
        bsnap.within <- which(data.list$snapshot$PCTime>=seg.start[i-1] &
                               data.list$snapshot$PCTime<seg.start[i]) 
        data.list$snapshot$Segment.Label[bsnap.within] <- i-1
      }
    }  
  }
  close(pb)
  
  data.list
}