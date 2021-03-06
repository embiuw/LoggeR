#' \code{mkLeaflet} Make interactive Leaflet map of tracks and sightings
#'
#' @param data.list List of sublists, each of which contains data frames created using \code{readLogger}
#' @param birds Logical, indicating if map should present birds (TRUE) or whales (FALSE).
#' Default is to present whales. Both cannot (currently) be shown on the same map.
#' @return Saves a Leaflet object as an html file called \code{map.html} in the working directory
#' @details Draws an interactive map in a web browser, where the background is a WMS data layer.
#' NOTE! Computer must currently be connected to the internet in order to render the map background!
#' @family Logger sightings database functions
#' @seealso \code{\link{readLoggerTable}} to read single table from sightings database,
#'   \code{\link{readLogger}} to read entire sightings database,
#'   \code{\link{correctEffort}} to correct inconsistencies in effort table,
#'   \code{\link{filterGPS}} to flag bad GPS positions,
#'   \code{\link{fillGPS}} to fill in missing GPS positions after filtering,
#'   \code{\link{convert2spatial}} to convert selected parts of sightings data list to shapefiles.
#' @author Martin Biuw
#' @example
#' mkLeaflet()
#' @import leaflet
#' @import htmlwidgets
#' @import leaflet.minicharts
#' @import RColorBrewer
#' @importFrom randomcoloR randomColor
#' @export
#'

mkLeaflet <- function(data.list=list(midnat, fram), birds=F) {
  require(leaflet)
  require(htmlwidgets)
  require(RColorBrewer)
  require(randomcoloR)
  require(leaflet.minicharts)
  require(tidyr)

  if(length(data.list)>1) {
    set.view <- list(Lon=mean(unlist(lapply(data.list, function(x) x$gps$Longitude))),
                     Lat=mean(unlist(lapply(data.list, function(x) x$gps$Latitude))))

    if(birds) {
      groups <- unique(unlist(lapply(data.list[[1]]$birdSpecies$Code, function(x) unlist(strsplit(x, '-'))[1])))

      gHues <- c('red', 'orange', 'yellow', 'green', 'blue', 'purple', 'pink', 'monochrome')

      spCols <- vector('character')
      for(i in 1:length(gHues)) {
        nSpecies <- length(grep(groups[i], data.list[[1]]$birdSpecies$Code))
        spCols <- c(spCols, randomColor(nSpecies, hue=gHues[i]))
      }
    } else {
       spp <- do.call('rbind', lapply(data.list, function(x) data.frame(Species=x$sightings$Species, NumSP=x$sightings$NumSP)))

        for(i in 1:length(data.list)) {

          data.list[[i]]$sightings$Species <- factor(data.list[[i]]$sightings$Species,
                                       levels=spp$Species[match(sort(unique(as.numeric(spp$NumSP))),
                                                                       spp$NumSP)])
          spCols <- randomColor(nlevels(data.list[[1]]$sightings$Species),
                              luminosity='light')
        }
      }
  } else {
    set.view <- list(Lon=mean(data.list[[1]]$gps$Longitude),
                     Lat=mean(data.list[[1]]$gps$Latitude))
    if(birds) {
      groups <- unique(unlist(lapply(data.list[[1]]$birdSpecies$Code, function(x) unlist(strsplit(x, '-'))[1])))

      gHues <- c('red', 'orange', 'yellow', 'green', 'blue', 'purple', 'pink', 'monochrome')

      spCols <- vector('character')
      for(i in 1:length(gHues)) {
        nSpecies <- length(grep(groups[i], data.list[[1]]$birdSpecies$Code))
        spCols <- c(spCols, randomColor(nSpecies, hue=gHues[i]))
      }
    } else {
      spp <- data.frame(Species=data.list[[1]]$sightings$Species, NumSP=data.list[[1]]$sightings$NumSP)

      data.list[[1]]$sightings$Species <- factor(data.list[[1]]$sightings$Species,
                                                 levels=spp$Species[match(sort(unique(as.numeric(spp$NumSP))),
                                                                          spp$NumSP)])
      spCols <- randomColor(nlevels(data.list[[1]]$sightings$Species),
                            luminosity='light')
    }
  }

  m <- leaflet()
  m <- addTiles(m, 'http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}')
  ## m <- addTiles(m)
  m <- setView(m, set.view$Lon,
               set.view$Lat, zoom=5)

  for(l in 1:length(data.list)) {
    dl <- data.list[[l]]
    Sample.Label=unique(dl$gps$Sample.Label)
    Sample.Label <- Sample.Label[which(!is.na(Sample.Label))]
    m <- addPolylines(m, dl$gps$Longitude[which(dl$gps$ok)],
                      dl$gps$Latitude[which(dl$gps$ok)],
                      weight=1, col=brewer.pal(3, 'Set3')[2],
                      opacity=0.8)
    for(i in 1:length(Sample.Label)) {
      tmp <- dl$gps[which(dl$gps$Sample.Label==Sample.Label[i]),]
      m <- addPolylines(m, tmp$Longitude[which(tmp$ok)],
                        tmp$Latitude[which(tmp$ok)],
                        weight=4, col=brewer.pal(12, 'Set3')[12],
                        opacity=1)

    }

    if(birds) {
      smatch <- match(dl$birdSpecies$Code, names(dl$bsight))
      smatch <- smatch[which(!is.na(smatch))]
      for(i in 1:nrow(dl$bsight)) {
        piemat <- dl$bsight[i,smatch] %>%
           pivot_longer(names(dl$bsight)[smatch],
                        names_to='Code', values_to='N')
       piemat <- piemat[which(!is.na(piemat$N)),]
       which.sp <- match(piemat$Code, dl$birdSpecies$Code)
       piemat$Species <- dl$birdSpecies$CommonName[which.sp]

       if(nrow(piemat)>0) {
          m <- addCircleMarkers(m, dl$bsight$Longitude[i],
                                dl$bsight$Latitude[i],
                                radius=5*log(piemat$N+2),
                                stroke=T, fillOpacity=0.8,
                                col=spCols[which.sp],
                                clusterOptions = markerClusterOptions(radius=5*log(sum(piemat$N)+2),
                                                                      spiderfyDistanceMultiplier=1.5,
                                                                      spiderLegPolylineOptions = list(col='white')),
                                popup=paste('<center>10-minute sector count<br>Species:<b>', piemat$Species, '<br></b>Group size:<b>',
                                            piemat$N, '<br></b>Time sighted: <b>',
                                            format(dl$bsight$GpsTime[i], '%b-%d %H:%M'), ' UTC<br></b>Sighted by: <b>',
                                            dl$bsight[i,match('Seen-By', names(dl$bsight))], '</b></center>'))
        }
      }

      smatch <- match(dl$birdSpecies$Code, names(dl$snapshot))
      smatch <- smatch[which(!is.na(smatch))]

      for(i in 1:nrow(dl$snapshot)) {
        piemat <- dl$snapshot[i,smatch] %>%
          pivot_longer(names(dl$snapshot)[smatch],
                       names_to='Code', values_to='N')
        piemat <- piemat[which(!is.na(piemat$N)),]
        which.sp <- match(piemat$Code, dl$birdSpecies$Code)
        piemat$Species <- dl$birdSpecies$CommonName[which.sp]

        if(nrow(piemat)>0) {
          m <- addCircleMarkers(m, dl$snapshot$Longitude[i],
                                dl$snapshot$Latitude[i],
                                radius=5*log(piemat$N+2),
                                stroke=T, fillOpacity=0.8,
                                col=spCols[which.sp],
                                clusterOptions = markerClusterOptions(radius=5*log(sum(piemat$N)+2),
                                                                      spiderfyDistanceMultiplier=1.5,
                                                                      spiderLegPolylineOptions = list(col='white')),
                                popup=paste('<center>Aft snapshot<br>Species:<b>', piemat$Species, '<br></b>Group size:<b>',
                                            piemat$N, '<br></b>Time sighted: <b>',
                                            format(dl$snapshot$GpsTime[i], '%b-%d %H:%M'), ' UTC<br></b>Sighted by: <b>',
                                            dl$snapshot[i,match('Seen-By', names(dl$snapshot))], '</b></center>'))
        }
      }

    } else {
      for(i in 1:nlevels(dl$sightings$Species)) {
        grp <- levels(dl$sightings$Species)[i]
        tmp <- dl$sightings[which(dl$sightings$Species==levels(dl$sightings$Species)[i]),]
        grpsize <- paste0(tmp$'Num-Best', ' (', tmp$'Num-Min', '-', tmp$'Num-Max', ')')
        grpsize[which(tmp$'Num-Min'==tmp$'Num-Max')] <- tmp$'Num-Best'
        m <- addCircleMarkers(m, tmp$Longitude,
                              tmp$Latitude,
                              radius=5*log(tmp$`Num-Best`+2),
                              stroke=T, fillOpacity=0.8,
                              col=spCols[i],
                              ##clusterOptions = markerClusterOptions(),
                              popup=paste('<center>Species:<b>', tmp$Species, '<br></b>Group size:<b>',
                                          grpsize, '<br></b>Time sighted: <b>',
                                          format(tmp$GpsTime, '%b-%d %H:%M'), ' UTC<br></b>Sighted by: <b>',
                                          tmp[,match('Seen-By', names(tmp))], '</b></center>'))
        }
    }

    ##m <- addLayersControl(m, position='bottomleft',
    ##                      overlayGroups =levels(dl$sightings$Species),
    ##                      options = layersControlOptions(collapsed=FALSE))

  }
  saveWidget(m, "map.html", selfcontained=T)
  viewer <- getOption("viewer")
  viewer('map.html')

}





