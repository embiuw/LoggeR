#' \code{correctEffort} Correct errors start & end times in effort table
#'
#' @param data.list List of data frames created using \code{readLogger}
#' @param include.birds Logical, indicating if bird sightings table should be used
#' when correcting effort. Default is FALSE, i.e. only whale sightings and effort tables are used.
#' @param tGap Time (in minutes) at which new effort start is assumed even if first data record
#' does not have a designated start ('SE') event
#' @return Returns the same list as in the input, but with corrected start and end of effort designations,
#' and with all sightings and gps points allocated to transects (defined as contiguous effort periods).
#' @details
#' @family Logger sightings database functions
#' @seealso \code{\link{readLoggerTable}} to read single table from sightings database,
#'   \code{\link{readLogger}} to read entire sightings database,
#'   \code{\link{filterGPS}} to flag bad GPS positions,
#'   \code{\link{fillGPS}} To fill in missing GPS positions after filtering
#'   \code{\link{mkLeaflet}} to make interactive Leaflet map of sightings.
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

correctEffort <- function(data.list=midnat, include.birds=F, tGap=60, plotType='dygraph') {

  effort <- data.list$effort
  effort <- effort[which(apply(effort[,c(3:6)], 1, function(x) !all(is.na(x)))),]

  gps <- data.list$gps

  if('sightings' %in% names(data.list)) sightings <- data.list$sightings
  if('bsight' %in% names(data.list)) bsight <- data.list$bsight
  if('bsnap' %in% names(data.list)) bsnap <- data.list$bsnap

  all.times <- data.frame(GpsTime=effort$GpsTime,
                          GpsIndex=effort$GpsIndex,
                          Event=effort$Event,
                          Activity=effort$Activity,
                          Recorder=effort$Recorder)

  if('sightings' %in% names(data.list)) {
    dedicated <- which(sightings$SightingType=='Dedicated')
    all.times <- rbind(all.times,
                       data.frame(GpsTime=sightings$GpsTime[dedicated],
                                  GpsIndex=sightings$GpsIndex[dedicated],
                                  Event=rep('WhaleSight', length(dedicated)),
                                  Activity=sightings$SightingType[dedicated],
                                  Recorder=sightings$`Seen-By`[dedicated]))
  }

  if(include.birds) {
    if('bsight' %in% names(data.list)) {
      all.times <- rbind(all.times,
                         data.frame(GpsTime=bsight$GpsTime,
                         GpsIndex=bsight$GpsIndex,
                         Event=rep('BirdSight', nrow(bsight)),
                         Activity=rep('Scan', nrow(bsight)),
                         Recorder=bsight$`Seen-By`))

      all.times <- rbind(all.times,
                         data.frame(GpsTime=bsnap$GpsTime,
                                    GpsIndex=bsnap$GpsIndex,
                                    Event=rep('BirdSight', nrow(bsnap)),
                                    Activity=rep('Snapshot', nrow(bsnap)),
                                    Recorder=bsnap$`Seen-By`))
    }
  }

  all.times$Event <- factor(as.character(all.times$Event),
                            levels=c("SE", "End", "OR", "15", "WC",
                                     "WhaleSight", "BirdSight"))

  all.times <- all.times[order(all.times$GpsTime),]
  all.times$tDiff <- c(NA, as.numeric(difftime(all.times$GpsTime[-1],
                                               all.times$GpsTime[-nrow(all.times)], units='mins')))

  starts <- which(all.times$Event=='SE')
  ends <- which(all.times$Event=='End')

  starts <- sort(unique(c(starts, which(all.times$tDiff>tGap))))
  ends <- sort(unique(c(ends, which(all.times$tDiff>tGap)-1)))

  if(any(diff(starts)==1)) starts <- starts[-(which(diff(starts)==1)+1)]
  if(any(diff(ends)==1)) ends <- ends[-which(diff(ends)==1)]


  old.new <- data.frame(start=vector('logical'), end=vector('logical'))
  if(ends[1]==1) ends <- ends[-1]
  if(ends[1]<starts[1]) {
    starts <- c(1, starts)
  }

  all.times$real.start <- rep(F, nrow(all.times))
  all.times$real.end <- rep(F, nrow(all.times))

  all.times$real.start[starts] <- T
  all.times$real.end[ends] <- T

  both <- which(all.times$real.start & all.times$real.end)
  if(length(both)>0) {
    all.times$real.start[both] <- F
    all.times$real.end[both] <- F
    starts <- starts[-match(both, starts)]
    ends <- ends[-match(both, ends)]
  }

  or.starts <- which(all.times$Event[-1]=='SE' &
                       all.times$Event[-nrow(all.times)]=='OR'
                     & all.times$tDiff[-1]<tGap)+1

  if(length(or.starts)>0) {
    all.times$real.start[or.starts] <- F
    starts <- setdiff(starts, or.starts)
  }

  corrected <- F
  while(!corrected) {
    if(length(starts)>length(ends)) {
      reversed <- which(starts>ends[c(1:length(starts))])[1]
    } else {
      reversed <- which(starts[c(1:length(ends))]>ends)[1]
    }
    if(!is.na(reversed)) {
      rows <- c(ends[reversed-1]:starts[reversed])
      if(all.times$Event[rows[1]]=='End') {
        new.start <- rows[1]+1
        starts <- sort(c(starts, new.start))
        all.times$real.start[starts] <- T
        if(length(starts)==length(ends)) {
          if(all(starts>ends)) corrected <- T
        }
      } else {
        max.tDiff <- which.max(diff(as.numeric(all.times$GpsTime[rows])))+1
        new.start <- rows[max.tDiff]
        starts <- sort(c(starts, new.start))
        all.times$real.start[starts] <- T
        if(length(starts)==length(ends)) {
          if(all(starts>ends)) corrected <- T
        }
      }
    } else {
      corrected <- T
    }
  }

  if(length(starts)>length(ends)) {
    is.swapped <- any(starts[-1]<ends[c(1:(length(starts)-1))])
    swapped <- which(starts[-1]<ends[c(1:(length(starts)-1))])[1]
  } else {
    is.swapped <- any(starts[c(2:(length(starts)))]<ends[-length(ends)])
    swapped <- which(starts[c(2:(length(starts)))]<ends[-length(ends)])[1]
  }


    while(is.swapped) {
      ## LOOP NEVER BREAKS, SOMETHIG WRONG!!
      rows <- starts[swapped]:ends[swapped]
      unmatched.start <- length(which(all.times$real.start[rows]))>1
      unmatched.end <- length(which(all.times$real.end[rows]))>1

      if(unmatched.start) {
        unm.row <- rows[which(all.times$real.start[rows])[-1]]
        ## CHANGE TO ALLOW MORE THAN ONE UNMATCHED??
        if(all.times$tDiff[unm.row]<60) all.times$real.start[unm.row] <- F
        starts <- setdiff(starts, unm.row)
      } else {
        if(unmatched.end) {
          unm.row <- rows[which(all.times$real.end[rows])]
          unm.row <- unm.row[-length(unm.row)]
          ## CHANGE TO ALLOW MORE THAN ONE UNMATCHED??
          if(all.times$tDiff[unm.row+1]<60) all.times$real.end[unm.row] <- F
          ends <- setdiff(ends, unm.row)
        }
      }
      if(length(starts)>length(ends)) {
        is.swapped <- any(starts[-1]<ends[c(1:(length(starts)-1))])
        swapped <- which(starts[-1]<ends[c(1:(length(starts)-1))])[1]
      } else {
        is.swapped <- any(starts[c(2:(length(ends)))]<ends[-length(ends)])
        swapped <- which(starts[c(2:(length(ends)))]<ends[-length(ends)])[1]
      }
    }

  effort$start.end <- rep(NA, nrow(effort))

  for(i in which(all.times$real.start & all.times$Event!='SE')) {
    add.start <- match(all.times$GpsTime[i], effort$GpsTime)

    if(is.na(add.start)) {
      which.add <- match(all.times$GpsTime[i], sightings$GpsTime)
      new.start <- data.frame(GpsIndex=all.times$GpsIndex[i],
                              Index=NA,
                              GpsTime=all.times$GpsTime[i],
                              Vessel=effort$Vessel[1],
                              Event='newSE',
                              Activity=all.times$Activity[i],
                              Port=NA, Stbd=NA, Platform=NA,
                              Recorder=all.times$Recorder[i],
                              Weather=NA, Sightability=NA,
                              Visibility=NA, BeaufortSea=NA,
                              Swell=NA, Wind_Speed=NA,
                              Wind_Dir=NA, Glare=NA, GlareDirection=NA,
                              GlareWidth=NA, IceCover=NA, Comment='Added from sightings',
                              PCTime=all.times$GpsTime[i],
                              Latitude=sightings$Latitude[which.add],
                              Longitude=sightings$Longitude[which.add],
                              start.end='SE')
      effort <- rbind(effort, new.start)
    }
  }


  for(i in which(all.times$real.end & all.times$Event!='End')) {
    add.end <- match(all.times$GpsTime[i], effort$GpsTime)

    if(is.na(add.end)) {
      which.add <- match(all.times$GpsTime[i], sightings$GpsTime)
      new.end <- data.frame(GpsIndex=all.times$GpsIndex[i],
                            Index=NA,
                            GpsTime=all.times$GpsTime[i],
                            Vessel=effort$Vessel[1],
                            Event='newEnd',
                            Activity=all.times$Activity[i],
                            Port=NA, Stbd=NA, Platform=NA,
                            Recorder=all.times$Recorder[i],
                            Weather=NA, Sightability=NA,
                            Visibility=NA, BeaufortSea=NA,
                            Swell=NA, Wind_Speed=NA,
                            Wind_Dir=NA, Glare=NA, GlareDirection=NA,
                            GlareWidth=NA, IceCover=NA,
                            Comment='Added from sightings',
                            PCTime=all.times$GpsTime[i],
                            Latitude=sightings$Latitude[which.add],
                            Longitude=sightings$Longitude[which.add],
                            start.end='End')
      effort <- rbind(effort, new.end)
    }
  }

  effort$start.end[match(all.times$GpsTime[which(all.times$real.start)], effort$GpsTime)] <- 'SE'
  effort$start.end[match(all.times$GpsTime[which(all.times$real.end)], effort$GpsTime)] <- 'End'

  effort <- effort[order(effort$GpsTime),]

  gps$Sample.Label <- rep(NA, nrow(gps))
  sightings$Sample.Label <- rep(NA, nrow(sightings))

  for(i in 1:length(which(effort$start.end=='SE'))) {
    start <- effort$GpsTime[which(effort$start.end=='SE')[i]]
    end <- effort$GpsTime[which(effort$start.end=='End')[i]]
    which.gps <- which(gps$PCTime>=start & gps$PCTime<=end)
    which.sight <- which(sightings$PCTime>=start & sightings$PCTime<=end)
    gps$Sample.Label[which.gps] <- paste('T', i, sep='.')
    sightings$Sample.Label[which.sight] <- paste('T', i, sep='.')
  }

  if(plotType=='dygraph') {
    require(dygraphs)
    require(xts)

    ## Make dygraph
    dat.mat <- data.frame(matrix(rep(as.numeric(all.times$Event), nlevels(all.times$Event)), ncol=nlevels(all.times$Event)))
    names(dat.mat) <- levels(all.times$Event)
    for(i in 1:nlevels(all.times$Event)) {
      which.change <- which(dat.mat[,i]!=i)
      dat.mat[which.change,i] <- rep(NA, length(which.change))
    }
    xts.dat <- xts(dat.mat, order.by = all.times$GpsTime)
    dg <- dygraph(xts.dat) %>%
      dyOptions(strokeWidth=2, drawPoints=T, pointSize=10, pointShape='diamond',
                colors=brewer.pal(nlevels(all.times$Event), 'Dark2'),
                connectSeparatedPoints = T, drawGrid=F) %>%
      dyLegend(labelsSeparateLines = F, show='always', width = 400) %>%
      dyRangeSelector(fillColor='', strokeColor='')
    dg <- dyAxis(dg, 'x', drawGrid=T, valueRange=c(0, (nlevels(all.times$Event)+1)), axisLabelColor = 'white')
    dg <- dyAxis(dg, 'y', drawGrid=F, valueRange=c(0, (nlevels(all.times$Event)+1)), axisLabelColor = 'white')
    dg <- dyAxis(dg, 'y2', axisLabelColor = 'white')
    ##  axisLabelFormatter = 'function(d){return dg.toString.replace(/1/g, "S")}'

    for(i in 1:length(which(all.times$real.start))) {
      dg <- dyShading(dg, all.times$GpsTime[which(all.times$real.start)[i]],
                      all.times$GpsTime[which(all.times$real.end)[i]],
                      col='lightgreen')
      dg <- dyEvent(dg, all.times$GpsTime[which(all.times$real.start)[i]],
                    "", labelLoc = "bottom")
      dg <- dyEvent(dg, all.times$GpsTime[which(all.times$real.end)[i]],
                    "", labelLoc = "bottom")
    }
    saveWidget(dg, "dygraph.html", selfcontained=F)
    viewer <- getOption("viewer")
    viewer('dygraph.html')
  }

  if(plotType=='plotly') {
    require(plotly)
    require(ggthemes)
    theCols <- brewer.pal(nlevels(all.times$Event), 'Dark2')
    gp <- plot_ly(data = all.times, x = ~GpsTime, y = ~Event,
                  type='scatter', mode='markers',
                  color=~Event, colors=theCols, symbol=I('10'))
    shps <- list()
    for(i in 2:length(which(all.times$real.start))) {
      shps[[i]] <- list(type='rect',
                        fillcolor='darkgrey',
                        line = list(color = "darkgrey"),
                        opacity=0.3,
                        x0=all.times$GpsTime[which(all.times$real.end)[i-1]],
                        x1=all.times$GpsTime[which(all.times$real.start)[i]],
                        y0=0,
                        y1=nlevels(all.times$Event))
    }

    gp <- gp %>% layout(xaxis=list(rangeslider = list(type = "date")),
                        shapes=shps)
    gp <- hide_legend(gp)
    gp <- ggplotly(gp)
    saveWidget(gp, "plotly.html", selfcontained=F)
    viewer <- getOption("viewer")
    viewer('plotly.html')

  }
  data.list$effort <- effort
  data.list$sightings <- sightings
  data.list$gps <- gps[order(gps$PCTime),]
  data.list
}


