#' \code{readLoggerTable} Read table from Logger Access database
#'
#' @param theDB Name of database as defined in ODBC data sources
#' @param theTable Name of the table to read
#' @param to.spatial Logical indicating if a table containing GPS data
#' should be returned as a data frame (default) or as a SpatialPointsDataFrame
#' @param int Interval setting the downscaling of GPS data.
#' Read in every \code{int} line from data table.
#' @return Returns a data frame corresponding to the desired table.
#' @details The database must first be set up as a DSN data source.
#'   Note! On Windows, only 32-bit data sources can be used,
#'   and R (or RStudio) has to be run in 32-bit mode for these functions to work.
#' @family Logger sightings database functions
#' @seealso \code{\link{readLogger}} to read entire sightings database,
#'   \code{\link{correctEffort}} to correct inconsistencies in effort table,
#'   \code{\link{filterGPS}} to flag bad GPS positions,
#'   \code{\link{fillGPS}} to fill in missing GPS positions after filtering
#'   \code{\link{mkLeaflet}} to make interactive Leaflet map of sightings.
#'   \code{\link{convert2spatial}} to convert selected parts of sightings data list to shapefiles.
#' @author Martin Biuw
#' @example
#' gps <- readLoggerTable()
#' @importFrom RODBC odbcConnect sqlQuery odbcClose
#' @import rgdal
#' @import sp
#' @export

readLoggerTable <- function(theDB='Midnatsol_20191122', theTable='GpsData',
                       to.spatial=F, int=1) {
  require(RODBC)
  con <- odbcConnect(theDB)
  cat('Reading', theTable, 'data from', theDB, '(Can be time consuming for large datafiles....)\n')
  flush.console()
  qry <- sqlQuery(con, paste('SELECT * FROM', theTable), as.is=T)
  cat('\n\nDone!\n\n')
  ##if(theTable=='GpsData') {
  ##  int <- int/as.numeric(diff(qry$PCTime)[1])
  ##  qry <- qry[seq(1,nrow(qry),by=int),]
  ##  qry <- qry[order(qry$PCTime),]
  ##}
  odbcClose(con)
  if(to.spatial) {
    require(rgdal)
    coordinates(qry) <- c('Longitude', 'Latitude')
    proj4string(qry) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  }
  qry
}


#' \code{readLogger} Read all tables from Logger Access sightings database
#'
#' @param DB Name of database as defined in ODBC data sources
#' @param whales Logical, indicating if whale sightings table should be read
#' @param birds Logical, indicating if bird sightings tables should be read
#' @param spatial Logical indicating if a table containing GPS data
#' should be returned as a data frame (default) or as a SpatialPointsDataFrame
#' @param interval Interval setting the downscaling of GPS data.
#' Read in every \code{interval} line from data table.
#' @return Returns a list of data frames corresponding to desired tables in the database.
#' @details The database must first be set up as a DSN data source.
#'   Note! On Windows, only 32-bit data sources can be used,
#'   and R (or RStudio) has to be run in 32-bit mode for these functions to work.
#' @family Logger sightings database functions
#' @seealso \code{\link{readLoggerTable}} to read single table from sightings database,
#'   \code{\link{correctEffort}} to correct inconsistencies in effort table,
#'   \code{\link{filterGPS}} to flag bad GPS positions,
#'   \code{\link{fillGPS}} to fill in missing GPS positions after filtering
#'   \code{\link{mkLeaflet}} to make interactive Leaflet map of sightings.
#'   \code{\link{convert2spatial}} to convert selected parts of sightings data list to shapefiles.
#' @author Martin Biuw
#' @example
#' midnat <- readLogger()
#' @import RODBC
#' @import rgdal
#' @export

readLogger <- function(DB='Midnatsol_20191122', whales=T, birds=T,
                       spatial=F, interval=60) {
  gps <- readLoggerTable(theDB=DB, to.spatial = spatial)

  names(gps) <- gsub(' ', '_', names(gps))

  if((min(gps$Longitude, na.rm=T)==0 & min(gps$Latitude, na.rm=T)==0) |
     (max(gps$Longitude, na.rm=T)==0 & max(gps$Latitude, na.rm=T)==0) |
     (min(gps$Longitude, na.rm=T)==0 & max(gps$Latitude, na.rm=T)==0) |
     (max(gps$Longitude, na.rm=T)==0 & min(gps$Latitude, na.rm=T)==0)) {
    gps <- gps[-which(gps$Longitude==0 | gps$Latitude==0),]
  }

  gps <-gps[-which(duplicated(gps$PCTime)),]

  effort <- readLoggerTable(theDB=DB, theTable='Effort')
  names(effort) <- gsub(' ', '_', names(effort))

  effort <- merge(effort, gps[,match(c('Index', 'Latitude', 'PCTime', 'Longitude'), names(gps))],
                  by.x='GpsIndex', by.y='Index', all.x=T, all.y=F)


  effort$GpsTime <- as.POSIXct(strptime(effort$GpsTime, '%Y-%m-%d %H:%M:%S'), tz='UTC')
  effort$PCTime <- as.POSIXct(strptime(effort$PCTime, '%Y-%m-%d %H:%M:%S'), tz='UTC')
  effort <- effort[which(!is.na(effort$GpsTime)),]
  effort$GpsTime[which(format(effort$GpsTime, '%Y-%m-%d')=='1900-01-01')] <- NA
  gps$PCTime <- as.POSIXct(strptime(gps$PCTime, '%Y-%m-%d %H:%M:%S'), tz='UTC')

  gps <- gps[order(gps$PCTime),]

  if(any(is.na(effort$GpsTime))) {
    which.na <- which(is.na(effort$GpsTime))
    effort$GpsTime[which.na] <- effort$PCTime[which.na]
  }

  effort$Event <- as.character(effort$Event)
  effort$Event <- trimws(effort$Event)

  effort$Activity <- as.character(effort$Activity)
  effort$Activity <- trimws(effort$Activity)

  if(whales) {
    dsightings <- readLoggerTable(theDB=DB, theTable='Sightings_Dedicated')
    dsightings$SightingType <- rep('Dedicated', nrow(dsightings))
    isightings <- readLoggerTable(theDB=DB, theTable='Sightings_Incidental')
    isightings$SightingType <- rep('Incidental', nrow(isightings))
    rsightings <- readLoggerTable(theDB=DB, theTable='Sightings_Resights')
    rsightings$SightingType <- rep('Resights', nrow(rsightings))
    sightings <- plyr::rbind.fill(dsightings, isightings, rsightings)
    sightings <- merge(sightings, gps[,match(c('Index', 'Latitude', 'Longitude'), names(gps))],
          by.x='GpsIndex', by.y='Index', all.x=T, all.y=F)

    sp <- readLoggerTable(theDB=DB, theTable='Lookup')
    sp <- sp[which(sp$Topic=='Species'),]
    sightings$NumSP <- sightings$Species
    sightings$NumSP2 <- sightings$Species2

    spm <- match(sightings$Species, as.numeric(as.character(sp$Code)))
    sightings$Species <- sp$Text[spm]

    spm <- match(sightings$Species2, as.numeric(as.character(sp$Code)))
    sightings$Species2 <- sp$Text[spm]

    sightings$GpsTime <- as.POSIXct(strptime(sightings$GpsTime, '%Y-%m-%d %H:%M:%S'), tz='UTC')
    sightings$PCTime <- as.POSIXct(strptime(sightings$Time, '%Y-%m-%d %H:%M:%S'), tz='UTC')

    names(sightings) <- gsub(' ', '_', names(sightings))

    sightings$NumSP <- trimws(sightings$NumSP)
    sightings$NumSP2 <- trimws(sightings$NumSP2)
    lookup <- readLoggerTable(theTable='Lookup')

    sp.lookup <- lookup[which(lookup$Topic=='Species'),]
    sightings$Species <- sp.lookup$Text[match(sightings$NumSP, sp.lookup$Code)]
    sightings$Species2 <- sp.lookup$Text[match(sightings$NumSP2, sp.lookup$Code)]

    obs.lookup <- lookup[which(lookup$Topic=='Observer'),]
    sightings$'Seen-By' <- trimws(sightings$'Seen-By')
    sightings$'Seen-By' <- obs.lookup$Text[match(sightings$'Seen-By', obs.lookup$Code)]
  }


  if(birds){
    bsight <- readLoggerTable(theDB=DB, theTable='Sightings_Seabirds')
    bsnap <- try(readLoggerTable(theDB=DB, theTable='Sightings_SeabirdsSnapshot'), silent=T)

    names(bsight) <- gsub(' ', '_', names(bsight))
    names(bsnap) <- gsub(' ', '_', names(bsnap))

    if(class(bsight)=='data.frame') {
      bsight <- merge(bsight, gps[,match(c('Index', 'Latitude', 'Longitude'), names(gps))],
                         by.x='GpsIndex', by.y='Index', all.x=T, all.y=F)
      obs.lookup <- lookup[which(lookup$Topic=='Observer'),]
      bsight$'Seen-By' <- trimws(bsight$'Seen-By')
      bsight$'Seen-By' <- obs.lookup$Text[match(bsight$'Seen-By', obs.lookup$Code)]

    }

    if(class(bsnap)=='data.frame') {
      bsnap <- merge(bsnap, gps[,match(c('Index', 'Latitude', 'Longitude'), names(gps))],
                         by.x='GpsIndex', by.y='Index', all.x=T, all.y=F)
      obs.lookup <- lookup[which(lookup$Topic=='Observer'),]
      bsnap$'Seen-By' <- trimws(bsnap$'Seen-By')
      bsnap$'Seen-By' <- obs.lookup$Text[match(bsnap$'Seen-By', obs.lookup$Code)]
    }

    b.lookup <- readLoggerTable(DB, 'UDF_Sightings_Seabirds')
    species <- setdiff(unique(c(names(bsight), names(bsnap))),
                                        c("GpsIndex", "Index", "GpsTime", "SightingNo", "Time", "Seen-By",
                                          "Seen-From", "ButtonRef", "Comments", "Latitude", "Longitude"))
   common <- b.lookup$Title[match(species, b.lookup$DbTitle)]
   if(any(is.na(common))) {
     common[which(is.na(common))] <- unlist(lapply(species[which(is.na(common))], function(x) unlist(strsplit(x, '-'))[2]))
   }
   common <- gsub(' / ', '/', common)
   common <- gsub('S. ', 'Southern ', common)
   common <- gsub('S ', 'Southern ', common)
   common <- gsub('Black brow', 'Black-browed', common)

   names(bsight)[match(c("PT-Stormzy", "PT_Diving", "PT_Other", "PG-Linux", "PG-BigMac", "FL-South",
                    "PRI-Skua", "PRI-KelpGull", "PRI-Shag", "PRI-Tern",
                    "PRI-Shearwater"), names(bsight))] <- c('PT-Storm', 'PT-Diving', 'PT-Other', 'PG-Gentoo',
                                                       'PG-Mac', 'PRO-Fulmar',
                                                       'ST-Skua', 'LA-KelpGull',
                                                       'PH-Shag', 'LA-Tern', 'PRO-Shearwater')

   if(class(bsnap)=='data.frame') {
     names(bsnap)[match(c("PT-Stormzy", "PT_Diving", "PT_Other", "PG-Linux", "PG-BigMac", "FL-South",
                          "PRI-Skua", "PRI-KelpGull", "PRI-Shag", "PRI-Tern",
                           "PRI-Shearwater"), names(bsnap))] <- c('PT-Storm', 'PT-Diving', 'PT-Other', 'PG-Gentoo',
                                                                   'PG-Mac', 'PRO-Fulmar',
                                                                   'ST-Skua', 'LA-KelpGull',
                                                                   'PH-Shag', 'LA-Tern', 'PRO-Shearwater')
   }

   species[match(c("PT-Stormzy", "PT_Diving", "PT_Other", "PG-Linux", "PG-BigMac", "FL-South",
                         "PRI-Skua", "PRI-KelpGull", "PRI-Shag", "PRI-Tern",
                         "PRI-Shearwater"), species)] <- c('PT-Storm', 'PT-Diving', 'PT-Other', 'PG-Gentoo',
                                                                 'PG-Mac', 'PRO-Fulmar',
                                                                 'ST-Skua', 'LA-KelpGull',
                                                                 'PH-Shag', 'LA-Tern', 'PRO-Shearwater')
   common <- paste(common, unlist(lapply(species, function(x) unlist(strsplit(unlist(strsplit(x, '-'))[1], '_'))[1])))

   common <- gsub('AL', 'albatross', common)
   common <- gsub(' PRO', '', common)
   common <- gsub('PT', 'petrel', common)
   common <- gsub('PRI', 'prion', common)
   common <- gsub('PG', 'penguin', common)
   common <- gsub('ST', 'Spp', common)
   common <- gsub('KelpGull LA', 'Kelp gull', common)
   common <- gsub('PrionSpp prion', 'Prion Spp', common)
   common <- gsub('PH', 'Spp', common)
   common <- gsub('LA', 'Spp', common)

   group <- species
   group[grep('AL', species)] <- 'Albatrosses'
   group[grep('PRO', species)] <- 'Fulmars/Shearwaters'
   group[grep('PT', species)] <- 'Petrels'
   group[grep('PRI', species)] <- 'Prions'
   group[grep('PG', species)] <- 'Penguins'
   group[grep('ST', species)] <- 'Skuas'
   group[grep('LA', species)] <- 'Gulls/Terns'
   group[grep('PH', species)] <- 'Shags/Cormorants'
   group[grep('PT-Giant', species)] <- 'Giant petrels'

   birdSpecies <- data.frame(Code=species, CommonName=common, stringsAsFactors=F)

  }

  int <- interval/as.numeric(diff(gps$PCTime)[1])
  gps <- gps[seq(1,nrow(gps),by=int),]

  if(whales & !birds) {
    list(gps=gps, effort=effort, sightings=sightings)
  }

  if(birds & !whales) {
    list(gps=gps, effort=effort, sightings=bsight, snapshot=bsnap, birdSpecies=birdSpecies)
  }

  if(birds & whales) {
    list(gps=gps, effort=effort, sightings=sightings, bsight=bsight, snapshot=bsnap, birdSpecies=birdSpecies)
  }
}


