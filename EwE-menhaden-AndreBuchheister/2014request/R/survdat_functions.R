#'Post stratify NEFSC survey data
#'
#'Uses a shapefile to post stratify survey data collected
#'during the NEFSC surveys
#'
#'@family Survdat
#'
#'@param survdat NEFSC survey data generated by Survdat.R.
#'@param stratum R object containing the shapefile.
#'@param strata.col Column name from stratum that contains the strata designations.
#'@param na.keep Logical value to indicate whether original strata names should be retained.
#'
#'@return Returns a survdat object with new strata designations labeled as 'newstrata'.
#'@import data.table
#'@import rgdal
#'@export
poststrat <- function (survdat, stratum, strata.col = 'EPU', na.keep = F) {
  if (!requireNamespace("rgdal", quietly = TRUE)) {
    stop("rgdal needed for this function to work. Please install it.",
         call. = FALSE)
  }

  #Data
  x <- copy(survdat)

  #Use only station data
  setkey(x, CRUISE6, STRATUM, STATION)
  stations <- unique(x, by = key(x))
  stations <- stations[, list(CRUISE6, STRATUM, STATION, LAT, LON)]

  #Convert to spatial points data frame
  coordinates(stations) <- ~LON+LAT
  stations@proj4string  <- CRS('+init=epsg:4326') #Lat/Lon code
  lcc <- CRS("+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ") #Lambert Conformal Conic
  stations <- spTransform(stations, lcc)

  #Identify tows within new strata
  stratum     <- spTransform(stratum, lcc)
  names(stratum@data)[which(names(stratum@data) == strata.col)] <- "strata.col"
  stations$newstrata <- over(stations, stratum)[ ,'strata.col']
  names(stratum@data)[which(names(stratum@data) == "strata.col")] <- strata.col

  #Output data (convert spatial data frame back to lat/lon)
  stations <- spTransform(stations, CRS('+init=epsg:4326'))
  sta.data <- as.data.table(as.data.frame(stations))
  if(na.keep == F) sta.data <- sta.data[!is.na(newstrata), ]
  sta.data[, c('LAT', 'LON') := NULL]
  out <- merge(x, sta.data, by = c('CRUISE6', 'STRATUM', 'STATION'))

  return(out)
}

#'Generate a table of stratum areas
#'
#'Calculates the area of a stratum from a shapefile.  Necessary if post stratifying
#'a survdat file.
#'
#'@family Survdat
#'
#'@param stratum Name of the R object containing the shapefile.
#'@param strata.col Column name from stratum that contains the strata designations.
#'
#'@return Returns a table of areas in square kilometers.
#'@import data.table
#'@import rgdal
#'@export
getarea <- function(stratum, strat.col){
  #Get stratum areas
  lcc <- sp::CRS("+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ") #Lambert Conformal Conic

  strat <- sp::spTransform(stratum, lcc)

  #Calculate areas
  names(strat@data)[which(names(strat@data) == strat.col)] <- "STRAT"
  strata.A <- data.table(STRAT = slot(strat, "data")$STRAT, Area = sapply(slot(strat, "polygons"), slot, "area")/1e6)

  setnames(strata.A, "STRAT", strat.col)

  return(strata.A)
}

#'Prepare survey data for calculating stratified mean
#'
#'Calculates the number of tows per strata and strata weight.  Run before
#'subsetting the data by species of interest to ensure all stations are accounted.
#'
#'@family Survdat
#'
#'@param survdat NEFSC survey data generated by Survdat.R.
#'@param areas R object with stratum areas.  Can be generated by getarea().
#'@param strata.col Column name containing strata designations.  Must be the same for
#'survdat and areas.
#'@param area.col Column name containing the stratum areas in the areas object.
#'
#'@return Returns a survdat object with added columns for the number of tows (ntows) and
#'stratum weights (W.h).
#'@import data.table
#'@export
stratprep <- function (survdat, areas, strat.col, area.col = 'Area') {
  x <- copy(survdat)
  y <- copy(areas)

  setnames(x, strat.col, 'STRAT')
  setnames(y, c(strat.col, area.col),
           c('STRAT',   'S.AREA'))
  if(strat.col != 'STRATUM'){
    x[, STATION2 := as.numeric(paste0(STRATUM, STATION))][]
    setnames(x, c('STATION', 'STATION2'), c('ORIGSTATION', 'STATION'))
  }

  #Station data - remove catch/length
  setkey(x, CRUISE6, STRAT, STATION)
  stations <- unique(x, by = key(x))
  stations <- stations[, list(YEAR, CRUISE6, STRAT, STATION)]

  #count stations
  setkey(stations, YEAR, STRAT)
  stations[, ntows := length(STATION), by = key(stations)]

  #Merge stations and area
  stations <- merge(stations, y, by = 'STRAT', all.x = T)

  #Calculate stratum weight
  setkey(stations, 'YEAR', 'STRAT')
  strat.year <- unique(stations, by = key(stations))
  strat.year[, c('CRUISE6', 'STATION', 'ntows') := NULL]
  strat.year[, W.h := S.AREA / sum(S.AREA, na.rm = T), by = YEAR]
  strat.year[is.na(W.h), W.h := 0]
  strat.year[, S.AREA := NULL]

  #Merge back
  stations <- merge(stations, strat.year, by = key(stations))

  #Merge catch with station data
  strat.survdat <- merge(x, stations, by = c('YEAR', 'CRUISE6', 'STRAT', 'STATION'))

  setnames(strat.survdat, c('STRAT', 'S.AREA'),
           c(strat.col, area.col))

  if(strat.col != 'STRATUM'){
    setnames(strat.survdat, c('STATION', 'ORIGSTATION'), c('STATION2', 'STATION'))
    strat.survdat[, STATION2 := NULL]
  }

  return(strat.survdat)
}

#'Calculate stratified mean biomass and abundance
#'
#'Calculates the stratified mean biomass and abundance.  Also calculates the associated
#'variance and standard error.  Requires you to run stratprep() to survdat object beforehand.
#'
#'@family Survdat
#'
#'@param survdat NEFSC survey data generated by Survdat.R and modified by stratprep().
#'@param groups Specify a particular group you want.  The default "all" will calculate
#'means for all groups in the group.col.
#'@param merge.sex Logical value to merge sexed species such as dogfish.
#'@param sex.col Column of survdat containing the sex of the group.  If merge.sex is true,
#'this parameter is ignored.
#'@param group.col Column of survdat upon which the means are based (i.e. SVSPP).
#'@param strata.col Column of survdat containing strata designations.
#'@param poststrat Logical value indicating whether the original strata design was
#'used or not.  Changes the calculation for variance.
#'@param nsta.col Column of survdat containing the number of stations per strata.
#'@param area.wgt Column of survdat containing the strata weights.
#'@param weight Column of survdat containing the biomass per station.
#'@param number Column of survdat containing the abundance per station.
#'
#'@return Returns a table with stratified mean biomass and abundance for each group
#'indicated by the group.col parameter.  In addition, the variance and standard error
#'for both means are provided.
#'@import data.table
#'@export
stratmean <- function (survdat, groups = 'all', group.col = 'SVSPP',
                       merge.sex = T, sex.col = 'CATCHSEX',
                       strat.col = 'STRATUM', poststrat = F, nsta.col = 'ntows',
                       area.wgt = 'W.h', weight = 'BIOMASS', number = 'ABUNDANCE') {
  x <- copy(survdat)

  #Remove length data if present
  setkey(x, CRUISE6, STRATUM, STATION, SVSPP, CATCHSEX)
  x <- unique(x, by = key(x))
  x[, c('LENGTH', 'NUMLEN') := NULL]

  setnames(x, c(group.col, sex.col, strat.col, nsta.col, area.wgt, weight, number),
           c('group', 'sex', 'strat', 'ntows', 'W.h', 'BIO', 'NUM'))

  #Merge sex or keep seperate
  if(merge.sex == F) x[, group := paste(group, sex, sep = '')]

  setkey(x, CRUISE6, strat, STATION, group)
  x[, BIO := sum(BIO), by = key(x)]
  x[, NUM := sum(NUM), by = key(x)]
  x <- unique(x, by = key(x))

  #Fix Na's
  x[is.na(BIO), BIO := 0]
  x[is.na(NUM), NUM := 0]

  #Calculate total number of stations per year
  setkey(x, strat, YEAR)
  N <- unique(x, by = key(x))
  N <- N[, sum(ntows), by = 'YEAR']
  setnames(N, 'V1', 'N')

  #Subset data if necessary
  if(groups[1] != 'all'){
    if(merge.sex == F) groups <- c(paste(groups, 0, sep = ''), paste(groups, 1, sep = ''),
                                   paste(groups, 2, sep = ''), paste(groups, 3, sep = ''))
    x <- x[group %in% groups, ]
  }

  #Calculate weight per tow and number per tow
  setkey(x, group, strat, YEAR)

  x[, biomass.tow   := sum(BIO) / ntows, by = key(x)]
  x[, abundance.tow := sum(NUM) / ntows, by = key(x)]

  #Calculated stratified means
  x[, weighted.biomass   := biomass.tow   * W.h]
  x[, weighted.abundance := abundance.tow * W.h]

  #Variance - need to account for zero catch
  x[, n.zero     := ntows - length(BIO), by = key(x)]

  x[, zero.var.b := n.zero * (0 - biomass.tow)^2]
  x[, vari.b := (BIO - biomass.tow)^2]
  x[, Sh.2.b := (zero.var.b + sum(vari.b)) / (ntows - 1), by = key(x)]
  x[is.nan(Sh.2.b), Sh.2.b := 0]

  x[, zero.var.a := n.zero * (0 - abundance.tow)^2]
  x[, vari.a := (NUM - abundance.tow)^2]
  x[, Sh.2.a := (zero.var.a + sum(vari.a)) / (ntows - 1), by = key(x)]
  x[is.nan(Sh.2.a), Sh.2.a := 0]

  stratified <- unique(x, by = key(x))

  stratified <- merge(stratified, N, by = 'YEAR')

  #Stratified mean
  setkey(stratified, group, YEAR)

  stratified[, strat.biomass := sum(weighted.biomass),   by = key(stratified)]
  stratified[, strat.abund   := sum(weighted.abundance), by = key(stratified)]

  #Stratified variance
  if(poststrat == F){
    stratified[, biomass.var := sum(((W.h^2) * Sh.2.b) / ntows), by = key(stratified)]
    stratified[, abund.var   := sum(((W.h^2) * Sh.2.a) / ntows), by = key(stratified)]
  }

  if(poststrat == T){
    stratified[, biomass.var := sum(Sh.2.b * W.h) / N + sum((1 - W.h) * Sh.2.b) / N^2, by = key(stratified)]
    stratified[, abund.var   := sum(Sh.2.a * W.h) / N + sum((1 - W.h) * Sh.2.a) / N^2, by = key(stratified)]

  }

  #standard error of the means
  stratified[, biomass.SE := sqrt(biomass.var), by = key(stratified)]
  stratified[, abund.SE   := sqrt(abund.var),   by = key(stratified)]

  #Delete extra rows/columns
  stratified.means <- unique(stratified, by = key(stratified))
  stratified.means <- stratified.means[, list(YEAR, group, sex, N, strat.biomass, biomass.var, biomass.SE,
                                              strat.abund, abund.var, abund.SE)]
  if(merge.sex == T) stratified.means[, sex := NULL]

  if(merge.sex == F){
    stratified.means[, glen := nchar(group)]
    for(i in 2:4){
      stratified.means[glen == i, group := as.numeric(substr(group, 1, i - 1))]
    }
    stratified.means[, glen := NULL]
    setkey(stratified.means, YEAR, SVSPP, sex)
  }

  setnames(stratified.means, 'group', group.col)

  return(stratified.means)
}

#'Calculate the estimate of total biomass and abundance
#'
#'Calculates the estimate of total biomass and abundance using the swept area.
#'Variance of the estimate is also calculated.  Requires the outputs from both
#'the stratprep and stratmean functions.
#'
#'@family Survdat
#'
#'@param survdat NEFSC survey data generated by Survdat.R and modified by stratprep().
#'@param stratmean Output from stratmean().
#'@param q Table of survey catchability with a column of group names and a column of
#'catchabilities.  If not provided, assumes a q of 1 for each group (Minimum swept area
#'estimates).
#'@param a The average swept area of the trawl.  Default value is the swept area of a
#'standard NOAA Ship Albatross IV tow.
#'@param strat.col Column of survdat containing strata designations.
#'@param area.col Column of survdat containing the stratum areas.
#'@param group.col Column of survdat upon which the totals are based (i.e. SVSPP).
#'
#'@return Returns a table with the estimates of total biomass and abundance as well as
#'the stratified mean biomass and abundance for each group indicated by the group.col
#'parameter.  In addition, the variance of the totals and variance and standard error
#'for both means are provided.
#'@import data.table
#'@export
sweptarea <- function (survdat, stratmean, q = NULL, a = 0.0384, strat.col, area.col,
                       group.col = 'SVSPP') {
  #This is necessary to break the link with the original data table
  stratprep.x  <- copy(survdat)
  stratmean.x  <- copy(stratmean)

  #Calculate A (Total area)
  setnames(stratprep.x, c(strat.col, area.col),
           c('STRAT', 'S.AREA'))

  setkey(stratprep.x, YEAR, STRAT)
  stratum <- unique(stratprep.x, by = key(stratprep.x))
  stratum <- stratum[, sum(S.AREA, na.rm = T), by = 'YEAR']
  setnames(stratum, "V1", "A")

  #Merge A
  swept.area <- merge(stratmean.x, stratum, by = 'YEAR')

  #Merge q
  if(is.null(q)) q <- data.table(groups = unique(swept.area[, get(group.col)]), q = 1)
  setnames(q, names(q), c(group.col, 'q'))
  swept.area <- merge(swept.area, q, by = group.col, all.x = T)
  swept.area[is.na(q), q := 1]

  #Calculate swept area biomass
  swept.area[, tot.biomass   :=       (strat.biomass * A/a)/q]
  swept.area[, tot.abundance := round((strat.abund   * A/a)/q)]

  #Calculate variance
  swept.area[, var.constant := (A/a)/q]
  swept.area[, tot.bio.var   := var.constant^2 * biomass.var]
  swept.area[, tot.abund.var := var.constant^2 * abund.var]

  #Calculate standard error
  swept.area[, tot.bio.SE   := sqrt(tot.bio.var)]
  swept.area[, tot.abund.SE := sqrt(tot.abund.var)]

  #remove extra columns - need to add sex column if stratmean object does not have one
  #then remove before output
  if(length(which(names(stratmean.x) == 'sex')) == 0) swept.area[, sex := 0]
  swept.area <- swept.area[, list(YEAR, get(group.col), sex, N,
                                  strat.biomass, biomass.var,   biomass.SE,
                                  strat.abund,   abund.var,     abund.SE,
                                  tot.biomass,   tot.bio.var,   tot.bio.SE,
                                  tot.abundance, tot.abund.var, tot.abund.SE)]
  setnames(swept.area, 'V2', group.col)
  if(length(which(names(stratmean.x) == 'sex')) == 0) swept.area[, sex := NULL]

  return(swept.area)
}
