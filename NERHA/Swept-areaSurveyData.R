#Data pull for Chris Haak/NRHA (June 4, 2024):
#C:\Users\laurel.smith\Documents\EDAB\NERHA
#Swept-areaSurveyData.R uploaded to https://github.com/NOAA-EDAB/data-requests
out.dir="output"

source(("C:\\Users\\laurel.smith\\Documents\\EDAB\\ConditionGAM\\R\\ConnectOracle.R"))
conn <- dbConnect(drv,username=user,password=passwd, dbname=connect.string)
#Above gives error about target host or object does not exist if not on WH network or VPN

#Old school SQL data retrieval:
#If you include TOW_EVALUATION, it limits data to starting in 2008
qry <- c(
  "select b.cruise6,b.stratum,b.tow,b.station,
  s.est_year year,season, est_month month,est_day day,
  substr(est_time,1,2)||substr(est_time,4,2) time,
  round(substr(beglat,1,2) + (substr(beglat,3,7)/60),6) beglat,  
  round(substr(endlat,1,2) + (substr(endlat,3,7)/60),6) endlat,
  round(((substr(beglon,1,2) + (substr(beglon,3,7)/60)) * -1), 6) beglon,
  round(((substr(endlon,1,2) + (substr(endlon,3,7)/60)) * -1), 6) endlon,
  towdur, setdepth, surftemp, bottemp,
  b.svspp, b.catchsex, expcatchwt, expcatchnum, length, expnumlen, MEAN_SOG_KNOTS, MEAN_DOOR_SPRD_METERS, MEAN_WING_SPRD_METERS, MEAN_HEIGHT_METERS
  
  from svdbs.UNION_FSCS_SVLEN b, svdbs.UNION_FSCS_SVCAT p, svdbs.UNION_FSCS_SVSTA s, svdbs.mstr_cruise c, svdbs.TOW_EVALUATION e
  where
  (b.cruise6=s.cruise6) and
  (c.cruise6=b.cruise6) and
  (p.cruise6=c.cruise6) and
  (s.cruise6=e.cruise6) and
  (p.stratum=b.stratum) and
  (b.stratum=s.stratum) and
  (p.station=b.station) and
  (b.station=s.station) and
  (s.station=e.station) and
  (p.tow=b.tow) and
  (b.tow=s.tow) and
  (p.svspp=b.svspp) and
  (p.catchsex=b.catchsex) and
  purpose_code = 10 and
  year >=1963
  order by year, cruise6, station, svspp, catchsex, length "
)

#removed species selection from SQL:
# b.svspp in ('013','015','023','026','028','032','072','073','074','075','076','077','078','102','103','104','105','106','107','108','121','131','135','141','143','145','155','164','193','197') and


survey <- DBI::dbGetQuery(channel, qry)

dateOfPull <- format(Sys.time(), "%m-%d-%y")
saveRDS(survey,file = here::here(out.dir,paste0("NEFSC_survey_data_sweptarea",dateOfPull,".rds")))


#********************************************
#Swept-area from Sean, didn't complete this:

#Sean's data:
surveySwept <- load("C:\\Users\\laurel.smith\\Documents\\EDAB\\NERHA\\NRHA_survdat_OCT_23.RData")
View(surveySwept)
View(nrha.survdat)

View(nrha.survdat[["survdat"]])

#without data.table:
cruise <- DBI::dbGetQuery(channel, (cruise.qry <- paste0("select unique year, cruise6, svvessel, season 
                                                         from svdbs.mstr_cruise
                                                         where purpose_code = 10
                                                         order by year, cruise6")))

cruise.qry <- paste0("select unique year, cruise6, svvessel, season
      from svdbs.mstr_cruise
      where purpose_code = 10
      and year ", years,
                     "order by year, cruise6")

cruise <- data.table::as.data.table(DBI::dbGetQuery(channel, cruise.qry))
cruise <- na.omit(cruise)
data.table::setkey(cruise, CRUISE6, SVVESSEL)

#Use cruise codes to select other data
cruise6 <- survdat:::sqltext(cruise$CRUISE6)

#Station data
station.qry <- paste0("select unique cruise6, svvessel, station, stratum, tow,
                         decdeg_beglat as lat, decdeg_beglon as lon,
                         begin_est_towdate as est_towdate, avgdepth as depth,
                         surftemp, surfsalin, bottemp, botsalin
                         from svdbs.UNION_FSCS_SVSTA
                         where cruise6 in (", cruise6, ")
                         order by cruise6, station")

# pull data
station <- data.table::as.data.table(DBI::dbGetQuery(channel, station.qry))

data.table::setkey(station, CRUISE6, SVVESSEL)
#merge cruise and station
survdat <- merge(cruise, station)

#Catch data
catch.qry <- paste0("select cruise6, station, stratum, tow, svspp, catchsex,
                     expcatchnum as abundance, expcatchwt as biomass
                     from svdbs.UNION_FSCS_SVCAT
                     where cruise6 in (", cruise6, ")
                     and stratum not like 'YT%'
                     order by cruise6, station, svspp")

catch <- data.table::as.data.table(DBI::dbGetQuery(channel, catch.qry))
data.table::setkey(catch, CRUISE6, STATION, STRATUM, TOW)

#merge with survdat
data.table::setkey(survdat, CRUISE6, STATION, STRATUM, TOW)
survdat <- merge(survdat, catch, by = data.table::key(survdat))

#Length data
length.qry <- paste0("select cruise6, station, stratum, tow, svspp, catchsex,
                      length, expnumlen as numlen
                      from svdbs.UNION_FSCS_SVLEN
                      where cruise6 in (", cruise6, ")
                      and stratum not like 'YT%'
                      order by cruise6, station, svspp, length")

len <- data.table::as.data.table(DBI::dbGetQuery(channel, length.qry))
data.table::setkey(len, CRUISE6, STATION, STRATUM, TOW, SVSPP, CATCHSEX)

#merge with survdat
data.table::setkey(survdat, CRUISE6, STATION, STRATUM, TOW, SVSPP, CATCHSEX)
survdat <- merge(survdat, len, all.x = T)

#Biology data
# bio.qry <- paste0("select cruise6, station, stratum, svspp, catchsex, length, indid,
#                   indwt, sex, maturity, age, stom_volume, stom_wgt
#                   from svdbs.UNION_FSCS_SVBIO
#                   where cruise6 in (", cruise6, ")")



