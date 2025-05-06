#Scallop data pull for Chris Haak/EFH (May 6, 2025):
#Swept-areaSurveyData-Scallops.R uploaded to NERHA folder in: https://github.com/NOAA-EDAB/data-requests

out.dir="C:\\Users\\laurel.smith\\Documents\\EDAB\\NERHA\\output"

source(("C:\\Users\\laurel.smith\\Documents\\EDAB\\ConditionGAM\\R\\ConnectOracle.R"))
conn <- dbConnect(drv,username=user,password=passwd, dbname=connect.string)
#Above gives error about target host or object does not exist if not on WH network or VPN

#Old school SQL data retrieval:
#Doppler distance should be in both Albatross and Bigelow time series:
qry <- c(
  "select b.cruise6,b.stratum,b.tow,b.station,
  s.est_year year,season, est_month month,est_day day,
  substr(est_time,1,2)||substr(est_time,4,2) time,
  round(substr(beglat,1,2) + (substr(beglat,3,7)/60),6) beglat,  
  round(substr(endlat,1,2) + (substr(endlat,3,7)/60),6) endlat,
  round(((substr(beglon,1,2) + (substr(beglon,3,7)/60)) * -1), 6) beglon,
  round(((substr(endlon,1,2) + (substr(endlon,3,7)/60)) * -1), 6) endlon,
  towdur, setdepth, enddepth, mindepth, maxdepth, avgdepth,
  surftemp, bottemp, surfsalin, botsalin, 
  b.svspp, b.catchsex, expcatchwt as biomass, expcatchnum as abundance, length, expnumlen as numlen, s.svvessel, dopdistb
  
  from svdbs.UNION_FSCS_SVLEN b, svdbs.UNION_FSCS_SVCAT p, svdbs.UNION_FSCS_SVSTA s, svdbs.mstr_cruise c
  where
  (b.cruise6=s.cruise6) and
  (c.cruise6=b.cruise6) and
  (p.cruise6=c.cruise6) and
  (p.stratum=b.stratum) and
  (b.stratum=s.stratum) and
  (p.station=b.station) and
  (b.station=s.station) and
  (p.tow=b.tow) and
  (b.tow=s.tow) and
  (c.svvessel=s.svvessel) and
  (p.svspp=b.svspp) and
  (p.catchsex=b.catchsex) and 
  year >=1963 and 
  purpose_code = 60 and
  (SHG <= 136)
  order by year, cruise6, station, svspp, catchsex, length "
)

survey <- DBI::dbGetQuery(channel, qry)

dateOfPull <- format(Sys.time(), "%m-%d-%y")
saveRDS(survey,file = here::here(out.dir,paste0("NEFSC_Scallop_Survey_data",dateOfPull,".rds")))