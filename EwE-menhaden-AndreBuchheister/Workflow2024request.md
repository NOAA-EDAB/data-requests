Workflow notes
================
Sarah Gaichas
2024-05-17

# Preliminary analysis steps

1.  What did Sean do? Find old scripts (best guess is
    `Andre_data_pulls_v2.R` in 2014request folder)
2.  Compare inputs, 2014 `survdat_lw.RData` which did not use current
    `survdat` package vs today’s `survdat_lw.rds`
3.  If inputs look similar, run old script and see how far we get

## 1. What did Sean do?

<details>
<summary>
Old script (we think, there was also one called `Andre_data_pulls.R` in
a different folder)
</summary>

``` r
#Andre_data_pull.r
#swept area biomass estimates for each fish box
#8/14
#SML

#User parameters
#User parameters
if(Sys.info()['sysname']=="Windows"){
  data.dir <- "L:\\EcoAP\\Data\\survey"
  out.dir  <- "L:\\EcoAP\\Data\\survey"
  gis.dir  <- "L:\\Rworkspace\\GIS_files"
  memory.limit(4000)
}
if(Sys.info()['sysname']=="Linux"){
  data.dir <- '/home/slucey/slucey/EcoAP/Data/survey'
  out.dir  <- '/home/slucey/slucey/EcoAP/Data/survey'
  gis.dir  <- '/home/slucey/slucey/Rworkspace/GIS_files'
}

#-------------------------------------------------------------------------------
#Required packages
library(data.table); library(RODBC); library(rgdal); library(Survdat)

#-------------------------------------------------------------------------------
count <- function(x){
    num <- rep(1, length(x))
    out <- sum(num)
    return(out)
    }

#-------------------------------------------------------------------------------
#Grab survdat.r
load(file.path(data.dir, 'survdat_lw.RData'))

#Grab strata
strata <- readOGR(gis.dir, 'strata')

#Stratas
#Andre wants MAB, SNE, GB, and GOM from EMAX work with depth bins of
#<27m, 27m - 55m, and >55m
regions <- c('mab.shallow', 'mab.mid', 'mab.deep', 'sne.shallow', 'sne.mid', 'sne.deep',
                            'gb.mid',  'gb.deep',  'gom.shallow', 'gom.mid', 'gom.deep')

mab.shallow <- c(03150:03440) 
mab.mid     <- c(01610, 01650, 01690, 01730)
mab.deep    <- c(01620:01640, 01660:01680, 01700:01720, 01740:01760)
sne.shallow <- c(03010:03140, 03450:03560)
sne.mid     <- c(01010, 01050, 01090)
sne.deep    <- c(01020:01040, 01060:01080, 01100:01120)
gb.mid      <- c(01190, 01200, 01250)
gb.deep     <- c(01130:01180, 01210:01230)
gom.shallow <- c(03570:03590, 03620:03640, 03670, 03680, 03700, 03710, 03730, 
                 03740, 03760, 03790, 03820, 03840, 03860, 03880)
gom.mid     <- c(03600, 03610, 03650, 03660, 03690, 03720, 03750, 03770, 03780,
                 03800, 03810, 03830, 03850, 03870, 03890)
gom.deep    <- c(03900, 01240, 01260:01300, 01360:01400)

#Add ecoregion designation
eco.info <- as.data.table(strata@data)
for(ireg in 1:length(regions)) eco.info[FINSTR_ID %in% get(regions[ireg]), Eco := regions[ireg]]
strata@data <- eco.info

#Generate area table
strat.area <- getarea(strata, 'Eco')
strat.area <- strat.area[, sum(Area), by = 'Eco']
setnames(strat.area, 'V1', 'Area')

save(strat.area, file = file.path(out.dir, 'Andre_regions.RData'))

#Build species list
load(file.path(data.dir, 'Species_codes.RData'))
andre.spp <- as.data.table(read.csv(file.path(out.dir, "Andre_groups.csv")))
spp <- merge(unique(spp[, list(SVSPP, Fall.q, Spring.q)]), andre.spp, by = 'SVSPP', all.x = T)
spp <- spp[!is.na(EWE), ]

save(spp, file = file.path(out.dir, 'Buchheister_spp.RData'))


#Post stratify
andre <- poststrat(survdat.lw, strata, strata.col = 'Eco')
setnames(andre, 'newstrata', 'Region')
andre <- andre[!is.na(Region), ]

#Segregate by season
#NA for now

#Set up for stratification
#Break size structured groups
andre[SVSPP %in% c(15, 36, 69, 72, 73, 76:78, 103, 105, 135, 139, 145), MULTI := T]
andre[is.na(MULTI), MULTI := F] 

single.box <- andre[MULTI == F, ]

multi.box  <- andre[MULTI == T, ]

#size group designation
#For reference: Spiny Dogish = 15
#               Atl Menhaden = 36
#               Atl Cod = 73
#               Summer Flounder = 103
#               Yellowtail Flounder = 105
#               Bluefish = 135
#               Striped Bass = 139
#               Weakfish = 145
#               Hakes (Offshore, Silver, White, Red, Spotted) = 69, 72, 76, 77, 78
multi.box[SVSPP == 15  & LENGTH <= 60, SIZE := 'SMALL']
multi.box[SVSPP == 15  & LENGTH >  60, SIZE := 'LARGE']
multi.box[SVSPP == 36  & LENGTH <= 14, SIZE := 'SMALL']
multi.box[SVSPP == 36  & LENGTH >  14 & LENGTH <= 24, SIZE := 'MEDIUM']
multi.box[SVSPP == 36  & LENGTH >  24, SIZE := 'LARGE']
multi.box[SVSPP == 73  & LENGTH <= 20, SIZE := 'SMALL']
multi.box[SVSPP == 73  & LENGTH >  20 & LENGTH <= 50, SIZE := 'MEDIUM']
multi.box[SVSPP == 73  & LENGTH >  50, SIZE := 'LARGE']
multi.box[SVSPP == 103 & LENGTH <= 25, SIZE := 'SMALL']
multi.box[SVSPP == 103 & LENGTH >  25, SIZE := 'LARGE']
multi.box[SVSPP == 105 & LENGTH <= 20, SIZE := 'SMALL']
multi.box[SVSPP == 105 & LENGTH >  20, SIZE := 'LARGE']
multi.box[SVSPP == 135 & LENGTH <= 30, SIZE := 'SMALL']
multi.box[SVSPP == 135 & LENGTH >  30 & LENGTH <= 60, SIZE := 'MEDIUM']
multi.box[SVSPP == 135 & LENGTH >  60, SIZE := 'LARGE']
multi.box[SVSPP == 139 & LENGTH <= 25, SIZE := 'SMALL']
multi.box[SVSPP == 139 & LENGTH >  25 & LENGTH <= 70, SIZE := 'MEDIUM']
multi.box[SVSPP == 139 & LENGTH >  70, SIZE := 'LARGE']
multi.box[SVSPP == 145 & LENGTH <= 20, SIZE := 'SMALL']
multi.box[SVSPP == 145 & LENGTH >  20 & LENGTH <= 40, SIZE := 'MEDIUM']
multi.box[SVSPP == 145 & LENGTH >  40, SIZE := 'LARGE']
multi.box[SVSPP %in% c(69, 72, 76:78) & LENGTH <= 40, SIZE := 'SMALL']
multi.box[SVSPP %in% c(69, 72, 76:78) & LENGTH >  40, SIZE := 'LARGE']

multi.box[, SIZE := as.factor(SIZE)]

#Sum weight at length
setkey(multi.box, CRUISE6, STRATUM, STATION, SVSPP, SIZE)
multi.box[, box.biomass := sum(WGTLEN), by = key(multi.box)]

#Alter SVSPP so prestrat treats as seperate species
multi.box[SIZE == 'SMALL',  SVSPP := SVSPP + 1000L] 
multi.box[SIZE == 'MEDIUM', SVSPP := SVSPP + 2000L]
multi.box[SIZE == 'LARGE',  SVSPP := SVSPP + 3000L]
ewe.spp[EWE %like% '\\(S)', SVSPP := SVSPP + 1000L]
ewe.spp[EWE %like% '\\(M)', SVSPP := SVSPP + 2000L]
ewe.spp[EWE %like% '\\(L)', SVSPP := SVSPP + 3000L]

#Change biomass and drop extra columns to merge back
multi.box[, BIOMASS := box.biomass]
multi.box[, c('SIZE', 'box.biomass') := NULL]

#drop unknown lengths
multi.box <- multi.box[!is.na(LENGTH), ]

#Merge multi-stanza species with single box species
andre <- rbindlist(list(multi.box, single.box))

#Calculate stratified mean/ swept area estimate
#Need to do this on a region by region bases
andre.biomass <- c()
for(i in 1:length(regions)){
  #Segregate by region
  andre.region <- andre[Region == regions[i], ]
  andre.region[, Region := NULL] #Will get added back in next step
  
  #Run pre stratmean function
  andre.pre <- prestrat(andre.region, stratum, strat.col = 'STRATUM', area.col = 'STRATUM_AREA')
  
  #Reduce number of species
  pre.ewe.box <- merge(andre.pre, ewe.spp, by = 'SVSPP')
  
  #Calculate stratified mean
  ewe.box <- stratmean(pre.ewe.box, group.col = 'SVSPP', strat.col = 'STRATUM')

  #Calculate swept area
  ewe.biomass <- sweptarea(pre.ewe.box, ewe.box, q = ewe.spp[, list(SVSPP, fallq)], 
                           strat.col = 'STRATUM', area.col = 'STRATUM_AREA', group.col = 'SVSPP')

  #Collapse species to EwE boxes
  ewe.biomass <- merge(ewe.biomass, ewe.spp[, list(SVSPP, EWE)])
  setkey(ewe.biomass, YEAR, EWE)
  ewe.biomass[, sum.biomass := sum(Tot.biomass),   by = key(ewe.biomass)]
  ewe.biomass[, sum.abund   := sum(Tot.abundance), by = key(ewe.biomass)]
  ewe.biomass <- unique(ewe.biomass)
  ewe.biomass[, c('Tot.biomass', 'Tot.abundance', 'SVSPP') := NULL]
  setnames(ewe.biomass, c('sum.biomass', 'sum.abund'), c('Tot.biomass', 'Tot.abundance'))
  ewe.biomass[, Region := regions[i]]
  setcolorder(ewe.biomass, c('Region',        'EWE',        'YEAR', 
                             'strat.biomass', 'biomass.S2', 'biomass.SE', 'Tot.biomass',
                             'strat.abund',   'abund.S2',   'abund.SE',   'Tot.abundance'))

  #Merge together
  andre.biomass <- rbindlist(list(andre.biomass, ewe.biomass))
}

andre.final <- andre.biomass[, list(Region, EWE, YEAR, Tot.biomass)]
andre.final <- merge(andre.final, region.area, by = 'Region')
andre.final[, kg.km2 := Tot.biomass/AREA]

write.csv(andre.final, file = paste(out.dir, 'Buchheister_request_biomass.csv', sep = ''), row.names = F)
```

</details>

## 2. Compare inputs

<details>
<summary>
Read in 2014 and 2024 survdat_lw files.
</summary>

``` r
#Grab survdat.r (object is called survdat.lw)
load(file.path(here::here('EwE-menhaden-AndreBuchheister/2014request/survdat_lw.RData')))

survdat24 <- readRDS(here::here('EwE-menhaden-AndreBuchheister/survdat_lw.rds'))

survdat.lw24 <- survdat24$survdat
```

</details>
<details>
<summary>
Column names, types, and number of rows, columns in `survdat.lw` from
2014:
</summary>

``` r
cat("Names\n")
```

    ## Names

``` r
names(survdat.lw)
```

    ##  [1] "SVSPP"       "CATCHSEX"    "CRUISE6"     "STATION"     "STRATUM"    
    ##  [6] "TOW"         "SVVESSEL"    "YEAR"        "SEASON"      "LAT"        
    ## [11] "LON"         "EST_TOWDATE" "DEPTH"       "SURFTEMP"    "SURFSALIN"  
    ## [16] "BOTTEMP"     "BOTSALIN"    "ABUNDANCE"   "BIOMASS"     "LENGTH"     
    ## [21] "NUMLEN"      "INDWT"       "WGTLEN"      "SIZECAT"

``` r
str(survdat.lw)
```

    ## Classes 'data.table' and 'data.frame':   2892498 obs. of  24 variables:
    ##  $ SVSPP      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CATCHSEX   : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CRUISE6    : int  196721 196817 196817 196817 196817 196817 196817 196817 196817 196817 ...
    ##  $ STATION    : int  9 8 8 8 38 41 41 43 46 50 ...
    ##  $ STRATUM    : int  1080 1080 1080 1080 1720 1720 1720 1700 1680 1680 ...
    ##  $ TOW        : int  2 1 1 1 2 1 1 1 2 1 ...
    ##  $ SVVESSEL   : Factor w/ 12 levels "AL","AT","BG",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ YEAR       : int  1967 1968 1968 1968 1968 1968 1968 1968 1968 1968 ...
    ##  $ SEASON     : Factor w/ 2 levels "FALL","SPRING": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ LAT        : num  40 40 40 40 38 ...
    ##  $ LON        : num  -71.2 -70.6 -70.6 -70.6 -73.9 ...
    ##  $ EST_TOWDATE: POSIXct, format: "1967-10-18 14:47:00" "1968-10-11 06:19:00" ...
    ##  $ DEPTH      : int  308 325 325 325 293 325 325 88 375 220 ...
    ##  $ SURFTEMP   : num  16.4 18.2 18.2 18.2 19.2 19.3 19.3 19.6 20.4 21.1 ...
    ##  $ SURFSALIN  : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ BOTTEMP    : num  NA 9.4 9.4 9.4 9.1 8.2 8.2 9.5 NA 11.9 ...
    ##  $ BOTSALIN   : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ ABUNDANCE  : num  1 3 3 3 1 2 2 1 1 2 ...
    ##  $ BIOMASS    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ LENGTH     : num  23 11 14 22 9 23 25 28 15 22 ...
    ##  $ NUMLEN     : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ INDWT      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ WGTLEN     : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ SIZECAT    : Factor w/ 5 levels "XS","S","M","L",..: 3 2 2 3 2 3 3 3 2 3 ...
    ##  - attr(*, ".internal.selfref")=<externalptr> 
    ##  - attr(*, "sorted")= chr [1:2] "SVSPP" "CATCHSEX"

``` r
cat("N rows, N columns\n")
```

    ## N rows, N columns

``` r
dim(survdat.lw)
```

    ## [1] 2892498      24

</details>
<details>
<summary>
Column names, types, and number of rows, columns in `survdat.lw24` from
2024:
</summary>

``` r
cat("Names\n")
```

    ## Names

``` r
names(survdat.lw24)
```

    ##  [1] "SVSPP"       "CATCHSEX"    "CRUISE6"     "STATION"     "STRATUM"    
    ##  [6] "TOW"         "SVVESSEL"    "YEAR"        "SEASON"      "LAT"        
    ## [11] "LON"         "EST_TOWDATE" "DEPTH"       "SURFTEMP"    "SURFSALIN"  
    ## [16] "BOTTEMP"     "BOTSALIN"    "ABUNDANCE"   "BIOMASS"     "LENGTH"     
    ## [21] "NUMLEN"      "PREDWT"      "WGTLEN"

``` r
str(survdat.lw24)
```

    ## Classes 'data.table' and 'data.frame':   3112779 obs. of  23 variables:
    ##  $ SVSPP      : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CATCHSEX   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CRUISE6    : num  196721 196817 196817 196817 196817 ...
    ##  $ STATION    : num  9 8 8 8 38 41 41 43 46 50 ...
    ##  $ STRATUM    : num  1080 1080 1080 1080 1720 1720 1720 1700 1680 1680 ...
    ##  $ TOW        : num  2 1 1 1 2 1 1 1 2 1 ...
    ##  $ SVVESSEL   : chr  "AL" "AL" "AL" "AL" ...
    ##  $ YEAR       : num  1967 1968 1968 1968 1968 ...
    ##  $ SEASON     : chr  "FALL" "FALL" "FALL" "FALL" ...
    ##  $ LAT        : num  40 40 40 40 38 ...
    ##  $ LON        : num  -71.2 -70.6 -70.6 -70.6 -73.9 ...
    ##  $ EST_TOWDATE: POSIXct, format: "1967-10-18 15:47:00" "1968-10-11 07:19:00" ...
    ##  $ DEPTH      : int  308 325 325 325 293 325 325 88 375 220 ...
    ##  $ SURFTEMP   : num  16.4 18.2 18.2 18.2 19.2 19.3 19.3 19.6 20.4 21.1 ...
    ##  $ SURFSALIN  : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ BOTTEMP    : num  NA 9.4 9.4 9.4 9.1 8.2 8.2 9.5 NA 11.9 ...
    ##  $ BOTSALIN   : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ ABUNDANCE  : num  1 3 3 3 1 2 2 1 1 2 ...
    ##  $ BIOMASS    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ LENGTH     : num  23 11 14 22 9 23 25 28 15 22 ...
    ##  $ NUMLEN     : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ PREDWT     : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ WGTLEN     : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  - attr(*, ".internal.selfref")=<externalptr> 
    ##  - attr(*, "index")= int(0) 
    ##   ..- attr(*, "__SEASON")= int [1:3112779] 1 2 3 4 5 6 7 8 9 10 ...

``` r
cat("N rows, N columns\n")
```

    ## N rows, N columns

``` r
dim(survdat.lw24)
```

    ## [1] 3112779      23

</details>

The 2014 pull has an differently named column, `INDWT` which might be
`PREDWT` now, need to check.

The 2014 pull has an additional column, `SIZECAT` which is definitely
used in the script. Need to see how to recreate this.

We don’t have the sql code that created the 2014 pull.

<details>
<summary>
Attributes of the 2024 pull here:
</summary>

``` r
cat("SQL query\n")
```

    ## SQL query

``` r
cat(paste(names(survdat24$sql), "\n", unlist(survdat24$sql), collapse = "\n\n"))
```

    ## catch 
    ##  select cruise6, station, stratum, tow, svspp, catchsex,
    ##                      expcatchnum as abundance, expcatchwt as biomass
    ##                      from svdbs.UNION_FSCS_SVCAT
    ##                      where cruise6 in ('196307','196413','196514','196614','196721','196803','196817','196902','196911','197003','197006','197006','197101','197106','197202','197208','197208','197208','197303','197303','197303','197308','197308','197404','197404','197404','197411','197411','197503','197503','197512','197512','197602','197602','197609','197702','197702','197712','197804','197806','197904','197904','197910','197910','198002','198002','198007','198102','198106','198106','198202','198206','198303','198306','198402','198405','198502','198508','198508','198603','198606','198606','198702','198702','198705','198801','198803','198901','198904','199002','199004','199102','199105','199202','199206','199302','199306','199402','199406','199503','199507','199602','199604','199702','199706','199802','199804','199902','199908','200002','200005','200102','200109','200202','200209','200303','200306','200402','200407','200503','200510','200604','200610','200703','200709','200803','200807','200902','200904','201001','201004','201102','201105','201202','201204','201302','201304','201402','201404','201502','201504','201602','201604','201702','201704','201802','201804','201902','201904','202002','202004','202102','202104','202202','202204','202302','202304','202402','202404')
    ##                      and stratum not like 'YT%'
    ##                      order by cruise6, station, svspp
    ## 
    ## cruise 
    ##  select unique year, cruise6, svvessel, season
    ##       from svdbs.mstr_cruise
    ##       where purpose_code = 10
    ##       and year >= 1963and (season = 'FALL'
    ##         or season = 'SPRING')
    ##       order by year, cruise6
    ## 
    ## station 
    ##  select unique cruise6, svvessel, station, stratum,
    ##                                tow, decdeg_beglat as lat, decdeg_beglon as lon,
    ##                                begin_est_towdate as est_towdate, avgdepth as depth,
    ##                                surftemp, surfsalin, bottemp, botsalin
    ##                                from svdbs.UNION_FSCS_SVSTA
    ##                                where cruise6 in ('196307','196413','196514','196614','196721','196803','196817','196902','196911','197003','197006','197006','197101','197106','197202','197208','197208','197208','197303','197303','197303','197308','197308','197404','197404','197404','197411','197411','197503','197503','197512','197512','197602','197602','197609','197702','197702','197712','197804','197806','197904','197904','197910','197910','198002','198002','198007','198102','198106','198106','198202','198206','198303','198306','198402','198405','198502','198508','198508','198603','198606','198606','198702','198702','198705','198801','198803','198901','198904','199002','199004','199102','199105','199202','199206','199302','199306','199402','199406','199503','199507','199602','199604','199702','199706','199802','199804','199902','199908','200002','200005','200102','200109','200202','200209','200303','200306','200402','200407','200503','200510','200604','200610','200703','200709','200803','200807','200902','200904','201001','201004','201102','201105','201202','201204','201302','201304','201402','201404','201502','201504','201602','201604','201702','201704','201802','201804','201902','201904','202002','202004','202102','202104','202202','202204','202302','202304','202402','202404')
    ##                                and (SHG <= 136 and cruise6 <= 200900)
    ##                                or (TOGA <= 1324 and cruise6 > 200900)
    ##                                order by cruise6, station
    ## 
    ## length 
    ##  select cruise6, station, stratum, tow, svspp, catchsex,
    ##                       length, expnumlen as numlen
    ##                       from svdbs.UNION_FSCS_SVLEN
    ##                       where cruise6 in ('196307','196413','196514','196614','196721','196803','196817','196902','196911','197003','197006','197006','197101','197106','197202','197208','197208','197208','197303','197303','197303','197308','197308','197404','197404','197404','197411','197411','197503','197503','197512','197512','197602','197602','197609','197702','197702','197712','197804','197806','197904','197904','197910','197910','198002','198002','198007','198102','198106','198106','198202','198206','198303','198306','198402','198405','198502','198508','198508','198603','198606','198606','198702','198702','198705','198801','198803','198901','198904','199002','199004','199102','199105','199202','199206','199302','199306','199402','199406','199503','199507','199602','199604','199702','199706','199802','199804','199902','199908','200002','200005','200102','200109','200202','200209','200303','200306','200402','200407','200503','200510','200604','200610','200703','200709','200803','200807','200902','200904','201001','201004','201102','201105','201202','201204','201302','201304','201402','201404','201502','201504','201602','201604','201702','201704','201802','201804','201902','201904','202002','202004','202102','202104','202202','202204','202302','202304','202402','202404')
    ##                       and stratum not like 'YT%'
    ##                       order by cruise6, station, svspp, length
    ## 
    ## weightlength 
    ##  select svspp, sex, svlwexp, svlwcoeff, svlwexp_spring, svlwcoeff_spring,
    ##                svlwexp_fall, svlwcoeff_fall, svlwexp_winter, svlwcoeff_winter,
    ##                svlwexp_summer, svlwcoeff_summer
    ##                from svdbs.length_weight_coefficients
    ## 
    ## sad 
    ##  
    ## 
    ## conversions 
    ##  select *
    ##     from svdbs.survan_conversion_factors

``` r
cat("\n\nDate pulled\n")
```

    ## 
    ## 
    ## Date pulled

``` r
cat(survdat24$pullDate)
```

    ## Fri May 17 09:16:30 2024

``` r
cat("\n\nFunction call\n")
```

    ## 
    ## 
    ## Function call

``` r
print(survdat24$functionCall)
```

    ## survdat::get_survdat_data(channel = channel, filterByYear = NA, 
    ##     all.season = F, shg.check = T, conversion.factor = T, use.SAD = F, 
    ##     getBio = F, getLengths = T, getWeightLength = T)

</details>

Some types are different. If the nums need to change to ints in the
script we can do that.

## 3. Run old script

<details>
<summary>
Trying initial portion: modified code here reads in 2014 `survdat_lw`
file and strata files
</summary>

``` r
#Required packages
library(data.table) #; library(RODBC); 
library(rgdal) #; what has replaced this? 
```

    ## Loading required package: sp

    ## Please note that rgdal will be retired by the end of 2023,
    ## plan transition to sf/stars/terra functions using GDAL and PROJ
    ## at your earliest convenience.
    ## 
    ## rgdal: version: 1.5-32, (SVN revision 1176)
    ## Geospatial Data Abstraction Library extensions to R successfully loaded
    ## Loaded GDAL runtime: GDAL 3.2.2, released 2021/03/05
    ## Path to GDAL shared files: /usr/local/Cellar/gdal/3.2.2/share/gdal
    ## GDAL binary built with GEOS: TRUE 
    ## Loaded PROJ runtime: Rel. 7.2.1, January 1st, 2021, [PJ_VERSION: 721]
    ## Path to PROJ shared files: /Users/sarah.gaichas/Library/Application Support/proj:/usr/local/opt/proj/share/proj:/usr/local/Cellar/proj/7.2.1/share/proj
    ## PROJ CDN enabled: FALSE
    ## Linking to sp version:1.5-0
    ## To mute warnings of possible GDAL/OSR exportToProj4() degradation,
    ## use options("rgdal_show_exportToProj4_warnings"="none") before loading sp or rgdal.

``` r
library(survdat)

#-------------------------------------------------------------------------------
count <- function(x){
    num <- rep(1, length(x))
    out <- sum(num)
    return(out)
    }

#-------------------------------------------------------------------------------

#Grab survdat.r
#load(file.path(data.dir, 'survdat_lw.RData'))
load(file.path(here::here('EwE-menhaden-AndreBuchheister/2014request/survdat_lw.RData')))


#Grab strata
#strata <- readOGR(gis.dir, 'strata')
strata <- readOGR(here::here('EwE-menhaden-AndreBuchheister/2014request/'), 'strata')
```

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "/Users/sarah.gaichas/Documents/0_Data/data-requests/EwE-menhaden-AndreBuchheister/2014request", layer: "strata"
    ## with 310 features
    ## It has 8 fields

``` r
#Stratas
#Andre wants MAB, SNE, GB, and GOM from EMAX work with depth bins of
#<27m, 27m - 55m, and >55m
regions <- c('mab.shallow', 'mab.mid', 'mab.deep', 'sne.shallow', 'sne.mid', 'sne.deep',
                            'gb.mid',  'gb.deep',  'gom.shallow', 'gom.mid', 'gom.deep')

mab.shallow <- c(03150:03440) 
mab.mid     <- c(01610, 01650, 01690, 01730)
mab.deep    <- c(01620:01640, 01660:01680, 01700:01720, 01740:01760)
sne.shallow <- c(03010:03140, 03450:03560)
sne.mid     <- c(01010, 01050, 01090)
sne.deep    <- c(01020:01040, 01060:01080, 01100:01120)
gb.mid      <- c(01190, 01200, 01250)
gb.deep     <- c(01130:01180, 01210:01230)
gom.shallow <- c(03570:03590, 03620:03640, 03670, 03680, 03700, 03710, 03730, 
                 03740, 03760, 03790, 03820, 03840, 03860, 03880)
gom.mid     <- c(03600, 03610, 03650, 03660, 03690, 03720, 03750, 03770, 03780,
                 03800, 03810, 03830, 03850, 03870, 03890)
gom.deep    <- c(03900, 01240, 01260:01300, 01360:01400)

#Add ecoregion designation
eco.info <- as.data.table(strata@data)
for(ireg in 1:length(regions)) eco.info[FINSTR_ID %in% get(regions[ireg]), Eco := regions[ireg]]
strata@data <- eco.info

#Generate area table

#original getarea function from 2014request/R/survdat_functions.R
getarea <- function(stratum, strat.col){
  #Get stratum areas
  lcc <- CRS("+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ") #Lambert Conformal Conic
  
  strat <- spTransform(stratum, lcc)
  
  #Calculate areas
  names(strat@data)[which(names(strat@data) == strat.col)] <- "STRAT"
  strata.A <- data.table(STRAT = slot(strat, "data")$STRAT, Area = sapply(slot(strat, "polygons"), slot, "area")/1e6)
  
  setnames(strata.A, "STRAT", strat.col)
  
  return(strata.A)
}

strat.area <- getarea(strata, 'Eco') # function replaced with get_area?
#strat.area <- survdat::get_area(strata, 'Eco') #requires different input
strat.area <- strat.area[, sum(Area), by = 'Eco']
setnames(strat.area, 'V1', 'Area')

#save(strat.area, file = file.path(out.dir, 'Andre_regions.RData')) #original file for comparison
saveRDS(strat.area, here::here('EwE-menhaden-AndreBuchheister/test2014/Andre_regions.rds'))
```

</details>

We can read in the old shapefiles used in 2014, but new
`survdat::get_area` doesn’t work with this format.

For now using the older Survdat function `getarea` but this will need to
be updated. The strat.area object was saved in the test2014 folder.

<details>
<summary>
Did we get the same areas from the same file? Yes.
</summary>

``` r
load(here::here('EwE-menhaden-AndreBuchheister/2014request/Andre_regions.RData'))
cat("Original file\n")
```

    ## Original file

``` r
strat.area
```

    ##             Eco      Area
    ##  1: gom.shallow  3091.005
    ##  2: sne.shallow 10640.336
    ##  3:    gom.deep 64055.727
    ##  4:        <NA> 66266.048
    ##  5:     gom.mid  3847.600
    ##  6:     gb.deep 26673.911
    ##  7:      gb.mid 12277.996
    ##  8:     sne.mid 16910.037
    ##  9:    sne.deep 30354.360
    ## 10: mab.shallow 14438.877
    ## 11:     mab.mid 25738.136
    ## 12:    mab.deep 12021.615

``` r
strat.area.test <- readRDS(here::here('EwE-menhaden-AndreBuchheister/test2014/Andre_regions.rds'))
cat("File we just made\n")
```

    ## File we just made

``` r
strat.area.test
```

    ##             Eco      Area
    ##  1: gom.shallow  3091.005
    ##  2: sne.shallow 10640.336
    ##  3:    gom.deep 64055.727
    ##  4:        <NA> 66266.048
    ##  5:     gom.mid  3847.600
    ##  6:     gb.deep 26673.911
    ##  7:      gb.mid 12277.996
    ##  8:     sne.mid 16910.037
    ##  9:    sne.deep 30354.360
    ## 10: mab.shallow 14438.877
    ## 11:     mab.mid 25738.136
    ## 12:    mab.deep 12021.615

</details>

We will need to ensure the shapefiles here are what we want to use going
forward, and figure out equivalents on NEFSC_Spatial or another central
source.

<details>
<summary>
Next build species list and set up length stratification
</summary>

``` r
#Build species list
#load(file.path(data.dir, 'Species_codes.RData'))
# object is called spp
load(file.path(here::here('EwE-menhaden-AndreBuchheister/2014request/Species_codes.RData')))
andre.spp <- as.data.table(read.csv(here::here("EwE-menhaden-AndreBuchheister/2014request/Andre_groups.csv")))
spp <- merge(unique(spp[, list(SVSPP, Fall.q, Spring.q)]), andre.spp, by = 'SVSPP', all.x = T)
spp <- spp[!is.na(EWE), ]

#save(spp, file = file.path(out.dir, 'Buchheister_spp.RData'))
saveRDS(spp, here::here('EwE-menhaden-AndreBuchheister/test2014/Buchheister_spp.rds'))

# old Survdat poststrat
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


#Post stratify
andre <- poststrat(survdat.lw, strata, strata.col = 'Eco')
setnames(andre, 'newstrata', 'Region')
andre <- andre[!is.na(Region), ]

#Segregate by season
#NA for now

#Set up for stratification
#Break size structured groups
andre[SVSPP %in% c(15, 36, 69, 72, 73, 76:78, 103, 105, 135, 139, 145), MULTI := T]
andre[is.na(MULTI), MULTI := F] 

single.box <- andre[MULTI == F, ]

multi.box  <- andre[MULTI == T, ]

#size group designation
#For reference: Spiny Dogish = 15
#               Atl Menhaden = 36
#               Atl Cod = 73
#               Summer Flounder = 103
#               Yellowtail Flounder = 105
#               Bluefish = 135
#               Striped Bass = 139
#               Weakfish = 145
#               Hakes (Offshore, Silver, White, Red, Spotted) = 69, 72, 76, 77, 78
multi.box[SVSPP == 15  & LENGTH <= 60, SIZE := 'SMALL']
multi.box[SVSPP == 15  & LENGTH >  60, SIZE := 'LARGE']
multi.box[SVSPP == 36  & LENGTH <= 14, SIZE := 'SMALL']
multi.box[SVSPP == 36  & LENGTH >  14 & LENGTH <= 24, SIZE := 'MEDIUM']
multi.box[SVSPP == 36  & LENGTH >  24, SIZE := 'LARGE']
multi.box[SVSPP == 73  & LENGTH <= 20, SIZE := 'SMALL']
multi.box[SVSPP == 73  & LENGTH >  20 & LENGTH <= 50, SIZE := 'MEDIUM']
multi.box[SVSPP == 73  & LENGTH >  50, SIZE := 'LARGE']
multi.box[SVSPP == 103 & LENGTH <= 25, SIZE := 'SMALL']
multi.box[SVSPP == 103 & LENGTH >  25, SIZE := 'LARGE']
multi.box[SVSPP == 105 & LENGTH <= 20, SIZE := 'SMALL']
multi.box[SVSPP == 105 & LENGTH >  20, SIZE := 'LARGE']
multi.box[SVSPP == 135 & LENGTH <= 30, SIZE := 'SMALL']
multi.box[SVSPP == 135 & LENGTH >  30 & LENGTH <= 60, SIZE := 'MEDIUM']
multi.box[SVSPP == 135 & LENGTH >  60, SIZE := 'LARGE']
multi.box[SVSPP == 139 & LENGTH <= 25, SIZE := 'SMALL']
multi.box[SVSPP == 139 & LENGTH >  25 & LENGTH <= 70, SIZE := 'MEDIUM']
multi.box[SVSPP == 139 & LENGTH >  70, SIZE := 'LARGE']
multi.box[SVSPP == 145 & LENGTH <= 20, SIZE := 'SMALL']
multi.box[SVSPP == 145 & LENGTH >  20 & LENGTH <= 40, SIZE := 'MEDIUM']
multi.box[SVSPP == 145 & LENGTH >  40, SIZE := 'LARGE']
multi.box[SVSPP %in% c(69, 72, 76:78) & LENGTH <= 40, SIZE := 'SMALL']
multi.box[SVSPP %in% c(69, 72, 76:78) & LENGTH >  40, SIZE := 'LARGE']

multi.box[, SIZE := as.factor(SIZE)]

#Sum weight at length
setkey(multi.box, CRUISE6, STRATUM, STATION, SVSPP, SIZE)
multi.box[, box.biomass := sum(WGTLEN), by = key(multi.box)]

#Alter SVSPP so prestrat treats as seperate species
multi.box[SIZE == 'SMALL',  SVSPP := SVSPP + 1000L] 
multi.box[SIZE == 'MEDIUM', SVSPP := SVSPP + 2000L]
multi.box[SIZE == 'LARGE',  SVSPP := SVSPP + 3000L]
# These are uncommented in original script, can't see how they could work
# unless ewe.spp was in memory from somewhere else
#ewe.spp[EWE %like% '\\(S)', SVSPP := SVSPP + 1000L]
#ewe.spp[EWE %like% '\\(M)', SVSPP := SVSPP + 2000L]
#ewe.spp[EWE %like% '\\(L)', SVSPP := SVSPP + 3000L]

#Change biomass and drop extra columns to merge back
multi.box[, BIOMASS := box.biomass]
multi.box[, c('SIZE', 'box.biomass') := NULL]

#drop unknown lengths
multi.box <- multi.box[!is.na(LENGTH), ]

#Merge multi-stanza species with single box species
andre <- rbindlist(list(multi.box, single.box))
```

</details>

Is the data from this species list captured in an existing object? it
includes Fall and Spring q values.

Are we missing an `ewe.spp` object somewhere? Bypassed this but maybe
important?

Now try the calculations.

<details>
<summary>
For these we need the rest of the old Survdat functions
</summary>

``` r
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

# is this what is called `prestrat` below?
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
```

</details>

The `survdat_functions.R` is missing the `prestrat` function used in the
code below. I am hoping that I can substitute the function `stratprep`
in here.

Sadly no. This breaks at the `stratprep` function. I tried passing it
the `strat.area` object but it does not have the column names this
function is looking for.

<details>
<summary>
Now try the calculations
</summary>

``` r
#Calculate stratified mean/ swept area estimate
#Need to do this on a region by region bases
andre.biomass <- c()
for(i in 1:length(regions)){
  #Segregate by region
  andre.region <- andre[Region == regions[i], ]
  andre.region[, Region := NULL] #Will get added back in next step
  
  #Run pre stratmean function
  #andre.pre <- prestrat(andre.region, stratum, strat.col = 'STRATUM', area.col = 'STRATUM_AREA')
  
  #prestrat does not exist, renamed stratprep?
  andre.pre <- stratprep(andre.region, strat.area, strat.col = 'STRATUM', area.col = 'STRATUM_AREA')
  
  #Reduce number of species
  pre.ewe.box <- merge(andre.pre, ewe.spp, by = 'SVSPP')
  
  #Calculate stratified mean
  ewe.box <- stratmean(pre.ewe.box, group.col = 'SVSPP', strat.col = 'STRATUM')

  #Calculate swept area
  ewe.biomass <- sweptarea(pre.ewe.box, ewe.box, q = ewe.spp[, list(SVSPP, fallq)], 
                           strat.col = 'STRATUM', area.col = 'STRATUM_AREA', group.col = 'SVSPP')

  #Collapse species to EwE boxes
  ewe.biomass <- merge(ewe.biomass, ewe.spp[, list(SVSPP, EWE)])
  setkey(ewe.biomass, YEAR, EWE)
  ewe.biomass[, sum.biomass := sum(Tot.biomass),   by = key(ewe.biomass)]
  ewe.biomass[, sum.abund   := sum(Tot.abundance), by = key(ewe.biomass)]
  ewe.biomass <- unique(ewe.biomass)
  ewe.biomass[, c('Tot.biomass', 'Tot.abundance', 'SVSPP') := NULL]
  setnames(ewe.biomass, c('sum.biomass', 'sum.abund'), c('Tot.biomass', 'Tot.abundance'))
  ewe.biomass[, Region := regions[i]]
  setcolorder(ewe.biomass, c('Region',        'EWE',        'YEAR', 
                             'strat.biomass', 'biomass.S2', 'biomass.SE', 'Tot.biomass',
                             'strat.abund',   'abund.S2',   'abund.SE',   'Tot.abundance'))

  #Merge together
  andre.biomass <- rbindlist(list(andre.biomass, ewe.biomass))
}

andre.final <- andre.biomass[, list(Region, EWE, YEAR, Tot.biomass)]
andre.final <- merge(andre.final, region.area, by = 'Region')
andre.final[, kg.km2 := Tot.biomass/AREA]

write.csv(andre.final, file = paste(out.dir, 'Buchheister_request_biomass.csv', sep = ''), row.names = F)
```

</details>

# Try with 2024 survdat
