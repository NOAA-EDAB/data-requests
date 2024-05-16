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
