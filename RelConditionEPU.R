#' Calculated condition factor
#'
#' describe further. We will clean this up further if/when we make this into a package
#'
#' @param pullNewData boolean. Should a connection to oracle be made to pull data or use existing data
#' @param out.dir character string. name of directory in which plots and data files will be saved
#'
#' @return list of 2 items
#' \item{annualcond}{tibble (n x 6) Condition factor for each species, sex over time}
#' \item{stom}{data.frame (m x 52). stomach contents by year, cruise, sex, species etc.}
#'
#' @export


#Once package is complete, source files below:
# source("R/install_cond_packages.R")
# source("R/connect_to_database.R")
# source("R/pull_from_svdbs.R")
# source("R/utilities.R")
# source("R/plot_condition.R")
#source("R/StomFullnessData_allfh.R")

#Required packages
library(devtools)
#devtools::install_github('slucey/RSurvey/Survdat', )
#updated install of Survdat (2022):
library(remotes)
#remotes::install_github('NOAA-EDAB/Survdat', force=T)
#library(Survdat)
#Modified survdat with corrected Bigelow conversions (build_vignettes not working):
#remotes::install_github("NOAA-EDAB/survdat",build_vignettes = FALSE)
library(survdat)
library(data.table)
# library(graphics)
# library(grDevices)
#library(TeachingDemos)
library(dplyr)
library(tidyr)
library(tidyverse)
#library(gapminder)
#rgdal won't install without Rtools, Rtools can't be installed on this version of R
#library(rgdal)
#library(RODBC)
#dowload dbutils to pull survdat data:
#remotes::install_github("andybeet/dbutils")
#library(gam)
library(magrittr)
#Changepoint analysis:
#library(EnvCpt)
library(rpart)
library(rpart.plot)
#library(readr)

#Turn this on when running outside of function
out.dir="output"

data.dir <- "data"
gis.dir  <- "gis"

#Turn off the function when running outside of function to test new code
#RelConditionEPU <- function(pullNewData=F,out.dir="output") {
#RelConditionEPU <- function(out.dir,data.dir,gis.dir) {
# create output directory if it doesn't exist
#  if (!dir.exists(out.dir)) dir.create(out.dir)




#-------------------------------------------------------------------------------
#This section is used to pull NEFSC survey bottom trawl data directly from SVDBS.
#It requires a current Oracle connection, permission to access the database and a user name/password. 


#laptop (for 2023 SOE condition data):
#   source("C:\\Users\\laurel.smith\\Documents\\R\\Oracle_User_Data.R")
#   channel <- dbutils::connect_to_database(server="sole.nefsc.noaa.gov",uid=user.name)
#Sean gave connection code on Nov. 14, 2023 (no longer works after new laptop on Nov. 28, 2023):   
#   source("C:\\Users\\laurel.smith\\Documents\\R\\Oracle_User_Data.R")
#   channel <- dbutils::connect_to_database(server='sole.nefsc.noaa.gov',uid=lsmith)
# 
#ITD gave script to connect to Oracle on Dec. 7, 2023, still works Jan. 2025:
#    source(("C:\\Users\\laurel.smith\\Documents\\EDAB\\ConditionGAM\\R\\ConnectOracle.R"))
#    channel <- dbConnect(drv,username=user,password=passwd, dbname=connect.string)

# #NERHA Surfclam and ocean quahog data:
#    clam <- survdat::get_survdat_clam_data(channel)
#    save(clam[["data"]],file = here::here("other",paste0("Clam_Survey_1982_2022.Rdata")))
   
   # # # #getBio for individual weights (this line and next line works as of Jan. 2025):#
#   survey <- survdat::get_survdat_data(channel, all.season = T, getBio = T)
# #  #first line works as of July 13th, 2023:
#    survdat <- survey$survdat
  # survbio=as.data.frame(survey[['survdat']])
#    saveRDS(survdat,file = here::here("other",paste0("survdat_allseasons_1-24-2025.rds")))

#   data <- readRDS(here::here("other", "survdat_allseasons_2-6-2024.rds"))
   survdat <- readRDS(here::here("other", "survdat_allseasons_1-24-2025.rds"))   
#   
#   wing and door spread data included for NERHA from Sean Lucey Oct. 17, 2023:
#   load(file.path("C:/Users/laurel.smith/Documents/EDAB/data", "NRHA_survdat.RData", sep=''))
#   spread <- nrha.survdat$survdat
   
#   #Save survdat as RDS for Tori Kentner data request and NRHA update:
#   saveRDS(survdat,file = here::here("other",paste0("survdat_7-13-2023.rds")))

 #Strata setsif using Sean's code:
 # EPU <- c('MAB', 'GB', 'GOM', 'SS')
 # MAB <- c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510)
 # GB  <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)
 # GOM <- c(1220, 1240, 1260:1290, 1360:1400, 3560:3830)
 # SS  <- c(1300:1352, 3840:3990)
 # #Unknown strata 41-59:
 # Other <- c(1401:1599)

# survey.data <- c()

 #Parsing survey data to EPU based on STRATUM instead of EPU.shp files in survdat:
 survey.data <- survdat %>% dplyr::mutate(EPU = case_when(STRATUM %in% c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510) ~ 'MAB',
                                                     STRATUM %in% c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550) ~ 'GB',
                                                     STRATUM %in% c(1220, 1240, 1260:1290, 1360:1400, 3560:3830)~ 'GOM',
                                                     STRATUM %in% c(1300:1352, 1401:1599, 3840:3990)~ 'SS'))

  #Included strata 1410-1590 as SS:                                                   
#                                                     STRATUM %in% c(1401:1599)~ 'Other'))
 
 EPUna <- survey.data %>% dplyr::filter(is.na(EPU))
 
#   #comment out if not running as function
# if (pullNewData == T) {
#   # grabs data from oracle
#   uid <- readline("Please Enter your username for oracle: ")
#   message(paste0("Pulling data from svdbs ... please wait ... data will be saved in directory: ",here::here(out.dir)))
#   survey <- pull_from_svdbs(uid) 
#   # save data pull
#   dateOfPull <- format(Sys.time(), "%m-%d-%y")
# saveRDS(survey,file = here::here(out.dir,paste0("NEFSC_survey_data_",dateOfPull,".rds")))
#  saveRDS(survey,file = here::here(out.dir,paste0("NEFSC_survey_data_1-5-2023.rds")))
# } else {
# Otherwise, load data below:
#If not pulling from SVDBS, load NEFSC survey data (2023 SOE Condition Data):
#When trying to load on Jan 6th, 2023, got error in load: bad restore file magic number:
 #load(file.path(data.dir, 'NEFSC_survey_data_1-5-2023.rds', sep = ''))

#or if that doesn't work:
#Used for AFS 2019 GAM analyses from direct SVDBS data pull (no calibration coefficients and selecting all tows not just representative tows):
#survey <-  readRDS(file.path(data.dir, 'NEFSC_survey_data_02-13-20.rds', sep = ''))
#Survdat data with indwt and sex from Union_FSCS_SVBIO (https://github.com/NOAA-EDAB/survdat):   
#Currently missing smooth and spiny dogfish prior to 2001
#  data <- readRDS(here::here(data.dir, "survdatBio.rds"))

#Data pull used in AFS 2019 GAM analyses, adding filters for purpose_code, SHG, TOGA and survdat door/vessel/gear/Bigelow conversions (not lenght conversions):
#RDS file isn't working:
#survey <- readRDS(here::here(data.dir, "SurveyData.rds"))

#Data from Survdat updated to fix Bigelow conversion issues (Feb. 2021 from remotes::install_github("NOAA-EDAB/survdat",build_vignettes = TRUE):

###From survdat helpfile (pull data once using lines below, then turn off and load(survbio) for future use:
#In survdat package, have to install dbutils from Andy's github at top:
#copy into console and fill in server and uid:
#channel <- dbutils::connect_to_database(server="",uid="")

# survey <- get_survdat_data(channel, getBio = T)
# 
#survbio=as.data.frame(survey[['survdat']])
# 
# #save survbio object so RData data doesn't need to be pulled each time:
#  save(survbio, file='survbio.RData')
###end data pull

#Pull data for NRHA:
#survey <- get_survdat_data(channel, all.season = T)

#survbio=as.data.frame(survey[['survdat']])

#save survbio object so RData data doesn't need to be pulled each time:
# save(survbio, file='survbio_NRHA.RData')
###end data pull

 
#load("survbio.Rdata")
#Got survdat.RData from Sean Lucey on Jan 6th, 2023 for 2023 SOE because it stalled out and wouldn't load from my data pull.
#load(file.path(data.dir, "survdat.RData"))

#for total swept-area biomass estimates (not currently used in condition GAMS):
#swept_area <- calc_swept_area(survey)

#CSV from Andy_DataPull.R:
#  survey <- readr::read_csv(here::here(data.dir, "SurveyData.csv"))

#   survey <- readRDS(here::here(out.dir, "NEFSC_survey_data_1-5-2023.rds"))
# }


#pulling directly from SVDBS:
#fall survey to be used for most species:
#fall <- survey %>% filter(SEASON == 'FALL') %>% mutate(SEX=as.character(SEX), 
  #                                                     LAT = BEGLAT, LON = BEGLON)

#Using survdat data (change SEX== NA to sex == 0)
#fall <- survbio %>% filter(SEASON == 'FALL') %>% dplyr::mutate(sex = if_else(is.na(SEX), '0', SEX))
#fall <- survdat %>% filter(SEASON == 'FALL') %>% dplyr::mutate(sex = if_else(is.na(SEX), '0', SEX))
#When parsing by EPUs based on Sean Lucey's code:
# fall <- data %>% filter(SEASON == 'FALL') %>% dplyr::mutate(sex = if_else(is.na(SEX), '0', SEX))
#When parsing EPUs based on survey strata instead of through suvdat:
fall <- survey.data %>% filter(SEASON == 'FALL') %>% dplyr::mutate(sex = if_else(is.na(SEX), '0', SEX))

 #about 1/4 of fish with indwt have sex = 0:
#fall_indwt <- fall %>% filter(!is.na(INDWT))

#Spring survey data to be used for herring, mackerel and OP:
#spring <- data %>% filter(SEASON == 'SPRING') %>% dplyr::mutate(sex = if_else(is.na(SEX), '0', SEX))
#spring <- survey %>% filter(SEASON == 'SPRING') %>% mutate(SEX=as.character(SEX), 
#                                                       LAT = BEGLAT, LON = BEGLON)

#SVDBS has errors in SEX data. If not fixed in pull, reassign SEX for red hake in 1980-1981:
# fall <- fallOrig %>% mutate(SEX= (ifelse(SEX=='M', '1',
#                             ifelse(SEX=='F', '2',
#                                    ifelse(SEX=='f', '2', SEX)))))

#------------------------------------------------------------------------------

#####THIS SECTION FOR Martin LW params:
#reading in condition lw paramteters from Michael Martin (survey L-W for sampling error messages) for tidyverse:
#LWparams <- readr::read_csv(here::here(data.dir, "lw_parameters_Condition.csv"))

#Coding 
#using tidyverse to recode sex:
# LWpar1 <- dplyr::mutate(LWparams,
#                         SEX = as.character(SEXMF))
# LWpar <- LWpar1 %>% mutate(SVSPP = if_else(LW_SVSPP<100, as.character(paste0('0',LW_SVSPP)),
#                                            if_else(LW_SVSPP<10, as.character(paste0('00',LW_SVSPP)),
#                                                    if_else(LW_SVSPP>=100, as.character(LW_SVSPP), 'NA'))))
# 
# LWpar$SEX[LWpar$SEXMF=='M'] <- '1'
# LWpar$SEX[LWpar$SEXMF=='F'] <- '2'
# LWpar$SEX[is.na(LWpar$SEXMF)] <- '0'

#view(LWparams)
#Use seasonal L-W parameters when available

#this works to fill EXPONENT_FALL_COMP with SEASONLESS_EXPONENT if NA, otherwise use EXPONENT_FALL (but only if brought in as a data frame, not as a data table)
# ind <- is.na(LWpar$EXPONENT_FALL)
# LWpar$EXPONENT_FALL_COMPL <- LWpar$EXPONENT_FALL
# LWpar$EXPONENT_FALL_COMPL[ind]<-LWpar$SEASONLESS_EXPONENT[ind]
# #cbind(LWpar$EXPONENT_FALL,LWpar$EXPONENT_FALL_COMPL,LWpar$SEASONLESS_EXPONENT)

# ind <- is.na(LWpar$EXPONENT_SPRING)
# LWpar$EXPONENT_SPRING_COMPL <- LWpar$EXPONENT_SPRING
# LWpar$EXPONENT_SPRING_COMPL[ind]<-LWpar$SEASONLESS_EXPONENT[ind]
# #cbind(LWpar$EXPONENT_SPRING,LWpar$EXPONENT_SPRING_COMPL,LWpar$SEASONLESS_EXPONENT)

# ind <- is.na(LWpar$COEFFICIENT_FALL)
# LWpar$COEFFICIENT_FALL_COMPL <- LWpar$COEFFICIENT_FALL
# LWpar$COEFFICIENT_FALL_COMPL[ind]<-LWpar$SEASONLESS_COEFFICIENT[ind]
# #cbind(LWpar$COEFFICIENT_FALL,LWpar$COEFFICIENT_FALL_COMPL,LWpar$SEASONLESS_COEFFICIENT)

# ind <- is.na(LWpar$COEFFICIENT_SPRING)
# LWpar$COEFFICIENT_SPRING_COMPL <- LWpar$COEFFICIENT_SPRING
# LWpar$COEFFICIENT_SPRING_COMPL[ind]<-LWpar$SEASONLESS_COEFFICIENT[ind]
# #cbind(LWpar$COEFFICIENT_SPRING,LWpar$COEFFICIENT_SPRING_COMPL,LWpar$SEASONLESS_COEFFICIENT)

#merge with tidyr:
#LWparInt <- transform(LWpar, SEX = as.integer(SEX))
# LWparInt <- LWpar
# summary(LWparInt)
#summary(fall)
#mergedata <- merge(fall, LWparInt, all.fall=T, all.LWparInt = F)
#left_join gave NAs for some scup and BSB L-W params

#Wigley et al. 2003 L-W parameters:
LWparams <- readr::read_csv(here::here(data.dir, "tech_memo_parameters_table_format.csv"))

#head(LWparams)
#View(LWparams)
#head(fall)

#Standardize syntax of Condition L-W data for merge with survey data:
#remove ? and replace with negative from error saving .xlsx as .csv
LWparams1 <- dplyr::mutate(LWparams,
                           lna1 = substr(ln_a, 2, nchar(ln_a)),
                           lna = as.numeric(lna1)*-1)

#Parse data by season:
LWpar <- LWparams1 %>% dplyr::mutate(SEASON = if_else(Season == 'Autumn', as.character('FALL'),
                                      if_else(Season == 'Win/Aut', as.character('FALL'),
    #*****If using spring data, change Spr/Aut, Wint/Spr/Aut to SPRING:                                                   
                                      if_else(Season == 'Spr/Aut', as.character('FALL'),
                                      if_else(Season == 'Win/Spr/Aut', as.character('FALL'),        
                                      if_else(Season == 'Win/Spr', as.character('SPRING'),
                                      if_else(Season == 'Spring', as.character('SPRING'), 
                                      if_else(Season == 'Winter', as.character('WINTER'),'NA'))))))))

LWfall <- LWpar %>% dplyr::filter(SEASON == 'FALL')
#LWspring <- LWpar %>% dplyr::filter(SEASON == 'SPRING')

#By Species: Parse Combined gender L-Ws by sex if no sex-specific parameters available. Otherwise assign SEX codes:
# Rob's code
LWfall_orig <- LWfall
LWfall <- LWfall[-c(1:nrow(LWfall)),]
speciesList <- unique(LWfall_orig$SpeciesName)
numSpecies <- length(speciesList)
for (spp in 1:numSpecies) {
  sppTibble <- filter(LWfall_orig,SpeciesName == speciesList[spp])
  if (nrow(sppTibble) == 1) {
    LWfall <- rbind(LWfall,sppTibble)
    newRow <- sppTibble[1,]
    newRow$Gender <- "Male"
    LWfall <- rbind(LWfall,newRow)
    newRow <- sppTibble[1,]
    newRow$Gender <- "Female"
    LWfall <- rbind(LWfall,newRow)
  } else if (nrow(sppTibble) == 3) {
    LWfall <- rbind(LWfall,sppTibble)
  }
 }

#spring:
# LWspring_orig <- LWspring
# LWspring <- LWspring[-c(1:nrow(LWspring)),]
# speciesList <- unique(LWspring_orig$SpeciesName)
# numSpecies <- length(speciesList)
# for (spp in 1:numSpecies) {
#   sppTibble <- filter(LWspring_orig,SpeciesName == speciesList[spp])
#   if (nrow(sppTibble) == 1) {
#     LWspring <- rbind(LWspring,sppTibble)
#     newRow <- sppTibble[1,]
#     newRow$Gender <- "Male"
#     LWspring <- rbind(LWspring,newRow)
#     newRow <- sppTibble[1,]
#     newRow$Gender <- "Female"
#     LWspring <- rbind(LWspring,newRow)
#   } else if (nrow(sppTibble) == 3) {
#     LWspring <- rbind(LWspring,sppTibble)
#   }
# }

#Add rows to assign SEX when Gender == Combined in Wigley et al ref (didn't work):
# LWpar_sex <- LWparams1 %>% dplyr::filter(Gender == 'Combined') %>%
#   slice(rep(1:n(), each=3)) %>%
#   mutate(SEX = as.character(rep(0:2, length.out = n())))

#Add SEX for Combined gender back into Wigley at all data (loses 4 Gender==Unsexed):
#LWpar_sexed <- LWspring %>%
LWpar_sexed <- LWfall %>% 
  dplyr::mutate(sex = if_else(Gender == 'Combined', as.character(0),
                      if_else(Gender == 'Unsexed', as.character(0),
                      if_else(Gender == 'Male', as.character(1),
                      if_else(Gender == 'Female', as.character(2),'NA')))))

#Duplicate Combined for sex=0 and sex=4 (Trans) for BSB:
LWpar_BSB <- LWpar_sexed %>% dplyr::filter(LW_SVSPP == 141, Gender == 'Combined') %>%
     slice(1) %>%
    mutate(sex = as.character(4))
  
LWpar_sex <- dplyr::bind_rows(LWpar_sexed, LWpar_BSB)
  
LWpar_spp <- LWpar_sex %>% mutate(SVSPP = as.numeric(LW_SVSPP))


#mergedata <- left_join(fall, LWparInt, by= c('SVSPP', 'SEX'))
mergedata <- left_join(fall, LWpar_spp, by= c('SEASON', 'SVSPP', 'sex'))

#mergedata <- left_join(fall, LWparInt, by= c('SVSPP', 'SEX'))
#mergedata <- left_join(spring, LWpar_spp, by= c('SEASON', 'SVSPP', 'sex'))

#checking for missing complete L-W params (over 96,000 species don't have LW parameters or aren't assigned a M/F sex code)
# nocompl <- dplyr::filter(mergedata, is.na(COEFFICIENT_FALL_COMPL))
# unique(nocompl$SVSPP)
# unique(nocompl$YEAR)

#filters out values without losing rows with NAs:
mergewt <- dplyr::filter(mergedata, is.na(INDWT) | INDWT<900)
mergewtno0 <- dplyr::filter(mergewt, is.na(INDWT) | INDWT>0.004)
mergelenno0 <- dplyr::filter(mergewtno0, is.na(LENGTH) | LENGTH>0)
#mergelenno0 <- dplyr::filter(mergewtno0, is.na(LENGTH) | LENGTH>4)
#mergelenno0 <- dplyr::filter(mergewtno0, is.na(LENGTH) | LENGTH>=10)
mergelen <- dplyr::filter(mergelenno0, !is.na(LENGTH))
#over 176,000 missing Indwt:
mergeindwt <- dplyr::filter(mergelen, !is.na(INDWT))
#91,606 records don't have LW parameters:
#mergeLW <- dplyr::filter(mergeindwt, !is.na(EXPONENT_FALL_COMPL))
mergeLW <- dplyr::filter(mergeindwt, !is.na(lna))

#Calculate relative condition:
###Not sure why RelCond is missing for species like red hake (SVSPP = 077):
# cond <- dplyr::mutate(mergeLW, 
#                       predwt = (exp(COEFFICIENT_FALL_COMPL))*LENGTH**EXPONENT_FALL_COMPL,
#                       RelCond = INDWT/predwt*100)
#Relative condition from LeCren 1951:
condcalc <- dplyr::mutate(mergeLW, 
                      predwt = (exp(lna))*LENGTH^b,
                      RelCond = INDWT/predwt)


cond <- dplyr::filter(condcalc, is.na(RelCond) | RelCond<300)

#Add ITIS codes and link to SVSPP:
#connect to vpn, copy into console and fill in server and uid:
#channel <- dbutils::connect_to_database(server="",uid="")

# Gets itis codes
#itis <- dbutils::create_species_lookup(channel,species=unique(cond$SVSPP),speciesType = "SVSPP")

# clean output
#ITIScodes <- itis$data %>% 
#  dplyr::select(SVSPPsv,COMNAME,SCIENTIFIC_NAME,SPECIES_ITIS) %>% 
#  dplyr::rename(SVSPP=SVSPPsv) %>%
#  dplyr::distinct()

#save ITIS codes so connection to Oracle isn't needed:
#save(ITIScodes, file=here::here(data.dir,paste0('ITIScodes.rds')))
#load(file.path(data.dir, 'ITIScodes.rds', sep = ''))

#cond.itis <- left_join(cond, ITIScodes)
#fill in ITIS codes for ocean pout
#cond <- cond.itis %>% dplyr::mutate(ITIS= ifelse(SVSPP==193, '630979', SPECIES_ITIS))
#If needed for merges other than stockSMART, fill in ITIS codes for ocean pout, roughtail stingray, spiny butterfly ray, bullnose ray:
#cond.itis$ITIS <- ifelse(cond.itis$SVSPP==193, '630979', 
#                                                 ifelse(cond.itis$SVSPP==4, '160952', 
#                                                 ifelse(cond.itis$SVSPP==375, '160961', 
#                                                 ifelse(cond.itis$SVSPP==18, '160954', 
#                                                 ifelse(cond.itis$SVSPP==19, '564391', cond.itis$SPECIES_ITIS)))))

#check where condition is missing
#nocond <- filter(cond, is.na(RelCond))
#unique(nocond$SVSPP)
#unique(nocond$YEAR)

#unique(cond$YEAR)

#Parse condition data by EPU:
#Grab strata
#load(file.path(data.dir, 'Survdat.RData'))

#strata <- rgdal::readOGR(dsn=here::here(gis.dir),layer="EPU",verbose=F)

#For survdat package:
# strata <- sf::st_read(dsn = system.file("extdata", "epu.shp", package = "survdat"),
#                     quiet = T)

#Needed in direct data pull but not in Survdat:
#data.table::setnames(cond,"BEGLAT","LAT") # change name of column header
#data.table::setnames(cond,"BEGLON","LON")
#cond <- cond %>% dplyr::filter(!is.na(LAT)) # remove all NA's 

###Not sure why 41,511 records fewer (lost ~11%) after poststrata line:
#cond.epu <- survdat::poststrat(as.data.table(cond), strata)
#data.table::setnames(cond.epu, 'newstrata', 'EPU')
#check if scup exists, doesn't because LWparams only have unsexed 
#sort(unique(cond.epu$SEX[cond.epu$SVPP==143]))

#Paring by EPU using corrected conversions in survdat package and Wigley et all L-W params:
# cond.epu <- survdat::post_strat(as.data.table(cond), strata, areaDescription = 'EPU', na.keep = TRUE)


#If parsing by strata, change to cond.epu:
cond.epu <- cond


#View(cond.epu)
condnoEPU <- filter(cond.epu, is.na(EPU))
#condno <- filter(cond.epu, is.na(SEX))
#cond.001wgt <- subset(cond.epu, cond.epu$INDWT == 0.001)
#Remove fish that are 0.001 kg or less
#condNoTiny <- subset(cond.epu, cond.epu$INDWT > 0.005)

#summarize condition as annual average:
#calculate single standard deviation of relative condition for each species
#condstdev <- aggregate(cond.epu$RelCond, by = list('SVSPP'=cond.epu$SVSPP), sd)
#names(condstdev)[ncol(condstdev)] = 'OrigCondSD'

#calculate single standard deviation and mean of relative condition for each species and sex:
 condstdev <- group_by(cond.epu, SVSPP, SEX) %>% summarize(mean = mean(RelCond), sd = sd(RelCond))
# 
# #Remove relative conditions that are outside of 1 standard deviation
 condsd <- left_join(cond.epu, condstdev, by=c('SVSPP', 'SEX'))
 ungroup(condsd)
# 
# #condsd <- merge(condNoTiny, condstdev, by='SVSPP', all.cond.epu=T, all.condClean = F)
# #cond.sd <- subset(condsd, condsd$RelCond < (100+condsd$OrigCondSD) & condsd$RelCond > (100-condsd$OrigCondSD))
  cond.sd <- filter(condsd, RelCond < (mean+(2*sd)) & RelCond > (mean-(2*sd)))
 
 # 
# #Standard deviation after removing outliers:
# condstdev_NoOutliers <- group_by(cond.sd, SVSPP, SEX) %>% summarize(meanNoOutliers = mean(RelCond), sd = sd(RelCond))
# 
# names(condstdev_NoOutliers)[ncol(condstdev_NoOutliers)] = 'CondSD_noOutliers'
# 
# #Remove relative conditons that are outside of 1 standard deviation
# condsd_NoOutliers <- left_join(cond.sd, condstdev_NoOutliers, by=c('SVSPP', 'SEX'))
# ungroup(condsd_NoOutliers)

#cond.epu <- cond.sd %>% dplyr::filter(is.na(sex) | sex != 0) # remove all other category for sex (when I used != c(0, 4) it didn't remove all 4s)
#Only including condition that is within 1 standard deviation of mean for each species:
#cond.epu <- condsd_NoOutliers %>% dplyr::filter(is.na(sex) | sex != 4)

cond.epu <- cond.sd %>% dplyr::filter(is.na(sex) | sex != 4)

cond.epu <- cond.epu %>% dplyr::mutate(sexMF = sex)

#recoding SEX (1,2) to sex (M, F)
# cond.epu$sex[cond.epu$sexMF==1] <- 'M'
# cond.epu$sex[cond.epu$sexMF==2] <- 'F'
# cond.epu$sex[cond.epu$sexMF==0] <- 'U'

#Tried to use LOGGED_SPECIES_NAME for species names, but doesn't exist before 2001 and too many version of names
#Names for SVSPP codes '013','015','023','026','028','032','072','073','074','075','076','077','078','102','103','104','105','106','107','108','121','131','135','141','143','145','155','164','193','197':
cond.epu <- cond.epu %>% dplyr::mutate(Species = SVSPP)

#Renamed Species names to match Stock SMART names for merge:
cond.epu$Species[cond.epu$SVSPP=='13'] <- 'Smooth dogfish'
cond.epu$Species[cond.epu$SVSPP=='15'] <- 'Spiny dogfish'
cond.epu$Species[cond.epu$SVSPP=='23'] <- 'Winter skate'
cond.epu$Species[cond.epu$SVSPP=='26'] <- 'Little skate'
cond.epu$Species[cond.epu$SVSPP=='28'] <- 'Thorny skate'
cond.epu$Species[cond.epu$SVSPP=='32'] <- 'Atlantic herring'
cond.epu$Species[cond.epu$SVSPP=='72'] <- 'Silver hake'
cond.epu$Species[cond.epu$SVSPP=='73'] <- 'Atlantic cod'
cond.epu$Species[cond.epu$SVSPP=='74'] <- 'Haddock'
cond.epu$Species[cond.epu$SVSPP=='75'] <- 'Pollock'
cond.epu$Species[cond.epu$SVSPP=='76'] <- 'White hake'
cond.epu$Species[cond.epu$SVSPP=='77'] <- 'Red hake'
cond.epu$Species[cond.epu$SVSPP=='78'] <- 'Spotted hake'
cond.epu$Species[cond.epu$SVSPP=='102'] <- 'American plaice'
cond.epu$Species[cond.epu$SVSPP=='103'] <- 'Summer flounder'
cond.epu$Species[cond.epu$SVSPP=='104'] <- 'Fourspot'
cond.epu$Species[cond.epu$SVSPP=='105'] <- 'Yellowtail flounder'
cond.epu$Species[cond.epu$SVSPP=='106'] <- 'Winter flounder'
cond.epu$Species[cond.epu$SVSPP=='107'] <- 'Witch flounder'
cond.epu$Species[cond.epu$SVSPP=='108'] <- 'Windowpane'
cond.epu$Species[cond.epu$SVSPP=='121'] <- 'Atlantic mackerel'
cond.epu$Species[cond.epu$SVSPP=='131'] <- 'Butterfish'
cond.epu$Species[cond.epu$SVSPP=='135'] <- 'Bluefish'
cond.epu$Species[cond.epu$SVSPP=='141'] <- 'Black sea bass'
cond.epu$Species[cond.epu$SVSPP=='143'] <- 'Scup'
cond.epu$Species[cond.epu$SVSPP=='145'] <- 'Weakfish'
cond.epu$Species[cond.epu$SVSPP=='155'] <- 'Acadian redfish'
cond.epu$Species[cond.epu$SVSPP=='164'] <- 'Sea raven'
cond.epu$Species[cond.epu$SVSPP=='193'] <- 'Ocean pout'
cond.epu$Species[cond.epu$SVSPP=='197'] <- 'Goosefish'
cond.epu$Species[cond.epu$SVSPP=='84'] <- 'Cusk'
cond.epu$Species[cond.epu$SVSPP=='69'] <- 'Offshore hake'
cond.epu$Species[cond.epu$SVSPP=='4'] <- 'Roughtail stingray'
cond.epu$Species[cond.epu$SVSPP=='375'] <- 'Spiny butterfly ray'
cond.epu$Species[cond.epu$SVSPP=='27'] <- 'Smooth skate'
cond.epu$Species[cond.epu$SVSPP=='25'] <- 'Rosette skate'
cond.epu$Species[cond.epu$SVSPP=='24'] <- 'Clearnose skate'
cond.epu$Species[cond.epu$SVSPP=='22'] <- 'Barndoor skate'
cond.epu$Species[cond.epu$SVSPP=='19'] <- 'Bullnose ray'
cond.epu$Species[cond.epu$SVSPP=='18'] <- 'Bluntnose stingray'
cond.epu$Species[cond.epu$SVSPP=='163'] <- 'Longhorn sculpin'
cond.epu$Species[cond.epu$SVSPP=='156'] <- 'Blackbelly rosefish'
cond.epu$Species[cond.epu$SVSPP=='136'] <- 'Atlantic croaker'
cond.epu$Species[cond.epu$SVSPP=='149'] <- 'Spot'
cond.epu$Species[cond.epu$SVSPP=='35'] <- 'Atlantic menhaden'
cond.epu$Species[cond.epu$SVSPP=='192'] <- 'Atlantic wolffish'
cond.epu$Species[cond.epu$SVSPP=='360'] <- 'Atlantic sharpnose shark'
cond.epu$Species[cond.epu$SVSPP=='101'] <- 'Atlantic halibut'
cond.epu$Species[cond.epu$SVSPP=='139'] <- 'Striped bass'
cond.epu$Species[cond.epu$SVSPP=='270'] <- 'Cownose ray'

count(cond.epu, is.na(EPU))

#Raw relative condition data for Andi Delgado, Matt Robertson's PhD student:
readr::write_csv(cond.epu, here::here(out.dir,"RawData_RelCond2024.csv"))

#Output GOM raw condition data for Kim Bastile:
# RawCondGOM <- cond.epu %>% dplyr::filter(EPU == "GOM") %>%
#   dplyr::select('YEAR', 'EPU', 'Species', 'SVSPP', 
#                 'RelCond')
# readr::write_csv(RawCondGOM, here::here(out.dir,"StrataRelCond2021_GOM.csv"))

#Summarize annually and filter based on count of condition data by species
#2021: cusk, offshore hake, roughtail stingray,  spiny butterfly ray, smooth skate, rosette skate, clearnose skate, 
  #barndoor skate, bullnose ray, bluntnose stingray, longhorn sculpin, blackbelly rosefish, Atlantic croaker have more than 20 years of >3 samples each:
#After removing samples outside of 1 std. dev, cusk, smooth dogfish and blackbelly rosefish no longer have n>3 for >20 years:
annualcond <- cond.epu %>% dplyr::group_by(Species, YEAR) %>% dplyr::summarize(MeanCond = mean(RelCond), nCond = dplyr::n())
condNshelf <- dplyr::filter(annualcond, nCond>=3) %>% ungroup()
condNshelfSpp <- condNshelf %>% dplyr::add_count(Species) %>% 
  dplyr::filter(n >= 20)

condNYrSpp <- condNshelfSpp %>% dplyr::distinct(Species)

#Summarize annually by EPU (use for SOE plots)
annualcondEPU <- cond.epu %>% dplyr::group_by(Species,EPU, YEAR) %>% dplyr::summarize(MeanCond = mean(RelCond), nCond = dplyr::n())
condN <- dplyr::filter(annualcondEPU, nCond>=3) %>% ungroup()
condNSppEPU <- condN %>% dplyr::add_count(Species, EPU) %>% 
  dplyr::filter(n >= 20)

#Sarah Gaichas data request 8/1/2024 for condition by EPU based on all survey strata (including 01410-01590):
#readr::write_csv(condNSppEPU, here::here(out.dir,"AnnualRelCond2023_Fall.csv"))

#SOE Condition data renamed for submission into google form and ecodata (Dec. 21, 2023, Jan. 27, 2025):
cond_ecodata <- condNSppEPU %>% dplyr::rename(Time = YEAR, Var = Species)
rel_condition <- cond_ecodata %>% dplyr::select(Var, EPU, Time, MeanCond)
#readr::write_csv(rel_condition, here::here(out.dir,"RelCond2024_Year.csv"))


#Proportion of species below average for 2024 SOE:
# propLowCond <- condNSppEPU %>% dplyr::group_by(Species) %>% 
#   dplyr::summarize(TotalMeanCond = mean(MeanCond)) %>%
#   ungroup() %>%
#   dplyr::select(MeanCond<TotalMeanCond) %>%
#   count(Species )

#Output for socio-economic models (by EPU and length):
annualcondEPUlen <- cond.epu %>% dplyr::group_by(Species,SVSPP, EPU, YEAR, LENGTH) %>% 
  dplyr::summarize(MeanCond = mean(RelCond), StdDevCond = sd(RelCond), nCond = dplyr::n()) %>% ungroup()
#condN <- dplyr::filter(annualcondEPU, nCond>=3)
condNSppEPUlen <- annualcondEPUlen %>% dplyr::add_count(Species, EPU) 
#%>% 
#  dplyr::filter(n >= 20)

condEPUlen <- condNSppEPUlen 
#readr::write_csv(condEPUlen, here::here(out.dir,"RelCond2020_EPU_length.csv"))

#Output for socio-economic models (by year):
annualcondYear <- cond.epu %>% dplyr::group_by(Species,SVSPP, YEAR) %>% 
  dplyr::summarize(MeanCond = mean(RelCond), StdDevCond = sd(RelCond), nCond = dplyr::n()) %>% ungroup()

#saveRDS(annualcondYear,file = here::here("other",paste0("condSPP_Year.rds")))

#Output for Dynamic Factor Analysis (Scott Large):
annualcondEPU <- cond.epu %>% dplyr::group_by(Species,SVSPP, EPU, YEAR) %>% 
  dplyr::summarize(MeanCond = mean(RelCond), StdDevCond = sd(RelCond), nCond = dplyr::n()) %>% ungroup()

#saveRDS(annualcondEPU,file = here::here("other",paste0("condSPP_EPU.rds")))
#CondEPU <-  readRDS(file.path("other", 'condSPP_EPU.rds', sep = ''))

#condN <- dplyr::filter(annualcondEPU, nCond>=3)
condNSppYear <- annualcondYear %>% dplyr::add_count(Species) 
#%>% 
#  dplyr::filter(n >= 20)

condYear <- condNSppYear
#readr::write_csv(condYear, here::here(out.dir,"RelCond2020_YearEcon.csv"))

####For 2021 SOE: 
condYear <- condNSppYear %>% dplyr::select(Species, YEAR, MeanCond, StdDevCond)
#originally submitted this for 2023 SOE, but changed to include EPU on Jan. 3rd, 2024:
#readr::write_csv(condYear, here::here(out.dir,"RelCond2023_Year.csv"))

####For Jamie Behan and Lisa Kerr data request: 
# annualcondEPUYear <- cond.epu %>% dplyr::group_by(Species,SVSPP, EPU, YEAR) %>% 
#   dplyr::summarize(MeanCond = mean(RelCond), StdDevCond = sd(RelCond), nCond = dplyr::n()) %>% 
#   dplyr::filter(SVSPP==102, nCond>=3) %>% ungroup()


# condYear <- annualcondEPUYear %>% dplyr::select(Species, SVSPP, EPU, YEAR, MeanCond, StdDevCond)
# readr::write_csv(condYear, here::here(out.dir,"RelCond_AmPl_2021_EPU_Year.csv"))

#Summarize annually by Strata
annualcondStrata <- cond.epu %>% dplyr::group_by(Species,STRATUM, sexMF, YEAR) %>% dplyr::summarize(MeanCond = mean(RelCond), nCond = dplyr::n())
condN <- dplyr::filter(annualcondStrata, nCond>=3) %>% ungroup()
condNSppStrata <- condN %>% dplyr::add_count(Species, STRATUM, sexMF) %>% 
  dplyr::filter(n >= 20)
#Format output to be read into plotting function:


#View(annualcond)
#condFormat <- tidyr::gather(condNSppEPU, key= c("Species", "EPU", "sex"), value = "MeanCond", -YEAR)
#condFormat <- tidyr::spread(condNSppEPU, key = c("Species", "EPU", "sex"), value = 'MeanCond', -YEAR)

#Outputing condition data by EPU:
#readr::write_csv(condNshelfSpp, here::here(out.dir,"AnnualRelCond2024_shelf.csv"))
#
condSS <- condNSppEPU %>% dplyr::filter(EPU == "SS")
#readr::write_csv(condSS, here::here(out.dir,"AnnualRelCond2021_SS.csv"))
#
condGOM <- condNSppEPU %>% dplyr::filter(EPU == "GOM")
#readr::write_csv(condGOM, here::here(out.dir,"AnnualRelCond2021_GOM.csv"))
#
condGB <- condNSppEPU %>% dplyr::filter(EPU == "GB")
#readr::write_csv(condGB,here::here(out.dir, "AnnualRelCond2021_GB.csv"))
# #
condMAB <- condNSppEPU %>% dplyr::filter(EPU == "MAB")
# readr::write_csv(condMAB, here::here(out.dir,"AnnualRelCond2021_MAB.csv"))


#select fluke annual condition data:
#condfluke <- filter(annualcond, SVSPP==103, SEX==2)
#group_by(condfluke, YEAR, SEX)
#View(condfluke)
#fluke <- condfluke %>% drop_na(YEAR)
#describe(stom)
#unique(stom$YEAR)
#View(fluke)

#write.csv(fluke, "Fluke_Cond.csv")

#Comment out output while adding in code and running outside of the function
#return(list(condNshelfSpp=condNshelfSpp,condGB=condGB,condSS=condSS,condGOM=condGOM,condMAB=condMAB,stom = stom.epu))

#  return(cond.epu)

#}
