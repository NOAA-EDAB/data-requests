## working on a review paper concerning the lobster fishery (this is going back to our roots in graduate school). 
# One part of the review deals with regime shifts in the Gulf of Maine. 
# Would it be possible to get a spreadsheet with the following variables from the latest SOE?:  
## Slope water proportions in the NE Channel
# 
## Gulf Stream Positions  (both the standard and the western portion estimates) (this would be in the Mid-Atlantic report)
# 
## Cal fin Relative Abundance (Gulf of Maine only)

remotes::install_github("NOAA-EDAB/ecodata", "dev")
library(ecodata)
library(dplyr)

slope_water <- ecodata::slopewater  
gulf_stream <- ecodata::gsi
calfin <- ecodata::calanus_stage %>% 
  filter(EPU == "GOM",
         Var == "CalfinCONC_100M3")

write.csv(slope_water, "slope_water_NEChannel.csv", row.names = FALSE)
write.csv(gulf_stream, "gulf_stream_position.csv", row.names = FALSE)
write.csv(calfin, "calfin_rel_abund.csv", row.names = FALSE)

