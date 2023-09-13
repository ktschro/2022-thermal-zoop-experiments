#Life table experiment spring 2022
#Mortality data cleaning 
#Add spore yield calculation, lifespan, length corrections, get rid of useless columns

#usual stuff
setwd("~/")
library(tidyverse)
library(magrittr)

#read in raw spore yield data
mort<-read.csv("Library/CloudStorage/OneDrive-SharedLibraries-UniversityofGeorgia/Strauss Lab OSE - Shared Files/Strauss Lab Sharing/Data/Lab Experiments/Life Table Spring 2022 (Suh and Schroeder)/main_mort_edit.csv")

#remove any males, any individuals that were killed by pipette, any missing individuals
mort %<>% filter(!male %in% c(1) & !missing %in% c(1) & !KBP %in% c(1) & !removed %in% c(1))

#spore yield calculation - see spore yield script for details:
mort %<>% mutate(spore_yield = case_when(
  is.na(spore_RAW) ~ NA,
  spore_water_added == 1 ~ (spore_RAW/8)*10000*0.5,
  spore_water_added == 0 ~ (spore_RAW/8)*10000*0.25,
  spore_RAW>=0 & is.na(spore_water_added) ~ (spore_RAW/8)*10000*0.5,
))


#lifespan calculation - see lifespan calculation script for more details
mort %<>% mutate(birth_day = ifelse(species=="D",
                                    "4/5/22",
                                    "4/6/22"),
                 birthday = mdy(birth_day),
                 deathday = mdy(mortality_day),
                 lifespan_days = as.numeric(deathday-birthday))


