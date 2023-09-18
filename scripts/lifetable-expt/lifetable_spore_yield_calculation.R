#Life table experiment spring 2022
#Spore yield calculations

#usual stuff
setwd("~/")
library(tidyverse)
library(magrittr)

#read in raw spore yield data
mort<-read.csv("Library/CloudStorage/OneDrive-SharedLibraries-UniversityofGeorgia/Strauss Lab OSE - Shared Files/Strauss Lab Sharing/Data/Lab Experiments/Life Table Spring 2022 (Suh and Schroeder)/main_mort_edit.csv")

#remove any males, any individuals that were killed by pipette, any missing individuals
mort %<>% filter(!male %in% c(1) & !missing %in% c(1) & !KBP %in% c(1) & !removed %in% c(1))

#calculate spore yield for each tube
#some samples were diluted. If this was the case, the spore water column added column will have a 1. If the sample wasn't diluted, the column will have a zero. 79 samples with no water added, 194 diluted. 
#calculation: spore_RAW = spores in 8 grids of haemocytometer. Divide that by 8 to get an average and then multiply 10,000 to get spores/mL. Each was in 500 microL, so *0.5 if not diluted to get spores/infected host or *.25 if diluted to half
mort %<>% mutate(spore_yield = case_when(
  is.na(spore_RAW) ~ NA,
  spore_water_added == 1 ~ (spore_RAW/8)*10000*0.5,
  spore_water_added == 0 ~ (spore_RAW/8)*10000*0.25,
  spore_RAW>=0 & is.na(spore_water_added) ~ (spore_RAW/8)*10000*0.5,
))

#calculate mean, var, and se
#drop uninfected individuals and those with raw spore yields <10
spore_yield <- mort %>% filter(spore_RAW>10) %>% 
  group_by(temp,resource,species) %>%
  summarize(mean_spore_yield = mean(spore_yield,na.rm=T),
            var_spore_yield = sd(spore_yield,na.rm=T),
            se_spore_yield = sd(spore_yield,na.rm=T)/sqrt(n()),
            log_spore_yield = log(mean_spore_yield),
            log_var_spore_yield = log(var_spore_yield),
            log_se_spore_yield = log(se_spore_yield))
saveRDS(spore_yield,"Documents/GitHub/2022-thermal-zoop-experiments/processed-data/lifetable-expt/spore_yield.rds")

