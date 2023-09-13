#Life table experiment spring 2022
#Lifespan calculation

#usual stuff
setwd("~/")
library(tidyverse)
library(magrittr)

#read in data
mort<-read.csv("Library/CloudStorage/OneDrive-SharedLibraries-UniversityofGeorgia/Strauss Lab OSE - Shared Files/Strauss Lab Sharing/Data/Lab Experiments/Life Table Spring 2022 (Suh and Schroeder)/main_mort_edit.csv")

#remove any males, any individuals that were killed by pipette, any missing individuals, any individuals that were removed
mort %<>% filter(!male %in% c(1) & !missing %in% c(1) & !KBP %in% c(1) & !removed %in% c(1))

#add day of "birth" i.e. the day we harvested neonates. Different dates for Daphnia and Ceriodaphnia
mort %<>% mutate(birth_day = ifelse(species=="D",
                                   "4/5/22",
                                   "4/6/22"),
                 birthday = mdy(birth_day),
                 deathday = mdy(mortality_day),
                 lifespan_days = as.numeric(deathday-birthday))

#calculate average lifespans
lifespan <-mort %>% filter(lifespan_days>0) %>% 
  group_by(temp,resource,species) %>%
  summarize(mean_lifespan_days = mean(lifespan_days,na.rm=T),
            var_lifespan_days = sd(lifespan_days,na.rm=T),
            se_lifespan_days = sd(lifespan_days,na.rm=T)/sqrt(n()))

#save lifespan summary as an RDS
saveRDS(lifespan,"Documents/GitHub/2022-thermal-zoop-experiments/processed-data/lifetable-expt/lifespan.rds")
