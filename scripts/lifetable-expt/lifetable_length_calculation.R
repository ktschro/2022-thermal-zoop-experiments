#Life table experiment spring 2022
#Length calculations

#Lengths were measured at different magnifications
#Purpose: convert measurements to mm

#conversions as follows
#at default magnification (5.6x), 1 unit is equal to 17.86 micron
#at 5x, 1 = 20
#at 4x, 1 = 25

mort<-read.csv("Library/CloudStorage/OneDrive-SharedLibraries-UniversityofGeorgia/Strauss Lab OSE - Shared Files/Strauss Lab Sharing/Data/Lab Experiments/Life Table Spring 2022 (Suh and Schroeder)/main_mort_edit.csv")

library(tidyverse)
library(magrittr)

mort %<>% filter(!male %in% c(1) & !missing %in% c(1) & !KBP %in% c(1) & !removed %in% c(1))
mort %<>% mutate(length_mm = case_when(
  lm_corr_factor == -1 ~ length_RAW * 20 / 1000,
  lm_corr_factor == -2 ~ length_RAW * 25 / 1000
))

mort$length <- c(0)

for (i in 1:nrow(mort)){
  if (!is.na(mort$length_mag_correction[i])){
    if (mort$lm_corr_factor[i] == -1){
      mort$length[i] <- mort$length_RAW[i] * 20 / 1000
    }
    else if (mort$lm_corr_factor[i] == -2){
      mort$length[i] <- mort$length_RAW[i] * 25 / 1000
    }
  }
  else
    mort$length[i] <- mort$length_RAW[i] * 17.86 / 1000
  print(i)
}

saveRDS(mort, file = here("raw_data", "main_mort_edit.rds"))
write_csv(mort, file = here("raw_data", "main_mort_edit.csv"))