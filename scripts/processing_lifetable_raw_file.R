# Data processing for life table experiment
#Goals: remove males and missing data, convert spore yields, make a treatment ID

#read in files ----
library(magrittr)
library(tidyverse)
mort <- read.csv("lifetable-raw/lifetable_mortality.csv")

#processing - remove males and columns that don't matter ----
mort %<>% filter(is.na(male)) %>% select(-c(male,))
