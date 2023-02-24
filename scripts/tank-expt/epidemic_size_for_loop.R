##Code to get integrated epidemic size calculations

#load in data and packages
library(tidyverse)
library(magrittr)
library(ggnewscale)
library(lubridate)

#load in data
setwd("~/Documents/GitHub/2022-thermal-zoop-experiments")
meta <- read.csv("tank_metadata.csv")
counts <- read.csv("tank_counts.csv")

#bind metadata, correct dates and data types
meta$bucket <- as.factor(meta$bucket)
counts$bucket <- as.factor(counts$bucket)
data <- left_join(counts, meta, by = "bucket")
data$community <- as.factor(data$community)
data$sample_date <- mdy(data$sample_date)
data$zoop_date <- mdy(data$zoop_date)
data$spore_date <- mdy(data$spore_date)

#add columns for date of exp start and date when spores were added
data %<>% mutate(days_since_hosts_added = as.Date(sample_date)-as.Date(zoop_date),
                 days_since_spores_added = as.Date(sample_date)-as.Date(spore_date))

#add new columns for inf summary
data %<>% mutate(total = rowSums(across(juvenile_daphnia_uninf:cerio_inf),na.rm=T), 
                 daphnia = rowSums(across(juvenile_daphnia_uninf:male_daphnia_inf),na.rm=T),
                 cerio = rowSums(across(cerio_uninf:cerio_inf)),
                 treatment = paste(community, temp, sep="_"),
                 daphnia_inf = rowSums(across(contains("daphnia_inf")),na.rm=T),
                 daphnia_sus = rowSums(across(contains("daphnia_uninf")),na.rm=T),
                 daphnia_prev = daphnia_inf/daphnia,
                 daphnia_inf_status = ifelse(daphnia_prev>0,"inf","uninf"),
                 cerio_prev = cerio_inf/cerio,
                 cerio_inf_status = ifelse(cerio_prev>0,"inf","uninf"))
                 
#for loop to get epidemic size
library(pracma)

buckets <- unique(data$bucket)
epidemic_size_df <- data.frame(bucket <- buckets)

for (i in 1:length(buckets)){
  sample_data <- data[data$bucket==buckets[i],]
  #relevant extra info
  epidemic_size_df$temp[i] <- as.character(sample_data$temp[1])
  epidemic_size_df$community[i] <- as.character(sample_data$community[1])
  epidemic_size_df$treatment[i] <- as.character(sample_data$treatment[1])
  #now try to actually integrate
  epidemic_size_df$epidemic_size[i] <- trapz(as.numeric(sample_data$days_since_spores_added),matrix(as.numeric(sample_data$daphnia_prev)))
  epidemic_size_df$epidemic_size_ind[i] <- trapz(as.numeric(sample_data$days_since_spores_added),matrix(as.numeric(sample_data$daphnia_inf)))
  #add cerio in
  epidemic_size_df$epidemic_size_c[i] <- trapz(as.numeric(sample_data$days_since_spores_added),matrix(as.numeric(sample_data$cerio_prev)))
  epidemic_size_df$epidemic_size_ind_c[i] <- trapz(as.numeric(sample_data$days_since_spores_added),matrix(as.numeric(sample_data$cerio_inf)))
}

epidemic_size_df %<>% rename(bucket = bucket....buckets) 

