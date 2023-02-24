#Code to make graph of densities over time for the tank experiment

#load packages and data
library(tidyverse)
library(lubridate)
library(magrittr)
library(ggnewscale)
setwd("~/Documents/GitHub/2022-thermal-zoop-experiments")
meta <- read.csv("tank_metadata.csv")
counts <- read.csv("tank_counts.csv")

#join metadata to raw data and change things to factors and dates that R likes to work with
meta$bucket <- as.factor(meta$bucket)
counts$bucket <- as.factor(counts$bucket)
data <- left_join(counts, meta, by = "bucket")
data$community <- as.factor(data$community)
data$sample_date <- mdy(data$sample_date)
data$zoop_date <- mdy(data$zoop_date)
data$spore_date <- mdy(data$spore_date)

#Set up calculations - total Daphnia and Cerio
data %<>% mutate(total = rowSums(across(juvenile_daphnia_uninf:cerio_inf),na.rm=T), 
                 daphnia = rowSums(across(juvenile_daphnia_uninf:male_daphnia_inf),na.rm=T),
                 cerio = rowSums(across(cerio_uninf:cerio_inf)),
                 treatment = paste(community, temp, sep="_"))

#Overall densities over time - total zoops, Daphnia, Cerio
ggplot(data,aes(x=sample_date,y=total, color=temp)) + 
  geom_point() + 
  geom_line(aes(group = bucket)) + 
  scale_color_discrete() + theme(legend.position = "none") + 
  facet_grid(rows= vars(temp), cols = vars(community)) + 
  theme_bw() + 
  ylab("Total zoop density") + 
  ggtitle("Total Zoop")
ggplot(data,aes(x=sample_date,y=daphnia, color=temp)) + 
  geom_point() + geom_line(aes(group = bucket)) + 
  scale_color_discrete() + facet_grid(rows= vars(temp), cols = vars(community)) + 
  theme_bw() + 
  ylab("Total Daphnia density") + 
  ggtitle("Daphnia")
ggplot(filter(data,community=="CM"|community=="DCM"),aes(x=sample_date,y=cerio_uninf, color=temp)) + 
  geom_point() + geom_line(aes(group = bucket)) + 
  scale_color_discrete() + 
  facet_grid(rows= vars(temp), cols = vars(community)) +
  theme_bw() + 
  ylab("Total Cerio density") + 
  ggtitle("Cerio")

#Overall densities over time (log scale) - total zoops, Daphnia, Cerio
ggplot(data,aes(x=sample_date,y=total, color=temp))+ geom_point() + 
  geom_line(aes(group = bucket)) + 
  scale_color_discrete() + 
  theme(legend.position = "none") + 
  facet_grid(rows= vars(temp), cols = vars(community)) + 
  theme_bw() + ylab("Total zoop density (log scale)") + 
  ggtitle("Total Zoop") + scale_y_continuous(trans='log10')
daphnia_log_density_fig <- ggplot(data,aes(x=sample_date,y=daphnia, color=temp)) + 
  geom_point() + 
  geom_line(aes(group = bucket)) + 
  scale_color_discrete() + 
  facet_grid(rows= vars(temp), cols = vars(community)) + 
  theme_bw() + 
  ylab("Total Daphnia density (log scale)") + 
  ggtitle("Daphnia") + scale_y_continuous(trans='log10')
cerio_log_density_fig <- ggplot(filter(data,community=="CM"|community=="DCM"),aes(x=sample_date,y=cerio_uninf, color=temp)) + 
  geom_point() + 
  geom_line(aes(group = bucket)) + 
  scale_color_discrete() + facet_grid(rows= vars(temp), cols = vars(community)) +theme_bw() + 
  ylab("Total Cerio density (log scale)") + 
  ggtitle("Cerio") + 
  scale_y_continuous(trans='log10')

#Do we see larger average populations of Daphnia in buckets without Metsch vs those with Metsch? Do we see more total zooplankton over the entire course of the experiment?
density_summary <- data %>% filter(community=="D"|community=="DM") %>% group_by(community,treatment) %>% summarize(mean_daphnia = mean(daphnia),
                                                                                                                   se_daphnia = sqrt(var(daphnia,na.rm=T)/n()),
                                                                                                                   temp = unique(temp))
ggplot(density_summary,aes(x=temp,y=mean_daphnia,color=community)) +
  geom_point() +
  theme_bw() +
  geom_pointrange(aes(ymin=mean_daphnia-se_daphnia,ymax=mean_daphnia+se_daphnia)) +
  xlab("Temperature (deg C)") + ylab("Average Daphnia density")

#Use trapz to integrate density over time and get total number of individuals (do infected individuals and prevalence since we have the for loop anyway)
library(pracma)

#Try it as a for loop ):
buckets <- unique(data$bucket)
intregrate_df <- data.frame(bucket <- buckets)

for (i in 1:length(buckets)){
  sample_data <- data[data$bucket==buckets[i],]
  #relevant extra info
  integrate_df$temp[i] <- as.character(sample_data$temp[1])
  integrate_df$community[i] <- as.character(sample_data$community[1])
  integrate_df$treatment[i] <- as.character(sample_data$treatment[1])
  #now try to actually integrate
  integrate_df$prev_[i] <- trapz(as.numeric(sample_data$days_since_spores_added),matrix(as.numeric(sample_data$daphnia_prev)))
  integrate_df$integrate_ind[i] <- trapz(as.numeric(sample_data$days_since_spores_added),matrix(as.numeric(sample_data$daphnia_inf)))
}



