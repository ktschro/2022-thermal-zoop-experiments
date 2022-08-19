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
                 treatment = paste(community, temp, sep="_"),
                 )

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
cerio_log_density_fig<-ggplot(filter(data,community=="CM"|community=="DCM"),aes(x=sample_date,y=cerio_uninf, color=temp))+ geom_point() + geom_line(aes(group = bucket)) + scale_color_discrete() + facet_grid(rows= vars(temp), cols = vars(community)) +theme_bw() + ylab("Total Cerio density (log scale)") + ggtitle("Cerio") + scale_y_continuous(trans='log10')
