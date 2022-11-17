#purpose - look at size across temperature treatments and fluctuating temp ones
setwd("~/Documents/GitHub/2022-thermal-zoop-experiments/lengthtable-raw")

library(tidyverse) #for everything
library(lubridate) #for mdy
library(magrittr) #for %<>%
library(ggplot2) #for all the plots

mort<-read.csv("lengthtable_mortality.csv")

#add some better columns - change 1 and 0 for males with male/female and mean temp for V trts to make graphing easier
mort %<>% mutate(mean_temp = if_else(endsWith(as.character(temp),"V"), "20", temp)) %>% 
  mutate(male = if_else(is.na(male), "female", "male")) %>% 
  mutate(inf_assumption = if_else(is.na(inf),"0",inf))

#filter to be only the treatments that I care about (resource - 1 mg C, not cerio, no males)
mort %<>% filter(resource>=0.6 & species == "D" & male == "female")

#reorder the temp levels
mort$temp <- factor(mort$temp, levels = c("15","20","25","2V","6V", "14V"))

#graph out raw data
mort %>% ggplot(aes(x=temp, y=length))+geom_boxplot()+theme_bw()

#time to get some averages
library(dplyr)
mort_summary <- mort %>% filter(length >= 0.01) %>% group_by(temp,) %>% summarize(mean_length = mean(length),
                                                                                  sd_length = sd(length), 
                                                                                  se_length = sd(length)/n(),
                                                                                  trt = unique(temp),
                                                                                  mean_temp = unique(mean_temp))

mort_summary %>% ggplot(aes(x=trt, y=mean_length)) +
  geom_point() +
  geom_pointrange(aes(ymin=mean_length-se_length,ymax=mean_length+se_length)) +
  theme_bw()

#average plus/minus se with points jittered
length<- mort_summary %>% ggplot(aes(x=mean_temp, y=mean_length, color=trt)) +
  geom_point(position=position_dodge(width=0.5)) +
  scale_color_manual(values=c("15"="black","20"="black","25"="black","2V"="lightgreen","6V"="mediumseagreen","14V"="forestgreen")) +
  geom_pointrange(aes(ymin=mean_length-se_length,ymax=mean_length+se_length),position=position_dodge(width=0.5)) +
  theme_bw() +
  ylab("mean length (mm)") +
  xlab("mean temperature (deg C)")

ggsave("mean_length_lifetable.png")
