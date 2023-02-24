#purpose - look at lifespan across temperature treatments and fluctuating temp ones
setwd("~/Documents/GitHub/2022-thermal-zoop-experiments/lifetable-raw")

library(tidyverse) #for everything
library(lubridate) #for mdy
library(magrittr) #for %<>%
library(ggplot2) #for all the plots

mort<-read.csv("lifetable_mortality.csv")

#calculate lifespan for animals that weren't removed from the experiment
mort$mortality_day<-mdy(mort$mortality_day)
mort$lifespan <- difftime(mort$mortality_day,"2022-04-10",units = "days")

#add some better columns
mort %<>% mutate(mean_temp = if_else(endsWith(as.character(temp),"V"), "20", temp)) %>% mutate(male = if_else(is.na(male), "female", "male"))

#filter to be only the treatments that I care about (resource - 1 mg C, not cerio, no males)
mort %<>% filter(resource>=0.6 & species == "D" & male == "female")

#reorder the temp levels
mort$temp <- factor(mort$temp, levels = c("15","20","25","2V","6V", "14V"))

#graph out raw data
mort %>% ggplot(aes(x=temp, y=lifespan))+geom_point()+theme_bw()

#time to get some averages
library(dplyr)
mort_summary <- mort %>% filter(lifespan >= 0.5) %>% group_by(temp) %>% summarize(mean_life = mean(lifespan),
                                                                                  sd = sd(lifespan), 
                                                                                  se_life = sd(lifespan)/n(),
                                                                                  trt = unique(temp),
                                                                                  mean_temp = unique(mean_temp))

mort_summary %>% ggplot(aes(x=trt, y=mean_life)) +
  geom_point() +
  geom_pointrange(aes(ymin=mean_life-se_life,ymax=mean_life+se_life)) +
  theme_bw()

#average plus/minus se with points jittered
lifespan<- mort_summary %>% ggplot(aes(x=mean_temp, y=mean_life, color=trt)) +
  geom_point(position=position_dodge(width=0.5)) +
  scale_color_manual(values=c("15"="black","20"="black","25"="black","2V"="lightgreen","6V"="mediumseagreen","14V"="forestgreen")) +
  geom_pointrange(aes(ymin=mean_life-se_life,ymax=mean_life+se_life),position=position_dodge(width=0.5)) +
  theme_bw() +
  ylab("mean lifespan") +
  xlab("mean temperature (deg C)")

ggsave("mean_lifespan_lifetable.png")



