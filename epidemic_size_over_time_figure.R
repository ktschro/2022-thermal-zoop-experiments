#script to graph epidemic size over time

#load in data and packages
library(tidyverse)
library(magrittr)
library(ggnewscale)
library(lubridate)

#load in data
setwd("~/Documents/GitHub/2022-thermal-zoop-experiments")
epidemic_size<-read.csv("epidemic_size_over_time.csv")
epidemic_size$sample_date <-mdy(epidemic_size$sample_date)

library(ggplot2)
#daphnia plots
ggplot(filter(epidemic_size,community=="DM"|community=="DCM"),aes(x=sample_date,y=epidemic_size,color=temp))+
  geom_point()+
  facet_wrap(.~community)+
  theme_bw()+
  geom_line(aes(group=bucket))
ggplot(filter(epidemic_size,community=="DM"|community=="DCM"),aes(x=sample_date,y=epidemic_size_ind,color=temp))+
  geom_point()+
  facet_wrap(.~community)+
  theme_bw()+
  geom_line(aes(group=bucket))

#cerio plots
ggplot(filter(epidemic_size,community=="CM"|community=="DCM"),aes(x=sample_date,y=epidemic_size_c,color=temp))+
  geom_point()+
  facet_wrap(.~community)+
  theme_bw()+
  geom_line(aes(group=bucket))
ggplot(filter(epidemic_size,community=="CM"|community=="DCM"),aes(x=sample_date,y=epidemic_size_ind_c,color=temp))+
  geom_point()+
  facet_wrap(.~community)+
  theme_bw()+
  geom_line(aes(group=bucket))

#now make summary plots
#summary data frame - removing buckets that fell to 0 density
summary_ep_ot <- filter(epidemic_size,bucket!="26"&bucket!="30"&bucket!="22"&bucket!="23"&bucket!="43") %>% 
  group_by(sample_date,treatment) %>% 
  summarize(mean_epidemic_size = mean(epidemic_size,na.rm=T),
            se_epidemic_size=sqrt(var(epidemic_size)/n()),
            mean_epidemic_size_ind = mean(epidemic_size_ind,na.rm=T),
            se_epidemic_size_ind=sqrt(var(epidemic_size_ind)/n()),
            mean_epidemic_size_c = mean(epidemic_size_c,na.rm=T),
            se_epidemic_size_c=sqrt(var(epidemic_size_c)/n()),
            mean_epidemic_size_ind_c = mean(epidemic_size_ind_c,na.rm=T),
            se_epidemic_size_ind_c=sqrt(var(epidemic_size_ind_c)/n()),
            temp = unique(temp),
            community = unique(community))
View(summary_ep_ot)                                                                                                                                                       

#daphnia
#prevalence days
ggplot(filter(summary_ep_ot,community!="CM"),aes(x=sample_date,y=mean_epidemic_size,color=temp))+
  geom_point()+facet_wrap(.~community)+theme_bw()+
  geom_pointrange(aes(ymin=mean_epidemic_size-se_epidemic_size,ymax=mean_epidemic_size+se_epidemic_size)) + 
  ylab("daphnia epidemic size (prevalence days)")

#infected individuals
ggplot(filter(summary_ep_ot,community!="CM"),aes(x=sample_date,y=mean_epidemic_size_ind,color=temp))+
  geom_point()+facet_wrap(.~community)+theme_bw()+
  geom_pointrange(aes(ymin=mean_epidemic_size_ind-se_epidemic_size_ind,ymax=mean_epidemic_size_ind+se_epidemic_size_ind)) + 
  ylab("daphnia epidemic size (total inf individuals)")

#cerio
#prevalence days
ggplot(filter(summary_ep_ot,community=="CM"),aes(x=sample_date,y=mean_epidemic_size_c,color=temp))+
  geom_point()+facet_wrap(.~community)+theme_bw()+
  geom_pointrange(aes(ymin=mean_epidemic_size_c-se_epidemic_size_c,ymax=mean_epidemic_size_c+se_epidemic_size_c)) + 
  ylab("daphnia epidemic size (prevalence days)")

#infected individuals
ggplot(filter(summary_ep_ot,community=="CM"),aes(x=sample_date,y=mean_epidemic_size_ind_c,color=temp))+
  geom_point()+facet_wrap(.~community)+theme_bw()+
  geom_pointrange(aes(ymin=mean_epidemic_size_ind_c-se_epidemic_size_ind_c,ymax=mean_epidemic_size_ind_c+se_epidemic_size_ind_c)) + 
  ylab("daphnia epidemic size (total inf individuals)")

#next steps: look at host density at the date of spore addition and look at ratios of cerio to daphnia and epidemic size in the DCM communities
