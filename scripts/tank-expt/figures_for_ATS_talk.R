#figures for Alex's FSU talk

setwd("/Users/katieschroeder/Documents/GitHub/2022-thermal-zoop-experiments")
meta <- read.csv("raw-data/tank-expt/tank_metadata.csv")
counts <- read.csv("raw-data/tank-expt/tank_counts.csv")

library(tidyverse)
library(magrittr)

#set bucket as a factor
meta$bucket <- as.factor(meta$bucket)
counts$bucket <- as.factor(counts$bucket)

#join together
data<-left_join(counts,meta, by="bucket")

#random reorg and calcuating relevant sums
data$community <- as.factor(data$community)
data$sample_date <- mdy(data$sample_date)
data$zoop_date <- mdy(data$zoop_date)
data$spore_date <- mdy(data$spore_date)
data %<>% mutate(total = rowSums(across(
  juvenile_daphnia_uninf:cerio_inf),na.rm=T), 
  daphnia = rowSums(across(
    juvenile_daphnia_uninf:male_daphnia_inf),na.rm=T),
  cerio = rowSums(across(cerio_uninf:cerio_inf)),
  treatment = paste(community, temp, sep="_"),
  daphnia_inf = rowSums(across(contains("daphnia_inf")),na.rm=T),
  daphnia_sus = rowSums(across(contains("daphnia_uninf")),na.rm=T),
  daphnia_prev = daphnia_inf/daphnia,
  daphnia_inf_status = ifelse(daphnia_prev>0,"inf","uninf"),
  cerio_prev = cerio_inf/cerio,
  cerio_inf_status = ifelse(cerio_prev>0,"inf","uninf"),
  d_ratio = daphnia/total,
  juveniles = juvenile_daphnia_inf+juvenile_daphnia_uninf,
  j_ratio = juveniles/daphnia,
  total_infected = cerio_inf+daphnia_inf,
  total_prev = total_infected/total)

data$exp_day = as.double(difftime(
  data$sample_date, ymd("2022-06-27"),units="days"))

#figure out if any went to 0
data %>% filter(community=="CDM"|community=="DM") %>%
  ggplot(aes(x=exp_day,y=total,group=bucket)) +
  geom_point() +
  geom_line() +
  theme_classic()

#find buckets that went extinct
data %>% filter(community=="CDM"|community=="DM") %>% 
  filter(exp_day >= 20 & total==0) %>% select(c(bucket,community,temp)) %>% distinct()

total_summary <- data %>% 
  filter(bucket!="30"&bucket!="26"&bucket!="23"&bucket!="22") %>% 
  # removes buckets that went extinct
  group_by(temp,exp_day,community) %>% 
  summarize(mean_total = mean(total,na.rm=TRUE),
            count = n(),
            sd_total = sd(total,na.rm=TRUE),
            se_total = sd(total,na.rm=T)/sqrt(count),
            mean_daphnia_inf = mean(daphnia_inf,na.rm=T),
            se_daphnia_inf = sd(daphnia_inf,na.rm=T/sqrt(count)),
            mean_daphnia_prev = mean(daphnia_prev,na.rm=T),
            se_daphnia_prev = sd(daphnia_prev,na.rm=T/sqrt(count)),
            mean_cerio_prev = mean(cerio_prev,na.rm=T),
            se_cerio_prev = sd(cerio_prev,na.rm=T)/sqrt(count),
            mean_cerio = mean(cerio,na.rm=T),
            se_cerio = sd(cerio,na.rm=T)/sqrt(count))

total_summary %>% filter(community=="CM") %>%
ggplot(aes(x=exp_day,y=mean_cerio_prev,color=Temperature,group=Temperature)) +
  geom_point() +
  geom_line() +
  #geom_errorbar(aes(ymin=mean_daphnia_prev-se_daphnia_prev,ymax=mean_daphnia_prev+se_daphnia_prev)) +
  facet_wrap(~community) +
  theme_bw()

ggplot(total_summary,aes(x=exp_day,y=mean_cerio_prev,color=temp,group=temp)) +
  geom_point() +
  geom_line() +
  #geom_errorbar(aes(ymin=mean_cerio_prev-se_cerio_prev,ymax=mean_cerio_prev+se_cerio_prev)) +
  facet_wrap(~community) +
  theme_bw()

colnames(total_summary) [1] <- "Temperature"
total_summary$Temperature <- ifelse(str_detect(total_summary$Temperature,"var"),"Fluctuating",total_summary$Temperature)

total_summary %>%
  filter(community=="DCM") %>%
  filter(exp_day >= 15) %>%
  ggplot(aes(x=exp_day,y=mean_daphnia_prev,color=Temperature,group=Temperature)) +
  scale_color_manual(values = c("#c5e1ef","#6cb0d6","#226e9c","#e05c5c")) +
  geom_point(size=3) +
  geom_line(size=1.5) +
  geom_errorbar(aes(ymin=mean_daphnia_prev-se_daphnia_prev,ymax=mean_daphnia_prev+se_daphnia_prev),color="lightgrey",alpha=0.4,width=0.4,position=position_dodge(width=0.4)) +
  theme_classic() +
  labs(y="Infection prevalence",x="Day of experiment")+
  theme(text = element_text(size=30))
ggsave("figures/DCM_prev_over_time_forATS.png",dpi=500,height=7.5,width=13,units="in")

total_summary %>%
  filter(community=="DM") %>%
  filter(exp_day >= 15) %>%
  ggplot(aes(x=exp_day,y=mean_daphnia_prev,color=Temperature,group=Temperature)) +
  scale_color_manual(values = c("#c5e1ef","#6cb0d6","#226e9c","#e05c5c")) +
  geom_point(size=3) +
  geom_line(size=1.5) +
  #geom_errorbar(aes(ymin=mean_daphnia_prev-se_daphnia_prev,ymax     =mean_daphnia_prev+se_daphnia_prev),color="lightgrey",alpha=0.4,width=0.4,position=position_dodge(width=0.4)) +
  theme_classic() +
  labs(y="Infection prevalence",x="Day of experiment")+
  theme(text = element_text(size=30))
ggsave("figures/DM_prev_over_time_forATS.png",dpi=500,height=7.5,width=13,units="in")

#---- 
# total_summary %>%
#   filter(community=="DCM") %>%
#   filter(exp_day >= 15) %>%
#   ggplot(aes(x=exp_day,y=mean_daphnia_prev,color=Temperature,group=Temperature)) +
#   scale_color_manual(values = c("#c5e1ef","#6cb0d6","#226e9c","#e05c5c")) +
#   geom_point() +
#   geom_line() +
#   #geom_errorbar(aes(ymin=mean_daphnia_prev-se_daphnia_prev,ymax     =mean_daphnia_prev+se_daphnia_prev)) +
#   theme_classic() +
#   labs(y="Infection prevalence",x="Day of experiment")
# ggsave("figures/DCM_prev_over_time_forATS2.png",dpi=500,height=7.5,width=13,units="in")

# total_summary %>%
#   filter(community=="CM") %>%
#   filter(exp_day >= 15) %>%
#   ggplot(aes(x=exp_day,y=mean_cerio,color=Temperature,group=Temperature)) +
#   scale_color_manual(values = c("#c5e1ef","#6cb0d6","#226e9c","#e05c5c")) +
#   geom_point() +
#   geom_line() +
#   #geom_errorbar(aes(ymin=mean_daphnia_prev-se_daphnia_prev,ymax     =mean_daphnia_prev+se_daphnia_prev)) +
#   theme_classic() +
#   labs(y="Cerio density (per L)",x="Day of experiment")
# ggsave("figures/CM_abund_over_time_forATS2.png",dpi=500,height=7.5,width=13,units="in")

#----

total_summary %>%
  filter(community=="DCM") %>%
  filter(exp_day >= 15) %>%
  ggplot(aes(x=exp_day,y=mean_cerio,color=Temperature,group=Temperature)) +
  scale_color_manual(values = c("#c5e1ef","#6cb0d6","#226e9c","#e05c5c")) +
  geom_point(size=3) +
  geom_line(size=1.5) +
  #geom_errorbar(aes(ymin=mean_cerio-se_cerio,ymax=mean_cerio+se_cerio),color="lightgrey",alpha=0.3,width=0.4,position=position_dodge(width=0.4)) +
  theme_classic() +
  #scale_y_continuous(trans="log10") +
  theme(text = element_text(size=30))+
  labs(y="Cerio density (per L)",x="Day of experiment")
ggsave("figures/CM_abund_over_time_forATS2.png",dpi=500,height=7.5,width=13,units="in")
