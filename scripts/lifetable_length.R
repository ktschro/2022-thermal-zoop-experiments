#purpose - look at size across temperature treatments and fluctuating temp ones

#packages
library(tidyverse) #for everything
library(lubridate) #for mdy
library(magrittr) #for %<>%

#step one = correct lengths that were measured at different magnifications ----
mort <- read.csv("raw-data/lifetable-expt/lifetable_mortality_raw_2022.csv")


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

mort %<>% select(-c(spore_water_added,spore_RAW,length_mag_correction,length_RAW))

#get length / age to have more of a rate
#first get age at day of measurement
mort %<>% 
  mutate(length_age = case_when(
  (species=="daphnia" & is.na(lifespan)) ~ 
    as.numeric(difftime(as.Date(end_data_date,format="%m/%d/%y"),"2022/04/05")),
  (species=="ceriodaphnia" & is.na(lifespan)) ~ 
    as.numeric(difftime(as.Date(end_data_date,format="%m/%d/%y"),"2022/04/06")),
  T ~ as.numeric(lifespan)
))

mort2 <- mort %>% filter(is.na(male) & is.na(missing) & is.na(KBP)) %>% filter(!is.na(length)) %>% mutate(length_div_age = length/length_age)
mort2$temp_var<-as.factor(mort2$temp_var)

#graph out length / age to get differences in how fast they're growing. Graph out just overall length measurements too

#filter out tube 5 because it's a wild outlier. One day old Daphnia that seems too big
mort2 %>% 
  filter(resource == "1"&species=="daphnia"&tube!="5") %>% 
  filter(!is.na(inf)) %>%
  ggplot(aes(x=mean_temp,y=length_div_age,color=temp_var,group=temp_id)) + 
  geom_boxplot() + 
  theme_classic()+
  ylab("Length/lifespan") + 
  xlab("Mean temperature (°C)") +
  scale_x_continuous(breaks = c(15,20,25)) +
  facet_wrap(.~inf)

mort2 %>% 
  filter(resource == "1"&species=="daphnia"&tube!="5") %>% 
  filter(!is.na(inf)) %>%
  ggplot(aes(x=length_age,y=length,color=temp_id,group=temp_id)) + 
  geom_point() + 
  geom_smooth(method=lm,se=FALSE) +
  theme_classic()+
  ylab("Length") + 
  xlab("Age when lifespan was measured") +
  facet_wrap(.~inf)

#graph out just overall length
mort2 %>% 
  filter(resource == "1"&species=="daphnia"&tube!="5") %>% 
  filter(!is.na(inf)) %>%
  ggplot(aes(x=mean_temp,y=length,color=as.factor(temp_var),group=temp_id)) + 
  geom_boxplot() + 
  theme_classic()+
  ylab("Length (mm)") + 
  xlab("Mean temperature (°C)") +
  scale_x_continuous(breaks = c(15,20,25)) +
  facet_wrap(.~inf) 

#look at lifespan
mort2 %>% 
  filter(resource == "1") %>% 
  filter(!is.na(inf)) %>%
  ggplot(aes(x=mean_temp,y=length_age,color=as.factor(temp_var),group=temp_id)) + 
  geom_boxplot() + 
  theme_classic()+
  ylab("Life span (days)") + 
  xlab("Mean temperature (°C)") +
  scale_x_continuous(breaks = c(15,20,25)) +
  facet_wrap(.~inf) 

#calculate some summary info
length_summary <- mort2 %>% filter()

mort3<- mort2 %>% 
  filter(resource == "1"&species=="daphnia"&tube!="5") %>% 
  filter(!is.na(inf))





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
