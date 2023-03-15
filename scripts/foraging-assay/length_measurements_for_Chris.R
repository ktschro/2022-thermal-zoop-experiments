#purpose look at length data from foraging rate assay experiment attempt #2. For Chris's CURO poster

#load file
length<-read.csv("Daphnia Lengths 03_06_2023 - Sheet1.csv")
str(length)

#load packages
library(tidyverse)
library(magrittr)

#remove 999 data (NA's) and change no and yes to 1 and 0
length %<>% 
  filter(Length!="999") %>% 
  mutate(dead = ifelse(Dead=="no",0,1),
         eggs = ifelse(Eggs=="no",0,1),
         babies = ifelse(Babies=="no",0,1)) %>%
  select(-c(Dead,Eggs,Babies)) %>%
  rename(sample=Tube.Number, length=Length)

#need to get treatment info for each tube, so load foraging rate data and use it to match up with length data
meta<-read.csv("~/Documents/GitHub/2022-thermal-zoop-experiments/processed-data/foraging-assay/Schroeder_Strauss_3_6_2023_microplate reader_expt2.csv")

meta%<>% 
  select(c(resource,temp,sample)) %>% 
  distinct() %>%
  filter(sample!="R"&sample!="C1"&sample!="C2"&sample!="C3"&sample!="C4"&sample!="C5"&sample!="C6"&sample!="0") %>%
  rename()

#merge the two together and calculate summary info
length2<-merge(length,meta,by="sample")

length_sum <- length2 %>% 
  filter(dead!=1) %>%
  group_by(temp,resource) %>% 
  summarize(mean_length = mean(length),
            sd_length = sd(length),
            n = n(),
            moms = sum(eggs)/n,
            se_length = sd_length/sqrt(n),
            temp = unique(temp),
            resource = unique(resource))

#and visualize
length_sum %>% ggplot(aes(x=resource,y=mean_length,group=temp,color=as.factor(temp))) + geom_point() + geom_line(aes(group=temp)) + geom_errorbar(aes(ymin=mean_length-se_length,ymax=mean_length+se_length),width=0.1) + theme_classic()

length_sum %>% ggplot(aes(x=temp,y=mean_length,group=resource,color=as.factor(resource))) + geom_point() + geom_line(aes(group=resource)) + geom_errorbar(aes(ymin=mean_length-se_length,ymax=mean_length+se_length),width=0.1) + theme_classic()

#what about proportion having babies for each temp and resource
length_sum %>% ggplot(aes(x=temp,y=moms,group=resource,color=as.factor(resource))) + geom_point() + geom_line(aes(group=resource)) + theme_bw()


##look at lengths from lifetable expt ----
long <- read.csv("~/Documents/GitHub/2022-thermal-zoop-experiments/raw-data/lifetable-expt/lifetable_day_5_lengths_raw_2022.csv")
long_sum <- long %>% group_by(temp_id,resource) %>% summarize(mean_length=mean(converted_um), sd_length=sd(converted_um),num=n(),se_length=sd_length/sqrt(num),mean_temp=unique(mean_temp))

long_sum %>% filter(temp_id!="2V"&temp_id!="6V"&temp_id!="14V") %>% ggplot(aes(x=resource,y=mean_length,group=temp_id,color=as.factor(temp_id))) + geom_point() + geom_line(aes(group=temp_id)) + geom_errorbar(aes(ymin=mean_length-se_length,ymax=mean_length+se_length),width=0.1) + theme_classic()

long_sum %>% filter(temp_id!="2V"&temp_id!="6V"&temp_id!="14V") %>% ggplot(aes(x=temp_id,y=mean_length,group=resource,color=as.factor(resource))) + geom_point() + geom_line(aes(group=resource)) + geom_errorbar(aes(ymin=mean_length-se_length,ymax=mean_length+se_length),width=0.1) + theme_classic()

