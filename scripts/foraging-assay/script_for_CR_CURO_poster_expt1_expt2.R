############################################################################################################################################################################################################################################################################################################################################################################################################################EXPERIMENT 1 - 40 mL tubes####################################################################

#load in data and check structure -----
forage<- read.csv("raw-data/foraging-rate-assay/Foraging_rate_assay_trial1_feb_2023_data.csv")
str(forage)

#load in libraries
library(tidyverse)
library(magrittr)

#summarize data ----
forage %<>% mutate(type = case_when(
  startsWith(sample,"C") ~ "control",
  sample == "R" ~ "resource",
  sample == "0" ~ "blank",
  TRUE ~ "trt"
))

summary<- forage %>% 
  group_by(plate,sample) %>% 
  summarize(mean_read = mean(plate_reader_value),
            sd = sd(plate_reader_value),
            se = sd(plate_reader_value)/n(),
            count = n(),
            temp = unique(temp),
            resource = unique(resource),
            type = unique(type))


summary2 <- summary %>% 
  group_by(temp,resource,type) %>% 
  summarize(mean_read2 = mean(mean_read),
            sd2 = sd(mean_read)) %>% 
  mutate(ID = paste(temp,resource,type,sep="_"))

#control samples across temp and resource
summary2 %>% 
  filter(type=="control") %>% 
  ggplot(aes(x=resource,y=mean_read2, color = as.factor(temp))) + 
  geom_point() + 
  geom_errorbar(aes(ymin=mean_read2-sd2,ymax=mean_read2+sd2),width=0.2) + 
  theme_classic() + 
  facet_wrap(.~temp) 

#constant 1.0mgC across temps 
summary2 %>% filter(type=="resource") %>% 
  ggplot(aes(x=temp,y=mean_read2, color = as.factor(temp))) + 
  geom_point() + geom_errorbar(aes(ymin=mean_read2-sd2,ymax=mean_read2+sd2),width=0.2) + 
  theme_classic() 

#calculate foraging rate: ----
trt <- forage %>% 
  filter(type=="trt") %>%
  mutate(sample = as.numeric(sample)) %>%
  group_by(sample) %>%
  summarize(mean_plate_reader_value = mean(plate_reader_value),
            sd_plate_reader_value = sd(plate_reader_value),
            se_plate_reader_value = sd_plate_reader_value/sqrt(n()),
            number_of_samples = n(),
            resource = unique(resource),
            temp = unique(temp),
            plate = unique(plate)
  ) %>%
  select(-c(number_of_samples))

control <- summary %>% 
  filter(type=="control") %>% 
  rename(mean_control = mean_read, se_control = se, sd_control = sd) %>% 
  select(-c(temp, resource,type))

#merge control and trt data frames together by plate so we can calculate
forage_calc <- merge(trt,control,by="plate",all = TRUE, sort = FALSE)

#### Calculate foraging rate ---- 
v<- 40 #volume of the water Daphnia were in (in mL)
t<- 8 #length of foraging rate assay (in hours)

forage_calc %<>%
  mutate(feeding_rate = log(mean_control/mean_plate_reader_value)*v/t) %>% 
  group_by(plate) %>% 
  mutate(numbering=row_number()) %>% as.data.frame()

forage_calc %>% ggplot(aes(x=numbering,y=feeding_rate)) + geom_point() + theme_classic() + facet_wrap(.~plate,nrow=3) + geom_hline(yintercept = 0)

calc_df_summary <- forage_calc %>% 
  group_by(plate) %>% 
  summarize(mean=mean(feeding_rate),
            se=sd(feeding_rate)/sqrt(n())) 

ggplot(calc_df_summary,aes(x=plate,y=mean))+geom_point()+geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.1) + theme_classic() + geom_hline(yintercept = 0)

#make a graph of this with different facets for temperature and/or resource. You'll want to add something in the calc-df_summary generation to make sure you have temperature and resource info to make graphing that easier.

#before you start working with the experiment 2 data, make sure to clear your global environment. You can do this using the broom icon under the environment tab or by using rm(list = ls())


######################################################################################################################################################################################################################################################################################################################################################################################################################################################################EXPERIMENT 2 - 15 mL tubes########################## 


#experiment: foraging rate assay experiment redo
#people on project: Katie, Chris, Gabe, and Daniel. Christopher helped with pipetting too
#purpose of exp: see how temp and resource interact to affect foraging rate. Trial one had some problems, so this one used smaller volumes for the assay (15 mL) and a little more light during the pipetting stage to help prevent errors.
#Treatments: 15, 20, 25C crossed with 0.1, 0.5, 0.1.0 mgC/L. Replicates per treatment 30. 10 control tubes per treatment. Total number of tubes - 360.

#import data and load libraries ----
#change the file path as needed so you can access the file
forage<-read.csv("processed-data/foraging-assay/Schroeder_Strauss_3_6_2023_microplate reader_expt2.csv")
library(tidyverse)
library(magrittr) # %<>%

#data cleaning, processing, initial visualizations ----
#step 1: initial visualization: look at each sample as a boxplot of the three reader values
forage %<>%
  filter(sample!="0") %>% 
  mutate(type = case_when(
    startsWith(sample,"C") ~ "control",
    sample == "R" ~ "TT",
    TRUE ~ "trt")) %>%
  group_by(plate) %>%
  mutate(numbering = case_when(
    type=="control" ~ "ctrl",
    type=="TT" ~ "TT",
    type=="trt" ~ as.character(row_number())
  ))

#insert each temp (15, 20, 25) into the filter to look at as groups of three. Too slow and confusing to look at everything all at the same time
forage %>% filter(temp=="25") %>% ggplot(aes(x=numbering,y=read,color=type))+geom_boxplot()+theme_classic()+facet_wrap(.~resource)


#step 2: summarizing 
forage_summary <- forage %>% 
  filter(sample!="0") %>% 
  mutate(type = case_when(
    startsWith(sample,"C") ~ "control",
    sample == "R" ~ "TT",
    TRUE ~ "trt"
  )) %>%
  group_by(plate,type) %>%
  summarize(mean_read = mean(read),
            sd_read = sd(read),
            se_read = sd_read/sqrt(n()),
            number_of_samples = n(),
            resource = unique(resource),
            temp = unique(temp)
  ) 

#visualize the summarized data
forage_summary %>% ggplot(aes(x=type,y=mean_read,color=as.factor(type)),group=plate) +
  geom_point(position=position_dodge(width=0.1)) +
  geom_line(aes(group=plate)) +
  geom_errorbar(aes(ymin=mean_read-se_read,ymax=mean_read+se_read),width=.1,position = position_dodge()) +
  theme_bw() +
  facet_grid(vars(resource),vars(temp))

#look at just the 1.0s between each temperature. Should be different and reflect different sizes of grazers
forage_summary %>% filter(resource=="1"&type!="TT") %>%
  ggplot(aes(x=temp,y=mean_read,group=plate,color=type)) +
  geom_point(position=position_dodge(width=0.1)) +
  geom_errorbar(aes(ymin=mean_read-se_read,ymax=mean_read+se_read),width=.1,position = position_dodge()) +
  theme_bw() 

#are the controls in each resource treatment different? (i.e. are we getting different reads for higher concentrations of algae)
forage_summary %>% 
  filter(type=="control") %>% 
  ggplot(aes(x=resource,y=mean_read, color=as.factor(temp))) +
  geom_point() +
  geom_errorbar(aes(ymin=mean_read-se_read,ymax=mean_read+se_read)) +
  theme_classic() +
  facet_wrap(.~temp)
#yes, generally increasing trend as resource increases for all temps, which is good.
#look at each control sample individually
forage %>% filter(type=="control") %>% ggplot(aes(x=sample,y=read))+geom_boxplot()+theme_classic()+facet_wrap(.~temp+resource)

# calculating foraging rate ---- 
#subdivide into control and trt frames and then merge to get separate columns for control and trt observations
#first, get new summary dataframe for trts that summarize by each sample
trt <- forage %>% 
  filter(type=="trt") %>%
  mutate(sample = as.numeric(sample)) %>%
  group_by(sample) %>%
  summarize(mean_read = mean(read),
            sd_read = sd(read),
            se_read = sd_read/sqrt(n()),
            number_of_samples = n(),
            resource = unique(resource),
            temp = unique(temp),
            plate = unique(plate)
  ) %>%
  select(-c(number_of_samples))
control <- forage_summary %>% 
  filter(type=="control") %>% 
  rename(mean_control = mean_read, se_control = se_read, sd_control = sd_read) %>% 
  select(-c(temp, resource,number_of_samples,type))

#merge control and trt data frames together by plate so we can calculate
forage_calc <- merge(trt,control,by="plate",all = TRUE, sort = FALSE)

#calculate the foraging rate:
#first set the volume and length of incubation
v <- 15 # volume in tube w/ Daphnia
t <- 8 # length of foraging rate assay (hours)

#eqn for foraging rate = ln(control_read/sample_read) * v/time. 
forage_calc %<>% mutate(feeding_rate = log(mean_control/mean_read)*v/t)

#how many samples per plate show a negative feeding rate?
forage_calc %>% mutate(feeding_rate_sign = if_else(feeding_rate > 0, "positive", "negative")) %>% count(plate,feeding_rate_sign) 

#visualizing foraging rate ---- 
#overall feeding rate graph
forage_calc %>% ggplot(aes(x=resource,y=feeding_rate,color=as.factor(resource))) +
  geom_point() +
  theme_classic() + 
  facet_wrap(.~temp) + 
  geom_hline(yintercept=0)

forage_calc %>% 
  group_by(plate) %>% 
  summarize(
    mean_feed = mean(feeding_rate),
    se_feed = sd(feeding_rate)/sqrt(n()),
    temp = unique(temp), 
    resource = unique(resource)) %>% 
  ggplot(aes(x=resource,y=mean_feed,color=as.factor(temp))) + 
  geom_point() + 
  geom_errorbar(aes(ymin=mean_feed-se_feed,ymax=mean_feed+se_feed),width=0.1) + 
  theme_classic() + 
  facet_wrap(.~temp) + 
  geom_hline(yintercept=0)

#just looking at the positive ones
forage_calc %>% filter(feeding_rate>0) %>% ggplot(aes(x=resource,y=feeding_rate,color=as.factor(resource))) +
  geom_point() +
  theme_classic() + 
  facet_wrap(.~temp) 
