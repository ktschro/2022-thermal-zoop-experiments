#experiment: foraging rate assay experiment redo
#people on project: Katie, Chris, Gabe, and Daniel. Christopher helped with pipetting too
#purpose of exp: see how temp and resource interact to affect foraging rate. Trial one had some problems, so this one used smaller volumes for the assay (15 mL) and a little more light during the pipetting stage to help prevent errors.
#Treatments: 15, 20, 25C crossed with 0.1, 0.5, 0.1.0 mgC/L. Replicates per treatment 30. 10 control tubes per treatment. Total number of tubes - 360.

#Import data and load libraries ----
forage<-read.csv("processed-data/foraging-assay/Schroeder_Strauss_3_6_2023_microplate reader_expt2.csv")
library(tidyverse)
library(magrittr) # %<>%

#Data cleaning and processing ----
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

#insert each temp into the filter to look at as groups of three. Too slow and confusing to look at everything all at the same time
forage %>% filter(temp=="15") %>% ggplot(aes(x=numbering,y=read,color=type))+geom_boxplot()+theme_classic()+facet_wrap(.~resource)

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

# get rid of messy samples ---- 
#what happens if we get rid of the messiest samples (i.e. ones where there was a lot of variation between reads)?
#group by plate and sample so each one is distinct for controls and treatment and get rid of ones with a treshold variance amount
forage_messy <- forage %>% 
  filter(type=="trt"|type=="control") %>% 
  group_by(plate, sample) %>% 
  summarize(mean_read = mean(read),
            sd_read = sd(read),
            resource = unique(resource),
            temp = unique(temp),
            type=unique(type))

#look at histogram of sd so we can look for some sort of arbitrary cut off
hist(forage_messy$sd_read, breaks = 50)

#break it off at 1400 for no good reason and subset the forage data
forage_messy %<>% filter(sd_read <= 500)
#takes us from 378 samples to 326

#now calculate the foraging rate for the remaining samples
trt_messy <- forage_messy %>% 
  filter(type=="trt") 
control_messy <- forage_messy %>% 
  filter(type=="control") %>% 
  rename(mean_control = mean_read, sd_control = sd_read) %>% 
  select(-c(temp, resource, sample,type))

#merge control and trt data frames together by plate so we can calculate
forage_calc_messy <- merge(trt_messy,control_messy,by="plate",all = TRUE, sort = FALSE)

#eqn for foraging rate = ln(control_read/sample_read) * v/time. 
forage_calc_messy %<>% mutate(feeding_rate = log(mean_control/mean_read)*v/t)

#graph the results with a horizontal line at 0
forage_calc_messy %>% ggplot(aes(x=resource,y=feeding_rate,color=as.factor(resource))) +
  geom_point() +
  theme_classic() + 
  facet_wrap(.~temp) + 
  geom_hline(yintercept=0)

forage_calc_messy %>% 
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
