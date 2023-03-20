#experiment: foraging rate assay trial 2
#people on project: Katie and Chris (ran trial), Gabe and Daniel (on bigger experiment)
#purpose of trial: trial 1 didn't see any differences between control and trt samples. Ideas on why - volume of water was too big, poor pipetting on some samples, too dark and mistakes on placement. For this trial - smaller volume (10 mL), more light during pipetting, and pipetting practice
#Treatments: all 25C with 10 replicates for 0.1 and 1.0 mgC/L. 5 controls for each. Total number of tubes: 30. 

#Import data and load libraries ----
forage<-read.csv("processed-data/foraging-assay/Schroeder_Strauss_3_1_2023_microplate reader_trial_smaller_vol.csv")
library(tidyverse)
library(magrittr) # %<>%

#Data cleaning and processing ----
#step 1: initial visualization: look at each sample as a boxplot of the three reader values
forage_boxplot <- forage %>%
  filter(sample!="0") %>% 
  mutate(type = case_when(
    startsWith(sample,"C") ~ "control",
    sample == "TT" ~ "TT",
    TRUE ~ "trt")) %>%
  group_by(plate) %>%
  mutate(numbering = case_when(
      type=="control" ~ "ctrl",
      type=="TT" ~ "TT",
      type=="trt" ~ as.character(row_number())
    ))
forage_boxplot %>% ggplot(aes(x=sample,y=read,color=type))+geom_boxplot()+theme_classic()+facet_wrap(.~plate)

#remove readings for empty wells and add a column (type) that groups controls, samples, and treated tap wells (first two steps - filter and mutate)
#then group by type and resource to get averages, standard deviation, standard error, and the number of replicates for each
forage_summary <- forage %>% 
  filter(sample!="0") %>% 
  mutate(type = case_when(
    startsWith(sample,"C") ~ "control",
    sample == "TT" ~ "TT",
    TRUE ~ "trt"
    )) %>%
  group_by(resource,type,size) %>%
  summarize(mean_read = mean(read),
            sd_read = sd(read),
            se_read = sd_read/sqrt(n()),
            number_of_samples = n(),
            temp = unique(temp),
            ) 



forage_summary %>% filter(size!="small") %>% ggplot(aes(x=resource,y=mean_read,group=type,color=as.factor(type))) +
  geom_point(position=position_dodge(width=0.1)) +
  geom_errorbar(aes(ymin=mean_read-se_read,ymax=mean_read+se_read),width=.1,position = position_dodge()) +
  theme_classic() +
  facet_wrap(.~resource)
  

#subdivide into control and trt frames and then merge to get separate columns for control and trt observations
control <- forage_summary %>% filter(type=="control")
trt <- forage_summary %>% filter(type=="trt")
