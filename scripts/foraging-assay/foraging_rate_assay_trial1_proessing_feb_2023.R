forage<- read.csv("raw-data/foraging-rate-assay/Foraging_rate_assay_trial1_feb_2023_data.csv")
str(forage)

library(tidyverse)
forage %>% filter(sample == "0") %>% ggplot(aes(x=plate,y=plate_reader_value,group=plate,color=as.factor(temp))) + geom_boxplot() + theme_bw()

forage %>% filter(exp=="for_controls") %>% ggplot(aes(x=plate,y=plate_reader_value,group=plate,color=as.factor(expected_value))) + geom_boxplot() + theme_bw() + facet_wrap(.~sample)

summary<- forage %>% 
  group_by(plate,sample) %>% 
  summarize(mean_read = mean(plate_reader_value),
            sd = sd(plate_reader_value),
            se = sd(plate_reader_value)/n(),
            count = n(),
            temp = unique(temp),
            resource = unique(resource))

summary %<>% mutate(type = case_when(
  startsWith(sample,"C") ~ "control",
  sample == "R" ~ "resource",
  sample == "0" ~ "blank",
  TRUE ~ "trt"
))

summary %>% 
  filter(plate=="7"|plate=="8") %>%
  ggplot(aes(x=sample,y=mean_read,color=as.factor(type),group=type)) + 
  geom_point() +
  geom_errorbar(aes(ymin=mean_read-se,ymax=mean_read+se)) +
  theme_classic() +
  facet_wrap(.~plate)
  

summary2 <- summary %>% 
  group_by(temp,resource,type) %>% 
  summarize(mean_read2 = mean(mean_read),
            sd2 = sd(mean_read)) %>% 
  mutate(ID = paste(temp,resource,type,sep="_"))

#measured samples
summary2 %>% 
  ggplot(aes(x=ID,y=mean_read2, color = type)) + 
  geom_point() + 
  geom_errorbar(aes(ymin=mean_read2-sd2,ymax=mean_read2+sd2)) + 
  theme_classic() + 
  facet_grid(cols = vars(temp), rows = vars(resource)) + 
  theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1))

#control samples across temp and resource
summary2 %>% 
  filter(type=="control") %>% 
  ggplot(aes(x=resource,y=mean_read2, color = as.factor(temp))) + 
  geom_point() + 
  geom_errorbar(aes(ymin=mean_read2-sd2,ymax=mean_read2+sd2),width=0.2) + 
  theme_classic() + 
  facet_wrap(.~temp) 

#constant 1.0mgC across temps - separate by plate tomorrow
summary2 %>% filter(type=="resource") %>% 
  ggplot(aes(x=temp,y=mean_read2, color = as.factor(temp))) + 
  geom_point() + geom_errorbar(aes(ymin=mean_read2-sd2,ymax=mean_read2+sd2),width=0.2) + 
  theme_classic() 

#calculate foraging rate: ----
#code from Hite et al. 2018 paper
# load libraries
library(tidyverse)
library(MESS)

#get type of sample for each thing
forage %<>% mutate(type = case_when(
  startsWith(sample,"C") ~ "control",
  sample == "R" ~ "resource",
  sample == "0" ~ "blank",
  TRUE ~ "trt"
))

#### Create this summary to get the means of the controls  - these means for each treatment (i.e., plate_control) are then used in the calculation below 
summary_df <- forage %>%
  group_by(plate,type,sample) %>%
  mutate(
    control_read = ifelse(
    type=="control", mean(plate_reader_value),NA),
    sample_read = ifelse(
      type=="trt", mean(plate_reader_value),NA),
    resource_read = ifelse(type=="resource",mean(plate_reader_value),NA),
    blank_read = ifelse(
      type=="blank",mean(plate_reader_value),NA
    )) %>%
  select(sample,plate, type, control_read,sample_read,blank_read,resource_read) %>%
  distinct() %>% as.data.frame()

v<- 40
t<- 8

calc_df <- summary_df %>% 
  filter(type=="control"|type=="trt") %>% 
  mutate(type2 = case_when(
    type=="control" ~ "control",
    type=="trt" ~ sample
  ))%>% group_by(plate) %>%
  mutate(
    control_mean = ifelse(
      type2=="control", mean(control_read,na.rm=T),NA),
    sample_mean = ifelse(
      type2!="sample", sample_read,NA),
    plate=unique(plate)
    ) %>% select(-c(blank_read,resource_read,control_read,sample_read)) %>%
  distinct() %>% group_by(plate,type2) %>%
    summarize(control_read=mean(control_mean,na.rm=T),
              sample_read=sum(sample_mean,na.rm=T)) 
  
control<-calc_df %>% filter(control_read>0) %>% select(-sample_read)
sample <- calc_df %>% filter(sample_read>0) %>% select(-control_read)

calc_df_2<-merge(sample,control,by="plate",all=TRUE,sort=FALSE) %>%
  mutate(feeding_rate = log(control_read/sample_read)*v/t) %>% 
  group_by(plate) %>% 
  mutate(numbering=row_number()) %>% as.data.frame()
  
calc_df_2 %>% ggplot(aes(x=numbering,y=feeding_rate)) + geom_point() + theme_classic() + facet_wrap(.~plate,nrow=3) + geom_hline(yintercept = 0)



calc_df_summary <- calc_df_2 %>% group_by(plate) %>% summarize(mean=mean(feeding_rate),se=sd(feeding_rate)/sqrt(n())) 
ggplot(calc_df_summary,aes(x=plate,y=mean))+geom_point()+geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.1) + theme_classic() + geom_hline(yintercept = 0)


















#match control mean with plate treatment
map = setNames(summary_df$control_mean,summary_df$plate_control)
df$control_mean <- map[as.character(df$plate_treatment)]

#calculate feeding rate rate
#### df$fr_sw = log(df$control_mean/df$fluor_reading) * (v/t)

# define volume and time
v <- 
  t <-
  
  df$fr_sw <- ifelse(df$experiment == "V1",log(df$control_mean/df$fluor_reading) * (v/t),
                     log(df$control_mean/df$fluor_reading) * (v/t))


#mean fluor reading per animal 
animal_mean <- df%>%
  filter(fr_sw > 0) %>% # remove negative feeding rates
  group_by(genotype_numeric, plate_treatment, animal) %>%
  summarise(mean_fluor_animal = mean(fr_sw, na.rm = TRUE))
animal_mean


