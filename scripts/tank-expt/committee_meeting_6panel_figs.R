#figures for committee meeting

#loading files
setwd("/Users/katieschroeder/Documents/GitHub/2022-thermal-zoop-experiments")
meta <- read.csv("raw-data/tank-expt/tank_metadata.csv")
counts <- read.csv("raw-data/tank-expt/tank_counts.csv")

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
                 j_ratio = juveniles/daphnia)

#separate all communities
total_summary <- data %>% 
  filter(bucket!="30"&bucket!="26"&bucket!="23"&bucket!="22"&bucket!="63"&bucket!="64") %>%
  group_by(temp,sample_date,community) %>% 
  summarize(mean_total = mean(total,na.rm=TRUE),
            count = n(),
            sd_total = sd(total,na.rm=TRUE),
            se_total = sd(total,na.rm=T)/sqrt(count),
            mean_daphnia_inf = mean(daphnia_inf,na.rm=T),
            se_daphnia_inf = sd(daphnia_inf,na.rm=T/sqrt(count)),
            mean_daphnia_prev = mean(daphnia_prev,na.rm=T),
            se_daphnia_prev = sd(daphnia_prev,na.rm=T/sqrt(count)))

#combine DM and DCM, compare to D
data2 <- data %>% 
  filter(community!="CM") %>%
  mutate(
  community2 = ifelse(str_detect(community,"M"),"DM","D"))

total_summary2 <- data2 %>% 
  filter(bucket!="30"&bucket!="26"&bucket!="23"&bucket!="22"&bucket!="63"&bucket!="64"&bucket!="56") %>%
  group_by(temp,sample_date,community2) %>% 
  summarize(mean_total = mean(total,na.rm=TRUE),
            count = n(),
            sd_total = sd(total,na.rm=TRUE),
            se_total = sd(total,na.rm=T)/sqrt(count),
            mean_daphnia_inf = mean(daphnia_inf,na.rm=T),
            se_daphnia_inf = sd(daphnia_inf,na.rm=T/sqrt(count)),
            mean_daphnia_prev = mean(daphnia_prev,na.rm=T),
            se_daphnia_prev = sd(daphnia_prev,na.rm=T/sqrt(count)))

#new column for experiment day to clean up x-axis
total_summary$exp_day = as.double(difftime(
  total_summary$sample_date, ymd("2022-06-27"),units="days"))
total_summary2$exp_day = as.double(difftime(
  total_summary2$sample_date, ymd("2022-06-27"),units="days"))

#graph total ----
total_summary %>%
  mutate(Temperature = ifelse(str_detect(temp,"var"),"Fluctuating",temp)) %>%
  filter(community=="D") %>%
  ggplot(aes(x=exp_day,y=mean_total,group=temp,color=Temperature)) +
  geom_point(size=0.5) +
  geom_line(size=2) +
  geom_errorbar(aes(ymin=mean_total-se_total,ymax=mean_total+se_total),
                color="lightgrey",alpha=0.7,width=0.4,position=position_dodge(width=0.4))+
  scale_y_continuous(trans="log10") +
  scale_color_manual(values = c("#c5e1ef","#6cb0d6","#226e9c","#e05c5c")) +
  theme_classic() +
  ylab("Mean number of zooplankton") +
  xlab("Day of experiment")+
  theme(text = element_text(size=30))
ggsave("figures/DM_pop_over_time.png",dpi=500,height=7.5,width=13,units="in")

total_summary2$Temperature <- total_summary2$temp

total_summary2 %>%
  mutate(Temperature = ifelse(str_detect(temp,"var"),"Fluctuating",temp)) %>%
  filter(community2=="DM") %>%
  ggplot(aes(x=exp_day,y=mean_total,group=temp,color=Temperature)) +
  geom_point(size=0.5) +
  geom_line(size=2) +
  geom_errorbar(aes(ymin=mean_total-se_total,ymax=mean_total+se_total),
                color="lightgrey",alpha=0.7,width=0.4,position=position_dodge(width=0.4))+
  scale_y_continuous(trans="log10") +
  scale_color_manual(values = c("#c5e1ef","#6cb0d6","#226e9c","#e05c5c")) +
  theme_classic() +
  ylab("Mean number of zooplankton") +
  xlab("Day of experiment")+
  theme(text = element_text(size=30))
ggsave("figures/DM_DCM_pop_over_time.png",dpi=500,height=7.5,width=13,units="in")

#get total number of zooplankton
#To use trapz
library(pracma)
data %<>% mutate(days_since_hosts_added = as.Date(sample_date)-as.Date(zoop_date),
                 days_since_spores_added = as.Date(sample_date)-as.Date(spore_date))

#Try it as a for loop ):
buckets <- unique(data$bucket)
epidemic_size_df <- data.frame(bucket <- buckets)

for (i in 1:length(buckets)){
  sample_data <- data[data$bucket==buckets[i],]
  #relevant extra info
  epidemic_size_df$temp[i] <- as.character(sample_data$temp[1])
  epidemic_size_df$community[i] <- as.character(sample_data$community[1])
  epidemic_size_df$treatment[i] <- as.character(sample_data$treatment[1])
  #now try to actually integrate
  epidemic_size_df$epidemic_size[i] <- trapz(as.numeric(sample_data$days_since_spores_added),
                                             matrix(as.numeric(sample_data$daphnia_prev)))
  epidemic_size_df$epidemic_size_ind[i] <- trapz(as.numeric(sample_data$days_since_spores_added),
                                                 matrix(as.numeric(sample_data$daphnia_inf)))
  #population size
  epidemic_size_df$population_size[i] <- trapz(as.numeric(sample_data$days_since_hosts_added),
                                               matrix(as.numeric(sample_data$total)))
  #add cerio in
  epidemic_size_df$epidemic_size_c[i] <- trapz(as.numeric(sample_data$days_since_spores_added),
                                               matrix(as.numeric(sample_data$cerio_prev)))
  epidemic_size_df$epidemic_size_ind_c[i] <- trapz(as.numeric(sample_data$days_since_spores_added),
                                                   matrix(as.numeric(sample_data$cerio_inf)))
}

#summarize population size
population <- epidemic_size_df %>% 
  filter(bucket!="30"&bucket!="26"&bucket!="23"&bucket!="22"&bucket!="63"&bucket!="64"&bucket=="56") %>% #excludes buckets that went to zero (22,23,26 and 30) and buckets that had a lot of ceriodaphnia (63 and 64)
  group_by(temp,community) %>%
  summarize(mean_pop = mean(population_size),
            se_pop = sd(population_size)/sqrt(n()),
            count=n())

#raw data
ggplot(epidemic_size_df,aes(x=temp,y=population_size)) +
  geom_point()

population %>% filter(community=="DM"|community=="D") %>%
  mutate(Temperature = ifelse(str_detect(temp,"var"),"Fluctuating",temp)) %>%
  ggplot(aes(x=Temperature,y=mean_pop,fill=Temperature)) +
  geom_bar(stat="identity") +
  theme_classic() +
  geom_errorbar(aes(ymin=mean_pop-se_pop,ymax=mean_pop+se_pop),
                width=0.2,color="#000000") +
  ylab("Total population over experiment")+
  scale_fill_manual(values = c("#c5e1ef","#6cb0d6","#226e9c","#e05c5c")) +
  facet_wrap(~community,nrow=1)

population %>% filter(community=="DM") %>%
  mutate(Temperature = ifelse(str_detect(temp,"var"),"Fluctuating",temp)) %>%
  mutate(avg_pop = mean_pop/16,
         se_avg_pop = se_pop/16)
  ggplot(aes(x=Temperature,y=mean_pop,fill=Temperature)) +
  geom_bar(stat="identity") +
  theme_classic() +
  geom_errorbar(aes(ymin=mean_pop-se_pop,ymax=mean_pop+se_pop),
                width=0.2,color="#000000") +
  ylab("Total population over experiment")+
  scale_fill_manual(values = c("#c5e1ef","#6cb0d6","#226e9c","#e05c5c"))
ggsave("figures/DM_pop_integrated.png",dpi=500,height=7.5,width=6,units="in")

colnames(epidemic_size_df) [1] <- "bucket"
epidemic_size_df %>% filter(community=="DM"|community=="DCM") %>%
  filter(bucket!="30"&bucket!="26"&bucket!="23"&bucket!="22"
           &bucket!="63"&bucket!="64"
        ) %>%
  mutate(Temperature = ifelse(str_detect(temp,"var"),"Fluctuating",temp)) %>%
  ggplot(aes(x=Temperature,y=population_size)) +
  geom_boxplot(aes(fill=Temperature),alpha=0.9) +
  geom_point(size=3,color="black",alpha=0.5,
             position=position_dodge2(width=0.2))+
  theme_classic() +
  ylab("Total population over experiment")+
  scale_fill_manual(values = c("#c5e1ef","#6cb0d6","#226e9c","#e05c5c"))+
  theme(text = element_text(size=30),legend.position="none")
ggsave("figures/DM_DCM_pop_integrated_boxplot.png",dpi=500,height=9,width=7,units="in")

#now number of infected individuals
total_summary %>%
  mutate(Temperature = ifelse(str_detect(temp,"var"),"Fluctuating",temp)) %>%
  filter(community=="DM") %>%
  ggplot(aes(x=exp_day,y=mean_daphnia_inf,group=temp,color=Temperature)) +
  geom_point(size=0.5) +
  geom_line(size=2) +
  #geom_errorbar(aes(ymin=mean_daphnia_inf-se_daphnia_inf,ymax=mean_daphnia_inf+se_daphnia_inf),
                #color="lightgrey",alpha=0.7,width=0.4,position=position_dodge(width=0.4))+
  scale_color_manual(values = c("#c5e1ef","#6cb0d6","#226e9c","#e05c5c")) +
  theme_classic() +
  ylab(expression(paste("Mean number of infected ", italic("Daphnia")))) +
  xlab("Day of experiment")+
  theme(text = element_text(size=30))
ggsave("figures/DM_infected_daph_over_time.png",dpi=500,height=7.5,width=13,units="in")

total_summary2 %>%
  mutate(Temperature = ifelse(str_detect(temp,"var"),"Fluctuating",temp)) %>%
  filter(community2=="DM") %>%
  ggplot(aes(x=exp_day,y=mean_daphnia_inf,group=temp,color=Temperature)) +
  geom_point(size=0.5) +
  geom_line(size=2) +
  #geom_errorbar(aes(ymin=mean_daphnia_inf-se_daphnia_inf,ymax=mean_daphnia_inf+se_daphnia_inf),
  #color="lightgrey",alpha=0.7,width=0.4,position=position_dodge(width=0.4))+
  scale_color_manual(values = c("#c5e1ef","#6cb0d6","#226e9c","#e05c5c")) +
  theme_classic() +
  ylab(expression(paste("Mean number of infected ", italic("Daphnia")))) +
  xlab("Day of experiment")+
  theme(text = element_text(size=30))
ggsave("figures/DM_DCM_infected_daph_over_time.png",dpi=500,height=7.5,width=13,units="in")

epidemic_size_df %>% filter(community=="DM"|community=="DCM") %>%
  filter(bucket!="30"&bucket!="26"&bucket!="23"&bucket!="22"
         &bucket!="63"&bucket!="64"
  ) %>%
  mutate(Temperature = ifelse(str_detect(temp,"var"),"Fluctuating",temp)) %>%
  ggplot(aes(x=Temperature,y=epidemic_size_ind)) +
  geom_boxplot(aes(fill=Temperature),alpha=0.9) +
  geom_point(size=3,color="black",alpha=0.5,
             position=position_dodge2(width=0.2))+
  theme_classic() +
  ylab(expression(paste("Total number of infected ", italic("Daphnia ")))) +
  scale_fill_manual(values = c("#c5e1ef","#6cb0d6","#226e9c","#e05c5c"))+
  theme(text = element_text(size=30),legend.position="none") 
ggsave("figures/DM_DCM_inf_ind_integrated_boxplot.png",dpi=500,height=9,width=7,units="in")

#and prevalence
total_summary %>%
  mutate(Temperature = ifelse(str_detect(temp,"var"),"Fluctuating",temp)) %>%
  filter(community=="DM") %>%
  ggplot(aes(x=exp_day,y=mean_daphnia_prev,group=temp,color=Temperature)) +
  geom_point(size=0.5) +
  geom_line(size=2) +
  #geom_errorbar(aes(ymin=mean_daphnia_prev-se_daphnia_prev,ymax=mean_daphnia_prev+se_daphnia_prev),
  #color="lightgrey",alpha=0.7,width=0.4,position=position_dodge(width=0.4))+
  scale_color_manual(values = c("#c5e1ef","#6cb0d6","#226e9c","#e05c5c")) +
  theme_classic() +
  ylab("Mean prevalence") +
  xlab("Day of experiment")+
  theme(text = element_text(size=30))
ggsave("figures/DM_prev_over_time.png",dpi=500,height=7.5,width=13,units="in")

total_summary2 %>%
  mutate(Temperature = ifelse(str_detect(temp,"var"),"Fluctuating",temp)) %>%
  filter(community2=="DM") %>%
  ggplot(aes(x=exp_day,y=mean_daphnia_prev,group=temp,color=Temperature)) +
  geom_point(size=0.5) +
  geom_line(size=2) +
  #geom_errorbar(aes(ymin=mean_daphnia_prev-se_daphnia_prev,ymax=mean_daphnia_prev+se_daphnia_prev),
  #color="lightgrey",alpha=0.7,width=0.4,position=position_dodge(width=0.4))+
  scale_color_manual(values = c("#c5e1ef","#6cb0d6","#226e9c","#e05c5c")) +
  theme_classic() +
  ylab("Mean prevalence") +
  xlab("Day of experiment")+
  theme(text = element_text(size=30))
ggsave("figures/DM_DCM_prev_over_time.png",dpi=500,height=7.5,width=13,units="in")

epidemic_size_df %>% filter(community=="DM"|community=="DCM") %>%
  filter(bucket!="30"&bucket!="26"&bucket!="23"&bucket!="22"
         &bucket!="63"&bucket!="64"
  ) %>%
  mutate(Temperature = ifelse(str_detect(temp,"var"),"Fluctuating",temp)) %>%
  ggplot(aes(x=Temperature,y=epidemic_size)) +
  geom_boxplot(aes(fill=Temperature),alpha=0.9) +
  geom_point(size=3,color="black",alpha=0.5,
             position=position_dodge2(width=0.2))+
  theme_classic() +
  ylab("Epidemic size (prevalence days)") +
  scale_fill_manual(values = c("#c5e1ef","#6cb0d6","#226e9c","#e05c5c"))+
  theme(text = element_text(size=30)) +
  theme(legend.position = "none")
ggsave("figures/DM_DCM_prev_integrated_boxplot.png",dpi=500,height=9,width=7,units="in")

#infection assay results ---- 
inf_assay <- read.csv("processed-data/tank-expt/infection_assay_results.csv")
ggplot(inf_assay,aes(x=Temperature,y=Proportion_Infected,fill=Temperature)) +
  geom_boxplot() +
  ylab("Proportion of assay infected") +
  xlab("Temperature treatment")+
  scale_fill_manual(values = c("#c5e1ef","#6cb0d6","#226e9c","#e05c5c"))+
  theme(text = element_text(size=30),legend.position="none") +
  theme_classic()
ggsave("figures/infection_assay.png",dpi=500,height=5,width=7,units="in")


#what's going on with the prevalence and density----
epidemic_size_df %>% 
  filter(temp=="var") %>%
  filter(community!="D") %>%
  ggplot(aes(x=population_size,epidemic_size,color=temp)) +
  geom_point(size=2.5) +
  xlim(2500,12000)+
  theme_classic()

epidemic_size_df %>% filter(temp=="20"|temp=="var") %>%
  #filter(community=="DM") %>%
  ggplot(aes(x=population_size,epidemic_size,color=temp)) +
  geom_point(size=2.5) +
  xlim(2500,12000)+
  theme_classic()

epidemic_size_df %>% filter(temp=="20"|temp=="var") %>%
  ggplot(aes(x=population_size,epidemic_size,color=temp)) +
  geom_point(size=2.5) +
  xlim(2500,12000)+
  theme_classic()
