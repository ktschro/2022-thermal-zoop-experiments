#analyzing 2022 tank data in 2024

#packages:
library(tidyverse)
library(magrittr)
library(ggnewscale)
library(lubridate)

#data: 
setwd("/Users/katieschroeder/Documents/GitHub/2022-thermal-zoop-experiments/raw-data/tank-expt")
meta <- read.csv("tank_metadata.csv")
counts <- read.csv("tank_counts.csv")
ysi <- read_csv("tank_ysi.csv")

#cleaning:
colnames(ysi)[5:24] <- c("temp_f","mmHg","do_per","do_conc","spc","c-us","nlfc",
                         "ohm","tds","sal","density","density_2","pH","pH_mv",
                         "phyco_rfu","phyco_ug","chlor_rfu","chlor_ug","DEP","vpos")


ysi %<>% mutate(bucket = as.factor(as.numeric(gsub("Tank ", "", Site))))
ysi %<>% mutate(temp_c = (temp_f-32)*(5/9))
ysi$Date <- mdy(ysi$Date)
meta$bucket <- as.factor(meta$bucket)
counts$bucket <- as.factor(counts$bucket)


#merge temp info with counts and calculate a bunch of prev
data<-left_join(counts,meta, by="bucket")

data$community <- as.factor(data$community)
data$sample_date <- mdy(data$sample_date)
data$zoop_date <- mdy(data$zoop_date)
data$spore_date <- mdy(data$spore_date)
data$days_since_zoops_added <- as.numeric(difftime(data$sample_date,data$zoop_date,units="days"))
data %<>% mutate(total = rowSums(across(juvenile_daphnia_uninf:cerio_inf),na.rm=T), 
                 daphnia = rowSums(across(juvenile_daphnia_uninf:male_daphnia_inf),na.rm=T),
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

#analysis one: population size ----
total_summary <- data %>% 
  group_by(temp,sample_date,community) %>% 
  dplyr::summarise(total = mean(total,na.rm=TRUE),
                   count = n(),
                   se = sd(total)/sqrt(count))

data %>% filter(community=="D") %>%
  filter(bucket!="30"&bucket!="26"&bucket!="23"&bucket!="22") %>%
  ggplot(aes(x=sample_date,y=total))+ 
  geom_point(color="lightgrey") +
  geom_line(aes(group = bucket),color="lightgrey") + 
  geom_point(data=filter(total_summary,community=="D"),color="black") + 
  geom_line(data=filter(total_summary,community=="D"),color="black")+
  scale_color_discrete() + 
  facet_wrap(~temp,nrow=1) +
  theme_classic() + 
  ylab("Population Size (# of zoops)") +
  theme(text = element_text(size = 20),legend.position="none")+
  xlab("Sampling Date")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#look at just the increase in population on log scale (easier to see without log scale though)
data %>% filter(community=="D") %>%
  filter(sample_date == "2022-06-27"|sample_date == "2022-07-05"|sample_date == "2022-07-11"|sample_date == "2022-07-14"|sample_date == "2022-07-18"|sample_date == "2022-07-20") %>%
  filter(bucket!="30"&bucket!="26"&bucket!="23"&bucket!="22") %>%
  ggplot(aes(x=sample_date,y=total,group=bucket)) +
  geom_point(color="gray7 0") +
  facet_wrap(~temp,nrow=1) +
  theme_classic() +
  geom_line(color="gray70") +
  geom_smooth(method="lm",se=F,color="lightblue",alpha=0.2) +
  labs(x="Sampling Date",y="Total Number of Zooplankton in Sample")
  #scale_y_continuous(trans='log10')

#look at DM communities 
data %>% filter(community=="DM") %>%
  filter(sample_date == "2022-06-27"|sample_date == "2022-07-05"|sample_date == "2022-07-11"|sample_date == "2022-07-14"|sample_date == "2022-07-18"|sample_date == "2022-07-20") %>%
  ggplot(aes(x=sample_date,y=total,group=bucket)) +
  geom_point(color="gray7 0") +
  facet_wrap(~temp,nrow=1) +
  theme_classic() +
  geom_line(color="gray70") +
  geom_smooth(method="lm",se=F,color="lightblue",alpha=0.2) +
  labs(x="Sampling Date",y="Total Number of Zooplankton in Sample")

#are fluctuating temp communities more stable? Look at population size changes over time
data %<>% group_by(bucket) %>%
  mutate(total_t2 = lead(total),
         t2 = lead(sample_date),
         difftime = as.numeric(difftime(t2,sample_date,units="days")),
         dN_dt = (total_t2-total)/difftime) 

mean_dN_dt <- data %>% 
  group_by(temp,community,sample_date) %>% 
  dplyr::summarise(dN_dt = mean(dN_dt,na.rm=T))


data %>% filter(community=="D") %>%
  ggplot(aes(x=sample_date,y=dN_dt)) +
  geom_point(alpha=0.2) +
  geom_point(data=filter(mean_dN_dt,community=="D"),color="black") + 
  geom_line(data=filter(mean_dN_dt,community=="D"),color="black")+
  facet_wrap(~temp) +
  theme_classic()

#use pracma to get area under the curve for total population estimate
library(pracma)

buckets <- unique(data$bucket)
pop_size_df <- data.frame(bucket <- buckets)

for (i in 1:length(buckets)){
  sample_data <- data[data$bucket==buckets[i],]
  #relevant extra info
  pop_size_df$temp[i] <- as.character(sample_data$temp[1])
  pop_size_df$community[i] <- as.character(sample_data$community[1])
  pop_size_df$treatment[i] <- as.character(sample_data$treatment[1])
  #now try to actually integrate
  pop_size_df$daphnia_pop[i] <- trapz(as.numeric(sample_data$days_since_hosts_added),matrix(as.numeric(sample_data$daphnia)))
  #add cerio in
  pop_size_df$cerio_pop[i] <- trapz(as.numeric(sample_data$days_since_hosts_added),matrix(as.numeric(sample_data$cerio)))
}

colnames(pop_size_df)[1] <- "bucket"

#plot total population as a boxplot
pop_size_df %>% filter(community == "D") %>% 
  filter(bucket!="30"&bucket!="26"&bucket!="23"&bucket!="22") %>%
  ggplot(aes(x=temp,y=daphnia_pop)) +
  geom_boxplot() +
  geom_point(size=5,alpha=0.2)+
  ylab("Integrated total population") +
  xlab("Temperature treatment")+
  theme_classic()
  
#make the same plots but for DM communities
data %>% filter(community=="DM") %>%
  #filter(bucket!="30"&bucket!="26"&bucket!="23"&bucket!="22") %>% #buckets that crashed
  ggplot(aes(x=sample_date,y=total))+ 
  geom_point(color="lightgrey") +
  geom_line(aes(group = bucket),color="lightgrey") + 
  geom_point(data=filter(total_summary,community=="DM"),color="black") + 
  geom_line(data=filter(total_summary,community=="DM"),color="black")+
  scale_color_discrete() + 
  facet_wrap(~temp,nrow=1) +
  theme_classic() + 
  ylab("Population Size (# of zoops)") +
  theme(text = element_text(size = 20),legend.position="none")+
  xlab("Sampling Date")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

pop_size_df %>% filter(community == "DM") %>% 
  #filter(bucket!="30"&bucket!="26"&bucket!="23"&bucket!="22") %>% #buckets that crashed
  ggplot(aes(x=temp,y=daphnia_pop)) +
  geom_boxplot() +
  geom_point(size=5,alpha=0.2)+
  ylab("Integrated total population") +
  xlab("Temperature treatment")+
  theme_classic()

#look at cerio just for fun
data %>% filter(community=="CM") %>%
  ggplot(aes(x=sample_date,y=total))+ 
  geom_point(color="lightgrey") +
  geom_line(aes(group = bucket),color="lightgrey") + 
  geom_point(data=filter(total_summary,community=="CM"),color="black") + 
  geom_line(data=filter(total_summary,community=="CM"),color="black")+
  scale_color_discrete() + 
  facet_wrap(~temp,nrow=1) +
  theme_classic() + 
  ylab("Population Size (# of zoops)") +
  theme(text = element_text(size = 20),legend.position="none")+
  xlab("Sampling Date")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

pop_size_df %>% filter(community == "CM") %>% 
  ggplot(aes(x=temp,y=cerio_pop)) +
  geom_boxplot() +
  geom_point(size=5,alpha=0.2)+
  ylab("Integrated total population") +
  xlab("Temperature treatment")+
  theme_classic()
  
#analysis two: infections ---- 
prev_summary <- data %>% 
  filter(community=="Daph"|community=="DM") %>%
  group_by(temp,sample_date) %>% 
  dplyr::summarize(daphnia_prev = mean(daphnia_prev,na.rm=TRUE),
                             se = sd(daphnia_prev)/sqrt(n()),
                   adult_daphnia_inf = mean(adult_daphnia_inf,na.rm=T))

#prevalence over time
ggplot(data=filter(data,community=="DCM"|community=="DM"),aes(x=sample_date,y=daphnia_prev))+ 
  geom_point(color="lightgrey") +
  geom_line(data=data,aes(group = bucket),color="lightgrey") + 
  geom_point(data=prev_summary,color="black") + 
  geom_line(data=prev_summary,color="black")+
  scale_color_discrete() + 
  facet_wrap(~temp,nrow=1) +
  theme_classic() + 
  ylab("Infection Prevalence") +
  theme(text = element_text(size = 20),legend.position="none")+
  xlab("Sampling Date") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#infected individuals over time
ggplot(data=filter(data,community=="DCM"|community=="DM"),
       aes(x=sample_date,y=adult_daphnia_inf))+ 
  geom_point(color="lightgrey") +
  geom_line(data=data,aes(group = bucket),color="lightgrey") + 
  geom_point(data=prev_summary,color="black") + 
  geom_line(data=prev_summary,color="black")+
  scale_color_discrete() + 
  facet_wrap(~temp,nrow=1) +
  theme_classic() + 
  ylab("Number of Infected Daphnia") +
  theme(text = element_text(size = 20),legend.position="none")+
  xlab("Sampling Date") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#pracma
#for loop to get epidemic size
epidemic_size_df <- data.frame(bucket <- buckets)

for (i in 1:length(buckets)){
  sample_data <- data[data$bucket==buckets[i],]
  #relevant extra info
  epidemic_size_df$temp[i] <- as.character(sample_data$temp[1])
  epidemic_size_df$community[i] <- as.character(sample_data$community[1])
  epidemic_size_df$treatment[i] <- as.character(sample_data$treatment[1])
  #now try to actually integrate
  epidemic_size_df$epidemic_size[i] <- trapz(as.numeric(sample_data$days_since_spores_added),matrix(as.numeric(sample_data$daphnia_prev)))
  epidemic_size_df$epidemic_size_ind[i] <- trapz(as.numeric(sample_data$days_since_spores_added),matrix(as.numeric(sample_data$daphnia_inf)))
  #add cerio in
  epidemic_size_df$epidemic_size_c[i] <- trapz(as.numeric(sample_data$days_since_spores_added),matrix(as.numeric(sample_data$cerio_prev)))
  epidemic_size_df$epidemic_size_ind_c[i] <- trapz(as.numeric(sample_data$days_since_spores_added),matrix(as.numeric(sample_data$cerio_inf)))
}

colnames(epidemic_size_df)[1] <- "bucket"

#infected individuals
epidemic_size_df %>% 
  filter(community=="DM"|community=="DCM") %>% 
  filter(epidemic_size_ind>10) %>% 
  ggplot(aes(x=temp,y=epidemic_size_ind)) + 
    geom_boxplot() + 
    labs(x="Temperature Treatment",y="Epidemic Size (Total Number Infected)") + 
    geom_jitter(shape = 16, position =position_jitter(0.1),alpha=0.3)+
    theme_classic()

#infection prevalence
epidemic_size_df %>% 
  filter(community=="DM"|community=="DCM") %>% 
  filter(epidemic_size_ind>10) %>% 
  ggplot(aes(x=temp,y=epidemic_size)) + 
  geom_boxplot() + 
  labs(x="Temperature Treatment",y="Epidemic Size (Prevalence Days)") + 
  geom_jitter(shape = 16, position =position_jitter(0.1),alpha=0.3)+
  theme_classic()

#summary
summary <- data %>% group_by(treatment, sample_date) %>% 
  dplyr::summarize(mean_juvenile = mean(juvenile_daphnia_uninf),
                   se_juvenile_daphnia_uninf=sqrt(var(juvenile_daphnia_uninf,na.rm=T)/n()),
                   mean_adult = mean(adult_daphnia_uninf),
                   se_adult = sqrt(var(adult_daphnia_uninf,na.rm=T)/n()),
                   mean_cerio = mean(cerio_uninf),
                   se_cerio = sqrt(var(cerio_uninf,na.rm=T)/n()),
                   mean_daphnia = mean(daphnia),
                   se_daphnia = sqrt(var(daphnia,na.rm=T)/n()),
                   mean_total = mean(total),
                   se_total = sqrt(var(total,na.rm=T)/n()),
                   mean_daphnia_inf = mean(daphnia_inf),
                   se_daphnia_inf = sqrt(var(daphnia_inf,na.rm=T)/n()),
                   mean_cerio_inf = mean(cerio_inf),
                   se_cerio_inf = sqrt(var(cerio_inf,na.rm=T)/n()),
                   mean_daphnia_prev = mean(daphnia_prev),
                   se_daphnia_prev = sqrt(var(daphnia_prev,na.rm=T)/n()),
                   mean_cerio_prev = mean(cerio_prev),
                   se_cerio_prev = sqrt(var(cerio_prev,na.rm=T)/n()),
                   mean_d_ratio = mean(d_ratio),
                   se_d_ratio = sqrt(var(d_ratio,na.rm=T)/n()),
                   mean_j_ratio = mean(j_ratio),
                   se_j_ratio = sqrt(var(j_ratio,na.rm=T)/n()),
                   temp = temp,
                   community = community)

#analysis 3: Daphnia and Ceriodaphnia competition
data %>% filter(community=="DCM") %>% 
  ggplot(aes(x=sample_date,y=d_ratio)) + 
  geom_point() +
  geom_line(aes(group = bucket)) + 
  labs(x="Sample Date",y="Relative Daphnia Abundance (1=All Daphnia)")+
  facet_wrap(vars(temp))+
  theme_classic()

#graph average d_ratio for each temp treatment
data %>% filter(community=="DCM") %>% 
  ggplot(aes(x=temp,y=d_ratio)) + 
  geom_boxplot() +
  labs(x="Temperature treatment",y="Relative Daphnia Abundance (1=All Daphnia)") +
  theme_classic()


#zoom in on early numbers (before big epidemics)
data %>% filter(community=="DCM") %>% 
  filter(sample_date == "2022-06-27"|sample_date == "2022-07-05"|
           sample_date == "2022-07-11"|sample_date == "2022-07-14"|
           sample_date == "2022-07-18"|sample_date == "2022-07-20") %>%
  ggplot(aes(x=sample_date,y=d_ratio)) + 
  geom_point(color="lightgray") +
  geom_line(aes(group = bucket),color="lightgray") + 
  labs(x="Sample Date",y="Relative Daphnia Abundance (1=All Daphnia)")+
  facet_wrap(vars(temp),nrow=1)+
  theme_classic()+
  geom_smooth(method="lm",color="gray20",se=F)
  
#actually do the lms
DCM_early <- data %>% filter(community=="DCM") %>% 
  filter(sample_date == "2022-06-27"|sample_date == "2022-07-05"|
           sample_date == "2022-07-11"|sample_date == "2022-07-14"|
           sample_date == "2022-07-18"|sample_date == "2022-07-20")

list_of_DCM_dfs = split(DCM_early, DCM_early$bucket)
list_of_DCM_dfs = list_of_DCM_dfs[49:64]

results = lapply(list_of_DCM_dfs, function(dat) lm(d_ratio ~ days_since_zoops_added, data = dat))
lapply(results, summary)

DCM_pval <- sapply(results, function(x) summary(x)$coefficients)
DCM_pval

DCM_pval <- as.data.frame(t(DCM_pval))

DCM_pval$bucket <- rownames(DCM_pval)

colnames(DCM_pval)[1:8] <- c("intercept","slope","intercept_se","slope_se",
                               "intercept_t_val","slope_t_val","intercept_p_value",
                               "slope_p_value")
DCM_pval_ordered <- DCM_pval[, c(9,1,2,3,4,5,6,7,8)]

#add temp treatment info
meta_DCM <- meta %>% filter(community=="DCM")
DCM_pval_ordered <- merge(DCM_pval_ordered,meta_DCM,by="bucket")

#boxplot of slope
DCM_pval_ordered %>%
  ggplot(aes(x=temp,y=slope)) +
  geom_boxplot() +
  theme_classic() +
  geom_point(size=5,alpha=0.2) +
  labs(x="Temperature treatment",y="Slope of lm of Daphnia:Cerio vs time")

#population growth lms ---- 
Daph_pop_early <- data %>% filter(community=="DM"|community=="D") %>% 
  filter(sample_date == "2022-06-27"|sample_date == "2022-07-05"|
           sample_date == "2022-07-11"|sample_date == "2022-07-14"|
           sample_date == "2022-07-18"|sample_date == "2022-07-20")

#initial plot
data %>% filter(community=="DM"|community=="D") %>% 
  filter(sample_date == "2022-06-27"|sample_date == "2022-07-05"|
           sample_date == "2022-07-11"|sample_date == "2022-07-14"|
           sample_date == "2022-07-18"|sample_date == "2022-07-20") %>%
  ggplot(aes(x=sample_date,y=daphnia)) + 
  geom_point(color="lightgray") +
  geom_line(aes(group = bucket),color="lightgray") + 
  labs(x="Sample Date",y="Number of Daphnia in Sample")+
  facet_wrap(vars(temp),nrow=1)+
  theme_classic()+
  scale_y_continuous(trans='log10')+
  geom_smooth(method="lm",color="gray20",se=F)

list_of_Daph_dfs = split(Daph_pop_early, Daph_pop_early$bucket)
list_of_Daph_dfs = list_of_Daph_dfs[1:32]

results_daph = lapply(list_of_Daph_dfs, function(dat) lm(log(daphnia) ~ days_since_zoops_added, data = dat))
lapply(results_daph, summary)

Daph_pval <- sapply(results_daph, function(x) summary(x)$coefficients)
Daph_pval

Daph_pval <- as.data.frame(t(Daph_pval))

Daph_pval$bucket <- rownames(Daph_pval)

colnames(Daph_pval)[1:8] <- c("intercept","slope","intercept_se","slope_se",
                             "intercept_t_val","slope_t_val","intercept_p_value",
                             "slope_p_value")
Daph_pval_ordered <- Daph_pval[, c(9,1,2,3,4,5,6,7,8)]

#add temp treatment info
meta_Daph <- meta %>% filter(community=="DM"|community=="D")
Daph_pval_ordered <- merge(Daph_pval_ordered,meta_Daph,by="bucket")

#and look at slopes
Daph_pval_ordered %>%
  ggplot(aes(x=temp,y=slope)) +
  geom_boxplot() +
  theme_classic() +
  geom_point(size=5,alpha=0.4,aes(color=community)) +
  labs(x="Temperature treatment",y="Slope of lm of log(Daphnia count) vs time")
  
