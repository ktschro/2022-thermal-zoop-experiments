#look at early clutch sizes
spore <- readRDS("Documents/GitHub/2022-thermal-zoop-experiments/processed-data/lifetable-expt/spore_yield.rds")

library(tidyverse)
library(magrittr)

spore %<>% filter(resource==1&species=="D"&mean_temp==20)

spore %<>%
    mutate(temp_ID = case_when(
             temp == "14V" ~ "7V",
             temp == "6V" ~ "3V",
             temp == "2V" ~ "1V",
             TRUE ~ temp
           ),
           temp_cond = ifelse(
             str_detect(temp_ID,"V"),
             "fluctuating","constant"),
           mean_temp = ifelse(
             str_detect(temp_ID,"V"),
             20,as.numeric(temp)))
spore$temp_ID <- factor(spore$temp_ID,levels=c("15","20","25","1V","3V","7V"))

pd = position_dodge(width=0.4)

ggplot(data=subset(spore,temp_cond=="constant"),aes(x=mean_temp,y=mean_spore_yield)) +
  geom_point(position=pd) +
  geom_errorbar(aes(ymin=mean_spore_yield-se_spore_yield,
                ymax=mean_spore_yield+se_spore_yield),width=0.2,
                position=pd)+
  theme_classic() +
  geom_smooth(data=subset(spore,temp_cond=="constant"),
              method=lm,formula = y ~ x + I(x^2),se=FALSE)

ggplot(spore,aes(x=temp_ID,y=mean_spore_yield)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean_spore_yield-se_spore_yield,
                    ymax=mean_spore_yield+se_spore_yield),width=0.2)+
  theme_classic() 

life <- read.csv("Documents/GitHub/2022-thermal-zoop-experiments/processed-data/lifetable-expt/lifetable_edit_2022.csv")

colnames(life) [13:57] <- 1:45
colnames(life) [57] <- "inf_status"
life_long <- life %>% 
  filter(species=="daphnia"&resource==1&is.na(male)&is.na(KBP)) %>%
  relocate(inf_status) %>%
  pivot_longer(cols=!(inf_status:final_date), 
               names_to ="day",
               values_to = "offspring")

life_long$day <- as.numeric(life_long$day)
life_long$temp_id <- factor(life_long$temp_id,levels=c("15","20","25","2V","6V","14V"))

life_long$end_data_date <- mdy(life_long$end_data_date)
life_long$final_date <- mdy(life_long$final_date)

life_long %>%
  filter(final_date > "2022-04-28") %>%
  ggplot(aes(x=day,y=offspring,group=tube)) +
  geom_point()+
  geom_line()+
  theme_classic()+
  facet_wrap(~temp_id)

life_long_sum <- life_long %>% 
  filter(final_date > "2022-04-28") %>%
  group_by(tube) %>%
  reframe(temp_id=unique(temp_id),
          day=unique(day),
          inf_status=unique(inf_status),
          cumu_births=cumsum(offspring))

dub_sum <- life_long_sum %>%
  group_by(temp_id,inf_status,day) %>%
  summarize(mean_off = mean(cumu_births,na.rm=T))

dub_sum %>% filter(!is.na(inf_status)) %>%
  filter(temp_id =="14V"|temp_id=="20") %>%
  filter(inf_status=="0") %>%
  ggplot(aes(x=day,y=mean_off,color=temp_id)) +
  geom_point() +
  facet_wrap(~inf_status) +
  theme_classic()
  
life_long_sum %>% 
  filter(final_date > "2022-04-28") %>%
  ggplot(aes(x=day,y=offspring,group=tube)) +
  geom_point()+
  geom_line()+
  theme_classic()+
  facet_wrap(~temp_id)

life_long %>%
  filter(inf_status=="I") %>%
  ggplot(aes(x=exp_day,y=cumulative_offspring,group=tube)) +
  geom_point(size=0.5) +
  geom_line()+
  facet_grid(cols=vars(temp_ID))+
  theme_classic()

life_long_sum <- life_long %>% 
  filter(inf_status=="U"|inf_status=="I") %>%
  group_by(temp_ID,exp_day,inf_status) %>%
  summarize(mean_cumul = mean(cumulative_offspring,na.rm=T),
            se_cumul = sd(cumulative_offspring,na.rm=T)/sqrt(n()))
  
  

life_long_sum %>% filter(exp_day<24) %>%
  ggplot(aes(x=exp_day,y=mean_cumul,group=temp_ID,color=temp_ID)) +
    geom_point(size=0.5) +
    geom_line()+
    facet_grid(cols=vars(inf_status))+
    theme_classic()

#look at average lifespan and average number of offspring produced over that lifespan
lifespan <- readRDS("~/Documents/GitHub/2022-thermal-zoop-experiments/processed-data/lifetable-expt/lifespan.rds")

lifespan %<>% select(tube,lifespan)

dub<-merge(life,lifespan,by="tube")
dub$offspring <- rowSums(dub[13:57],na.rm=T)

dub %>%
  filter(inf_status==1) %>%
  ggplot(aes(x=lifespan,y=offspring,color=temp_id)) +
  geom_point()+
  theme_classic()

dub$avg_daily_offspring <- dub$offspring/dub$lifespan

dub$temp_id <- factor(dub$temp_id,levels=c("15","20","25","2V","6V","14V"))

ggplot(dub,aes(x=temp_id,y=avg_daily_offspring)) +
  geom_boxplot() +
  theme_classic() +
  facet_wrap(~inf_status)

ggplot(dub,aes(x=temp_id,y=lifespan)) +
  geom_boxplot() +
  theme_classic() +
  facet_wrap(~inf_status)

dub_sum2 <- dub%>% 
  filter(species=="daphnia"&resource==1&is.na(male)&is.na(KBP)) %>%
  filter(lifespan>15) %>%
  group_by(inf_status,temp_id) %>%
  summarize(avg_daily = mean(avg_daily_offspring,na.rm=T),
            sd_daily = sd(avg_daily_offspring,na.rm=T),
            se_daily = sd(avg_daily_offspring,na.rm=T)/sqrt(n()))

dub_sum2$temp_id <- factor(dub_sum2$temp_id,levels=c("15","20","25","2V","6V","14V"))

dub_sum2 %<>% 
  mutate(amplitude = case_when(
    temp_id=="2V" ~ 1,
    temp_id=="6V" ~ 3,
    temp_id=="14V" ~ 7,
    TRUE ~ 0
  )) 

dub_sum2 %>%
  filter(!is.na(inf_status)) %>%
  filter(temp_id!="15"&temp_id!="25") %>%
  ggplot(aes(x=amplitude,y=avg_daily)) +
  geom_point() +
  scale_x_continuous(breaks=c(0,1,3,7))+
  geom_errorbar(aes(ymin=avg_daily-se_daily,ymax=avg_daily+se_daily),width=0) +
  theme_classic(base_size=18) +
  labs(x="Amplitude of temperature fluctuation (°C)",y="Average daily reproduction (neonates/day)")+
  facet_wrap(~inf_status)
ggsave("Documents/GitHub/2022-thermal-zoop-experiments/figures/life_table_average_offspring.png",dpi=500,height=5,width=7,units="in")
