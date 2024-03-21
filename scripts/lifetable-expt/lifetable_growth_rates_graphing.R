# Graphing lifetable rates ---- 
setwd("~/")
lt.summary_factors <- readRDS("Documents/GitHub/2022-thermal-zoop-experiments/processed-data/lifetable-expt/rates_bdr.rds")

lt.summary_factors %<>% mutate(
  amp = case_when(
    temp_var=="0"~0,
    temp_var=="2"~1,
    temp_var=="6"~3,
    temp_var=="14"~7,
  ),
  katie = case_when(
    temp_var=="0"&species=="daphnia"&resource==1&inf_status=="U" ~ "uninf",
    temp_var=="0"&species=="daphnia"&resource==1&inf_status=="I" ~ "inf",
    TRUE ~ "ignore"
  )
)

#little r, intrinsic growth rate
lt.summary_factors %>% 
  filter(species=="daphnia"&resource=="1") %>% 
  filter(inf_status=="U") %>%
  #filter(temp_var=="0") %>%
  ggplot(.,aes(x=mean_temp,y=S.r,color=temp_var)) + 
  geom_point(position=position_dodge(width=0.7),size=2) + 
  geom_errorbar(aes(ymax=S.r.975,ymin=S.r.025),width=0,position=position_dodge(width=0.7),size=1) +
  scale_color_manual("Amplitude of fluctuation (°C)", values = c("black", "lightgreen","green3","forestgreen")) + 
  theme_classic(base_size = 20) +
  #facet_wrap(.~inf_status) +
  labs(x = "Mean Temperature (°C)", 
       y = expression(paste("Intrinsic growth rate ", (day^-1)))) +
  scale_x_continuous(breaks = c(15,20,25))+
  theme(legend.position = "none")+
  geom_smooth(data=subset(lt.summary_factors,katie=="uninf"),method=lm,formula = y ~ x + I(x^2),size=1.5,color=alpha("lightgrey",0.35),se=FALSE)
ggsave("Documents/GitHub/2022-thermal-zoop-experiments/figures/lifetable_uninfect_r_all.png",width=7.5,height=7,units="in",dpi=300)
  

#zoom in on mean 20
lt.summary_factors %>% 
  filter(species=="daphnia"&resource=="1"&mean_temp=="20") %>% 
  filter(inf_status=="U") %>%
  ggplot(aes(x=amp,y=S.r,color=temp_var)) + 
  geom_point() +
  geom_errorbar(aes(ymax=S.r.975,ymin=S.r.025),width=0) +
  scale_color_manual("Amplitude of fluctuation (°C)", values = c("black", "lightgreen","green3","forestgreen")) + 
  theme_classic(base_size = 15) +
  #facet_wrap(.~inf_status) +
  scale_x_continuous(breaks = c(0,1,3,7))+
  labs(x = "Magnitude of fluctuation (°C)", 
       y = expression(paste("Intrinsic growth rate ", (day^-1)))) +
  theme(legend.position = "none")
ggsave("Documents/GitHub/2022-thermal-zoop-experiments/figures/lifetable_uninfect_r.png",width=3.7,height=5.5,units="in",dpi=300)

#b, birth rate
lt.summary_factors %>% 
  filter(species=="daphnia"&resource=="1") %>% 
  filter(inf_status=="U") %>%
  #filter(temp_var=="0") %>%
  ggplot(.,aes(x=mean_temp,y=S.b,color=temp_var)) + 
  geom_point(position=position_dodge(width=0.7),size=2) + 
  geom_errorbar(aes(ymax=S.b.975,ymin=S.b.025),width=0,position=position_dodge(width=0.7),size=1) +
  scale_color_manual("Amplitude of fluctuation (°C)", values = c("black", "lightgreen","green3","forestgreen")) + 
  theme_classic(base_size = 20) +
  #facet_wrap(.~inf_status) +
  labs(x = "Mean Temperature (°C)", 
       y = expression(paste("Birth rate ", (day^-1)))) +
  scale_x_continuous(breaks = c(15,20,25))+
  theme(legend.position = "none")+
  geom_smooth(data=subset(lt.summary_factors,katie=="uninf"),method=lm,formula = y ~ x + I(x^2),size=1.5,color=alpha("lightgrey",0.35),se=FALSE)
ggsave("Documents/GitHub/2022-thermal-zoop-experiments/figures/lifetable_uninfect_b_all.png",width=7.5,height=7,units="in",dpi=300)

#zoom in on mean 20
lt.summary_factors %>% 
  filter(species=="daphnia"&resource=="1"&mean_temp=="20") %>% 
  filter(inf_status=="U") %>%
  ggplot(aes(x=amp,y=S.b,color=temp_var)) + 
  geom_point() +
  geom_errorbar(aes(ymax=S.b.975,ymin=S.b.025),width=0) +
  scale_color_manual("Amplitude of fluctuation (°C)", values = c("black", "lightgreen","green3","forestgreen")) + 
  theme_classic(base_size = 15) +
  #facet_wrap(.~inf_status) +
  scale_x_continuous(breaks = c(0,1,3,7))+
  labs(x = "Magnitude of fluctuation (°C)", 
       y = expression(paste("Birth rate ", (day^-1)))) +
  theme(legend.position = "none")
ggsave("Documents/GitHub/2022-thermal-zoop-experiments/figures/lifetable_uninfect_b.png",width=3.7,height=5.5,units="in",dpi=300)

#d, death rate
lt.summary_factors %>% 
  filter(species=="daphnia"&resource=="1") %>% 
  filter(inf_status=="U") %>%
  #filter(temp_var=="0") %>%
  ggplot(.,aes(x=mean_temp,y=S.d,color=temp_var)) + 
  geom_point(position=position_dodge(width=0.7),size=2) + 
  geom_errorbar(aes(ymax=S.d.975,ymin=S.d.025),width=0,position=position_dodge(width=0.7),size=1) +
  scale_color_manual("Amplitude of fluctuation (°C)", values = c("black", "lightgreen","green3","forestgreen")) + 
  theme_classic(base_size = 20) +
  #facet_wrap(.~inf_status) +
  labs(x = "Mean Temperature (°C)", 
       y = expression(paste("Death rate ", (day^-1)))) +
  scale_x_continuous(breaks = c(15,20,25))+
  theme(legend.position = "none")+
  geom_smooth(data=subset(lt.summary_factors,katie=="uninf"),method=lm,formula = y ~ x + I(x^2),size=1.5,color=alpha("lightgrey",0.35),se=FALSE)
ggsave("Documents/GitHub/2022-thermal-zoop-experiments/figures/lifetable_uninfect_d_all.png",width=7.5,height=7,units="in",dpi=300)


#zoom in on mean 20
lt.summary_factors %>% 
  filter(species=="daphnia"&resource=="1"&mean_temp=="20") %>% 
  filter(inf_status=="U") %>%
  ggplot(aes(x=amp,y=S.d,color=temp_var)) + 
  geom_point() +
  geom_errorbar(aes(ymax=S.d.975,ymin=S.d.025),width=0) +
  scale_color_manual("Amplitude of fluctuation (°C)", values = c("black", "lightgreen","green3","forestgreen")) + 
  theme_classic(base_size = 15) +
  #facet_wrap(.~inf_status) +
  scale_x_continuous(breaks = c(0,1,3,7))+
  labs(x = "Magnitude of fluctuation (°C)", 
       y = expression(paste("Death rate ", (day^-1)))) +
  theme(legend.position = "none")
ggsave("Documents/GitHub/2022-thermal-zoop-experiments/figures/lifetable_uninfect_d.png",width=3.7,height=5.5,units="in",dpi=300)

# graphing the difference in r between uninfected and infected groups, just 20 and var
lt.summary_factors$temp_var<-as.factor(lt.summary_factors$temp_var)
lt.summary_factors %>% filter(species=="daphnia"&resource=="1"&mean_temp=="20") %>% 
  ggplot(.,aes(x=temp_var,y=S.r,color=inf_status)) + 
  geom_point(position=position_dodge(width=0.1)) + 
  geom_errorbar(aes(ymax=S.r.975,ymin=S.r.025),width=0,position=position_dodge(width=0.1)) +
  scale_color_manual("Infection Status", values = c("red", "black")) + 
  theme_classic(base_size = 14) +
  ylab("little r") + xlab("2*fluctuation amplitude (°C)") 

lt.summary_factors_uninf <- lt.summary_factors %>% filter(inf_status=="U") %>% select(-c("S.d.975","S.d.025","S.b.975","S.b.025","S.r.975","S.r.025","resource","species","mean_temp","temp_var","temp_id")) %>% rename(ID_u = ID, S.r_u = S.r, S.d_u=S.d,S.b_u=S.b,inf = inf_status)
lt.summary_factors_inf<- lt.summary_factors %>% filter(inf_status=="I") %>% select(-c("S.d.975","S.d.025","S.b.975","S.b.025","S.r.975","S.r.025"))

lt.summary_factors_wide <- bind_cols(lt.summary_factors_inf,lt.summary_factors_uninf) 
lt.summary_factors_wide %<>% mutate(diff_d = S.d - S.d_u,
                       diff_b = S.b - S.b_u,
                       diff_r = S.r_u - S.r) %>%
  select("ID","diff_b","diff_r","diff_d") %>%
  pivot_longer(cols = starts_with("diff_"),names_to = "rate",names_prefix = "diff_",values_to = "diff")

lt.summary_factors_wide %<>% 
  separate(ID,c('species','temp_ID','resource','idk'),sep="_") %>%
  mutate(temp_var = case_when(
    temp_ID == "2V" ~ 1,
    temp_ID == "6V" ~ 3,
    temp_ID == "14V" ~ 7,
    TRUE ~ 0
  ))

lt.summary_factors_wide %>% 
  filter(resource =="1" & 
           species == "daphnia" & 
           (temp_ID != "15"&temp_ID !="20")) %>%
  ggplot(.,aes(x=factor(temp_var,levels=c('0','1','3','7')),
               y=diff,color=rate,group=rate)) + 
  geom_point(size=4)+
  theme_classic(base_size = 14)+
  ylab("difference in rate (inf-uninf)") + 
  xlab("Fluctuation amplitude (°C)")+facet_wrap(.~rate)
