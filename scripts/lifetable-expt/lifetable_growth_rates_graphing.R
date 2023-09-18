# Graphing lifetable rates ---- 
setwd("~/")
lt.summary_factors <- readRDS("Documents/GitHub/2022-thermal-zoop-experiments/processed-data/lifetable-expt/rates_bdr.rds")

#little r, intrinsic growth rate
lt.summary_factors %>% filter(species=="daphnia"&resource=="1") %>% 
  ggplot(.,aes(x=mean_temp,y=S.r,color=temp_var)) + 
  geom_point(position=position_dodge(width=0.7)) + 
  geom_errorbar(aes(ymax=S.r.975,ymin=S.r.025),width=0,position=position_dodge(width=0.7)) +
  scale_color_manual("Amplitude of fluctuation (°C)", values = c("black", "lightgreen","green3","forestgreen")) + 
  theme_classic(base_size = 14) +
  facet_wrap(.~inf_status) +
  ylab("Intrinsic growth rate") + xlab("Mean temperature (°C)") +
  scale_x_continuous(breaks = c(15,20,25))

#zoom in on mean 20
lt.summary_factors %>% filter(species=="daphnia"&resource=="1"&mean_temp=="20") %>% 
  ggplot(.,aes(x=mean_temp,y=S.r,color=temp_var)) + 
  geom_point(position=position_dodge(width=0.7)) + 
  geom_errorbar(aes(ymax=S.r.975,ymin=S.r.025),width=0,position=position_dodge(width=0.7)) +
  scale_color_manual("Amplitude of fluctuation (°C)", values = c("black", "lightgreen","green3","forestgreen")) + 
  theme_classic(base_size = 14) +
  facet_wrap(.~inf_status) +
  ylab("Intrinsic growth rate") + xlab("Mean temperature (°C)") +
  scale_x_continuous(breaks = c(20))

#b, birth rate
lt.summary_factors %>% filter(species=="daphnia"&resource=="1") %>% 
  ggplot(.,aes(x=mean_temp,y=S.b,color=temp_var)) + 
  geom_point(position=position_dodge(width=0.7)) + 
  geom_errorbar(aes(ymax=S.b.975,ymin=S.b.025),width=0,position=position_dodge(width=0.7)) +
  scale_color_manual("Amplitude of fluctuation (°C)", values = c("black", "lightgreen","green3","forestgreen")) + 
  theme_classic(base_size = 14) +
  facet_wrap(.~inf_status) +
  ylab("Birth rate") + xlab("Mean temperature (°C)") +
  scale_x_continuous(breaks = c(15,20,25))
#zoom in on mean 20
lt.summary_factors %>% filter(species=="daphnia"&resource=="1"&mean_temp=="20") %>% 
  ggplot(.,aes(x=mean_temp,y=S.b,color=temp_var)) + 
  geom_point(position=position_dodge(width=0.7)) + 
  geom_errorbar(aes(ymax=S.b.975,ymin=S.b.025),width=0,position=position_dodge(width=0.7)) +
  scale_color_manual("Amplitude of fluctuation (°C)", values = c("black", "lightgreen","green3","forestgreen")) + 
  theme_classic(base_size = 14) +
  facet_wrap(.~inf_status) +
  ylab("Birth rate") + xlab("Mean temperature (°C)") +
  scale_x_continuous(breaks = c(20))

#d, death rate
lt.summary_factors %>% filter(species=="daphnia"&resource=="1") %>% 
  ggplot(.,aes(x=mean_temp,y=S.d,color=temp_var)) + 
  geom_point(position=position_dodge(width=0.7)) + 
  geom_errorbar(aes(ymax=S.d.975,ymin=S.d.025),width=0,position=position_dodge(width=0.7)) +
  scale_color_manual("Amplitude of fluctuation (°C)", values = c("black", "lightgreen","green3","forestgreen")) + 
  theme_classic(base_size = 14) +
  facet_wrap(.~inf_status) +
  ylab("Death rate") + xlab("Mean temperature (°C)") +
  scale_x_continuous(breaks = c(15,20,25))
#zoom in on mean 20
lt.summary_factors %>% filter(species=="daphnia"&resource=="1"&mean_temp=="20") %>% 
  ggplot(.,aes(x=mean_temp,y=S.d,color=temp_var)) + 
  geom_point(position=position_dodge(width=0.2)) + 
  geom_errorbar(aes(ymax=S.d.975,ymin=S.d.025),width=0,position=position_dodge(width=0.2)) +
  scale_color_manual("Amplitude of fluctuation (°C)", values = c("black", "lightgreen","green3","forestgreen")) + 
  theme_classic(base_size = 14) +
  facet_wrap(.~inf_status) +
  ylab("Death rate") + xlab("Mean temperature (°C)") +
  scale_x_continuous(breaks = c(20))

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
