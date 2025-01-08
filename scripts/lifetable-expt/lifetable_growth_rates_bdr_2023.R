# Data processing for life table experiment
#Goals: calculate b,d, and r from lifetable data

#read in files, packages ----
library(bbmle)
library(tidyverse)
library(magrittr)
library(lubridate)

life <- read.csv("Library/CloudStorage/OneDrive-SharedLibraries-UniversityofGeorgia/Strauss Lab OSE - Shared Files/Strauss Lab Sharing/Data/Lab Experiments/Life Table Spring 2022 (Suh and Schroeder)/main_fitness_edit.csv")

#calculate lifespan, get treatment IDs ---- 
#add start of life for each species - daphnia 'born' on 4/5/22 and cerio 'born' on 4/6/22. Calculate lifespan based on difference between date of death and birthdate. Then remove the males
life %<>% mutate(birthdate = ifelse(species == "daphnia", "4/5/22", "4/6/22"), 
                 lifespan = as.numeric(mdy(final_date) - mdy(birthdate))) %>% filter(is.na(male))

#change inf status to inf or 0, add column for dead Daphnia
life %<>% mutate(inf_status = replace_na(inf, 0), 
                 dead = ifelse(is.na(REMOVED) & is.na(KBP), 1, 0)) %>% 
    mutate(ID = paste(species, temp_id, resource, inf_status, sep = "_"))

treatment_factors <- life %>% select(ID, species, temp_id, mean_temp, temp_var, resource, inf_status) %>% distinct()

treatments <- unique(life$ID) 
length(treatments) #length tells you how big to make the final dataframe. In this case, 30

#add in experiment metadata ----
age.at.start <- 5  #this tells the function what the starting age of animals in the spread sheet is
day.columns <- which(names(life)%in%c("X4.10.22","X4.11.22","X4.12.22","X4.13.22","X4.14.22","X4.15.22","X4.16.22",
                                      "X4.17.22","X4.18.22","X4.19.22","X4.20.22","X4.21.22","X4.22.22","X4.23.22",
                                      "X4.24.22","X4.25.22","X4.26.22","X4.27.22","X4.28.22","X4.29.22","X4.30.22",
                                      "X5.1.22","X5.2.22","X5.3.22","X5.4.22","X5.5.22","X5.6.22","X5.7.22",
                                      "X5.8.22","X5.9.22","X5.10.22","X5.11.22","X5.12.22","X5.13.22","X5.14.22",
                                      "X5.15.22","X5.16.22","X5.17.22","X5.18.22","X5.19.22","X5.20.22","X5.21.22",
                                      "X5.22.22","X5.23.22"))
# which columns in the spreadsheet have entries that correspond to # babies on each day?
max.age <- length(day.columns) + age.at.start - 1 # we use this in the calculation of death rate, in case some hosts never died

#define functions for estimating r and d ---- 

Euler = function(r, column, day){
  sum(column*exp(-r*day)) - 1}
# define the euler lotka equation. this would be a complex polynomial and really difficult/impossible to solve by hand... so we just ask the computer to do it for us

d.NLL = function(d){
  d <- exp(d) # apply exp transform to ensure positive death rate
  observed <- ddata$dead
  day <- ddata$lifespan
  NLL <- 0
  for (i in 1:length(observed)){
    NLL <- ifelse(observed[i]==1, NLL - log(d*exp(-d*day[i])),
                  NLL - (-d*max.age))}
  NLL}
# the if/else statement says lets us leverage data from hosts that died (and which day they died) but also data for hosts that were still alive at the end

little.r.calculator = function(rdat){
  # fx is mean fecundity produced by surviving moms on each day x
  fx <- colMeans(rdat[day.columns], na.rm=T) # note that moms that 
  # had previously died should be blank (or NA) so they are removed from this mean
  
  #DCS note: daphnia and ceriodaphnia started on different dates so they must use different age.at.start
  if(unique(rdat$species=="cerio")){
    age.at.start <- 4
  }
  
  # z is total number of moms at start of the experiment
  z <- nrow(rdat)
  # sx is proportion of original moms surviving to day x
  Sx <- numeric(length(day.columns))
  for (i in day.columns){
    Sx[(i-day.columns[1]+1)] <- length(na.omit(rdat[,i]))/z}
  
  # now combine survivorship and fecundity into an expression to solve for r
  day <- age.at.start:(length(Sx)+age.at.start-1)
  both <- Sx*fx
  for( k in 1:length(both)){
    if(Sx[k] == 0)
    {both[k] = 0}
    else{both[k]=Sx[k]*fx[k]}}
  
  # finally use the function from earlier to find out what value of 'r' solves the euler-lotka
  little.r <<- uniroot(Euler, interval= c(-2, 2), column=both, day=day)$root
  # need to use <<- to define outside of function (global environment)
  little.r 
}

#Loop through each treatment ---- 
lt.summary=data.frame(ID=treatments) # will fill out this summary spreadsheet as
# we loop through each clone
iterations = 10 # how many samples to use in bootstrap

for (j in 1:length(treatments)){
  print(j) # ask R to tell you where it is in the loop 
  #j=1   # if something is broken in your loop in can be helpful to set the value 
  # of j and go through the inside of the loop line by line to figure out what is wrong
  
  trtdata <- life[life$ID==treatments[j],]
  # subset the data so you are just looking at trt j. this will get copied over each time
  S.data <- trtdata 
  
  little.r.calculator(rdat=S.data) # this function saves r to global environment
  lt.summary$S.r[j] <- little.r # add it to the summary spreadsheet
  
  ddata <- S.data[!is.na(S.data$dead),] # for death data, we need to remove observations 
  # where we don't know whether host died or not (in practice, user error during experiment)
  d <- ifelse(mean(ddata$dead) == 0, 0,   #this in case nothing died..
              exp(coef(mle2(minuslogl=d.NLL, start=list(d=log(0.05)), 
                            data=list(ddata=ddata), skip.hessian=T))))
  lt.summary$S.d[j] = as.numeric(d) # save to summary
  lt.summary$S.b[j] = lt.summary$S.r[j] + lt.summary$S.d[j] # and calculate b as b = r+d
  
  # now create another summary sheet where we'll save all of the bootstrapped
  # parameter estimates.. make it as long as # iterations you decided earlier
  boot.summary <- data.frame(iterations=1:iterations)
  for(k in 1:iterations){
    boot.rows <- sample(1:nrow(S.data), size=nrow(S.data), replace=TRUE)
    S.boot.data <- S.data[boot.rows,]
    little.r.calculator(rdat=S.boot.data)
    boot.summary$r.boot[k] <- little.r
    ddata <- S.boot.data #[!is.na(S.boot.data$dead),]
    d <- ifelse(mean(ddata$dead) == 0, 0,   #this in case nothing died..
                exp(coef(mle2(minuslogl=d.NLL, start=list(d=log(0.05)),
                              data=list(ddata=ddata), skip.hessian=T))))
    boot.summary$d.boot[k] <- as.numeric(d)
    boot.summary$b.boot[k] <- boot.summary$d.boot[k] + boot.summary$r.boot[k]
  }
  
  # save upper and lower 95% CI's from bootstrapped params:
  lt.summary$S.r.975[j] <- quantile(boot.summary$r.boot, probs=seq(0.025, 0.975, 0.95))[2]  
  lt.summary$S.r.025[j] <- quantile(boot.summary$r.boot, probs=seq(0.025, 0.975, 0.95))[1]  
  lt.summary$S.d.975[j] <- quantile(boot.summary$d.boot, probs=seq(0.025, 0.975, 0.95))[2]  
  lt.summary$S.d.025[j] <- quantile(boot.summary$d.boot, probs=seq(0.025, 0.975, 0.95))[1]  
  lt.summary$S.b.975[j] <- quantile(boot.summary$b.boot, probs=seq(0.025, 0.975, 0.95))[2]  
  lt.summary$S.b.025[j] <- quantile(boot.summary$b.boot, probs=seq(0.025, 0.975, 0.95))[1]  
  
}

# take a look at the results!  (you can save as csv if you want)
lt.summary

lt.summary_factors <- left_join(lt.summary, treatment_factors, by = "ID")

lt.summary_factors$ID <- factor(lt.summary_factors$ID, levels = c(   "daphnia_15_0.1_0", "daphnia_15_0.1_1", 
                                                                     "daphnia_15_0.5_0", "daphnia_15_0.5_1", 
                                                                     "daphnia_15_1_0",   "daphnia_15_1_1",
                                                                     "cerio_15_1_0",     "cerio_15_1_1", 
                                                                     "daphnia_20_0.1_0", "daphnia_20_0.1_1", 
                                                                     "daphnia_20_0.5_0", "daphnia_20_0.5_1",
                                                                     "daphnia_20_1_0",   "daphnia_20_1_1",
                                                                     "cerio_20_1_0",     "cerio_20_1_1",
                                                                     "daphnia_2V_1_0",   "daphnia_2V_1_1",  
                                                                     "daphnia_6V_1_0",   "daphnia_6V_1_1",   
                                                                     "daphnia_14V_1_0",  "daphnia_14V_1_1",     
                                                                     "daphnia_25_0.1_0", "daphnia_25_0.1_1", 
                                                                     "daphnia_25_0.5_0", "daphnia_25_0.5_1", 
                                                                     "daphnia_25_1_0",   "daphnia_25_1_1",     
                                                                     "cerio_25_1_0",     "cerio_25_1_1"))
lt.summary_factors$inf_status <- ifelse(lt.summary_factors$inf_status==1,"I","U")
lt.summary_factors$temp_var <- as.factor(lt.summary_factors$temp_var)


## save lt.summary as an RDS
saveRDS(lt.summary_factors,"Documents/GitHub/2022-thermal-zoop-experiments/processed-data/lifetable-expt/rates_bdr.rds")

# Graphing ---- 
lt.summary_factors %<>% 
  mutate(magnitude = case_when(
  temp_var == "2" ~ "1",
  temp_var == "6" ~ "3",
  temp_var == "14" ~ "7",
  TRUE ~ "0"
),
  temp_var_inf = paste(temp_var,inf_status,sep="_"))
  
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
ggsave("Documents/GitHub/2022-thermal-zoop-experiments/figures/little_r_all_temps.png")

#zoom in on mean 20
lt.summary_factors %>% filter(species=="daphnia"&resource=="1"&mean_temp=="20") %>% 
  ggplot(.,aes(x=magnitude,y=S.r,color=temp_var)) + 
  geom_point(position=position_dodge(width=0.7)) + 
  geom_errorbar(aes(ymax=S.r.975,ymin=S.r.025),width=0,position=position_dodge(width=0.7)) +
  scale_color_manual("Amplitude of fluctuation (°C)", values = c("black", "lightgreen","green3","forestgreen")) + 
  theme_classic(base_size = 14) +
  facet_wrap(.~inf_status) +
  ylab("Intrinsic growth rate") + 
  xlab("Amplitude of temperature fluctuation (°C)") +
  theme(legend.position = "none")
ggsave("Documents/GitHub/2022-thermal-zoop-experiments/figures/little_r_20_fluct.png")

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

#zoom in on mean 20, no facet ----
#birth rate
lt.summary_factors %>% filter(species=="daphnia"&resource=="1"&mean_temp=="20") %>% 
  ggplot(aes(x=magnitude,y=S.b,color=inf_status,shape=inf_status)) + 
  geom_point(position=position_dodge(width=0.15),size=4) + 
  geom_errorbar(aes(ymax=S.b.975,ymin=S.b.025),width=0,position=position_dodge(width=0.15),linewidth=1.5) +
  scale_color_manual("Amplitude of fluctuation (°C)", values = c("red3", "black")) + 
  theme_classic(base_size = 20) +
  ylab("Birth rate") + 
  xlab("Amplitude of temperature fluctuation (°C)") + theme(legend.position = "none")
ggsave("Documents/GitHub/2022-thermal-zoop-experiments/figures/birth_20_fluct.png")

#death rate
lt.summary_factors %>% filter(species=="daphnia"&resource=="1"&mean_temp=="20") %>% 
  ggplot(aes(x=magnitude,y=S.d,color=inf_status,shape=inf_status)) + 
  geom_point(position=position_dodge(width=0.1),size=4) + 
  geom_errorbar(aes(ymax=S.d.975,ymin=S.d.025),width=0,position=position_dodge(width=0.1),linewidth=1.5) +
  scale_color_manual("Amplitude of fluctuation (°C)", values = c("red3", "black")) + 
  theme_classic(base_size = 20) +
  ylab("Death rate") + 
  xlab("Amplitude of temperature fluctuation (°C)") + theme(legend.position = "none")
ggsave("Documents/GitHub/2022-thermal-zoop-experiments/figures/death_20_fluct.png")

#little r
lt.summary_factors %>% filter(species=="daphnia"&resource=="1"&mean_temp=="20") %>% 
  ggplot(aes(x=magnitude,y=S.r,color=inf_status,shape=inf_status)) + 
  geom_point(position=position_dodge(width=0.1),size=4) + 
  geom_errorbar(aes(ymax=S.r.975,ymin=S.r.025),width=0,position=position_dodge(width=0.1),linewidth=1.5) +
  scale_color_manual("Amplitude of fluctuation (°C)", values = c("red3", "black")) + 
  theme_classic(base_size = 20) +
  ylab("Intrinsic growth rate") + 
  xlab("Amplitude of temperature fluctuation (°C)") + theme(legend.position = "none")
ggsave("Documents/GitHub/2022-thermal-zoop-experiments/figures/little_r_20_fluct.png")
  
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

# save summary data frame - b,d, little r
saveRDS(lt.summary_factors, file ="processed-data/lifetable-expt/lifetable_growth_rates_w_CI_2022.rds")

# graphing the difference in r between uninfected and infected groups, just 20 and var
rates<-readRDS("processed-data/lifetable-expt/lifetable_growth_rates_w_CI_2022.rds")
rates<-lt.summary_factors
rates$temp_var<-as.factor(rates$temp_var)
rates %>% filter(species=="daphnia"&resource=="1"&mean_temp=="20") %>% 
  mutate(magnitude = case_when(
    temp_var=="0" ~ 0,
    temp_var=="2" ~ 1,
    temp_var=="6" ~ 3,
    temp_var=="14" ~ 7
  )) %>%
  ggplot(.,aes(x=magnitude,y=S.r,color=inf_status)) + 
  geom_point(position=position_dodge(width=0.1)) + 
  geom_errorbar(aes(ymax=S.r.975,ymin=S.r.025),width=0,position=position_dodge(width=0.1)) +
  scale_color_manual("Infection Status", values = c("red", "black")) + 
  theme_classic(base_size = 14) +
  scale_x_continuous(breaks=c(0,1,3,7))+
  ylab("little r") + xlab("Amplitude of temperature fluctuation (°C)") 

rates %<>% filter(resource =="1" & 
                    species == "daphnia" & 
                    mean_temp=="20")
rates %>% pivot_longer(cols = ,names_to = ,values_to=)

rates_uninf <- rates %>% 
  filter(inf_status=="U") %>% 
  select(-c("S.d.975","S.d.025","S.b.975","S.b.025","S.r.975","S.r.025","resource","species","mean_temp","temp_var","temp_id")) %>% 
  rename(ID_u = ID, S.r_u = S.r, S.d_u=S.d,S.b_u=S.b,inf = inf_status)
rates_inf<- rates %>% 
  filter(inf_status=="I") %>% 
  select(-c("S.d.975","S.d.025","S.b.975","S.b.025","S.r.975","S.r.025"))

rates_wide <- bind_cols(rates_inf,rates_uninf) 
rates_wide %<>% mutate(diff_d = S.d - S.d_u,
                       diff_b = S.b - S.b_u,
                       diff_r = S.r_u - S.r) %>%
  select("ID","diff_b","diff_r","diff_d") %>%
  pivot_longer(cols = starts_with("diff_"),names_to = "rate",names_prefix = "diff_",values_to = "diff")

rates_wide %<>% 
  separate(ID,c('species','temp_ID','resource','idk'),sep="_") %>%
  mutate(temp_var = case_when(
  temp_ID == "2V" ~ "1",
  temp_ID == "6V" ~ "3",
  temp_ID == "14V" ~ "7",
  TRUE ~ "0"
))

rates_wide %>% filter(resource =="1" & species == "daphnia" & (temp_ID != "15"&temp_ID !="25")) %>%
  ggplot(.,aes(x=factor(temp_var,levels=c('0','1','3','7')),y=diff,color=rate,group=rate)) + 
  geom_point(size=4)+
  theme_classic(base_size = 14)+
  ylab("difference in rate (inf-uninf)") + xlab("Fluctuation amplitude (°C)")+facet_wrap(.~rate)


