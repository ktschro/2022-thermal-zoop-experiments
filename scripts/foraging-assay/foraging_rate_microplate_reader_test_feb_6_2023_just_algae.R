plate<-read.csv("processed-data/foraging-assay/Schroeder_Strauss_2_3_2023_microplate reader_trial_2_all_data.csv")
plate$plate<-as.factor(plate$plate)

library(tidyverse)
library(ggpubr)

plate %>% ggerrorplot(x="conc_ank",y="reading_485_665",desc_stat="mean_se",facet.by = "plate")

plate %>% ggplot(aes(x=conc_ank,y=reading_485_665,group=conc_ank)) + theme_classic() + geom_boxplot() +facet_wrap(.~plate)

plate_2 <- plate %>% filter(plate=="2")

m1<-lm(reading_485_665~conc_ank,data=plate_2)

#check assumptions - some funneling, fine
plot(m1)
hist(m1$residuals)

summary(m1)

#add predicted values
plate_2$pred_fluor <- fitted(m1)

ggplot(data=plate_2,aes(x=conc_ank,y=reading_485_665)) +geom_point()+geom_line(aes(x=conc_ank,y=pred_fluor))+theme_classic()

summary(m1)
