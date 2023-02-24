#looking at algae edibility data

setwd("~/Documents/GitHub/2022-thermal-zoop-experiments/raw-tank-data")
edible<-read.csv("YSI_algae_edibility.csv")
meta <- read.csv("tank_metadata.csv")

library("tidyverse")
library("magrittr")
edible %<>% mutate(bucket = as.factor(as.numeric(gsub("Tank ", "", Site))))

library("lubridate")
edible$Date <- mdy(edible$Date)

meta$bucket <- as.factor(meta$bucket)
edible <- left_join(edible, meta, by = "bucket")

edible %<>% rename('temp_f' = 'F.21A104910', "do_per" = "DO...21A106679", "do_conc" = "DO.mg.L.21A106679",
                "ph" = "pH.21B101988", "ph_mv" = "pH.mV.21B101988", "chlor_rfu" = "Chlorophyll.RFU.21B101736",
                "chlor_conc" = "Chlorophyll.ug.L.21B101736")
edible %<>% mutate(temp_c = (temp_f-32)*(5/9))

library("ggplot2")
ggplot(edible,aes(x=temp,y=chlor_conc,color=algale_edibility)) +
  geom_point() +
  theme_bw() +
  facet_wrap(.~community)

ggplot(filter(edible,algale_edibility=="edible"),aes(x=temp,y=chlor_conc,color=algale_edibility)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(.~community)

ggplot(filter(edible,algale_edibility=="inedible"),aes(x=temp,y=chlor_conc,color=algale_edibility)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(.~community)

#look at buckets that lost Daphnia
ggplot(filter(edible,algale_edibility=="edible"&(bucket!="22"|bucket!="23"|bucket!="26"|bucket!="30"|bucket!="43")),
       aes(x=temp,y=chlor_conc,color=algale_edibility)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(.~community)
