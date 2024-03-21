#fitting TPCs to life table data 
littler <- readRDS("lt_full_summary.rds")

library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)

#example chlorella tpc
data("chlorella_tpc")

# keep just a single curve
d <- filter(chlorella_tpc, curve_id == 1)

# show the data
ggplot(d, aes(temp, rate)) +
  geom_point() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Metabolic rate',
       title = 'Respiration across temperatures')

# choose model
mod = 'sharpschoolhigh_1981'

# get start vals
start_vals <- get_start_vals(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981')

# get limits
low_lims <- get_lower_lims(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981')
upper_lims <- get_upper_lims(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981')

fit <- nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 15),
                     data = d,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')
fit

new_data <- data.frame(temp = seq(min(d$temp), max(d$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit
ggplot(d, aes(temp, rate)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Metabolic rate',
       title = 'Respiration across temperatures')

#trying it with lifetable data ---- 
data1 <- littler %>% filter(species=="D"&resource==1&temp_var==0)
data2 <- littler %>% filter(species=="D"&resource==0.5&temp_var==0)
data3 <- littler %>% filter(species=="D"&resource==0.1&temp_var==0)

ggplot(data, aes(x=as.numeric(temp_id), y=S.r.U)) +
  geom_point() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Little r',
       title = '1.0')
ggplot(data2, aes(x=as.numeric(temp_id), y=S.r.U)) +
  geom_point() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Little r',
       title = "0.5")
ggplot(data3, aes(x=as.numeric(temp_id), y=S.r.U)) +
  geom_point() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       title = '0.1',
       y = 'Little r')

# 0.1 resource level ---- 

# get start vals
data3$temp_id <- as.numeric(data3$temp_id)
start_vals <- get_start_vals(data3$temp_id, data3$S.r.U, model_name = 'quadratic_2008')

# get limits
low_lims <- get_lower_lims(data3$temp_id, data3$S.r.U, model_name = 'quadratic_2008')
upper_lims <- get_upper_lims(data3$temp_id, data3$S.r.U, model_name = 'quadratic_2008')

fit3 <- nls_multstart(S.r.U~quadratic_2008(temp = temp_id, a,b,c),
                     data = data3,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims)
fit3

new_data3 <- data.frame(temp = seq(min(data3$temp_id), max(data3$temp_id), 0.5))
preds3 <- augment(fit3, newdata = new_data3)
preds3$calc <- (-0.654787 + 0.075822*preds3$temp - 0.001636 *(preds3$temp*preds3$temp))

# plot data3 and model fit
ggplot(data3, aes(temp_id, S.r.U)) +
  geom_point() +
  geom_line(aes(temp, calc), preds3, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'little r',
       title = '0.1')

#0.5 resource level ----

# get start vals
data2$temp_id <- as.numeric(data2$temp_id)
start_vals <- get_start_vals(data2$temp_id, data2$S.r.U, model_name = 'quadratic_2008')

# get limits
low_lims <- get_lower_lims(data2$temp_id, data2$S.r.U, model_name = 'quadratic_2008')
upper_lims <- get_upper_lims(data2$temp_id, data2$S.r.U, model_name = 'quadratic_2008')

fit2 <- nls_multstart(S.r.U~quadratic_2008(temp = temp_id, a,b,c),
                     data = data2,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims)
fit2

new_data2 <- data.frame(temp = seq(min(data2$temp_id), max(data2$temp_id), 0.5))
preds2 <- augment(fit2, newdata = new_data2)
preds2$calc <- (0.281750 - 0.029312*preds2$temp + 0.001265*(preds2$temp*preds2$temp))

# plot data2 and model fit
ggplot(data2, aes(temp_id, S.r.U)) +
  geom_point() +
  geom_line(aes(temp, calc), preds2, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'little r',
       title = '0.5')

# 1.0 resource level ----
# get start vals
data1$temp_id <- as.numeric(data1$temp_id)
start_vals <- get_start_vals(data1$temp_id, data1$S.r.U, model_name = 'quadratic_2008')

# get limits
low_lims <- get_lower_lims(data1$temp_id, data1$S.r.U, model_name = 'quadratic_2008')
upper_lims <- get_upper_lims(data1$temp_id, data1$S.r.U, model_name = 'quadratic_2008')

fit1 <- nls_multstart(S.r.U~quadratic_2008(temp = temp_id, a,b,c),
                     data = data1,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims)
fit1

new_data1 <- data.frame(temp = seq(min(data1$temp_id), max(data1$temp_id), 0.5))
preds1 <- augment(fit1, newdata = new_data1)
preds1$calc <- (-0.2746398 + 0.0330025*preds1$temp - 0.0003426*(preds1$temp*preds1$temp))

# plot data and model fit
ggplot(data1, aes(temp_id, S.r.U)) +
  geom_point() +
  geom_line(aes(temp, calc), preds1, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'little r',
       title = '1.0')

# now plot them all together
all_preds <- data.frame(id=1:21)
all_preds$temp <- preds1$temp
all_preds$fit1 <- preds1$calc
all_preds$fit0.5 <- preds2$calc
all_preds$fit0.1 <- preds3$calc

all_preds_long <- all_preds %>% pivot_longer(cols=starts_with("fit"),names_to = "model",values_to = "estimated_little_r")

ggplot(all_preds_long,aes(x=temp,y=estimated_little_r,group=model,color=model)) +
  geom_line() +
  geom_point(size=0.5) +
  theme_classic()

#find maximum for all resource levels
calc_params(fit1)
calc_params(fit2)
calc_params(fit3)

#now do it all again for infected ---- 
#trying it with lifetable data ---- 
