littler <- readRDS("lt_full_summary.rds")

library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)


data1 <- littler %>% filter(species=="D"&resource==1&temp_var==0)
data2 <- littler %>% filter(species=="D"&resource==0.5&temp_var==0)
data3 <- littler %>% filter(species=="D"&resource==0.1&temp_var==0)

ggplot(data1, aes(x=as.numeric(temp_id), y=S.r.I)) +
  geom_point() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Little r',
       title = '1.0')
ggplot(data2, aes(x=as.numeric(temp_id), y=S.r.I)) +
  geom_point() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Little r',
       title = "0.5")
ggplot(data3, aes(x=as.numeric(temp_id), y=S.r.I)) +
  geom_point() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       title = '0.1',
       y = 'Little r')

# 0.1 resource level ---- 

# get start vals
data3$temp_id <- as.numeric(data3$temp_id)
start_vals <- get_start_vals(data3$temp_id, data3$S.r.I, model_name = 'quadratic_2008')

# get limits
low_lims <- get_lower_lims(data3$temp_id, data3$S.r.I, model_name = 'quadratic_2008')
upper_lims <- get_upper_lims(data3$temp_id, data3$S.r.I, model_name = 'quadratic_2008')

fit3 <- nls_multstart(S.r.I~quadratic_2008(temp = temp_id, a,b,c),
                      data = data3,
                      iter = 500,
                      start_lower = start_vals - 10,
                      start_upper = start_vals + 10,
                      lower = low_lims,
                      upper = upper_lims)
fit3

new_data3 <- data.frame(temp = seq(min(data3$temp_id), max(data3$temp_id), 0.5))
preds3 <- augment(fit3, newdata = new_data3)
preds3$calc <- (-0.1187691 + 0.0221034*preds3$temp -0.0004079 *(preds3$temp*preds3$temp))

# plot data3 and model fit
ggplot(data3, aes(temp_id, S.r.I)) +
  geom_point() +
  geom_line(aes(temp, calc), preds3, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'little r',
       title = '0.1')

#0.5 resource level ----

# get start vals
data2$temp_id <- as.numeric(data2$temp_id)
start_vals <- get_start_vals(data2$temp_id, data2$S.r.I, model_name = 'quadratic_2008')

# get limits
low_lims <- get_lower_lims(data2$temp_id, data2$S.r.I, model_name = 'quadratic_2008')
upper_lims <- get_upper_lims(data2$temp_id, data2$S.r.I, model_name = 'quadratic_2008')

fit2 <- nls_multstart(S.r.I~quadratic_2008(temp = temp_id, a,b,c),
                      data = data2,
                      iter = 500,
                      start_lower = start_vals - 10,
                      start_upper = start_vals + 10,
                      lower = low_lims,
                      upper = upper_lims)
fit2

new_data2 <- data.frame(temp = seq(min(data2$temp_id), max(data2$temp_id), 0.5))
preds2 <- augment(fit2, newdata = new_data2)
preds2$calc <- (0.2280680 -0.0215886*preds2$temp + 0.0009451 *(preds2$temp*preds2$temp))

# plot data2 and model fit
ggplot(data2, aes(temp_id, S.r.I)) +
  geom_point() +
  geom_line(aes(temp, calc), preds2, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'little r',
       title = '0.5')

# 1.0 resource level ----
# get start vals
data1$temp_id <- as.numeric(data1$temp_id)
start_vals <- get_start_vals(data1$temp_id, data1$S.r.I, model_name = 'quadratic_2008')

# get limits
low_lims <- get_lower_lims(data1$temp_id, data1$S.r.I, model_name = 'quadratic_2008')
upper_lims <- get_upper_lims(data1$temp_id, data1$S.r.I, model_name = 'quadratic_2008')

fit1 <- nls_multstart(S.r.I~quadratic_2008(temp = temp_id, a,b,c),
                      data = data1,
                      iter = 500,
                      start_lower = start_vals - 10,
                      start_upper = start_vals + 10,
                      lower = low_lims,
                      upper = upper_lims)
fit1

new_data1 <- data.frame(temp = seq(min(data1$temp_id), max(data1$temp_id), 0.5))
preds1 <- augment(fit1, newdata = new_data1)
preds1$calc <- (0.0372319 + 0.0032733*preds1$temp + 0.0002772*(preds1$temp*preds1$temp))

# plot data and model fit
ggplot(data1, aes(temp_id, S.r.I)) +
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
