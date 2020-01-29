# Author: Kevin See
# Purpose: Fit initial observer net error model
# Created: 1/29/2020
# Last Modified: 1/29/2020
# Notes: data comes from the Methow, with 2 observers

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)


#-----------------------------------------------------------------
# read in data
redd_data = read_csv('analysis/data/raw_data/Master_Observer Efficiency_2012-2013_1-3-14.csv') %>%
  mutate(Date = mdy(Date)) %>%
  select(Year:Surveyor,
         Date,
         VisibleRedds,
         TotalFeatures,
         CorrectRedds,
         ExpSpTotal = EXP_Total,
         MeanDischarge,
         MeanThalwegCV,
         NaiveDensity_km = NaiveDensity) %>%
  mutate(ExpSpTotal_log = log(ExpSpTotal + 1),
         NetError_rate = TotalFeatures / VisibleRedds)

# get mean and standard deviation of covariates
covar_center = redd_data %>%
  select(ExpSpTotal:ExpSpTotal_log) %>%
  gather(metric, value) %>%
  group_by(metric) %>%
  summarise_at(vars(value),
               list(mu = mean,
                    stddev = sd),
               na.rm = T)

# normalize covariates
mod_data = redd_data %>%
  gather(metric, value, one_of(covar_center$metric)) %>%
  left_join(covar_center) %>%
  # mutate(value = value - mu) %>%
  mutate(value = (value - mu) / stddev ) %>%
  select(-mu, -stddev) %>%
  spread(metric, value)

# fit model
net_err_mod = glm(NetError_rate ~ ExpSpTotal_log + MeanDischarge + MeanThalwegCV + NaiveDensity_km,
                  family = gaussian,
                  data = mod_data)


#-----------------------------------------------------------------
# save some things
write_rds(redd_data,
          path = 'analysis/data/derived_data/orginal_data.rds')
write_rds(mod_data,
          path = 'analysis/data/derived_data/model_data.rds')

usethis::use_data(covar_center,
                  net_err_mod,
                  internal = T,
                  overwrite = T,
                  version = 2)
