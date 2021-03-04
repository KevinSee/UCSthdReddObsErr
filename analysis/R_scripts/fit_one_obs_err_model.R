# Author: Kevin See
# Purpose: Fit single observer net error model
# Created: 5/15/2020
# Last Modified: 5/15/2020
# Notes: data comes from the Wenatchee, with a single observer

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)

#-----------------------------------------------------------------
# read in data
one_obs_redd_data = read_excel('analysis/data/raw_data/Observer Efficiency all years_20120803.xlsx',
                               2) %>%
  mutate(Date = ymd(Date)) %>%
  clean_names(case = 'big_camel') %>%
  mutate(NaiveDensity_km = TotalFeatures / (ReachLengthM / 1000)) %>%
  select(Year:Reach,
         Surveyor,
         Date,
         VisibleRedds,
         TotalFeatures,
         CorrectRedds,
         OmittedRedds,
         ExpSpTotal,
         MeanWidth,
         MeanDepth,
         MeanDischarge = MeanDis,
         MeanThalwegCV = MeanThalwegCv,
         ReachLengthM,
         NaiveDensity_km) %>%
  mutate(ExpSpTotal_log = log(ExpSpTotal + 1),
         NetError = TotalFeatures / VisibleRedds)

# get mean and standard deviation of covariates
one_obs_covar_center = one_obs_redd_data %>%
  pivot_longer(cols = MeanWidth:ExpSpTotal_log,
               names_to = "metric",
               values_to = "value") %>%
  group_by(metric) %>%
  summarise_at(vars(value),
               list(mu = mean,
                    stddev = sd),
               na.rm = T)

# normalize covariates
one_obs_mod_data = one_obs_redd_data %>%
  pivot_longer(one_of(one_obs_covar_center$metric),
               names_to = "metric",
               values_to = "value") %>%
  left_join(one_obs_covar_center) %>%
  # mutate(value = value - mu) %>%
  mutate(value = (value - mu) / stddev ) %>%
  select(-mu, -stddev) %>%
  pivot_wider(names_from = "metric",
              values_from = "value")

# fit model
one_obs_net_mod = glm(NetError ~ MeanDepth + MeanThalwegCV + NaiveDensity_km,
                      family = gaussian,
                      data = one_obs_mod_data)


#-----------------------------------------------------------------
# save some things
write_rds(one_obs_redd_data,
          path = 'analysis/data/derived_data/one_obs_original_data.rds')
write_rds(one_obs_mod_data,
          path = 'analysis/data/derived_data/one_obs_model_data.rds')
write_rds(one_obs_net_mod,
          path = 'analysis/data/derived_data/one_obs_net_error_model.rds')
write_rds(one_obs_covar_center,
          path = 'analysis/data/derived_data/one_obs_covar_center.rds')

#-----------------------------------------------------------------
# save for use in package functions
two_obs_covar_center = read_rds('analysis/data/derived_data/two_obs_covar_center.rds')
two_obs_net_mod = read_rds('analysis/data/derived_data/two_obs_net_error_model.rds')

usethis::use_data(one_obs_covar_center,
                  one_obs_net_mod,
                  two_obs_covar_center,
                  two_obs_net_mod,
                  internal = T,
                  overwrite = T,
                  version = 2)
