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
two_obs_redd_data = read_csv('analysis/data/raw_data/Master_Observer Efficiency_2012-2013_1-3-14.csv') %>%
  mutate(Date = mdy(Date)) %>%
  select(Year:Surveyor,
         ReachLength_km,
         Date,
         VisibleRedds,
         TotalFeatures,
         CorrectRedds,
         OmittedRedds = OmittedRedds_Visible,
         ExpSpTotal = EXP_Total,
         MeanDischarge,
         MeanThalwegCV,
         NaiveDensity_km = NaiveDensity) %>%
  mutate(ExpSpTotal_log = log(ExpSpTotal + 1),
         NetError = TotalFeatures / VisibleRedds)

# get mean and standard deviation of covariates
two_obs_covar_center = two_obs_redd_data %>%
  pivot_longer(cols = ExpSpTotal:ExpSpTotal_log,
               names_to = "metric",
               values_to = "value") %>%
  group_by(metric) %>%
  summarise_at(vars(value),
               list(mu = mean,
                    stddev = sd),
               na.rm = T)

# normalize covariates
two_obs_mod_data = two_obs_redd_data %>%
  pivot_longer(cols = one_of(two_obs_covar_center$metric),
               names_to = "metric",
               values_to = "value") %>%
  left_join(two_obs_covar_center) %>%
  # mutate(value = value - mu) %>%
  mutate(value = (value - mu) / stddev ) %>%
  select(-mu, -stddev) %>%
  pivot_wider(names_from = "metric",
              values_from = "value")

# fit model
two_obs_net_mod = glm(NetError ~ ExpSpTotal_log + MeanDischarge + MeanThalwegCV + NaiveDensity_km,
                          family = gaussian,
                          data = two_obs_mod_data)


#-----------------------------------------------------------------
# save some things
write_rds(two_obs_redd_data,
          file = 'analysis/data/derived_data/two_obs_original_data.rds')
write_rds(two_obs_mod_data,
          file = 'analysis/data/derived_data/two_obs_model_data.rds')
write_rds(two_obs_net_mod,
          file = 'analysis/data/derived_data/two_obs_net_error_model.rds')
write_rds(two_obs_covar_center,
          file = 'analysis/data/derived_data/two_obs_covar_center.rds')


#-----------------------------------------------------------------
# save for use in package functions
one_obs_covar_center = read_rds('analysis/data/derived_data/one_obs_covar_center.rds')
one_obs_net_mod = read_rds('analysis/data/derived_data/one_obs_net_error_model.rds')

usethis::use_data(two_obs_covar_center,
                  two_obs_net_mod,
                  one_obs_covar_center,
                  one_obs_net_mod,
                  internal = T,
                  overwrite = T,
                  version = 2)
