# Author: Kevin See
# Purpose: Prep redd data from 2019
# Created: 1/29/2020
# Last Modified: 1/29/2020
# Notes: this data is from the Wenatchee

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(magrittr)
library(SthdReddObsError)

# what year are we prepping?
yr = 2019

#-----------------------------------------------------------------
# read in data
redd_org = read_excel(paste0('analysis/data/raw_data/', yr, '_SteelheadData.xlsx'),
                      sheet = 1) %>%
  clean_names(case = "upper_camel") %>%
  rename(ExpSpTotal = ExpTotal,
         MeanThalwegCV = MeanThalwegCv) %>%
  left_join(read_excel(paste0('analysis/data/raw_data/', yr, '_SteelheadData.xlsx'),
                       sheet = 'ReachArea')) %>%
  mutate_at(vars(River, Reach, Index, SurveyType),
            list(as.factor)) %>%
  mutate_at(vars(NewRedds, MeanEffortHrs, ReachArea, MeanThalwegCV, Width),
            list(as.numeric)) %>%
  mutate_at(vars(SurveyDate),
            list(ymd)) %>%
  mutate(Day = yday(SurveyDate),
         ExpSpTotal_log = log(ExpSpTotal + 1),
         NaiveDensity_km = VisibleRedds / (Length / 1000),
         MeanWidth_m = ReachArea / Length) %>%
  mutate(Reach = fct_relevel(Reach, 'W10', after = Inf)) %>%
  mutate_at(vars(MeanThalwegCV),
            list(~ . * 100))

#-----------------------------------------------------------------
# predict net error
redd_df = predict_neterr(redd_org)

#-----------------------------------------------------------------
# save
write_rds(redd_df,
          path = paste0('analysis/data/derived_data/wen_', yr, '.rds'))
