# Author: Kevin See
# Purpose: Prep redd data from 2018
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


#-----------------------------------------------------------------
# read in data
redd_org = read_excel('analysis/data/raw_data/2018_SteelheadData.xlsx',
                      sheet = 1) %>%
  clean_names(case = "upper_camel") %>%
  rename(ExpSpTotal = ExpTotal,
         MeanThalwegCV = MeanThalwegCv) %>%
  left_join(read_excel('analysis/data/raw_data/2018_SteelheadData.xlsx',
                       sheet = 'ReachArea')) %>%
  mutate_at(vars(River, Reach, Index, SurveyType),
            list(as.factor)) %>%
  mutate_at(vars(MeanEffortHrs, ReachArea, MeanThalwegCV),
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

# fix one thalweg data point
redd_org %<>%
  mutate_at(vars(MeanThalwegCV),
            list(~ if_else(Reach == 'W10' & Index == 'Y',
                           58.0,
                           .)))

#-----------------------------------------------------------------
# predict net error
redd_df = predict_neterr(redd_org)

#-----------------------------------------------------------------
# save
write_rds(redd_df,
          path = 'analysis/data/derived_data/wen_2018.rds')
