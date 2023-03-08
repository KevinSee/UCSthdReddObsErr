# Author: Kevin See
# Purpose: Prep redd data from 2014-2015
# Created: 3/7/23
# Last Modified: 3/7/2023
# Notes: this data is from the Wenatchee

#-----------------------------------------------------------------
# install old version of PITcleanr
# remotes::install_github("KevinSee/PITcleanr",
#                         ref = "v1.2.0")

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(magrittr)
library(msm)
library(UCSthdReddObsErr)
library(PITcleanr)
library(here)

#-----------------------------------------------------------------
data("thlwg_summ")

#-----------------------------------------------------------------
# 2014
#-----------------------------------------------------------------
# read in data
file_nm = "2014 Wenatchee Modeling Data - Final.xlsx"

redd_org = read_excel(here('analysis/data/raw_data/', file_nm),
                      sheet = 1) %>%
  clean_names(case = "upper_camel") %>%
  filter(River == "Wenatchee") %>%
  rename(ExpSpTotal = MeanTotalExperience,
         MeanThalwegCV = MeanThalwegCv,
         Index = IndexYOrN,
         SurveyType = SurveyTypeWeeklyOrPeak,
         MeanEffortHrs = MeanEffort,
         ReachArea = ReachAreaM2,
         MeanDischarge = DischargeCfs) %>%
  left_join(read_excel(here('analysis/data/raw_data/', file_nm),
                       sheet = 'Reach area') %>%
              mutate(Index = if_else(Type == "Index",
                                     "Y",
                                     "N")) %>%
              select(River, Reach, Index,
                     Length, Width, Area)) %>%
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
  mutate(Reach = fct_relevel(Reach, 'W10', after = Inf))

# replace MeanThalwegCV with values calculated from ALL measurements (across years)
redd_org %<>%
  rename(thlwg_org = MeanThalwegCV) %>%
  left_join(thlwg_summ %>%
              select(Reach, MeanThalwegCV)) %>%
  mutate(MeanThalwegCV = if_else(is.na(MeanThalwegCV),
                                 thlwg_org,
                                 MeanThalwegCV)) %>%
  select(-thlwg_org) %>%
  mutate(Reach = factor(Reach,
                        levels = levels(redd_org$Reach)))

# determine if reaches are in upper or lower mainstem
redd_org %<>%
  mutate(Location = if_else(!grepl('W[[:digit:]]', Reach),
                            'Tribs_above_TUM',
                            if_else(Reach %in% paste0('W', 1:7),
                                    'Below_TUM',
                                    'TUM_bb')))

# predict net error
redd_df = predict_neterr(redd_org,
                         num_obs = "two") |>
  add_column(spawn_year = 2014,
             .before = 1)

redd_df_all <- redd_df

rm(redd_org,
   redd_df,
   file_nm)

#-----------------------------------------------------------------
# 2015
#-----------------------------------------------------------------
file_nm = "2015 Wenatchee Steelhead Redd Modeling Data - Final.xlsx"

redd_org = read_excel(here('analysis/data/raw_data/', file_nm),
                      sheet = 1) %>%
  clean_names(case = "upper_camel") %>%
  rename(ExpSpTotal = MeanTotalExperience,
         MeanThalwegCV = MeanThalwegCv,
         Index = IndexYOrN,
         SurveyType = SurveyTypeWeeklyOrPeak,
         MeanEffortHrs = MeanEffort,
         MeanDischarge = Discharge) %>%
  left_join(read_excel(here('analysis/data/raw_data/', file_nm),
                       sheet = 'Reach Area') %>%
              mutate(Index = if_else(Type == "Index",
                                     "Y",
                                     "N")) %>%
              select(River, Reach, Index,
                     Length, Width, Area)) %>%
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
  mutate(Reach = fct_relevel(Reach, 'W10', after = Inf))

# replace MeanThalwegCV with values calculated from ALL measurements (across years)
redd_org %<>%
  rename(thlwg_org = MeanThalwegCV) %>%
  left_join(thlwg_summ %>%
              select(Reach, MeanThalwegCV)) %>%
  mutate(MeanThalwegCV = if_else(is.na(MeanThalwegCV),
                                 thlwg_org,
                                 MeanThalwegCV)) %>%
  select(-thlwg_org) %>%
  mutate(Reach = factor(Reach,
                        levels = levels(redd_org$Reach)))

# predict net error
redd_df = predict_neterr(redd_org,
                         num_obs = "two") |>
  add_column(spawn_year = 2015,
             .before = 1)

redd_df_all <- redd_df_all |>
  bind_rows(redd_df)

rm(redd_org,
   redd_df,
   file_nm)

#-----------------------------------------------------------------
# 2016
#-----------------------------------------------------------------
file_nm = "2016_Final_ModelingData_Steelhead.xlsx"

redd_org = read_excel(here('analysis/data/raw_data/', file_nm),
                      sheet = 1) %>%
  clean_names(case = "upper_camel") %>%
  rename(ExpSpTotal = MeanTotalExperience,
         MeanThalwegCV = MeanThalwegCv,
         Index = IndexYOrN,
         SurveyType = SurveyTypeWeeklyOrPeak,
         MeanEffortHrs = MeanEffort,
         MeanDischarge = Discharge) %>%
  left_join(read_excel(here('analysis/data/raw_data/', file_nm),
                       sheet = 'Reach Area')) %>%
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

# replace MeanThalwegCV with values calculated from ALL measurements (across years)
data("thlwg_summ")
redd_org %<>%
  rename(thlwg_org = MeanThalwegCV) %>%
  left_join(thlwg_summ %>%
              select(Reach, MeanThalwegCV)) %>%
  mutate(MeanThalwegCV = if_else(is.na(MeanThalwegCV),
                                 thlwg_org,
                                 MeanThalwegCV)) %>%
  select(-thlwg_org) %>%
  mutate(Reach = factor(Reach,
                        levels = levels(redd_org$Reach)))

# predict net error
redd_df = predict_neterr(redd_org,
                         num_obs = "two") |>
  add_column(spawn_year = 2016,
             .before = 1)

redd_df_all <- redd_df_all |>
  bind_rows(redd_df)

rm(redd_org,
   redd_df,
   file_nm)

#-----------------------------------------------------------------
# 2017
#-----------------------------------------------------------------
file_nm = "2017_CensusData_Final.xlsx"
redd_org = read_excel(here('analysis/data/raw_data/', file_nm),
                      sheet = 1) %>%
  clean_names(case = "upper_camel") %>%
  rename(ExpSpTotal = ExpTotal,
         MeanThalwegCV = MeanThalwegCv) %>%
  left_join(read_excel(here('analysis/data/raw_data/', file_nm),
                       sheet = 'Reach Area')) %>%
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

# replace MeanThalwegCV with values calculated from ALL measurements (across years)
redd_org %<>%
  rename(thlwg_org = MeanThalwegCV) %>%
  left_join(thlwg_summ %>%
              select(Reach, MeanThalwegCV)) %>%
  mutate(MeanThalwegCV = if_else(is.na(MeanThalwegCV),
                                 thlwg_org,
                                 MeanThalwegCV)) %>%
  select(-thlwg_org) %>%
  mutate(Reach = factor(Reach,
                        levels = levels(redd_org$Reach)))

# predict net error
redd_df = predict_neterr(redd_org,
                         num_obs = "two") |>
  add_column(spawn_year = 2017,
             .before = 1)

redd_df_all <- redd_df_all |>
  bind_rows(redd_df)

rm(redd_org,
   redd_df,
   file_nm)

#-----------------------------------------------------------------
# 2018
#-----------------------------------------------------------------
file_nm = "2018_SteelheadData.xlsx"

redd_org = read_excel(here('analysis/data/raw_data/',
                           file_nm),
                      sheet = 1) %>%
  clean_names(case = "upper_camel") %>%
  rename(ExpSpTotal = ExpTotal,
         MeanThalwegCV = MeanThalwegCv) %>%
  left_join(read_excel(here('analysis/data/raw_data/',
                            file_nm),
                       sheet = 'ReachArea')) %>%
  mutate_at(vars(River, Reach, Index, SurveyType),
            list(as.factor)) %>%
  mutate_at(vars(NewRedds, MeanEffortHrs, ReachArea, MeanThalwegCV),
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

# replace MeanThalwegCV with values calculated from ALL measurements (across years)
redd_org %<>%
  rename(thlwg_org = MeanThalwegCV) %>%
  left_join(thlwg_summ %>%
              select(Reach, MeanThalwegCV)) %>%
  mutate(MeanThalwegCV = if_else(is.na(MeanThalwegCV),
                                 thlwg_org,
                                 MeanThalwegCV)) %>%
  select(-thlwg_org) %>%
  mutate(Reach = factor(Reach,
                        levels = levels(redd_org$Reach)))

# predict net error
redd_df = predict_neterr(redd_org,
                         num_obs = "two") |>
  add_column(spawn_year = 2018,
             .before = 1)

redd_df_all <- redd_df_all |>
  bind_rows(redd_df)

rm(redd_org,
   redd_df,
   file_nm)

#-----------------------------------------------------------------
# 2019
#-----------------------------------------------------------------
file_nm = "2019_SteelheadData.xlsx"

redd_org = read_excel(here('analysis/data/raw_data/',
                           file_nm),
                      sheet = 1) %>%
  clean_names(case = "upper_camel") %>%
  rename(ExpSpTotal = ExpTotal,
         MeanThalwegCV = MeanThalwegCv) %>%
  left_join(read_excel(here('analysis/data/raw_data/',
                            file_nm),
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

# replace MeanThalwegCV with values calculated from ALL measurements (across years)
redd_org %<>%
  rename(thlwg_org = MeanThalwegCV) %>%
  left_join(thlwg_summ %>%
              select(Reach, MeanThalwegCV)) %>%
  mutate(MeanThalwegCV = if_else(is.na(MeanThalwegCV),
                                 thlwg_org,
                                 MeanThalwegCV)) %>%
  select(-thlwg_org) %>%
  mutate(Reach = factor(Reach,
                        levels = levels(redd_org$Reach)))

# predict net error
redd_df = predict_neterr(redd_org,
                         num_obs = "two") |>
  add_column(spawn_year = 2019,
             .before = 1)

redd_df_all <- redd_df_all |>
  bind_rows(redd_df)

rm(redd_org,
   redd_df,
   file_nm)

#-----------------------------------------------------------------
# 2021
#-----------------------------------------------------------------
file_nm = "2021_Steelhead Data.xlsx"

redd_org = read_excel(here('analysis/data/raw_data',
                           file_nm),
                      sheet = 1) %>%
  clean_names(case = "upper_camel") %>%
  mutate(MeanEffortHrs = hms(paste(hour(MeanEffortHrs),
                                   minute(MeanEffortHrs),
                                   second(MeanEffortHrs))),
         MeanEffortHrs = as.duration(MeanEffortHrs) / dhours(1)) %>%
  rename(ExpSpTotal = ExpTotal,
         MeanThalwegCV = MeanThalwegCv,
         MeanDischarge = MeanDailyDiscahrge) %>%
  # get reach lengths from earlier dataset
  left_join(read_excel(here('analysis/data/raw_data/',
                            "2019_SteelheadData.xlsx"),
                       sheet = 'ReachArea') %>%
              select(-Area)) %>%
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

# replace MeanThalwegCV with values calculated from ALL measurements (across years)
redd_org %<>%
  rename(thlwg_org = MeanThalwegCV) %>%
  left_join(thlwg_summ %>%
              select(Reach, MeanThalwegCV)) %>%
  mutate(MeanThalwegCV = if_else(is.na(MeanThalwegCV),
                                 thlwg_org,
                                 MeanThalwegCV)) %>%
  select(-thlwg_org) %>%
  mutate(Reach = factor(Reach,
                        levels = levels(redd_org$Reach)))

# predict net error
redd_df = predict_neterr(redd_org,
                         num_obs = "two") |>
  add_column(spawn_year = 2021,
             .before = 1)

redd_df_all <- redd_df_all |>
  bind_rows(redd_df)

rm(redd_org,
   redd_df,
   file_nm)

#-----------------------------------------------------------------
# 2022
#-----------------------------------------------------------------
file_nm = "2022_Steelhead Data.xlsx"

redd_org = read_excel(here('analysis/data/raw_data',
                           file_nm),
                      sheet = 1) %>%
  clean_names(case = "upper_camel") %>%
  mutate(MeanEffortHrs = hms(paste(hour(MeanEffortHrs),
                                   minute(MeanEffortHrs),
                                   second(MeanEffortHrs))),
         MeanEffortHrs = as.duration(MeanEffortHrs) / dhours(1)) %>%
  rename(ExpSpTotal = ExpTotal,
         MeanThalwegCV = MeanThalwegCv,
         MeanDischarge = MeanDailyDiscahrge) %>%
  # get reach lengths from earlier dataset
  left_join(read_excel(here('analysis/data/raw_data/',
                            "2019_SteelheadData.xlsx"),
                       sheet = 'ReachArea') %>%
              select(-Area)) %>%
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

# replace MeanThalwegCV with values calculated from ALL measurements (across years)
redd_org %<>%
  rename(thlwg_org = MeanThalwegCV) %>%
  left_join(thlwg_summ %>%
              select(Reach, MeanThalwegCV)) %>%
  mutate(MeanThalwegCV = if_else(is.na(MeanThalwegCV),
                                 thlwg_org,
                                 MeanThalwegCV)) %>%
  select(-thlwg_org) %>%
  mutate(Reach = factor(Reach,
                        levels = levels(redd_org$Reach)))

# predict net error
redd_df = predict_neterr(redd_org,
                         num_obs = "two") |>
  add_column(spawn_year = 2022,
             .before = 1)

redd_df_all <- redd_df_all |>
  bind_rows(redd_df)

rm(redd_org,
   redd_df,
   file_nm)

#-----------------------------------------------------------------
# save for future comparison
write_rds(redd_df_all,
          file = here("analysis/data/derived_data",
                      "old_redd_data_2014-2022.rds"))

#-----------------------------------------------------------------
redd_df_all |>
  group_by(spawn_year, Reach, Index) |>
  summarize(across(NetError,
                   mean),
            .groups = "drop") |>
  filter(Reach %in% c("W6", "W10"),
         Index == "Y")
