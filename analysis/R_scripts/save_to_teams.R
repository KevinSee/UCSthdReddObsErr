# Author: Kevin See
# Purpose: save all redd survey data to Teams site
# Created: 1/27/23
# Last Modified: 2/3/23
# Notes: save 2014 - 2022 data from Wenatchee and Methow to Teams site

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(here)
library(janitor)
library(UCSthdReddObsErr)
library(writexl)

# use consistent thalweg CVs
data("thlwg_summ")
# use consistent definition of reach length
data("rch_lngth")

#----------------------------------------
# Wenatchee

# get all the redd count data
all_wen_data <- tibble(spawn_year = 2014:2022) %>%
  mutate(redd_data = map(spawn_year,
                         .f = possibly(function(yr) {
                           load(here('analysis/data/derived_data',
                                     paste0('wen_', yr, '.rda')))

                           res <- list("redd_df" = redd_df,
                                       "redds_below_arrays" = redds_below_arrays)

                           return(res)
                         },
                         otherwise = NULL)))

all_wen_df = all_wen_data %>%
  mutate(redd_df = map(redd_data,
                       "redd_df")) %>%
  select(-redd_data) %>%
  unnest(redd_df) %>%
  bind_rows(all_wen_data %>%
              mutate(redd_df = map(redd_data,
                                   "redds_below_arrays")) %>%
              select(-redd_data) %>%
              unnest(redd_df)) %>%
  mutate(across(c(Reach,
                  River),
                as.factor),
         across(Reach,
                fct_relevel,
                "W10",
                after = Inf),
         across(Reach,
                fct_relevel,
                "C1", "N1", "P1",
                after = Inf)) %>%
  arrange(spawn_year,
          Reach, Index,
          SurveyDate) %>%
  # select(spawn_year:ExpSpTotal) %>%
  clean_names()

wen_discharge <- all_wen_df %>%
  select(spawn_year:survey_date,
         mean_discharge)

# determine what USGS gage should be used for each reach
usgs_codes <- tibble(location = c("Chiwawa",
                                  "Plain",
                                  "Peshastin",
                                  "Monitor",
                                  "Pateros",
                                  "Methow - Twisp",
                                  "Twisp - Twisp",
                                  "Methow - Winthrop",
                                  "Chewuch - Winthrop"),
                     usgs_site_code = c("12456500",
                                        '12457000',
                                        "12459000",
                                        "12462500",
                                        "12449950",
                                        "12449500",
                                        "12448998",
                                        "12448500",
                                        "12448000"))

wen_discharge_sites <- all_wen_df |>
  # filter(index == "Y") |>
  filter(str_detect(reach, "^W")) |>
  select(reach) |>
  distinct() |>
  mutate(location = recode(reach,
                           "W1" = "Monitor",
                           "W2" = "Monitor",
                           "W3" = "Monitor",
                           "W4" = "Peshastin",
                           "W5" = "Peshastin",
                           "W6" = "Plain",
                           "W8" = "Plain",
                           "W9" = "Plain",
                           "W10" = "Chiwawa")) |>
  left_join(usgs_codes) |>
  mutate(agency = "USGS") |>
  select(reach,
         agency,
         location,
         site_code = usgs_site_code) |>
  mutate(notes = if_else(reach == "W10",
                         "Use discharge at Plain MINUS discharge at USGS 12456500 Chiwawa River near Plain, WA",
                         NA_character_))



wen_redds_df <- all_wen_df %>%
  mutate(across(surveyors,
                str_replace,
                ":",
                ","),
         across(surveyors,
                str_remove,
                "\\."),
         across(surveyors,
                ~ if_else(str_detect(., "\\(", negate = T),
                          if_else(str_detect(., ",[:alpha:]"),
                                  str_replace(., ",", ", "),
                                  if_else(str_detect(., ",", negate = T),
                                          str_replace(., " ", ", "),
                                          .)),
                          .))) %>%
  mutate(surveyor1 = str_split(surveyors, "\\, ", simplify = T)[,1],
         surveyor2 = str_split(surveyors, "\\, ", simplify = T)[,2],
         across(surveyor1,
                str_remove,
                ","),
         across(surveyor2,
                ~ if_else(. == "",
                          NA_character_,
                          .))) %>%
  select(spawn_year:surveyors,
         surveyor1, surveyor2,
         exp_sp_total)

save_list <- list("Reach Length" = rch_lngth %>%
                    filter(river %in% c("Chiwawa",
                                        "Icicle",
                                        "Nason",
                                        "Peshastin",
                                        "Wenatchee")) |>
                    select(-reach_descp) |>
                    rename(reach_description = type_descp),
                  "Thalweg CV" = thlwg_summ %>%
                    filter(River %in% c("Chiwawa",
                                        "Icicle",
                                        "Nason",
                                        "Peshastin",
                                        "Wenatchee")) %>%
                    arrange(River, Reach),
                  "Redd Surveys" = wen_redds_df,
                  "Discharge" = wen_discharge,
                  "Discharge Gages" = wen_discharge_sites) %>%
  map(clean_names,
      case = "title")

write_xlsx(save_list,
           path = paste("T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd",
                        "inputs",
                        "Redd Data",
                        "Wenatchee_Redd_Surveys.xlsx",
                        sep = "/"))


write_csv(save_list$`Thalweg CV`,
          "O:/Desktop/Wen_Thlwg_CV.csv")

#----------------------------------------
# Methow

all_met_data <- tibble(spawn_year = 2021:2022) %>%
  mutate(redd_data = map(spawn_year,
                         .f = possibly(function(yr) {
                           load(here('analysis/data/derived_data',
                                     paste0('met_', yr, '.rda')))

                           res <- list("redd_df" = redd_df,
                                       "redds_below_arrays" = redds_below_arrays)

                           return(res)
                         },
                         otherwise = NULL)))

all_met_df <- all_met_data %>%
  mutate(redd_df = map(redd_data,
                       "redd_df")) %>%
  select(-redd_data) %>%
  unnest(redd_df) %>%
  bind_rows(all_met_data %>%
              mutate(redd_df = map(redd_data,
                                   "redds_below_arrays")) %>%
              select(-redd_data) %>%
              unnest(redd_df)) %>%
  clean_names()

met_discharge <- all_met_df %>%
  select(spawn_year:survey_date,
         mean_discharge)

met_discharge_sites <- all_met_df |>
  filter(str_detect(reach, "^MRW")) |>
  select(reach) |>
  distinct() |>
  mutate(location = recode(reach,
                           "MRW1" = "Pateros",
                           "MRW2" = "Pateros",
                           "MRW3" = "Pateros",
                           "MRW4" = "Methow - Twisp",
                           "MRW5" = "Methow - Twisp",
                           "MRW6" = "Methow - Twisp",
                           "MRW7" = "Methow - Winthrop",
                           "MRW8" = "Methow - Winthrop")) |>
  left_join(usgs_codes) |>
  mutate(agency = "USGS") |>
  select(reach,
         agency,
         location,
         site_code = usgs_site_code) |>
  arrange(reach)



met_redds_df <- all_met_df %>%
  mutate(across(surveyors,
                str_replace,
                ":",
                ","),
         across(surveyors,
                str_remove,
                "\\."),
         across(surveyors,
                ~ if_else(str_detect(., "\\(", negate = T),
                          if_else(str_detect(., ",[:alpha:]"),
                                  str_replace(., ",", ", "),
                                  if_else(str_detect(., ",", negate = T),
                                          str_replace(., " ", ", "),
                                          .)),
                          .))) %>%
  mutate(surveyor1 = str_split(surveyors, "\\, ", simplify = T)[,1],
         surveyor2 = str_split(surveyors, "\\, ", simplify = T)[,2],
         surveyor3 = str_split(surveyors, "\\, ", simplify = T)[,3],
         across(surveyor1,
                str_remove,
                ","),
         across(c(surveyor2,
                  surveyor3),
                ~ if_else(. == "",
                          NA_character_,
                          .))) %>%
  select(spawn_year:surveyors,
         surveyor1, surveyor2,
         surveyor3,
         exp_sp_total)




save_list <- list("Reach Length" = rch_lngth %>%
                    filter(river %in% c("Methow",
                                        "Methow Fish Hatchery",
                                        "Spring Creek",
                                        "Twisp")) |>
                    select(-type_descp) |>
                    mutate(across(type,
                                  recode,
                                  "N" = "non_index",
                                  "Y" = "index")),
                  "Thalweg CV" = thlwg_summ %>%
                    filter(River == "Methow"),
                  "Redd Surveys" = met_redds_df,
                  "Discharge" = met_discharge,
                  "Discharge Gages" = met_discharge_sites) %>%
  map(clean_names,
      case = "title")

write_xlsx(save_list,
           path = paste("T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd",
                        "inputs",
                        "Redd Data",
                        "Methow_Redd_Surveys.xlsx",
                        sep = "/"))



#----------------------------------------
# test reading data back in
library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(dataRetrieval)

# determine what USGS gage should be used for each reach
usgs_codes <- tibble(location = c("Chiwawa",
                                  "Plain",
                                  "Peshastin",
                                  "Monitor",
                                  "Pateros",
                                  "Methow - Twisp",
                                  "Twisp - Twisp",
                                  "Methow - Winthrop",
                                  "Chewuch - Winthrop"),
                     usgs_site_code = c("12456500",
                                        '12457000',
                                        "12459000",
                                        "12462500",
                                        "12449950",
                                        "12449500",
                                        "12448998",
                                        "12448500",
                                        "12448000"))


data_file <- paste("T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd",
                   "inputs",
                   "Redd Data",
                   # "Methow_Redd_Surveys.xlsx",
                   "Wenatchee_Redd_Surveys.xlsx",
                   sep = "/")

data_list <- excel_sheets(data_file) |>
  as.list() |>
  rlang::set_names() |>
  map(.f = function(x) {
    read_excel(data_file,
               sheet = x) |>
      clean_names()
  })

# data_list = save_list |>
#   map(.f = clean_names)

redd_df <- data_list$`Redd Surveys` |>
  select(-surveyors) |>
  left_join(data_list$`Reach Length` |>
              select(river,
                     reach,
                     type, index,
                     length_km),
            by = c("river", "reach", "index")) |>
  left_join(data_list$`Thalweg CV` |>
              select(river,
                     reach,
                     mean_thalweg_cv),
            by = c("river", "reach")) |>
  left_join(data_list$`Discharge Gages` |>
              select(reach,
                     usgs_site_code = site_code)) |>
  left_join(data_list$Discharge,
            by = c("spawn_year", "river", "reach", "index", "survey_type", "survey_date")) |>
  mutate(across(c(reach,
                  river),
                as.factor),
         across(reach,
                fct_relevel,
                "W10",
                "C1", "N1", "P1",
                after = Inf),
         across(reach,
                fct_relevel,
                "MH1", "T1", "WN1",
                after = Inf))

# query USGS for discharge data
discharge_df <- redd_df |>
  filter(str_detect(reach, "^W")) |>
  rowwise() |>
  mutate(readNWISdv(usgs_site_code,
                    parameterCd = "00060", # discharge
                    startDate = as.character(ymd(survey_date)),
                    endDate = as.character(ymd(survey_date)),
                    statCd = "00003" # mean
                    )) |>
  rename(discharge = X_00060_00003) |>
  select(-c(agency_cd:Date, X_00060_00003_cd))

# adjust discharge for W10: Plain - Chiwawa discharge
plain_code <- redd_df |>
  filter(reach == "W9") |>
  pull(usgs_site_code) |>
  unique()

w10_discharge <- discharge_df |>
  filter(reach == "W10") |>
  mutate(readNWISdv(plain_code,
                    pcode,
                    startDate = as.character(ymd(survey_date)),
                    endDate = as.character(ymd(survey_date)),
                    statCd = stat_code)) |>
  rename(plain_discharge = X_00060_00003) |>
  select(-c(agency_cd:Date, X_00060_00003_cd)) |>
  mutate(discharge = plain_discharge - discharge) |>
  select(-plain_discharge)

# put it all back together
discharge_df |>
  filter(reach != "W10") |>
  bind_rows(w10_discharge) |>
  bind_rows(redd_df |>
              filter(str_detect(reach, "^W",
                                negate = T))) |>
  arrange(spawn_year,
          river,
          reach,
          index,
          survey_date) |>
  ungroup() -> redd_df


# query USGS for discharge data
discharge_df <- redd_df |>
  filter(str_detect(reach, "^MRW")) |>
  rowwise() |>
  mutate(readNWISdv(usgs_site_code,
                    pcode,
                    startDate = as.character(ymd(survey_date)),
                    endDate = as.character(ymd(survey_date)),
                    statCd = stat_code)) |>
  rename(discharge = X_00060_00003) |>
  select(-c(agency_cd:Date, X_00060_00003_cd))

discharge_df |>
  bind_rows(redd_df |>
              filter(str_detect(reach, "^MRW",
                                negate = T))) |>
  arrange(spawn_year,
          river,
          reach,
          index,
          survey_date) |>
  ungroup() -> redd_df


# quick comparison of newly queried discharge data and what was recorded previously
redd_df |>
  filter(!is.na(discharge)) |>
  mutate(across(c(river,
                  reach),
                fct_drop)) |>
  ggplot(aes(x = discharge,
             y = mean_discharge,
             color = reach)) +
  geom_abline(linetype = 2,
              color = "gray") +
  geom_point() +
  facet_wrap(~ spawn_year)


redd_summ <- redd_df |>
  clean_names("big_camel") |>
  select(-MeanDischarge) |>
  rename(MeanThalwegCV = MeanThalwegCv,
         MeanDischarge = Discharge) |>
  mutate(ExpSpTotal_log = log(ExpSpTotal),
         NaiveDensity_km = VisibleRedds / LengthKm) |>
  predict_neterr() |>
  # filter(NetError > 1)
  summarizeRedds(group_vars = c("SpawnYear",
                                "River",
                                "Reach",
                                "Index",
                                "SurveyType"),
                 summ_vars = c("SpawnYear",
                               "River"))

redd_summ$rch_est |>
  filter(err_est > 1)
