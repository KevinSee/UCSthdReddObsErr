# Author: Kevin See
# Purpose: save all redd survey data to Teams site
# Created: 1/27/23
# Last Modified: 1/27/23
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

list("Reach Length" = rch_lngth %>%
       filter(river %in% c("Chiwawa",
                           "Icicle",
                           "Nason",
                           "Peshastin",
                           "Wenatchee")),
     "Thalweg CV" = thlwg_summ %>%
       filter(River == "Wenatchee"),
     "Redd Surveys" = wen_redds_df,
     "Discharge" = wen_discharge) %>%
  map(clean_names,
      case = "title") |>
  write_xlsx(path = paste("T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd",
                          "inputs",
                          "Redd Data",
                          "Wenatchee_Redd_Surveys.xlsx",
                          sep = "/"))



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




list("Reach Length" = rch_lngth %>%
       filter(river %in% c("Methow",
                           "Methow Fish Hatchery",
                           "Spring Creek",
                           "Twisp")),
     "Thalweg CV" = thlwg_summ %>%
       filter(River == "Methow"),
     "Redd Surveys" = met_redds_df,
     "Discharge" = met_discharge) %>%
  map(clean_names,
      case = "title") |>
  write_xlsx(path = paste("T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd",
                          "inputs",
                          "Redd Data",
                          "Methow_Redd_Surveys.xlsx",
                          sep = "/"))



#----------------------------------------
# test reading data back in
library(readxl)

data_file <- paste("T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd",
                   "inputs",
                   "Redd Data",
                   "Wenatchee_Redd_Surveys.xlsx",
                   sep = "/")

data_list <- excel_sheets(data_file) |>
  as.list() |>
  set_names() |>
  map(.f = function(x) {
    read_excel(data_file,
               sheet = x) |>
      clean_names()
  })

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
  left_join(data_list$Discharge,
            by = c("spawn_year", "river", "reach", "index", "survey_type", "survey_date")) |>
  clean_names("big_camel") |>
  rename(MeanThalwegCV = MeanThalwegCv) |>
  mutate(ExpSpTotal_log = log(ExpSpTotal),
         NaiveDensity_km = VisibleRedds / LengthKm) |>
  predict_neterr()

