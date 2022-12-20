# Author: Kevin See
# Purpose: Prep steelhead redd data from the Methow
# Created: 12/12/2022
# Last Modified: 12/12/2022
# Notes:

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

# what year are we prepping?
yr = 2022

# for(yr in c(2021:2022)) {

  if(yr == 2020) {
    next
  }

  cat(paste("\t Prepping year", yr, "\n\n"))

  #-----------------------------------------------------------------
  # read in data

  if(yr == 2021) {
    file_nm = "2021 Methow Steelhead data for model.xlsx"

    redd_org = read_excel(here('analysis/data/raw_data',
                               file_nm),
                          sheet = 1) %>%
      clean_names(case = "upper_camel") %>%
      rename(ExpSpTotal = ExpTotal,
             MeanThalwegCV = MeanThalwegCv,
             MeanDischarge = MeanDailyDiscahrge) %>%
      mutate_at(vars(River, Reach, Index, SurveyType),
                list(as.factor)) %>%
      mutate_at(vars(NewRedds, MeanEffortHrs, MeanThalwegCV),
                list(as.numeric)) %>%
      mutate_at(vars(SurveyDate),
                list(ymd)) %>%
      mutate(Day = yday(SurveyDate),
             ExpSpTotal_log = log(ExpSpTotal + 1),
             NaiveDensity_km = VisibleRedds / (ReachLength / 1000))

  }

  if(yr == 2022) {
    file_nm = "2022 Methow Steelhead data for model.xlsx"

    redd_org = read_excel(here('analysis/data/raw_data',
                               file_nm),
                          sheet = "Census 2022") %>%
      clean_names(case = "upper_camel") %>%
      filter(!is.na(SurveyDate)) %>%
      select(-any_of("Sequence")) %>%
      rename(ExpSpTotal = ExpTotal,
             MeanThalwegCV = MeanThalwegCv,
             MeanDischarge = MeanDailyDiscahrge) %>%
      mutate_at(vars(River, Reach, Index, SurveyType),
                list(as.factor)) %>%
      mutate_at(vars(NewRedds, MeanEffortHrs, MeanThalwegCV),
                list(as.numeric)) %>%
      mutate_at(vars(SurveyDate),
                list(ymd)) %>%
      mutate(Day = yday(SurveyDate),
             ExpSpTotal_log = log(ExpSpTotal + 1),
             NaiveDensity_km = VisibleRedds / (ReachLength / 1000))

  }

  #-----------------------------------------------------------------
  # manipulate some of the data

  # # replace MeanThalwegCV with values calculated from ALL measurements (across years)
  # data("thlwg_summ")
  # redd_org %<>%
  #   rename(thlwg_org = MeanThalwegCV) %>%
  #   left_join(thlwg_summ %>%
  #               select(Reach, MeanThalwegCV)) %>%
  #   mutate(MeanThalwegCV = if_else(is.na(MeanThalwegCV),
  #                                  thlwg_org,
  #                                  MeanThalwegCV)) %>%
  #   select(-thlwg_org) %>%
  #   mutate(Reach = factor(Reach,
  #                         levels = levels(redd_org$Reach)))

  # add a location
  redd_org %<>%
    mutate(Location = "Lower Methow")

  #-----------------------------------------------------------------
  # predict net error
  redd_df = predict_neterr(redd_org,
                           num_obs = "two")

  #-----------------------------------------------------------------
  # any redds below PIT tag arrays?
  redds_below_arrays = NULL

  #-----------------------------------------------------------------
  # pull in PIT tag escapement results for use in converting redds to spawners

  # load DABOM results, including prepped data
  # load(paste0('../DabomPriestRapidsSthd/analysis/data/derived_data/PITcleanr/UC_Steelhead_', yr, '.rda'))
  # load('O:/Documents/Git/MyProjects/DabomPriestRapidsSthd/analysis/data/derived_data/site_config.rda')

  # what dam counts are the abundance estimates derived from?
  if(yr %in% c(2011:2015, 2018)) {
    dam_cnt_name = "PriestRapids"
  } else {
    dam_cnt_name = "RockIsland"
  }

  load(here("O:/Documents/Git/MyProjects/DabomPriestRapidsSthd/analysis/data/derived_data/estimates",
            dam_cnt_name,
            paste0("UC_Sthd_DABOM_", yr, ".rda")))

  # pull out Wenatchee tags
  met_tags = tag_summ %>%
    filter(str_detect(path, "LMR")) %>%
    mutate(Area = if_else(str_detect(spawn_node, "^MRC") |
                            str_detect(spawn_node, "^LMR"),
                          'Lower Methow',
                          if_else(str_detect(path, " LBC"),
                                  "Libby",
                                  if_else(str_detect(path, " GLC"),
                                          "Gold",
                                          if_else(str_detect(path, " BVC"),
                                                  "Beaver",
                                                  if_else(str_detect(path, " TWR"),
                                                          "Twisp",
                                                          if_else(str_detect(path, " MSH"),
                                                                  "Methow Fish Hatchery",
                                                                  if_else(str_detect(path, " SCP"),
                                                                          "Spring Creek",
                                                                          if_else(str_detect(path, " CRW"),
                                                                                  "Chewuch",
                                                                                  if_else(str_detect(path, " MRW"),
                                                                                          "Upper Methow",
                                                                                          NA_character_)))))))))) %>%
    mutate(Area = factor(Area,
                         levels = c("Lower Methow",
                                    "Upper Methow",
                                    "Chewuch",
                                    "Twisp",
                                    "Methow Fish Hatchery",
                                    "Spring Creek",
                                    "Beaver",
                                    "Gold",
                                    "Libby"))) %>%
    select(TagID = tag_code,
           Location = Area,
           Origin = origin,
           Sex = sex,
           AD = ad_clip,
           CWT = cwt) %>%
    # differentiate different tags in hatchery fish
    mutate(Group = if_else(Origin == "W",
                           "W",
                           if_else(!is.na(AD) & is.na(CWT),
                                   "HOR-SN",
                                   if_else(!is.na(CWT),
                                           "HOR-C",
                                           "HOR-C")))) %>%
    mutate(Group = factor(Group,
                          levels = c("W",
                                     "HOR-SN",
                                     "HOR-C")))

  #-------------------------------------------------------
  # generate fish / redd and pHOS for different areas
  fpr_df = met_tags %>%
    group_by(Location) %>%
    summarize(n_male = n_distinct(TagID[Sex == "M"]),
              n_female = n_distinct(TagID[Sex == "F"]),
              n_sexed = n_male + n_female,
              n_wild = n_distinct(TagID[Origin == "W"]),
              n_hatch = n_distinct(TagID[Origin == "H"]),
              n_wild = n_distinct(TagID[Group == "W"]),
              n_origin = n_wild + n_hatch,
              n_hor_sn = n_distinct(TagID[Group == "HOR-SN"]),
              n_hor_c = n_distinct(TagID[Group == "HOR-C"]),
              # n_origin2 = n_wild + n_hor_sn + n_hor_c,
              .groups = "drop") %>%
    mutate(prop_m = n_male / n_sexed,
           prop_se = sqrt((prop_m * (1 - prop_m)) / (n_sexed)),
           fpr = (prop_m) / (1 - prop_m) + 1) %>%
    rowwise() %>%
    mutate(fpr_se = deltamethod(~ x1 / (1 - x1) + 1,
                                mean = prop_m,
                                cov = prop_se^2)) %>%
    ungroup() %>%
    mutate(phos = n_hatch / n_origin,
           phos_se = sqrt((phos * (1 - phos)) / (n_origin)))


  #-----------------------------------------------------------------
  # pull in some estimates from DABOM
  all_escp = escape_summ %>%
    filter(location %in% c('LMR',
                           'LMR_bb',
                           'MRC_bb',
                           "GLC",
                           "LBC",
                           "MSH",
                           "MRW",
                           "TWR",
                           "CRW",
                           "SCP",
                           "BVC"))

  # pull out estimates of tributary spawners from DABOM
  trib_spawners = all_escp %>%
    filter(location %in% c("GLC",
                           "LBC",
                           "MSH",
                           "MRW",
                           "TWR",
                           "CRW",
                           "SCP",
                           "BVC")) %>%
    mutate(River = recode(location,
                          "GLC" = "Gold",
                          "LBC" = "Libby",
                          "MSH" = "Methow Fish Hatchery",
                          "MRW" = "Upper Methow",
                          "TWR" = "Twisp",
                          "CRW" = "Chewuch",
                          "SCP" = "Spring Creek",
                          "BVC" = "Beaver")) %>%
    select(Origin = origin,
           Location = location,
           River,
           Spawners = median,
           Spawners_SE = sd) %>%
    mutate(Origin = recode(Origin,
                           "W" = "Natural",
                           "H" = "Hatchery")) %>%
    arrange(Location, Origin)

  # pull out mainstem escapement estimates
  escp_met = all_escp %>%
    filter(location %in% c('LMR',
                           'LMR_bb',
                           'MRC_bb')) %>%
    mutate(Area = recode(location,
                         'LMR' = 'Methow_all',
                         'LMR_bb' = 'Lower Methow',
                         'MRC_bb' = 'Lower Methow')) %>%
    mutate(Origin = recode(origin,
                           "W" = "Natural",
                           "H" = "Hatchery")) %>%
    group_by(Area,
             Origin) %>%
    summarise(estimate = sum(median),
              se = sqrt(sum(sd^2)),
              .groups = "drop")

  #-----------------------------------------------------------------
  # save
  save(redd_df,
       redds_below_arrays,
       met_tags,
       fpr_df,
       trib_spawners,
       escp_met,
       file = here('analysis/data/derived_data',
                   paste0('met_', yr, '.rda')))

# }