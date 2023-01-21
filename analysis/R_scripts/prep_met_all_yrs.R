# Author: Kevin See
# Purpose: Prep steelhead redd data from the Methow
# Created: 12/12/2022
# Last Modified: 1/19/2023
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


#-----------------------------------------------------------------
# replace MeanThalwegCV with values calculated from ALL measurements (across years)
data("thlwg_summ")
# use consistent definition of reach length
data("rch_lngth")
# for adjusting fish/redd due to error rate in sex calls at Priest
data("sex_err_rate")


# what year are we prepping?
# yr = 2022

for(yr in c(2021:2022)) {

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
      select(River,
             Reach,
             Index,
             SurveyType,
             SurveyDate,
             NewRedds,
             VisibleRedds,
             Surveyors,
             ExpSpTotal = ExpTotal,
             MeanDischarge = MeanDailyDiscahrge) %>%
      mutate(across(Reach,
                    as.factor))

    # any redds below PIT tag arrays?
    redds_below_arrays = NULL

  }

  if(yr == 2022) {
    # file_nm = "2022 Methow Steelhead data for model.xlsx"
    file_nm = "2022 Methow Steelhead data for model-with tribs.xlsx"

    redd_org = read_excel(here('analysis/data/raw_data',
                               file_nm),
                          sheet = "Census 2022") %>%
      clean_names(case = "upper_camel") %>%
      select(River,
             Reach,
             Index,
             SurveyType,
             SurveyDate,
             NewRedds,
             VisibleRedds,
             Surveyors,
             ExpSpTotal = ExpTotal,
             MeanDischarge = MeanDailyDiscahrge) %>%
      mutate(across(Reach,
                    as.factor)) %>%
      filter(!is.na(SurveyDate))

    # any redds below PIT tag arrays?
    redds_below_arrays = read_excel(here('analysis/data/raw_data',
                                         file_nm),
                                    sheet = "Tribs") %>%
      clean_names(case = "upper_camel") %>%
      filter(!is.na(Reach)) %>%
      mutate(across(River,
                    recode,
                    "Twisp River" = "Twisp",
                    "Methow Hatchery outlet channel" = "Methow Fish Hatchery",
                    "no log for this date" = "Spring Creek",
                    "Winthrop NFH outfall (spring Creek)" = "Spring Creek"),
             Location = recode(River,
                               "Methow Fish Hatchery" = "Spring Creek"),
             Index = "Tributary") %>%
      select(any_of(names(redd_org)),
             Location)


  }

  #-----------------------------------------------------------------
  # manipulate some of the data
  # add some of the covariates that are constant year-to-year

  redd_org %<>%
    left_join(rch_lngth %>%
                clean_names(case = "upper_camel") %>%
                select(River,
                       Reach,
                       Index,
                       LengthKm),
              by = c("River", "Reach", "Index")) %>%
    left_join(thlwg_summ %>%
                mutate(Index = "Y") %>%
                select(Reach,
                       Index,
                       MeanThalwegCV),
              by = c("Reach", "Index")) %>%
    mutate(ExpSpTotal_log = log(ExpSpTotal + 1),
           NaiveDensity_km = VisibleRedds / LengthKm)

  # add a location
  redd_org %<>%
    mutate(Location = "Lower Methow")

  #-----------------------------------------------------------------
  # predict net error
  redd_df = predict_neterr(redd_org,
                           num_obs = "two")

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

  # adjust fish / redd for years when Priest sex calls are questionable
  # if(yr %in% c(2022)) {

    adj_fpr <- fpr_df %>%
      select(Location,
             n_male,
             n_female) %>%
      pivot_longer(cols = c(n_male,
                            n_female),
                   names_to = "sex",
                   values_to = "n_fish") %>%
      mutate(across(sex,
                    str_remove,
                    "^n_"),
             across(sex,
                    str_to_title)) %>%
      mutate(across(sex,
                    recode,
                    "Male" = "M",
                    "Female" = "F")) %>%
      left_join(sex_err_rate %>%
                  filter(brood_year == yr) %>%
                  mutate(perc_se = sqrt((perc_false * (1 - perc_false)) / n_tags)) %>%
                  select(sex,
                         starts_with("perc_"))) %>%
      pivot_wider(names_from = sex,
                  values_from = c(n_fish,
                                  perc_false,
                                  perc_se)) %>%
      mutate(true_male = n_fish_M - (n_fish_M * perc_false_M) + (n_fish_F * perc_false_F),
             true_female = n_fish_F - (n_fish_F * perc_false_F) + (n_fish_M * perc_false_M),
             across(starts_with("true"),
                    round_half_up)) %>%
      rowwise() %>%
      mutate(true_m_se = msm::deltamethod(~ x1 - (x1 * x2) + (x3 * x4),
                                          mean = c(n_fish_M,
                                                   perc_false_M,
                                                   n_fish_F,
                                                   perc_false_F),
                                          cov = diag(c(0,
                                                       perc_se_M,
                                                       0,
                                                       perc_se_F)^2)),
             true_f_se = msm::deltamethod(~ x1 - (x1 * x2) + (x3 * x4),
                                          mean = c(n_fish_F,
                                                   perc_false_F,
                                                   n_fish_M,
                                                   perc_false_M),
                                          cov = diag(c(0,
                                                       perc_se_F,
                                                       0,
                                                       perc_se_M)^2))) %>%
      mutate(n_sexed = true_male + true_female,
             prop_m = true_male / (true_male + true_female),
             prop_se = deltamethod(~ x1 / (x1 + x2),
                                   mean = c(true_male,
                                            true_female),
                                   cov = diag(c(true_m_se,
                                                true_f_se)^2)),
             fpr = (prop_m) / (1 - prop_m) + 1,
             fpr_se = deltamethod(~ x1 / (1 - x1) + 1,
                                  mean = prop_m,
                                  cov = prop_se^2)) %>%
      ungroup() %>%
      rename(n_male = true_male,
             n_female = true_female) %>%
      left_join(fpr_df %>%
                  select(Location,
                         n_wild:n_hor_c,
                         starts_with("phos"))) %>%
      select(any_of(names(fpr_df)))

    # if any fpr values are Inf, use the older ones
    adj_fpr %<>%
      left_join(fpr_df %>%
                  select(Location,
                         old_fpr = fpr,
                         old_se = fpr_se)) %>%
      mutate(fpr = if_else(is.na(fpr) | fpr == Inf,
                           old_fpr,
                           fpr),
             fpr_se = if_else(is.na(fpr_se) | fpr_se == Inf,
                              old_se,
                              fpr_se)) %>%
      select(-starts_with("old"))

    fpr_df <- adj_fpr

    rm(adj_fpr)
  # }



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

}
