# Author: Kevin See
# Purpose: Prep steelhead redd data from the Wenatchee
# Created: 12/9/2022
# Last Modified: 1/5/2023
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
# yr = 2014

for(yr in c(2014:2022)) {

  if(yr == 2020) {
    next
  }

  cat(paste("\n\t Prepping year", yr, "\n\n"))

  #-----------------------------------------------------------------
  # read in data

  if(yr == 2014) {
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

  }

  if(yr == 2015) {
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
  }


  if(yr == 2016) {
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
  }

  if(yr == 2017) {
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
  }

  if(yr == 2018) {
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
  }

  if(yr == 2019) {
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
  }

  if(yr == 2021) {
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
  }

  #-----------------------------------------------------------------
  # read in redd counts below PIT tag arrays
  if(yr %in% c(2014:2021)) {

    if(sum(c("C1", "N1", "P1") %in% unique(redd_org$Reach)) > 0) {
      redds_below_arrays <- redd_org %>%
        filter(Reach %in% c("C1", "N1", "P1")) %>%
        mutate(Index = "Tributary")

      redd_org %<>%
        filter(!Reach %in% c("C1", "N1", "P1"))
    } else {

    redds_below_arrays = read_excel(here('analysis/data/raw_data',
                                         'Tributary Redds_Below Arrays_2014 to 2021.xlsx'),
                                    skip = 1) %>%
      rename(Year = `...1`,
             Total = `...5`,
             Notes = `...6`) %>%
      select(-Total, -Notes) %>%
      pivot_longer(starts_with("WEN"),
                   names_to = "Reach",
                   values_to = "NewRedds") %>%
      mutate(across(NewRedds,
                    as.integer)) %>%
      mutate(VisibleRedds = NewRedds) %>%
      mutate(across(Reach,
                    str_remove,
                    "^WEN-")) %>%
      mutate(River = if_else(Reach == "C1",
                             "Chiwawa",
                             if_else(Reach == "N1",
                                     "Nason",
                                     if_else(Reach == "P1",
                                             "Peshastin",
                                             NA_character_))),
             Location = River) %>%
      mutate(Index = "Tributary",
             SurveyType = "peak",
             NetError = 1,
             NetErrorSE = 0) %>%
      filter(Year == yr) %>%
      select(any_of(names(redd_org)),
             Location,
             NetError,
             NetErrorSE)
    }
  }

  if(yr == 2022) {

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

    redds_below_arrays = read_excel(here('analysis/data/raw_data',
                                         file_nm),
                                    sheet = paste("Tributaries", yr)) %>%
      mutate(River = if_else(Reach == "C1",
                             "Chiwawa",
                             if_else(Reach == "N1",
                                     "Nason",
                                     if_else(Reach == "P1",
                                             "Peshastin",
                                             NA_character_)))) %>%
      clean_names(case = "big_camel") %>%
      mutate(Location = River) %>%
      select(any_of(names(redd_org)),
             Location) %>%
      mutate(across(MeanEffortHrs,
                    ~ minute(.) / 60),
             across(ReachArea,
                    as.numeric))
  }



  #-----------------------------------------------------------------
  # manipulate some of the data

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

  # determine if reaches are in upper or lower mainstem
  redd_org %<>%
    mutate(Location = if_else(!grepl('W[[:digit:]]', Reach),
                              'Tribs_above_TUM',
                              if_else(Reach %in% paste0('W', 1:7),
                                      'Below_TUM',
                                      'TUM_bb')))

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
  wen_tags = tag_summ %>%
    filter(str_detect(path, "LWE")) %>%
    mutate(Area = if_else(spawn_node %in% c('TUM', 'UWE'),
                         'TUM_bb',
                         if_else(str_detect(spawn_node, "^LWE"),
                                 "Below_TUM",
                                 "Tributaries"))) %>%
    mutate(Area = factor(Area,
                         levels = c("Below_TUM", 'TUM_bb', 'Tributaries'))) %>%
    select(TagID = tag_code,
           Location = Area,
           Origin = origin,
           Sex = sex)



  # add in tags from specific tribs (will result in some duplicate tags)
  wen_tags %<>%
    # filter(Location != "Tribs_above_TUM") %>%
    bind_rows(tag_summ %>%
                filter(str_detect(path, "CHL")) %>%
                mutate(Location = "Chiwawa") %>%
                select(TagID = tag_code,
                       Location,
                       Origin = origin,
                       Sex = sex)) %>%
    bind_rows(tag_summ %>%
                filter(str_detect(path, "NAL")) %>%
                mutate(Location = "Nason") %>%
                select(TagID = tag_code,
                       Location,
                       Origin = origin,
                       Sex = sex)) %>%
    bind_rows(tag_summ %>%
                filter(str_detect(path, "PES")) %>%
                mutate(Location = "Peshastin") %>%
                select(TagID = tag_code,
                       Location,
                       Origin = origin,
                       Sex = sex)) %>%
    mutate(Location = factor(Location,
                             levels = c("Below_TUM",
                                        "TUM_bb",
                                        "Tributaries",
                                        "Peshastin",
                                        "Nason",
                                        "Chiwawa")))

  #-------------------------------------------------------
  # generate fish / redd and pHOS for different areas
  fpr_df = wen_tags %>%
    group_by(Location) %>%
    summarize(n_male = n_distinct(TagID[Sex == "M"]),
              n_female = n_distinct(TagID[Sex == "F"]),
              n_sexed = n_male + n_female,
              n_wild = n_distinct(TagID[Origin == "W"]),
              n_hatch = n_distinct(TagID[Origin == "H"]),
              n_origin = n_wild + n_hatch,
              .groups = "drop") %>%
    mutate(prop_m = n_male / n_sexed,
           prop_se = sqrt((prop_m * (1 - prop_m)) / (n_sexed)),
           fpr = (prop_m) / (1 - prop_m) + 1) %>%
    select(-contains("Wild"),
           -contains("Hatchery")) %>%
    rowwise() %>%
    mutate(fpr_se = deltamethod(~ x1 / (1 - x1) + 1,
                                mean = prop_m,
                                cov = prop_se^2)) %>%
    ungroup() %>%
    mutate(phos = n_hatch / n_origin,
           phos_se = sqrt((phos * (1 - phos)) / (n_origin)))

  # adjust fish / redd for years when Priest sex calls are questionable
  if(yr %in% c(2022)) {
    data("sex_err_rate")

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
      left_join(sex_err_rate %>%
                  filter(brood_year == yr) %>%
                  mutate(perc_se = sqrt((perc_false * (1 - perc_false)) / n_tags)) %>%
                  select(sex,
                         starts_with("perc_"))) %>%
      mutate(across(sex,
                    recode,
                    "Male" = "M",
                    "Female" = "F")) %>%
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
                         n_hatch,
                         n_origin,
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
  }



#
#   #---------------------------------------------------------
#   # divide Tumwater bb escapement into origin and sex groups
#   # estimate escapement, subtract known removals by origin and sex
#   tum_est <- escape_summ %>%
#     filter(location == "TUM_bb") %>%
#     select(Origin = origin, location, median, sd) %>%
#     left_join(wen_tags %>%
#                 filter(Location == "TUM_bb") %>%
#                 count(Origin, Sex) %>%
#                 group_by(Origin) %>%
#                 mutate(sex_prop = n / sum(n),
#                        sex_prop_se = sqrt((sex_prop * (1 - sex_prop)) / sum(n)))) %>%
#     rowwise() %>%
#     mutate(escp = median * sex_prop,
#            escp_se = deltamethod(~ x1 * x2,
#                                  mean = c(median,
#                                           sex_prop),
#                                  cov = diag(c(sd,
#                                               sex_prop_se)^2))) %>%
#     ungroup() %>%
#     select(location,
#            Origin,
#            Sex,
#            starts_with("escp")) %>%
#     mutate(rem = 0) %>%
#     mutate(est = escp - rem)
#
#   # generate fish / redd and pHOS statistics from remaining fish
#   fpr_df <- tum_est %>%
#     summarize(m_tot = sum(est[Sex == "M"]),
#               m_se = sqrt(sum(escp_se[Sex == "M"]^2)),
#               f_tot = sum(est[Sex == "F"]),
#               f_se = sqrt(sum(escp_se[Sex == "F"]^2)),
#               h_tot = sum(est[Origin == "H"]),
#               h_se = sqrt(sum(escp_se[Origin == "H"]^2)),
#               w_tot = sum(est[Origin == "W"]),
#               w_se = sqrt(sum(escp_se[Origin == "W"]^2))) %>%
#     mutate(fpr = m_tot / f_tot + 1,
#            fpr_se = deltamethod(~ x1 / x2 + 1,
#                                 mean = c(m_tot,
#                                          f_tot),
#                                 cov = diag(c(m_se,
#                                              f_se)^2)),
#            phos = h_tot / (h_tot + w_tot),
#            phos_se = deltamethod(~ x1 / (x1 + x2),
#                                  mean = c(h_tot,
#                                           w_tot),
#                                  cov = diag(c(h_se,
#                                               w_se)^2))) %>%
#     add_column(Location = c("TUM_bb"),
#                .before = 1) %>%
#     select(Location,
#            starts_with("fpr"),
#            starts_with("phos"))
#
#   # assign this fish/redd and pHOS value to mainstem below Tumwater too
#   fpr_df %<>%
#     bind_rows(fpr_df %>%
#                 mutate(Location = "Below_TUM"))



  #-----------------------------------------------------------------
  # pull in some estimates from DABOM
  all_escp = escape_summ %>%
    filter(location %in% c('ICL',
                           'PES',
                           'MCL',
                           'CHM',
                           'CHW',
                           'CHL',
                           'NAL',
                           'LWN',
                           'WTL',
                           'LWE',
                           'LWE_bb',
                           'TUM_bb',
                           'UWE_bb'))

  # pull out estimates of tributary spawners from DABOM
  trib_spawners = all_escp %>%
    filter(location %in% c('ICL',
                           'PES',
                           'MCL',
                           'CHM',
                           'CHW',
                           'CHL',
                           'NAL',
                           'LWN',
                           'WTL')) %>%
    select(Origin = origin,
           Location = location,
           Spawners = median,
           Spawners_SE = sd) %>%
    mutate(Origin = recode(Origin,
                           "W" = "Natural",
                           "H" = "Hatchery")) %>%
    arrange(Location, Origin)

  # pull out mainstem escapement estimates
  escp_wen = all_escp %>%
    filter(location %in% c('LWE',
                           'LWE_bb',
                           'TUM_bb',
                           'UWE_bb')) %>%
    mutate(Area = recode(location,
                         'LWE' = 'Wen_all',
                         'LWE_bb' = 'Below_TUM',
                         'UWE_bb' = 'TUM_bb')) %>%
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
       wen_tags,
       fpr_df,
       trib_spawners,
       escp_wen,
       file = here('analysis/data/derived_data',
                   paste0('wen_', yr, '.rda')))

}
