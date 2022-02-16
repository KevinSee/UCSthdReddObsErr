# Author: Kevin See
# Purpose: Prep redd data from 2021
# Created: 11/4/2021
# Last Modified: 2/14/2022
# Notes: this data is from the Methow

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
yr = 2021

#-----------------------------------------------------------------
# read in data
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

#-----------------------------------------------------------------
# predict net error
redd_df = predict_neterr(redd_org,
                         num_obs = "two")

#-----------------------------------------------------------------
# pull in PIT tag escapement results for use in converting redds to spawners

# load DABOM results, including prepped data
load('../DabomPriestRapidsSthd/analysis/data/derived_data/site_config.rda')
load(paste0('../DabomPriestRapidsSthd/analysis/data/derived_data/model_fits/PRA_DABOM_Steelhead_', yr, '.rda'))


tag_summ <- summarizeTagData(filter_obs,
                             bio_df %>%
                               group_by(tag_code) %>%
                               slice(1) %>%
                               ungroup())

# look at which branch each tag was assigned to for spawning
# look at which branch each tag was assigned to for spawning
brnch_df = buildNodeOrder(addParentChildNodes(parent_child,
                                              configuration))

tag_summ %<>%
  left_join(brnch_df,
            by = c("spawn_node" = "node"))


# pull out Methow tags
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
         CWT = cwt)

# differentiate different tags in hatchery fish
met_tags %<>%
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

# # filter out some areas not needed for the report
# met_tags %<>%
#   filter(Location %in% c("Lower Methow",
#                          "Upper Methow",
#                          "Chewuch",
#                          "Twisp")) %>%
#   mutate(across(Location,
#                 fct_drop))

#-------------------------------------------------------
# if calculating a different fish / redd in each area
met_prop_sex = met_tags %>%
  group_by(Location, Sex) %>%
  summarise(n_tags = n()) %>%
  ungroup() %>%
  left_join(group_by(., Location) %>%
              summarise(tot_tags = sum(n_tags))) %>%
  mutate(prop = n_tags / tot_tags,
         prop_se = sqrt((prop * (1 - prop)) / tot_tags))

met_sex_tab = met_prop_sex %>%
  select(Location, Sex, tot_tags, n_tags) %>%
  pivot_wider(names_from = Sex,
              values_from = n_tags,
              values_fill = 0) %>%
  rename(f_tags = F,
         m_tags = M) %>%
  left_join(met_prop_sex %>%
              select(Location, Sex, tot_tags, prop) %>%
              pivot_wider(names_from = Sex,
                          values_from = prop,
                          values_fill = 0) %>%
              rename(f_prop = F,
                     m_prop = M)) %>%
  rowwise() %>%
  mutate(prop_se = sqrt((f_prop * m_prop) / tot_tags),
         # calculate fish / redd as F:M + 1
         fpr = (m_prop / f_prop) + 1,
         fpr_se = deltamethod(~ (x1 / x2) + 1,
                              mean = c(m_prop, f_prop),
                              cov = diag(prop_se^2, nrow = 2)))

met_prop_origin = met_tags %>%
  group_by(Location, Origin = Group) %>%
  summarise(n_tags = n(),
            .groups = "drop") %>%
  full_join(expand(met_tags, Location, Origin = Group)) %>%
  arrange(Location, Origin) %>%
  mutate(across(n_tags,
                replace_na,
                0)) %>%
  left_join(group_by(., Location) %>%
              summarise(tot_tags = sum(n_tags),
                        .groups = "drop")) %>%
  mutate(prop = n_tags / tot_tags,
         prop_se = sqrt((prop * (1 - prop)) / tot_tags)) %>%
  relocate(tot_tags, .after = Origin)

met_origin_tab = met_prop_origin %>%
  select(Location, Origin, tot_tags, n_tags) %>%
  pivot_wider(names_from = Origin,
              values_from = n_tags,
              values_fill = 0) %>%
  left_join(met_prop_origin %>%
              select(Location, Origin, tot_tags, prop) %>%
              pivot_wider(names_from = Origin,
                          values_from = prop,
                          values_fill = 0) %>%
              rename_with(.fn = function(x) paste(x, "p", sep = "_"),
                          .cols = -c(1:2)))

#-----------------------------------------------------------------
# pull in some estimates from DABOM
est_files = list.files('../DabomPriestRapidsSthd/outgoing/estimates')
est_files <- est_files[str_detect(est_files, paste0("_", yr, "_"))]
est_dates = ymd(str_sub(est_files, -13, -6))
max_date = max(est_dates) %>%
  format("%Y%m%d")

all_escp = read_excel(paste0('../DabomPriestRapidsSthd/outgoing/estimates/UC_Steelhead_', yr, '_', max_date, '.xlsx'),
                      'All Escapement')


#-----------------------------------------------------------------
# read in redd counts below PIT tag arrays
# redds_below_arrays = read_excel(here('analysis/data/raw_data',
#                 file_nm),
#            sheet = "Tributaries 2021") %>%
#   group_by(River, Reach) %>%
#   summarise(redd_est = sum(New.Redds),
#             redd_se = 0,
#             .groups = "drop")

# pull out specific escapement estimates for the Methow
escp_met = all_escp %>%
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
                         "BVC")) %>%
  mutate(Area = recode(location,
                       'LMR' = 'Methow_all',
                       'LMR_bb' = 'Lower Methow',
                       'MRC_bb' = 'Lower Methow',
                       "GLC" = "Gold",
                       "LBC" = "Libby",
                       "MSH" = "Methow Fish Hatchery",
                       "MRW" = "Upper Methow",
                       "TWR" = "Twisp",
                       "CRW" = "Chewuch",
                       "SCP" = "Spring Creek",
                       "BVC" = "Beaver")) %>%
  mutate(Origin = recode(origin,
                         "W" = "Natural",
                         "H" = "Hatchery")) %>%
  group_by(Area,
           Origin) %>%
  summarise(estimate = sum(estimate),
            se = sqrt(sum(se^2)),
            .groups = "drop")




#-----------------------------------------------------------------
# save
save(redd_df,
     # redds_below_arrays,
     met_tags,
     met_sex_tab,
     met_prop_origin,
     met_origin_tab,
     escp_met,
     file = here('analysis/data/derived_data',
                 paste0('met_', yr, '.rda')))
