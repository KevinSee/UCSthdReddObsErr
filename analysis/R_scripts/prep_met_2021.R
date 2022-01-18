# Author: Kevin See
# Purpose: Prep redd data from 2021
# Created: 11/4/2021
# Last Modified: 1/18/2022
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
load(paste0('../DabomPriestRapidsSthd/analysis/data/derived_data/PITcleanr/UC_Steelhead_', yr, '.rda'))
load(paste0('../DabomPriestRapidsSthd/analysis/data/derived_data/model_fits/PRA_DABOM_Steelhead_', yr, '.rda'))


tag_summ <- summarizeTagData(filter_obs,
                             bio_df %>%
                               group_by(tag_code) %>%
                               slice(1) %>%
                               ungroup())

# look at which branch each tag was assigned to for spawning
brnch_df = buildNodeOrder(addParentChildNodes(parent_child, configuration)) %>%
  separate(col = path,
           into = paste("step", 1:max(.$node_order), sep = "_"),
           remove = F) %>%
  mutate(group = if_else(node == "PRA",
                         "Start",
                         if_else(grepl('LWE', path) | node %in% c("CLK"),
                                 "Wenatchee",
                                 if_else(grepl("ENL", path),
                                         "Entiat",
                                         if_else(grepl("LMR", path),
                                                 "Methow",
                                                 if_else(grepl("OKL", path) | node %in% c("FST"),
                                                         "Okanogan",
                                                         if_else(step_2 != "RIA" & !is.na(step_2),
                                                                 "BelowPriest",
                                                                 if_else(node == "WEA",
                                                                         "WellsPool",
                                                                         "Other")))))))) %>%
  select(-starts_with("step")) %>%
  mutate(group = factor(group,
                        levels = c("Wenatchee",
                                   "Entiat",
                                   "Methow",
                                   "Okanogan",
                                   "BelowPriest",
                                   "WellsPool",
                                   "Start",
                                   "Other")))

tag_summ %<>%
  left_join(brnch_df,
            by = c("spawn_node" = "node"))


# pull out Methow tags
met_tags = tag_summ %>%
  filter(str_detect(path, "LMR")) %>%
  mutate(Area = ifelse(spawn_node == 'MRC',
                       'MRC_bb',
                       if_else(str_detect(path, 'MRC'),
                              'Tribs_above_MRC',
                              'Below_MRC'))) %>%
  mutate(Area = factor(Area,
                       levels = c("Below_MRC", 'MRC_bb', 'Tribs_above_MRC'))) %>%
  select(TagID = tag_code,
         Location = Area,
         Origin = origin,
         Sex = sex)

# add in tags from specific tribs
met_tags %<>%
  bind_rows(tag_summ %>%
              filter(str_detect(path, "TWR")) %>%
              mutate(Location = "Twisp") %>%
              select(TagID = tag_code,
                     Location,
                     Origin = origin,
                     Sex = sex)) %>%
  bind_rows(tag_summ %>%
              filter(str_detect(path, "CRW")) %>%
              mutate(Location = "Chewuch") %>%
              select(TagID = tag_code,
                     Location,
                     Origin = origin,
                     Sex = sex)) %>%
  bind_rows(tag_summ %>%
              filter(str_detect(path, "BVC")) %>%
              mutate(Location = "Beaver") %>%
              select(TagID = tag_code,
                     Location,
                     Origin = origin,
                     Sex = sex)) %>%
  bind_rows(tag_summ %>%
              filter(str_detect(path, "MRW")) %>%
              mutate(Location = "AboveWinthrop") %>%
              select(TagID = tag_code,
                     Location,
                     Origin = origin,
                     Sex = sex)) %>%
  mutate(Location = factor(Location,
                           levels = c("Below_MRC",
                                      "MRC_bb",
                                      "Tribs_above_MRC",
                                      "Twisp",
                                      "Chewuch",
                                      "Beaver",
                                      "AboveWinthrop")))

#-------------------------------------------------------
# if differentiating between above and below Tumwater
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
  group_by(Location, Origin) %>%
  summarise(n_tags = n(),
            .groups = "drop") %>%
  mutate(across(n_tags,
                replace_na,
                0)) %>%
  left_join(group_by(., Location) %>%
              summarise(tot_tags = sum(n_tags),
                        .groups = "drop")) %>%
  mutate(prop = n_tags / tot_tags,
         prop_se = sqrt((prop * (1 - prop)) / tot_tags))

met_origin_tab = met_prop_origin %>%
  select(Location, Origin, tot_tags, n_tags) %>%
  pivot_wider(names_from = Origin,
              values_from = n_tags,
              values_fill = 0) %>%
  rename(h_tags = H,
         w_tags = W) %>%
  left_join(met_prop_origin %>%
              select(Location, Origin, tot_tags, prop) %>%
              pivot_wider(names_from = Origin,
                          values_from = prop,
                          values_fill = 0) %>%
              rename(h_prop = H,
                     w_prop = W)) %>%
  mutate(prop_se = sqrt((h_prop * w_prop) / tot_tags))

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



trib_spawners = all_escp %>%
  filter(location %in% c("GLC",
                         "LBC",
                         "MSH",
                         "MRW",
                         "TWR",
                         "CRW",
                         "SCP",
                         "BVC")) %>%
  select(Origin = origin,
         Location = location,
         Spawners = estimate,
         Spawners_SE = se) %>%
  mutate(Origin = recode(Origin,
                         "W" = "Natural",
                         "H" = "Hatchery")) %>%
  arrange(Location, Origin)

escp_met = all_escp %>%
  filter(location %in% c('LMR',
                         'LMR_bb',
                         'MRC_bb')) %>%
  mutate(Area = recode(location,
                       'LMR' = 'Met_all',
                       'LMR_bb' = 'Below_MRC',
                       'MRC_bb' = 'MRC_bb')) %>%
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
     met_origin_tab,
     trib_spawners,
     escp_met,
     file = here('analysis/data/derived_data',
                 paste0('met_', yr, '.rda')))
