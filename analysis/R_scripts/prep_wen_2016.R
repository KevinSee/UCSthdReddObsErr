# Author: Kevin See
# Purpose: Prep redd data from 2019
# Created: 1/29/2020
# Last Modified: 10/19/2021
# Notes: this data is from the Wenatchee

#-----------------------------------------------------------------
# install old version of PITcleanr
remotes::install_github("KevinSee/PITcleanr",
                        ref = "v1.2.0")

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
yr = 2016

#-----------------------------------------------------------------
# read in data
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

#-----------------------------------------------------------------
# predict net error
redd_df = predict_neterr(redd_org,
                         num_obs = "two")

#-----------------------------------------------------------------
# pull in PIT tag escapement results for use in converting redds to spawners

# load DABOM results, including prepped data
load(paste0('../DabomPriestRapidsSthd/analysis/data/derived_data/model_fits/PRA_Steelhead_', yr, '_DABOM.rda'))

tag_summ = summariseTagData(proc_list$proc_ch %>%
                              mutate(lastObsDate = ObsDate) %>%
                              left_join(proc_list$NodeOrder %>%
                                          mutate(Group = fct_expand(Group, 'WellsPool'),
                                                 Group = if_else(Node == 'WEA',
                                                                 'WellsPool',
                                                                 as.character(Group)),
                                                 Group = ifelse(NodeSite %in% c('FST', 'RRF', 'WVT', 'RIA', 'CLK'),
                                                                NA,
                                                                Group),
                                                 Group = factor(Group,
                                                                levels = c('Wenatchee', 'Entiat', 'Methow', 'Okanogan', 'WellsPool', 'BelowPriest')))) %>%
                              mutate(SiteID = NodeSite),
                            trap_data = bio_df %>%
                              mutate(Age = str_replace(Age, 'r', 'R'))) %>%
  rename(Branch = Group) %>%
  distinct() %>%
  # deal with duplicated records (usually from fish caught in the trap twice)
  arrange(TagID, TrapDate) %>%
  group_by(TagID) %>%
  slice(1) %>%
  ungroup()

# any duplicated tags?
identical(nrow(tag_summ),
          n_distinct(tag_summ$TagID))

# n_distinct(bio_df$TagID)
#
# tag_summ %>%
#   filter(TagID %in% TagID[duplicated(TagID)]) %>%
#   # filter(is.na(tag_summ$Age)) %>%
#   select(TagID) %>%
#   left_join(bio_df) %>%
#   select(TagID, TrapDate:CWT)

wen_tags = tag_summ %>%
  select(-TagPath) %>%
  left_join(proc_list$NodeOrder %>%
              select(AssignSpawnNode = Node,
                     TagPath = Path),
            by = "AssignSpawnNode") %>%
  filter(grepl('LWE', TagPath)) %>%
  mutate(Area = ifelse(AssignSpawnNode == 'TUM',
                       'TUM_bb',
                       ifelse(grepl('TUM', TagPath),
                              'Tribs_above_TUM',
                              'Below_TUM'))) %>%
  mutate(Area = factor(Area,
                       levels = c("Below_TUM", 'TUM_bb', 'Tribs_above_TUM'))) %>%
  select(TagID, Location = Area, Origin, Sex)

# add in tags from specific tribs
wen_tags %<>%
  # filter(Location != "Tribs_above_TUM") %>%
  bind_rows(tag_summ %>%
              filter(str_detect(AssignSpawnNode, "CHL")) %>%
              mutate(Location = "Chiwawa") %>%
              select(TagID, Location, Origin, Sex)) %>%
  bind_rows(tag_summ %>%
              filter(str_detect(AssignSpawnNode, "NAL")) %>%
              mutate(Location = "Nason") %>%
              select(TagID, Location, Origin, Sex)) %>%
  bind_rows(tag_summ %>%
              filter(str_detect(AssignSpawnNode, "PES")) %>%
              mutate(Location = "Peshastin") %>%
              select(TagID, Location, Origin, Sex)) %>%
  mutate(Location = factor(Location,
                           levels = c("Below_TUM",
                                      "TUM_bb",
                                      "Tribs_above_TUM",
                                      "Peshastin",
                                      "Nason",
                                      "Chiwawa")))

# # remove duplicated tags
# wen_tags %<>%
#   filter(!TagID %in% TagID[duplicated(TagID)]) %>%
#   bind_rows(wen_tags %>%
#               filter(TagID %in% TagID[duplicated(TagID)]) %>%
#               filter(str_detect(Location,
#                                 "_TUM$",
#                                 negate = T)))

#-------------------------------------------------------
# if differentiating between above and below Tumwater
wen_prop_sex = wen_tags %>%
  group_by(Location, Sex) %>%
  summarise(n_tags = n()) %>%
  ungroup() %>%
  left_join(group_by(., Location) %>%
              summarise(tot_tags = sum(n_tags))) %>%
  mutate(prop = n_tags / tot_tags,
         prop_se = sqrt((prop * (1 - prop)) / tot_tags))

wen_sex_tab = wen_prop_sex %>%
  select(Location, Sex, tot_tags, n_tags) %>%
  spread(Sex, n_tags) %>%
  rename(f_tags = F,
         m_tags = M) %>%
  left_join(wen_prop_sex %>%
              select(Location, Sex, tot_tags, prop) %>%
              spread(Sex, prop) %>%
              rename(f_prop = F,
                     m_prop = M)) %>%
  rowwise() %>%
  mutate(prop_se = sqrt((f_prop * m_prop) / tot_tags),
         # calculate fish / redd as F:M + 1
         fpr = (m_prop / f_prop) + 1,
         fpr_se = deltamethod(~ (x1 / x2) + 1,
                              mean = c(m_prop, f_prop),
                              cov = diag(prop_se^2, nrow = 2)))

wen_prop_origin = wen_tags %>%
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

wen_origin_tab = wen_prop_origin %>%
  select(Location, Origin, tot_tags, n_tags) %>%
  pivot_wider(names_from = Origin,
              values_from = n_tags,
              values_fill = 0) %>%
  rename(h_tags = H,
         w_tags = W) %>%
  left_join(wen_prop_origin %>%
              select(Location, Origin, tot_tags, prop) %>%
              pivot_wider(names_from = Origin,
                          values_from = prop,
                          values_fill = 0) %>%
              rename(h_prop = H,
                     w_prop = W)) %>%
  mutate(prop_se = sqrt((h_prop * w_prop) / tot_tags))

#-----------------------------------------------------------------
# pull in some estimates from DABOM
all_escp = read_excel(paste0('../DabomPriestRapidsSthd/outgoing/estimates/PRA_Steelhead_', yr, '_20200604.xlsx'),
                      'All Escapement')


#-----------------------------------------------------------------
# read in redd counts below PIT tag arrays
redds_below_arrays = read_excel(here('analysis/data/raw_data',
                                     'Tributary Redds_Below Arrays_2014 to 2021.xlsx'),
                                skip = 1) %>%
  rename(Year = `...1`,
         Total = `...5`,
         Notes = `...6`) %>%
  filter(Year == yr) %>%
  select(starts_with("WEN")) %>%
  pivot_longer(everything(),
               names_to = "Reach",
               values_to = "redd_est") %>%
  mutate(across(redd_est,
                as.numeric)) %>%
  mutate(across(Reach,
                str_remove,
                "^WEN-")) %>%
  mutate(redd_se = 0) %>%
  add_column(River = "Wenatchee",
             .before = 0)

trib_spawners = all_escp %>%
  filter(param %in% c('past_ICL',
                      'past_PES',
                      'past_MCL',
                      'past_CHM',
                      'past_CHW',
                      'past_CHL',
                      'past_NAL',
                      'past_LWN',
                      'past_WTL')) %>%
  select(Origin, Location = param, Spawners = estimate, Spawners_SE = se) %>%
  arrange(Location, Origin)

escp_wen = all_escp %>%
  filter(param %in% c('past_LWE',
                      'LWE_bb',
                      'TUM_bb',
                      'UWE_bb')) %>%
  mutate(Area = recode(param,
                       'past_LWE' = 'Wen_all',
                       'LWE_bb' = 'Below_TUM',
                       'UWE_bb' = 'TUM_bb')) %>%
  group_by(Area, Origin) %>%
  summarise(estimate = sum(estimate),
            se = sqrt(sum(se^2)))

#-----------------------------------------------------------------
# save
save(redd_df,
     redds_below_arrays,
     wen_tags,
     wen_sex_tab,
     wen_origin_tab,
     trib_spawners,
     escp_wen,
     file = here('analysis/data/derived_data',
                 paste0('wen_', yr, '.rda')))
