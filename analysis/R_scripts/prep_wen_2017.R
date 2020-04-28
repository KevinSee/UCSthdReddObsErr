# Author: Kevin See
# Purpose: Prep redd data from 2017
# Created: 4/28/2020
# Last Modified: 4/28/2020
# Notes: this data is from the Wenatchee

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(magrittr)
library(msm)
library(SthdReddObsError)
library(PITcleanr)

#-----------------------------------------------------------------
# read in data
file_nm = "2017_CensusData_Final.xlsx"
redd_org = read_excel(paste0('analysis/data/raw_data/', file_nm),
                      sheet = 1) %>%
  clean_names(case = "upper_camel") %>%
  rename(ExpSpTotal = ExpTotal,
         MeanThalwegCV = MeanThalwegCv) %>%
  left_join(read_excel(paste0('analysis/data/raw_data/', file_nm),
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

# # fix one thalweg data point
# redd_org %<>%
#   mutate_at(vars(MeanThalwegCV),
#             list(~ if_else(Reach == 'W10' & Index == 'Y',
#                            58.0,
#                            .)))

#-----------------------------------------------------------------
# predict net error
redd_df = predict_neterr(redd_org)

#-----------------------------------------------------------------
# pull in PIT tag escapement results for use in converting redds to spawners

# load prepped data
load('../DABOM_PriestRapids_Sthd_old/modelFits/PRA_Steelhead_2017_DABOM.rda')
# # fix a few prefixes for tags with known issues (Ben Truscott had to enter them incorrectly in his database)
# bio_df %<>%
#   mutate(TagID = ifelse(!TagID %in% dabom_df$TagID,
#                         str_replace(TagID,
#                                     '^3DD',
#                                     '3DA'),
#                         TagID))

tag_summ = summariseTagData(proc_list$ProcCapHist %>%
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
  rename(Branch = Group)

wen_tags = tag_summ %>%
  filter(grepl('LWE', TagPath)) %>%
  mutate(Area = ifelse(AssignSpawnNode == 'TUM',
                       'TUM_bb',
                       ifelse(grepl('TUM', TagPath),
                              'Tribs_above_TUM',
                              'Below_TUM'))) %>%
  mutate(Area = factor(Area,
                       levels = c("Below_TUM", 'TUM_bb', 'Tribs_above_TUM'))) %>%
  select(TagID, Location = Area, Origin, Sex)

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
  summarise(n_tags = n()) %>%
  ungroup() %>%
  left_join(group_by(., Location) %>%
              summarise(tot_tags = sum(n_tags))) %>%
  mutate(prop = n_tags / tot_tags,
         prop_se = sqrt((prop * (1 - prop)) / tot_tags))

wen_origin_tab = wen_prop_origin %>%
  select(Location, Origin, tot_tags, n_tags) %>%
  spread(Origin, n_tags) %>%
  rename(h_tags = H,
         w_tags = W) %>%
  left_join(wen_prop_origin %>%
              select(Location, Origin, tot_tags, prop) %>%
              spread(Origin, prop) %>%
              rename(h_prop = H,
                     w_prop = W)) %>%
  mutate(prop_se = sqrt((h_prop * w_prop) / tot_tags))

# pull in some estimates from DABOM
all_escp = read_excel('../DABOM_PriestRapids_Sthd_old/outgoing/PRA_Steelhead_2017_20181218.xlsx',
                      'All Escapement') %>%
  mutate_at(vars(skew, kurtosis),
            list(as.numeric))

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
  select(Origin, Location = param, Spawners = median, Spawners_SE = sd) %>%
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
  summarise(mean = sum(mean),
            median = sum(median),
            mode = sum(mode),
            se = sqrt(sum(sd^2)))

#-----------------------------------------------------------------
# known removals at Tumwater and Dryden dams, by origin, for broodstock or surplussed
# also account for any harvest by origin
rem_df = crossing(Source = c('Tumwater', 'Dryden', 'Harvest'),
                  Origin = c('Natural', 'Hatchery')) %>%
  mutate(rem = c(13, 0,
                 0, 0,
                 69, 62)) %>%
  mutate(Area = recode(Source,
                       'Tumwater' = 'TUM_bb',
                       'Dryden' = 'Below_TUM'))

#-----------------------------------------------------------------
# save
save(redd_df,
     wen_sex_tab,
     wen_origin_tab,
     trib_spawners,
     escp_wen,
     rem_df,
     file = 'analysis/data/derived_data/wen_2017.rda')
