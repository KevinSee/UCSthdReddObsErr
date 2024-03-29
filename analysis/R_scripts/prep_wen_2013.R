# Author: Kevin See
# Purpose: Prep redd data from 2013
# Created: 2/26/21
# Last Modified: 3/2/21
# Notes: this data is from the Wenatchee

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

# what year are we prepping?
yr = 2013

#-----------------------------------------------------------------
# read in data
file_nm = "STHD Redd Ob Eff Data_2011 to 2013.xlsx"

redd_org = read_excel(paste0('analysis/data/raw_data/', file_nm),
                      sheet = 2) %>%
  clean_names(case = "upper_camel") %>%
  rename(Year = River1,
         River = River2) %>%
  rename(ExpSpTotal = ExpTotal,
         MeanThalwegCV = MeanThalwegCv,
         MeanDepth = WaterDept,
         Length = ReachLength) %>%
  left_join(read_excel(paste0('analysis/data/raw_data/', file_nm),
                       sheet = 'Reach Variables') %>%
              select(Reach, Index,
                     ReachArea = Reach.Area)) %>%
  mutate(across(c(River, Reach, Index, SurveyType),
                as.factor)) %>%
  mutate(across(c(ExpSpTotal),
                as.numeric)) %>%
  mutate(across(MeanThalwegCV,
                ~ . * 100)) %>%
  mutate(across(c(SurveyDate),
                ymd)) %>%
  # mutate(across(Length,
  #               ~ measurements::conv_unit(. / 2,
  #                                         from  = "mi",
  #                                         to = 'km'))) %>%
  group_by(Year, River, Reach, Index) %>%
  mutate(VisibleRedds = cumsum(NewRedds)) %>%
  ungroup() %>%
  mutate(Day = yday(SurveyDate),
         ExpSpTotal_log = log(ExpSpTotal + 1),
         NaiveDensity_km = VisibleRedds / Length,
         MeanWidth = ReachArea / (Length * 1000)) %>%
  mutate(Reach = fct_relevel(Reach, 'W10', after = Inf)) %>%
  filter(Year == yr)

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
                         num_obs = "one")

#-----------------------------------------------------------------
# pull in PIT tag escapement results for use in converting redds to spawners

# where all the files from DabomPriestRapidsSthd GitHub repo kept?
dabom_pra_sthd_path = '../DabomPriestRapidsSthd'
# load DABOM results, including prepped data
load(paste0(dabom_pra_sthd_path, '/analysis/data/derived_data/model_fits/PRA_Steelhead_', yr, '_DABOM.rda'))

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
#   distinct() %>%
#   left_join(bio_df) %>%
#   select(TagID, TrapDate:CWT)

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
escp_file = tibble(file_nms = list.files(paste0(dabom_pra_sthd_path, "/outgoing/estimates"))) %>%
  filter(grepl(paste0("_", yr, "_"), file_nms)) %>%
  mutate(date = str_sub(file_nms, -13, -5),
         date = ymd(date)) %>%
  filter(date == max(date)) %>%
  pull(file_nms)

all_escp = read_excel(paste(dabom_pra_sthd_path, "outgoing/estimates", escp_file, sep = "/"),
                      'All Escapement')

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
     wen_tags,
     wen_sex_tab,
     wen_origin_tab,
     trib_spawners,
     escp_wen,
     file = paste0('analysis/data/derived_data/wen_', yr, '.rda'))
