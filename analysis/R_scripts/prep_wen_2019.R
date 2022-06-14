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
yr = 2019

#-----------------------------------------------------------------
# read in data
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

load('../DabomPriestRapidsSthd/analysis/data/derived_data/site_config.rda')
load(paste0('../DabomPriestRapidsSthd/analysis/data/derived_data/model_fits/PRA_DABOM_Steelhead_', yr, '.rda'))


tag_summ <- summarizeTagData(filter_obs,
                             bio_df %>%
                               group_by(tag_code) %>%
                               slice(1) %>%
                               ungroup())

# look at which branch each tag was assigned to for spawning
brnch_df = buildNodeOrder(addParentChildNodes(parent_child,
                                              configuration))

tag_summ %<>%
  left_join(brnch_df,
            by = c("spawn_node" = "node"))


# pull out Wenatchee tags
wen_tags = tag_summ %>%
  filter(str_detect(path, "LWE")) %>%
  mutate(Area = ifelse(spawn_node == 'TUM',
                       'TUM_bb',
                       if_else(str_detect(path, 'TUM'),
                               'Tribs_above_TUM',
                               'Below_TUM'))) %>%
  mutate(Area = factor(Area,
                       levels = c("Below_TUM", 'TUM_bb', 'Tribs_above_TUM'))) %>%
  select(TagID = tag_code,
         Location = Area,
         Origin = origin,
         Sex = sex)



# add in tags from specific tribs (will result in some duplicate tags)
wen_tags %<>%
  # filter(Location != "Tribs_above_TUM") %>%
  bind_rows(tag_summ %>%
              filter(str_detect(spawn_node, "CHL")) %>%
              mutate(Location = "Chiwawa") %>%
              select(TagID = tag_code,
                     Location,
                     Origin = origin,
                     Sex = sex)) %>%
  bind_rows(tag_summ %>%
              filter(str_detect(spawn_node, "NAL")) %>%
              mutate(Location = "Nason") %>%
              select(TagID = tag_code,
                     Location,
                     Origin = origin,
                     Sex = sex)) %>%
  bind_rows(tag_summ %>%
              filter(str_detect(spawn_node, "PES")) %>%
              mutate(Location = "Peshastin") %>%
              select(TagID = tag_code,
                     Location,
                     Origin = origin,
                     Sex = sex)) %>%
  mutate(Location = factor(Location,
                           levels = c("Below_TUM",
                                      "TUM_bb",
                                      "Tribs_above_TUM",
                                      "Peshastin",
                                      "Nason",
                                      "Chiwawa")))


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
  pivot_wider(names_from = Sex,
              values_from = n_tags,
              values_fill = 0) %>%
  rename(f_tags = F,
         m_tags = M) %>%
  left_join(wen_prop_sex %>%
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
dabom_folder = '../DabomPriestRapidsSthd/outgoing/estimates'
yr_est_files = list.files(dabom_folder)[str_detect(list.files(dabom_folder), paste0("_", yr, "_"))]
max_date <- str_sub(yr_est_files, -13, -6) %>%
  ymd() %>%
  max() %>%
  format("%Y%m%d")

all_escp <- read_excel(paste(dabom_folder,
                             yr_est_files[str_detect(yr_est_files, max_date)],
                             sep = "/"),
                       "All Escapement")


# all_escp = read_excel(paste0('../DabomPriestRapidsSthd/outgoing/estimates/PRA_Steelhead_', yr, '_20210208.xlsx'),
#                       'All Escapement')


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
  filter(location %in% c('ICL',
                         'PES',
                         'MCL',
                         'CHM',
                         'CHW',
                         'CHL',
                         'NAL',
                         'LWN',
                         'WTL')) %>%
  mutate(origin = recode(origin,
                         "1" = "W",
                         "2" = "H")) %>%
  select(Origin = origin,
         Location = location,
         Spawners = estimate,
         Spawners_SE = se) %>%
  mutate(Origin = recode(Origin,
                         "W" = "Natural",
                         "H" = "Hatchery")) %>%
  arrange(Location, Origin)

escp_wen = all_escp %>%
  filter(location %in% c('LWE',
                         'LWE_bb',
                         'TUM_bb',
                         'UWE_bb')) %>%
  mutate(Area = recode(location,
                       'LWE' = 'Wen_all',
                       'LWE_bb' = 'Below_TUM',
                       'UWE_bb' = 'TUM_bb')) %>%
  mutate(origin = recode(origin,
                         "1" = "W",
                         "2" = "H")) %>%
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
     redds_below_arrays,
     wen_tags,
     wen_sex_tab,
     wen_origin_tab,
     trib_spawners,
     escp_wen,
     file = here('analysis/data/derived_data',
                 paste0('wen_', yr, '.rda')))
