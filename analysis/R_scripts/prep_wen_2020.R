# Author: Kevin See
# Purpose: Prep redd data from 2020
# Created: 12/16/2020
# Last Modified: 12/9/2022
# Notes: this data is from the Wenatchee
# No redd data available this year due to COVID (prevented redd surveys from being conducted)

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
yr = 2020


#-----------------------------------------------------------------
# pull in PIT tag escapement results for use in converting redds to spawners

# load DABOM results, including prepped data
# load(paste0('../DabomPriestRapidsSthd/analysis/data/derived_data/PITcleanr/UC_Steelhead_', yr, '.rda'))
load('O:/Documents/Git/MyProjects/DabomPriestRapidsSthd/analysis/data/derived_data/site_config.rda')

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
                                      "Tribs_above_TUM",
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
save(wen_tags,
     fpr_df,
     trib_spawners,
     escp_wen,
     file = paste0('analysis/data/derived_data/wen_', yr, '.rda'))
