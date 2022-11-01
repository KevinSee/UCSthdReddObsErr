# Author: Kevin See
# Purpose: Pull together relevant data for UC HETT discussion
# Created: 10/31/22
# Last Modified: 10/31/2022
# Notes: focus on 2011-2021, really 2014 - 2021

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
data_file = here("analysis/data/raw_data",
                 "Final_Historical covariates Wenatchee Steelhead 5-23-22.xlsx")
# excel_sheets(data_file)

# what method should be used for each reach / year?
method_df <- read_excel(data_file,
                        1) %>%
  # W9 was listed twice, and one row is incorrect
  filter(!(Reach == "W9" & `2012` == "Redd")) %>%
  pivot_longer(-c(1:2),
               names_to = "year",
               values_to = "method") %>%
  mutate(across(year,
                as.numeric)) %>%
  clean_names() %>%
  mutate(reach = if_else(reach == "P2" & year %in% 2004:2010 & method == "Redd",
                         "P3 (P2)",
                         reach)) %>%
  mutate(across(reach,
                as_factor)) %>%
  distinct() %>%
  mutate(across(river,
                recode,
                "Little Wen" = "Little Wenatchee",
                "White" = "White River"),
         across(river,
                str_remove,
                " Ck")) %>%
  arrange(river, reach, year)

# # for 2004-2010, P3 and P4 are lumped in with P2
# # but we'll call P4 the non-index stretch, so let's keep P4
# method_df %<>%
#   filter(!(year %in% c(2004:2010) &
#              reach %in% c("P3")))
#
# # add a couple reaches not included in the original file
# method_df %<>%
#   mutate(across(reach,
#                 fct_expand,
#                 "H2", "L2")) %>%
#   bind_rows(tibble(river = c("Little Wenatchee",
#                              "White River"),
#                    reach = factor(c("L2",
#                                     "H2")),
#                    method = "Redd") %>%
#               crossing(year = 2004:2010)) %>%
#   mutate(across(reach,
#                 fct_relevel,
#                 "H2",
#                 after = 24),
#          across(reach,
#                 fct_relevel,
#                 "L2",
#                 after = 23)) %>%
#   arrange(river, reach)


# drop years prior to 2011
method_df %<>%
  filter(year >= 2011)

#-----------------------------------------------------------------
# redd data
#-----------------------------------------------------------------

# non-index reaches
non_index_rch <- read_excel(data_file,
                            "Non-index Reaches") %>%
  clean_names() %>%
  rename(river = stream) %>%
  select(-x6) %>%
  mutate(across(non_index_reach,
                recode,
                "P2" = "P4"),
         across(index_reach,
                recode,
                "P2" = "P3 (P2)")) %>%
  mutate(across(river,
                recode,
                "Little Wen" = "Little Wenatchee",
                "White" = "White River"),
         across(river,
                str_remove,
                " Ck")) %>%
  mutate(location = if_else(!grepl('W[[:digit:]]', non_index_reach),
                            river,
                            if_else(non_index_reach %in% paste0('W', 1:7),
                                    'Below_TUM',
                                    'Above_TUM'))) %>%
  relocate(location,
           .after = "river") %>%
  arrange(year,
          river,
          location,
          non_index_reach,
          date) %>%
  left_join(method_df %>%
              rename(non_index_reach = reach)) %>%
  filter(method != "PIT",
         year %in% unique(method_df$year))

# index reaches
index_rch <- read_excel(data_file,
                        "Index Reaches") %>%
  clean_names() %>%
  rename(river = stream) %>%
  mutate(across(river,
                recode,
                "Little Wen" = "Little Wenatchee",
                "White" = "White River"),
         across(river,
                str_remove,
                " Ck")) %>%
  mutate(location = if_else(!grepl('W[[:digit:]]', index_reach),
                            river,
                            if_else(index_reach %in% paste0('W', 1:7),
                                    'Below_TUM',
                                    'Above_TUM'))) %>%
  relocate(location,
           .after = "river") %>%
  filter(year %in% unique(method_df$year)) %>%
  arrange(year,
          river,
          location,
          index_reach,
          date)

# no-error reaches
no_err_rch <- read_excel(data_file,
                         "No Error Tribs") %>%
  clean_names() %>%
  mutate(across(major,
                recode,
                "Little Wen" = "Little Wenatchee",
                "White" = "White River"),
         across(major,
                str_remove,
                " Ck")) %>%
  mutate(location = if_else(major == "Wenatchee",
                            "Above_TUM",
                            major)) %>%
  relocate(location,
           .after = "major") %>%
  filter(year %in% unique(method_df$year))

# put a couple of index reaches into the no-error reaches
no_err_rch %<>%
  bind_rows(index_rch %>%
              filter(index_reach %in% c("H2",
                                        "H3",
                                        "L2",
                                        "L3")) %>%
              group_by(year,
                       river,
                       location,
                       index_reach) %>%
              summarize(total = sum(redds,
                                    na.rm = T),
                        .groups = "drop") %>%
              rename(major = river,
                     minor = index_reach)) %>%
  arrange(year,
          major,
          minor)

index_rch %<>%
  filter(!index_reach %in% c("H2",
                             "H3",
                             "L2",
                             "L3"))

index_rch %<>%
  left_join(method_df %>%
              rename(index_reach = reach))


# combine all reaches, and sum total observed redds in each reach, each year
all_rch <- index_rch %>%
  # mutate(across(index_reach,
  #               recode,
  #               "P3 (P2)" = "P2")) %>%
  group_by(year, river,
           location,
           reach = index_reach) %>%
  summarize(across(redds,
                   sum,
                   na.rm = T),
            .groups = "drop") %>%
  mutate(type = "I") %>%
  bind_rows(non_index_rch %>%
              select(year, river,
                     location,
                     reach = non_index_reach,
                     redds) %>%
              mutate(type = "NI")) %>%
  bind_rows(no_err_rch %>%
              select(year,
                     river = major,
                     location,
                     reach = minor,
                     redds = total) %>%
              mutate(type = "NE")) %>%
  filter(year %in% unique(method_df$year)) %>%
  arrange(year, river, reach, type)


#-----------------------------------------------------------------
# redd observer error model covariates
#-----------------------------------------------------------------
# reach length
rch_lngth_org <- read_excel(data_file,
                            "Reach Length") %>%
  clean_names() %>%
  rename(reach = reach_2,
         reach_descp = reach_3) %>%
  pivot_longer(cols = c(index,
                        non_index),
               names_to = "type",
               values_to = "type_descp") %>%
  filter(!is.na(type_descp)) %>%
  relocate(length_km,
           .after = type_descp) %>%
  mutate(across(reach,
                as_factor),
         across(reach,
                fct_expand,
                "P3 (P2)"),
         across(reach,
                fct_relevel,
                "P3 (P2)",
                after = 13))

# make a few modifications
rch_lngth <- rch_lngth_org %>%
  filter(river != "Peshastin" |
           reach == "P1") %>%
  bind_rows(rch_lngth_org %>%
              filter(str_detect(reach, "P"),
                     reach != "P1",
                     str_detect(type_descp,
                                "No surveys",
                                negate = T)) %>%
              mutate(reach = if_else(type == "index",
                                     "P3 (P2)",
                                     as.character(reach)),
                     across(reach,
                            factor,
                            levels = levels(rch_lngth_org$reach)),
                     reach_descp = if_else(type == "index",
                                           "Ingalls Ck. To Scott Ck.",
                                           reach_descp)) %>%
              group_by(river,
                       reach,
                       reach_descp,
                       type) %>%
              summarize(across(type_descp,
                               paste,
                               collapse = ", "),
                        across(length_km,
                               sum),
                        .groups = "drop")) %>%
  arrange(river, reach)


# thalweg data
thlwg_df <- read_excel(data_file,
                       "Final Pooled CV Thalwegs",
                       range = cell_rows(c(3, 7))) %>%
  rename(metric = `...1`) %>%
  select(-2) %>%
  mutate(across(-1,
                as.character)) %>%
  pivot_longer(-1,
               names_to = "reach",
               values_to = "value") %>%
  mutate(across(metric,
                recode,
                "Distance Requirement Met (> 50% Reach Distance)" = "dist_req_met",
                "Pooled Non-overlapping  Redd Thalweg CVs" = "MeanThalwegCV",
                "Sample Size" = "n_samp",
                "Notes" = "notes")) %>%
  pivot_wider(names_from = metric,
              values_from = value) %>%
  mutate(across(c(MeanThalwegCV,
                  n_samp),
                as.numeric)) %>%
  mutate(across(reach,
                as_factor)) %>%
  left_join(method_df %>%
              select(river, reach) %>%
              distinct()) %>%
  relocate(river) %>%
  # for 2010, use the mean thalweg CV from N3 for N2 (since N2 is unavailable)
  mutate(across(MeanThalwegCV,
                ~ if_else(reach == "N2",
                          MeanThalwegCV[reach == "N3"],
                          .)),
         across(notes,
                ~ if_else(reach == "N2",
                          "Using thalweg CV from N3 because N2 was only sampled in one year for observer error",
                          .)))
thlwg_df %<>%
  mutate(across(reach,
                fct_expand,
                "P3 (P2)")) %>%
  bind_rows(thlwg_df %>%
              filter(reach %in% c("P3", "P4")) %>%
              group_by(river) %>%
              summarize(across(MeanThalwegCV,
                               mean),
                        across(n_samp,
                               sum),
                        across(notes,
                               ~ paste(.[!is.na(.)],
                                       collapse = ',')),
                        .groups = "drop") %>%
              mutate(reach = as.factor("P3 (P2)"),
                     dist_req_met = "N") %>%
              mutate(notes = paste(notes, "Average of P3 and P4."))) %>%
  arrange(river, reach)

# depth
depth_df <- read_excel(data_file,
                       "Water depth",
                       range = "H2:AN24") %>%
  select(reach = Reach,
         `2004...24`:ncol(.)) %>%
  rename_with(.fn  = function(x) str_sub(x, 1, 4),
              .cols = -1) %>%
  pivot_longer(-1,
               names_to = "year",
               values_to = "MeanDepth") %>%
  filter(!is.na(reach)) %>%
  mutate(across(year,
                as.numeric)) %>%
  mutate(reach = if_else(reach == "P3" & year %in% 2004:2010,
                         "P3 (P2)",
                         reach))

#----------------------------------------------------------------
# predict net error
#----------------------------------------------------------------
# pull out the redds for index reaches where method == "Redd"
# add covariates and apply one observer redd error model
index_redds <- index_rch |>
  filter(method == "Redd") %>%
  left_join(rch_lngth %>%
              filter(type == "index" |
                       reach == "N2") %>%
              select(river,
                     index_reach = reach,
                     length_km),
            by = c("river", "index_reach")) %>%
  mutate(NaiveDensity_km = visible_redds / length_km) %>%
  left_join(depth_df,
            by = c("year",
                   "index_reach" = "reach")) %>%
  left_join(thlwg_df %>%
              select(index_reach = reach,
                     MeanThalwegCV),
            by = "index_reach") %>%
  predict_neterr(num_obs = "one")
