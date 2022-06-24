# Author: Kevin See
# Purpose: Prep redd data from 2004-2013
# Created: 6/9/22
# Last Modified: 6/10/22
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
library(here)

#-----------------------------------------------------------------
data_file = here("analysis/data/raw_data",
                 "Final_Historical covariates Wenatchee Steelhead 5-23-22.xlsx")
excel_sheets(data_file)

# what method should be used for each reach / year?
method_df <- read_excel(data_file,
                        1) %>%
  filter(!(Reach == "W9" & `2012` == "Redd")) %>%
  pivot_longer(-c(1:2),
               names_to = "year",
               values_to = "method") %>%
  mutate(across(year,
                as.numeric)) %>%
  clean_names() %>%
  mutate(across(reach,
                as_factor)) %>%
  distinct()

tabyl(method_df,
      method,
      year)

# reach length
rch_lngth <- read_excel(data_file,
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
                as_factor))


# data on redd life
redd_life_df <- read_excel(data_file,
                           "Redd life") %>%
  clean_names() %>%
  left_join(method_df %>%
              select(river,
                     reach) %>%
              distinct()) %>%
  mutate(across(river,
                ~ if_else(reach == "I2",
                          "Icicle",
                          .))) %>%
  relocate(river) %>%
  mutate(across(reach,
                as_factor),
         across(reach,
                fct_relevel,
                "W10",
                after = Inf))

redd_life_df %>%
  ggplot(aes(x = redd_life,
             color = river,
             fill = river)) +
  geom_density(alpha = 0.2)

redd_life_df %>%
  ggplot(aes(x = river,
             y = redd_life,
             fill = river)) +
  geom_boxplot()

redd_life_df %>%
  filter(is.na(river))

redd_life_df %>%
  filter(river == "Wenatchee") %>%
  mutate(rch_grp = fct_collapse(reach,
                                W10 = "W10",
                                "W6-W9" = paste0("W", 6:9),
                                "W1-W5" = paste0("W", 1:5))) %>%
  ggplot(aes(x = reach,
             y = redd_life,
             fill = rch_grp)) +
  geom_boxplot()

# non-index reaches
non_index_rch <- read_excel(data_file,
                            "Non-index Reaches") %>%
  clean_names() %>%
  rename(river = stream) %>%
  select(-x6) %>%
  arrange(year,
          river,
          non_index_reach,
          date)

# index reaches
index_rch <- read_excel(data_file,
                        "Index Reaches") %>%
  clean_names() %>%
  rename(river = stream) %>%
  arrange(year,
          river,
          index_reach,
          date)

# no-error reaches
no_err_rch <- read_excel(data_file,
                         "No Error Tribs") %>%
  clean_names()

# put a couple of index reaches into the no-error reaches
no_err_rch %<>%
  bind_rows(index_rch %>%
              filter(index_reach %in% c("H3",
                                        "L3")) %>%
              group_by(year,
                       river,
                       index_reach) %>%
              summarize(total = sum(redds),
                        .groups = "drop") %>%
              rename(major = river,
                     minor = index_reach)) %>%
  arrange(year,
          major,
          minor)

index_rch %<>%
  filter(!index_reach %in% c("H3",
                            "L3"))

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
                as.numeric))


#-----------------------------------------------------------------
# summary of average redd life
redd_life_summ <- redd_life_df %>%
  filter(!is.na(river)) %>%
  mutate(rch_grp = river,
         rch_grp = if_else(reach == "W10",
                           "W10",
                           if_else(reach %in% paste0("W", 6:9),
                                   "W6-W9",
                                   if_else(reach %in% paste0("W", 1:5),
                                           "W1-W5",
                                           rch_grp))),
         across(rch_grp,
                as.factor),
         across(rch_grp,
                fct_relevel,
                "W10",
                after = Inf)) %>%
  mutate(across(rch_grp,
                fct_collapse,
                "W6-W10" = c("W6-W9",
                             "W10"))) %>%
  group_by(rch_grp) %>%
  summarize(across(redd_life,
                   list(mean = mean,
                        sd = sd)))

# these are the reaches we need to apply a redd-life estimate to
# to estimate visible redds
redd_life_needed <- index_rch %>%
  mutate(across(index_reach,
                recode,
                "P3 (P2)" = "P3")) %>%
  left_join(method_df %>%
              rename(index_reach = reach)) %>%
  filter(year < 2009,
         !is.na(redds),
         is.na(visible_redds))

redd_life_needed %<>%
  mutate(rch_grp = river,
         rch_grp = if_else(index_reach == "W10",
                           "W10",
                           if_else(index_reach %in% paste0("W", 6:9),
                                   "W6-W9",
                                   if_else(index_reach %in% paste0("W", 1:5),
                                           "W1-W5",
                                           rch_grp))),
         across(rch_grp,
                as.factor),
         across(rch_grp,
                fct_relevel,
                "W10",
                after = Inf)) %>%
  mutate(across(rch_grp,
                fct_collapse,
                "W6-W10" = c("W6-W9",
                             "W10"))) %>%
  left_join(redd_life_summ) %>%
  mutate(last_visible = date + days(round(redd_life_mean)))

# fill in what redds were visible during each survey
redd_life_needed %<>%
  group_by(year,
           river,
           index_reach) %>%
  nest() %>%
  ungroup() %>%
  mutate(vis_data = map(data,
                        .f = function(x) {
                          vis_redds <- x %>%
                            select(date, redds, last_visible) %>%
                            rename(survey_date = date) %>%
                            pivot_longer(c(survey_date,
                                           last_visible),
                                         names_to = "type",
                                         values_to = "date") %>%
                            mutate(across(redds,
                                          ~if_else(type == "last_visible",
                                                   . * -1,
                                                   .))) %>%
                            arrange(date) %>%
                            mutate(visible_redds = cumsum(redds)) %>%
                            filter(type == "survey_date") %>%
                            pull(visible_redds)

                          x %>%
                            mutate(visible_redds = vis_redds) %>%
                            return()
                        })) %>%
  unnest(vis_data) %>%
  select(-data,
         -rch_grp,
         -starts_with("redd_life"),
         -last_visible)

redd_life_needed

index_rch %>%
  mutate(across(index_reach,
                recode,
                "P3 (P2)" = "P3")) %>%
  left_join(method_df %>%
              rename(index_reach = reach)) %>%
  anti_join(redd_life_needed %>%
              select(year:date)) %>%
  filter(!is.na(redds)) %>%
  bind_rows(redd_life_needed) %>%
  arrange(year,
          river,
          index_reach,
          date) -> index_rch


#-----------------------------------------------------------------
census_surv <- index_rch |>
  filter(method == "Truth")


index_redds <- index_rch |>
  filter(method == "Redd") %>%
  left_join(rch_lngth %>%
              filter(type == "index" |
                       reach == "N2") %>%
              select(river,
                     index_reach = reach,
                     length_km)) %>%
  mutate(NaiveDensity_km = visible_redds / length_km) %>%
  left_join(depth_df,
            by = c("year",
                   "index_reach" = "reach")) %>%
  left_join(thlwg_df %>%
              select(index_reach = reach,
                     MeanThalwegCV)) %>%
  predict_neterr(num_obs = "one")

index_redds %>%
  ggplot(aes(x = river,
             y = NetError,
             fill = river)) +
  geom_boxplot() +
  geom_hline(yintercept = 1,
             linetype = 3) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(fill = "River",
       y = "Net Error")


index_redds %>%
  select(year:index_reach,
         NaiveDensity_km:MeanThalwegCV) %>%
  pivot_longer(NaiveDensity_km:MeanThalwegCV,
               names_to = "metric",
               values_to = "value") %>%
  distinct() %>%
  left_join(one_obs_covar_center) %>%
  mutate(z_value = (value - mu) / stddev) %>%
  ggplot(aes(x = river,
             y = z_value,
             fill = river)) +
  geom_boxplot() +
  geom_hline(yintercept = 0,
             linetype = 2) +
  facet_wrap(~ metric) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(fill = "River")


index_redds %>%
  group_by(year,
           river,
           index_reach) %>%
  nest() %>%
  mutate(mod_data = map(data,
                        .f = function(x) {
                          x %>%
                            mutate(day = 1:n()) %>%
                            select(day,
                                   redds)
                        }),
         n_surv = map_dbl(mod_data,
                          nrow),
         tot_obs_redds = map_dbl(mod_data,
                                 .f = function(x) sum(x$redds)),
         n_non_zero = map_dbl(mod_data,
                          .f = function(x) sum(x$redds > 0)),
         net_err = map(data,
                       .f = function(x) {
                         c(v = mean(x$NetError),
                           v_se = mean(x$NetErrorSE))
                       })) %>%
  filter(tot_obs_redds > 2,
         n_non_zero >= 3) %>%
  mutate(gauc_fit = )

#-----------------------------------------------------------------
