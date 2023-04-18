# Author: Kevin See
# Purpose: compare older results and newer results (using sroem package)
# Created: 3/6/23
# Last Modified: 3/6/23
# Notes: Focused on Wenatchee

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(janitor)
library(here)
library(lubridate)
library(sroem)

#-----------------------------------------------------------------
# newer results

new_est <- crossing(population = c("Wenatchee"),
                    spawn_year = c(2014:2022)) |>
  mutate(run_year = spawn_year - 1) %>%
  select(run_year, spawn_year, population) |>
  filter(spawn_year != 2020) |>
  mutate(results_list = map2(spawn_year,
                             population,
                             .f = possibly(function(yr, pop) {

                               message(paste("Prepping data from", yr, "\n\n"))


                               # load data for selected years
                               redd_df_all <- query_redd_data(query_year = yr)

                               if(!is.null(redd_df_all)) {
                                 # divide reaches into various location categories
                                 redd_df_all <- redd_df_all |>
                                   dplyr::mutate(location = dplyr::if_else(reach %in% paste0("W", 8:10),
                                                                           "Above Tumwater",
                                                                           dplyr::if_else(reach %in% paste0("W", 1:7),
                                                                                          "Below Tumwater",
                                                                                          "Tributaries")))

                                 # predict net error
                                 redd_df_all <- redd_df_all |>
                                   sroem::predict_neterr(species = "Steelhead",
                                                         num_obs = "two")
                               }


                               results_lst <- summarize_redds(redd_df_all,
                                                              species = "Steelhead",
                                                              group_vars = c("river", "reach", "index", "survey_type"),
                                                              summ_vars = c("river", "location", "index"),
                                                              # min_non0_wks = min_non0_wks,
                                                              # min_redds = min_redds,
                                                              gauc = T,
                                                              add_zeros = T,
                                                              use_cor = T)

                               res <- list(redd_data = redd_df_all,
                                           rch_est = results_lst$rch_est)

                               return(res)
                             },
                             otherwise = NULL)))

redd_df_new <- new_est |>
  select(-spawn_year) |>
  mutate(redd_data = map(results_list,
                         function(x) x[["redd_data"]])) |>
  select(-results_list) |>
  unnest(redd_data)

rch_est_new <- new_est |>
  mutate(redd_est = map(results_list,
                        function(x) x[["rch_est"]])) |>
  select(-results_list) |>
  unnest(redd_est)

prob_rchs <- rch_est_new |>
  # filter(err_est > 0) |>
  filter(redd_est < tot_feat) |>
  select(-population,
         -river,
         -gauc_list,
         -data) |>
  relocate(redd_est,
           redd_se,
           .after = "tot_feat")

prob_rchs |>
  select(run_year:index) |>
  left_join(redd_df_new,
            multiple = "all") |>
  select(spawn_year:index,
         river,
         survey_date:visible_redds,
         exp_sp_total,
         mean_thalweg_cv,
         length_km,
         naive_density_km,
         mean_discharge,
         net_error)

redd_df_new |>
  filter(is.na(exceed_ceiling))

redd_df_new |>
  group_by(spawn_year,
           reach,
           index) |>
  summarize(across(net_error,
                   mean),
            across(exceed_ceiling,
                   sum),
            .groups = "drop") |>
  tabyl(spawn_year, exceed_ceiling)

#-----------------------------------------------------------------
# older results

old_est <- new_est |>
  select(run_year,
         spawn_year,
         population) |>
  mutate(results_list = map(spawn_year,
                            .f = possibly(function(yr) {

                              message(paste("Prepping data from", yr, "\n\n"))


                              # load data for selected years
                              load(here("analysis/data/derived_data",
                                        paste0("wen_", yr, ".rda")))

                              redd_df <- redd_df |>
                                clean_names()


                              results_lst <- summarize_redds(redd_df,
                                                             species = "Steelhead",
                                                             group_vars = c("river", "reach", "index", "survey_type"),
                                                             summ_vars = c("river", "location", "index"),
                                                             # min_non0_wks = min_non0_wks,
                                                             # min_redds = min_redds,
                                                             gauc = T,
                                                             add_zeros = T,
                                                             use_cor = T)

                              res <- list(redd_data = redd_df,
                                          rch_est = results_lst$rch_est)

                              return(res)
                            },
                            otherwise = NULL)))

redd_df_old <- old_est |>
  select(-spawn_year) |>
  mutate(redd_data = map(results_list,
                         function(x) x[["redd_data"]])) |>
  select(-results_list) |>
  unnest(redd_data)

rch_est_old <- old_est |>
  mutate(redd_est = map(results_list,
                        function(x) x[["rch_est"]])) |>
  select(-results_list) |>
  unnest(redd_est)

rch_est_old |>
  filter(redd_est < tot_feat) |>
  select(-population,
         -river,
         -gauc_list,
         -data) |>
  relocate(redd_est,
           redd_se,
           .after = "tot_feat")
