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
  filter(err_est > 0) |>
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

#-----------------------------------------------------------------
# older results

old_redd_df <- read_rds(here("analysis/data/derived_data",
                             "old_redd_data_2014-2022.rds")) |>
  clean_names() |>
  select(spawn_year:net_error_se) |>
  mutate(across(net_error,
                ~ . - 1))

old_est <- old_redd_df |>
  nest(redd_data = -spawn_year) |>
  mutate(rch_est = map(redd_data,
                       .f = function(x) {
                         results_lst <- summarize_redds(x,
                                                        species = "Steelhead",
                                                        group_vars = c("river", "reach", "index", "survey_type"),
                                                        summ_vars = c("river", "location", "index"),
                                                        # min_non0_wks = min_non0_wks,
                                                        # min_redds = min_redds,
                                                        gauc = T,
                                                        add_zeros = T,
                                                        use_cor = T)

                         return(results_lst$rch_est)

                       }))


redd_df_old <- old_est |>
  # select(-spawn_year) |>
  select(-rch_est) |>
  unnest(redd_data)

rch_est_old <- old_est |>
  select(-redd_data) |>
  unnest(rch_est)

#-----------------------------------------------------------------
# center some covariates
covar_center = read_rds(here("analysis",
                             "data",
                             "derived_data",
                             "two_obs_covar_center.rds")) |>
  mutate(across(metric,
                recode,
                "ExpSpTotal" = "exp_sp_total",
                "ExpSpTotal_log" = "exp_sp_total_log",
                "MeanDischarge" = "mean_discharge",
                "MeanThalwegCV" = "mean_thalweg_cv",
                "NaiveDensity_km" = "naive_density_km"))

org_mod_df <- read_rds(here("analysis",
                            "data",
                            "derived_data",
                            "two_obs_model_data.rds")) |>
  clean_names() |>
  select(net_error,
         exp_sp_total,
         mean_discharge,
         mean_thalweg_cv,
         naive_density_km) |>
  mutate(id = 1:n()) |>
  pivot_longer(exp_sp_total:naive_density_km,
               names_to = "metric") |>
  left_join(covar_center) |>
  mutate(across(value,
                ~ . * stddev + mu)) |>
  select(-mu,
         -stddev) |>
  pivot_wider(names_from = metric) |>
  select(-id) |>
  mutate(across(net_error,
                ~ . - 1))

org_mod_df |>
  summary()

qplot(net_error,
      data = org_mod_df)

org_mod_df |>
  arrange(desc(net_error))

#-----------------------------------------------------------------

prob_rchs |>
  select(run_year:index,
         new_cnt = tot_feat,
         new_est = redd_est,
         new_err = err_est) |>
  left_join(rch_est_old |>
              select(spawn_year,
                     reach,
                     index,
                     old_cnt = tot_feat,
                     old_est = redd_est,
                     old_err = err_est))

est_comp <- rch_est_new |>
  select(run_year:index,
         new_cnt = tot_feat,
         new_est = redd_est,
         new_err = err_est) |>
  inner_join(rch_est_old |>
              select(spawn_year,
                     reach,
                     index,
                     old_cnt = tot_feat,
                     old_est = redd_est,
                     old_err = err_est)) |>
  mutate(diff_est = old_est - new_est,
         diff_err = old_err - new_err,
         rel_diff_est = diff_est / old_est) |>
  arrange(desc(abs(diff_est)))

est_comp |>
  filter(abs(diff_est) > 10) |>
  # filter(abs(diff_err) > 0.1,
  #        new_est != old_est)# |>
  # filter(new_err > 0,
  #        new_est != old_est) |>
  arrange(spawn_year, reach)

rch_est_old |>
  filter(redd_est < tot_feat) |>
  select(spawn_year,
         reach,
         index,
         old_cnt = tot_feat,
         old_est = redd_est,
         old_err = err_est)


# compare all covariates between old and new datasets
old_lengths <- redd_df_old |>
  filter(naive_density_km > 0) |>
  mutate(length_km = visible_redds / naive_density_km) |>
  mutate(across(length_km,
                ~ round(.x,
                        digits = 2))) |>
  select(spawn_year,
         reach,
         index,
         length_km) |>
  distinct()


all_comp <- redd_df_old |>
  left_join(old_lengths) |>
  filter(index == "Y") |>
  select(spawn_year:index,
         river,
         survey_date,
         new_redds,
         visible_redds,
         exp_sp_total,
         mean_thalweg_cv,
         length_km,
         naive_density_km,
         mean_discharge,
         net_error) |>
  filter(!is.na(net_error)) |>
  pivot_longer(cols = c(exp_sp_total,
                        mean_thalweg_cv,
                        length_km,
                        naive_density_km,
                        mean_discharge,
                        net_error),
               names_to = "covariates",
               values_to = "old") |>
  full_join(redd_df_new |>
              filter(index == "Y") |>
              select(spawn_year:index,
                     river,
                     survey_date,
                     new_redds,
                     visible_redds,
                     exp_sp_total,
                     mean_thalweg_cv,
                     length_km,
                     naive_density_km,
                     mean_discharge,
                     net_error) |>
              filter(!is.na(net_error)) |>
              pivot_longer(cols = c(exp_sp_total,
                                    mean_thalweg_cv,
                                    length_km,
                                    naive_density_km,
                                    mean_discharge,
                                    net_error),
                           names_to = "covariates",
                           values_to = "new") |>
              distinct()) |>
  filter(visible_redds > 0) |>
  left_join(covar_center,
            by = c("covariates" = "metric")) |>
  mutate(old_z = (old - mu) / stddev,
         new_z = (new - mu) / stddev) |>
  select(-mu, -stddev) |>
  mutate(diff = old - new,
         rel_diff = diff / old,
         diff_z = old_z - new_z)

prob_comp <- all_comp |>
  right_join(prob_rchs |>
               select(run_year:index))

prob_comp <- all_comp |>
  right_join(est_comp |>
               filter(abs(diff_est) >= 10) |>
               select(spawn_year,
                      reach,
                      index))


# prob_comp |>
all_comp |>
  filter(!is.na(diff_z)) |>
  mutate(across(covariates,
                ~ str_replace_all(., "_", " ")),
         across(covariates,
                str_to_title)) |>
  group_by(covariates) |>
  mutate(old_z_min = min(old_z),
         old_z_max = max(old_z)) |>
  ungroup() |>
  ggplot(aes(x = new_z,
             color = as.factor(spawn_year),
             fill = as.factor(spawn_year))) +
  geom_histogram(position = "dodge") +
  scale_fill_brewer(palette = "Set1",
                     name = "Spawn\nYear") +
  scale_color_brewer(palette = "Set1",
                    name = "Spawn\nYear") +
  geom_vline(aes(xintercept = old_z_min),
             linetype = 2) +
  geom_vline(aes(xintercept = old_z_max),
             linetype = 2) +
  facet_wrap(~ covariates,
             scales = "free") +
  labs(x = "Updated Z-Score")

all_comp |>
  select(spawn_year:new) |>
  pivot_longer(c(old, new),
               names_to = "data_set") |>
  filter(covariates == "net_error") |>
  mutate(covariates = paste(covariates, data_set, sep = "_")) |>
  select(-data_set) |>
  pivot_wider(names_from = "covariates",
              values_from = "value") |>
  left_join(all_comp |>
              filter(covariates != "net_error"),
            multiple = "all") |>
  mutate(across(covariates,
                ~ str_replace_all(., "_", " ")),
         across(covariates,
                str_to_title)) |>
  ggplot(aes(x = old,
             y = new,
             size = net_error_new,
             shape = reach,
             color = as.factor(spawn_year))) +
  geom_abline(linetype = 2,
              color = "black") +
  geom_point() +
  scale_color_brewer(palette = "Set1",
                     name = "Spawn\nYear",
                     guide = guide_legend(nrow = 3)) +
  scale_shape_discrete(solid = T,
                       name = "Reach",
                       guide = guide_legend(nrow = 3)) +
  scale_size(breaks = c(-1, -0.5, 0, 0.5, 1, 2, 3),
             name = "Net Error\n(Updated)",
             guide = guide_legend(nrow = 2)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal") +
  facet_wrap(~ covariates,
             scales = "free") +
  labs(x = "Old Value",
       y = "Updated Value")

ggsave(here("analysis",
            "figures",
            "covariate_comparison.pdf"),
       width = 8,
       height = 6)

#-----------------------------------------------------------------
# comparison put together by Mike Hughes
library(readxl)
redd_comp <- read_excel(here("analysis",
                             "data",
                             "raw_data",
                             "redd_est Comparisons.xlsx"),
                        range = "A8:I51") |>
  clean_names() |>
  select(-riber) |>
  rename(spawn_year = spawn,
         new_est = redd_est) |>
  mutate(index = recode(method,
                        "Index" = "Y",
                        "Non-Index" = "N")) |>
  full_join(read_excel(here("analysis",
                            "data",
                            "raw_data",
                            "redd_est Comparisons.xlsx"),
                       range = "K8:R51") |>
              clean_names() |>
              select(-location) |>
              rename(spawn_year = year,
                     old_est = redd_est)) |>
  relocate(index,
           .after = "reach") |>
  select(-method) |>
  mutate(diff = old_est - new_est,
         rel_diff = diff / old_est) |>
  arrange(desc(abs(diff)))

redd_comp |>
  filter(abs(diff) >= 10) |>
  select(spawn_year, reach,
         index,
         obs_redds,
         old_est2 = old_est) |>
  left_join(est_comp |>
              select(-c(run_year,
                        population:river))) |>
  arrange(spawn_year,
          reach) |>
  select(spawn_year,
         reach,
         index,
         obs_redds,
         contains("est"),
         contains("err"))


redd_comp |>
  filter(abs(diff) >= 10) |>
  select(spawn_year, reach,
         index) |>
  left_join(all_comp,
            multiple = "all") |>
  filter(covariates != "net_error") |>
  group_by(spawn_year, reach, index) |>
  filter(abs(rel_diff) == max(abs(rel_diff))) |>
  tabyl(covariates)
