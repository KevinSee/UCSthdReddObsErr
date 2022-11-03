# Author: Kevin See
# Purpose: Pull together relevant data for UC HETT discussion
# Created: 10/31/22
# Last Modified: 11/3/2022
# Notes: focus on 2014 - 2021 since that's when the two-observer redd surveys were conducted

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
library(ggrepel)
library(ggpubr)

theme_set(theme_bw())


#-----------------------------------------------------------------
# where to save figures
save_loc <- "O:/Documents/Presentations/2022_HETT/figures/"

#-----------------------------------------------------------------
# all redd-based methods
# skip 2020 because we did it differently that year
all_wen_df = tibble(year = c(2014:2019, 2021),
                    redd_data = NA,
                    redds_below_arrays = NA)
for(yr in all_wen_df$year) {
  # load data for this year
  load(here('analysis/data/derived_data',
            paste0('wen_', yr, '.rda')))

  all_wen_df$redd_data[all_wen_df$year == yr] = list(redd_df %>%
                                                       select(-contains("NetError")) %>%
                                                       predict_neterr(num_obs = 'two') %>%
                                                       clean_names())
  all_wen_df$redds_below_arrays[all_wen_df$year == yr] = list(redds_below_arrays %>%
                                                                clean_names() %>%
                                                                mutate(river = if_else(str_detect(reach, "^C"),
                                                                                       "Chiwawa",
                                                                                       if_else(str_detect(reach, "^N"),
                                                                                               "Nason",
                                                                                               if_else(str_detect(reach, "^P"),
                                                                                                       "Peshastin",
                                                                                                       NA_character_)))))

  # wen_sex_org <- wen_sex_tab %>%
  #   mutate(fpr = (m_prop / f_prop) + 1,
  #          fpr_se = deltamethod(~ (x1 / x2) + 1,
  #                               mean = c(m_prop, f_prop),
  #                               cov = diag(prop_se^2, nrow = 2))) %>%
  #   select(Location, contains("fpr")) %>%
  #   full_join(wen_origin_tab %>%
  #               select(Location, contains("prop")),
  #             by = "Location") %>%
  #   rename(location = Location) %>%
  #   mutate(across(location,
  #                 recode,
  #                 "TUM_bb" = "Above_TUM"))
  #
  # all_wen_df$wen_sex_org[all_wen_df$year == yr] = list(wen_sex_org)
}


# set some thresholds
# minimum number of total redds observed
min_redds = 2
# minimum number of weeks with at least one new redd observed
min_non0_wks = 3
# which estimates to switch?
# set the threshold for net error
# err_thres = min(one_obs_model_data$NetError)
err_thres = 0.2

all_wen_df %<>%
  mutate(index_summ = map(redd_data,
                          .f = function(x) {
                            x %>%
                              group_by(river,
                                       reach,
                                       index,
                                       survey_type) %>%
                              nest() %>%
                              mutate(mod_data = map(data,
                                                    .f = function(x) {
                                                      mod_df <- x %>%
                                                        mutate(day = 1:n()) %>%
                                                        select(day,
                                                               redds = new_redds)
                                                      # add a final 0 to the surveys if necessary
                                                      if(mod_df$redds[nrow(mod_df)] > 0) {
                                                        mod_df %<>%
                                                          bind_rows(tibble(day = max(mod_df$day) + 1,
                                                                           redds = 0))
                                                      }
                                                      return(mod_df)
                                                    }),
                                     n_surv = map_dbl(mod_data,
                                                      nrow),
                                     tot_obs_redds = map_dbl(mod_data,
                                                             .f = function(x) sum(x$redds)),
                                     n_non_zero = map_dbl(mod_data,
                                                          .f = function(x) sum(x$redds > 0)),
                                     net_err = map(data,
                                                   .f = function(x) {
                                                     x %>%
                                                       filter(visible_redds > 0) %>%
                                                       rename(NetError = net_error,
                                                              NetErrorSE = net_error_se) %>%
                                                       summarise(across(c(NetError, NetErrorSE),
                                                                        mean,
                                                                        na.rm = T)) %>%
                                                       mutate(NetErrorSE = if_else(is.na(NetError),
                                                                                   0,
                                                                                   NetErrorSE),
                                                              NetError = if_else(is.na(NetError),
                                                                                 1,
                                                                                 NetError)) %>%
                                                       rename(err_est = NetError,
                                                              err_se = NetErrorSE)
                                                   })) %>%
                              ungroup() %>%
                              unnest(net_err)
                          })) %>%
  mutate(ne_switch = map(index_summ,
                         .f = function(index_summ) {
                           if(sum(index_summ$err_est < err_thres) == 0) {
                             return(NULL)
                           }
                           index_summ %>%
                             filter(err_est < err_thres) %>%
                             select(river,
                                    old_reach = reach) %>%
                             left_join(expand(index_summ,
                                              nesting(year, river,
                                                      new_reach = reach,
                                                      err_est)),
                                       by = c("river")) %>%
                             filter(old_reach != new_reach,
                                    err_est > err_thres) %>%
                             mutate(old_num = str_extract(old_reach,
                                                          "[:digit:]+"),
                                    new_num = str_extract(new_reach,
                                                          "[:digit:]+"),
                                    across(ends_with("_num"),
                                           as.numeric),
                                    upstrm = if_else(new_num > old_num | old_reach == "W10",
                                                     T, F),
                                    diff = abs(old_num - new_num)) %>%
                             group_by(river, old_reach) %>%
                             filter(diff == min(diff)) %>%
                             # filter(diff == min(diff[upstrm])) %>%
                             ungroup() %>%
                             full_join(index_summ %>%
                                         filter(err_est < err_thres) %>%
                                         select(river,
                                                old_reach = reach),
                                       by = c("river", "old_reach")) %>%
                             select(river:new_reach) %>%
                             left_join(index_summ %>%
                                         select(river,
                                                new_reach = reach,
                                                new_err = err_est,
                                                new_se = err_se))
                         })) %>%
  mutate(index_summ = map2(index_summ,
                           ne_switch,
                           .f = function(x, y) {
                             if(is.null(y)) {
                               return(x)
                             } else {
                               # assign the new reach net error estimates where necessary
                               x %>%
                                 left_join(y,
                                           by = c("year",
                                                  "river",
                                                  "index_reach" = "old_reach")) %>%
                                 mutate(err_est = if_else(!is.na(new_reach),
                                                          new_err,
                                                          err_est),
                                        err_se = if_else(!is.na(new_reach),
                                                         new_se,
                                                         err_se)) %>%
                                 select(-starts_with("new")) %>%
                                 return()
                             }
                           })) %>%
  select(-ne_switch) %>%
  mutate(redd_results = map(index_summ,
                            .f = function(x) {
                              x %>%
                                nest(net_err = c(err_est, err_se)) %>%
                                mutate(gauc_list = map2(mod_data,
                                                        net_err,
                                                        .f = function(mod_df,
                                                                      v_vec) {
                                                          # # if you want to assume no observer error
                                                          # v_vec = tibble(NetError = 1,
                                                          #                NetErrorSE = 0)

                                                          res_list = fit_gauc(data = mod_df,
                                                                              v = v_vec$err_est,
                                                                              v_se = v_vec$err_se)
                                                          return(res_list)
                                                        })) %>%
                                unnest(net_err) %>%
                                mutate(converged = map_lgl(gauc_list,
                                                           .f = function(x) {
                                                             x$model$converged
                                                           }),
                                       # is the curve bending the right way?
                                       good_curve = map_lgl(gauc_list,
                                                            .f = function(x) {
                                                              coef(x$model)[3] < 0
                                                            })) %>%
                                mutate(redd_est = map_dbl(gauc_list,
                                                          .f = 'E'),
                                       redd_se = map_dbl(gauc_list,
                                                         .f = 'E_se')) %>%
                                mutate(GAUC = if_else(!converged |
                                                        n_non_zero < min_non0_wks |
                                                        tot_obs_redds < min_redds |
                                                        !good_curve,
                                                      F, T)) %>%
                                rowwise() %>%
                                mutate(redd_est = if_else(!GAUC,
                                                          tot_obs_redds / err_est,
                                                          redd_est),
                                       redd_se = if_else(!GAUC,
                                                         msm::deltamethod(~ x1 / x2,
                                                                          mean = c(tot_obs_redds, err_est),
                                                                          cov = diag(c(0, err_se)^2)),
                                                         redd_se)) %>%
                                ungroup() %>%
                                mutate_at(vars(redd_est),
                                          list(round_half_up))
                            })) %>%
  mutate(redd_results = map2(redd_results,
                             redds_below_arrays,
                             .f = function(x, y) {
                               x %>%
                                 bind_rows(y %>%
                                             mutate(index = "N",
                                                    tot_obs_redds = redd_est,
                                                    err_est = 1,
                                                    err_se = 0,
                                                    GAUC = F)) %>%
                                 mutate(location = if_else(!grepl('W[[:digit:]]', reach),
                                                           river,
                                                           if_else(reach %in% paste0('W', 1:7),
                                                                   'Below_TUM',
                                                                   'Above_TUM'))) %>%
                                 return()
                             })) %>%
mutate(river_est = map(redd_results,
                           .f = function(x) {
                             redd_tot <- x %>%
                               group_by(river,
                                        # location,
                                        index) %>%
                               summarize(across(err_est,
                                                mean),
                                         across(c(tot_obs_redds,
                                                  redd_est),
                                                sum),
                                         across(redd_se,
                                                ~ sqrt(sum(.^2))),
                                         .groups = "drop") %>%
                               rename(redd_se_naive = redd_se)


                             z = x %>%
                               select(river,
                                      index,
                                      err_est,
                                      contains("redd")) %>%
                               group_by(river,
                                        index) %>%
                               nest(redd_df = c(err_est, contains("redd")))

                             x %>%
                               select(river,
                                      reach,
                                      index,
                                      data) %>%
                               unnest(data) %>%
                               select(river,
                                      reach,
                                      index,
                                      survey_date,
                                      new_redds) %>%
                               group_by(river,
                                        index) %>%
                               nest() %>%
                               ungroup() %>%
                               full_join(z,
                                         by = c("river", "index")) %>%
                               mutate(cor_mat = map(data,
                                                    .f = function(y) {
                                                      if(is.null(y)) {
                                                        return(NULL)
                                                      }
                                                      cor_mat = try(y %>%
                                                                      group_by(reach) %>%
                                                                      mutate(survey = 1:n()) %>%
                                                                      ungroup() %>%
                                                                      select(reach, survey, new_redds) %>%
                                                                      pivot_wider(names_from = reach,
                                                                                  values_from = new_redds) %>%
                                                                      select(-survey) %>%
                                                                      cor(use = "pairwise",
                                                                          # method = 'kendall')
                                                                          method = 'spearman'),
                                                                    silent = T)

                                                      if(class(cor_mat)[1] == 'try-error') {
                                                        return(diag(1, nrow = 1))
                                                      } else {
                                                        cor_mat[is.na(cor_mat)] = 0
                                                        diag(cor_mat) = 1
                                                        return(cor_mat)
                                                      }
                                                    })) %>%
                               mutate(redd_estimate = map2(redd_df,
                                                           cor_mat,
                                                           .f = function(x, y) {
                                                             tot_df <- x %>%
                                                               summarize(across(err_est,
                                                                                mean),
                                                                         across(c(tot_obs_redds,
                                                                                  redd_est),
                                                                                sum))

                                                             se = try(deltamethod(as.formula(paste('~', paste0('x', 1:nrow(x), collapse='+'))),
                                                                                  mean = x$redd_est,
                                                                                  cov = diag(x = x$redd_se,
                                                                                             nrow = nrow(y)) %*% y %*% diag(x = x$redd_se,
                                                                                                                            nrow = nrow(y))),
                                                                      silent = T)

                                                             if(class(se)[1] == "try-error") {
                                                               se = sqrt(sum(x$redd_se^2))
                                                             }
                                                             tot_df %<>%
                                                               mutate(redd_se = se)
                                                             return(tot_df)
                                                           })) %>%
                               select(-data,
                                      -redd_df,
                                      -cor_mat) %>%
                               unnest(redd_estimate) %>%
                               full_join(redd_tot,
                                         by = c("river", "index", "err_est", "tot_obs_redds", "redd_est")) %>%
                               rename(obs_redds = tot_obs_redds) %>%
                               return()
                           })) %>%
mutate(loc_est = map(redd_results,
                     .f = function(x) {
                       redd_tot <- x %>%
                         group_by(location,
                                  index) %>%
                         summarize(across(err_est,
                                          mean),
                                   across(c(tot_obs_redds,
                                            redd_est),
                                          sum),
                                   across(redd_se,
                                          ~ sqrt(sum(.^2))),
                                   .groups = "drop") %>%
                         rename(redd_se_naive = redd_se)


                       z = x %>%
                         select(location,
                                index,
                                err_est,
                                contains("redd")) %>%
                         group_by(location,
                                  index) %>%
                         nest(redd_df = c(err_est, contains("redd")))

                       x %>%
                         select(location,
                                reach,
                                index,
                                data) %>%
                         unnest(data) %>%
                         select(location,
                                reach,
                                index,
                                survey_date,
                                new_redds) %>%
                         group_by(location,
                                  index) %>%
                         nest() %>%
                         ungroup() %>%
                         full_join(z,
                                   by = c("location", "index")) %>%
                         mutate(cor_mat = map(data,
                                              .f = function(y) {
                                                if(is.null(y)) {
                                                  return(NULL)
                                                }
                                                cor_mat = try(y %>%
                                                                group_by(reach) %>%
                                                                mutate(survey = 1:n()) %>%
                                                                ungroup() %>%
                                                                select(reach, survey, new_redds) %>%
                                                                pivot_wider(names_from = reach,
                                                                            values_from = new_redds) %>%
                                                                select(-survey) %>%
                                                                cor(use = "pairwise",
                                                                    # method = 'kendall')
                                                                    method = 'spearman'),
                                                              silent = T)

                                                if(class(cor_mat)[1] == 'try-error') {
                                                  return(diag(1, nrow = 1))
                                                } else {
                                                  cor_mat[is.na(cor_mat)] = 0
                                                  diag(cor_mat) = 1
                                                  return(cor_mat)
                                                }
                                              })) %>%
                         mutate(redd_estimate = map2(redd_df,
                                                     cor_mat,
                                                     .f = function(x, y) {
                                                       tot_df <- x %>%
                                                         summarize(across(err_est,
                                                                          mean),
                                                                   across(c(tot_obs_redds,
                                                                            redd_est),
                                                                          sum))

                                                       se = try(deltamethod(as.formula(paste('~', paste0('x', 1:nrow(x), collapse='+'))),
                                                                            mean = x$redd_est,
                                                                            cov = diag(x = x$redd_se,
                                                                                       nrow = nrow(y)) %*% y %*% diag(x = x$redd_se,
                                                                                                                      nrow = nrow(y))),
                                                                silent = T)

                                                       if(class(se)[1] == "try-error") {
                                                         se = sqrt(sum(x$redd_se^2))
                                                       }
                                                       tot_df %<>%
                                                         mutate(redd_se = se)
                                                       return(tot_df)
                                                     })) %>%
                         select(-data,
                                -redd_df,
                                -cor_mat) %>%
                         unnest(redd_estimate) %>%
                         full_join(redd_tot,
                                   by = c("location", "index", "err_est", "tot_obs_redds", "redd_est")) %>%
                         rename(obs_redds = tot_obs_redds) %>%
                         return()
                     }))

#-------------------------------------------------------------------
# get all PIT tag data/estimates
pit_list <- tibble(year = 2011:2021) %>%
  # what dam count to use?
  mutate(dam_cnt_name = if_else(year %in% c(2016:2017, 2019:2021),
                                "RockIsland",
                                "PriestRapids")) %>%
  mutate(pit_info = map2(year,
                         dam_cnt_name,
                         .f = function(yr,
                                       dam_cnt_name) {
                           load(paste0('O:Documents/Git/MyProjects/DabomPriestRapidsSthd/analysis/data/derived_data/estimates/', dam_cnt_name,
                                       "/PRA_DABOM_Steelhead_", yr, ".rda"))

                           wen_tags = save_list$`Tag Summary` %>%
                             filter(str_detect(path, "LWE")) %>%
                             mutate(Location = if_else(spawn_node %in% c('TUM', 'UWE'),
                                                       'Above_TUM',
                                                       if_else(str_detect(path, 'TUM'),
                                                               'Tribs_above_TUM',
                                                               if_else(str_detect(spawn_node, "LWE"),
                                                                       "Below_TUM",
                                                                       "Tribs_below_TUM")))) %>%
                             mutate(Location = factor(Location,
                                                      levels = c("Below_TUM",
                                                                 "Tribs_below_TUM",
                                                                 'Above_TUM',
                                                                 'Tribs_above_TUM'))) %>%
                             mutate(trib = if_else(str_detect(path, " CHL"),
                                                   "Chiwawa",
                                                   if_else(str_detect(path, "CHM"),
                                                           "Chumstick",
                                                           if_else(str_detect(path, 'CHW'),
                                                                   'Chiwaukum',
                                                                   if_else(str_detect(path, 'ICL'),
                                                                           'Icicle',
                                                                           if_else(str_detect(path, 'LWN'),
                                                                                   'Little Wenatchee',
                                                                                   if_else(str_detect(path, 'MCL'),
                                                                                           'Mission',
                                                                                           if_else(str_detect(path, 'NAL'),
                                                                                                   'Nason',
                                                                                                   if_else(str_detect(path, 'PES'),
                                                                                                           'Peshastin',
                                                                                                           if_else(str_detect(path, 'WTL'),
                                                                                                                   'White River',
                                                                                                                   NA_character_)))))))))) %>%
                             select(TagID = tag_code,
                                    path,
                                    trib,
                                    Location,
                                    spawn_node,
                                    Origin = origin,
                                    Sex = sex,
                                    Age = age)

                           load(paste0("O:Documents/Git/MyProjects/DabomPriestRapidsSthd/analysis/data/derived_data/model_fits/",
                                     'PRA_DABOM_Steelhead_', yr,'.rda'))

                           wen_ch = filter_obs %>%
                             filter(tag_code %in% wen_tags$TagID)

                           list(escp = escape_summ,
                                tags = wen_tags,
                                cap_hist = wen_ch) %>%
                             return()
                         }))

pit_est = pit_list %>%
  mutate(est = map(pit_info,
                   "escp")) %>%
  select(-pit_info) %>%
  unnest(est) %>%
  select(-species,
         -spawn_year)


pit_list %>%
  mutate(ch = map(pit_info,
                   "cap_hist")) %>%
  select(-pit_info) %>%
  unnest(ch) %>%
  filter(year >= 2014) %>%
  filter(str_detect(node, "^LWE")) %>%
  mutate(wen_enter = month(min_det, label = T)) %>%
  tabyl(year, wen_enter) %>%
  adorn_percentages() %>%
  adorn_pct_formatting()

# generate fish/redd and pHOS based on PIT tags
pit_sex_org = pit_list %>%
  mutate(tags = map(pit_info,
                    "tags")) %>%
  select(-pit_info) %>%
  unnest(tags) %>%
  mutate(location = if_else(Location %in% c("Above_TUM", "Below_TUM"),
                            as.character(Location),
                            trib)) %>%
  group_by(year, location) %>%
  summarize(n_tags = n(),
            n_f = sum(Sex == "F"),
            n_m = sum(Sex == "M"),
            n_nor = sum(Origin == "W"),
            n_hor = sum(Origin == "H"),
            .groups = "drop") %>%
  rowwise() %>%
  mutate(f_prop = n_f / (n_f + n_m),
         f_prop_se = sqrt((f_prop * (1 - f_prop)) / (n_f + n_m)),
         fpr = (1 - f_prop) / f_prop + 1,
         # fpr_se = deltamethod(~ (1 - x1) / x1 + 1,
         #                      mean = f_prop,
         #                      cov = diag(f_prop_se^2,
         #                                 nrow = 1,
         #                                 ncol = 1,
         #                                 names = F)),
         fpr_se = 1 / (f_prop^2) * f_prop_se,
         phos = n_hor / (n_hor + n_nor),
         phos_se = sqrt((phos * (1 - phos)) / (n_hor + n_nor))) %>%
  select(year, location,
         n_tags,
         starts_with("fpr"),
         starts_with("phos"))

# averaged across entire mainstem areas
pit_sex_org2 = pit_list %>%
  mutate(tags = map(pit_info,
                    "tags")) %>%
  select(-pit_info) %>%
  unnest(tags) %>%
  mutate(location = if_else(Location %in% c("Above_TUM", "Below_TUM"),
                            as.character(Location),
                            trib)) %>%
  group_by(year) %>%
  summarize(n_tags = n(),
            n_f = sum(Sex == "F"),
            n_m = sum(Sex == "M"),
            n_nor = sum(Origin == "W"),
            n_hor = sum(Origin == "H"),
            .groups = "drop") %>%
  rowwise() %>%
  mutate(f_prop = n_f / (n_f + n_m),
         f_prop_se = sqrt((f_prop * (1 - f_prop)) / (n_f + n_m)),
         fpr = (1 - f_prop) / f_prop + 1,
         fpr_se = 1 / (f_prop^2) * f_prop_se,
         phos = n_hor / (n_hor + n_nor),
         phos_se = sqrt((phos * (1 - phos)) / (n_hor + n_nor))) %>%
  select(year,
         n_tags,
         starts_with("fpr"),
         starts_with("phos"))


pit_sex_org_age <- pit_list %>%
  mutate(tags = map(pit_info,
                    "tags")) %>%
  select(-pit_info) %>%
  unnest(tags) %>%
  group_by(year) %>%
  count(Origin, Sex, Age) %>%
  filter(!is.na(Age)) %>%
  mutate(fw_chr = str_split(Age, "\\.", simplify = T)[,1],
         oc_chr = str_split(Age, "\\.", simplify = T)[,2],
         fw_age = as.integer(fw_chr),
         oc_age = as.integer(oc_chr),
         tot_age = fw_age + oc_age) %>%
  filter(!is.na(tot_age)) %>%
  group_by(year, Origin, Sex) %>%
  mutate(perc = n / sum(n)) %>%
  ungroup()

# tributary spawners
trib_spawners_all = pit_est %>%
  filter(location %in% c('ICL',
                         'PES',
                         'MCL',
                         'CHM',
                         'CHW',
                         'CHL',
                         'NAL',
                         'LWN',
                         'WTL')) %>%
  mutate(across(location,
                recode,
                'CHL' = 'Chiwawa',
                'CHM' = 'Chumstick',
                'CHW' = 'Chiwaukum',
                'ICL' = 'Icicle',
                'LWN' = 'Little Wenatchee',
                'MCL' = 'Mission',
                'NAL' = 'Nason',
                'PES' = 'Peshastin',
                'WTL' = 'White River')) %>%
  select(year,
         origin,
         river = location,
         spwn = mean,
         se = sd) %>%
  mutate(origin = recode(origin,
                         "W" = "nor",
                         "H" = "hor")) %>%
  arrange(year, river, origin) %>%
  pivot_wider(names_from = origin,
              values_from = c(spwn,
                              se),
              names_glue = "{origin}_{.value}")


# use fish/redd and pHOS estimates for each section of Wenatchee (upper, lower, tribs)
# based on PIT tag detections within each section
recent_spwn_redd <- all_wen_df %>%
  select(year,
         loc_est) %>%
  unnest(loc_est) %>%
  left_join(pit_sex_org) %>%
  filter(str_detect(location, "_TUM$")) %>%
  rowwise() %>%
  mutate(spawn_est = redd_est * fpr,
         spawn_se = deltamethod(~ x1 * x2,
                                mean = c(redd_est,
                                         fpr),
                                cov = diag(c(redd_se,
                                             fpr_se)^2))) %>%
  mutate(hor_spwn = redd_est * fpr * phos,
         hor_se = deltamethod(~ x1 * x2 * x3,
                              mean = c(redd_est,
                                       fpr,
                                       phos),
                              cov = diag(c(redd_se,
                                           fpr_se,
                                           phos_se)^2))) %>%
  mutate(nor_spwn = redd_est * fpr * (1 - phos),
         nor_se = deltamethod(~ x1 * x2 * x3,
                              mean = c(redd_est,
                                       fpr,
                                       1 - phos),
                              cov = diag(c(redd_se,
                                           fpr_se,
                                           phos_se)^2))) %>%
  ungroup()



#-------------------------------------------------------------------
# calculate 2020 spawners using RT survivals, to complete the total timeseries
# data from WDFW (Nate Fuchs' radio telemetry study)
rt_df = tibble(year = rep(2015:2016, each = 2),
               Origin = rep(c("Hatchery", "Natural"), 2),
               ow_fish = c(20, 25, 4, 12),
               surv_fish = c(16, 24, 3, 9)) %>%
  mutate(phi = surv_fish / ow_fish,
         phi_se = sqrt((phi * (1 - phi))/ ow_fish))

# add years together
rt_df %<>%
  bind_rows(rt_df %>%
              group_by(Origin) %>%
              summarize(across(c(ow_fish, surv_fish),
                               sum),
                        .groups = "drop") %>%
              mutate(phi = surv_fish / ow_fish,
                     phi_se = sqrt((phi * (1 - phi))/ ow_fish))) %>%
  mutate(across(year,
                as.factor)) %>%
  mutate(across(year,
                fct_explicit_na,
                na_level = "Total"))


rt_df %>%
  filter(year != "Total") %>%
  group_by(Origin) %>%
  summarize(across(phi,
                   mean))



rt_df %>%
  filter(year != "Total") %>%
  group_by(year) %>%
  summarize(across(ends_with("fish"),
                   sum),
            .groups = "drop") %>%
  mutate(phi = surv_fish / ow_fish,
         phi_se = sqrt((phi * (1 - phi))/ ow_fish)) %>%
  summarize(across(phi,
                   mean))


rt_df %>%
  ggplot(aes(x = year,
             y = phi,
             color = Origin)) +
  geom_errorbar(aes(ymin = qnorm(0.025, phi, phi_se),
                    ymax = qnorm(0.975, phi, phi_se)),
                width = 0) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set1",
                     name = "Origin") +
  facet_wrap(~ Origin) +
  labs(x = "Year",
       y = "Overwinter Survival")


test <- rt_df %>%
  mutate(prop_sims = map2(phi, phi_se,
                           .f = function(mu, se) {
                             var = se^2
                             alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
                             beta <- alpha * (1 / mu - 1)
                             sims <- rbeta(10000, alpha, beta)
                             return(sims)
                           }))
test %>%
  # filter(year != 'Total') %>%
  filter(year == 'Total') %>%
  unnest(prop_sims) %>%
  group_by(Origin) %>%
  summarize(mu = mean(prop_sims),
            se = sd(prop_sims),
            cv = se / mu)

test %>%
  # filter(year != 'Total') %>%
  filter(year == 'Total') %>%
  unnest(prop_sims) %>%
  # group_by(year) %>%
  summarize(mu = mean(prop_sims),
            se = sd(prop_sims),
            cv = se / mu)



#--------------------------------------------------------------
# removal data
data("removal_df")

spwn_main_2020 = pit_est %>%
  filter(year == 2020,
         location %in% c("LWE")) %>%
  mutate(across(origin,
                recode,
                "H" = "hor",
                "W" = "nor")) %>%
  left_join(removal_df %>%
              group_by(year = Year,
                       origin = Origin) %>%
              summarize(across(rem,
                               sum)) %>%
              mutate(across(origin,
                            recode,
                            "Hatchery" = "hor",
                            "Natural" = "nor"))) %>%
  rowwise() %>%
  mutate(escp = max(0, mean - rem)) %>%
  ungroup() %>%
  select(year, origin, location,
         mean, rem,
         escp, escp_se = sd) %>%
  full_join(rt_df %>%
              mutate(across(Origin,
                            recode,
                            "Hatchery" = "hor",
                            "Natural" = "nor")) %>%
              rename(origin = Origin) %>%
              group_by(origin) %>%
              summarize(across(c(ow_fish, surv_fish),
                               sum),
                        .groups = "drop") %>%
              mutate(phi = surv_fish / ow_fish,
                     phi_se = sqrt((phi * (1 - phi))/ ow_fish)) %>%
              select(-ends_with("fish"))) %>%
  rowwise() %>%
  mutate(spwn = escp * phi) %>%
  mutate(spwn_se = msm::deltamethod(~ x1 * x2,
                                    mean = c(escp, phi),
                                    cov = diag(c(escp_se, phi_se)^2))) %>%
  ungroup() %>%
  select(year, origin,
         # river = location,
         mean,
         rem,
         escp,
         phi,
         spwn,
         spwn_se) %>%
  left_join(trib_spawners_all %>%
              pivot_longer(cols = c(starts_with("hor"),
                                    starts_with("nor")),
                           names_to = "source",
                           values_to = "value") %>%
              mutate(origin = if_else(str_detect(source, "hor"),
                                      "hor",
                                      "nor"),
                     estimate = if_else(str_detect(source, "se"),
                                        "se",
                                        "est")) %>%
              select(-source) %>%
              pivot_wider(names_from = estimate,
                          values_from = value) %>%
              group_by(year,
                       origin) %>%
              summarize(across(est,
                               sum),
                        across(se,
                               ~ sqrt(sum(.^2))),
                        .groups = "drop") %>%
              rename(trib_spwn = est,
                     trib_se = se)) %>%
  mutate(main_spwn = spwn - trib_spwn,
         main_se = sqrt(spwn_se^2 + trib_se^2)) %>%
  select(year, origin,
         spwn = main_spwn,
         se = main_se) %>%
  mutate(across(spwn,
                ~ if_else(. < 0, 0, .))) %>%
  pivot_wider(names_from = origin,
              values_from = c(spwn, se),
              names_glue = "{origin}_{.value}")

# add tributary spawners
spwn_2020 <- spwn_main_2020 %>%
  bind_rows(trib_spawners_all %>%
              filter(year == "2020")) %>%
  group_by(year) %>%
  summarize(across(ends_with("spwn"),
                   sum),
            across(ends_with("se"),
                   ~ sqrt(sum(.^2))))


#-------------------------------------------------------------------
# put some of these together
rem_df <- removal_df %>%
  select(year = Year,
         origin = Origin,
         location = Area,
         rem) %>%
  mutate(across(origin,
                recode,
                "Hatchery" = "hor",
                "Natural" = "nor"),
         across(location,
                recode,
                "TUM_bb" = "Above_TUM")) %>%
  group_by(year, origin) %>%
  summarize(across(rem,
                   sum))

trib_df <- trib_spawners_all %>%
  pivot_longer(cols = c(starts_with("hor"),
                        starts_with("nor")),
               names_to = "source",
               values_to = "value") %>%
  mutate(origin = if_else(str_detect(source, "hor"),
                          "hor",
                          "nor"),
         estimate = if_else(str_detect(source, "se"),
                            "se",
                            "est")) %>%
  select(-source) %>%
  pivot_wider(names_from = estimate,
              values_from = value) %>%
  group_by(year,
           origin) %>%
  summarize(across(est,
                   sum),
            across(se,
                   ~ sqrt(sum(.^2))),
            .groups = "drop") %>%
  rename(trib_spwn = est,
         trib_se = se)

phi_df <- rt_df %>%
  filter(year == "Total") %>%
  mutate(across(Origin,
                recode,
                "Hatchery" = "hor",
                "Natural" = "nor")) %>%
  rename(origin = Origin) %>%
  # group_by(origin) %>%
  # summarize(across(c(ow_fish, surv_fish),
  #                  sum),
  #           .groups = "drop") %>%
  # mutate(phi = surv_fish / ow_fish,
  #        phi_se = sqrt((phi * (1 - phi))/ ow_fish)) %>%
  select(origin,
         contains("phi"))

main_spwn_df <- recent_spwn_redd %>%
  filter(str_detect(location, "TUM")) %>%
  group_by(year) %>%
  summarize(across(c(obs_redds,
                     redd_est,
                     # spawn_est,
                     hor_spwn,
                     nor_spwn),
                   sum),
            across(c(redd_se,
                     # spawn_se,
                     hor_se,
                     nor_se),
                   ~ sqrt(sum(.^2))),
            .groups = "drop") %>%
  pivot_longer(cols = c(starts_with("hor"),
                        starts_with("nor")),
               names_to = "source",
               values_to = "value") %>%
  mutate(origin = if_else(str_detect(source, "hor"),
                          "hor",
                          "nor"),
         estimate = if_else(str_detect(source, "se"),
                            "se",
                            "est")) %>%
  select(-source) %>%
  pivot_wider(names_from = estimate,
              values_from = value) %>%
  rename(main_redd_spwn = est,
         main_redd_se = se)


comp_df <- pit_est %>%
  filter(location == "LWE") %>%
  mutate(across(origin,
                recode,
                "H" = "hor",
                "W" = "nor")) %>%
  select(year, origin,
         lwe_fish = mean,
         lwe_se = sd) %>%
  inner_join(rem_df) %>%
  inner_join(phi_df) %>%
  inner_join(trib_df) %>%
  left_join(main_spwn_df) %>%
  rowwise() %>%
  mutate(lwe_escp = lwe_fish - rem,
         main_rt_spwn = (lwe_escp * phi) - trib_spwn,
         main_rt_se = msm::deltamethod(~ (x1 * x2) - x3,
                                       mean = c(lwe_escp,
                                                phi,
                                                trib_spwn),
                                       cov = diag(c(lwe_se,
                                                    phi_se,
                                                    trib_se)^2))) %>%
  mutate(all_redd_spwn = trib_spwn + main_redd_spwn,
         all_redd_se = sqrt(trib_se^2 + main_redd_se^2),
         all_rt_spwn = lwe_escp * phi,
         all_rt_se = msm::deltamethod(~ x1 * x2,
                                           mean = c(lwe_escp,
                                                    phi),
                                           cov = diag(c(lwe_se,
                                                        phi_se)^2))) %>%
  mutate(ows = all_redd_spwn / lwe_escp,
         ows_se = msm::deltamethod(~ x1 / x2,
                                   mean = c(all_redd_spwn,
                                            lwe_fish),
                                   cov = diag(c(all_redd_se,
                                                lwe_se)^2)),
         psm = (lwe_escp - trib_spwn - main_redd_spwn) / lwe_escp,
         psm_se = msm::deltamethod(~ (x1 - x2 - x3) / x1,
                                   mean = c(lwe_escp,
                                            trib_spwn,
                                            main_redd_spwn),
                                   cov = diag(c(lwe_se,
                                                trib_se,
                                                main_redd_se)^2))) %>%
  ungroup()

# pull out pHOS metric
phos_comp = comp_df %>%
  select(year,
         origin,
         starts_with("main_r"),
         starts_with("all_r")) %>%
  mutate(across(ends_with("spwn"),
                ~ if_else(. < 0, 0, .))) %>%
  pivot_longer(-c(year, origin)) %>%
  mutate(type = if_else(str_detect(name, "se"),
                        "se",
                        "est"),
         source = if_else(str_detect(name, "redd"),
                          "redd",
                          "rt"),
         area = if_else(str_detect(name, "main"),
                        "mainstem",
                        "population")) %>%
  select(-name) %>%
  pivot_wider(names_from = c(origin, type),
              values_from = value) %>%
  arrange(year,
          source) %>%
  rowwise() %>%
  mutate(phos = hor_est / (hor_est + nor_est),
         phos_se = deltamethod(~ x1 / (x1 + x2),
                               mean = c(hor_est,
                                        nor_est),
                               cov = diag(c(hor_se, nor_se)^2)),
         phos_cv = phos_se / phos) %>%
  ungroup()

phos_comp %>%
  filter(area == "mainstem") %>%
  left_join(pit_sex_org2 %>%
              select(year,
                     phos2 = phos))

#--------------------------------------------------------------
# create figures
#--------------------------------------------------------------
pd = 0.3
phos_est_p <- phos_comp %>%
  mutate(across(source,
                recode,
                "redd" = "Redd-based",
                "rt" = "RT-based"),
         across(area,
                str_to_title)) %>%
  ggplot(aes(x = year,
             y = phos,
             color = source)) +
  # goal in Wenatchee is <= 30%
  geom_hline(yintercept = 0.3,
             linetype = 2) +
  geom_errorbar(aes(ymin = qnorm(0.025, phos, phos_se),
                    ymax = qnorm(0.975, phos, phos_se)),
                width = 0,
                position = position_dodge(pd)) +
  geom_point(position = position_dodge(pd),
             size = 3) +
  scale_color_viridis_d(end = 0.8,
                        name = "Method") +
  facet_wrap(~ area) +
  labs(x = "Year",
       y = "pHOS") +
  coord_cartesian(ylim = c(0, 1))


phos_cv_p <- phos_comp %>%
  mutate(across(source,
                recode,
                "redd" = "Redd-based",
                "rt" = "RT-based"),
         across(area,
                str_to_title)) %>%
  filter(phos != 0) %>%
  ggplot(aes(x = year,
             y = phos_cv,
             color = source)) +
  geom_hline(yintercept = 0.15,
             linetype = 2) +
  geom_point(position = position_dodge(pd),
             size = 3) +
  scale_color_viridis_d(end = 0.8,
                        name = "Method") +
  facet_wrap(~ area) +
  labs(x = "Year",
       y = "CV of pHOS") +
  coord_cartesian(ylim = c(0.05, 0.5))

phos_bias_p <- phos_comp %>%
  select(year:area,
         contains("phos")) %>%
  pivot_wider(names_from = source,
              values_from = contains("phos")) %>%
  mutate(bias = phos_rt - phos_redd,
         rel_bias = bias / phos_redd,
         pos_bias = if_else(rel_bias > 0, F, T)) %>%
  mutate(across(area,
                str_to_title)) %>%
  ggplot(aes(x = year,
             y = bias)) +
  geom_hline(yintercept = 0,
             linetype = 2,
             color = "darkgray") +
  geom_line() +
  geom_point(aes(color = pos_bias),
             size = 3) +
  scale_color_brewer(palette = "Dark2",
                     guide = "none") +
  facet_wrap(~ area) +
  labs(x = "Year",
       y = "Bias in pHOS of RT\nCompared to Redds")


phos_rel_bias_p <- phos_comp %>%
  select(year:area,
         contains("phos")) %>%
  pivot_wider(names_from = source,
              values_from = contains("phos")) %>%
  mutate(bias = phos_rt - phos_redd,
         rel_bias = bias / phos_redd,
         pos_bias = if_else(rel_bias > 0, F, T)) %>%
  mutate(across(area,
                str_to_title)) %>%
  ggplot(aes(x = year,
             y = rel_bias)) +
  geom_hline(yintercept = 0,
             linetype = 2,
             color = "darkgray") +
  geom_line() +
  geom_point(aes(color = pos_bias),
             size = 3) +
  scale_color_brewer(palette = "Dark2",
                     guide = "none") +
  facet_wrap(~ area) +
  labs(x = "Year",
       y = "Relative Bias (%) of RT\nCompared to Redds",
       title = "pHOS Bias")


phos_plot_lst <- list(
  phos_estimates = phos_est_p,
  phos_cv = phos_cv_p,
  phos_bias = phos_bias_p,
  phos_rel_bias = phos_rel_bias_p)

for(i in 1:length(phos_plot_lst)) {
  ggsave(paste0(save_loc,
               names(phos_plot_lst)[i],
               ".png"),
         phos_plot_lst[[i]],
         width = 7,
         height = 4)
}





phos_comp %>%
  select(year:area,
         contains("phos")) %>%
  pivot_wider(names_from = source,
              values_from = contains("phos")) %>%
  mutate(bias = phos_rt - phos_redd,
         rel_bias = bias / phos_redd,
         pos_bias = if_else(rel_bias > 0, T, F)) %>%
  mutate(across(area,
                str_to_title)) %>%
  ggplot(aes(x = area,
             y = rel_bias)) +
  geom_boxplot(fill = "lightblue") +
  geom_hline(yintercept = 0,
             linetype = 2,
             color = "darkgray") +
  labs(x = "Spatial Area",
       y = "Relative Bias (%) of RT\nCompared to Redds",
       title = "pHOS Bias")


pd = 0.3
spwn_ts <- comp_df %>%
  mutate(across(origin,
                recode,
                "hor" = "Hatchery",
                "nor" = "Natural")) %>%
  select(year,
         origin,
         starts_with("main_"),
         starts_with("all_")) %>%
  pivot_longer(-c(year, origin)) %>%
  mutate(type = if_else(str_detect(name, "se"),
                        "se",
                        "est"),
         source = if_else(str_detect(name, "redd"),
                          "Redd-based",
                          "RT-based"),
         area = if_else(str_detect(name, "main"),
                        "Mainstem",
                        "Population")) %>%
  select(-name) %>%
  pivot_wider(names_from = type,
              values_from = value) %>%
  arrange(year,
          source,
          area,
          origin) %>%
  ggplot(aes(x = year,
             y = est,
             color = source)) +
  geom_errorbar(aes(ymin = qnorm(0.025, est, se),
                    ymax = qnorm(0.975, est, se)),
                position = position_dodge(pd),
                width = 0) +
  geom_point(position = position_dodge(pd),
             size = 2) +
  geom_line(position = position_dodge(pd)) +
  facet_grid(area ~ origin,
             scales = "free_y") +
  scale_color_viridis_d(name = "Method",
                        end = 0.8) +
  theme(legend.position = "bottom") +
  labs(x = "Year",
       y = "Estimate",
       title = "Wenatchee Steelhead Spawners")

spwn_cv <- comp_df %>%
  mutate(across(origin,
                recode,
                "hor" = "Hatchery",
                "nor" = "Natural")) %>%
  mutate(across(ends_with("spwn"),
                ~ if_else(. < 0, 0, .))) %>%
  mutate(main_redd_cv = main_redd_se / main_redd_spwn,
         all_redd_cv = all_redd_se / all_redd_spwn,
         main_rt_cv = main_rt_se / main_rt_spwn,
         all_rt_cv = all_rt_se / all_rt_spwn) %>%
  select(year,
         origin,
         ends_with("cv")) %>%
  pivot_longer(-c(year, origin),
               values_to = "cv") %>%
  mutate(source = if_else(str_detect(name, "redd"),
                          "Redd-based",
                          "RT-based"),
         area = if_else(str_detect(name, "main"),
                        "Mainstem",
                        "Population")) %>%
  select(-name) %>%
  arrange(year,
          source,
          area,
          origin) %>%
  filter(cv <= 1) %>%
  ggplot(aes(x = year,
             y = cv,
             color = source)) +
  geom_hline(yintercept = 0.15,
             linetype = 2) +
  geom_point(position = position_dodge(pd),
             size = 3) +
  # geom_line(position = position_dodge(pd)) +
  facet_grid(area ~ origin,
             scales = "free_y") +
  scale_color_viridis_d(name = "Method",
                        end = 0.8) +
  # coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom") +
  labs(x = "Year",
       y = "CV of Spawner Estimates",
       title = "Wenatchee Steelhead Spawners")


comp_df %>%
  ggplot(aes(x = main_redd_spwn,
             y = main_rt_spwn,
             color = origin)) +
  geom_abline(linetype = 2) +
  # geom_errorbar(aes(ymin = qnorm(0.025, main_rt_spwn, main_rt_se),
  #                   ymax = qnorm(0.975, main_rt_spwn, main_rt_se))) +
  # geom_errorbarh(aes(xmin = qnorm(0.025, main_redd_spwn, main_redd_se),
  #                    xmax = qnorm(0.975, main_redd_spwn, main_redd_se))) +
  geom_point(size = 3) +
  # geom_smooth(method = lm,
  #             formula = y ~ x - 1,
  #             se = F,
  #             fullrange = T) +
  geom_text_repel(aes(label = year),
                  show.legend = F) +
  scale_color_brewer(palette = "Set1",
                     name = "Origin") +
  scale_y_continuous(limits = c(-100, 310)) +
  geom_hline(yintercept = 0,
             linetype = 3) +
  geom_vline(xintercept = 0,
             linetype = 3) +
  labs(title = "Mainstem Wenatchee Steelhead Spawners",
       x = "Redd-based",
       y = "RT based")

comp_df %>%
  ggplot(aes(x = all_redd_spwn,
             y = all_rt_spwn,
             color = origin)) +
  geom_abline(linetype = 2) +
  geom_errorbar(aes(ymin = qnorm(0.025, all_rt_spwn, all_rt_se),
                    ymax = qnorm(0.975, all_rt_spwn, all_rt_se)),
                width = 0) +
  geom_errorbarh(aes(xmin = qnorm(0.025, all_redd_spwn, all_redd_se),
                     xmax = qnorm(0.975, all_redd_spwn, all_redd_se)),
                 height = 0) +
  geom_point(size = 3) +
  geom_smooth(method = lm,
              formula = y ~ x - 1,
              se = F,
              fullrange = T) +
  geom_text_repel(aes(label = year),
                  show.legend = F) +
  scale_color_brewer(palette = "Set1",
                     name = "Origin") +
  labs(title = "Wenatchee Population Steelhead Spawners",
       x = "Redd-based",
       y = "RT based")


spwn_rel_bias_p <- comp_df %>%
  rename(total_redd_spwn = all_redd_spwn,
         total_rt_spwn = all_rt_spwn) %>%
  mutate(main_bias = main_rt_spwn - main_redd_spwn,
         total_bias = total_rt_spwn - total_redd_spwn,
         main_rel_bias = main_bias / main_redd_spwn,
         main_abs_bias = abs(main_bias),
         total_rel_bias = total_bias / total_redd_spwn,
         total_abs_bias = abs(total_bias)) %>%
  select(year, origin,
         contains("spwn"),
         contains("bias")) %>%
  pivot_longer(c(contains("main"),
                 contains("total"))) %>%
  mutate(type = if_else(str_detect(name, "main"),
                        "Mainstem",
                        "Population"),
         across(name,
                str_remove,
                "^main_"),
         across(name,
                str_remove,
                "^total_")) %>%
  pivot_wider() %>%
  mutate(across(origin,
                recode,
                "hor" = "Hatchery",
                "nor" = "Natural")) %>%
  ggplot(aes(x = origin,
             y = rel_bias,
             fill = origin)) +
  geom_boxplot() +
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  stat_summary(fun = "mean",
               geom = "point",
               shape = 17,
               size = 4,
               color = "blue",
               show.legend = F) +
  geom_point(size = 2,
             position = position_jitter(width = 0.1)) +
  geom_hline(yintercept = 0,
             linetype = 2,
             lwd = 1) +
  scale_fill_brewer(palette = "Set1",
                    name = "Origin") +
  facet_wrap(~ type,
             scales = "free_y") +
  theme(legend.position = "bottom",
        axis.title.x = element_blank()) +
  labs(y = "Relative Bias (%) of RT\nCompared to Redds",
       title = "Wenatchee Steelhead Spawners")


spwn_rel_bias_ts <- comp_df %>%
  rename(total_redd_spwn = all_redd_spwn,
         total_rt_spwn = all_rt_spwn) %>%
  mutate(main_bias = main_rt_spwn - main_redd_spwn,
         total_bias = total_rt_spwn - total_redd_spwn,
         main_rel_bias = main_bias / main_redd_spwn,
         main_abs_bias = abs(main_bias),
         total_rel_bias = total_bias / total_redd_spwn,
         total_abs_bias = abs(total_bias)) %>%
  select(year, origin,
         contains("spwn"),
         contains("bias")) %>%
  pivot_longer(c(contains("main"),
                 contains("total"))) %>%
  mutate(type = if_else(str_detect(name, "main"),
                        "Mainstem",
                        "Population"),
         across(name,
                str_remove,
                "^main_"),
         across(name,
                str_remove,
                "^total_")) %>%
  pivot_wider() %>%
  mutate(across(origin,
                recode,
                "hor" = "Hatchery",
                "nor" = "Natural")) %>%
  mutate(pos_bias = if_else(rel_bias > 0, F, T)) %>%
  ggplot(aes(x = year,
             y = rel_bias)) +
  geom_hline(yintercept = 0,
             linetype = 2) +
  geom_line() +
  geom_point(aes(color = pos_bias),
             size = 3) +
  scale_color_brewer(palette = "Dark2",
                     guide = "none") +
  facet_grid(type ~ origin,
             scales = "free_y") +
  labs(x = "Year",
       y = "Relative Bias (%) of RT\nCompared to Redds",
       title = "Wenatchee Steelhead Spawners")


spwn_plot_lst <- list(
  spwn_ts = spwn_ts,
  spwn_cv = spwn_cv,
  spwn_rel_bias = spwn_rel_bias_p,
  spwn_rel_bias_ts = spwn_rel_bias_ts)

for(i in 1:length(spwn_plot_lst)) {
  ggsave(paste0(save_loc,
                names(spwn_plot_lst)[i],
                ".png"),
         spwn_plot_lst[[i]],
         width = 7,
         height = 4)
}

