# Author: Kevin See
# Purpose: Prep Wenatchee estimates for 2014-2020
# Created: 6/17/22
# Last Modified: 6/20/22
# Notes: For comparison purposes with Four Peaks and CBR model results

#######################
# deal with removals  #
# - only needed for   #
# pre-spawn mortality #
#######################

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
# how many fish removed this year?
data("removal_df")
wen_rem <- removal_df %>%
  filter(Subbasin == "Wenatchee") %>%
  clean_names() %>%
  mutate(area = recode(area,
                       "Below_TUM" = "Mainstem below Tumwater",
                       "TUM_bb" = "Mainstem above Tumwater"),
         origin = recode(origin,
                         "Hatchery" = "H",
                         "Natural" = "W"))

#-----------------------------------------------------------------
# compile all redd data
all_redd_df <- tibble(year = 2014:2021) %>%
  mutate(redd_data = map(year,
                         .f = function(yr) {
                           load(here('analysis/data/derived_data',
                                     paste0("wen_", yr, ".rda")))
                           if(sum(str_detect(ls(), "redd_df")) > 0) {
                             return(redd_df)
                           } else {
                             return(NULL)
                           }
                         })) %>%
  unnest(redd_data) %>%
  select(year:NetErrorSE) %>%
  mutate(across(SurveyType,
                str_to_title)) %>%
  mutate(location = if_else(Reach %in% paste0("W", 8:10),
                            "Mainstem above Tumwater",
                            if_else(Reach %in% paste0("W", 1:7),
                                    "Mainstem below Tumwater",
                                    if_else(str_detect(Reach, "^P"),
                                            "Tributaries below Tumwater",
                                            "Tributaries above Tumwater")))) %>%
  relocate(location,
           .after = Reach)

# redd counts in tributaries below the PIT tag antennas
redds_below_arrays = read_excel(here('analysis/data/raw_data',
                                     'Tributary Redds_Below Arrays_2014 to 2021.xlsx'),
                                skip = 1) %>%
  rename(year = `...1`,
         total = `...5`,
         notes = `...6`) %>%
  select(year,
         starts_with("WEN")) %>%
  pivot_longer(starts_with("WEN"),
               names_to = "Reach",
               values_to = "redd_add") %>%
  mutate(across(redd_add,
                as.numeric)) %>%
  mutate(across(Reach,
                str_remove,
                "^WEN-"),
         across(Reach,
                as_factor)) %>%
  mutate(redd_se = 0) %>%
  mutate(River = recode(Reach,
                        "C1" = "Chiwawa",
                        "N1" = "Nason",
                        "P1" = "Peshastin")) %>%
  relocate(River) %>%
  # add_column(River = "Wenatchee",
  #            .before = 0) %>%
  filter(!is.na(redd_add))


#-------------------------------------
# generate GAUC estimates
# set some thresholds
# minimum number of total redds observed
min_redds = 2
# minimum number of weeks with at least one new redd observed
min_non0_wks = 3
#-------------------------------------

redd_results = all_redd_df %>%
  group_by(year, River, Reach, Index, SurveyType, location) %>%
  summarise(n_weeks = n(),
            n_non0_wks = sum(NewRedds > 0, na.rm=T),
            tot_feat = sum(NewRedds, na.rm=T),
            err_est = mean(NetError[VisibleRedds > 0]),
            err_se = mean(NetErrorSE[VisibleRedds > 0])) %>%
  ungroup() %>%
  mutate(err_se = ifelse(is.na(err_est), 0, err_se),
         err_est = ifelse(is.na(err_est), 1, err_est)) %>%
  full_join(all_redd_df %>%
              group_by(year, River, Reach, Index, SurveyType) %>%
              nest()) %>%
  mutate(gauc_list = map(data,
                         .f = function(x) {
                           mod_df = x %>%
                             select(redds = NewRedds) %>%
                             mutate(day = 1:n())

                           v_vec = x %>%
                             filter(VisibleRedds > 0) %>%
                             summarise_at(vars(NetError, NetErrorSE),
                                          list(mean),
                                          na.rm = T) %>%
                             mutate(NetErrorSE = if_else(is.na(NetError),
                                                         0,
                                                         NetErrorSE),
                                    NetError = if_else(is.na(NetError),
                                                       1,
                                                       NetError))

                           # # if you want to assume no observer error
                           # v_vec = tibble(NetError = 1,
                           #                NetErrorSE = 0)

                           res_list = fit_gauc(data = mod_df,
                                               v = v_vec$NetError,
                                               v_se = v_vec$NetErrorSE)
                           return(res_list)
                         })) %>%
  mutate(converged = map_lgl(gauc_list,
                             .f = function(x) {
                               x$model$converged
                             })) %>%
  mutate(redd_est = map_dbl(gauc_list,
                            .f = 'E'),
         redd_se = map_dbl(gauc_list,
                           .f = 'E_se')) %>%
  mutate(GAUC = if_else(!converged | n_non0_wks < min_non0_wks | tot_feat < min_redds,
                        F, T)) %>%
  rowwise() %>%
  mutate(redd_est = if_else(!GAUC,
                            tot_feat / err_est,
                            redd_est),
         redd_se = if_else(!GAUC,
                           msm::deltamethod(~ x1 / x2,
                                            mean = c(tot_feat, err_est),
                                            cov = diag(c(0, err_se)^2)),
                           redd_se)) %>%
  ungroup() %>%
  mutate_at(vars(redd_est),
            list(round_half_up))

# add the redd surveys from below the PIT arrays in the tributaries
redd_results %<>%
  full_join(redds_below_arrays %>%
              select(-redd_se)) %>%
  mutate(across(redd_add,
                replace_na,
                0)) %>%
  mutate(across(Index,
                replace_na,
                as.factor("N")),
         across(SurveyType,
                replace_na,
                "Peak")) %>%
  mutate(redd_est = redd_est + redd_add,
         redd_est = if_else(is.na(redd_est) & redd_add > 0,
                            redd_add,
                            redd_est))

# redd_results %<>%
#   mutate(across(Reach,
#                 fct_expand,
#                 levels(redds_below_arrays$Reach))) %>%
#   bind_rows(redds_below_arrays %>%
#               mutate(Index = "N",
#                      err_est = 1,
#                      err_se = 0,
#                      tot_feat = redd_est,
#                      GAUC = F)) %>%
#   arrange(year,
#           River,
#           Reach)

# label by broad location categories
redd_results %<>%
  mutate(location = if_else(Reach %in% paste0("W", 8:10),
                            "Mainstem above Tumwater",
                            if_else(Reach %in% paste0("W", 1:7),
                                    "Mainstem below Tumwater",
                                    if_else(str_detect(Reach, "^P"),
                                            "Tributaries below Tumwater",
                                            "Tributaries above Tumwater")))) %>%
  relocate(location,
           .after = Reach)


mainstm_est_index <- redd_results %>%
  filter(str_detect(location,
                    "Mainstem"),
         SurveyType == "Weekly") %>%
  select(year, River, Index, location, Reach, tot_feat, redd_est, redd_se) %>%
  group_by(year, River, location, Index) %>%
  nest() %>%
  rename(redd_tot = data) %>%
  mutate(n_rchs = map_int(redd_tot,
                          .f = function(x) {
                            n_distinct(x$Reach)
                          })) %>%
  # get correlations between reaches within a stream
  left_join(all_redd_df %>%
              group_by(year, River, location, Index) %>%
              nest()) %>%
  mutate(cor_mat = map(data,
                       .f = function(x) {
                         cor_mat = try(x %>%
                                         group_by(Reach) %>%
                                         mutate(Survey = 1:n()) %>%
                                         ungroup() %>%
                                         select(Reach, Survey, NewRedds) %>%
                                         spread(Reach, NewRedds) %>%
                                         select(-Survey) %>%
                                         cor(use = "pairwise",
                                             # method = 'kendall')
                                             method = 'spearman'),
                                       silent = T)

                         if(class(cor_mat) == 'try-error') {
                           return(as.numeric(1))
                         }else{

                           cor_mat[is.na(cor_mat)] = 0
                           diag(cor_mat) = 1
                           return(cor_mat)
                         }
                       })) %>%
  mutate(redd_obs = map_dbl(redd_tot,
                            .f = function(x) {
                              sum(x$tot_feat, na.rm = T)
                            }),
         redd_est = map_dbl(redd_tot,
                            .f = function(x) {
                              sum(x$redd_est, na.rm = T)
                            }),
         redd_se_naive = map_dbl(redd_tot,
                                 .f = function(x) {
                                   sqrt(sum(x$redd_se, na.rm = T)^2)
                                 })) %>%
  ungroup() %>%
  # use correlations between reaches to get appropriate standard error
  mutate(redd_se = map2_dbl(redd_tot,
                            cor_mat,
                            .f = function(x, y) {

                              deltamethod(as.formula(paste('~', paste0('x', 1:nrow(x), collapse='+'))),
                                          mean = x$redd_est,
                                          cov = diag(x$redd_se) %*% y %*% diag(x$redd_se))
                            })) %>%
  # select(-redd_tot, -data, -cor_mat) %>%
  mutate(redd_cv = redd_se / redd_est)

mainstm_est_nonindex <- redd_results %>%
  filter(str_detect(location,
                    "Mainstem"),
         SurveyType != "Weekly") %>%
  select(year, River, Index, location, Reach, tot_feat, redd_est, redd_se) %>%
  group_by(year, River, location, Index) %>%
  nest() %>%
  rename(redd_tot = data) %>%
  mutate(n_rchs = map_int(redd_tot,
                          .f = function(x) {
                            n_distinct(x$Reach)
                          })) %>%
  mutate(redd_obs = map_dbl(redd_tot,
                            .f = function(x) {
                              sum(x$tot_feat, na.rm = T)
                            }),
         redd_est = map_dbl(redd_tot,
                            .f = function(x) {
                              sum(x$redd_est, na.rm = T)
                            }),
         redd_se = map_dbl(redd_tot,
                           .f = function(x) {
                             sqrt(sum(x$redd_se, na.rm = T)^2)
                           })) %>%
  mutate(redd_cv = redd_se / redd_est) %>%
  ungroup()

mainstm_est_all <- redd_results %>%
  filter(str_detect(location,
                    "Mainstem")) %>%
  group_by(year, River,
           location, Index, SurveyType) %>%
  summarize(across(err_est,
                   weighted.mean,
                   w = n_non0_wks,
                   na.rm = T),
            across(c(tot_feat,
                     n_non0_wks),
                   sum,
                   na.rm = T),
            .groups = "drop") %>%
  left_join(mainstm_est_index %>%
              select(year:location,
                     # redd_obs,
                     redd_est,
                     redd_se,
                     redd_cv) %>%
              bind_rows(mainstm_est_nonindex %>%
                          select(year:location,
                                 # redd_obs,
                                 redd_est,
                                 redd_se,
                                 redd_cv))) %>%
  group_by(year,
           River,
           location) %>%
  summarize(across(err_est,
                   weighted.mean,
                   w = tot_feat,
                   na.rm = T),
            across(c(tot_feat,
                     redd_est),
                   sum,
                   na.rm = T),
            across(redd_se,
                   ~ sqrt(sum(.^2, na.rm = T))),
            redd_cv = redd_se / redd_est,
            .groups = "drop")



#-----------------------------------------------------------------
# load relevant data from DABOM
# what dam count to use?
dam_cnt_name = c("PriestRapids",
                 "RockIsland",
                 "RockyReach")[2]


all_dabom_df <- tibble(year = 2014:2021) %>%
  mutate(wen_data = map(year,
                        .f = function(yr) {
                          load(paste0('O:Documents/Git/MyProjects/DabomPriestRapidsSthd/analysis/data/derived_data/estimates/',
                                      dam_cnt_name,
                                      "/PRA_DABOM_Steelhead_",
                                      yr, ".rda"))

                          list(wen_escp = escape_summ %>%
                                 filter(location %in% c("LWE",
                                                        "LWE_bb",
                                                        "TUM",
                                                        "TUM_bb",
                                                        'ICL',
                                                        'PES',
                                                        'MCL',
                                                        'CHM',
                                                        'CHW',
                                                        'CHL',
                                                        'NAL',
                                                        'LWN',
                                                        'WTL')),
                               wen_tags = save_list[["Tag Summary"]] %>%
                                 filter(str_detect(path, "LWEB0"))) %>%
                            return()
                        })) %>%
  mutate(wen_tags = map(wen_data,
                        "wen_tags"),
         wen_escp = map(wen_data,
                        "wen_escp"),
         main_escp = map(wen_escp,
                         .f = function(x) {
                           x %>%
                             filter(location %in% c("LWE",
                                                    "LWE_bb",
                                                    # "TUM",
                                                    "TUM_bb")) %>%
                             mutate(area = recode(location,
                                                  'LWE' = 'Wenatchee Subbasin',
                                                  'LWE_bb' = "Mainstem below Tumwater",
                                                  'TUM_bb' = "Mainstem above Tumwater")) %>%
                             relocate(area,
                                      .after = 2) %>%
                             rename(est = mean,
                                    se = sd) %>%
                             select(-median,
                                    -mode,
                                    -skew,
                                    -kurtosis)
                         }),
         trib_escp = map(wen_escp,
                         .f = function(x) {
                           x %>%
                             filter(!location %in% c("LWE",
                                                    "LWE_bb",
                                                    "TUM",
                                                    "TUM_bb")) %>%
                             mutate(area = "Tributary") %>%
                             relocate(area,
                                      .after = 2) %>%
                             rename(est = mean,
                                    se = sd) %>%
                             select(-median,
                                    -mode,
                                    -skew,
                                    -kurtosis)
                         }))

main_escp <- all_dabom_df %>%
  select(year, main_escp) %>%
  unnest(main_escp) %>%
  select(-location)

trib_escp <- all_dabom_df %>%
  select(year, trib_escp) %>%
  unnest(trib_escp) %>%
  select(-species,
         -spawn_year) %>%
  mutate(River = recode(location,
                        'CHL' = 'Chiwawa',
                        'CHM' = 'Chumstick',
                        'CHW' = 'Chiwaukum',
                        'ICL' = 'Icicle',
                        'LWN' = 'Little Wenatchee',
                        'MCL' = 'Mission',
                        'NAL' = 'Nason',
                        'PES' = 'Peshastin',
                        'WTL' = 'White River')) %>%
  relocate(River, location,
           .after = area)

#----------------------------------------------
# pull out all tags in the Wenatchee
# group them in certain areas
wen_tags_all <- all_dabom_df %>%
  select(wen_tags) %>%
  unnest(wen_tags) %>%
  mutate(River = if_else(str_detect(path, " CHL"),
                         "Chiwawa",
                         if_else(str_detect(path, " NAL"),
                                 "Nason",
                                 if_else(str_detect(path, " PES"),
                                         "Peshastin",
                                         NA_character_)))) %>%
  mutate(area = if_else(str_detect(spawn_node, "^LWE"),
                        "Mainstem below Tumwater",
                        if_else(spawn_node == "TUM",
                                "Mainstem above Tumwater",
                                if_else(str_detect(path, "TUM"),
                                        "Tributaries above Tumwater",
                                        "Tributaries below Tumwater")))) %>%
  select(year, tag_code, River, area, origin, sex)

# estimates of pHOS
phos_main <- wen_tags_all %>%
  filter(str_detect(area, "Mainstem")) %>%
  group_by(year,
           area,
           origin) %>%
  summarize(n_tags = n_distinct(tag_code),
            .groups = "drop") %>%
  pivot_wider(names_from = "origin",
              values_from = n_tags) %>%
  mutate(phos = H / (H + W),
         phos_se = sqrt((phos * (1 - phos)) / (H + W)))

phos_trib <- wen_tags_all %>%
  filter(!is.na(River)) %>%
  group_by(year,
           River,
           area,
           origin) %>%
  summarize(n_tags = n_distinct(tag_code),
            .groups = "drop") %>%
  pivot_wider(names_from = "origin",
              values_from = n_tags) %>%
  mutate(phos = H / (H + W),
         phos_se = sqrt((phos * (1 - phos)) / (H + W)))

# estimates of fish / redd
fpr_main <- wen_tags_all %>%
  filter(str_detect(area, "Mainstem")) %>%
  group_by(year,
           area,
           sex) %>%
  summarize(n_tags = n_distinct(tag_code),
            .groups = "drop") %>%
  pivot_wider(names_from = "sex",
              values_from = n_tags) %>%
  mutate(m_prop = M / (M + F),
         f_prop = F / (M + F),
         prop_se = sqrt((m_prop * (1 - m_prop)) / (M + F))) %>%
  rowwise() %>%
  mutate(fpr = M / F + 1,
         fpr_se = deltamethod(~ (x1 / x2) + 1,
                              mean = c(m_prop, f_prop),
                              cov = diag(prop_se^2, nrow = 2))) %>%
  ungroup()

fpr_trib <- wen_tags_all %>%
  filter(!is.na(River)) %>%
  group_by(year,
           River,
           area,
           sex) %>%
  summarize(n_tags = n_distinct(tag_code),
            .groups = "drop") %>%
  pivot_wider(names_from = "sex",
              values_from = n_tags) %>%
  mutate(m_prop = M / (M + F),
         f_prop = F / (M + F),
         prop_se = sqrt((m_prop * (1 - m_prop)) / (M + F))) %>%
  rowwise() %>%
  mutate(fpr = M / F + 1,
         fpr_se = deltamethod(~ (x1 / x2) + 1,
                              mean = c(m_prop, f_prop),
                              cov = diag(prop_se^2, nrow = 2))) %>%
  ungroup()


#----------------------------------------------
# redds --> spawners
# apply fish / redd to mainstem estimates of redds

spawner_est_main <- mainstm_est_all %>%
  left_join(fpr_main %>%
              select(year,
                     location = area,
                     matches("fpr"))) %>%
  left_join(phos_main %>%
              select(year,
                     location = area,
                     matches("phos"))) %>%
  mutate(tot_spawn = redd_est * fpr,
         hatch_spawn = tot_spawn * phos,
         wild_spawn = tot_spawn * (1 - phos)) %>%
  rowwise() %>%
  mutate(tot_spawn_se = deltamethod(~ x1 * x2,
                                    mean = c(redd_est,
                                             fpr),
                                    cov = diag(c(redd_se,
                                                 fpr_se)^2)),
         hatch_spawn_se = deltamethod(~ x1 * x2 * x3,
                                    mean = c(redd_est,
                                             fpr,
                                             phos),
                                    cov = diag(c(redd_se,
                                                 fpr_se,
                                                 phos_se)^2)),
         wild_spawn_se = deltamethod(~ x1 * x2 * x3,
                                      mean = c(redd_est,
                                               fpr,
                                               (1 - phos)),
                                      cov = diag(c(redd_se,
                                                   fpr_se,
                                                   phos_se)^2))) %>%
  ungroup()

#-----------------------------------------------------------
# apply fish / redd and pHOS to redds below arrays in tribs
spawner_est_trib_below_arrays = redds_below_arrays %>%
  left_join(fpr_trib %>%
              select(year,
                     River,
                     location = area,
                     matches("fpr"))) %>%
  left_join(phos_trib %>%
              select(year,
                     River,
                     location = area,
                     matches("phos"))) %>%
  mutate(tot_spawn = redd_add * fpr,
         hatch_spawn = tot_spawn * phos,
         wild_spawn = tot_spawn * (1 - phos)) %>%
  rowwise() %>%
  mutate(tot_spawn_se = deltamethod(~ x1 * x2,
                                    mean = c(redd_add,
                                             fpr),
                                    cov = diag(c(redd_se,
                                                 fpr_se)^2)),
         hatch_spawn_se = deltamethod(~ x1 * x2 * x3,
                                      mean = c(redd_add,
                                               fpr,
                                               phos),
                                      cov = diag(c(redd_se,
                                                   fpr_se,
                                                   phos_se)^2)),
         wild_spawn_se = deltamethod(~ x1 * x2 * x3,
                                     mean = c(redd_add,
                                              fpr,
                                              (1 - phos)),
                                     cov = diag(c(redd_se,
                                                  fpr_se,
                                                  phos_se)^2))) %>%
  ungroup()


# put trib spawners above arrays in correct format
spawner_est_trib <- trib_escp %>%
  select(-ends_with("CI")) %>%
  pivot_wider(names_from = origin,
              values_from = c(est, se)) %>%
  rename(wild_spawn = est_W,
         hatch_spawn = est_H,
         wild_spawn_se = se_W,
         hatch_spawn_se = se_H) %>%
  rowwise() %>%
  mutate(tot_spawn = wild_spawn + hatch_spawn,
         tot_spawn_se = sqrt(sum(c(wild_spawn_se,
                                   hatch_spawn_se)^2))) %>%
  ungroup() %>%
  relocate(contains("tot_spawn"),
           .after = location)

spawner_est_main
spawner_est_trib
spawner_est_trib_below_arrays

#-------------------------------------------
# combine in estimates across entire Wenatchee
wen_est <- spawner_est_main %>%
  select(year,
         contains("spawn")) %>%
  bind_rows(spawner_est_trib_below_arrays %>%
              select(year,
                     contains("spawn"))) %>%
  bind_rows(spawner_est_trib %>%
              filter(year != 2020) %>%
              select(year,
                     contains("spawn"))) %>%
  group_by(year) %>%
  summarize(across(ends_with("spawn"),
                   sum,
                   na.rm = T),
            across(ends_with("se"),
                   ~ sqrt(sum(.^2, na.rm = T))),
            .groups = "drop") %>%
  select(year,
         starts_with("tot_"),
         starts_with("wild"),
         starts_with("hatch"),
         everything())


#-------------------------------------------------------
# when was there no harvest?
wen_rem %>%
  filter(source == "Harvest") %>%
  filter(rem == 0)

tot_rem <- wen_rem %>%
  # filter(source != "Harvest") %>%
  group_by(year, origin) %>%
  summarize(across(rem,
                   sum),
            .groups = "drop") %>%
  pivot_wider(names_from = origin,
              values_from = rem) %>%
  rename(rem_H = H,
         rem_W = W)

wen_est %>%
  left_join(tot_rem) %>%
  mutate(tot_spawn = tot_spawn - rem_H - rem_W,
         hatch_spawn = hatch_spawn - rem_H,
         wild_spawn = wild_spawn - rem_W)

# pull out removals from mainstem escapement estimates
# for any year/origin combo that leads to negative estimates
# add redds, pHOS and spawner estimates
main_escp %>%
  filter(area != "Wenatchee Subbasin") %>%
  select(year,
         area:se) %>%
  left_join(wen_rem %>%
              filter(source != "Harvest") %>%
              select(year, origin, area, rem)) %>%
  mutate(escp = est - rem) %>%
  # filter(escp < 0) %>%
  left_join(spawner_est_main %>%
              select(year, area = location,
                     tot_feat,
                     phos,
                     starts_with("hatch"),
                     starts_with("wild")) %>%
              pivot_longer(c(starts_with("hatch"),
                             starts_with("wild")),
                           names_to = "stat",
                           values_to = "value") %>%
              mutate(origin = if_else(str_detect(stat, "hatch"),
                                      "H", "W"),
                     type = if_else(str_detect(stat, "_se"),
                                    "spawn_se", "spawn_est")) %>%
              select(-stat) %>%
              pivot_wider(names_from = type,
                          values_from = value)) %>%
  mutate(psm = 1 - (spawn_est / escp)) %>%
  filter(psm < 0)



#-----------------------------------------------------------
# use prs-spawn mortality estimate from Fuchs et al. 2021
# data from WDFW (Nate Fuchs' radio telemetry study)
rt_df = tibble(year = rep(2015:2016, each = 2),
               origin = rep(c("H", "W"), 2),
               ow_fish = c(20, 25, 4, 12),
               surv_fish = c(16, 24, 3, 9)) %>%
  mutate(phi = surv_fish / ow_fish,
         phi_se = sqrt((phi * (1 - phi))/ ow_fish))

# add years together
rt_df %<>%
  bind_rows(rt_df %>%
              group_by(origin) %>%
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

mainstm_spawn_rt <- main_escp %>%
  filter(area != "Wenatchee Subbasin") %>%
  left_join(rt_df %>%
              filter(year == "Total") %>%
              select(origin,
                     contains("phi"))) %>%
  rowwise() %>%
  mutate(spawn = est * phi,
         spawn_se = deltamethod(~ x1 * x2,
                                mean = c(est,
                                         phi),
                                cov = diag(c(se,
                                             phi_se)^2))) %>%
  ungroup() %>%
  select(year, location = area, origin,
         spawn, spawn_se) %>%
  pivot_wider(names_from = origin,
              values_from = c(spawn, spawn_se)) %>%
  rename(wild_spawn = spawn_W,
         hatch_spawn = spawn_H,
         wild_spawn_se = spawn_se_W,
         hatch_spawn_se = spawn_se_H) %>%
  mutate(tot_spawn = wild_spawn + hatch_spawn) %>%
  relocate(tot_spawn,
           .after = location)

mainstm_spawn_rt %<>%
  bind_rows(mainstm_spawn_rt %>%
  group_by(year) %>%
  summarize(across(ends_with("spawn"),
                   sum),
            across(ends_with("spawn_se"),
                   ~ sqrt(sum(.^2))),
            .groups = "drop") %>%
  add_column(location = "Total Mainstem",
             .after = "year")) %>%
  arrange(year, location)

# add 2020 estimates to overall Wenatchee estimates
wen_est %<>%
  bind_rows(mainstm_spawn_rt %>%
              select(year,
                     contains("spawn")) %>%
              mutate(tot_spawn_se = sqrt(sum(c(wild_spawn_se,
                                               hatch_spawn_se)^2))) %>%
              bind_rows(spawner_est_trib %>%
                          select(year,
                                 contains("spawn"))) %>%
              filter(year == 2020) %>%
              group_by(year) %>%
              summarize(across(ends_with("spawn"),
                               sum,
                               na.rm = T),
                        across(ends_with("se"),
                               ~ sqrt(sum(.^2, na.rm = T))),
                        .groups = "drop")) %>%
  arrange(year)

#-------------------------------------------------------
# save various components
#-------------------------------------------------------

save_list = list("Wenatchee Spawners" = wen_est,
                 "Trib Spawners - PIT tag" = spawner_est_trib %>%
                   select(-area),
                 "Trib Spawner All" = spawner_est_trib %>%
                   select(-area) %>%
                   bind_rows(spawner_est_trib_below_arrays %>%
                               select(year, River,
                                      Reach,
                                      redd_add,
                                      contains("spawn"))) %>%
                   group_by(year, River) %>%
                   summarize(across(ends_with("spawn"),
                                    sum,
                                    na.rm = T),
                             across(ends_with("se"),
                                    ~ sqrt(sum(.^2, na.rm = T))),
                             .groups = "drop"),
                 "Mainstem Spawners - split" = spawner_est_main,
                 "Mainstem Spawners - total" = spawner_est_main %>%
                   select(year, River,
                          contains("spawn")) %>%
                   group_by(year, River) %>%
                   summarize(across(ends_with("spawn"),
                                    sum,
                                    na.rm = T),
                             across(ends_with("se"),
                                    ~ sqrt(sum(.^2, na.rm = T))),
                             .groups = "drop"),
                 "Known Removals" = wen_rem,
                 "Mainstem Spawners - RT" = mainstm_spawn_rt)

writexl::write_xlsx(save_list,
                    path = here("outgoing",
                                "Redds_vs_RT_comparison.xlsx"))

# compare estimates from redd-based methods to predictions from RT survival methods
# assume redd-based estimates are "correct"
spawner_est_main %>%
  select(year, location,
         contains("spawn")) %>%
  pivot_longer(contains("spawn"),
               names_to = "stat",
               values_to = "value") %>%
  add_column(model = "Redds",
             .after = "location") %>%
  bind_rows(mainstm_spawn_rt %>%
              filter(location != "Total Mainstem") %>%
              pivot_longer(contains("spawn"),
                           names_to = "stat",
                           values_to = "value") %>%
              add_column(model = "RT",
                         .after = "location")) %>%
  pivot_wider(names_from = model,
              values_from = value) %>%
  filter(!is.na(RT),
         !is.na(Redds)) %>%
  mutate(diff = RT - Redds,
         rel_diff = diff / Redds) %>%
  # # filter(year %in% c(2015, 2016))
  # ggplot(aes(x = stat,
  #            y = rel_diff)) +
  # geom_hline(yintercept = 1,
  #            linetype = 2) +
  # geom_boxplot() +
  # theme(axis.text.x = element_text(angle = 45,
  #                                  hjust = 1)) +
  # scale_y_continuous(trans = "log",
  #                    breaks = scales::pretty_breaks()) +
  # labs(x = "Statistic",
  #      y = "Relative Difference")
  group_by(location, stat) %>%
  summarize(MB = median(diff),
            MAE = mean(abs(diff), na.rm = T),
            MAPE = mean(abs(rel_diff) * 100, na.rm = T),
            MSA = exp(median(abs(log(RT/Redds)), na.rm = T)) * 100,
            RMSE = sqrt(mean(diff^2)),
            .groups = "drop") %>%
  arrange(desc(RMSE))

#-------------------------------------------------------
# read in MSMR estimates
#-------------------------------------------------------
msmr_df <- read_csv("O:Documents/Presentations/2022_CCPUD/FourPeaks/escapement_ests_msmr.csv") %>%
  rename(year = brood_year,
         River = name,
         area = type,
         msmr_est = mean,
         msmr_se = se) %>%
  mutate(across(River,
                recode,
                "LWE to TUF" = "Mainstem below Tumwater",
                "Above TUF" = "Mainstem above Tumwater"))

msmr_df %>%
  filter(lcb < 0)

trib_escp %>%
  group_by(year,
           area,
           River) %>%
  summarize(across(est,
                   sum),
            across(se,
                   ~ sqrt(sum(.^2))),
            .groups = "drop") %>%
  bind_rows(main_escp %>%
              filter(area != "Wenatchee Subbasin") %>%
              rename(River = area) %>%
              group_by(year,
                       # area,
                       River) %>%
              summarize(across(est,
                               sum),
                        across(se,
                               ~ sqrt(sum(.^2))),
                        .groups = "drop") %>%
              mutate(area = "Mainstem")) %>%
  left_join(msmr_df %>%
              filter(area != "Basin")) %>%
  filter(!is.na(msmr_est)) %>%
  ggplot(aes(x = est,
             y = msmr_est)) +
  geom_abline(linetype = 2,
              color = "gray") +
  geom_errorbarh(aes(xmin = qnorm(0.025, est, se),
                     xmax = qnorm(0.975, est, se)),
                 height = 0) +
  geom_errorbar(aes(ymin = qnorm(0.025, msmr_est, msmr_se),
                    ymax = qnorm(0.975, msmr_est, msmr_se)),
                 width = 0) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_wrap(~ River,
             scales = "free") +
  theme_bw() +
  labs(x = "POM",
       y = "MSMR")


spawner_est_main %>%
  select(year,
         River = location,
         contains("tot_spawn")) %>%
  rename(est = tot_spawn,
         se = tot_spawn_se) %>%
  left_join(msmr_df %>%
              filter(area != "Basin")) %>%
  ggplot(aes(x = est,
             y = msmr_est,
             color = as.factor(year))) +
  geom_abline(linetype = 2,
              color = "gray") +
  geom_errorbarh(aes(xmin = qnorm(0.025, est, se),
                     xmax = qnorm(0.975, est, se)),
                 height = 0) +
  geom_errorbar(aes(ymin = qnorm(0.025, msmr_est, msmr_se),
                    ymax = qnorm(0.975, msmr_est, msmr_se)),
                width = 0) +
  geom_point() +
  # geom_smooth(method = lm) +
  facet_wrap(~ River,
             scales = "free") +
  theme_bw() +
  labs(x = "POM",
       y = "MSMR")


wen_est %>%
  filter(year != 2020) %>%
  select(year,
         contains("tot_")) %>%
  rename(est = tot_spawn,
         se = tot_spawn_se) %>%
  left_join(msmr_df %>%
              filter(area != "Basin") %>%
              group_by(year) %>%
              summarize(across(msmr_est,
                               sum),
                        across(msmr_se,
                               ~ sqrt(sum(.^2))))) %>%
  ggplot(aes(x = est,
             y = msmr_est,
             color = as.factor(year))) +
  geom_abline(linetype = 2,
              color = "gray") +
  geom_errorbarh(aes(xmin = qnorm(0.025, est, se),
                     xmax = qnorm(0.975, est, se)),
                 height = 0) +
  geom_errorbar(aes(ymin = qnorm(0.025, msmr_est, msmr_se),
                    ymax = qnorm(0.975, msmr_est, msmr_se)),
                width = 0) +
  geom_point() +
  theme_bw() +
  coord_equal() +
  labs(x = "POM",
       y = "MSMR")

wen_est %>%
  filter(year != 2020) %>%
  select(year,
         contains("tot_")) %>%
  rename(est = tot_spawn,
         se = tot_spawn_se) %>%
  left_join(msmr_df %>%
              filter(area != "Basin") %>%
              group_by(year) %>%
              summarize(across(msmr_est,
                               sum),
                        across(msmr_se,
                               ~ sqrt(sum(.^2))))) %>%
  mutate(bias = msmr_est - est,
         rel_bias = bias / est)
