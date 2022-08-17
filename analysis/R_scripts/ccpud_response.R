# Author: Kevin See
# Purpose: Generate estimates of mainstem spawners
# Created: 8/16/22
# Last Modified: 8/16/2022
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
library(ggrepel)

#-----------------------------------------------------------------
data("removal_df")

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
  select(-ends_with("fish"))

main_spwn_df <- recent_spwn_redd %>%
  # bind_rows(spwn_org_redd) %>%
  filter(str_detect(location, "TUM")) %>%
  group_by(year) %>%
  summarize(across(c(obs_redds,
                     redd_est,
                     # spawn_est,
                     hor_spwn,
                     nor_spwn),
                   sum),
            across(c(#redd_se,
                     # spawn_se,
                     hor_se,
                     nor_se),
                   ~ sqrt(sum(.^2))),
            # across(c(fpr,
            #          fpr_se,
            #          phos,
            #          phos_se),
            #        mean),
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
  inner_join(main_spwn_df) %>%
  rowwise() %>%
  mutate(main_rt_spwn = ((lwe_fish - rem) * phi) - trib_spwn,
         main_rt_se = msm::deltamethod(~ ((x1 - x2) * x3) - x4,
                                    mean = c(lwe_fish,
                                             rem,
                                             phi,
                                             trib_spwn),
                                    cov = diag(c(lwe_se,
                                                 0,
                                                 phi_se,
                                                 trib_se)^2))) %>%
  mutate(ows = (main_redd_spwn + trib_spwn) / (lwe_fish - rem),
         ows_se = msm::deltamethod(~ (x1 + x2) / (x3 - x4),
                                   mean = c(main_redd_spwn,
                                            trib_spwn,
                                            lwe_fish,
                                            rem),
                                   cov = diag(c(main_redd_se,
                                                trib_se,
                                                lwe_se,
                                                0)^2))) %>%
  ungroup() %>%
  mutate(across(ows,
                ~ if_else(. > 1, 1, .)))

comp_df %>%
  select(year, origin,
         lwe_fish,
         rem,
         trib_spwn,
         main_redd_spwn,
         contains("ows"),
         contains("phi"))

pd = 0.4
comp_df %>%
  rowwise() %>%
  mutate(ows_min = qnorm(0.025, ows, ows_se),
         ows_max = qnorm(0.975, ows, ows_se)) %>%
  mutate(across(ows_max,
                ~ if_else(. > 1, 1, .))) %>%
  ungroup() %>%
  ggplot(aes(x = year,
             y = ows,
             color = origin)) +
  geom_ribbon(data = phi_df %>%
                mutate(phi_min = qnorm(0.025, phi, phi_se),
                       phi_max = qnorm(0.975, phi, phi_se)) %>%
                tidyr::crossing(year = unique(comp_df$year)),
              aes(ymin = phi_min,
                  ymax = phi_max,
                  y = phi,
                  fill = origin),
              color = NA,
              alpha = 0.3) +
  geom_hline(data = phi_df,
             aes(yintercept = phi,
                 color = origin),
             linetype = 2) +
  geom_errorbar(aes(ymin = ows_min,
                    ymax = ows_max),
                position = position_dodge(pd),
                width = 0) +
  geom_point(size = 3,
             position = position_dodge(pd)) +
  scale_color_brewer(palette = "Set1",
                     name = "Origin") +
  scale_fill_brewer(palette = "Set1",
                    name = "Origin") +
  # scale_color_viridis_d(begin = 0.1,
  #                       end = 0.6,
  #                       name = "Origin") +
  # scale_fill_viridis_d(begin = 0.1,
  #                       end = 0.6,
  #                       name = "Origin") +
  labs(x = "Year",
       y = "Overwinter Survival")

ggsave(here("analysis/figures/ccpud_response",
            "overwinter_survival.png"),
       width = 6,
       height = 6)

comp_df %>%
  ggplot(aes(x = main_redd_spwn,
             y = main_rt_spwn,
             color = origin)) +
  geom_abline(linetype = 2) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = year)) +
  scale_color_brewer(palette = "Set1",
                     name = "Origin") +
  labs(title = "Mainstem Wenatchee Steelhead Spawners",
       x = "Redd-based",
       y = "RT based")

ggsave(here("analysis/figures/ccpud_response",
            "mainstem_spawners.png"),
       width = 6,
       height = 6)


pd = 0.1
comp_df %>%
  select(year, origin,
         contains("main")) %>%
  pivot_longer(contains("main"),
               names_to = "source") %>%
  mutate(model = if_else(str_detect(source, "redd"),
                         "Redd",
                         "RT"),
         type = if_else(str_detect(source, "se"),
                        "se",
                        "est")) %>%
  select(-source) %>%
  pivot_wider(names_from = type,
              values_from = value) %>%
  ggplot(aes(x = year,
             y = est,
             color = model)) +
  geom_ribbon(aes(ymin = qnorm(0.025, est, se),
                  ymax = qnorm(0.975, est, se),
                  fill = model),
              color = NA,
              position = position_dodge(pd),
              alpha = 0.3) +
  geom_line(position = position_dodge(pd)) +
  geom_point(size = 3,
             position = position_dodge(pd)) +
  facet_wrap(~ origin,
             ncol = 1) +
  labs(title = "Wenatchee Mainstem Spawners",
       x = "Year",
       y = "Estimate")

comp_df %>%
  mutate(redd_cv = main_redd_se / main_redd_spwn,
         rt_cv = main_rt_se / main_rt_spwn) %>%
  group_by(origin) %>%
  summarize(across(ends_with("cv"),
                   median))

comp_df %>%
  mutate(poss_main = lwe_fish - rem - trib_spwn,
         poss_se = sqrt(lwe_se^2 + trib_se^2)) %>%
  ggplot(aes(poss_main,
             main_redd_spwn)) +#,
             # color = origin,
             # fill = origin)) +
  geom_abline(linetype = 2) +
  geom_point(size = 3) +
  geom_smooth(method = lm,
              formula = y ~ x - 1,
              se = F,
              alpha = 0.2) +
  facet_wrap(~ origin,
             ncol = 1) +
  geom_text_repel(aes(label = year)) +
  coord_equal() +
  labs(x = "Possible Mainstem Spawners",
       y = "Estimated Mainstem Spawners")

ggsave(here("analysis/figures/ccpud_response",
            "poss_vs_est_mainstem_spawners.png"),
       width = 5,
       height = 7)


comp_df %>%
  mutate(poss_main = lwe_fish - rem - trib_spwn,
         poss_se = sqrt(lwe_se^2 + trib_se^2)) %>%
  lm(main_redd_spwn ~ poss_main:origin - 1,
     data = .) %>%
  summary()

comp_df %>%
  group_by(origin) %>%
  summarize(across(ows,
                   list(mean = mean,
                        median = median)))


all_redds %>%
  filter(river == "Wenatchee",
         method %in% c("Index",
                       "Non-Index")) %>%
  group_by(year, method) %>%
  summarize(across(c(obs_redds,
                     redd_est),
                   sum),
            .groups = "drop") %>%
  group_by(year) %>%
  mutate(across(c(obs_redds,
                  redd_est),
                ~ . / sum(.))) %>%
  group_by(method) %>%
  summarize(across(c(obs_redds,
                     redd_est),
                   mean))
