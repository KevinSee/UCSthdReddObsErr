# Author: Kevin See
# Purpose: respond to CCPUD proposal
# Created: 8/16/22
# Last Modified: 10/12/2022
# Notes: run Wen_2004-2021_prep.Rmd up to the Results section first

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
library(writexl)

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


phi_df2 <- rt_df %>%
  mutate(across(Origin,
                recode,
                "Hatchery" = "hor",
                "Natural" = "nor")) %>%
  rename(origin = Origin) %>%
  group_by(year, origin) %>%
  summarize(across(c(ow_fish, surv_fish),
                   sum),
            .groups = "drop") %>%
  mutate(phi = surv_fish / ow_fish,
         phi_se = sqrt((phi * (1 - phi))/ ow_fish)) %>%
  filter(year != "Total") %>%
  group_by(origin) %>%
  summarize(across(phi,
                   mean),
            across(phi_se,
                   ~ sqrt(mean(.^2))))

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
            across(c(redd_se,
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
  mutate(ows = (main_redd_spwn + trib_spwn) / (lwe_escp),
         ows_se = msm::deltamethod(~ (x1 + x2) / x3,
                                   mean = c(main_redd_spwn,
                                            trib_spwn,
                                            lwe_fish),
                                   cov = diag(c(main_redd_se,
                                                trib_se,
                                                lwe_se)^2)),
         psm = (lwe_escp - trib_spwn - main_redd_spwn) / lwe_escp,
         psm_se = msm::deltamethod(~ (x1 - x2 - x3) / x1,
                                   mean = c(lwe_escp,
                                            trib_spwn,
                                            main_redd_spwn),
                                   cov = diag(c(lwe_se,
                                                trib_se,
                                                main_redd_se)^2))) %>%
  ungroup()# %>%
  # mutate(across(ows,
  #               ~ if_else(. > 1, 1, .)))

comp_df %>%
  select(year, origin,
         lwe_fish,
         rem,
         trib_spwn,
         main_redd_spwn,
         contains("ows"),
         contains("psm"),
         contains("phi"))

comp_df %>%
  mutate(gut_check = (main_redd_spwn + trib_spwn) / lwe_escp) %>%
  filter(gut_check > 1) %>%
  as.data.frame()


comp_df %>%
  filter(psm < 0) %>%
  select(year,
         origin,
         contains("psm")) %>%
  mutate(psm_lci = qnorm(0.025, psm, psm_se),
         psm_uci = qnorm(0.975, psm, psm_se))

comp_df %>%
  mutate(lwe_escp = lwe_fish - rem,
         tot_spwn = trib_spwn + main_redd_spwn,
         psm = (lwe_fish - rem - tot_spwn) / lwe_fish) %>%
  qplot(psm, data = ., fill = origin)

#-----------------------------------------------------------------
# save data/estimates in a spreadsheet to share with CCPUD
save_list <- list(
  "Wenatchee Total Escp" =
    comp_df %>%
    select(year:rem, lwe_escp) %>%
    mutate(across(c(lwe_fish,
                    lwe_escp),
                  round,
                  1),
           across(ends_with("se"),
                  round,
                  2)),
  "Tributary Total Escp" =
    comp_df %>%
    select(year:origin,
           starts_with("trib")) %>%
    mutate(across(c(trib_spwn),
                  round,
                  1),
           across(ends_with("se"),
                  round,
                  2)),
  "Mainstem Redds" =
    comp_df %>%
    select(year,
           obs_redds:redd_se) %>%
    distinct() %>%
    mutate(across(ends_with("se"),
                  round,
                  2)),
  "Mainstem Spawners - Redds" =
    comp_df %>%
    select(year:origin,
           main_redd_spwn, main_redd_se) %>%
    mutate(lci = qnorm(0.025, main_redd_spwn, main_redd_se),
           uci = qnorm(0.975, main_redd_spwn, main_redd_se)) %>%
    mutate(across(main_redd_spwn,
                  round,
                  1),
           across(c(ends_with("se"),
                    ends_with("ci")),
                  round,
                  2)),
  "Mainstem Spawners - COVID" =
    comp_df %>%
    select(year:origin,
           phi, phi_se,
           main_rt_spwn, main_rt_se) %>%
    mutate(lci = qnorm(0.025, main_rt_spwn, main_rt_se),
           uci = qnorm(0.975, main_rt_spwn, main_rt_se)) %>%
    mutate(across(main_rt_spwn,
                  round,
                  1),
           across(c(ends_with("se"),
                    ends_with("ci")),
                  round,
                  2)),
  "Wenatchee Total Spawners-Redds" =
    comp_df %>%
    mutate(all_spwn_redd = trib_spwn + main_redd_spwn,
           all_spwn_redd_se = sqrt(trib_se^2 + main_redd_se^2)) %>%
    select(year:origin,
           starts_with("all_spwn")) %>%
    mutate(all_spwn_redd_cv = all_spwn_redd_se / all_spwn_redd) %>%
    mutate(lci = qnorm(0.025, all_spwn_redd , all_spwn_redd_se),
           uci = qnorm(0.975, all_spwn_redd , all_spwn_redd_se)) %>%
    mutate(across(all_spwn_redd,
                  round,
                  1),
           across(c(ends_with("se"),
                    ends_with("cv"),
                    ends_with("ci")),
                  round,
                  2)),
  "Wenatchee Total Spawners-COVID" =
    comp_df %>%
    mutate(all_spwn_rt = trib_spwn + main_rt_spwn,
           all_spwn_rt_se = sqrt(trib_se^2 + main_rt_se^2)) %>%
    select(year:origin,
           starts_with("all_spwn")) %>%
    mutate(all_spwn_rt_cv = all_spwn_rt_se / all_spwn_rt) %>%
    mutate(lci = qnorm(0.025, all_spwn_rt , all_spwn_rt_se),
           uci = qnorm(0.975, all_spwn_rt , all_spwn_rt_se)) %>%
    mutate(across(all_spwn_rt,
                  round,
                  1),
           across(c(ends_with("se"),
                    ends_with("cv"),
                    ends_with("ci")),
                  round,
                  2))
)

write_xlsx(save_list,
           path = here("outgoing",
                       "MethodsComparison.xlsx"))

#-----------------------------------------------------------------

pd = 0.4
comp_df %>%
  rowwise() %>%
  mutate(ows_min = qnorm(0.025, ows, ows_se),
         ows_max = qnorm(0.975, ows, ows_se)) %>%
  # mutate(across(ows_max,
  #               ~ if_else(. > 1, 1, .))) %>%
  ungroup() %>%
  ggplot(aes(x = year,
             y = ows,
             color = origin)) +
  geom_hline(yintercept = 1,
             linetype = 3) +
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
  select(year,
         origin,
         contains("psm")) %>%
  mutate(psm_lci = qnorm(0.025, psm, psm_se),
         psm_uci = qnorm(0.975, psm, psm_se)) %>%
  ggplot(aes(x = year,
             y = psm,
             color = origin)) +
  geom_hline(yintercept = c(0),
             linetype = 3) +
  geom_errorbar(aes(ymin = psm_lci,
                    ymax = psm_uci),
                position = position_dodge(pd),
                width = 0) +
  geom_point(size = 3,
             position = position_dodge(pd)) +
  scale_color_brewer(palette = "Set1",
                     name = "Origin") +
  labs(x = "Year",
       y = "Prespawn Mortality")

ggsave(here("analysis/figures/ccpud_response",
            "prespawn_mortality.png"),
       width = 6,
       height = 6)

comp_df %>%
  ggplot(aes(x = psm,
             fill = origin,
             color = origin)) +
  geom_histogram(binwidth = 0.05,
                 position = "dodge")

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
  geom_text_repel(aes(label = year)) +
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


crossing(lwe_escp = c(600, 2000),
         phi = c(0.15, 0.2, 0.3, 0.4)) %>%
  mutate(tot_spwn = lwe_escp - (lwe_escp * phi),
         main_spwn = tot_spwn * 0.2,
         trib_spwn = tot_spwn - main_spwn) %>%
  mutate(run_size = if_else(lwe_escp < 800,
                            "small",
                            "large")) %>%
  # ggplot(aes(x = phi,
  #            y = tot_spwn,
  #            color = run_size)) +
  # geom_line()
  group_by(run_size) %>%
  nest() %>%
  mutate(mod = map(data,
                   .f = function(x) {
                     lm(tot_spwn ~ phi,
                        data = x)
                   }),
         coef = map(mod,
                    .f = function(x) broom::tidy(x))) %>%
  unnest(coef)






pd = 0.1
comp_df %>%
  mutate(all_spwn_redd = trib_spwn + main_redd_spwn,
         all_spwn_redd_se = sqrt(trib_se^2 + main_redd_se^2)) %>%
  mutate(across(origin,
                recode,
                "hor" = "Hatchery",
                "nor" = "Natural")) %>%
  select(year,
         origin,
         lwe_escp, lwe_se,
         contains("all_spwn_redd")) %>%
  pivot_longer(-c(year, origin)) %>%
  mutate(type = if_else(str_detect(name, "se"),
                        "se",
                        "est"),
         source = if_else(str_detect(name, "spwn"),
                          "Spawners",
                          "Escapement")) %>%
  select(-name) %>%
  pivot_wider(names_from = type,
              values_from = value) %>%
  bind_rows(spwn_2020 %>%
              pivot_longer(-year) %>%
              mutate(origin = if_else(str_detect(name, "nor"),
                                      'Natural',
                                      'Hatchery'),
                     type = if_else(str_detect(name, "se"),
                                    "se",
                                    "est")) %>%
              select(-name) %>%
              pivot_wider(names_from = type,
                          values_from = value) %>%
              add_column(source = "Spawners",
                         .after = "origin") %>%
              bind_rows(pit_est %>%
                          filter(year == 2020,
                                 location == "LWE") %>%
                          select(year,
                                 origin,
                                 est = mean,
                                 se = sd) %>%
                          mutate(across(origin,
                                        recode,
                                        "H" = "Hatchery",
                                        "W" = "Natural")) %>%
                          left_join(rem_df %>%
                                      mutate(across(origin,
                                                    recode,
                                                    "hor" = "Hatchery",
                                                    "nor" = "Natural"))) %>%
                          mutate(est = est - rem) %>%
                          select(-rem) %>%
                          add_column(source = "Escapement",
                                     .after = "origin"))) %>%
  arrange(year,
          source,
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
  facet_wrap(~ origin,
             scales = "free_y",
             ncol = 1) +
  scale_color_viridis_d(name = "Source",
                        end = 0.8) +
  labs(x = "Year",
       y = "Estimate",
       title = "Wenatchee Population")

ggsave(here("analysis/figures/ccpud_response",
            "escp_vs_spawners.png"),
       width = 7,
       height = 7)

comp_df %>%
  mutate(poss_main = lwe_fish - rem - trib_spwn,
         poss_se = sqrt(lwe_se^2 + trib_se^2)) %>%
  select(year,
         origin,
         starts_with("poss"),
         starts_with("main_redd")) %>%
  pivot_longer(-c(year, origin)) %>%
  mutate(type = if_else(str_detect(name, "se"),
                        "se",
                        "est"),
         source = if_else(str_detect(name, "main_redd"),
                          "Spawners",
                          "Escapement")) %>%
  select(-name) %>%
  pivot_wider(names_from = type,
              values_from = value) %>%
  mutate(across(origin,
                recode,
                "hor" = "Hatchery",
                "nor" = "Natural")) %>%
  bind_rows(spwn_main_2020 %>%
              pivot_longer(-year) %>%
              mutate(origin = if_else(str_detect(name, "nor"),
                                      'Natural',
                                      'Hatchery'),
                     type = if_else(str_detect(name, "se"),
                                    "se",
                                    "est")) %>%
              select(-name) %>%
              pivot_wider(names_from = type,
                          values_from = value) %>%
              add_column(source = "Spawners",
                         .after = "origin") %>%
              bind_rows(pit_est %>%
                          filter(year == 2020,
                                 location %in% c("LWE_bb",
                                                 "TUM_bb")) %>%
                          group_by(year,
                                   origin) %>%
                          summarize(across(mean,
                                           sum),
                                    across(sd,
                                           ~ sqrt(sum(.^2)))) %>%
                          select(year,
                                 origin,
                                 est = mean,
                                 se = sd) %>%
                          mutate(across(origin,
                                        recode,
                                        "H" = "Hatchery",
                                        "W" = "Natural")) %>%
                          left_join(rem_df %>%
                                      mutate(across(origin,
                                                    recode,
                                                    "hor" = "Hatchery",
                                                    "nor" = "Natural"))) %>%
                          mutate(est = est - rem) %>%
                          select(-rem) %>%
                          add_column(source = "Escapement",
                                     .after = "origin"))) %>%
  arrange(year,
          source,
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
  facet_wrap(~ origin,
             scales = "free_y",
             ncol = 1) +
  scale_color_viridis_d(name = "Source",
                        end = 0.8) +
  labs(x = "Year",
       y = "Estimate",
       title = "Wenatchee Mainstem")

ggsave(here("analysis/figures/ccpud_response",
            "escp_vs_spawners_main.png"),
       width = 7,
       height = 7)

pd = 0.1
comp_df %>%
  mutate(all_spwn_redd = trib_spwn + main_redd_spwn,
         all_spwn_redd_se = sqrt(trib_se^2 + main_redd_se^2),
         all_spwn_rt = trib_spwn + main_rt_spwn,
         all_spwn_rt_se = sqrt(trib_se^2 + main_rt_se^2)) %>%
  mutate(across(origin,
                recode,
                "hor" = "Hatchery",
                "nor" = "Natural")) %>%
  select(year,
         origin,
         # lwe_escp, lwe_se,
         starts_with("all_spwn")) %>%
  pivot_longer(-c(year, origin)) %>%
  mutate(type = if_else(str_detect(name, "se"),
                        "se",
                        "est"),
         source = if_else(str_detect(name, "redd"),
                          "Redd-based",
                          "RT-based")) %>%
  select(-name) %>%
  pivot_wider(names_from = type,
              values_from = value) %>%
  arrange(year,
          source,
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
  facet_wrap(~ origin,
             scales = "free_y",
             ncol = 1) +
  scale_color_viridis_d(name = "Source",
                        end = 0.8) +
  labs(x = "Year",
       y = "Estimate",
       title = "Wenatchee Population")

ggsave(here("analysis/figures/ccpud_response",
            "wen_pop_spwn_comp.png"),
       width = 7,
       height = 7)


pd = 0.1
comp_df %>%
  mutate(across(origin,
                recode,
                "hor" = "Hatchery",
                "nor" = "Natural")) %>%
  select(year,
         origin,
         starts_with("main_")) %>%
  pivot_longer(-c(year, origin)) %>%
  mutate(type = if_else(str_detect(name, "se"),
                        "se",
                        "est"),
         source = if_else(str_detect(name, "redd"),
                          "Redd-based",
                          "RT-based")) %>%
  select(-name) %>%
  pivot_wider(names_from = type,
              values_from = value) %>%
  arrange(year,
          source,
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
  facet_wrap(~ origin,
             scales = "free_y",
             ncol = 1) +
  scale_color_viridis_d(name = "Source",
                        end = 0.8) +
  labs(x = "Year",
       y = "Estimate",
       title = "Wenatchee Mainstem")

ggsave(here("analysis/figures/ccpud_response",
            "wen_main_spwn_comp.png"),
       width = 7,
       height = 7)




comp_df %>%
  mutate(total_redd_spwn = main_redd_spwn + trib_spwn,
         total_rt_spwn = main_rt_spwn + trib_spwn) %>%
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
                        "Total"),
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
                "nor" = "Wild")) %>%
  ggplot(aes(x = origin,
             y = rel_bias,
             fill = origin)) +
  # geom_boxplot() +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  stat_summary(fun = "mean",
               geom = "point",
               shape = 17,
               size = 4,
               color = "blue") +
  geom_point(size = 2,
             position = position_jitter(width = 0.1)) +
  geom_hline(yintercept = 0,
             linetype = 2,
             lwd = 1) +
  scale_fill_brewer(palette = "Set1",
                    name = "Origin") +
  facet_wrap(~ type) +
  theme(legend.position = "bottom") +
  labs(x = "Origin",
       y = "Relative Bias",
       title = "Wenatchee Steelhead Spawners")

ggsave(here("analysis/figures/ccpud_response",
            "rel_bias_main_total.png"),
       width = 7,
       height = 7)



comp_df %>%
  mutate(perc_main = main_redd_spwn / (main_redd_spwn + trib_spwn)) %>%
  ggplot(aes(x = year,
             y = perc_main * 100,
             color = origin)) +
  geom_line() +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set1",
                     name = "Origin") +
  labs(x = "Year",
       y = "Percent Mainstem Spawners")

ggsave(here("analysis/figures/ccpud_response",
            "perc_main_spwn.png"),
       width = 7,
       height = 7)



comp_df %>%
  mutate(total_redd_spwn = main_redd_spwn + trib_spwn,
         total_rt_spwn = main_rt_spwn + trib_spwn) %>%
  mutate(main_bias = main_rt_spwn - main_redd_spwn,
         total_bias = total_rt_spwn - total_redd_spwn,
         main_rel_bias = main_bias / main_redd_spwn,
         main_abs_bias = abs(main_bias),
         total_rel_bias = total_bias / total_redd_spwn,
         total_abs_bias = abs(total_bias)) %>%
  select(year, origin,
         contains("spwn"),
         contains("bias")) %>%
  mutate(perc_main = main_redd_spwn / total_redd_spwn) %>%
  pivot_longer(c(contains("main"),
                 contains("total"),
                 -perc_main)) %>%
  mutate(type = if_else(str_detect(name, "main"),
                        "main",
                        "total"),
         across(name,
                str_remove,
                "^main_"),
         across(name,
                str_remove,
                "^total_")) %>%
  pivot_wider() %>%
  group_by(type,
           origin) %>%
  summarize(rmse = sqrt(mean(bias^2)),
            mae = mean(abs_bias),
            mape = mean(abs_bias / redd_spwn) * 100,
            across(c(perc_main,
                     bias,
                     rel_bias,
                     abs_bias),
                   mean))


trib_spawners_all %>%
  pivot_longer(c(contains("hor_"),
                 contains("nor_"))) %>%
  mutate(origin = if_else(str_detect(name, "hor_"),
                          "hor",
                          "nor"),
         across(name,
                str_remove,
                "^hor_"),
         across(name,
                str_remove,
                "^nor_")) %>%
  pivot_wider() %>%
  filter(year %in% unique(comp_df$year)) %>%
  bind_rows(comp_df %>%
              select(year, origin,
                     spwn = main_redd_spwn,
                     se = main_redd_se) %>%
              add_column(river = "Mainstem",
                         .before = 0)) %>%
  mutate(across(origin,
                recode,
                "hor" = "Hatchery",
                "nor" = "Wild")) %>%
  group_by(year, origin) %>%
  mutate(spwn_perc = spwn / sum(spwn)) %>%
  ggplot(aes(x = river,
             y = spwn_perc,
             fill = river)) +
  geom_boxplot() +
  stat_summary(fun = "mean",
               geom = "point",
               shape = 16,
               color = "blue") +
  facet_wrap(~ origin,
             nrow = 1) +
  scale_fill_brewer(palette = "Set3",
                    name = "River") +
  theme(axis.text.x = element_blank()) +
  labs(x = "Origin",
       y = "Percent of Spawners")


ggsave(here("analysis/figures/ccpud_response",
            "perc_spwn_rivers.png"),
       width = 7,
       height = 7)
