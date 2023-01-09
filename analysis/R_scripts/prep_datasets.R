# Author: Kevin See
# Purpose: Prepare some datasets for inclusion in the package
# Created: 5/4/2020
# Last Modified: 12/2/2022
# Notes: This data was sent by Michael Hughes
# comparison of Priest biological data was compiled by Charlie Snow (Wells Hatchery) and Katy Shelby (Wenatchee broodstock)

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(magrittr)
library(readxl)
library(usethis)
library(here)
library(janitor)
library(DescTools)

#-----------------------------------------------------------------
# removals at various sources, by year
removal_df = read_excel(here('analysis/data/raw_data/2014 to 2019 STHD Removals_Harvest and Brood Collected.xlsx'),
                    range = "A3:H9") %>%
  rename(Year = "...1") %>%
  gather(label, rem, -Year) %>%
  mutate(Origin = str_split(label, "\\.", simplify = T)[,1],
         Source = recode(label,
                         "Hatchery...2" = "Harvest",
                         "Natural...3" = "Harvest",
                         "Hatchery...4" = "Dryden",
                         "Natural...5" = "Dryden",
                         "Hatchery...6" = "Tumwater",
                         "Natural...7" = "Tumwater",
                         "Hatchery...8" = "Tumwater")) %>%
  group_by(Year, Source, Origin) %>%
  summarise_at(vars(rem),
               list(sum)) %>%
  ungroup() %>%
  mutate(Area = recode(Source,
                       'Tumwater' = 'TUM_bb',
                       'Dryden' = 'Below_TUM'))

# add on 2020
removal_2020 = removal_df %>%
  select(Source, Origin, Area) %>%
  distinct() %>%
  mutate(Year = 2020) %>%
  mutate(rem = c(22+17,
                 16+17,
                 0, 0,
                 12+12,
                 18+15)) %>%
  select(all_of(names(removal_df)))

removal_df %<>%
  bind_rows(removal_2020) %>%
  distinct()

# add on 2021
removal_2021 = read_excel(here("analysis/data/raw_data",
                               "STHD 2021 Spawn Year Removals.xlsx"),
                          1,
                          skip = 1) %>%
  mutate(Year = 2021) %>%
  filter(Location != "Grand Total") %>%
  rename(Source = Location) %>%
  mutate(across(Source,
                fct_recode,
                "Dryden" = "WEN-DRY",
                "Tumwater" = "WEN-TUM")) %>%
  select(-`Grand Total`) %>%
  pivot_longer(c(Hatchery, Wild),
               names_to = "Origin",
               values_to = "rem") %>%
  mutate(across(Origin,
                fct_recode,
                "Natural" = "Wild")) %>%
  mutate(Area = recode(Source,
                       'Tumwater' = 'TUM_bb',
                       'Dryden' = 'Below_TUM')) %>%
  select(any_of(names(removal_df)))

removal_df %<>%
  bind_rows(removal_2021) %>%
  distinct()


#-----------------------------------------------------------------
# add removals from the Methow in 2021
rem_met_2021 <- read_excel(here("analysis/data/raw_data",
                                "Net Methow removal.xlsx"),
                           1,
                           skip = 3) %>%
  select(Source = Location,
         Hatchery = HOR,
         Natural = NOR) %>%
  filter(!is.na(Source),
         !is.na(Hatchery)) %>%
  mutate(Area = recode(Source,
                       "Methow mainstem" = "Lower Methow",
                       "Twisp weir" = "Twisp",
                       "WNFH hatchery trap" = "Methow Fish Hatchery")) %>%
  pivot_longer(cols = c(Hatchery,
                        Natural),
               names_to = "Origin",
               values_to = "rem") %>%
  add_column(Year = 2021,
             .before = 0)

removal_df %<>%
  add_column(Subbasin = "Wenatchee",
             .before = 0) %>%
  bind_rows(rem_met_2021 %>%
              add_column(Subbasin = "Methow",
                         .before = 0))

#-----------------------------------------------------------------
# add removals from the Wenatchee in 2022
rem_wen_2022 <- crossing(Source = c("Dryden", "Tumwater"),
         Origin = c("Hatchery", "Natural")) %>%
  add_column(Subbasin = "Wenatchee",
             Year = 2022,
             .before = 1) %>%
  mutate(rem = c(16, 0,
                 32, 51)) %>%
  mutate(Area = recode(Source,
                       'Tumwater' = 'TUM_bb',
                       'Dryden' = 'Below_TUM'))

removal_df %<>%
  bind_rows(rem_wen_2022)

#-----------------------------------------------------------------
# add removals from the Methow in 2022
rem_met_2022 <- read_excel(here("analysis/data/raw_data",
                                "Net Methow removals in 2022.xlsx"),
                           1,
                           range = "B4:E8") %>%
  mutate(across(where(is.numeric),
                replace_na,
                0)) %>%
  select(Source = Location,
         Hatchery = HOR,
         Natural = NOR) %>%
  mutate(Area = recode(Source,
                       "Methow mainstem" = "Lower Methow",
                       "DCPUD to Wells" = "Lower Methow",
                       "Twisp weir" = "Twisp",
                       "WNFH hatchery trap" = "Methow Fish Hatchery")) %>%
  pivot_longer(cols = c(Hatchery,
                        Natural),
               names_to = "Origin",
               values_to = "rem") %>%
  add_column(Subbasin = "Methow",
             Year = 2022,
             .before = 0) %>%
  select(any_of(names(removal_df)))

removal_df %<>%
  bind_rows(rem_met_2022)



#-----------------------------------------------------------------
# save for use in package
usethis::use_data(removal_df,
                  overwrite = T)


#-----------------------------------------------------------------
# mean thalweg CV, using all measurements across years
thlwg_summ = read_excel('analysis/data/raw_data/Master_STHD Thalwegs.xlsx',
                        skip = 1) %>%
  rename(Year = `...1`) %>%
  mutate(across(Year,
                as.integer)) %>%
  filter(!is.na(Year)) %>%
  mutate(across(-Year,
                as.numeric)) %>%
  pivot_longer(W1:W10,
               names_to = "Reach",
               values_to = "CV") %>%
  group_by(Reach) %>%
  summarise(n_yrs = n_distinct(Year[!is.na(CV)]),
            n_meas = sum(!is.na(CV)),
            MeanThalwegCV = mean(CV, na.rm = T)) %>%
  mutate(across(MeanThalwegCV,
                ~ . * 100)) %>%
  mutate(MeanThalwegCV = if_else(Reach == 'W4',
                                 MeanThalwegCV[Reach =="W5"],
                                 MeanThalwegCV)) %>%
  ungroup() %>%
  mutate(Reach = fct_relevel(Reach, 'W10', after = Inf)) %>%
  arrange(Reach)

#-----------------------------------------------------------------
# save for use in package
usethis::use_data(thlwg_summ,
                  overwrite = T)


#-----------------------------------------------------------------
# comparison of sex, origin and mark calls at Priest vs some broodstock collections
pra_call_comp <- read_excel(here("analysis/data/raw_data",
                           "Wells-Priest steel comparison.xlsx"),
                      skip = 1) %>%
  clean_names() %>%
  select(brood_year:mark_9) %>%
  mutate(across(ends_with("year"),
                as.integer)) %>%
  pivot_longer(cols = sex_4:mark_9) %>%
  mutate(category = str_remove(name, "_[:digit:]$"),
         col_num = str_extract(name, "[:digit:]"),
         across(col_num,
                as.integer),
         location = if_else(between(col_num, 4, 6),
                            "Wells",
                            "Priest")) %>%
  select(-name, -col_num) %>%
  distinct() %>%
  pivot_wider(names_from = location,
              values_from = value) %>%
  add_column(qa_loc = "Wells Hatchery",
             .after = "run_year") %>%
  rename(Truth = Wells) %>%
  relocate(Truth, .after = last_col()) %>%
  # add data from the Wenatchee for a few years
  bind_rows(read_excel(here("analysis/data/raw_data",
                            "Priest-Wen Steelhead Comparison Brood 2021-23_010423.xlsx"),
                       skip = 1) %>%
              clean_names() %>%
              select(brood_year:mark_11) %>%
              mutate(across(c(starts_with("sex"),
                              starts_with("origin"),
                              starts_with("mark")),
                            ~ if_else(. == "NA",
                                      NA_character_,
                                      .))) %>%
              mutate(across(ends_with("year"),
                            as.integer)) %>%
              pivot_longer(cols = c(starts_with("sex"),
                                    starts_with("origin"),
                                    starts_with("mark"))) %>%
              mutate(category = str_remove(name, "_[:digit:]+$"),
                     col_num = str_extract(name, "[:digit:]+"),
                     across(col_num,
                            as.integer),
                     location = if_else(between(col_num, 3, 5),
                                        "Priest",
                                        if_else(between(col_num, 6, 8),
                                                "Tum_Dry",
                                                "Brdstk"))) %>%
              select(-name, -col_num) %>%
              distinct() %>%
              pivot_wider(names_from = location,
                          values_from = value) %>%
              # focus on broodstock collection, ignore Tumwater/Dryden calls
              select(-Tum_Dry) %>%
              filter(!is.na(Brdstk)) %>%
              rename(Truth = Brdstk,
                     pit_tag_number  = tag_code) %>%
              mutate(run_year = brood_year - 1,
                     qa_loc = "Wenatchee Broodstock")) %>%
  mutate(across(c(Priest,
                  Truth),
                recode,
                "F" = "Female",
                "M" = "Male"),
         across(c(Priest,
                  Truth),
                recode,
                "H" = "Hatchery",
                "W" = "Wild")) %>%
  mutate(agree = if_else(Priest == Truth,
                         T, F))

sex_err_rate <- pra_call_comp %>%
  filter(category == "sex") %>%
  group_by(brood_year,
           # qa_loc,
           sex = Priest) %>%
  summarize(n_tags = n_distinct(pit_tag_number),
            n_true = sum(agree),
            n_false = sum(!agree),
            .groups = "drop") %>%
  mutate(binom_ci = map2(n_false,
                         n_tags,
                         .f = function(x, y) {
                           BinomCI(x, y) %>%
                             as_tibble()
                         })) %>%
  unnest(binom_ci) %>%
  clean_names() %>%
  rename(perc_false = est)

org_err_rate <- pra_call_comp %>%
  filter(category == "origin") %>%
  group_by(brood_year,
           # qa_loc,
           origin = Priest) %>%
  summarize(n_tags = n_distinct(pit_tag_number),
            n_true = sum(agree),
            n_false = sum(!agree),
            .groups = "drop") %>%
  mutate(binom_ci = map2(n_false,
                         n_tags,
                         .f = function(x, y) {
                           BinomCI(x, y) %>%
                             as_tibble()
                         })) %>%
  unnest(binom_ci) %>%
  clean_names() %>%
  rename(perc_false = est)


# save for use in package
usethis::use_data(sex_err_rate,
                  overwrite = T)

# # this version uses the biological data on the Teams site
# # currently only for SY2022
# pra_call_comp <- read_csv(here("T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd",
#                                "inputs/Bio Data",
#                                "2021 OLAFT Steelhead CME-2021-183-PRD final.csv")) %>%
#   clean_names() %>%
#   select(pit_tag,
#          contains("sex"),
#          contains("origin")) %>%
#   add_column(brood_year = 2022,
#              .before = 1) %>%
#   pivot_longer(cols = c(contains("sex"),
#                         contains("origin"))) %>%
#   mutate(location = str_remove(name, "sex_"),
#          location = str_remove(location, "origin_"),
#          location = str_remove(location, "_origin"),
#          category = if_else(str_detect(name, "sex"),
#                             "sex",
#                             if_else(str_detect(name, "origin"),
#                                     "origin",
#                                     NA_character_))) %>%
#   select(-name) %>%
#   distinct() %>%
#   pivot_wider(names_from = location,
#               values_from = value) %>%
#   mutate(source = if_else(category == "origin",
#                           "Scales",
#                           source),
#          agree = if_else(field == final,
#                          T, F))
#
# sex_err_rate <- pra_call_comp %>%
#   filter(category == "sex") %>%
#   select(-scale) %>%
#   filter(!is.na(final)) %>%
#   group_by(brood_year,
#            sex = field) %>%
#   summarize(n_tags = n_distinct(pit_tag),
#             n_true = sum(agree),
#             n_false = sum(!agree),
#             .groups = "drop") %>%
#   mutate(binom_ci = map2(n_false,
#                          n_tags,
#                          .f = function(x, y) {
#                            BinomCI(x, y) %>%
#                              as_tibble()
#                          })) %>%
#   unnest(binom_ci) %>%
#   clean_names() %>%
#   rename(perc_false = est)
#
# org_err_rate <- pra_call_comp %>%
#   filter(category == "origin") %>%
#   filter(!is.na(final)) %>%
#   group_by(brood_year,
#            origin = field) %>%
#   summarize(n_tags = n_distinct(pit_tag),
#             n_true = sum(agree),
#             n_false = sum(!agree),
#             .groups = "drop") %>%
#   mutate(binom_ci = map2(n_false,
#                          n_tags,
#                          .f = function(x, y) {
#                            BinomCI(x, y) %>%
#                              as_tibble()
#                          })) %>%
#   unnest(binom_ci) %>%
#   clean_names() %>%
#   rename(perc_false = est)

#---------------------------
# make one plot of sex error rates
library(tidyverse)
library(UCSthdReddObsErr)
library(here)

data("sex_err_rate")

pd = 0.3
sex_err_rate %>%
  ggplot(aes(x = brood_year,
             y = perc_false,
             color = sex)) +
  geom_errorbar(aes(ymin = lwr_ci,
                    ymax = upr_ci),
                position = position_dodge(pd),
                width = 0) +
  # geom_line(position = position_dodge(pd)) +
  geom_point(aes(size = n_tags),
             position = position_dodge(pd)) +
  theme_bw() +
  labs(x = "Brood Year",
       y = "Wrong Call at Priest (%)",
       color = "Priest\nSex",
       size = "# Tags",
       title = "Sex Call Comparison")

ggsave(here("analysis/figures",
            "PriestSexCalls.pdf"),
       width = 8,
       height = 6)

