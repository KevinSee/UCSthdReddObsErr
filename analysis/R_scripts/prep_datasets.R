# Author: Kevin See
# Purpose: Prepare some datasets for inclusion in the package
# Created: 5/4/2020
# Last Modified: 11/17/2021
# Notes: This data was sent by Michael Hughes

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(magrittr)
library(readxl)
library(usethis)
library(here)

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

