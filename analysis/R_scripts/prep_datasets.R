# Author: Kevin See
# Purpose: Prepare some datasets for inclusion in the package
# Created: 5/4/2020
# Last Modified: 5/4/2020
# Notes: This data was sent by Michael Hughes

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(readxl)
library(usethis)

#-----------------------------------------------------------------
# removals at various sources, by year
removal_df = read_excel('analysis/data/raw_data/2014 to 2019 STHD Removals_Harvest and Brood Collected.xlsx',
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

removal_df = removal_df %>%
  bind_rows(removal_2020) %>%
  distinct()

#-----------------------------------------------------------------
# mean thalweg CV, using all measurements across years
thlwg_summ = read_excel('analysis/data/raw_data/Master_STHD Thalwegs.xlsx',
                        skip = 1) %>%
  rename(Year = `...1`) %>%
  mutate(Year = as.integer(Year)) %>%
  filter(!is.na(Year)) %>%
  gather(Reach, CV, W1:W10) %>%
  mutate_at(vars(CV),
            list(as.numeric)) %>%
  group_by(Reach) %>%
  summarise(n_yrs = n_distinct(Year[!is.na(CV)]),
            n_meas = sum(!is.na(CV)),
            MeanThalwegCV = mean(CV, na.rm = T)) %>%
  mutate_at(vars(MeanThalwegCV),
            list(~ . * 100)) %>%
  mutate(MeanThalwegCV = if_else(Reach == 'W4',
                                 MeanThalwegCV[Reach =="W5"],
                                 MeanThalwegCV)) %>%
  ungroup() %>%
  mutate(Reach = fct_relevel(Reach, 'W10', after = Inf)) %>%
  arrange(Reach)

#-----------------------------------------------------------------
# save for use in package
usethis::use_data(removal_df, thlwg_summ,
                  overwrite = T)

