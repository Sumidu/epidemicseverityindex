library(tidyverse)
library(lubridate)
library(labelled)
dat <- read_csv("imperialdata/all_countries_numerical.csv", guess_max = 20000)

germany <- read_csv("imperialdata/germany.csv")

cases_dat <- read_csv("imperialdata/Gathered Data/case numbers/cases.csv")
beh_dat <- read_csv2("imperialdata/Gathered Data/Behavior/COVID-19_gedrag.csv")
macro_dat <- read_csv("imperialdata/Gathered Data/macrodata.csv")


# example of how to label a variable
labelled::to_labelled(to_factor(germany$i7a_health))
var_label(germany$vac_1) <- "If a Covid-19 vaccine were made available to me this week, I would definitely get it"

# getting a plot for this variable
# also has code to fix the date
germany %>%
  mutate(date = str_sub(endtime, start = 0, end = 10)) %>%
  mutate(date = dmy(date)) %>%
  mutate(vac_1 = as_factor(vac_1) %>% fct_relevel(c(
  "1 - Strongly agree", "2", "3", "4", "5 – Strongly disagree"
))) %>%
  select(date, vac_1) %>%
  filter(!is.na(vac_1)) %>%
  ggplot() +
  aes(x = date, y = vac_1) +
  labs(caption = var_label(germany$vac_1)) +
    geom_bin_2d()





  cases_dat %>%
  filter(!is.na(confirmed_roll)) %>%
  filter(country == "germany") %>%
  filter(confirmed_roll > 0) %>%
  ggplot() +
  aes(x = date, y = confirmed_roll ) +
  geom_point() +
  NULL

names(dat)
