# This script generates the figures for the paper
# It uses two datasets:
# Population data from: https://corona-api.com
# Case data from: coronavirus R-package


library(tidyverse)
library(coronavirus)
library(gghighlight)
library(zoo)
library(scales)
library(ggridges)
library(patchwork)
#devtools::install_github("NightingaleHealth/ggforestplot")
#library(ggforestplot)
library(ggstance)
source("EpidemicExplorer/corona_api.R")
source("stripe_geom_ridges.R")

# Parameters ----
window_size <- 7
s_param <- 6.5

# locale to US ----
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Load data ----
coronavirus::update_dataset(silence = T)
df <- coronavirus

europe <-
  c(
    "Albania",
    "Andorra",
    "Armenia",
    "Austria",
    "Azerbaijan",
    "Belarus",
    "Belgium",
    "Bosnia and Herzegovina",
    "Bulgaria",
    "Croatia",
    "Cyprus",
    "Czechia",
    "Denmark",
    "Estonia",
    "Finland",
    "France",
    "Georgia",
    "Germany",
    "Greece",
    "Hungary",
    "Iceland",
    "Ireland",
    "Israel",
    "Italy",
    "Kazakhstan",
    "Kyrgyzstan",
    "Latvia",
    "Lithuania",
    "Luxembourg",
    "Malta",
    "Monaco",
    "Montenegro",
    "Netherlands",
    "North Macedonia",
    "Norway",
    "Poland",
    "Portugal",
    "Moldova",
    "Romania",
    "Russia",
    "San Marino",
    "Serbia",
    "Slovakia",
    "Slovenia",
    "Spain",
    "Sweden",
    "Switzerland",
    "Tajikistan",
    "Turkey",
#    "Turkmenistan",
    "Ukraine",
    "United Kingdom",
    "Uzbekistan"
  ) %>% sort()


# check country availabilty ----
available_countries <- df %>%
  filter(country %in% europe) %>%
  pull(country) %>%
  unique() %>%
  sort()


# looking for a country name?
# available_countries %>% as.data.frame %>% View()
missing <- data.frame(europe, ac = c(available_countries))


# calculate ESI and rolling values ----
df %>%
  # use data form our countries only
  filter(country %in% europe) %>%
  # put into wide format for ESI calculation
  pivot_wider(names_from = "type", values_from = "cases") %>%
  group_by(country, date) %>%
  summarise(death = sum(death),
            recovered = sum(recovered),
            confirmed = sum(confirmed)) %>%
  group_by(country) %>%
  mutate(death_roll = rollapplyr(death, window_size, mean, partial = TRUE, na.rm = TRUE), # use of a 7-day rolling average
         confirmed_roll = rollapplyr(confirmed, window_size, mean, partial = TRUE, na.rm = TRUE), # use of a 7-day rolling average
         recovered_roll = rollapplyr(recovered, window_size, mean, partial = TRUE, na.rm = TRUE)) %>%
  mutate(death_cum = cumsum(death),
         confirmed_cum = cumsum(confirmed),
         recovered_cum = cumsum(recovered)) %>%
  ungroup() %>%
  mutate(date = as.Date(date)) %>%
  mutate(esi = esi(death, recovered, severity = s_param)) %>%
  mutate(esi_cum = esi(death_cum, recovered_cum, severity = s_param)) %>%
  mutate(esi_roll = esi(death_roll, recovered_roll, severity = s_param)) -> our_data



# sanity check ----
check_country <- "Germany"
total_cases <- our_data %>%
  filter(country == check_country) %>%
  pull(confirmed_cum) %>% max()
total_deaths <- our_data %>%
  filter(country == check_country) %>%
  pull(death_cum) %>% max()
our_data %>% filter(country == check_country) %>%
  pivot_longer(cols = c("death", "death_roll", "death_cum",
                        "confirmed", "confirmed_roll", "confirmed_cum",
                        "recovered", "recovered_roll", "recovered_cum",
                        "esi", "esi_roll", "esi_cum",
                        )) %>%
  ggplot() + aes(x= date, y = value, color = name) +
  geom_line() +
  facet_wrap(vars(name), scales = "free_y", ncol = 3) +
  labs(title = paste("Country:", check_country ),
       caption = paste("Total:", format(total_cases, big.mark = ","),
                       "Deathts:", format(total_deaths, big.mark = ",")))


# get population data from https://corona-api.com

pop_data <- get_country_data() %>%
  select(country = name, population) %>%
  # adapt spellings between two datasets
  mutate(country = ifelse(country == "UK", "United Kingdom", country)) %>%
  mutate(ppm = population / 1000000) # population per million

# find missing data
left_join(our_data,
          pop_data %>% filter(country %in% europe) ) %>%
  filter(is.na(population))

# —PLOTTING STARTS HERE— ----

# Heatmap ----
our_data %>%
  left_join(pop_data) %>%
  group_by(country, date) %>%
  filter(date >= "2020-3-1") %>%
  mutate(confirmed_pm = confirmed / ppm,
         confirmed_roll_pm = confirmed_roll / ppm,
         confirmed_cum_pm = confirmed_cum / ppm,
         death_pm = death / ppm,
         death_roll_pm = death_roll / ppm,
         death_cum_pm = death_cum / ppm
         ) -> joined_data

joined_data %>%
  group_by(country) %>%
  arrange(desc(death_cum_pm)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(death_cum_pm)) %>%
  pull(country) -> country_levels


(joined_data %>%
  ggplot() +
  aes(x = date, fill = death_roll_pm, y = fct_relevel(country, country_levels)) +
  geom_tile(alpha = 0.9) +
  scale_fill_viridis_c("daily death incidence per million", option = "B", limits = c(0,NA)) +
  scale_x_date(date_breaks = "month", labels = label_date_short()) +
  scale_y_discrete(limits = rev) +
  scale_alpha_continuous(range = c(0.0,1)) +
  cowplot::theme_minimal_hgrid() +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 0.5), alpha = FALSE) +
  theme(legend.position="bottom") +
  labs(x = "Date", y= NULL, caption = paste("Gray areas denote missing data."),
       title = "Deaths per million (7-day average)") -> p1
)
ggsave("death_example.pdf")


(joined_data %>%
  ggplot() +
  aes(x = date, fill = confirmed_roll_pm, y = fct_relevel(country, country_levels), group = country) +
  #aes(x = date, height = confirmed_pm, alpha = confirmed_pm, y = fct_reorder(country, -death_pm), group = country) +
  geom_tile(alpha = 0.9) +
  #guides(color = FALSE) +
  #ggridges::geom_density_ridges() +
  NULL +
  scale_fill_viridis_c("weekly case incidence per million", option = "B", limits = c(0,NA)) +
  scale_x_date(date_breaks = "month", labels = label_date_short()) +
  scale_y_discrete(limits = rev) +
  scale_alpha_continuous(range = c(0.0,1)) +
  cowplot::theme_minimal_grid() +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 0.5), alpha = FALSE) +
  theme(legend.position="bottom") +
  labs(x = "Date", y= NULL, caption = paste(""),
       title = "Cases per million 7-day average") -> p2
)

p2 + p1
ggsave("heatmap_comparison.pdf")

our_data %>%
  ggplot() +
  aes(x = date, color = esi_roll, y = fct_relevel(country, country_levels)) +
  geom_point(size = 4, shape = 15, alpha = 0.5) +
  #guides(color = FALSE) +
  NULL +
  scale_color_viridis_c("ESI (7d avg)", option = "B") +
  scale_x_date(date_breaks = "month", labels = label_date_short()) +
  scale_y_discrete(limits = rev) +
  cowplot::theme_minimal_hgrid() +
  labs(x = "Date", y= NULL, caption = paste("ESI S-Parameter = ",s_param, "- Gray areas denote data errors."),
       title = "Epidemic Severity Index (ESI) in Europe over time")
ggsave("esi_europe_roll.pdf")




# Ridgeline plots ----

(joined_data %>%
  ggplot() +
  #aes(x = date, fill = confirmed_pm, alpha = confirmed_pm, y = fct_reorder(country, -death_pm), group = country) +
  aes(x = date, height = death_roll_pm, y = fct_relevel(country, rev(country_levels))) +
  #guides(color = FALSE) +
  geom_stripes(odd = "#22222222", even = "#00000000")+
  ggridges::geom_density_ridges(stat = "identity", fill = "skyblue", size = 0.5, show.legend = T) +
  cowplot::theme_minimal_grid() +
  scale_y_discrete(expand = expansion(add = c(0,2))) +
  scale_x_date(date_breaks = "month", labels = label_date_short(), expand = c(0,0)) +
  labs(x = NULL, y= NULL, caption = paste(""),
       title = "Reported deaths\n(daily cases per million people)") +
   theme(axis.title.y=element_blank(),
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank(),
         plot.title = element_text(hjust = 0.5)
         ) -> p1
)

(joined_data %>%
  ggplot() +
  #aes(x = date, fill = confirmed_pm, alpha = confirmed_pm, y = fct_reorder(country, -death_pm), group = country) +
  aes(x = date, height = confirmed_roll_pm, y = fct_relevel(country, rev(country_levels))) +
  #guides(color = FALSE) +
  geom_stripes(odd = "#22222222", even = "#00000000")+
  ggridges::geom_density_ridges(stat = "identity", fill = "skyblue", size = 0.5, show.legend = T) +
  cowplot::theme_minimal_grid() +
  scale_y_discrete(expand = expansion(add = c(0,2))) +
  scale_x_date(date_breaks = "month", labels = label_date_short(), expand = c(0,0)) +
  labs(x = NULL, y= NULL, caption = paste(""),
       title = "Reported Incidence\n(daily cases per million people)") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_text(hjust = 0.5, vjust = 0),
          plot.title = element_text(hjust = 0.5)
          ) -> p2
)

p1 + p2

ggsave("comparison_grid.pdf")




# Line plots ----
our_data %>%
  filter(country %in%  c("Germany", "Belgium", "Spain", "France", "Switzerland")) %>%
  ggplot() +
  aes(x = date, color = country, y = esi_roll) +
  geom_line() +
  #gghighlight::gghighlight(country %in% c("Germany")) +
  cowplot::theme_minimal_hgrid() +
  labs(x = "Date", y= "ESI", caption = paste("ESI S-Parameter = ",s_param),
       title = "Epidemic Severity Index (ESI) in Europe over time")

# Excess mortality plots

excess <- read_csv("excess-mortality-p-scores.csv") %>%
  mutate(excess_m = `Excess mortality P-scores, all ages`) %>%
  rename(country = Entity, date = Day) %>%
  bind_rows(
    data.frame(
      country = c("Bosnia and Herzegovina", "Ireland", "Monaco", "Turkey", "Kazakhstan", "Tajikistan"),
      date = as.Date("2020-01-31"),
      `Excess mortality P-scores, all ages` = 0,
      excess_m = 0)
    )


joined_data %>%
  filter(country %in% europe) %>%
  left_join(excess) %>%
  filter(!is.na(death_cum)) %>%
  filter(!is.na(excess_m)) %>%
  ggplot() +
  aes(x = date, y = fct_relevel(country, country_levels), height = excess_m) +
  geom_ridgeline(scale = 0.01, min_height = -0.1, fill = "darkred") +
  geom_ridgeline(scale = 0.01, min_height = -0.1, color = "darkgreen", fill = "darkolivegreen",
                 mapping = aes(x = date, y = country, height = -excess_m)) +
  cowplot::theme_minimal_grid() +
  scale_x_date(date_breaks = "month", labels = label_date_short(), expand = c(0,0)) +
  labs(x = NULL, y= NULL,
       title = "Excess mortality p-scores",
       caption = "Red indicates excess mortality, green indicates lower mortality.") -> p3

p2 + p1 + p3

ggsave("excess.pdf", width = 20)


