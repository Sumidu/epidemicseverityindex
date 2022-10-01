# Generate Epidemic data for analysis with YouGov Data
# created Sep 27th 2022
# auth: ACV


library(tidyverse)
library(coronavirus)
library(gghighlight)
library(zoo)
library(scales)
library(ggridges)
library(patchwork)

source("EpidemicExplorer/corona_api.R")

# Parameters ----
window_size <- 7
s_param <- 6.5

# locale to US ----
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Load data ----
coronavirus::update_dataset(silence = T)
df <- coronavirus


countries_of_interest <-
  c("australia",
    "brazil",
    "canada",
    "china",
    "denmark",
    "finland",
    "france",
    "germany",
    #"hong-kong",
    "india",
    "israel",
    "indonesia",
    "italy",
    "japan",
    "malaysia",
    "mexico",
    "netherlands",
    "norway",
    "philippines",
    "saudi-arabia",
    "singapore",
    "south-korea",
    "spain",
    "sweden",
    "taiwan",
    "thailand",
    "united-arab-emirates",
    "united-kingdom",
    "united-states",
    "vietnam"
  ) %>% sort()

df %>% mutate(country = str_to_lower(country)) %>%
  mutate(country = str_replace_all(country, pattern = " ", replacement = "-")) %>%
  mutate(country = str_replace(country, pattern = "us", replacement = "united-states"))

# check country availabilty ----
available_countries <- df %>%
  mutate(country = str_replace(country, pattern = "US", replacement = "united-states")) %>%
  mutate(country = str_replace(country, pattern = "Korea, South", replacement = "south-korea")) %>%
  mutate(country = str_to_lower(country)) %>%
  mutate(country = str_replace_all(country, pattern = " ", replacement = "-")) %>%
  mutate(country = str_replace_all(country, pattern = "\\*", replacement = "")) %>%
  #select(country) %>% unique()
  filter(country %in% countries_of_interest) %>%
  pull(country) %>%
  unique() %>%
  sort()
# looking for a country name?
# available_countries %>% as.data.frame %>% View()
missing <- data.frame(countries_of_interest, ac = c(available_countries))

df %>% pull(country) %>% unique()

available_countries
countries_of_interest

# calculate ESI and rolling values ----
df %>%
  mutate(country = str_replace(country, pattern = "US", replacement = "united-states")) %>%
  mutate(country = str_replace(country, pattern = "Korea, South", replacement = "south-korea")) %>%
  mutate(country = str_to_lower(country)) %>%
  mutate(country = str_replace_all(country, pattern = " ", replacement = "-")) %>%
  mutate(country = str_replace_all(country, pattern = "\\*", replacement = "")) %>%
  # use data form our countries only
  #filter(country %in% countries_of_interest) %>%
  # put into wide format for ESI calculation
  pivot_wider(names_from = "type", values_from = "cases") %>%
  group_by(country, date) %>%
  summarise(death = sum(death, na.rm = TRUE),
            recovered = sum(recovery, na.rm = TRUE),
            confirmed = sum(confirmed, na.rm = TRUE)) %>%
  group_by(country) %>%
  mutate(death_roll = rollapplyr(death, window_size, mean, partial = TRUE, na.rm = TRUE), # use of a 7-day rolling average
         confirmed_roll = rollapplyr(confirmed, window_size, mean, partial = TRUE, na.rm = TRUE), # use of a 7-day rolling average
         recovered_roll = rollapplyr(recovered, window_size, mean, partial = TRUE, na.rm = TRUE)) %>%
  mutate(death_cum = cumsum(death),
         confirmed_cum = cumsum(confirmed),
         recovered_cum = cumsum(recovered)) %>%
  ungroup() %>%
  mutate(date = as.Date(date)) -> our_data
  #mutate(esi = esi(death, recovered, severity = s_param)) %>%
  #mutate(esi_cum = esi(death_cum, recovered_cum, severity = s_param)) %>%
  #mutate(esi_roll = esi(death_roll, recovered_roll, severity = s_param))

our_data %>% pull(date) %>% max()


# sanity check ----
check_country <- "germany"
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
                        #"esi", "esi_roll", "esi_cum",
  )) %>%
  ggplot() + aes(x= date, y = value, color = name) +
  geom_line() +
  facet_wrap(vars(name), scales = "free_y", ncol = 3) +
  labs(title = paste("Country:", check_country ),
       caption = paste("Total:", format(total_cases, big.mark = ","),
                       "Deaths:", format(total_deaths, big.mark = ",")))


# get population data from https://corona-api.com ----

pop_data <- get_country_data() %>%
  select(country = name, population) %>%
  # adapt spellings between two datasets
  mutate(country = ifelse(country == "UK", "United Kingdom", country)) %>%
  mutate(ppm = population / 1000000) %>%  # population per million %>%
  mutate(country = str_replace(country, pattern = "USA", replacement = "united-states")) %>%
  mutate(country = str_replace(country, pattern = "S. Korea", replacement = "south-korea")) %>%
  mutate(country = str_to_lower(country)) %>%
  mutate(country = str_replace_all(country, pattern = " ", replacement = "-")) %>%
  mutate(country = str_replace_all(country, pattern = "\\*", replacement = ""))

# find missing data (should yield an empty dataframe)
left_join(our_data,
          pop_data %>% filter(country %in% countries_of_interest) ) %>%
  filter(is.na(population)) %>% distinct(country)

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

# Countries with missing data.
joined_data %>%
  filter(if_any(everything(), is.na)) %>%
  pull(country) %>%
  unique() -> missing_countries
cat("Missing:")
cat(paste("", missing_countries, "\n"))
joined_data %>%
  filter(!country %in% missing_countries)


(joined_data %>%
    filter(!country %in% missing_countries) %>%
    ggplot() +
    aes(x = date, fill = death_roll_pm, y = fct_relevel(country, country_levels)) +
    geom_tile(alpha = 0.9) +
    scale_fill_viridis_c("daily death incidence per million", option = "B", limits = c(0,NA)) +
    scale_x_date(date_breaks = "month", labels = label_date_short()) +
    scale_y_discrete(limits = rev) +
    scale_alpha_continuous(range = c(0.0,1)) +
    cowplot::theme_minimal_hgrid() +
    guides(fill = guide_colourbar(barwidth = 10, barheight = 0.5), alpha = "none") +
    theme(legend.position="bottom") +
    labs(x = "Date", y= NULL, caption = paste("Gray areas denote missing data."),
         title = "Deaths per million (7-day average)") -> p1
)

# Countries with missing data.
joined_data %>% filter(if_any(everything(), is.na)) %>% pull(country) %>% unique() -> missing_countries
cat(paste("Missing", missing_countries, "\n"))
joined_data %>%
  filter(!country %in% missing_countries) %>%
  write_csv("cases.csv")
