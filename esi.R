library(tidyverse)
library(coronavirus)
library(gghighlight)
library(zoo)
library(scales)

esi <- function(deaths, recovered, severity = 6.5, epsilon = 20) {
  D <- deaths
  S <- severity
  R <- recovered

  A <- log10(1 + D)

  beta <- tanh( (S * D - R) / epsilon + (D + R)/(2 * epsilon) )

  I = A * beta
  return(I)
}


coronavirus::update_dataset()


europe <- c("Germany", "Belgium", "Spain", "Italy", "United Kingdom", "France", "Denmark", "Norway", "Sweden", "Finland", "Portugal",
            "Slovakia", "Albania", "Andorra", "Austria", "Bosnia and Herzegovina", "Croatia", "Cyprus",
            "Czechia", "Greece", "Estonia", "Ireland", "Luxembourg", "Malta", "Montenegro", "Monaco", "Netherlands",
            "Poland", "Romania", "Serbia", "Switzerland")

df <- coronavirus



window_size <- 7

s_param <- 13
df %>%
  filter(country %in% europe) %>%
  pivot_wider(names_from = "type", values_from = "cases") %>%
  group_by(country, date) %>%
  summarise(death = sum(death),
            recovered = sum(recovered)) %>%
  group_by(country) %>%
  mutate(death_roll = rollapplyr(death, window_size, mean, partial = TRUE, na.rm = TRUE), # use of a 7-day rolling average
         recovered_roll = rollapplyr(recovered, window_size, mean, partial = TRUE, na.rm = TRUE)) %>%
  mutate(death_cum = cumsum(death),
         recovered_cum = cumsum(recovered)) %>%
  ungroup() %>%
  mutate(date = as.Date(date)) %>%
  mutate(esi = esi(death, recovered, severity = s_param)) %>%
  mutate(esi_cum = esi(death_cum, recovered_cum, severity = s_param)) %>%
  mutate(esi_roll = esi(death_roll, recovered_roll, severity = s_param)) -> our_data

# sanity check
our_data %>% filter(country == "Germany") %>%
#  filter(date > "2021-01-01") %>%
  ggplot() + aes(x= date, y = death) +
  geom_line()


our_data %>%
  filter(country == "Germany") %>%
  pull(recovered_cum) %>% max()

# Heatmap
our_data %>%
  ggplot() +
  aes(x = date, color = esi, y = country) +
  geom_point(size = 4, shape = 15, alpha = 0.5) +
  #guides(color = FALSE) +
  NULL +
  scale_color_viridis_c("ESI", option = "D") +
  scale_x_date(date_breaks = "month", labels = label_date_short()) +
  scale_y_discrete(limits = rev) +
  cowplot::theme_minimal_hgrid() +
  labs(x = "Date", y= NULL, caption = paste("ESI S-Parameter = ",s_param, "Gray areas denote data errors."),
       title = "Epidemic Severity Index (ESI) in Europe over time")
ggsave("esi_europe.pdf")


our_data %>%
  ggplot() +
  aes(x = date, color = esi_roll, y = country) +
  geom_point(size = 4, shape = 15, alpha = 0.5) +
  #guides(color = FALSE) +
  NULL +
  scale_color_viridis_c("ESI (7d avg)", option = "D") +
  scale_x_date(date_breaks = "month", labels = label_date_short()) +
  scale_y_discrete(limits = rev) +
  cowplot::theme_minimal_hgrid() +
  labs(x = "Date", y= NULL, caption = paste("ESI S-Parameter = ",s_param, "Gray areas denote data errors."),
       title = "Epidemic Severity Index (ESI) in Europe over time")
ggsave("esi_europe_roll.pdf")


# Line plots
our_data %>%
  filter(country %in%  c("Germany", "Belgium", "Spain", "France", "Switzerland")) %>%
  ggplot() +
  aes(x = date, color = country, y = esi) +
  geom_line() +
  gghighlight::gghighlight(country %in% c("Germany")) +
  cowplot::theme_minimal_hgrid() +
  labs(x = "Date", y= "ESI", caption = paste("ESI S-Parameter = ",s_param),
       title = "Epidemic Severity Index (ESI) in Europe over time")



our_data %>% filter(country == "Germany") %>% View()



