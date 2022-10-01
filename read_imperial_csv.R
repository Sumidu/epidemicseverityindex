library(tidyverse)
library(lubridate)

germany <- read_csv("imperialdata/germany.csv")


germany %>% select(-RecordNo) %>%
  mutate(endtime = lubridate::as_datetime(endtime)) %>%
  mutate(endtime = as_date(endtime))
