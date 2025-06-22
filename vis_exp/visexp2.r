library(tidyverse)
library(lubridate)  
library(dplyr)

df <- read.csv("rawdata/Activitylog.csv")

df <- df %>%
  mutate(
    time_started = ymd_hms(time_started),
    time_ended = ymd_hms(time_ended),
    date = as.Date(time_started),
    total_hours = (duration_minutes / 60)
  )

balance <- df %>%
  filter(categories %in% c("Work", "Leisure")) %>%
  group_by(date, categories) %>%
  summarise(total = sum(total_hours)) %>%
  pivot_wider(names_from = categories, values_from = total) %>%
  mutate(ratio = Work / Leisure)

print (balance)