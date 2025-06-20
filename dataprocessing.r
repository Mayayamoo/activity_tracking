library(tidyverse)
library(lubridate)
library(dplyr)
activities_raw <- read.csv("G:/My Drive/activitytracker/stt_records_automatic.csv")

activities.formatted <- activities_raw %>%
    rename(
        time_started_raw = "time.started",
        time_ended_raw = "time.ended",
        duration_minutes = "duration.minutes",
        category = "categories",
        activity= "activity.name"
    ) %>%
    mutate(
        time_started = ymd_hms(time_started_raw),
        time_ended = ymd_hms(time_ended_raw),
        date = as.Date(time_started),
        hour = hour(time_started),
        month = format(date, "%m"),
        year = format(date, "%Y"),
        month_year = as.Date(paste(year, month, "01", sep = "-"))
    ) %>%
    select(
        activity, category, date, duration_minutes, 
        time_started, time_ended, hour, month, year, month_year
)

mon.act.tot.hrs <- activities.formatted %>%
    group_by(month_year, activity) %>%
    summarise(
        total_hours = sum(duration_minutes/60, na.rm = TRUE),
        .groups='drop'
    ) %>%
    select(activity, month_year, total_hours)

mon.cat.tot.hrs <- activities.formatted %>%
    group_by(month_year, category) %>%
    summarise(
        total_hours = sum(duration_minutes/60, na.rm = TRUE),
        .groups='drop'
    ) %>%
    select(category, month_year, total_hours)


monthly.change <- function(df,x) {
    df %>%
        filter(
            if (identical(df, mon.act.tot.hrs)) {
                activity == x
            } else {
                category == x
            }
        ) %>%
        arrange(month_year) %>%
        mutate(
            change = total_hours - lag(total_hours),
            percent_change = (change/lag(total_hours)) * 100
        ) %>%
        select(month_year, total_hours, change, percent_change)
}
