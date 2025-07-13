source("config.r")

library(readr)

activities_raw <- read_csv(CONFIG$activity_data_path, show_col_types = FALSE)

#formats data
activities.formatted <- activities_raw %>%
    rename(
        time_started_raw = "time started",
        time_ended_raw = "time ended",
        duration_minutes = "duration minutes",
        category = "categories",
        activity= "activity name"
    ) %>%
    mutate(
        time_started = ymd_hms(time_started_raw),
        time_ended = ymd_hms(time_ended_raw),
        date = as.Date(time_started),
        hour = hour(time_started),
        month = format(date, "%m"),
        year = format(date, "%Y"),
        month_year = as.Date(paste(year, month, "01", sep = "-")),
        duration = as.numeric(period_to_seconds(hms(duration))) / 60
    ) %>%
    select(
        activity, category, date, duration_minutes, duration,
        time_started, time_ended, hour, month, year, month_year
)