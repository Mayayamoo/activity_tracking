library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(TraMineR)
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

curious <- monthly.change(mon.cat.tot.hrs, "Work")
curious2 <- monthly.change(mon.act.tot.hrs, "bathroom")

all.changes <- function(x) {
    if (x == "category") {
        df <- mon.cat.tot.hrs
    } else {
        df <- mon.act.tot.hrs
    }

    df_transform <-df %>%
        group_by(.data[[x]]) %>%
        arrange(.data[[x]], month_year) %>%
        mutate(
            MoM_change = total_hours - lag(total_hours),
            percent_change = (MoM_change / lag(total_hours)) * 100,
            summary = sprintf("%.1f hrs, %+.1f%%",
                total_hours, percent_change)
        ) %>%
    ungroup() %>%
    select(month_year, all_of(x), MoM_change, percent_change, summary)

    split_dfs <- split(df_transform, df_transform[[x]])

    return(split_dfs)
}

mon_change_all <- function(c) {
    df_list <- all.changes(c)
    clean_names <- names(df_list) %>%
        str_replace_all(" ", "_") %>%
        tolower()
    named_list <- setNames(
        df_list,
        paste0(clean_names,"_mon_change")
    )
    list2env(named_list, envir = .GlobalEnv)
    return(named_list)
}
cat.changes <- all.changes("category")
act.changes <- all.changes("activity")
work_df <- cat.changes[["Work"]]
bathroom_df <- act.changes[["Morning routine"]]

cat.changes.monthly <- mon_change_all("category")

act.changes.monthly <- mon_change_all("activity")


total_minutes <- function(col_name, value, end_date=Sys.Date(), start_date="2024-01-22") {
    total_minutes <- activities.formatted %>% 
        filter(.data[[col_name]] == value) %>%
        filter(between(date, as.Date(start_date), as.Date(end_date))) %>%
        summarise(
            total_minutes = sum(duration_minutes, na.rm = TRUE)) %>%
        pull(total_minutes)
    return(total_minutes)
}

total_summary <- function(col_name, value, end_date=Sys.Date(), start_date="2024-01-22") {
    days.ranged <- activities.formatted %>%
        filter(between(date, as.Date(start_date), as.Date(end_date)))
    days.ranged <- n_distinct(days.ranged$date)

    minutes.value <- total_minutes(col_name, value, end_date, start_date)
    if(minutes.value < 60) {
        unit <- "minutes"
        display_value <- minutes.value
    } else {
        unit <- "hours"
        display_value <- minutes.value/60
    }

    result <- activities.formatted %>%
        filter(.data[[col_name]] == value) %>%
        filter(between(date, as.Date(start_date), as.Date(end_date))) %>%
        mutate(
            minutes_since_midnight = hour(time_started)*60 + minute(time_started),
        ) %>%
        summarise(
            typical_minutes = median(minutes_since_midnight),
            typical_hour = floor(typical_minutes/60),
            typical_minute = round(typical_minutes %% 60),
            duration_variance = sd(duration_minutes)/mean(duration_minutes) *100,
            weighted_avg_hour =
                sum(hour * duration_minutes, na.rm = TRUE) / 
                sum(duration_minutes, na.rm = TRUE),
            total_time = display_value,
            time_unit = unit,
            total_days = n_distinct(date),
            days_logged_percent = n_distinct(date) / days.ranged * 100,
            average_time_per_day = minutes.value / days.ranged,
            sessions = n(),
            average_session_time = minutes.value / n(),
            .groups = 'drop'
    )

    if(result$average_time_per_day < 60) {
        avg_display <- sprintf("%.1f minutes", result$average_time_per_day)
    } else {
        avg <- result$average_time_per_day/60
        hours <- floor(avg)
        minutes <- round((avg %% 1) * 60)
        avg_display <- sprintf("%d hours %d minutes", hours, minutes)
    }
    cat("------------------------------------------\n")
    cat(sprintf("SUMMARY FOR: %s\n", toupper(value)))
    cat(start_date, "to", end_date, "\n")
    cat("------------------------------------------\n")
    cat(sprintf("Total time spent: %.2f %s\n", result$total_time, result$time_unit))
    cat(sprintf("Logged %.0f days, %.2f%% of days\n", result$total_days, result$days_logged_percent))
    cat(sprintf("Average time per day: %s\n", avg_display))
    cat(sprintf("%d sessions, Average session time: %.2f minutes with %.2f%% variance\n", result$sessions,result$average_session_time, result$duration_variance))
    cat(sprintf("Average time of day %d:%d\n", result$typical_hour, result$typical_minute))
    invisible(result)
}

total_summary("activity", "Puzzles", "2025-06-22")


