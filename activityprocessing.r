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

# Day of week distribution
analyze_weekday_distribution <- function(summary.data) {
    summary.data %>%
        mutate(weekday = wday(date, label = TRUE)) %>%
        group_by(weekday) %>%
        summarize(
            count = n(),
            total_time = sum(duration_minutes),
            .groups = 'drop'
        ) %>%
        mutate(
            percent = count / sum(count) * 100,
            time_percent = total_time / sum(total_time) * 100
        )
}

analyze_streaks_and_gaps <- function(summary.data, start_date, end_date, threshold_minutes = 120) {
    # Validate inputs
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
    
    date_range <- seq(start_date, end_date, by = "day")

    daily_totals <- summary.data %>%
    group_by(date) %>%
    summarize(
        daily_minutes = sum(duration_minutes, na.rm = TRUE),
        .groups = 'drop'
    )

    calendar_df <- data.frame(date = date_range) %>%
        mutate(has_activity = date %in% unique(summary.data$date))
    
    calendar_df_gap <- data.frame(date = date_range) %>%
        left_join(daily_totals, by = "date") %>%
        mutate(
        daily_minutes = ifelse(is.na(daily_minutes), 0, daily_minutes),
        low_activity = daily_minutes < threshold_minutes
        )
    # Calculate streaks and gaps
    runs <- rle(calendar_df$has_activity)
    runs_low <- rle(calendar_df_gap$low_activity)
    
    low_activity_periods <- runs_low$lengths[runs_low$values == TRUE]
    longest_low_act_period <- if(length(low_activity_periods) > 0) max(low_activity_periods) else 0
    
    streaks <- runs$lengths[runs$values == TRUE]
    gaps <- runs$lengths[runs$values == FALSE]
    
    longest_streak <- if(length(streaks) > 0) max(streaks) else 0
    longest_gap <- if(length(gaps) > 0) max(gaps) else 0
    average_streak <- mean(streaks, na.rm = TRUE)
    # Find streak dates
    streak_dates <- list(
        start_date = NA,
        end_date = NA
    )
    
    # Find gap dates
    gap_dates <- list(
        start_date = NA,
        end_date = NA
    )

    mod_gap_dates <- list(
        start_date = NA,
        end_date = NA
    )
    
    # Calculate positions for each run
    positions <- data.frame(
        is_activity = runs$values,
        length = runs$lengths,
        end_pos = cumsum(runs$lengths),
        start_pos = cumsum(runs$lengths) - runs$lengths + 1
    )

    positions_low <- data.frame(
        is_low_activity = runs_low$values,
        length = runs_low$lengths,
        end_pos = cumsum(runs_low$lengths),
        start_pos = cumsum(runs_low$lengths) - runs_low$lengths + 1
    )
    
    # Find the longest streak
    if(longest_streak > 0) {
        streak_idx <- which(positions$is_activity & positions$length == longest_streak)[1]
        if(length(streak_idx) > 0) {
            streak_start_pos <- positions$start_pos[streak_idx]
            streak_end_pos <- positions$end_pos[streak_idx]
            streak_dates$start_date <- calendar_df$date[streak_start_pos]
            streak_dates$end_date <- calendar_df$date[streak_end_pos]
        }
    }
    
    # Find the longest gap
    if(longest_gap > 0) {
        gap_idx <- which(!positions$is_activity & positions$length == longest_gap)[1]
        if(length(gap_idx) > 0) {
            gap_start_pos <- positions$start_pos[gap_idx]
            gap_end_pos <- positions$end_pos[gap_idx]
            gap_dates$start_date <- calendar_df$date[gap_start_pos]
            gap_dates$end_date <- calendar_df$date[gap_end_pos]
        }
    }

    if(longest_low_act_period > 0) {
        mod_gap_idx <- which(positions_low$is_low_activity & positions_low$length == longest_low_act_period)[1]
        if(length(mod_gap_idx) > 0) {
            mod_gap_start_pos <- positions_low$start_pos[mod_gap_idx]
            mod_gap_end_pos <- positions_low$end_pos[mod_gap_idx]
            mod_gap_dates$start_date <- calendar_df_gap$date[mod_gap_start_pos]
            mod_gap_dates$end_date <- calendar_df_gap$date[mod_gap_end_pos]
        }
    }
    
    return(list(
        longest_streak = longest_streak,
        longest_gap = longest_gap,
        streak_start_date = streak_dates$start_date,
        streak_end_date = streak_dates$end_date,
        gap_start_date = gap_dates$start_date,
        gap_end_date = gap_dates$end_date,
        average_streak = average_streak,
        longest_low_act_period = longest_low_act_period,
        low_act_period_start_date = mod_gap_dates$start_date,
        low_act_period_end_date = mod_gap_dates$end_date,
        threshold_minutes = threshold_minutes
    ))
}

total_summary <- function(col_name, value, end_date=Sys.Date(), start_date="2024-01-22") {

    # Get number of total days in range
    days.ranged <- activities.formatted %>%
        filter(between(date, as.Date(start_date), as.Date(end_date)))
    days.ranged <- n_distinct(days.ranged$date)

    # get time and units
    minutes.value <- total_minutes(col_name, value, end_date, start_date)
    if(minutes.value < 60) {
        unit <- "minutes"
        display_value <- minutes.value
    } else {
        unit <- "hours"
        display_value <- minutes.value/60
    }

    # filter data
    summary.data <- activities.formatted %>%
        filter(.data[[col_name]] == value) %>%
        filter(between(date, as.Date(start_date), as.Date(end_date)))
    
    weekday_distribution <- analyze_weekday_distribution(summary.data)
    weekday_top <- weekday_distribution %>% 
    arrange(desc(percent))

    result <- summary.data %>%  
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
            average_time_per_day_yes = minutes.value / n_distinct(date),
            sessions = n(),
            average_session_time = minutes.value / n(),
            .groups = 'drop'
    )
    streak_analysis <- analyze_streaks_and_gaps(summary.data, start_date, end_date)
    if(result$average_time_per_day < 60) {
        avg_display <- sprintf("%.1f minutes", result$average_time_per_day)
    } else {
        avg <- result$average_time_per_day/60
        hours <- floor(avg)
        minutes <- round((avg %% 1) * 60)
        avg_display <- sprintf("%d hours %d minutes", hours, minutes)
    }

    if(result$average_time_per_day_yes < 60) {
        avg_yes_display <- sprintf("%.1f minutes", result$average_time_per_day_yes)
    } else {
        avg <- result$average_time_per_day_yes/60
        hours <- floor(avg)
        minutes <- round((avg %% 1) * 60)
        avg_yes_display <- sprintf("%d hours %d minutes", hours, minutes)
    }


    cat("------------------------------------------\n")
    cat(sprintf("SUMMARY FOR: %s\n", toupper(value)))
    cat(start_date, "to", end_date, "\n")
    cat("------------------------------------------\n")
    cat("\n")
    cat(sprintf("BASIC METRICS:\n"))
    cat(sprintf("Total time spent: %.2f %s\n", result$total_time, result$time_unit))
    cat(sprintf("Days with activity: %.0f (%.2f%%)\n", result$total_days, result$days_logged_percent))
    cat(sprintf("Average time per day (total): %s\n", avg_display))
    cat(sprintf("Average time per day (days done): %s\n", avg_yes_display))
    cat(sprintf("Total sessions: %d (avg %.2f minutes)\n", result$sessions,result$average_session_time))
    cat("\n")
    cat(sprintf("TIMING: \n"))
    cat(sprintf("Average time of day: %d:%d\n", result$typical_hour, result$typical_minute))
    for(i in 1:nrow(weekday_top)) {
    cat(sprintf("  %s: %.1f%% of sessions (%.1f%% of time)\n", 
                weekday_top$weekday[i],
                weekday_top$percent[i],
                weekday_top$time_percent[i]))
    }
    cat("\n")
    cat(sprintf("VARIANCE: \n"))
    cat(sprintf("Duration consistency: %.2f%% variance\n", result$duration_variance))
    cat(sprintf("Longest streak: %d consecutive days\n", streak_analysis$longest_streak))
    if(!is.na(streak_analysis$streak_start_date)) {
        cat(sprintf(" %s to %s\n", 
                   format(streak_analysis$streak_start_date, "%b %d"), 
                   format(streak_analysis$streak_end_date, "%b %d")))
    }
    cat(sprintf("Longest gap: %d days without activity\n", streak_analysis$longest_gap))
    if(!is.na(streak_analysis$gap_start_date)) {
        cat(sprintf(" %s to %s\n", 
                   format(streak_analysis$gap_start_date, "%b %d"), 
                   format(streak_analysis$gap_end_date, "%b %d")))
    }
    cat(sprintf("Longest gap under %s min: %d days without activity\n", streak_analysis$threshold_minutes, streak_analysis$longest_low_act_period))
    if(!is.na(streak_analysis$low_act_period_start_date)) {
        cat(sprintf(" %s to %s\n", 
                   format(streak_analysis$low_act_period_start_date, "%b %d"), 
                   format(streak_analysis$low_act_period_end_date, "%b %d")))
    }
    cat(sprintf("Average streak length: %.1f days\n", streak_analysis$average_streak))
    invisible(result)
}

total_summary("category", "Work")