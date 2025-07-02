# ====================================================================
# ACTIVITY TRACKING SYSTEM - OPTIMIZED VERSION
# ====================================================================

library(tidyverse)
library(lubridate)

# ====================================================================
# 1. DATA PIPELINE
# ====================================================================

#' Load and cache activity data
#' @param refresh Force refresh cache even if data exists
#' @return List with raw and processed data
get_activity_data <- function(refresh = FALSE) {
  # Use cached data if available and not forcing refresh
  if (exists("activity_cache", envir = .GlobalEnv) && !refresh) {
    return(get("activity_cache", envir = .GlobalEnv))
  }
  
  # Load raw data
  activities_raw <- read.csv("G:/My Drive/activitytracker/stt_records_automatic.csv")
  
  # Process raw data once
  activities_formatted <- activities_raw %>%
    rename(
      time_started_raw = "time.started",
      time_ended_raw = "time.ended",
      duration_minutes = "duration.minutes",
      category = "categories",
      activity = "activity.name"
    ) %>%
    mutate(
      time_started = ymd_hms(time_started_raw),
      time_ended = ymd_hms(time_ended_raw),
      date = as.Date(time_started),
      hour = hour(time_started),
      minutes_since_midnight = hour(time_started) * 60 + minute(time_started),
      month = format(date, "%m"),
      year = format(date, "%Y"),
      month_year = as.Date(paste(year, month, "01", sep = "-")),
      weekday = wday(date, label = TRUE)
    ) %>%
    select(
      activity, category, date, duration_minutes,
      time_started, time_ended, hour, month_year, weekday, minutes_since_midnight
    ) %>%
    arrange(date, time_started)
  
  # Pre-calculate monthly data for faster access
  monthly_by_activity <- activities_formatted %>%
    group_by(month_year, activity) %>%
    summarise(total_hours = sum(duration_minutes / 60, na.rm = TRUE), .groups = 'drop')
  
  monthly_by_category <- activities_formatted %>%
    group_by(month_year, category) %>%
    summarise(total_hours = sum(duration_minutes / 60, na.rm = TRUE), .groups = 'drop')
  
  # Helper function to calculate month-over-month changes
  calculate_changes <- function(df, group_col) {
    df %>%
      group_by(.data[[group_col]]) %>%
      arrange(.data[[group_col]], month_year) %>%
      mutate(
        change = total_hours - lag(total_hours),
        percent_change = (change / lag(total_hours)) * 100
      ) %>%
      ungroup()
  }
  
  changes_by_activity <- calculate_changes(monthly_by_activity, "activity")
  changes_by_category <- calculate_changes(monthly_by_category, "category")
  
  cache <- list(
    raw_data = activities_raw,
    formatted_data = activities_formatted,
    monthly_activity = monthly_by_activity,
    monthly_category = monthly_by_category,
    changes_activity = changes_by_activity,
    changes_category = changes_by_category
  )
  
  # Save to global environment
  assign("activity_cache", cache, envir = .GlobalEnv)
  
  return(cache)
}

#' Create an analysis dataset for a specific category/activity and date range
#' @param col_name Column to filter ("category" or "activity")
#' @param value Value to filter for
#' @param start_date Start date
#' @param end_date End date
#' @param threshold_minutes Threshold for low activity
#' @return List with filtered data and analysis datasets
prepare_analysis_data <- function(col_name, value, start_date, end_date, threshold_minutes = 120) {
  # Ensure dates are in Date format
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Get cached data
  cache <- get_activity_data()
  
  # Get all days in range
  date_range <- seq(start_date, end_date, by = "day")
  
  # Filter data for this specific activity/category and date range
  filtered_data <- cache$formatted_data %>%
    filter(.data[[col_name]] == value, between(date, start_date, end_date))
  
  # Create daily activity summary
  daily_summary <- filtered_data %>%
    group_by(date) %>%
    summarise(
      minutes = sum(duration_minutes, na.rm = TRUE),
      sessions = n(),
      .groups = 'drop'
    )
  
  # Create complete calendar with activity markers
  calendar_data <- tibble(date = date_range) %>%
    left_join(daily_summary, by = "date") %>%
    mutate(
      minutes = replace_na(minutes, 0),
      sessions = replace_na(sessions, 0),
      has_activity = minutes > 0,
      is_low_activity = minutes < threshold_minutes
    )
  
  # Get monthly data
  monthly_data <- if (col_name == "category") {
    cache$changes_category %>% filter(category == value)
  } else {
    cache$changes_activity %>% filter(activity == value)
  } %>% filter(between(month_year, start_date, end_date))
  
  # Return all prepared data
  list(
    filtered_data = filtered_data,
    daily_summary = daily_summary,
    calendar_data = calendar_data,
    monthly_data = monthly_data,
    date_range = date_range,
    threshold_minutes = threshold_minutes
  )
}

# ====================================================================
# 2. ANALYSIS FUNCTIONS
# ====================================================================

#' Calculate basic metrics
#' @param data Prepared data from prepare_analysis_data()
#' @return List of basic metrics
analyze_basic_metrics <- function(data) {
  # Extract key data elements
  filtered_data <- data$filtered_data
  calendar_data <- data$calendar_data
  
  # Calculate totals
  total_minutes <- sum(calendar_data$minutes)
  total_days <- nrow(calendar_data)
  activity_days <- sum(calendar_data$has_activity)
  total_sessions <- nrow(filtered_data)
  
  # Handle empty data
  if (total_sessions == 0) {
    return(list(
      total_minutes = 0,
      total_days = total_days,
      activity_days = 0,
      activity_percent = 0,
      avg_per_day = 0,
      avg_per_activity_day = 0,
      total_sessions = 0,
      avg_session_minutes = 0
    ))
  }
  
  # Calculate averages
  list(
    total_minutes = total_minutes,
    total_days = total_days,
    activity_days = activity_days,
    activity_percent = (activity_days / total_days) * 100,
    avg_per_day = total_minutes / total_days,
    avg_per_activity_day = if (activity_days > 0) total_minutes / activity_days else 0,
    total_sessions = total_sessions,
    avg_session_minutes = total_minutes / total_sessions
  )
}

#' Analyze timing patterns
#' @param data Prepared data from prepare_analysis_data()
#' @return List of timing metrics
analyze_timing <- function(data) {
  filtered_data <- data$filtered_data
  
  # Handle empty data
  if (nrow(filtered_data) == 0) {
    return(list(
      typical_minutes = NA,
      typical_hour = NA,
      typical_minute = NA,
      duration_variance = NA
    ))
  }
  
  # Calculate timing metrics
  typical_minutes <- median(filtered_data$minutes_since_midnight)
  typical_hour <- floor(typical_minutes / 60)
  typical_minute <- round(typical_minutes %% 60)
  
  # Calculate variance in duration
  duration_variance <- sd(filtered_data$duration_minutes) / mean(filtered_data$duration_minutes) * 100
  
  list(
    typical_minutes = typical_minutes,
    typical_hour = typical_hour,
    typical_minute = typical_minute,
    duration_variance = duration_variance
  )
}

#' Analyze weekday distribution
#' @param data Prepared data from prepare_analysis_data()
#' @return Dataframe of weekday distribution
analyze_weekday_distribution <- function(data) {
  filtered_data <- data$filtered_data
  
  # Handle empty data
  if (nrow(filtered_data) == 0) {
    return(data.frame())
  }
  
  # Calculate distribution by weekday
  filtered_data %>%
    group_by(weekday) %>%
    summarize(
      count = n(),
      total_minutes = sum(duration_minutes),
      .groups = 'drop'
    ) %>%
    mutate(
      percent = count / sum(count) * 100,
      time_percent = total_minutes / sum(total_minutes) * 100
    ) %>%
    arrange(desc(percent))
}

# Returns dates for the longest run of a specific value
get_run_dates <- function(calendar, runs, run_value, column) {
  # Find positions for the specified run value
  positions <- data.frame(
    value = runs$values,
    length = runs$lengths,
    end_pos = cumsum(runs$lengths),
    start_pos = cumsum(runs$lengths) - runs$lengths + 1
  )
  
  # Find the longest run
  max_length <- if (any(positions$value == run_value)) {
    max(positions$length[positions$value == run_value])
  } else {
    0
  }
  
  # If no runs found, return NA
  if (max_length == 0) {
    return(list(start_date = NA, end_date = NA))
  }
  
  # Find the position of the longest run
  max_idx <- which(positions$value == run_value & positions$length == max_length)[1]
  
  # Get start and end positions
  start_pos <- positions$start_pos[max_idx]
  end_pos <- positions$end_pos[max_idx]
  
  # Get dates from positions
  list(
    start_date = calendar$date[start_pos],
    end_date = calendar$date[end_pos]
  )
}

#' Analyze streaks and gaps
#' @param data Prepared data from prepare_analysis_data()
#' @return List of streak and gap metrics
analyze_streaks_and_gaps <- function(data) {
  calendar_data <- data$calendar_data
  
  # Use run length encoding for efficient streak/gap analysis
  activity_runs <- rle(calendar_data$has_activity)
  low_activity_runs <- rle(calendar_data$is_low_activity)
  
  # Extract streaks and gaps
  streaks <- activity_runs$lengths[activity_runs$values]
  gaps <- activity_runs$lengths[!activity_runs$values]
  low_activity_periods <- low_activity_runs$lengths[low_activity_runs$values]
  
  # Find longest streak/gap/low period
  longest_streak <- if (length(streaks) > 0) max(streaks) else 0
  longest_gap <- if (length(gaps) > 0) max(gaps) else 0
  longest_low_period <- if (length(low_activity_periods) > 0) max(low_activity_periods) else 0
  average_streak <- if (length(streaks) > 0) mean(streaks) else 0
  
  # Find dates for longest streak
  streak_dates <- get_run_dates(calendar_data, activity_runs, TRUE, "has_activity")
  gap_dates <- get_run_dates(calendar_data, activity_runs, FALSE, "has_activity")
  low_period_dates <- get_run_dates(calendar_data, low_activity_runs, TRUE, "is_low_activity")
  
  list(
    longest_streak = longest_streak,
    average_streak = average_streak,
    longest_gap = longest_gap,
    longest_low_period = longest_low_period,
    streak_dates = streak_dates,
    gap_dates = gap_dates,
    low_period_dates = low_period_dates,
    threshold_minutes = data$threshold_minutes
  )
}

#' Analyze trends
#' @param data Prepared data from prepare_analysis_data()
#' @return List of trend metrics
analyze_trends <- function(data) {
  monthly_data <- data$monthly_data
  
  # Calculate 3-month trend
  three_month_trend <- if (nrow(monthly_data) >= 3) {
    mean(tail(monthly_data, 3)$percent_change, na.rm = TRUE)
  } else { NA_real_ }
  
  # Calculate recent change (last 30 days vs previous 30 days)
  cal_data <- data$calendar_data
  recent_change <- NA_real_
  if (nrow(cal_data) >= 60) {
    end_date <- max(cal_data$date)
    recent_data <- cal_data %>% filter(date > (end_date - 30))
    previous_data <- cal_data %>% filter(date <= (end_date - 30) & date > (end_date - 60))
    
    recent_total <- sum(recent_data$minutes)
    previous_total <- sum(previous_data$minutes)
    
    if (previous_total > 0) {
      recent_change <- ((recent_total - previous_total) / previous_total) * 100
    }
  }
  
  list(three_month_trend = three_month_trend, recent_change = recent_change)
}

#' Analyze correlations with other activities/categories
#' @param col_name Column to analyze ("category" or "activity")
#' @param value Value to analyze
#' @param start_date Start date
#' @param end_date End date
#' @return Dataframe of correlations
analyze_correlations <- function(col_name, value, start_date, end_date) {
  # Get all data
  cache <- get_activity_data()
  all_data <- cache$formatted_data
  
  # Find related items
  if (col_name == "category") {
    related_items <- unique(all_data$category[all_data$category != value])
  } else {
    # Get category of this activity
    target_category <- all_data %>%
      filter(activity == value) %>%
      slice(1) %>%
      pull(category)
    
    # Get activities in same category
    related_items <- unique(all_data$activity[all_data$activity != value & 
                                              all_data$category == target_category])
  }
  
  if (length(related_items) == 0) {
    return(data.frame())
  }
  
  # Efficient matrix-based correlation calculation
  daily_matrix <- all_data %>%
    filter(between(date, as.Date(start_date), as.Date(end_date))) %>%
    mutate(item = .data[[col_name]]) %>%
    filter(item %in% c(value, related_items)) %>%
    group_by(date, item) %>%
    summarise(minutes = sum(duration_minutes), .groups = 'drop') %>%
    pivot_wider(names_from = item, values_from = minutes, values_fill = 0)
  
  if (nrow(daily_matrix) < 5 || !value %in% names(daily_matrix)) return(data.frame())
  
  correlations <- map_dbl(related_items, function(item) {
    if (item %in% names(daily_matrix)) {
      cor(daily_matrix[[value]], daily_matrix[[item]], use = "complete.obs")
    } else { NA_real_ }
  })
  
  data.frame(item = related_items, correlation = correlations) %>%
    filter(!is.na(correlation)) %>%
    mutate(
      correlation_strength = abs(correlation),
      direction = ifelse(correlation > 0, "positive", "negative"),
      strength = case_when(
        correlation_strength > 0.7 ~ "very strong",
        correlation_strength > 0.5 ~ "strong",
        correlation_strength > 0.3 ~ "moderate",
        TRUE ~ "weak"
      )
    ) %>%
    arrange(desc(correlation_strength))
}

# Analyzes sequence patterns (what happens before/after activities)
analyze_sequences <- function(col_name, value, start_date, end_date) {
  cache <- get_activity_data()
  all_data <- cache$formatted_data
  
  activities_in_range <- all_data %>%
    filter(between(date, as.Date(start_date), as.Date(end_date))) %>%
    arrange(date, time_started) %>%
    select(date, time_started, activity, category)
  
  sequence_data <- activities_in_range %>%
    mutate(
      prev_activity = lag(activity),
      next_activity = lead(activity),
      same_day_prev = lag(date) == date,
      same_day_next = lead(date) == date
    ) %>%
    mutate(
      prev_activity = ifelse(same_day_prev, prev_activity, NA),
      next_activity = ifelse(same_day_next, next_activity, NA)
    )
  
  target_sessions <- sequence_data %>% filter(.data[[col_name]] == value)
  if (nrow(target_sessions) == 0) {
    return(list(preceding = data.frame(), following = data.frame(),
                cluster_rate = 0, total_sessions = 0))
  }
  
  preceding <- target_sessions %>%
    filter(!is.na(prev_activity)) %>%
    count(prev_activity, name = "count") %>%
    mutate(percent = count / sum(count) * 100) %>%
    arrange(desc(count)) %>% head(5)
  
  following <- target_sessions %>%
    filter(!is.na(next_activity)) %>%
    count(next_activity, name = "count") %>%
    mutate(percent = count / sum(count) * 100) %>%
    arrange(desc(count)) %>% head(5)
  
  target_sessions <- target_sessions %>%
    arrange(date, time_started) %>%
    mutate(
      time_since_last = as.numeric(difftime(time_started, lag(time_started), units = "hours")),
      is_clustered = !is.na(time_since_last) & time_since_last <= 2
    )
  
  cluster_rate <- if (nrow(target_sessions) > 1) {
    sum(target_sessions$is_clustered, na.rm = TRUE) / (nrow(target_sessions) - 1) * 100
  } else { 0 }
  
  list(preceding = preceding, following = following,
       cluster_rate = cluster_rate, total_sessions = nrow(target_sessions))
}

# ====================================================================
# 3. OUTPUT FUNCTIONS
# ====================================================================

#' Format time for display
#' @param minutes Minutes to format
#' @return Formatted string
format_time <- function(minutes) {
  if (is.na(minutes) || minutes < 1) {
    return("0 minutes")
  }
  
  if (minutes < 60) {
    return(sprintf("%.1f minutes", minutes))
  }
  
  hours <- floor(minutes / 60)
  mins <- round(minutes %% 60)
  
  if (mins == 0) {
    return(sprintf("%d hours", hours))
  } else {
    return(sprintf("%d hours %d minutes", hours, mins))
  }
}

#' Format date for display
#' @param date Date to format
#' @return Formatted string
format_date <- function(date) {
  if (is.na(date)) {
    return("N/A")
  }
  format(date, "%b %d")
}

#' Main summary function
#' @param col_name Column to analyze ("category" or "activity")
#' @param value Value to analyze
#' @param end_date End date
#' @param start_date Start date
#' @return Invisible list of all analysis results
total_summary <- function(col_name, value, end_date = Sys.Date(), start_date = "2024-01-22") {
  # Prepare all data
  data <- prepare_analysis_data(col_name, value, start_date, end_date)
  
  # Run all analyses
  basic <- analyze_basic_metrics(data)
  timing <- analyze_timing(data)
  weekday <- analyze_weekday_distribution(data)
  streaks <- analyze_streaks_and_gaps(data)
  trends <- analyze_trends(data)
  sequences <- analyze_sequences(col_name, value, start_date, end_date)
  correlations <- analyze_correlations(col_name, value, start_date, end_date)
  
  # Prepare display values
  if (basic$total_minutes < 60) {
    display_value <- basic$total_minutes
    display_unit <- "minutes"
  } else {
    display_value <- basic$total_minutes / 60
    display_unit <- "hours"
  }
  
  # OUTPUT
  cat("------------------------------------------\n")
  cat(sprintf("SUMMARY FOR: %s\n", toupper(value)))
  cat(start_date, "to", end_date, "\n")
  cat("------------------------------------------\n")
  cat("\n")
  
  # BASIC METRICS
  cat("BASIC METRICS:\n")
  cat(sprintf("Total time spent: %.2f %s\n", display_value, display_unit))
  cat(sprintf("Days with activity: %d (%.2f%%)\n", 
              basic$activity_days, basic$activity_percent))
  cat(sprintf("Average time per day (total): %s\n", 
              format_time(basic$avg_per_day)))
  cat(sprintf("Average time per day (days done): %s\n", 
              format_time(basic$avg_per_activity_day)))
  cat(sprintf("Total sessions: %d (avg %.2f minutes)\n", 
              basic$total_sessions, basic$avg_session_minutes))
  cat("\n")
  
  # TIMING
  if (!is.na(timing$typical_hour)) {
    cat("TIMING:\n")
    cat(sprintf("Typical start time: %02d:%02d\n", timing$typical_hour, timing$typical_minute))
    cat(sprintf("Duration variance: %.1f%%\n", timing$duration_variance))
    cat("\n")
  }
  
  # WEEKDAY DISTRIBUTION
  if (nrow(weekday) > 0) {
    cat("WEEKDAY DISTRIBUTION:\n")
    for (i in 1:nrow(weekday)) {
      cat(sprintf("%s: %d sessions, %.1f%% of total\n", 
                  weekday$weekday[i], 
                  weekday$count[i], 
                  weekday$time_percent[i]))
    }
    cat("\n")
  }
  
  # STREAKS AND GAPS
  cat("STREAKS AND GAPS:\n")
  cat(sprintf("Longest activity streak: %d days\n", streaks$longest_streak))
  cat(sprintf("Longest gap: %d days\n", streaks$longest_gap))
  cat(sprintf("Longest low activity period: %d days\n", streaks$longest_low_period))
  cat("\n")
  
  # TRENDS
  cat("TRENDS:\n")
  cat(sprintf("3-month trend: %.2f%%", trends$three_month_trend))
  if (!is.na(trends$recent_change)) {
    cat(sprintf(" (recent change: %.2f%%)", trends$recent_change))
  }
  cat("\n\n")
  
  # SEQUENCES
  if (nrow(sequences$preceding) > 0) {
    cat("TOP 5 PRECEDING ACTIVITIES:\n")
    for (i in 1:nrow(sequences$preceding)) {
      cat(sprintf("%s: %d times (%.1f%%)\n", 
                  sequences$preceding$prev_activity[i], 
                  sequences$preceding$count[i], 
                  sequences$preceding$percent[i]))
    }
    cat("\n")
  }
  
  if (nrow(sequences$following) > 0) {
    cat("TOP 5 FOLLOWING ACTIVITIES:\n")
    for (i in 1:nrow(sequences$following)) {
      cat(sprintf("%s: %d times (%.1f%%)\n", 
                  sequences$following$next_activity[i], 
                  sequences$following$count[i], 
                  sequences$following$percent[i]))
    }
    cat("\n")
  }
  
  cat(sprintf("Clustering rate: %.1f%%\n", sequences$cluster_rate))
  
  # CORRELATIONS
  if (nrow(correlations) > 0) {
    cat("\nCORRELATIONS WITH OTHER ITEMS:\n")
    for (i in 1:nrow(correlations)) {
      cat(sprintf("%s: %.2f (%s, %s)\n", 
                  correlations$item[i], 
                  correlations$correlation[i], 
                  correlations$strength[i], 
                  correlations$direction[i]))
    }
  }
  
  invisible(list(
    basic = basic,
    timing = timing,
    weekday = weekday,
    streaks = streaks,
    trends = trends,
    sequences = sequences,
    correlations = correlations
  ))
}

# Gets monthly changes for a specific category/activity 
monthly_change <- function(col_name, value) {
  cache <- get_activity_data()
  
  if (col_name == "category") {
    cache$changes_category %>% filter(category == value)
  } else {
    cache$changes_activity %>% filter(activity == value)
  }
}

# Calculates total minutes for a specific category/activity in a date range
total_minutes <- function(col_name, value, end_date = Sys.Date(), start_date = "2024-01-22") {
  cache <- get_activity_data()
  
  cache$formatted_data %>%
    filter(.data[[col_name]] == value) %>%
    filter(between(date, as.Date(start_date), as.Date(end_date))) %>%
    summarise(total = sum(duration_minutes, na.rm = TRUE)) %>%
    pull(total)
}

# Forces data refresh from source
refresh_data <- function() {
  get_activity_data(refresh = TRUE)
  cat("Data refreshed successfully\n")
}

# Initialize data on load
get_activity_data()