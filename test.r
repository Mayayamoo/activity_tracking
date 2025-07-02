# PACKAGES
library(tidyverse)
library(lubridate)

# ============================================================================
# CORE DATA PIPELINE (SINGLE SOURCE OF TRUTH)
# ============================================================================

# Raw data processing
load_and_process_data <- function(file_path = "G:/My Drive/activitytracker/stt_records_automatic.csv") {
    read.csv(file_path) %>%
        rename(
            time_started_raw = time.started,
            time_ended_raw = time.ended,
            duration_minutes = duration.minutes,
            category = categories,
            activity = activity.name
        ) %>%
        mutate(
            time_started = ymd_hms(time_started_raw),
            time_ended = ymd_hms(time_ended_raw),
            date = as.Date(time_started),
            hour = hour(time_started),
            month_year = floor_date(date, "month"),
            weekday = wday(date, label = TRUE),
            minutes_since_midnight = hour(time_started) * 60 + minute(time_started)
        ) %>%
        select(activity, category, date, duration_minutes, time_started, 
               hour, month_year, weekday, minutes_since_midnight) %>%
        arrange(date, time_started)
}

# ============================================================================
# EFFICIENT DATA PREPARATION ENGINE
# ============================================================================

create_analysis_cache <- function(activities_data, col_name, value, start_date, end_date) {
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
    date_range <- seq(start_date, end_date, by = "day")
    
    # Single-pass filtered data
    filtered_data <- activities_data %>%
        filter(.data[[col_name]] == value, between(date, start_date, end_date))
    
    if(nrow(filtered_data) == 0) {
        return(list(
            filtered_data = filtered_data,
            daily_summary = data.frame(),
            monthly_summary = data.frame(),
            calendar_data = data.frame(date = date_range, has_activity = FALSE, daily_minutes = 0),
            metrics = list(total_days = length(date_range), activity_days = 0, total_minutes = 0)
        ))
    }
    
    # Daily aggregations (single pass)
    daily_summary <- filtered_data %>%
        group_by(date) %>%
        summarise(
            daily_minutes = sum(duration_minutes),
            sessions = n(),
            avg_session_length = mean(duration_minutes),
            first_session = min(minutes_since_midnight),
            last_session = max(minutes_since_midnight),
            .groups = 'drop'
        )
    
    # Monthly aggregations (single pass)
    monthly_summary <- filtered_data %>%
        group_by(month_year) %>%
        summarise(
            total_hours = sum(duration_minutes) / 60,
            sessions = n(),
            .groups = 'drop'
        ) %>%
        arrange(month_year) %>%
        mutate(
            mom_change = total_hours - lag(total_hours),
            mom_percent = (mom_change / lag(total_hours)) * 100
        )
    
    # Calendar alignment
    calendar_data <- data.frame(date = date_range) %>%
        left_join(daily_summary, by = "date") %>%
        mutate(
            daily_minutes = replace_na(daily_minutes, 0),
            sessions = replace_na(sessions, 0),
            has_activity = daily_minutes > 0
        )
    
    # Core metrics
    metrics <- list(
        total_days = length(date_range),
        activity_days = sum(calendar_data$has_activity),
        total_minutes = sum(calendar_data$daily_minutes),
        total_sessions = nrow(filtered_data)
    )
    
    list(
        filtered_data = filtered_data,
        daily_summary = daily_summary,
        monthly_summary = monthly_summary,
        calendar_data = calendar_data,
        metrics = metrics
    )
}

# ============================================================================
# UNIFIED ANALYSIS ENGINE
# ============================================================================

# Function to get dates for streaks and gaps
get_run_dates <- function(calendar_data, runs, run_value) {
    positions <- data.frame(
        value = runs$values,
        length = runs$lengths,
        end_pos = cumsum(runs$lengths),
        start_pos = cumsum(runs$lengths) - runs$lengths + 1
    )
    
    max_length <- if (any(positions$value == run_value)) {
        max(positions$length[positions$value == run_value])
    } else { 0 }
    
    if (max_length == 0) return(list(start_date = NA, end_date = NA))
    
    max_idx <- which(positions$value == run_value & positions$length == max_length)[1]
    start_pos <- positions$start_pos[max_idx]
    end_pos <- positions$end_pos[max_idx]
    
    list(
        start_date = calendar_data$date[start_pos], 
        end_date = calendar_data$date[end_pos]
    )
}

# Enhanced analyze_patterns to include streak dates and low activity tracking
analyze_patterns <- function(cache_data, threshold_minutes = 120) {
    fd <- cache_data$filtered_data
    cd <- cache_data$calendar_data
    metrics <- cache_data$metrics
    
    if(nrow(fd) == 0) {
        return(list(
            basic = list(coverage_percent = 0, avg_per_day = 0),
            timing = list(typical_hour = NA, typical_minute = NA),
            consistency = list(duration_cv = NA, clustering_rate = 0),
            streaks = list(longest = 0, avg = 0, longest_gap = 0, longest_low = 0),
            weekday = data.frame(),
            trends = list(three_month = NA, recent_change = NA)
        ))
    }
    
    # BASIC METRICS
    basic <- list(
        coverage_percent = (metrics$activity_days / metrics$total_days) * 100,
        avg_per_day_total = metrics$total_minutes / metrics$total_days,
        avg_per_day_active = metrics$total_minutes / metrics$activity_days,
        avg_session_length = metrics$total_minutes / metrics$total_sessions
    )
    
    # TIMING PATTERNS (vectorized)
    timing_stats <- fd %>%
        summarise(
            typical_minutes = median(minutes_since_midnight),
            duration_cv = sd(duration_minutes) / mean(duration_minutes) * 100,
            .groups = 'drop'
        )
    
    timing <- list(
        typical_hour = floor(timing_stats$typical_minutes / 60),
        typical_minute = round(timing_stats$typical_minutes %% 60),
        duration_cv = timing_stats$duration_cv
    )
    
    # WEEKDAY DISTRIBUTION (single pass) - Now includes both time and session percentages
    weekday <- fd %>%
        group_by(weekday) %>%
        summarise(
            sessions = n(),
            total_time = sum(duration_minutes),
            .groups = 'drop'
        ) %>%
        mutate(
            session_percent = sessions / sum(sessions) * 100,
            time_percent = total_time / sum(total_time) * 100
        ) %>%
        arrange(desc(session_percent))
    
    # STREAKS AND GAPS (efficient RLE)
    activity_runs <- rle(cd$has_activity)
    low_activity_runs <- rle(cd$daily_minutes < threshold_minutes)
    
    streaks_data <- activity_runs$lengths[activity_runs$values]
    gaps_data <- activity_runs$lengths[!activity_runs$values]
    low_periods <- low_activity_runs$lengths[low_activity_runs$values]
    
    # Get dates for streaks and gaps
    streak_dates <- get_run_dates(cd, activity_runs, TRUE)
    gap_dates <- get_run_dates(cd, activity_runs, FALSE)
    low_period_dates <- get_run_dates(cd, low_activity_runs, TRUE)
    
    streaks <- list(
        longest = if(length(streaks_data) > 0) max(streaks_data) else 0,
        avg = if(length(streaks_data) > 0) mean(streaks_data) else 0,
        longest_gap = if(length(gaps_data) > 0) max(gaps_data) else 0,
        longest_low = if(length(low_periods) > 0) max(low_periods) else 0,
        streak_dates = streak_dates,
        gap_dates = gap_dates,
        low_period_dates = low_period_dates,
        threshold_minutes = threshold_minutes
    )
    
    # CLUSTERING (vectorized time diff)
    if(nrow(fd) > 1) {
        fd_sorted <- fd %>% arrange(date, time_started)
        time_diffs <- as.numeric(diff(fd_sorted$time_started), units = "hours")
        clustering_rate <- sum(time_diffs <= 2, na.rm = TRUE) / length(time_diffs) * 100
    } else {
        clustering_rate <- 0
    }
    
    # TRENDS (from monthly data)
    monthly_data <- cache_data$monthly_summary
    
    # Calculate Month-over-Month change
    recent_end <- max(cd$date)
    recent_start <- recent_end - 29  # Last 30 days
    previous_end <- recent_start - 1
    previous_start <- previous_end - 29  # Previous 30 days
    
    recent_change <- NA
    if (nrow(cd) >= 60) {
        recent_days <- cd %>% filter(date > recent_start)
        previous_days <- cd %>% filter(date <= recent_start & date > previous_start)
        
        recent_avg <- sum(recent_days$daily_minutes) / 30
        previous_avg <- sum(previous_days$daily_minutes) / 30
        
        if(previous_avg > 0) {
            recent_change <- ((recent_avg - previous_avg) / previous_avg) * 100
        }
    }
    
    trends <- list(
        three_month = if(nrow(monthly_data) >= 3) {
            recent_months <- tail(monthly_data, 3)
            mean(recent_months$mom_percent, na.rm = TRUE)
        } else NA,
        recent_change = recent_change
    )
    
    list(
        basic = basic,
        timing = timing,
        consistency = list(duration_cv = timing$duration_cv, clustering_rate = clustering_rate),
        streaks = streaks,
        weekday = weekday,
        trends = trends
    )
}

# ============================================================================
# EFFICIENT CORRELATION ENGINE
# ============================================================================

calculate_correlations <- function(activities_data, col_name, value, start_date, end_date) {
    # Get related items - MODIFIED to include more activities
    if(col_name == "category") {
        related_items <- activities_data %>% 
            filter(category != value) %>% 
            distinct(category) %>% 
            pull(category)
        filter_col <- "category"
    } else {
        # Get current category
        target_category <- activities_data %>% 
            filter(activity == value) %>% 
            slice(1) %>% 
            pull(category)
            
        # Include both same-category activities AND top activities from other categories
        same_category_items <- activities_data %>% 
            filter(category == target_category, activity != value) %>% 
            distinct(activity) %>% 
            pull(activity)
            
        # Add top activities from other categories (most frequent)
        other_top_activities <- activities_data %>%
            filter(category != target_category) %>%
            count(activity, sort = TRUE) %>%
            head(10) %>%
            pull(activity)
            
        # Combine both sets
        related_items <- c(same_category_items, other_top_activities)
        filter_col <- "activity"
    }
    
    if(length(related_items) == 0) return(data.frame())
    
    # Create daily matrix in single operation
    daily_matrix <- activities_data %>%
        filter(.data[[filter_col]] %in% c(value, related_items)) %>%
        filter(between(date, as.Date(start_date), as.Date(end_date))) %>%
        group_by(date, .data[[filter_col]]) %>%
        summarise(daily_minutes = sum(duration_minutes), .groups = 'drop') %>%
        pivot_wider(names_from = all_of(filter_col), values_from = daily_minutes, values_fill = 0)
    
    if(nrow(daily_matrix) < 5 || !value %in% names(daily_matrix)) return(data.frame())
    
    # Vectorized correlation calculation
    target_vector <- daily_matrix[[value]]
    correlations <- map_dbl(related_items, ~{
        if(.x %in% names(daily_matrix)) {
            cor(target_vector, daily_matrix[[.x]], use = "complete.obs")
        } else NA
    })
    
    # Add category info to correlation output
    correlation_df <- data.frame(
        item = related_items,
        correlation = correlations
    ) %>%
        filter(!is.na(correlation))
    
    # Add category for each correlated item
    correlation_df$item_category <- sapply(correlation_df$item, function(act) {
        if(col_name == "category") {
            return("N/A")
        } else {
            cat_result <- activities_data %>% 
                filter(activity == act) %>% 
                slice(1) %>% 
                pull(category)
            return(ifelse(length(cat_result) > 0, cat_result, "Unknown"))
        }
    })
    
    # Complete the data frame with strength and direction
    correlation_df %>%
        mutate(
            strength = case_when(
                abs(correlation) > 0.7 ~ "very strong",
                abs(correlation) > 0.5 ~ "strong",
                abs(correlation) > 0.3 ~ "moderate",
                TRUE ~ "weak"
            ),
            direction = ifelse(correlation > 0, "positive", "negative")
        ) %>%
        arrange(desc(abs(correlation)))
}

# ============================================================================
# SEQUENCE ANALYSIS (EFFICIENT)
# ============================================================================

analyze_sequences <- function(activities_data, col_name, value, start_date, end_date) {
    # Get all activities in date range, add sequence info in single pass
    sequences <- activities_data %>%
        filter(between(date, as.Date(start_date), as.Date(end_date))) %>%
        arrange(date, time_started) %>%
        mutate(
            prev_activity = lag(activity),
            next_activity = lead(activity),
            same_day_prev = lag(date) == date,
            same_day_next = lead(date) == date,
            prev_activity = ifelse(same_day_prev %in% TRUE, prev_activity, NA),
            next_activity = ifelse(same_day_next %in% TRUE, next_activity, NA)
        ) %>%
        filter(.data[[col_name]] == value)
    
    if(nrow(sequences) == 0) {
        return(list(preceding = data.frame(), following = data.frame()))
    }
    
    # Efficient counting
    preceding <- sequences %>%
        filter(!is.na(prev_activity)) %>%
        count(prev_activity, name = "count") %>%
        mutate(percent = count / sum(count) * 100) %>%
        arrange(desc(count)) %>%
        head(5)
    
    following <- sequences %>%
        filter(!is.na(next_activity)) %>%
        count(next_activity, name = "count") %>%
        mutate(percent = count / sum(count) * 100) %>%
        arrange(desc(count)) %>%
        head(5)
    
    list(preceding = preceding, following = following)
}

# ============================================================================
# UNIFIED OUTPUT ENGINE
# ============================================================================

format_time_display <- function(minutes) {
    if(minutes < 60) {
        sprintf("%.1f minutes", minutes)
    } else {
        hours <- floor(minutes / 60)
        mins <- round(minutes %% 60)
        sprintf("%d hours %d minutes", hours, mins)
    }
}

# Format date for display
format_date <- function(date) {
    if(is.na(date)) return("N/A")
    format(date, "%b %d")
}

# Enhanced output with all requested features
comprehensive_summary <- function(col_name, value, end_date = Sys.Date(), start_date = "2024-01-22") {
    # Load data once
    activities_data <- load_and_process_data()
    
    # Get target category if analyzing an activity
    target_category <- if(col_name == "activity") {
        activities_data %>% 
            filter(activity == value) %>% 
            slice(1) %>% 
            pull(category)
    } else {
        value  # If analyzing a category, target_category is the value itself
    }
    
    # Create analysis cache
    cache <- create_analysis_cache(activities_data, col_name, value, start_date, end_date)
    
    # Run all analyses
    patterns <- analyze_patterns(cache)
    correlations <- calculate_correlations(activities_data, col_name, value, start_date, end_date)
    sequences <- analyze_sequences(activities_data, col_name, value, start_date, end_date)
    
    # Efficient output formatting
    total_display <- if(cache$metrics$total_minutes < 60) {
        list(value = cache$metrics$total_minutes, unit = "minutes")
    } else {
        list(value = cache$metrics$total_minutes / 60, unit = "hours")
    }
    
    # Format time for active days average
    active_days_avg <- if(cache$metrics$activity_days > 0) {
        format_time_display(cache$metrics$total_minutes / cache$metrics$activity_days)
    } else {
        "0 minutes"
    }
    
    # CONSOLIDATED OUTPUT
    cat("==========================================\n")
    cat(sprintf("ANALYSIS: %s\n", toupper(value)))
    cat(sprintf("%s to %s\n", start_date, end_date))
    cat("==========================================\n\n")
    
    cat("OVERVIEW:\n")
    cat(sprintf("  Total time: %.2f %s\n", total_display$value, total_display$unit))
    cat(sprintf("  Coverage: %d/%d days (%.2f%%)\n", 
               cache$metrics$activity_days, cache$metrics$total_days, patterns$basic$coverage_percent))
    cat(sprintf("  Sessions: %d (avg %.2f min)\n", 
               cache$metrics$total_sessions, patterns$basic$avg_session_length))
    cat(sprintf("  Daily average (all days): %s\n", format_time_display(patterns$basic$avg_per_day_total)))
    cat(sprintf("  Daily average (active days only): %s\n", active_days_avg))
    
    if(!is.na(patterns$timing$typical_hour)) {
        cat("\nPATTERNS:\n")
        cat(sprintf("  Typical time: %02d:%02d\n", patterns$timing$typical_hour, patterns$timing$typical_minute))
        cat(sprintf("  Consistency: %.1f%% variation\n", patterns$consistency$duration_cv))
        cat(sprintf("  Clustering: %.1f%% within 2 hours\n", patterns$consistency$clustering_rate))
        
        if(nrow(patterns$weekday) > 0) {
            cat("  Day of week distribution:\n")
            # Display all weekdays with both session and time percentages
            for(i in 1:nrow(patterns$weekday)) {
                cat(sprintf("    %s: %.1f%% of sessions (%.1f%% of time)\n", 
                           patterns$weekday$weekday[i], 
                           patterns$weekday$session_percent[i],
                           patterns$weekday$time_percent[i]))
            }
        }
    }
    
    cat("\nCONSISTENCY:\n")
    cat(sprintf("  Longest streak: %d days", patterns$streaks$longest))
    if(!is.na(patterns$streaks$streak_dates$start_date)) {
        cat(sprintf(" (%s to %s)", 
                   format_date(patterns$streaks$streak_dates$start_date),
                   format_date(patterns$streaks$streak_dates$end_date)))
    }
    cat("\n")
    
    cat(sprintf("  Average streak: %.1f days\n", patterns$streaks$avg))
    
    cat(sprintf("  Longest gap: %d days", patterns$streaks$longest_gap))
    if(!is.na(patterns$streaks$gap_dates$start_date)) {
        cat(sprintf(" (%s to %s)", 
                   format_date(patterns$streaks$gap_dates$start_date),
                   format_date(patterns$streaks$gap_dates$end_date)))
    }
    cat("\n")
    
    cat(sprintf("  Longest period under %d min: %d days", 
               patterns$streaks$threshold_minutes, patterns$streaks$longest_low))
    if(!is.na(patterns$streaks$low_period_dates$start_date)) {
        cat(sprintf(" (%s to %s)", 
                   format_date(patterns$streaks$low_period_dates$start_date),
                   format_date(patterns$streaks$low_period_dates$end_date)))
    }
    cat("\n")
    
    cat("\nTRENDS:\n")
    if(!is.na(patterns$trends$three_month)) {
        cat(sprintf("  3-month trend: %+.1f%%\n", patterns$trends$three_month))
    } else {
        cat("  3-month trend: Not enough data\n")
    }
    
    if(!is.na(patterns$trends$recent_change)) {
        cat(sprintf("  Recent change (MoM): %+.1f%%\n", patterns$trends$recent_change))
    } else {
        cat("  Recent change (MoM): Not enough data\n")
    }
    
    if(nrow(sequences$preceding) > 0) {
        cat("\nSEQUENCES:\n")
        cat("  Common before:\n")
        for(i in 1:min(5, nrow(sequences$preceding))) {
            cat(sprintf("    %s (%.1f%%)\n", sequences$preceding$prev_activity[i], sequences$preceding$percent[i]))
        }
        if(nrow(sequences$following) > 0) {
            cat("  Common after:\n")
            for(i in 1:min(5, nrow(sequences$following))) {
                cat(sprintf("    %s (%.1f%%)\n", sequences$following$next_activity[i], sequences$following$percent[i]))
            }
        }
    }
    
    if(nrow(correlations) > 0) {
        cat("\nCORRELATIONS:\n")
        
        # Show all correlations sorted by absolute correlation value
        # Already sorted by abs(correlation) in calculate_correlations function
        
        # For activities - show category info
        if(col_name == "activity") {
            for(i in 1:min(8, nrow(correlations))) {
                cat(sprintf("  %s (%s): %s %s (%.3f)\n", 
                          correlations$item[i], 
                          correlations$item_category[i], 
                          correlations$strength[i], 
                          correlations$direction[i], 
                          correlations$correlation[i]))
            }
        } else {
            # For categories - no need for category info
            for(i in 1:min(8, nrow(correlations))) {
                cat(sprintf("  %s: %s %s (%.3f)\n", 
                          correlations$item[i], 
                          correlations$strength[i], 
                          correlations$direction[i], 
                          correlations$correlation[i]))
            }
        }
    }
    
    cat("\n")
    invisible(list(cache = cache, patterns = patterns, correlations = correlations, sequences = sequences))
}

# ============================================================================
# USAGE
# ============================================================================

# Main function - replaces total_summary
total_summary <- comprehensive_summary

# Quick functions for specific analyses
quick_monthly_changes <- function(col_name, value) {
    activities_data <- load_and_process_data()
    cache <- create_analysis_cache(activities_data, col_name, value, "2024-01-01", Sys.Date())
    cache$monthly_summary
}

quick_correlations <- function(col_name, value) {
    activities_data <- load_and_process_data()
    calculate_correlations(activities_data, col_name, value, "2024-01-01", Sys.Date())
}

# Function to create a comparison data frame of all activities
compare_activities <- function(start_date = "2024-01-22", end_date = Sys.Date(), 
                               filter_category = NULL) {
  # Get all data
  activities_data <- load_and_process_data()
  
  # Get list of all unique activities, optionally filtered by category
  all_activities <- if(!is.null(filter_category)) {
    activities_data %>%
      filter(category == filter_category) %>%
      distinct(activity) %>%
      pull(activity)
  } else {
    unique(activities_data$activity)
  }
  
  # Initialize empty data frame
  results <- data.frame()
  
  # Process each activity
  for(current_activity in all_activities) {
    # Skip activities with no data to prevent errors
    activity_count <- activities_data %>%
      filter(activity == current_activity, between(date, as.Date(start_date), as.Date(end_date))) %>%
      nrow()
    
    if(activity_count == 0) next
    
    # Suppress console output during analysis
    invisible(capture.output({
      # Run comprehensive analysis
      analysis <- comprehensive_summary("activity", current_activity, end_date, start_date)
    }))
    
    # Extract metrics from returned object
    patterns <- analysis$patterns
    
    # Handle missing data safely
    if(is.null(patterns) || is.null(analysis$cache)) next
    
    # Get top correlation
    top_corr_item <- if(!is.null(analysis$correlations) && nrow(analysis$correlations) > 0) {
      analysis$correlations$item[1]
    } else { "None" }
    
    top_corr_value <- if(!is.null(analysis$correlations) && nrow(analysis$correlations) > 0) {
      analysis$correlations$correlation[1]
    } else { 0 }
    
    # Get top sequences
    top_before <- if(!is.null(analysis$sequences) && 
                     !is.null(analysis$sequences$preceding) && 
                     nrow(analysis$sequences$preceding) > 0) {
      analysis$sequences$preceding$prev_activity[1]
    } else { "None" }
    
    top_after <- if(!is.null(analysis$sequences) && 
                   !is.null(analysis$sequences$following) && 
                   nrow(analysis$sequences$following) > 0) {
      analysis$sequences$following$next_activity[1]
    } else { "None" }
    
    # Get category for this activity - FIXED HERE
    category <- activities_data %>%
      filter(activity == current_activity) %>%  # This was comparing the column to itself!
      slice(1) %>%
      pull(category)
    
    # Extract weekday information safely - FIX HERE
    weekday_data <- if(!is.null(patterns$weekday)) patterns$weekday else data.frame()
    weekday_top <- if(nrow(weekday_data) > 0) {
      as.character(weekday_data$weekday[1])
    } else { 
      "Unknown"  # Default value if no weekday data
    }
    
    # Create a row for this activity with proper error checks
    activity_row <- data.frame(
      activity = current_activity,  # CHANGED to use the loop variable
      category = category,
      total_minutes = ifelse(!is.null(analysis$cache$metrics$total_minutes), 
                            analysis$cache$metrics$total_minutes, 0),
      total_hours = ifelse(!is.null(analysis$cache$metrics$total_minutes), 
                          analysis$cache$metrics$total_minutes / 60, 0),
      total_days = ifelse(!is.null(analysis$cache$metrics$total_days), 
                         analysis$cache$metrics$total_days, 0),
      days_active = ifelse(!is.null(analysis$cache$metrics$activity_days), 
                          analysis$cache$metrics$activity_days, 0),
      coverage_percent = ifelse(!is.null(patterns$basic$coverage_percent), 
                               patterns$basic$coverage_percent, 0),
      total_sessions = ifelse(!is.null(analysis$cache$metrics$total_sessions), 
                             analysis$cache$metrics$total_sessions, 0),
      avg_session_minutes = ifelse(!is.null(patterns$basic$avg_session_length), 
                                  patterns$basic$avg_session_length, 0),
      avg_daily_minutes_all = ifelse(!is.null(patterns$basic$avg_per_day_total), 
                                    patterns$basic$avg_per_day_total, 0),
      avg_daily_minutes_active = ifelse(!is.null(patterns$basic$avg_per_day_active), 
                                       patterns$basic$avg_per_day_active, 0),
      typical_hour = ifelse(!is.null(patterns$timing$typical_hour), 
                           patterns$timing$typical_hour, NA),
      typical_minute = ifelse(!is.null(patterns$timing$typical_minute), 
                             patterns$timing$typical_minute, NA),
      duration_variation = ifelse(!is.null(patterns$consistency$duration_cv), 
                                 patterns$consistency$duration_cv, NA),
      clustering_percent = ifelse(!is.null(patterns$consistency$clustering_rate), 
                                 patterns$consistency$clustering_rate, 0),
      favorite_weekday = weekday_top,  # Now weekday_top is always defined
      longest_streak = ifelse(!is.null(patterns$streaks$longest), 
                             patterns$streaks$longest, 0),
      average_streak = ifelse(!is.null(patterns$streaks$avg), 
                             patterns$streaks$avg, 0),
      longest_gap = ifelse(!is.null(patterns$streaks$longest_gap), 
                          patterns$streaks$longest_gap, 0),
      longest_low_period = ifelse(!is.null(patterns$streaks$longest_low), 
                                 patterns$streaks$longest_low, 0),
      trend_3month = ifelse(!is.null(patterns$trends$three_month), 
                           patterns$trends$three_month, NA),
      trend_recent = ifelse(!is.null(patterns$trends$recent_change), 
                           patterns$trends$recent_change, NA),
      top_correlation_item = top_corr_item,
      top_correlation_value = top_corr_value,
      top_preceding_activity = top_before,
      top_following_activity = top_after
    )
    
    # Add to results
    results <- rbind(results, activity_row)
  }
  
  # Sort by total time
  results <- results %>% arrange(desc(total_hours))
  
  return(results)
}

# Compare all activities
all_activities <- compare_activities()
View(all_activities)
