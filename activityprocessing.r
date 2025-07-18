#Packages needed
library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(TraMineR)

#DEPENDENCIES
source("config.r")
source("data_munch.r")
source("utils.r")

# --- total_minutes function ---
total_minutes <- function(col_name, value, end_date, start_date) {
    total_minutes <- activities.formatted %>% 
        filter(!!sym(col_name) == value) %>%   # FIXED: use !!sym(col_name)
        filter(between(date, as.Date(start_date), as.Date(end_date))) %>%
        summarise(
            total_minutes = sum(duration, na.rm = TRUE)) %>%
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
            total_time = sum(duration),
            .groups = 'drop'
        ) %>%
        mutate(
            percent = count / sum(count) * 100,
            time_percent = total_time / sum(total_time) * 100
        )
}

analyze_streaks_and_gaps <- function(summary.data, end_date, start_date, threshold_minutes = 120) {
    # Validate inputs
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
    
    date_range <- seq(start_date, end_date, by = "day")

    daily_totals <- summary.data %>%
    group_by(date) %>%
    summarize(
        daily_minutes = sum(duration, na.rm = TRUE),
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

analyze_trends <- function(col_name, value, end_date, start_date) {
    # Use existing monthly.change function to get the basic trend data
    if(col_name == "category") {
        monthly_data <- monthly.change(mon.cat.tot.hrs, value)
    } else {
        monthly_data <- monthly.change(mon.act.tot.hrs, value)
    }
    
    # Filter to the date range we're analyzing
    monthly_data <- filter_time(monthly_data, start_date, end_date)

    # Calculate 3-month trend using existing data
    recent_months <- tail(monthly_data, 3)
    three_month_trend <- if(nrow(recent_months) >= 2) {
        mean(recent_months$percent_change, na.rm = TRUE)
    } else {
        NA
    }
    
    # Recent change calculation (last 30 days vs previous 30 days)
    recent_end <- as.Date(end_date)
    recent_start <- recent_end - 29  # This gives exactly 30 days
    previous_end <- recent_start - 1
    previous_start <- previous_end - 29  # This gives exactly 30 days

    recent_change_percent <- NA
    if(as.Date(start_date) <= previous_start) {
        recent_minutes <- total_minutes(col_name, value, recent_end, recent_start)
        previous_minutes <- total_minutes(col_name, value, previous_end, previous_start)
        
        # Both periods are exactly 30 days now
        recent_per_day <- recent_minutes / 30
        previous_per_day <- previous_minutes / 30
        
        if(previous_per_day > 0.01) {  # At least 0.01 minutes per day
            recent_change_percent <- ((recent_per_day - previous_per_day) / previous_per_day) * 100
        }
    }
    
    # Add sessions count to monthly data
    monthly_data_with_sessions <- monthly_data %>%
        rowwise() %>%
        mutate(
            sessions = nrow(activities.formatted %>%
                filter(!!sym(col_name) == value) %>%   # FIXED: use !!sym(col_name)
                filter(format(date, "%Y-%m") == format(month_year, "%Y-%m")))
        ) %>%
        ungroup()
    
    return(list(
        monthly = monthly_data_with_sessions,
        three_month_trend = three_month_trend,
        recent_change_percent = recent_change_percent
    ))
}

analyze_correlations <- function(col_name, value, end_date, start_date) {
    # Use existing all.changes function to get correlation data
    if(col_name == "category") {
        all_changes_data <- all.changes("category")
        related_items <- names(all_changes_data)[names(all_changes_data) != value]
    } else {
        all_changes_data <- all.changes("activity")
        # Get activities in the same category as the target activity
        target_category <- activities.formatted %>%
            filter(activity == value) %>%
            pull(category) %>%
            unique() %>%
            .[1]
        
        related_items <- activities.formatted %>%
            filter(category == target_category, activity != value) %>%
            pull(activity) %>%
            unique()
    }
    
    if(length(related_items) == 0) {
        return(data.frame(item = character(), correlation = numeric(), strength = character(), direction = character()))
    }
    
    # Create daily totals using existing data structure
    date_range <- seq(as.Date(start_date), as.Date(end_date), by = "day")
    
    # Get daily totals for target item
    target_daily <- sapply(date_range, function(d) {
        total_minutes(col_name, value, d, d)
    })
    
    # Calculate correlations with related items
    correlations <- sapply(related_items, function(item) {
        item_daily <- sapply(date_range, function(d) {
            total_minutes(col_name, item, d, d)
        })
        
        # Only calculate correlation if both have some variation
        if(var(target_daily) > 0 && var(item_daily) > 0 && length(date_range) > 5) {
            cor(target_daily, item_daily, use = "complete.obs")
        } else {
            NA
        }
    })
    
    # Return results as data frame with strength and direction calculated
    result <- data.frame(
        item = names(correlations),
        correlation = as.numeric(correlations)
    ) %>%
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
    
    return(result)
}

analyze_activity_sequences <- function(col_name, value, end_date, start_date) {
    # Get all activities in the date range, sorted by time
    all_activities <- activities.formatted %>%
        filter(between(date, as.Date(start_date), as.Date(end_date))) %>%
        arrange(date, time_started) %>%
        select(date, time_started, activity, category)
    
    # Add sequence information (previous and next activities)
    all_activities <- all_activities %>%
        mutate(
            activity_id = row_number(),
            prev_activity = lag(activity),
            next_activity = lead(activity),
            # Only consider sequences within the same day
            same_day_prev = lag(date) == date,
            same_day_next = lead(date) == date
        ) %>%
        mutate(
            prev_activity = ifelse(same_day_prev %in% TRUE, prev_activity, NA),
            next_activity = ifelse(same_day_next %in% TRUE, next_activity, NA)
        )
    
    # Filter to target activity/category sessions
    if(col_name == "activity") {
        target_sessions <- all_activities %>% filter(activity == value)
    } else {
        target_sessions <- all_activities %>% filter(category == value)
    }
    
    # Analyze preceding activities
    preceding_activities <- target_sessions %>%
        filter(!is.na(prev_activity)) %>%
        group_by(prev_activity) %>%
        summarize(
            count = n(),
            .groups = 'drop'
        ) %>%
        mutate(
            percent = (count / sum(count)) * 100
        ) %>%
        arrange(desc(count)) %>%
        head(5)  # Top 5 preceding activities
    
    # Analyze following activities
    following_activities <- target_sessions %>%
        filter(!is.na(next_activity)) %>%
        group_by(next_activity) %>%
        summarize(
            count = n(),
            .groups = 'drop'
        ) %>%
        mutate(
            percent = (count / sum(count)) * 100
        ) %>%
        arrange(desc(count)) %>%
        head(5)  # Top 5 following activities
    
    # Calculate session clustering (how often sessions happen in groups)
    target_sessions <- target_sessions %>%
        arrange(date, time_started) %>%
        mutate(
            time_since_last = as.numeric(difftime(time_started, lag(time_started), units = "hours")),
            is_clustered = !is.na(time_since_last) & time_since_last <= 2  # Within 2 hours
        )
    
    cluster_rate <- if(nrow(target_sessions) > 1) {
        sum(target_sessions$is_clustered, na.rm = TRUE) / (nrow(target_sessions) - 1) * 100
    } else {
        0
    }
    
    return(list(
        preceding = preceding_activities,
        following = following_activities,
        cluster_rate_percent = cluster_rate,
        total_sessions = nrow(target_sessions)
    ))
}

total_summary <- function(col_name, value, end_date, start_date) {

    # Get number of total days in range
    days.ranged <- as.numeric(as.Date(end_date) - as.Date(start_date) + 1)


    # get time and units
    minutes.value <- total_minutes(col_name, value, end_date, start_date)
    if(minutes.value < 60) {
        unit <- "minutes"
        display_value <- minutes.value
    } else {
        unit <- "hours"
        display_value <- minutes.value/60
    }

    summary.data <- activities.formatted %>%
        filter(!!sym(col_name) == value) %>%
        filter(between(date, as.Date(start_date), as.Date(end_date)))
    
    weekday_distribution <- analyze_weekday_distribution(summary.data)
    weekday_top <- weekday_distribution %>% 
    arrange(desc(percent))

    monthly_changes <- analyze_trends(col_name, value, end_date, start_date)

    sequence_analysis <- analyze_activity_sequences(col_name, value, end_date, start_date)
    correlation_analysis <- analyze_correlations(col_name, value, end_date, start_date)


    result <- summary.data %>%  
        mutate(
            minutes_since_midnight = hour(time_started)*60 + minute(time_started),
        ) %>%
        summarise(
            typical_minutes = median(minutes_since_midnight),
            typical_hour = floor(typical_minutes/60),
            typical_minute = round(typical_minutes %% 60),
            duration_variance = sd(duration)/mean(duration) *100,
            weighted_avg_hour =
                sum(hour * duration, na.rm = TRUE) / 
                sum(duration, na.rm = TRUE),
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
    streak_analysis <- analyze_streaks_and_gaps(summary.data, end_date, start_date)

    if(result$average_time_per_day < 60) {
        avg_display <- sprintf("%.1f minutes", result$average_time_per_day)
    } else {
        avg <- result$average_time_per_day/60
        hours <- floor(avg)
        minutes <- round((avg %% 1) * 60)
        avg_display <- sprintf("%d hours %d minutes", as.integer(hours), as.integer(minutes))
    }

    if(result$average_time_per_day_yes < 60) {
        avg_yes_display <- sprintf("%.1f minutes", result$average_time_per_day_yes)
    } else {
        avg <- result$average_time_per_day_yes/60
        hours <- floor(avg)
        minutes <- round((avg %% 1) * 60)
        avg_yes_display <- sprintf("%d hours %d minutes", as.integer(hours), as.integer(minutes))
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
    cat(sprintf("Session clustering: %.1f%% occur within 2 hours of previous session\n", sequence_analysis$cluster_rate_percent))
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
    cat("\n")
    cat(sprintf("TRENDS: \n"))
    cat(sprintf("3-month trend: %.2f%%\n", monthly_changes$three_month_trend))
    cat(sprintf("Recent change: %.2f%%\n", monthly_changes$recent_change_percent))
    cat("Most common preceding activites:\n")
    for(i in 1:min(5, nrow(sequence_analysis$preceding))) {
        cat(sprintf(" %s (%.f%%)\n",
            sequence_analysis$preceding$prev_activity[i],
            sequence_analysis$preceding$percent[i]))
    }
    cat("Most common following activites:\n")
    for(i in 1:min(5, nrow(sequence_analysis$following))) {
        cat(sprintf(" %s (%.f%%)\n",
            sequence_analysis$following$next_activity[i],
            sequence_analysis$following$percent[i]))
    }
    cat(sprintf("Strongest correlations with other %s\n", col_name))
    for(i in 1:min(5, nrow(correlation_analysis))) {
       cat(sprintf(" %s: %s %s correlation (%.3f)\n",
            correlation_analysis$item[i],
            correlation_analysis$strength[i],
            correlation_analysis$direction[i],
            correlation_analysis$correlation[i]))
    }
    invisible(result)
}

total_summary("activity", "bathroom", end_date, start_date)

# Function to create a comparison data frame of all activities
compare_activities <- function( end_date = Sys.Date(), start_date = "2024-01-22") {
  # Get list of all unique activities
  all_activities <- unique(activities.formatted$activity)
  
  # Initialize empty data frame to store results
  results <- data.frame()
  
  # Process each activity
  for(activity in all_activities) {
    # Run analysis but capture output instead of printing
    output_capture <- capture.output({
      analysis <- total_summary("activity", activity, end_date, start_date)
    })
    
    # Get correlation data
    corr_data <- analyze_correlations("activity", activity, end_date, start_date)
    top_corr <- if(nrow(corr_data) > 0) {
      paste0(corr_data$item[1], " (", round(corr_data$correlation[1], 2), ")")
    } else {
      "None"
    }
    
    # Get sequence data
    seq_data <- analyze_activity_sequences("activity", activity, end_date, start_date)
    top_before <- if(nrow(seq_data$preceding) > 0) {
      paste0(seq_data$preceding$prev_activity[1], " (", round(seq_data$preceding$percent[1]), "%)")
    } else {
      "None"
    }
    
    top_after <- if(nrow(seq_data$following) > 0) {
      paste0(seq_data$following$next_activity[1], " (", round(seq_data$following$percent[1]), "%)")
    } else {
      "None"
    }
    
    # Get streak data
    streak_data <- analyze_streaks_and_gaps(
      activities.formatted %>% filter(activity == activity),  # OK: direct column name
      end_date, start_date
    )
    
    # Get trend data
    trend_data <- analyze_trends("activity", activity, end_date, start_date)
    
    # Get category
    category <- activities.formatted %>% 
      filter(activity == activity) %>% 
      pull(category) %>% 
      unique() %>% 
      .[1]
    
    # Create a row for this activity
    activity_row <- data.frame(
      activity = activity,
      category = category,
      total_minutes = analysis$average_session_time * analysis$sessions,
      total_hours = (analysis$average_session_time * analysis$sessions) / 60,
      days_active = analysis$total_days,
      days_active_pct = analysis$days_logged_percent,
      sessions = analysis$sessions,
      avg_session_min = analysis$average_session_time,
      avg_per_day_min = analysis$average_time_per_day,
      longest_streak = streak_data$longest_streak,
      longest_gap = streak_data$longest_gap,
      trend_3month = trend_data$three_month_trend,
      trend_recent = trend_data$recent_change_percent,
      typical_hour = analysis$typical_hour,
      duration_consistency = analysis$duration_variance,
      top_correlation = top_corr,
      top_preceding = top_before,
      top_following = top_after
    )
    
    # Add to results
    results <- rbind(results, activity_row)
  }
  
  # Sort by total time
  results <- results %>% arrange(desc(total_hours))
  
  return(results)
}

activity_comparison <- compare_activities()

# View the comparison
View(activity_comparison)

# Export to CSV
write.csv(activity_comparison, "activity_comparison.csv", row.names = FALSE)

# Filter to just see Work activities
work_activities <- activity_comparison %>% 
  filter(category == "Work") %>%
  arrange(desc(total_hours))
