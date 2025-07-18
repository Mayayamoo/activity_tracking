source("config.r")
get_cached_activities_data <- function(refresh = FALSE) {
    cache_key <- "activities_formatted"

  # Check if data exists in cache
      if (!exists(cache_key, envir = .session_cache) || refresh) {
          message("Loading data into cache...")

          # Load fresh data
          if (!exists("activities.formatted")) {
              source("data_munch.r")
          }

          # Store in cache
          assign(cache_key, activities.formatted, envir = .session_cache)
      }

      # Return cached data
      get(cache_key, envir = .session_cache)
}

create_daily_cache <- function(activities_data, start_date, end_date) {
      cache_key <- paste("daily", start_date, end_date, sep = "_")

      if (!exists(cache_key, envir = .session_cache)) {
          message("Creating daily aggregation cache...")

          daily_summary <- activities_data %>%
              filter(between(date, as.Date(start_date), as.Date(end_date))) %>%
              group_by(date, activity, category) %>%
              summarise(daily_minutes = sum(duration_minutes), .groups = 'drop') %>%
              # Create wide format for fast lookups
              pivot_wider(
                  names_from = c(activity, category),
                  values_from = daily_minutes,
                  values_fill = 0
              )

          assign(cache_key, daily_summary, envir = .session_cache)
      }

      get(cache_key, envir = .session_cache)
}