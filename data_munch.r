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

# --- formatting function ---
formatting <- function(df, x) {
    df %>% 
    group_by(month_year, !!sym(x)) %>%  
    summarise(
        total_minutes = sum(duration, na.rm = TRUE),
        total_hours = sum(duration/60, na.rm = TRUE),
        .groups='drop'
    ) %>%
    select(!!sym(x), month_year, total_minutes, total_hours) 
}

#filtering function. filters to specified date range
filter_time <- function(df, start_date, end_date) {
    df %>%
    filter(between(month_year, as.Date(start_date), as.Date(end_date)))%>%
    arrange(month_year)
}

# --- monthly.change function ---
monthly.change <- function(df,x) {
    df %>%
        filter(
            if (identical(df, mon.act.tot.hrs)) {
                !!sym("activity") == x  
            } else {
                !!sym("category") == x 
            }
        ) %>%
        arrange(month_year) %>%
        mutate(
            change = total_hours - lag(total_hours),
            percent_change = (change/lag(total_hours)) * 100
        ) %>%
        select(month_year, total_hours, change, percent_change)
}

#outputs all MoM changes for each sub-attribute of x. 
all.changes <- function(x) {
    if (x == "category") {
        df <- mon.cat.tot.hrs
    } else {
        df <- mon.act.tot.hrs
    }

    df_transform <- df %>%
        group_by(!!sym(x)) %>%
        arrange(!!sym(x), month_year) %>% 
        mutate(
            MoM_change = total_hours - lag(total_hours),
            percent_change = (MoM_change / lag(total_hours)) * 100,
            summary = sprintf("%.1f hrs, %+.1f%%",
                total_hours, percent_change)
        ) %>%
        ungroup() %>%
        select(month_year, !!sym(x), total_minutes, MoM_change, percent_change, summary)  # FIXED
    split_dfs <- split(df_transform, df_transform[[x]])
    return(split_dfs)
}


#Creates an MoM change df for each category and activity
mon_change_all <- function(c) {
    df_list <- all.changes(c)
    clean_names <- names(df_list) %>%
        str_replace_all(" ", "_") %>%
        tolower()
    named_list <- setNames(
        df_list,
        paste0(clean_names)
    )
    # turns each item in named_list into a global variable (df)
    list2env(named_list, envir = monthly_change)
    return(named_list)
}

#list of each activity grouped by hours per day
mon.act.tot.hrs <- formatting(activities.formatted, "activity")

#list of each category grouped by hours per day
mon.cat.tot.hrs <- formatting(activities.formatted, "category")

#calls mon_change_all function, creates a MoM change df for each category and activity
mon_change_all("category")

mon_change_all("activity")