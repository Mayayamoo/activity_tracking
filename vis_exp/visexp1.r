library(tidyverse)
library(lubridate)

df <- read.csv("rawdata/Activitylog.csv")

category_colors <- c(
    "Brain Blasters" = "#3f51b5",
    "Human Upkeep" = "#ffeb3b",
    "Leisure" = "#2196f3",
    "Life Duties" = "#f53639",
    "Mindfulness" = "#000000",
    "Socializing" = "#4caf50",
    "transportation" = "#ffc107",
    "Vices" = "#8bc34a",
    "Work" = "#673ab7"
)

df <- df %>%
    mutate(
        time_started = ymd_hms(time_started),
        time_ended = ymd_hms(time_ended),
        date = as.Date(time_started)
    )

df_daily <- df %>%
    group_by(date, categories) %>%
    summarise(
        total_minutes = sum(duration_minutes),
        .groups='drop'
    )

one <- ggplot(df_daily, aes(x = date, y = total_minutes, color = categories)) +
    geom_line() +
    scale_color_manual(values = category_colors) +
    labs(
        title = "Time spent per category over time",
        x = "Date",
        y = "Total Minutes") +
        theme_minimal()

ggsave("time_spent_plot2.png", one, width = 49, height = 30, dpi = 300)
df_category <- df %>%
    group_by(categories) %>%
    summarise(total_minutes = sum(duration_minutes), .groups = 'drop') %>%
    mutate(percentage = total_minutes / sum(total_minutes) * 100)

two <- ggplot(df_category, aes(x = reorder(categories, -total_minutes), y = total_minutes, fill = categories)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(
        title = "Total time spent per category",
        x = "Category",
        y = "Total Minutes") +
        theme_minimal()

df <- df %>%
  mutate(
         hour = hour(time_started))

df_heatmap <- df %>%
    group_by(date, hour) %>%
    summarise(total_minutes = sum(duration_minutes), .groups = 'drop')

three <- ggplot(df_heatmap, aes(x = hour, y = date, fill = total_minutes)) +
    geom_tile() +
    scale_fill_viridis_c() +
    labs(
        title = "Heatmap of activity by hour",
        x = "Hour of the day",
        y = "Date") +
        theme_minimal()

