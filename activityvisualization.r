source("activityprocessing.r")

category_colors <- c(
    "Work" = "#673ab7",
    "Brain Blasters" = "#3f51b5",
    "Leisure" = "#2196f3",
    "Life Duties" = "#f53639",
    "Human Upkeep" = "#ffeb3b",
    "Socializing" = "#4caf50",
    "transportation" = "#ffc107",
    "Vices" = "#8bc34a",
    "Mindfulness" = "#ef4091"
)

one <- ggplot(monthly, aes(x = month_year, y = total_hours, color = categories, group = categories)) +
    geom_line(linewidth = 2) +
    scale_color_manual(values = category_colors) +
    labs(
        x = "Date",
        y = "Total Hours") +
        scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") + 
        theme_minimal() +
        theme(
            panel.background = element_rect(fill = "black"),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            panel.grid.minor = element_blank()
            )


work.change <- activity.change("Work")
leisure.change <- activity.change("Leisure")
socializing.change <- activity.change("Socializing")
vices.change <- activity.change("Vices")
humanupkeep.change <- activity.change("Human Upkeep")
transportation.change <- activity.change("transportation")
lifeduties.change <- activity.change("Life Duties")
lifeduties.change <- activity.change("Brain Blasters")
mindfulness.change <- activity.change("Mindfulness")

mindfulness.change
work.change
leisure.change


activity.change.all <- function() {
    monthly %>%
        arrange (categories, month_year) %>%
        group_by(categories) %>%
        mutate(
            change = total_hours - lag(total_hours),
            percent_change = (change / lag(total_hours)) * 100
        ) %>%
        ungroup() %>%
        select(categories, month_year, total_hours, change, percent_change)
}

activity.change.all()

df_heatmap <- activities.formatted %>%
    group_by(categories, hour) %>%
    summarise(total_minutes = sum(duration_minutes), .groups = 'drop')

three <- ggplot(df_heatmap, aes(x = categories, y = hour, fill = total_minutes)) +
    geom_tile(color = "white") +
    scale_fill_distiller() +
    labs(
        title = "Heatmap of activity by hour",
        x = "Category",
        y = "Hour") +
        theme_minimal()