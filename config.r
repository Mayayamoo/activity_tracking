monthly_change <- new.env()
.session_cache <- new.env()

CONFIG <- list(
    activity_data_path = "G:/My Drive/activitytracker/stt_records_automatic.csv",
    output_dir = "outputs/"
)

start_date = as.Date("2024-01-22")
end_date = Sys.Date()   
threshold_minutes = 120
