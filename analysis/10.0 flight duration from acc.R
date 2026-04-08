# Code for identifying each migratory flight ussing accelerometer data: 
library(tidyverse)
library(data.table)    # for rleid
library(lubridate)     # for date & time handling
library(readxl)
library(dplyr)
rm(list=ls())

id_values <- list.dirs("./Data/", full.names = FALSE, recursive = FALSE) 
id_values

# Loop over each ID
for (id in id_values) {
  message("Processing ID: ", id)
  
  # Get the path to the Excel file
  file_path <- file.path(list.files(file.path("Data", id), pattern = "_acc\\.xlsx$", full.names = T))
  pressure_file <- file.path(list.files(file.path("Data", id), pattern = "_press\\.xlsx$", full.names = T))
  
  # Read the file
  df <- readxl::read_excel(file_path)
  df_pressure <- readxl::read_excel(pressure_file)
  
  # Drop unnamed or empty columns
  df <- df %>%
    # Drop columns where all names are "" or start with "..."
    dplyr::select(-matches("^\\.\\.\\.|^$")) %>%
    # Drop columns where all rows are NA
    dplyr::select(where(~ !all(is.na(.x))))
  
  # Rename and process
  df_flights <- df %>%
    rename(date = `Activity date & time`) %>%
    mutate(
      acc_3_4_5_sum = `acc[3]` + `acc[4]` + `acc[5]`,
      acc_4_5_any = (`acc[4]` > 0) | (`acc[5]` > 0),
      exclude_jul_aug = format(date, "%m-%d") >= "06-15" & format(date, "%m-%d") <= "08-01", #exclude months with high daytime activit due to chick feeding
      is_active = (acc_3_4_5_sum >= 3) & acc_4_5_any & !exclude_jul_aug,
      prev_acc_3_4_5_sum = lag(`acc[3]`) + lag(`acc[4]`) + lag(`acc[5]`),
      next_acc_3_4_5_sum = lead(`acc[3]`) + lead(`acc[4]`) + lead(`acc[5]`)
    ) %>%
    mutate(
      group_id = data.table::rleid(is_active),
      flight_group = if_else(
        is_active,
        match(group_id, unique(group_id[is_active])),
        NA_integer_
      )
    ) %>%
    dplyr::select(-group_id)
  
  # Adjust takeoff and landing times if the first and last row where 100% active
  df_flights <- df_flights %>%
    group_by(flight_group) %>%
    mutate(
      first_row = row_number() == 1,
      last_row = row_number() == n(),
      flight_min_per_h = if_else(
        acc_4_5_any & !exclude_jul_aug,
        acc_3_4_5_sum * 10,
        0
      ),
      takeoff_time_adjusted = case_when(
        first_row & acc_3_4_5_sum >=5 ~ 
          (date - minutes(flight_min_per_h)) - minutes(prev_acc_3_4_5_sum * 10),
        TRUE ~ 
          (date - minutes(flight_min_per_h))
      ),
      landing_time_adjusted = case_when(
        last_row & acc_3_4_5_sum >=5 ~ 
          date + minutes(next_acc_3_4_5_sum * 10),
        TRUE ~ 
          (date - minutes(60) + minutes(flight_min_per_h))
      )
    ) %>%
    ungroup()
  
  df_flights$date <- format(df_flights$date, "%Y-%m-%d %H:00:00")
  
  #adding logger temperature to the hourly data set
  df_flights$temperature_logger <- df_pressure$'Temp [C]'
  df_flights$pressure_logger <- df_pressure$'Pressure [hPa]'
  
  #filtering out incomplete migrations
  if (id %in% c("91A", "92A")) {
    df_flights <- df_flights %>%
      filter(as.Date(date) < as.Date("2023-06-10"))
  }
  
  # Summarise flight periods
  df_flights <- df_flights %>%
    mutate(next_landing_time = lead(landing_time_adjusted))
  
  flight_periods <- df_flights %>%
    filter(!is.na(flight_group)) %>%
    group_by(flight_group) %>%
    summarise(
      takeoff_time = min(takeoff_time_adjusted),
      landing_time = if(n() == 1) next_landing_time else max(landing_time_adjusted), #this deals with scenarios of flight duration being less than 1h
      duration_hours = as.numeric(difftime(landing_time, takeoff_time, units = "hours")),
      .groups = "drop"
    )
  
  # Save outputs
  out_dir <- file.path("Data", id)
  writexl::write_xlsx(flight_periods, file.path(out_dir, paste0(id, "_migratory_flights.xlsx")))
  writexl::write_xlsx(df_flights, file.path(out_dir, paste0(id, "_acc_press_hourly_flight.xlsx")))
  
  # Print total duration
  total_flight_hours <- sum(flight_periods$duration_hours, na.rm = TRUE)
  message("Total flight hours for ", id, ": ", total_flight_hours)
}


# id <- "92A"
# 
# df <- file.path("Data", id) %>% 
#   list.files(pattern = "_acc\\.xlsx$", full.names = TRUE) %>% 
#   read_excel()
# 
# str(df)
# 
# # Step 1: Identify rows with active flight events: at least 3 acc[3]/acc[4]/acc[5]
# df_flights <- df %>%
#   rename(date = `Activity date & time`) %>% 
#   mutate(
#     # Sum acc[3], acc[4], acc[5]
#     acc_3_4_5_sum = `acc[3]` + `acc[4]` + `acc[5]`,
#     # Check if any value in acc[4] or acc[5] is > 0
#     acc_4_5_any = (`acc[4]` > 0) | (`acc[5]` > 0),
#     # Exclude rows between July 1 and August 10
#     exclude_jul_aug = format(date, "%m-%d") >= "06-15" & 
#       format(date, "%m-%d") <= "08-10",
#     # Mark active only if all three conditions are TRUE
#     is_active = (acc_3_4_5_sum >= 3) & acc_4_5_any & !exclude_jul_aug,
#     # Get acc_3_4_5_sum from the previous row in original df
#     prev_acc_3_4_5_sum = lag(`acc[3]`) + lag(`acc[4]`) + lag(`acc[5]`),
#     next_acc_3_4_5_sum = lead(`acc[3]`) + lead(`acc[4]`) + lead(`acc[5]`),
#   )
# 
# # Step 2: Group consecutive active rows into flight periods
# df_flights <- df_flights %>%
#   mutate(
#     group_id = rleid(is_active),  # consecutive groups
#     flight_group = if_else(
#       is_active,
#       match(group_id, unique(group_id[is_active])),  # map TRUE group_ids to 1, 2, 3…
#       NA_integer_                                    # keep FALSE rows NA
#     )
#   ) %>%
#   select(-group_id)  # optional: drop intermediate column
# 
# # Step 3: Adjust takeoff and landing times
# df_flights <- df_flights %>%
#   group_by(flight_group) %>%
#   mutate(
#     first_row = row_number() == 1,
#     last_row = row_number() == n(),
#     
#     # Calculate takeoff offset 
#     flight_min_per_h = if_else(
#       acc_4_5_any & !exclude_jul_aug,
#       acc_3_4_5_sum * 10,
#       0
#     ),   # active: sum * 10 min
#     
#     # Adjust takeoff time:
#     # If first row of group has acc_3_4_5_sum >=5, subtract previous row’s sum * 10 min
#     takeoff_time_adjusted = case_when(
#       first_row & acc_3_4_5_sum >=5 ~ 
#         (date - minutes(flight_min_per_h)) - minutes(prev_acc_3_4_5_sum * 10),
#       TRUE ~ 
#         (date - minutes(flight_min_per_h))
#       ),
#     
#     landing_time_adjusted = case_when(
#       last_row & acc_3_4_5_sum >=5 ~ 
#         date + minutes(next_acc_3_4_5_sum * 10),
#       TRUE ~ 
#         (date - minutes(60) + minutes(flight_min_per_h))
#     ),
#   ) %>%
#   ungroup()
# 
# # Step 4: Summarize flight periods with adjusted times
# flight_periods <- df_flights %>%
#    filter(!is.na(flight_group)) %>%      # drop rows with NA flight_group
#   group_by(flight_group) %>%
#   summarise(
#     takeoff_time = min(takeoff_time_adjusted),
#     landing_time = max(landing_time_adjusted),
#     duration_hours = as.numeric(difftime(landing_time, takeoff_time, units = "hours")),
#     .groups = "drop"
#   )
# 
# # View the result
# print(flight_periods)
# sum(flight_periods$duration_hours)
# write.csv(flight_periods, file.path("Data", id, paste0(id, "_flight_periods.csv")))
# 
# 



##### Labelling migratory periods (clusters of migratory flights together) automatically #####
# Parameters
gap_days <- 5      # Start a new period if gap > 5 days
min_flights <- 3   # Minimum number of flights
window_days <- 5  # Must have >= min_flights in any rolling 10-day window
min_total_hours <- 16  # Total flight duration threshold in hours for the entire migratory period; avoid clusters of short flights in ACB

id_values <- list.dirs("./Data/", full.names = FALSE, recursive = FALSE) 
id_values

# Loop over each ID
for (id in id_values) {
  message("Processing ID: ", id)
  
  # Get Excel path
  migratory_flights_file <- file.path(
    list.files(file.path("Data", id), pattern = "_migratory_flights.xlsx$", full.names = TRUE)
  )
  
  flight_periods <- readxl::read_excel(migratory_flights_file)
  
  flight_periods <- flight_periods %>%
    arrange(takeoff_time) %>%
    mutate(
      date = as.Date(takeoff_time),
      yday = lubridate::yday(date),
      gap = as.numeric(difftime(date, lag(date), units = "days")),
      new_period = ifelse(is.na(gap) | gap >= gap_days, 1, 0),
      period_temp = cumsum(new_period)
    )
  
  # Check density rule
  flight_periods <- flight_periods %>%
    group_by(period_temp) %>%
    mutate(
      flights_in_window = sapply(row_number(), function(i) {
        sum(abs(as.numeric(date - date[i])) <= window_days)
      }),
      meets_density = any(flights_in_window >= min_flights)
    ) %>%
    ungroup()
  
  # Check total hours rule
  total_hours_per_period <- flight_periods %>%
    group_by(period_temp) %>%
    summarise(total_hours = sum(duration_hours, na.rm = TRUE), .groups = "drop")
  
  valid_periods <- total_hours_per_period %>%
    filter(total_hours > min_total_hours) %>%
    pull(period_temp)
  
  # Apply both rules
  flight_periods <- flight_periods %>%
    mutate(period = ifelse(meets_density & period_temp %in% valid_periods, period_temp, NA))
  
  ## Divide in two periods Spring migration. Identify the last period of the spring migration and cluster everything together before this.
  if (any(flight_periods$yday <= 150 & !is.na(flight_periods$period))) {
    
    # Identify last valid period within day 1–150
    last_period <- flight_periods %>%
      filter(yday <= 150 & !is.na(period)) %>%
      arrange(date) %>%
      slice_tail(n = 1) %>%
      pull(period)
    
    # Get the first day of the longest period
    start_longest <- min(flight_periods$date[flight_periods$period == last_period], na.rm = TRUE)
    
    # Flights before that date (and yday ≤ 150) = new separate period
    flight_periods <- flight_periods %>%
      mutate(
        period = case_when(
          yday <= 150 & !is.na(period) & date < start_longest ~ -1,  # temporary label for "before"
          yday <= 150 & !is.na(period) & period == last_period ~ last_period,
          TRUE ~ period
        )
      )
  }
  
  # Renumber consecutively and convert to even numbers
  flight_periods <- flight_periods %>%
    arrange(date) %>%
    mutate(
      period = ifelse(!is.na(period),
                      match(period, unique(period[!is.na(period)])) * 2,
                      NA)
    ) %>%
    dplyr::select(-period_temp, -new_period, -flights_in_window, -meets_density)
  
  #### tweaking of each period for specific birds ID 
  # ACB and ACA had incompleted spring migrations. So these are tossed as it is unsure how to group their periods
  #DB6 had 5 migratory periods in autumn, I renumber them so that they match the other birds for positioning
  flight_periods <- flight_periods %>% 
    mutate(
      period = if_else(id %in% c("ACA", "ACB") & period >= 10, NA, period),
      period = if_else(id %in% c("DB6") & period == 8, NA, period ),
      period = if_else(id %in% c("DB6") & period == 10, 8, period )
    )
  
  #removing all flights after autumn migrations for ACA and ACB
  if (id %in% c("ACA", "ACB")) {
    # Find the cutoff date = last row with period 8
    cutoff <- max(flight_periods$date[flight_periods$period == 8], na.rm = TRUE)

    # Keep only rows up to that cutoff
    flight_periods <- flight_periods %>%
      filter(date <= cutoff+1)
  }
  
  if(id %in% c("AC2")) {
    # Find the cutoff date = last row with period 8
    cutoff <- min(flight_periods$date[flight_periods$period == 2], na.rm = TRUE)
    # Keep only rows up to that cutoff
    flight_periods <- flight_periods %>%
      filter(date >= cutoff+1)
  }
  
  # 
  # Count flights not part of any valid period
  n_assigned <- sum(!is.na(flight_periods$period))
  n_unassigned <- sum(is.na(flight_periods$period))
  message("Flights not part of any period for ", id, ": ", n_assigned)
  message("Flights not part of any period for ", id, ": ", n_unassigned)
  
  out_dir <- file.path("Data", id)
  writexl::write_xlsx(flight_periods, file.path(out_dir, paste0(id, "_migratory_flight_periods.xlsx")))
  
  plot_df <- flight_periods %>%
    transmute(ID = id, day_of_year = yday, period = period)
  
  p_id <- ggplot(plot_df, aes(x = day_of_year, y = period)) +
    geom_point(size = 3.5, alpha = 0.6, color = "#374151") +
    scale_x_continuous(breaks = seq(0, 365, by = 30), limits = c(0, 365)) +
    scale_y_continuous(breaks = seq(0, max(plot_df$period, na.rm = TRUE), by = 2)) +
    labs(x = "Day of Year", y = "Period", title = paste("Periods by Day of Year —", id)) +
    theme_minimal()
  
  print(p_id)
}







   

       
