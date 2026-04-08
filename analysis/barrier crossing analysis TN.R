#This script generate all analysis and figures for the study of barrier crossings in thrush nightingales tagged in Lund, Sweden
#
#It runs after the script called "flight duration from acc". 
#
#
#


library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
rm(list=ls())

#combine data set from all hourly acc flight files

id_values <- list.dirs("Data/", full.names = FALSE, recursive = FALSE) 
#id_values <- id_values[id_values != "90A"] #90A had no pressure data and therefore no pressure_path

# for (id in id_values) {
#   acc_flight_file <- file.path("Data", id) %>% 
#     list.files(pattern = "_acc_press_hourly_flight\\.xlsx$", full.names = TRUE) %>% 
#     read_xlsx()
# }

all_acc_flight <- lapply(id_values, function(id) {
  file <- file.path("Data", id) %>%
    list.files(pattern = "_acc_press_hourly_flight\\.xlsx$", full.names = TRUE)
  
  if (length(file) == 0) return(NULL)
  
  read_xlsx(file) %>%
    mutate(ID = id)
}) %>%
  bind_rows()


all_flight_periods <- lapply(id_values, function(id) {
  file <- file.path("Data", id) %>%
    list.files(pattern = "_migratory_flight_periods\\.xlsx$", full.names = TRUE)
  
  if (length(file) == 0) return(NULL)
  
  read_xlsx(file) %>%
    mutate(ID = id)
}) %>%
  bind_rows()


all_pressurepath <- lapply(id_values, function(id) {
  file <- file.path("Data", id) %>%
    list.files(pattern = "_pressurepath_most_likely\\.xlsx$", full.names = TRUE)
  
  if (length(file) == 0) return(NULL)
  
  read_xlsx(file) %>%
    mutate(ID = id)
}) %>%
  bind_rows()


## Flight summary statistics per flight period & seasons##
##Adding seasons to the all_flight_periods
all_flight_periods <- all_flight_periods %>%
  mutate(
    seasons = case_when(
      period %in% 2:8       ~ "Autumn",
      period %in% 10:12     ~ "Spring",
      TRUE                  ~ NA_character_   # leave NA if nothing matched
    ),
    seasons = factor(seasons, levels = c("Autumn", "Spring"))
  ) %>%
  arrange(ID, date) %>%                  # make sure rows are in temporal order
  group_by(ID) %>%                       # carry forward season within each individual
  fill(seasons, .direction = "down") %>%
  ungroup()



##### Building the daily df with periods extracted from all_flight_periods. Also filling the gaps in periods
# Make sure all_acc_flight$date is POSIXct
all_acc_flight <- all_acc_flight %>%
  mutate(date = ymd_hms(date, tz = "UTC"))  # match the timezone of all_pressurepath

# Merge by ID and full datetime
merged_df <- left_join(all_acc_flight, all_pressurepath, by = c("ID", "date"))

#Summary per day; calcualting daytime activity when bird not flying & accounting for missing sunrise/sunset estimates
daily_summary <- merged_df %>%
  arrange(ID, date) %>%                       # ensure correct row order for lag()
  group_by(ID) %>%
  mutate(
    prev_is_active = lag(is_active, default = FALSE),
    next_is_active = lead(is_active, default = FALSE),
    day = as.Date(date),
    sunrise_fallback = if_else(
      is.na(sunrise), as.POSIXct(paste0(day, " 04:00:00"), tz = "UTC"), sunrise
    ),
    sunset_fallback = if_else(
      is.na(sunset),  as.POSIXct(paste0(day, " 16:00:00"), tz = "UTC"), sunset
    ),
    Act_score_flight = if_else( #This calculates the value in the actoscore when there is a migratory flight activity that should not be considered in the daytime activity
      prev_is_active == TRUE | next_is_active == TRUE, 
      (`acc[5]` * 5) + (`acc[4]` * 4), 
      0
    ),
    Act_score_no_flight = `Act score` - Act_score_flight
  ) %>%
  ungroup() %>%
  group_by(ID, day) %>%
  summarise( 
    total_flight_min = sum(`flight_min_per_h`[is_active == TRUE], na.rm = TRUE),
    flight_rows      = sum(is_active == TRUE, na.rm = TRUE),
    
    # only count daytime rows that the birds are not flying: is_active == FALSE
    daytime_rows = sum(
      (is_active != TRUE) & date >= sunrise_fallback & date <= sunset_fallback, na.rm = TRUE
    ),
    daytime_act_score = sum(
      `Act score`[(is_active != TRUE) & date >= sunrise_fallback & date <= sunset_fallback], na.rm = TRUE
    ),
    daytime_act_score_no_flight = sum(Act_score_no_flight[(is_active != TRUE) & date >= sunrise_fallback & date <= sunset_fallback]),
    daytime_activity_percentage = daytime_act_score_no_flight / (daytime_rows * 30) * 100,  
    lat = mean(lat[(is_active != TRUE)], na.rm = TRUE),
    lon = mean(lon[(is_active != TRUE)], na.rm = TRUE),
    stopover_id = min(stap_id[is_active != TRUE & stap_id %% 1 == 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(date = day)




# library(dplyr)
# problem_rows <- merged_df %>%
#   filter(is_active == FALSE,
#          !exclude_jul_aug,
#          date >= sunrise,
#          date <= sunset,
#          (`acc[4]` > 0 | `acc[5]` > 0))
# problem_rows

#extracting periods per day
period_per_day <- all_flight_periods %>%
  mutate(date = as.Date(takeoff_time)) %>%
  group_by(ID) %>%
  arrange(date, .by_group = TRUE) %>%
  complete(
    date = seq(
      from = make_date(year(min(date, na.rm = TRUE)), 6, 12),
      to   = max(date, na.rm = TRUE),
      by   = "day"
    ),
    fill = list(period = NA_real_)
  ) %>%
  ungroup() %>%
  dplyr::select(ID, date, period) %>% 
  distinct(ID, date, .keep_all = TRUE) # making sure there are not duplicates in date

# Join
daily_summary <- daily_summary %>%
  mutate(date = as.Date(date)) %>%                 # ensure Date type
  left_join(period_per_day, by = c("ID", "date")) %>%   # NA where no match
  #fill(period, .direction = "down") %>% 
  mutate(
    day_of_year = yday(date)                       # add day of year column
  )

#visualization of each period in the calendar year
#This plot should have clear cluster per bird ID
ggplot(daily_summary, aes(x = day_of_year, y = period, color = ID)) +
  geom_jitter(size = 4, alpha = 0.3) +
  scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  scale_y_continuous(breaks = seq(0, max(daily_summary$period, na.rm = TRUE))) +
  labs(
    x = "Day of Year",
    y = "Period",
    title = "Periods by Day of Year for All IDs"
  ) +
  theme_minimal()



#filling gaps in migratory periods for daily summary df
daily_filled <- daily_summary %>%
  arrange(ID, date) %>%
  group_by(ID) %>%
  filter(!is.na(period)) %>%                           # 1) keep only rows with a period
  # split into contiguous runs of the same period within each ID
  mutate(period_run = cumsum(coalesce(period != lag(period), TRUE))) %>%
  group_by(ID, period, period_run) %>%
  # 2) add any missing dates within the run (from first to last date in that run)
  complete(date = seq(min(date), max(date), by = "day")) %>%
  # 3) fill the period value for the new rows
  fill(period, .direction = "downup") %>%
  ungroup() %>%
  dplyr::select(-period_run) %>%
  arrange(ID, date)

#merging both
daily_summary <- daily_filled %>%
  dplyr::select(ID, date, period) %>%                      # only bring the expansion info
  right_join(daily_summary, by = c("ID", "date")) %>%
  mutate(period = coalesce(period.x, period.y)) %>% # prefer filled period
  dplyr::select(-period.x, -period.y) %>%
  arrange(ID, date)


#Filling NA for stationary periods
daily_summary <- daily_summary %>%
  group_by(ID) %>%
  arrange(date, .by_group = TRUE) %>%
  # forward-fill even periods so NA "blocks" know the next even number
  mutate(next_even = zoo::na.locf(period, na.rm = FALSE, fromLast = TRUE)) %>%
  # back-fill even periods so trailing NAs know the last seen even
  mutate(last_even = zoo::na.locf(period, na.rm = FALSE)) %>%
  mutate(
    period = case_when(
      # NAs before an even period → odd = next_even - 1
      is.na(period) & !is.na(next_even) ~ next_even - 1,
      # NAs after the last even period → odd = last_even + 1
      is.na(period) & !is.na(last_even) ~ last_even + 1,
      TRUE ~ period
    )
  ) %>%
  dplyr::select(-next_even, -last_even) %>%
  ungroup()

#Filtering out ACA and ACB incomplete spring migrations.
daily_summary <- daily_summary %>%
  filter(
    (ID == "ACA" & date < as.Date("2024-02-29")) |
      (ID == "ACB" & date < as.Date("2024-03-23")) |
      !(ID %in% c("ACA", "ACB"))   # keep all other IDs unchanged
  )


### Summary statistics for each period, stopover and migrartory
#removing some rows that had no activity when recaptured; when the logger was removed from the bird
daily_summary <- daily_summary %>%
  filter(!(period == 13 & daytime_act_score == 0)) %>%
  mutate(
    period = if_else(ID == "AC2" & period == 7, 9, period) #bird AC2 only had 3 autumn migratory periods; renaming the last wintering period so it matchs the rest of brids
  )

#for calculating diurnal activity activity for each period, the first day of a migratory period still belongs to the stopover
#I create a new column for quantifying diurnal activity for each stopover period i.e period_activity
daily_summary <- daily_summary %>%
  group_by(ID) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    period_activity = period, 
    period_activity = if_else(
      period %% 2 == 0 & (lag(period) != period),   # first row of an even period
      lag(period),                                  # substitute with previous
      period_activity
    )
  ) %>%
  ungroup()

##Adding seasons to the daily df
daily_summary <- daily_summary %>%
  mutate(
    seasons = case_when(
      period_activity %in% c(1, 13)       ~ "Breeding",
      period_activity %in% 2:8            ~ "Autumn",
      period_activity == 9                ~ "Wintering",
      period_activity %in% 10:12          ~ "Spring",
      TRUE                       ~ NA_character_
    ),
    seasons = factor(seasons, levels = c("Breeding", "Autumn", "Wintering", "Spring"))
  )





#### adding periods to merged_df hourly df
merged_df$date_day <- as.Date(merged_df$date)

daily_period <- daily_summary %>% 
  select(ID, date, period, seasons)

merged_df <- merged_df %>% 
  left_join(daily_period, by = c("ID", "date_day" = "date"))

merged_df$activity_percentage <- merged_df$`Act score` * 100 / 30


###### Compute sunrise by day for each bird
sunrise_by_day <- merged_df %>%
  filter(!is.na(sunrise)) %>%
  mutate(yday = yday(date)) %>%
  group_by(ID, yday) %>%
  summarise(
    sunrise = max(sunrise, na.rm = TRUE) + days(1),
    latitude = first(lat),
    .groups = "drop"
  )

# Combine flights with sunrise and compute minutes after sunrise
all_flight_periods2 <- all_flight_periods %>%
  left_join(sunrise_by_day, by = c("ID", "yday")) %>%
  mutate(
    minutes_after_sunrise = as.numeric(difftime(landing_time, sunrise, units = "mins")),
    flight_after_sunrise = minutes_after_sunrise > 0
  )

# Summarise flight stats by season
all_flight_periods2 %>%
  group_by(seasons) %>%
  summarise(
    n_flights = n(),
    n_after_sunrise = sum(flight_after_sunrise, na.rm = TRUE),
    n_ids = n_distinct(ID[flight_after_sunrise]),
    
    # Statistics only for flights after sunrise
    mean_minutes_after = mean(minutes_after_sunrise[flight_after_sunrise], na.rm = TRUE),
    median_minutes_after = median(minutes_after_sunrise[flight_after_sunrise], na.rm = TRUE),
    min_minutes_after = min(minutes_after_sunrise[flight_after_sunrise], na.rm = TRUE),
    max_minutes_after = max(minutes_after_sunrise[flight_after_sunrise], na.rm = TRUE),
    
    median_flight_duration = median(duration_hours[flight_after_sunrise], na.rm = TRUE),
    .groups = "drop"
  )





###### #plot locomotion % in period 4 ######
# Base output directory
base_dir <- "C:/Users/pa5772ma/OneDrive - Lund University/Lund PhD/Research/Fieldwork/Nightingales/Analysis/Thrush nightingale acelerometer flights/output"

all_ids <- unique(merged_df$ID)

#all_ids <- "ACA"

for (bird in all_ids) {
  
  # All data for this bird
  df_id <- merged_df %>% 
    filter(ID == bird)
  
  # Find all days where period == 4
  p4_days <- df_id %>%
    filter(period == 4) %>%
    pull(date_day)
  
  # If no period 4 exists → skip bird
  if (length(p4_days) == 0) next
  
  # Identify window limits
  start_day <- min(p4_days) - 1   # day before first P4 day
  end_day   <- max(p4_days) + 2   # day after last P4 day
  
  # Crop entire bird dataset to the extended window
  plot_df <- df_id %>%
    filter(date_day >= start_day,
           date_day <= end_day)
  
  # ---- Create night shading intervals ----
  # A rectangle from today's sunset → tomorrow's sunrise
  shade_df <- plot_df %>%
    group_by(date_day) %>%
    summarise(
      sunset      = max(sunset, na.rm = TRUE),
      sunrise     = min(sunrise, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(
      is.finite(sunset),
      is.finite(sunrise)
    ) %>%
    arrange(date_day) %>%
    mutate(
      next_sunrise = lead(sunrise)
    ) %>%
    filter(!is.na(next_sunrise))
  
  # ---- Create daily shading dataframe for sahara crossing ----
  first_37 <- plot_df %>%
    filter(lat <= 37) %>%
    arrange(date) %>%
    slice(1)
  
  first_18 <- plot_df %>%
    filter(lat <= 18) %>%
    arrange(date) %>%
    slice(1)
  
  t_start <- if (nrow(first_37) > 0) first_37$date else NA
  t_start
  t_end   <- if (nrow(first_18) > 0) first_18$date else NA
  t_end
  
  lat_rect_df <- if (!is.na(t_start) & !is.na(t_end)) {
    data.frame(xmin = t_start, xmax = t_end)
  } else {
    NULL
  }
  
  if (length(lat_rect_df) == 0) next
  
  # ---- Create folder 
  out_dir <- file.path(base_dir, bird)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # ---- Plot ----
  p <- ggplot(plot_df, aes(x = date, y = activity_percentage)) +
    # ---- Latitude shading (18°–37°) ----
  geom_rect( data = lat_rect_df,
             inherit.aes = FALSE,
             aes(
               xmin = xmin,
               xmax = xmax,
               ymin = -Inf,
               ymax = Inf
             ),
             fill = "brown",
             alpha = 0.3,
             colour = NA
  )+
    # Night shading (if available)
    geom_rect(
      data = shade_df,
      inherit.aes = FALSE,
      aes(
        xmin = sunset,
        xmax = next_sunrise,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "grey70",
      alpha = 0.3
    ) +
    # Activity curve
    geom_line(linewidth = 0.7, alpha = 0.9) +
    geom_point(size = 1.2, alpha = 0.7) +
    theme_minimal() +
    labs(
      title = paste("Hourly Activity — Period 4 — ID", bird),
      x = "Date & Time",
      y = "Locomotion (%)"
    )
  p
  # ---- Save plot ----
  file_path <- file.path(out_dir, paste0(bird, "_period4_activity.pdf"))
  ggsave(file_path, p, width = 10, height = 5, dpi = 300)
  file_path <- file.path(out_dir, paste0(bird, "_period4_activity.png"))
  ggsave(file_path, p, width = 10, height = 5, dpi = 300)
  
  message("Saved: ", file_path)
}





#Now for Arabian crossing in spring

for (bird in all_ids) {
  
  # All data for this bird
  df_id <- merged_df %>% 
    filter(ID == bird)
  
  # Find all days where period == 4
  p12_days<- df_id %>%
    filter(period == 12) %>%
    pull(date_day)
  
  # If no period 12 exists → skip bird
  if (length(p12_days) == 0) next
  
  # Identify window limits
  start_day <- min(p12_days) - 1   # day before first P4 day
  end_day   <- max(p12_days) + 2   # day after last P4 day
  
  # Crop entire bird dataset to the extended window
  plot_df <- df_id %>%
    filter(date_day >= start_day,
           date_day <= end_day)
  
  # ---- Create night shading intervals ----
  # A rectangle from today's sunset → tomorrow's sunrise
  shade_df <- plot_df %>%
    group_by(date_day) %>%
    summarise(
      sunset      = max(sunset, na.rm = TRUE),
      sunrise     = min(sunrise, na.rm = TRUE)
    ) %>%
    arrange(date_day) %>%
    mutate(
      next_sunrise = lead(sunrise) # sunrise of next day
    ) %>%
    filter(!is.na(sunset) & !is.na(next_sunrise))
  
  # ---- Create daily shading dataframe for Arabian desert crossing ----
  first_12 <- plot_df %>%
    filter(lat >= 12) %>%
    arrange(date) %>%
    slice(1)
  
  first_30 <- plot_df %>%
    filter(lat >= 30) %>%
    arrange(date) %>%
    slice(1)
  
  t_start <- if (nrow(first_12) > 0) first_12$date else NA
  t_start
  t_end   <- if (nrow(first_30) > 0) first_30$date else NA
  t_end
  
  lat_rect_df <- if (!is.na(t_start) & !is.na(t_end)) {
    data.frame(xmin = t_start, xmax = t_end)
  } else {
    NULL
  }
  
  if (length(lat_rect_df) == 0) next
  
  # ---- Create folder 
  out_dir <- file.path(base_dir, bird)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # ---- Plot 
  p <- ggplot(plot_df, aes(x = date, y = activity_percentage)) +
    # Night shading (if available)
    geom_rect(
      data = shade_df,
      inherit.aes = FALSE,
      aes(
        xmin = sunset,
        xmax = next_sunrise,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "grey70",
      alpha = 0.3
    ) +
    # ---- Latitude shading (12°–30°) ----
  geom_rect(data = lat_rect_df,
            inherit.aes = FALSE,
            aes(
              xmin = xmin,
              xmax = xmax,
              ymin = -Inf,
              ymax = Inf
            ),
            fill = "lightblue",
            alpha = 0.4,
            colour = NA
  )+
    # Activity curve
    geom_line(linewidth = 0.7, alpha = 0.9) +
    geom_point(size = 1.2, alpha = 0.7) +
    theme_minimal() +
    labs(
      title = paste("Hourly Activity — Period 12 — ID", bird),
      x = "Date & Time",
      y = "Locomotion (%)"
    )
  p
  # ---- Save plot ----
  file_path <- file.path(out_dir, paste0(bird, "_period12_activity.png"))
  ggsave(file_path, p, width = 10, height = 5, dpi = 300)
  
  message("Saved: ", file_path)
}



##### Modeling diurnal activity inside/outside migratory barrier ##### 
plot_df_autumn <- daily_summary %>%
  filter(seasons == "Autumn") %>%
  filter(!is.na(lat), !is.na(daytime_activity_percentage)) %>%
  mutate(
    barrier = ifelse(lat >= 18 & lat <= 37,
                     "Inside barrier (18°–37°N)",
                     "Outside barrier")
  )

ggplot(plot_df_autumn, aes(x = barrier, y = daytime_activity_percentage, )) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(aes(group = ID),
              width = 0.15, alpha = 0.2, size = 1.3) +
  theme_minimal()+
  theme(legend.position = "none")+
  labs(
    title = "Autumn migration",
    x = "",
    y = "Diurnal locomotion (%)"
  ) 


library(lme4)
library(lmerTest)   # p-values for fixed effects
library(emmeans)    # estimated marginal means & contrasts
model_barrier_mixed <- lmer(
  daytime_activity_percentage ~ barrier + (1 | ID),
  data = plot_df_autumn
)

summary(model_barrier_mixed)
VarCorr(model_barrier_mixed)
par(mfrow = c(1, 2))
plot(model_barrier_mixed)      # residuals vs fitted
qqnorm(residuals(model_barrier_mixed))
qqline(residuals(model_barrier_mixed))
par(mfrow = c(1, 1))

emm_barrier <- emmeans(model_barrier_mixed, ~ barrier)
emm_barrier


#SPRING

plot_df_spring <- daily_summary %>%
  filter(seasons == "Spring") %>%
  filter(!is.na(lat), !is.na(daytime_activity_percentage)) %>%
  mutate(
    barrier = ifelse(lat >= 12 & lat <= 30,
                     "Inside barrier (12°–31°N)",
                     "Outside barrier")
  )

ggplot(plot_df_spring, aes(x = barrier, y = daytime_activity_percentage, fill = )) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  theme_minimal()+
  geom_jitter(aes(group = ID),
              width = 0.15, alpha = 0.2, size = 1.3) +
  theme(legend.position = "none")+
  labs(
    title = "Spring migration",
    x = "",
    y = "Diurnal locomotion (%)"
  )+
  ylim(0,20)



library(lme4)
library(lmerTest)
model_barrier_mixed_spring <- lmer(
  daytime_activity_percentage ~ barrier + (1 | ID),
  data = plot_df_spring
)

summary(model_barrier_mixed_spring)
VarCorr(model_barrier_mixed_spring)
par(mfrow = c(1, 2))
plot(model_barrier_mixed_spring)
qqnorm(residuals(model_barrier_mixed_spring))
qqline(residuals(model_barrier_mixed_spring))
par(mfrow = c(1, 1))
library(emmeans)

emm_barrier_spring <- emmeans(
  model_barrier_mixed_spring,
  ~ barrier
)

emm_barrier_spring
pairs(emm_barrier_spring)



#diurnal activity statistics per season OVer Mediterranean and Sahara
# --- PERIODS: span in days (date range) ---
period_stats_ID <- daily_summary %>%
  filter(!is.na(lat)) %>% 
  group_by(ID, period_activity) %>%
  summarise(
    # optional: how many days actually observed (if you care about gaps)
    n_days_observed = n_distinct(date),
    mean_daytime_activity_id = mean(daytime_activity_percentage, na.rm = TRUE),
    n_days_2activityorless = sum(daytime_activity_percentage <= 2 , na.rm = TRUE),
    n_days_0activity  = sum(daytime_activity_percentage == 0, na.rm = TRUE),
    n_days_inside_barrier = n_distinct(date[lat >= 18 & lat <= 37]),
    n_days_0activity_over_barrier = sum(daytime_activity_percentage == 0 & lat >= 18 & lat <= 37, na.rm = TRUE), # change to number for Arabian Peninusla 30 - 12; 37-18 MEditerranean barrier
    n_days_2activity_over_barrier = sum(daytime_activity_percentage <= 2 & lat >= 18 & lat <= 37, na.rm = TRUE),
    n_days_2activity_outside_barrier = n_days_2activityorless-n_days_2activity_over_barrier
  ) 
period_stats_ID

period_stats <- period_stats_ID %>% 
  group_by(period_activity) %>%
  filter(period_activity == 4) %>%  #4 forMEditerranean and Sahara; 12 for Arabian Peninsula
  summarise(
    n_IDs = n_distinct(ID),
    
    # --- Daytime activity ---
    mean_daytime_activity = mean(mean_daytime_activity_id, na.rm = TRUE),
    sd_daytime_activity   = sd(mean_daytime_activity_id, na.rm = TRUE),
    
    # --- Observation effort ---
    mean_n_days_observed  = mean(n_days_observed, na.rm = TRUE),
    sd_n_days_observed    = sd(n_days_observed, na.rm = TRUE),
    
    # --- Low activity (≤1%) (all latitudes) ---
    mean_days_low_activity = mean(n_days_2activityorless, na.rm = TRUE),
    sd_days_low_activity   = sd(n_days_2activityorless, na.rm = TRUE),
    proportion_days_low_act =
      mean_days_low_activity / mean_n_days_observed,
    
    # --- Zero activity (all latitudes) ---
    mean_days_0activity = mean(n_days_0activity, na.rm = TRUE),
    sd_days_0activity   = sd(n_days_0activity, na.rm = TRUE),
    
    # --- Days inside barrier ---
    median_days_inside_barrier =
      median(n_days_inside_barrier, na.rm = TRUE),
    mean_days_inside_barrier =
      mean(n_days_inside_barrier, na.rm = TRUE),
    sd_days_inside_barrier =
      sd(n_days_inside_barrier, na.rm = TRUE),
    
    # --- Zero activity over barrier ---
    mean_days_0activity_barrier =
      mean(n_days_0activity_over_barrier, na.rm = TRUE),
    sd_days_0activity_barrier =
      sd(n_days_0activity_over_barrier, na.rm = TRUE),
    
    # --- Low activity (≤1%) over barrier ---
    mean_days_low_activity_barrier =
      mean(n_days_2activity_over_barrier, na.rm = TRUE),
    sd_days_low_activity_barrier =
      sd(n_days_2activity_over_barrier, na.rm = TRUE),
    
    mean_n_days_2activity_outside_barrier=
      mean(n_days_2activity_outside_barrier, na.rm = TRUE),
    sd_n_days_2activity_outside_barrier=
      sd(n_days_2activity_outside_barrier, na.rm = TRUE),
    .groups = "drop"
  )

period_stats




# Spring Arabian peninsula barrier numbers:
period_stats_ID <- daily_summary %>%
  filter(!is.na(lat)) %>% 
  group_by(ID, period_activity) %>%
  summarise(
    # optional: how many days actually observed (if you care about gaps)
    n_days_observed = n_distinct(date),
    mean_daytime_activity_id = mean(daytime_activity_percentage, na.rm = TRUE),
    n_days_2activityorless = sum(daytime_activity_percentage <= 2 , na.rm = TRUE),
    n_days_0activity  = sum(daytime_activity_percentage == 0, na.rm = TRUE),
    n_days_inside_barrier= n_distinct(date[lat >= 12 & lat <= 30]),
    n_days_0activity_over_barrier = sum(daytime_activity_percentage == 0 & lat >= 12 & lat <= 30, na.rm = TRUE), # change to number for Arabian Peninusla 30 - 12; 37-18 MEditerranean barrier
    n_days_2activity_over_barrier = sum(daytime_activity_percentage <=2 & lat >= 12 & lat <= 30, na.rm = TRUE),
    n_days_2activity_outside_barrier = n_days_2activityorless-n_days_2activity_over_barrier
  ) 
period_stats_ID

period_stats <- period_stats_ID %>% 
  group_by(period_activity) %>%
  filter(period_activity == 12) %>%  #4 forMEditerranean and Sahara; 12 for Arabian Peninsula
  summarise(
    n_IDs = n_distinct(ID),
    
    # --- Daytime activity ---
    mean_daytime_activity = mean(mean_daytime_activity_id, na.rm = TRUE),
    sd_daytime_activity   = sd(mean_daytime_activity_id, na.rm = TRUE),
    
    # --- Observation effort ---
    mean_n_days_observed  = mean(n_days_observed, na.rm = TRUE),
    sd_n_days_observed    = sd(n_days_observed, na.rm = TRUE),
    
    # --- Low activity (≤1%) (all latitudes) ---
    mean_days_low_activity = mean(n_days_2activityorless, na.rm = TRUE),
    sd_days_low_activity   = sd(n_days_2activityorless, na.rm = TRUE),
    proportion_days_low_act =
      mean_days_low_activity / mean_n_days_observed,
    
    # --- Zero activity (all latitudes) ---
    mean_days_0activity = mean(n_days_0activity, na.rm = TRUE),
    sd_days_0activity   = sd(n_days_0activity, na.rm = TRUE),
    
    # --- Days inside barrier ---
    median_days_inside_barrier =
      median(n_days_inside_barrier, na.rm = TRUE),
    mean_days_inside_barrier =
      mean(n_days_inside_barrier, na.rm = TRUE),
    sd_days_inside_barrier =
      sd(n_days_inside_barrier, na.rm = TRUE),
    
    # --- Zero activity over barrier ---
    mean_days_0activity_barrier =
      mean(n_days_0activity_over_barrier, na.rm = TRUE),
    sd_days_0activity_barrier =
      sd(n_days_0activity_over_barrier, na.rm = TRUE),
    
    # --- Low activity (≤1%) over barrier ---
    mean_days_low_activity_barrier =
      mean(n_days_2activity_over_barrier, na.rm = TRUE),
    sd_days_low_activity_barrier =
      sd(n_days_2activity_over_barrier, na.rm = TRUE),
    
    mean_n_days_2activity_outside_barrier=
      mean(n_days_2activity_outside_barrier, na.rm = TRUE),
    sd_n_days_2activity_outside_barrier=
      sd(n_days_2activity_outside_barrier, na.rm = TRUE),
    .groups = "drop"
  )

period_stats











##### Daytime activity per latitude #####
#autumn
# Create the plot for spring and then autumn in ggplot
data_mig_autumn_lat <- daily_summary %>%
  filter(ID != "90A") %>% 
  #ilter(ID == "DBA") %>% 
  filter(between(day_of_year, 166, 365)) %>%
  group_by(ID, lat) %>% 
  reframe(
    n_rows        = n(),                             # records (days)
    n_birds       = n_distinct(ID),                  # unique IDs
    mean_activity = mean(daytime_activity_percentage, na.rm = TRUE),
    SE            = sd(daytime_activity_percentage, na.rm = TRUE) / sqrt(n_rows),
    ID = ID,
    stopover = stopover_id
  ) %>%
  distinct()

autumn_mean_lat <- data_mig_autumn_lat %>%
  mutate(Lat_bin_3 = round(lat /3) * 3) %>%    # 3-degree bins 
  group_by(Lat_bin_3) %>%
  summarise(
    mean_across = mean(mean_activity, na.rm = TRUE),
    SE_across   = sd(mean_activity, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# daily_summary %>% filter(between(yday, 166, 365)) %>%
#   ggplot(aes(x = daytime_activity_percentage, y = lat, group = ID)) +
#   # Background annotation rectangle
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 18, ymax = 37,
#            fill = "#b4271a", alpha = 0.2) +
#   # Add mean point for each Lat_bin
#   # Add mean points for each Lat_bin
#   geom_point(size = 4, color = "#b4271a") +
#   geom_path(size = 1, color = "#b4271a")+
#   theme_minimal() +
#   labs(
#     x = "Daytime activity (%)",
#     y = "Latitude",
#     title = "Daytime activity per latitude"
#   )

library(viridis)
ggplot(data_mig_autumn_lat,aes(x = mean_activity, 
                               y = lat, 
                               group = ID, 
                               color = ID)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 18, ymax = 37,
           fill = "#b4271a", alpha = 0.2) +
  geom_point(size = 2, alpha = 0.6) +
  geom_path(size = 1, alpha = 0.6) +
  scale_color_viridis_d(option = "A", begin = 0.2, end = 0.8) +  # reddish tones
  theme_minimal() +
  labs(x = "Diurnal locomotion (%)",
       y = "Latitude",
       color = "ID")+
  scale_y_continuous( breaks = c(-20, -10, 0, 10, 20, 30, 40, 50))+
  geom_path(data = autumn_mean_lat,
            aes(x = mean_across, y = Lat_bin_3),
            inherit.aes = FALSE,
            color = "black", size = 1.5)


library(tidyplots)
save_plot(
  plot = ggplot2::last_plot(),
  filename = paste("./output/diurnal activity latitude all indiv.pdf"),
  width = 4,
  height = 8,
  units = "in",
)  

#Spring
# Create the plot for spring and then autumn in ggplot
data_mig_spring_lat <- daily_summary %>%
  filter(!ID %in% c("91A", "90A")) %>% 
  filter(period_activity > 9) %>% 
  filter(between(day_of_year, 1, 165)) %>%
  group_by(ID, lat) %>% 
  summarise(
    n_rows        = n(),                             # records (days)
    n_birds       = n_distinct(ID),                  # unique IDs
    mean_activity = mean(daytime_activity_percentage, na.rm = TRUE),
    SE            = sd(daytime_activity_percentage, na.rm = TRUE) / sqrt(n_rows),
    ID = ID
  ) %>%
  distinct()


spring_mean_lat <- data_mig_spring_lat %>%
  mutate(Lat_bin_3 = round(lat /3) * 3) %>%    # 5-degree bins 
  group_by(Lat_bin_3) %>%
  summarise(
    mean_across = mean(mean_activity, na.rm = TRUE),
    SE_across   = sd(mean_activity, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(data_mig_spring_lat, aes(x = mean_activity, y = lat, group = ID, color = ID)) +
  # Background annotation rectangle
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = 12, ymax = 30,
           fill = "#1aa7b4", alpha = 0.2) +
  # Points + line connecting them
  geom_point(size = 2, alpha = 0.6) +
  geom_path(size = 1, alpha = 0.6) +
  scale_color_viridis_d(option = "D", begin = 0.2, end = 0.8) +  # bluish tones
  theme_minimal() +
  labs(x = "Diurnal locomotion (%)",
       y = "Latitude")+
  scale_y_continuous( breaks = c(-20, -10, 0, 10, 20, 30, 40, 50))+
  geom_path(data = spring_mean_lat,
            inherit.aes = FALSE,
            aes(x = mean_across, y = Lat_bin_3),
            color = "black", size = 1.5)


library(tidyplots)
save_plot(
  plot = ggplot2::last_plot(),
  filename = paste("./output/diurnal activity latitude all indiv spring.png"),
  width = 4,
  height = 8,
  units = "in",
)  



