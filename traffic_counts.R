suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(timetk))

args <- commandArgs(trailingOnly = T)
filename <- args[1]
findpeak <- args[2]

counts <- read_json(filename)

subfolder <- paste0(word(filename,-2,sep=fixed("\\")),'/')
filename <- word(filename,-1,sep=fixed("\\"))
# input .tc file should hopefully have streets aligned correctly with compass direction
streets <- c(word(filename, 1), word(filename, 2))
# bind nested JSON into tidy df
counts <- do.call(rbind.data.frame, counts$loggedVehicles)

# correct the formatting for Excel counts template
counts <- counts |>
  mutate(
         dir = case_when(
           turning == 'left' ~ 'L',
           turning == 'right' ~ 'R',
           turning == 'straight' ~ 'T',
           turning == 'uturn' ~ 'U'
         ),
         dir = if_else(vehicleType == 'pedestrian', 'Peds', dir),
         vehicleType = if_else(vehicleType == 'pedestrian', 'car', vehicleType),
         timestamp = as.POSIXct(timestamp, origin = "2001-01-01",
                                tz = "America/New_York")
  ) |>
  mutate(heading = factor(heading, c('SB','WB','NB','EB')),
         dir = factor(dir, c('Peds','R','T','L','U')),
         vehicleType = factor(vehicleType, c('car','heavyTruck','lightTruck','bicycle')))

# determine if AM or PM peak and filter counts for within broadest AM/PM peak
count_dt <- min(counts$timestamp)
meridian <- if_else(count_dt < ymd_hms(paste(date(count_dt), "12:00:00"), tz = "America/New_York"), "AM", "PM")
am_peak <- interval(ymd_hms(paste(date(count_dt), "07:00:00"), tz = "America/New_York"),
                    ymd_hms(paste(date(count_dt), "10:00:00"), tz = "America/New_York"))
pm_peak <- interval(ymd_hms(paste(date(count_dt), "16:00:00"), tz = "America/New_York"),
                    ymd_hms(paste(date(count_dt), "19:00:00"), tz = "America/New_York"))

counts <- counts |> filter(timestamp %within% am_peak | timestamp %within% pm_peak)

# first export: for full period, aggregate by 15 min interval, mode, direction
counts_sum_all <- counts |>
  group_by(heading, dir, vehicleType) |>
  summarise_by_time(timestamp, .by = "15 minutes", tot = n()) |>
  ungroup() |> relocate(timestamp, vehicleType) |>
  arrange(timestamp, vehicleType, heading, dir)

# needed to print timestamps in correct timezone
timestamps <- as_datetime(str_unique(counts_sum_all$timestamp), tz = "America/New_York")

# expand for interval-mode-directions with zero observations
all <- expand_grid(timestamp = timestamps,
                   vehicleType = factor(c('car','heavyTruck','lightTruck','bicycle')),
                   heading = factor(c('SB','WB','NB','EB')),
                   dir = factor(c('Peds','R','T','L')))

# reformat interval for easier copy-paste
counts_sum_all <- counts_sum_all |>
  right_join(all, by = c("timestamp","vehicleType","heading","dir")) |>
  mutate(int = paste(str_sub(timestamp, 12, 16),
                     str_sub(timestamp + minutes(15), 12, 16), sep="-")) |>
  relocate(timestamp, int, vehicleType) |>
  arrange(timestamp, vehicleType, heading, dir) |>
  {\(.) {replace(.,is.na(.),0)}}()

counts_sum_all_w <- counts_sum_all |>
  select(-timestamp) |> pivot_wider(names_from = int, values_from = tot)

write_csv(counts_sum_all_w,
          paste0("out/", streets[1], '_', streets[2], '_', meridian, "_all.csv"))

# second export: find peak hour start through 15-min increment rolling sum
peak_hour <- counts |>
  summarise_by_time(timestamp, .by = "15 minutes", count = n()) |>
  mutate(sum_roll15 = slidify_vec(count, .period = 4, .f = sum, .align = "left", na.rm = F)) |>
  slice(which.max(sum_roll15))

print(paste("Peak hour:", peak_hour$timestamp))

# filter export for peak hour
counts_sum_peak <- counts_sum_all |>
  filter(timestamp %within% interval(peak_hour$timestamp, peak_hour$timestamp + hours() - seconds())) |>
  select(-timestamp) |> pivot_wider(names_from = int, values_from = tot)

if (!dir.exists(paste0("out/",subfolder))){
  dir.create(paste0("out/",subfolder))
}

write_csv(counts_sum_peak,
          paste0("out/", subfolder, streets[1], '_', streets[2], '_', meridian, ".csv"))
