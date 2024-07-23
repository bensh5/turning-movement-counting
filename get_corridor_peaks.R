library(tidyverse)
library(hms)

all <-
  list.files(path = "rscripts/p_traffic_counts/out/19th", pattern = "\\.*all.csv$",
             full.names = T) |>
  map_df(~read_csv(.)) |>
  mutate(across(everything(), ~replace_na(.x, 0)))

sum <- colSums(all[,4:ncol(all)]) |> stack() |>
  separate_wider_delim(ind, "-", names = c("int1", "int2")) |>
  mutate(int1 = as_hms(paste0(int1,":00"))) |> arrange(int1)

ampeak <- filter(sum, int1 < as_hms("12:00:00")) |>
  mutate(roll_sum = RcppRoll::roll_sum(values, 4, align = "left", fill = NA)) |>
  slice(which.max(roll_sum))
ampeak <- as.character(ampeak$int1)

pmpeak <- filter(sum, int1 > as_hms("12:00:00")) |>
  mutate(roll_sum = RcppRoll::roll_sum(values, 4, align = "left", fill = NA)) |>
  slice(which.max(roll_sum))
pmpeak <- as.character(pmpeak$int1)

