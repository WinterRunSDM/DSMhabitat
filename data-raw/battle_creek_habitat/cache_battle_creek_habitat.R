library(tidyverse)

# Update to add additional habitat projects -------------------------------
# This script adds Battle Creek habitat improvement projects to the data object 
calsim_30_day <- function(data) {
  dur_30 <-  data |>
    mutate(water_year = ifelse(month(date) %in% 10:12, year(date) + 1, year(date))) |>
    group_by(water_year) |>
    mutate(roll_mean = zoo::rollapply(flow_cfs, FUN = min,
                                      width = month(date), fill = NA, align = "left")) |>
    summarise(stat_in_duration = mean(roll_mean, na.rm = TRUE)) |>
    mutate(dist = round(cume_dist(-stat_in_duration), 3)) |>
    arrange(dist)
  
  interpolate_probs_30 <- approxfun(x = dur_30$dist, y = dur_30$stat_in_duration)
  d30 <- interpolate_probs_30(0.5)
  
  return(d30)
}

existing_cfs_median_comparison_point <- function (habitat_type, watershed, species, calsim_version) {
  spawning_months <- switch(species, 
                            "fr" = c(10:12),
                            "sr" = c(7:10),
                            "wr" = c(5:7))
  rearing_months <- switch(species,
                           "fr" = c(1:8), 
                           "sr" = c(1:5),
                           "wr" = c(1:5)) #c(5:9)
  if (habitat_type == "spawning") {
    DSMflow::flows_cfs[[calsim_version]] |> 
      filter(date >= as_date("1979-01-01")) |> 
      filter(month(date) %in% spawning_months) |> 
      pull(watershed) |> 
      median()
  } else if (habitat_type == "inchannel rearing") {
    DSMflow::flows_cfs[[calsim_version]] |> 
      filter(date >= as_date("1979-01-01")) |> 
      filter(month(date) %in% rearing_months) |> 
      pull(watershed) |> 
      median()
  } else if (habitat_type == "floodplain rearing") {
    if (watershed == "Lower-mid Sacramento River") {
      if(calsim_version == "action_5") {
        # TODO remove once we have the lower mid sac 1 and 2 node mapping
        flood = DSMflow::flows_cfs[[calsim_version]] |>
          filter(date >= as_date("1979-01-01")) |> 
          filter(month(date) %in% rearing_months) |> 
          select(watershed, date) |>
          rename(flow_cfs = watershed)
        calsim_30_day(flood)
      } else {
        flood = DSMflow::flows_cfs[[calsim_version]] |>
          filter(date >= as_date("1979-01-01")) |> 
          filter(month(date) %in% rearing_months) |> 
          select(`Lower-mid Sacramento River1`, `Lower-mid Sacramento River2`, date) |>
          mutate(flow_cfs = 35.6/58 * `Lower-mid Sacramento River1` + 22.4/58 * `Lower-mid Sacramento River2`)
        calsim_30_day(flood)
      }
    } else {
      flood = DSMflow::flows_cfs[[calsim_version]] |>
        filter(date >= as_date("1979-01-01")) |> 
        filter(month(date) %in% rearing_months) |> 
        select(watershed, date) |>
        rename(flow_cfs = watershed)
      calsim_30_day(flood)
    }
  }
}

# Battle Creek habitat improvements for Lower Battle Creek
habitat_projects <- tribble(
  ~"watershed", ~"habitat_type", ~"run", ~"total_acres", ~"gradient_class", ~"percent_suitable",
  "Battle Creek", "floodplain rearing", "winter", 15, NA, 0.90,
  "Battle Creek", "floodplain rearing", "winter", 32.5, NA, 0.90,
  "Battle Creek", "floodplain rearing", "winter", 39, NA, 0.90,
  "Battle Creek", "floodplain rearing", "winter", 22.5, NA, 0.90,
  "Battle Creek", "floodplain rearing", "winter", 11.25, NA, 0.90,
  "Battle Creek", "floodplain rearing", "winter", 4.8, NA, 0.90,
)

project_hab_added <- habitat_projects |> 
  mutate(suitable_acres = total_acres * percent_suitable) |> 
  group_by(watershed, habitat_type, run) |> 
  summarize(suitable_acres = sum(suitable_acres)) |> pull(suitable_acres)

# MW: This is the standard methodology used in R2R however it doesn't seem to be working for 
# Battle Creek because floodplain doesn't start getting activated until 1473 cfs. We chose to use this
# value and add to battle_creek$WR_floodplain_acres. This all gets done in set-floodplain-habitat.R
# and then run in cache-habitat.R 
thirty_day_mean_exceedence <- existing_cfs_median_comparison_point("floodplain rearing",
                                                                   "Battle Creek", "wr",
                                                                   "action_5")
# set_habitat <- DSMhabitat::set_floodplain_habitat(watershed, species, thirty_day_mean_exceedence)


