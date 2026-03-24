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

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

plot_compare_month_year_matrix <- function(x1,
                                           x2,
                                           name1 = "Scenario 1",
                                           name2 = "Scenario 2",
                                           value_name = "value",
                                           title = NULL,
                                           x_lab = "Year",
                                           y_lab = "Value") {
  
  month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  df1 <- as.data.frame(x1) |>
    rownames_to_column(var = "month") |>
    pivot_longer(
      cols = -month,
      names_to = "year",
      values_to = value_name
    ) |>
    mutate(
      year = as.integer(year),
      month = factor(month, levels = month_levels, ordered = TRUE),
      dataset = name1
    )
  
  df2 <- as.data.frame(x2) |>
    rownames_to_column(var = "month") |>
    pivot_longer(
      cols = -month,
      names_to = "year",
      values_to = value_name
    ) |>
    mutate(
      year = as.integer(year),
      month = factor(month, levels = month_levels, ordered = TRUE),
      dataset = name2
    )
  
  df <- bind_rows(df1, df2)
  
  p <- ggplot(
    df,
    aes(
      x = year,
      y = .data[[value_name]],
      color = dataset,
      linetype = dataset,
      group = interaction(month, dataset)
    )
  ) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~month, ncol = 4) +
    labs(
      title = title,
      x = x_lab,
      y = y_lab,
      color = NULL,
      linetype = NULL
    ) +
    theme_minimal()
  
  list(
    data = df,
    plot = p
  )
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

# BC-2 add instream acres -------------------------------------------------
# Here - we are using a similar approach as TMH and baseline hab in R2R where
# instream and spawning existing WUAs are scaled by the proportion increase 
# caused by the new habitat acres. The only differences is that instead of 
# using the median battle creek flow, we use the median WUA flow because 
# Battle Creek action 5 flows are significatly larger than the WUA flows. 

# 1.3 acres proposed instream habitat - representing Battle Creek Wildlife Area
# 1.7 acres proposed instream habitat - representing Battle Creek Levee

instream_project_hab_acres <- 1.3 + 1.7
project_hab_sqmeters <- DSMhabitat::acres_to_square_meters(instream_project_hab_acres)

# MW: the instream and spawning median action 5 flow for battle creek is very high so we are going to scale f
# rom the median WUA flow instead 
# instream_flow <- existing_cfs_median_comparison_point("inchannel rearing",
#                                                       "Battle Creek", "wr",
#                                                       "action_5") # 672.2546

#instream_habitat_juv <- DSMhabitat::set_instream_habitat("Battle Creek", "wr", life_stage = "juv", instream_flow) # 72526.23
#instream_habitat_fry <- DSMhabitat::set_instream_habitat("Battle Creek", "wr", life_stage = "fry", instream_flow) # 35411.36

instream_wua_flow <- DSMhabitat::battle_creek_instream |> 
  select(flow_cfs, WR_fry_wua, WR_juv_wua, WR_spawn_wua) |> 
  filter(!is.na(flow_cfs)) |> 
  summarise(median_flow = median(flow_cfs)) |> 
  pull(median_flow) # 66 cfs

instream_habitat_juv <- DSMhabitat::set_instream_habitat("Battle Creek", "wr", life_stage = "juv", instream_wua_flow) # 104482.3
instream_habitat_fry <- DSMhabitat::set_instream_habitat("Battle Creek", "wr", life_stage = "fry", instream_wua_flow) # 60410.99

prop_added_juv <- project_hab_sqmeters/instream_habitat_juv
prop_added_fry <- project_hab_sqmeters/instream_habitat_fry

# update wr_juv
action_5_bc_2_wr_juv <- DSMhabitat::wr_juv$action_5

add_project_habitat_juv <- DSMhabitat::wr_juv$action_5["Battle Creek" , , ] * prop_added_juv
updated_habitat_juv <- DSMhabitat::wr_juv$action_5["Battle Creek", , ] + add_project_habitat_juv

action_5_bc_2_wr_juv["Battle Creek", , ] <- updated_habitat_juv
wr_juv <- modifyList(DSMhabitat::wr_juv, list("action_5_bc_2" = action_5_bc_2_wr_juv))
usethis::use_data(wr_juv, overwrite = TRUE)

# update wr_fry
action_5_bc_2_wr_fry <- DSMhabitat::wr_fry$action_5

add_project_habitat_fry <- DSMhabitat::wr_fry$action_5["Battle Creek" , , ] * prop_added_fry
updated_habitat_fry <- DSMhabitat::wr_fry$action_5["Battle Creek", , ] + add_project_habitat_fry

action_5_bc_2_wr_fry["Battle Creek", , ] <- updated_habitat_fry
wr_fry <- modifyList(DSMhabitat::wr_fry, list("action_5_bc_2" = action_5_bc_2_wr_fry))
usethis::use_data(wr_fry, overwrite = TRUE)

# spawning for BC-2
# spawning_flow <- existing_cfs_median_comparison_point("spawning",
#                                                       "Battle Creek", "wr",
#                                                       "action_5") # 302.8045
# spawning_habitat <- DSMhabitat::set_spawning_habitat("Battle Creek", "wr",  spawning_flow, scenario = NULL) #6080.704
spawning_habitat <- DSMhabitat::set_spawning_habitat("Battle Creek", "wr",  instream_wua_flow, scenario = NULL) # 13592.19

prop_added_spawn <- project_hab_sqmeters/spawning_habitat

# update wr_spawn
action_5_bc_2_wr_spawn <- DSMhabitat::wr_spawn$action_5

add_project_habitat_spawn <- DSMhabitat::wr_spawn$action_5["Battle Creek" , , ] * prop_added_spawn
updated_habitat_spawn <- DSMhabitat::wr_spawn$action_5["Battle Creek", , ] + add_project_habitat_spawn

action_5_bc_2_wr_spawn["Battle Creek", , ] <- updated_habitat_spawn
wr_spawn <- modifyList(DSMhabitat::wr_spawn, list("action_5_bc_2" = action_5_bc_2_wr_spawn))
usethis::use_data(wr_spawn, overwrite = TRUE)

# make plots comparing: 
spawn <- plot_compare_month_year_matrix(
  x1 = wr_spawn$action_5_bc_2["Battle Creek", , ],
  x2 = wr_spawn$action_5["Battle Creek", , ],
  name1 = "Action 5 BC 2",
  name2 = "Action 5",
  value_name = "area",
  title = "Battle Creek - BC-2 spawning"
)

spawn$plot

juv <- plot_compare_month_year_matrix(
  x1 = wr_juv$action_5_bc_2["Battle Creek", , ],
  x2 = wr_juv$action_5["Battle Creek", , ],
  name1 = "Action 5 BC 2",
  name2 = "Action 5",
  value_name = "area",
  title = "Battle Creek - BC-2 instream juv rearing"
)

juv$plot

fry <- plot_compare_month_year_matrix(
  x1 = wr_fry$action_5_bc_2["Battle Creek", , ],
  x2 = wr_fry$action_5["Battle Creek", , ],
  name1 = "Action 5 BC 2",
  name2 = "Action 5",
  value_name = "area",
  title = "Battle Creek - BC-2 instream fry rearing"
)

fry$plot

# floodplain: 
fp <- plot_compare_month_year_matrix(
  x1 = wr_fp$action_5_bc_2["Battle Creek", , ],
  x2 = wr_fp$action_5["Battle Creek", , ],
  name1 = "Action 5 BC 2",
  name2 = "Action 5",
  value_name = "area",
  title = "Battle Creek - BC-2 floodplain"
)

fp$plot
