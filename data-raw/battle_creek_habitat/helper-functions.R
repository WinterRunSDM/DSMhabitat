library(tidyverse)
library(lubridate)

# calsim_30_day <- function(data) {
#   dur_30 <-  data |>
#     mutate(water_year = ifelse(month(date) %in% 10:12, year(date) + 1, year(date))) |> 
#     group_by(water_year) |>
#     mutate(roll_mean = zoo::rollapply(flow_cfs, FUN = min, 
#                                       width = month(date), fill = NA, align = "left")) |>
#     summarise(stat_in_duration = mean(roll_mean, na.rm = TRUE)) |>
#     mutate(dist = round(cume_dist(-stat_in_duration), 3)) |>
#     arrange(dist)
#   
#   interpolate_probs_30 <- approxfun(x = dur_30$dist, y = dur_30$stat_in_duration)
#   d30 <- interpolate_probs_30(0.5) 
#   
#   return(d30)
# }

# Pull existing flow comparison point
existing_cfs_median_comparison_point <- function (habitat_type, watershed, species, calsim_version) {
  spawning_months <- switch(species, 
                            "fr" = c(10:12),
                            "sr" = c(7:10),
                            "wr" = c(5:7))
  rearing_months <- switch(species,
                           "fr" = c(1:8), 
                           "sr" = c(1:5),
                           "wr" = c(1:5))
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

existing_cfs_median_comparison_point("inchannel rearing", "American River", "fr", 'biop_itp_2018_2019')
existing_cfs_median_comparison_point("floodplain rearing", "Tuolumne River", "fr", 'biop_itp_2018_2019')


# calculate the proportion change frm projects 
hab_prop_change_from_projects <- function(habitat_type, watershed, species, lifestage, calsim_version, habitat_projects) {
  # calculate total sq meters added per watershed and habitat type 
  hab <- habitat_type
  ws <- watershed 
  
  selected_run <- switch(species, 
                         "fr" = "fall", 
                         "sr" = "spring",
                         "wr" = "winter")
  
  # pull project hab out of project catalog 
  project_hab_added <- habitat_projects |> 
    mutate(suitable_acres = total_acres * percent_suitable) |> 
    group_by(watershed, habitat_type, run) |> 
    summarize(suitable_acres = sum(suitable_acres)) |> 
    filter(watershed == ws & habitat_type == hab & run == selected_run) |> pull(suitable_acres)
  
  project_hab_sqmeters <- DSMhabitat::acres_to_square_meters(project_hab_added)
  if (habitat_type == "inchannel rearing" & watershed == "Upper-mid Sacramento River") {
    median_flow <- existing_cfs_median_comparison_point(habitat_type, watershed, species, calsim_version)
    sit_habitat <- DSMhabitat::set_instream_habitat(watershed, "fr", lifestage, median_flow)
  } 
  if (habitat_type == "inchannel rearing" & watershed != "Upper-mid Sacramento River") {
    median_flow <- existing_cfs_median_comparison_point(habitat_type, watershed, species, calsim_version)
    sit_habitat <- DSMhabitat::set_instream_habitat(watershed, species, lifestage, median_flow)
  } 
  if (habitat_type == "spawning") {
    month <- 2 #TODO check in with mark on if we want to compare to Acids boards in or out 
    median_flow <- existing_cfs_median_comparison_point(habitat_type, watershed, species, calsim_version)
    sit_habitat <- DSMhabitat::set_spawning_habitat(watershed, species, median_flow, month)
    
  }
  if (habitat_type == "floodplain rearing" & watershed != "North Delta") {
    thirty_day_mean_exceedence <- existing_cfs_median_comparison_point(habitat_type, 
                                                                       watershed, species, 
                                                                       calsim_version)
    sit_habitat <- DSMhabitat::set_floodplain_habitat(watershed, species, thirty_day_mean_exceedence)
  }
  if (habitat_type == "floodplain rearing" & watershed == "Tuolumne River") {
    # pull comparison flow from FlowWest modeling instead of using the 30 day exceedence 
    comparison_flow <- 2500
    sit_habitat <- DSMhabitat::set_floodplain_habitat(watershed, species, comparison_flow)
  }
  if (watershed == "North Delta") {
    # Instead of taking hab at the median flow to compare take median hab 
    # Check in with Mark on this assumption 
    sit_habitat <- median(DSMhabitat::delta_habitat$sit_habitat[ , , "North Delta"])
  }
  
  # find proportion of habitat added 
  # TODO resolve yuba floodplain problem 
  prop_added <- ifelse(sit_habitat == 0, 0, project_hab_sqmeters/sit_habitat) 
  return(prop_added)
} 

# hab_prop_change_from_projects("floodplain rearing", "North Delta", "fr", "juv", "biop_itp_2018_2019")
hab_prop_change_from_projects("floodplain rearing", "Tuolumne River", "fr", "juv", "biop_itp_2018_2019", habitat_projects)
hab_prop_change_from_projects("spawning", "Cottonwood Creek", "sr", "adult", "biop_itp_2018_2019", habitat_projects)

