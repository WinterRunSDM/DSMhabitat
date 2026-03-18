library(tidyverse)

# This script adds Battle Creek habitat improvement projects to the data object 

source(here::here("data-raw", "battle_creek_habitat", "helper-functions.R"))

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

action_5_fp_wr <- DSMhabitat::wr_fp$action_5

project_hab_added <- habitat_projects |> 
  mutate(suitable_acres = total_acres * percent_suitable) |> 
  group_by(watershed, habitat_type, run) |> 
  summarize(suitable_acres = sum(suitable_acres)) |> pull(suitable_acres)

# MW: This is the standard methodology used in R2R however it doesn't seem to be working for 
# Battle Creek because floodplain doesn't start getting activated until 1473 cfs. For now
# I am going to take the median flow from the WUA and we can modify if needed. 
# 
# thirty_day_mean_exceedence <- existing_cfs_median_comparison_point("floodplain rearing", 
#                                                                    "Battle Creek", "wr", 
#                                                                    "action_5")
# set_habitat <- DSMhabitat::set_floodplain_habitat(watershed, species, thirty_day_mean_exceedence)

median_flow <- median(DSMhabitat::battle_creek_floodplain$flow_cfs)
set_habitat <- DSMhabitat::set_floodplain_habitat("Battle Creek", 'wr', median_flow)
project_hab_sqmeters <- DSMhabitat::acres_to_square_meters(project_hab_added)
prop_added <- ifelse(set_habitat == 0, 0, project_hab_sqmeters/set_habitat) 

add_project_habitat <- DSMhabitat::wr_fp$action_5["Battle Creek" , , ] * prop_added
updated_habitat <- DSMhabitat::wr_fp$action_5["Battle Creek", , ] + add_project_habitat

action_5_fp_wr["Battle Creek", , ] <- updated_habitat 
