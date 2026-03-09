# This script formats the Theoretical Maximum Habitat 
# values from GIS output to inputs into DSMhabitat within the 
# `cache_tmh_data.R` file 

library(tidyverse)

#run <- 'Fall Run' #'Spring Run' 'Winter Run'
run <- 'Spring Run'
area = 'Shasta'

tmh_data_format <- function(run, area) {
  
  source('data-raw/R2R_TMH_habitat_inputs/tmh_helper_functions.R')
  
  spwn_perc_suitable <- 0.12
  
  floodplain_perc_suitable <- 0.9
  
  rearing_perc_suitable_low <- 0.1
  rearing_perc_suitable_0_1 <- 0.78
  rearing_perc_suitable_1_2 <- 0.78
  rearing_perc_suitable_2_4 <- 0.585
  rearing_perc_suitable_4_8 <- 0.195
  
  # url <- 'https://docs.google.com/spreadsheets/d/1-3WZAcOzrk5ugZq4CAYK1LRLP0005NmjwpifNQHZ_iA/edit#gid=0'
  if(area == "Shasta") {
    # this adds Pit and McCloud as seperate areas rather than being part of Upper Sacramento River
    gradients_raw <- readxl::read_excel('data-raw/R2R_TMH_habitat_inputs/River Length Summary.xlsx', sheet = "Shasta Watershed Summary Table") 
  } else {
    gradients_raw <- readxl::read_excel('data-raw/R2R_TMH_habitat_inputs/River Length Summary.xlsx', sheet = "Watershed Summary Table") 
  }
  
  gradients <- gradients_raw |> 
    janitor::clean_names() |> 
    fill(watershed) |> 
    filter(!is.na(river)) |> 
    mutate(gradient = ifelse(hqt_boundary == "Valley Lowland (<0.4%)", 0, gradient)) |> 
    mutate(river_length_ft_grad = as.numeric(length_miles)*5280) |> 
    select(river = watershed, dam, river_length_ft_grad, gradient) |> 
    mutate(perc_suitable_rearing = case_when(gradient < 0 ~ 0.1, 
                                             gradient == 0 ~ rearing_perc_suitable_low,
                                             gradient > 0 & gradient < 1 ~ rearing_perc_suitable_0_1, 
                                             gradient > 1 & gradient < 2 ~ rearing_perc_suitable_1_2,
                                             gradient > 2 & gradient < 4 ~ rearing_perc_suitable_2_4,
                                             gradient > 4 ~ rearing_perc_suitable_4_8 
    )) |> 
    mutate(perc_suitable_rearing = as.numeric(perc_suitable_rearing)) |> 
    glimpse()
  
  # Above Dam Calculations --------------------------------------------------
  
  # read in modeled values as inputs
  above_dam <- readxl::read_excel('data-raw/R2R_TMH_habitat_inputs/Cleaned Floodplain Width Calculations.xlsx', sheet = "Above Dam") |> 
    janitor::clean_names() 
  
  above_dam_calcs <- above_dam |> 
    gis_calcs() |> 
    glimpse()
  
  above_dam_acres <- above_dam_calcs |> 
    group_by(river) |> 
    mutate(above_dam_spawning_acres = mean_channel_width * river_length_feet * spwn_perc_suitable / 43560,
           above_dam_floodplain_acres = (mean_inflection_width - mean_channel_width) * river_length_feet * floodplain_perc_suitable / 43560) |>
    ungroup() |> 
    inner_join(gradients |> filter(dam == "above dam")) |> 
    mutate(above_dam_rearing_acres = mean_channel_width * river_length_ft_grad * perc_suitable_rearing / 43560) |> 
    select(-river_length_ft_grad, -gradient, -perc_suitable_rearing) |>
    group_by(river, mean_channel_width, mean_inflection_width, above_dam_spawning_acres, above_dam_floodplain_acres) |> 
    summarise(above_dam_rearing_acres = sum(above_dam_rearing_acres)) |> 
    mutate(river = ifelse(grepl("San Joaquin River", river), "San Joaquin River", river)) 
  
  # Below Dam Calculations --------------------------------------------------
  
  below_dam <- readxl::read_excel('data-raw/R2R_TMH_habitat_inputs/Cleaned Floodplain Width Calculations.xlsx', sheet = "Below Dam") |> 
    janitor::clean_names() 
  
  below_dam_cals <- below_dam |> 
    gis_calcs() |> 
    glimpse()
  
  below_dam_acres <- below_dam_cals |> 
    group_by(river) |> 
    mutate(below_dam_spawning_acres = mean_channel_width * river_length_feet * spwn_perc_suitable / 43560,
           below_dam_floodplain_acres = (mean_inflection_width - mean_channel_width) * river_length_feet * floodplain_perc_suitable / 43560) |>
    ungroup() |> 
    mutate(regulated = "yes") |>
    inner_join(gradients |> filter(dam == "below dam")) |> 
    mutate(below_dam_rearing_acres = mean_channel_width * river_length_ft_grad * perc_suitable_rearing / 43560) |> 
    select(-river_length_ft_grad, -gradient, -perc_suitable_rearing) |>
    group_by(river, mean_channel_width, mean_inflection_width, below_dam_spawning_acres, below_dam_floodplain_acres, regulated) |> 
    summarise(below_dam_rearing_acres = sum(below_dam_rearing_acres)) |> 
    mutate(river = ifelse(grepl("San Joaquin River", river), "San Joaquin River", river)) 
  
  # HEC RAS -----------------------------------------------------------------
  # all below dam: 
  
  hec_ras <- readxl::read_excel('data-raw/R2R_TMH_habitat_inputs/Cleaned Floodplain Width Calculations.xlsx', sheet = "Hec Ras") |> 
    janitor::clean_names() 
  
  hec_ras_calc <- hec_ras |>  
    mutate(perc_suitable_rearing = case_when(gradient == 0 ~ rearing_perc_suitable_low,
                                             gradient > 0 & gradient < 1 ~ rearing_perc_suitable_0_1, 
                                             gradient > 1 & gradient < 2 ~ rearing_perc_suitable_1_2,
                                             gradient > 2 & gradient < 4 ~ rearing_perc_suitable_2_4,
                                             gradient > 4 ~ rearing_perc_suitable_4_8 )) |> 
    mutate(perc_suitable_rearing = as.numeric(perc_suitable_rearing)) |> 
    group_by(river) |> 
    mutate(channel_area_acres = channel_width * channel_length / 43560,
           below_dam_floodplain_acres = (projected_wse_area_acres - channel_area_acres) * floodplain_perc_suitable,
           below_dam_rearing_acres = ifelse(!is.na(hec_ras_channel_area_for_median_flow_during_rearing_acres), 
                                            hec_ras_channel_area_for_median_flow_during_rearing_acres * perc_suitable_rearing,
                                            channel_area_acres * perc_suitable_rearing), 
           below_dam_spawning_acres = ifelse(!is.na(hec_ras_channel_area_for_median_flow_during_spawning_acres), 
                                             hec_ras_channel_area_for_median_flow_during_spawning_acres * 
                                               spwn_perc_suitable, channel_area_acres * spwn_perc_suitable), 
           regulated = "yes + modeled with HEC RAS") |> 
    select(river, regulated, below_dam_floodplain_acres:below_dam_spawning_acres) |> 
    group_by(river, regulated) |> 
    summarise(below_dam_floodplain_acres = sum(below_dam_floodplain_acres),
              below_dam_rearing_acres = sum(below_dam_rearing_acres),
              below_dam_spawning_acres = sum(below_dam_spawning_acres))
  
  # add the below dam to the hec ras model outputs 
  all_below_dam_acres <- bind_rows(below_dam_acres, hec_ras_calc) 
  
  
  # total above and below dam acres  ---------------------------------------
  
  if(run %in% c("Winter Run", "Spring Run")) {
    total_acres <- all_below_dam_acres |> ungroup() |> 
      mutate(river = ifelse(grepl("San Joaquin River", river), "San Joaquin River", river)) |> 
      select(-mean_channel_width, -mean_inflection_width) |> 
      full_join(above_dam_acres |> ungroup() |>  
                  mutate(river = ifelse(grepl("San Joaquin River", river), "San Joaquin River", river)) |> 
                  select(-c(mean_channel_width, mean_inflection_width))
      ) |>  
      group_by(river, regulated) |> 
      summarise(max_spawning_acres = sum(below_dam_spawning_acres, above_dam_spawning_acres, na.rm = TRUE), 
                max_rearing_acres = sum(below_dam_rearing_acres, above_dam_rearing_acres, na.rm = TRUE), 
                max_floodplain_acres = sum(below_dam_floodplain_acres, above_dam_floodplain_acres, na.rm = TRUE)) |> 
      glimpse()
  } else if (run == "Fall Run") {
    ## FALL RUN: 
    ### does not include above dam
    total_acres <- all_below_dam_acres |> 
      ungroup() |> 
      select(-c(mean_channel_width, mean_inflection_width)) |> 
      # join in above dam JUST FOR SAN JOAQUIN, Liz edit 
      left_join(above_dam_acres |> ungroup() |>  
                  mutate(river = ifelse(grepl("San Joaquin River", river), "San Joaquin River", river)) |> 
                  select(-c(mean_channel_width, mean_inflection_width))
      ) |>  
      mutate(river = ifelse(grepl("San Joaquin River", river), "San Joaquin River", river)) |> 
      group_by(river, regulated) |>
      # San Joaquin can combine above dam and below dam acres to sum. Keep the same for every other stream
      summarise(max_spawning_acres = ifelse(river == "San Joaquin River", 
                                            sum(below_dam_spawning_acres, above_dam_spawning_acres, na.rm = TRUE),
                                            below_dam_spawning_acres), 
                max_rearing_acres = ifelse(river == "San Joaquin River",
                                           sum(below_dam_rearing_acres, above_dam_rearing_acres, na.rm = TRUE),
                                           below_dam_rearing_acres), 
                max_floodplain_acres = ifelse(river == "San Joaquin River",
                                              sum(below_dam_floodplain_acres, above_dam_floodplain_acres, na.rm = TRUE),
                                              below_dam_floodplain_acres))
  }
  # Unregulated Calculations ------------------------------------------------
  
  unregulated <- readxl::read_excel('data-raw/R2R_TMH_habitat_inputs/Cleaned Floodplain Width Calculations.xlsx', sheet = "Unregulated") |> 
    janitor::clean_names() 
  
  unreg_calcs <- unregulated |> 
    gis_calcs() |> 
    glimpse()
  
  unreg_acres <- unreg_calcs |> 
    group_by(river) |> 
    mutate(max_spawning_acres = mean_channel_width * river_length_feet * spwn_perc_suitable / 43560,
           max_floodplain_acres = (mean_inflection_width - mean_channel_width) * river_length_feet * floodplain_perc_suitable / 43560) |>
    select(-c(mean_inflection_width, river_length_feet)) |> 
    inner_join(gradients |> filter(dam == "unregulated")) |> 
    mutate(max_rearing_acres = mean_channel_width * river_length_ft_grad * perc_suitable_rearing / 43560) |> 
    select(-river_length_ft_grad, -gradient, -perc_suitable_rearing) |>
    group_by(river, mean_channel_width, max_spawning_acres, max_floodplain_acres) |> 
    summarise(max_rearing_acres = sum(max_rearing_acres)) |> 
    mutate(regulated = "no") |> 
    glimpse()
  
  
  # Combine Regulated and Non Regulated into a single DF --------------------
  
  all_max_habitat <- bind_rows(total_acres, unreg_acres) |> 
    select(river:max_floodplain_acres) |> 
    rename(watershed = river) |> 
    glimpse()
  
  
  # write.csv(all_max_habitat, 'data/all_watersheds_max_habitat.csv')
  
  # aggregate widths/lengths ------------------------------------------------
  
  hec_ras_calc <- hec_ras |> 
    rename(mean_channel_width = channel_width) |> 
    select(river, mean_channel_width) |> 
    mutate(dam = "below dam") |> 
    glimpse()
  
  widths_lengths <- bind_rows(above_dam_calcs |> mutate(dam = "above dam"), 
                              below_dam_cals |>  mutate(dam = "below dam"), 
                              unreg_calcs |> mutate(dam = "unregulated"),
                              hec_ras_calc)
  
  #saveRDS(widths_lengths, '../r2r_model_inputs_app/data/above_dam_extents/river_widths_lengths.rds')
  
  
  tmh_acres <- bind_rows(above_dam_acres |> mutate(dam = "above dam"), 
                         below_dam_acres |>  mutate(dam = "below dam"), 
                         unreg_acres |> mutate(dam = "unregulated"),
                         hec_ras_calc |> mutate(dam = "below dam"))
  
  return(all_max_habitat)
  
}

# format_all_data ---------------------------------------------------------

fall_run_tmh <- tmh_data_format("Fall Run", "Shasta") |> 
  mutate(run = "Fall Run")

spring_and_winter_tmh <- tmh_data_format("Spring Run", "Shasta") |> 
  mutate(run = "Winter and Spring Run")

all_max_habitat <- fall_run_tmh |> 
  bind_rows(spring_and_winter_tmh)

if (area == "Shasta") {
  saveRDS(all_max_habitat |> mutate(modeled_for = "Shasta SDM"), "data-raw/R2R_TMH_habitat_inputs/all_habitat_data_for_tmh_inputs_all_runs_with_upper_sac.rdata")
} else {
  saveRDS(all_max_habitat, "data-raw/R2R_TMH_habitat_inputs/all_habitat_data_for_tmh_inputs_all_runs.rdata")
}
