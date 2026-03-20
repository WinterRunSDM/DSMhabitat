library(tidyverse)
library(lubridate)

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

# Pull existing flow comparison point
existing_cfs_median_comparison_point <- function (habitat_type, watershed, species, calsim_version) {
  spawning_months <- switch(species, 
                            "fr" = c(10:12),
                            "sr" = c(7:10),
                            "wr" = c(5:7))
  rearing_months <- switch(species,
                           "fr" = c(1:8), 
                           "sr" = c(1:5),
                           "wr" = c(5:9))#c(1:5))
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


# get rearing and spawning habitat objects  -------------------------------

# get rearing habitat for all watersheds 
get_rear_hab_all_battle <- function(watersheds, species, life_stage, calsim_version, years = 1980:1999) {
  total_obs <- 12 * length(years)
  most <- map_df(watersheds, function(watershed) {
    flows <- get_flow(watershed, calsim_version, range(years))

      habitat <- DSMhabitat::set_instream_habitat(watershed,
                                                  species = species,
                                                  life_stage = life_stage,
                                                  flow = flows)
      
      tibble(
        year = rep(years, each = 12),
        month = rep(1:12, length(years)),
        watershed = watershed,
        hab_sq_m = habitat)
    
  })
  
  # action 5 does not have sacramento special cases
  if(calsim_version == "action_5") {
    low_mid_sac_action_5_flow <- get_flow("Lower-mid Sacramento River", 
                                          calsim_version, 
                                          range(years))
    low_mid_sac_hab <- DSMhabitat::set_instream_habitat("Lower-mid Sacramento River",
                                                        species = species,
                                                        life_stage = life_stage,
                                                        flow = low_mid_sac_action_5_flow)
  } else {
    # deal with sacramento special cases
    # lower-mid sac
    low_mid_sac_flow1 <- get_flow('Lower-mid Sacramento River1', calsim_version, range(years))
    low_mid_sac_flow2 <- get_flow('Lower-mid Sacramento River2', calsim_version, range(years))
    
    low_mid_sac_hab <- map2_dbl(low_mid_sac_flow1, low_mid_sac_flow2, function(flow, flow2) {
      DSMhabitat::set_instream_habitat('Lower-mid Sacramento River',
                                       species = species,
                                       life_stage = life_stage,
                                       flow = flow, flow2 = flow2)
    })
    
  }
  
  low_mid_sac <- tibble(
    year = rep(years, each = 12),
    month = rep(1:12, length(years)),
    watershed = 'Lower-mid Sacramento River',
    hab_sq_m = low_mid_sac_hab)
  
  hab <- bind_rows(most, low_mid_sac) %>%
    spread(watershed, hab_sq_m) %>% 
    bind_cols(tibble(`Sutter Bypass` = rep(0, total_obs),
                     `Yolo Bypass` = rep(0, total_obs))) %>%
    gather(watershed, habitat, -year, -month) %>%
    mutate(date = lubridate::ymd(paste(year, month, 1, '-'))) %>%
    select(date, watershed, habitat) %>%
    spread(date, habitat) %>%
    left_join(watersheds_order) %>%
    arrange(order) %>%
    select(-watershed, -order) %>%
    create_SIT_array()
  
  return(hab)
}

# get spawning habitat for all watersheds
get_spawn_hab_all <- function(watersheds, species, calsim_version, years = 1979:2000) {
  total_obs <- 12 * length(years)
  most <- map_df(watersheds, function(watershed) {
    flows <- get_flow(watershed, calsim_version, years=range(years))
    
    habitat <- DSMhabitat::set_spawning_habitat(watershed,
                                                species = species,
                                                flow = flows)
    
    tibble(
      year = rep(years, each = 12),
      month = rep(1:12, length(years)),
      watershed = watershed,
      hab_sq_m = habitat)
    
  })
  
  # deal with sacramento special cases
  # upper sac
  up_sac_flows <- get_flow('Upper Sacramento River', calsim_version, years=range(years))
  months <- rep(1:12, length(years))
  up_sac_hab <- map2_dbl(months, up_sac_flows, function(month, flow) {
    DSMhabitat::set_spawning_habitat('Upper Sacramento River',
                                     species = species,
                                     flow = flow, month = month)
  })
  
  up_sac <- tibble(
    year = rep(years, each = 12),
    month = rep(1:12, length(years)),
    watershed = 'Upper Sacramento River',
    hab_sq_m = up_sac_hab)
  
  hab <-   bind_rows(most, up_sac) %>%
    spread(watershed, hab_sq_m) %>%
    bind_cols(tibble(`Sutter Bypass` = rep(NA, total_obs),
                     `Yolo Bypass` = rep(NA, total_obs),
                     `Upper-mid Sacramento River` = rep(NA, total_obs),
                     `Lower-mid Sacramento River` = rep(NA, total_obs),
                     `Lower Sacramento River` = rep(NA, total_obs)))
  
  if(species != "sr") {
    
    hab <- hab |> 
      bind_cols(tibble(`San Joaquin River` = rep(NA, total_obs)))
  }
  
  hab <- hab |> 
    gather(watershed, habitat, -year, -month) %>%
    mutate(date = lubridate::ymd(paste(year, month, 1, '-'))) %>%
    select(date, watershed, habitat) %>%
    spread(date, habitat) %>%
    left_join(watersheds_order) %>%
    arrange(order) %>%
    select(-watershed, -order) %>%
    create_SIT_array()
  
  return(hab)
}

# wua to area
wua_to_area_battle <- function(wua, watershed_name,  life_stage, species_name) {
  stream_length <- dplyr::pull(dplyr::filter(DSMhabitat::watershed_lengths,
                                             watershed == watershed_name,
                                             species == species_name,
                                             lifestage == life_stage), feet)
  if (length(stream_length) == 0) {
    stream_length <- dplyr::pull(dplyr::filter(DSMhabitat::watershed_lengths,
                                               watershed == watershed_name,
                                               species == 'fr',
                                               lifestage == life_stage), feet)
  }
  
  ((stream_length/1000) * wua)/10.7639
}

# rearing


# instream_hab
set_instream_habitat_battle <- function(watershed, species, life_stage, flow, ...) {
  
  species_present <- subset(DSMhabitat::watershed_species_present, watershed_name == watershed,
                            species, drop = TRUE)
  
  if (!species_present) {
    return(NA)
  }
  
  quantification_mode <- subset(DSMhabitat::watershed_methods, 
                                watershed_name == watershed, instream, drop = TRUE)
  
  if (watershed %in% c('Upper Sacramento River', 'Upper-mid Sacramento River',
                       'Lower-mid Sacramento River', 'Lower Sacramento River')) {
    return(set_sac_habitat(watershed, flow, ...))
  }
  
  if (DSMhabitat::watershed_species_present$use_mid_sac_rear_proxy[DSMhabitat::watershed_species_present$watershed_name == watershed]) {
    watershed_name <- "Upper Mid Sac Region"
    species <- "fr"
  } else {
    watershed_name <- watershed
  }
  
  watershed_name <- tolower(gsub(pattern = "-| ", replacement = "_", x = watershed_name))
  watershed_rda_name <- paste(watershed_name, "instream", sep = "_")
  df <- as.data.frame(do.call(`::`, list(pkg = "DSMhabitat", name = watershed_rda_name)))
  
  hab_column <- get_habitat_selector(names(df), species, life_stage, mode = quantification_mode)
  df_na_rm <- df[!is.na(df[, hab_column]), ]
  flows <- df_na_rm[, "flow_cfs"]
  habs <- df_na_rm[ , hab_column]
  hab_func <- approxfun(flows, habs , rule = 2)
  
  
  if (quantification_mode == "wua") {
    wua <- hab_func(flow)
    habitat_area <- wua_to_area(wua = wua, watershed = watershed,
                                life_stage = "rearing", species_name = species)
  } else {
    habitat_area <- hab_func(flow)
  }
  
  return(habitat_area)
}