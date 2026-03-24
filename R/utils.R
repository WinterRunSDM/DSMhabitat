#' WUA to Area in Square Meters
#' @description Function converts a Weighted Usable Area (WUA) to an
#' area in square meters by multiplying the WUA by the stream's length used by the species.
#' The lengths were obtained through expert outreach in 2017 and are available here
#'  \code{\link{watershed_lengths}}.
#' @param wua WUA in square feet per 1000 feet
#' @param watershed watershed
#' @param species species, "fr" - fall run or "sr" - spring run or "st" - steel head or "lfr" - late fall run
#' @param life_stage life stage
#' @param scenario_option this is specific to Shasta SDM where different watershed lengths need to be applied for Battle Creek
wua_to_area <- function(wua, watershed_name,  life_stage, species_name, scenario_option = NULL) {
  if(!is.null(scenario_option)) {
    # This is specific section for running scenarios in Shasta SDM; 
    # using the fall run lengths that do not include north fork for baseline and 
    # bc_5 scenario should include NF
    stream_length <- DSMhabitat::watershed_lengths |> 
      mutate(scenario = case_when(watershed == "Battle Creek" & species == "wr" ~ "bc_5")) |> 
      bind_rows(tribble(
        ~"order", ~"watershed", ~"lifestage", ~"miles", ~"feet", ~ "source", ~"species", ~"date_updated", ~"scenario",
        3, "Battle Creek", "spawning", 3.97, 20945, NA, "wr", "03/20/2026", "baseline",
        3, "Battle Creek", "rearing", 5.87, 31003, NA, "wr", "03/20/2026", "baseline"
      )) |> 
      dplyr::filter(watershed == watershed_name,
                    species == species_name,
                    lifestage == life_stage,
                    scenario == scenario_option) |>
      pull(feet)
    
    print(paste0("applying scenario: ", scenario_option, " with ", stream_length, " feet "))
    
  } else {
    stream_length <- dplyr::pull(dplyr::filter(DSMhabitat::watershed_lengths,
                                               watershed == watershed_name,
                                               species == species_name,
                                               lifestage == life_stage), feet)
  }
  if (length(stream_length) == 0) {
    stream_length <- dplyr::pull(dplyr::filter(DSMhabitat::watershed_lengths,
                                               watershed == watershed_name,
                                               species == 'fr',
                                               lifestage == life_stage), feet)
  }
  
  ((stream_length/1000) * wua)/10.7639
}

#' Get Habitat Selector
#' @description Habitat Modeling Column Lookup
#' @details Habitat modeling column lookup by species and lifestage. If desired
#' combination of species and lifestage is not present in modeling data table for a watershed,
#' then the column representing the most appropriate proxy will be returned.
#' @param species_wuas vector of column names within modeling data table for the targeted watershed
#' @param species target species: "fr" for fall run, "lfr" for late fall run, "sr" for spring run, and "st" for steelhead
#' @param life_stage "spawn", fry", "juv", or "adult"
#' @param mode one of "wua" or "hsi"
#' @return column name of desired habitat relationship
get_habitat_selector <- function(species_wuas, species, life_stage, mode = "wua") {
  
  species_lifestage <- paste(toupper(species), life_stage, sep = "_")
  
  if (mode == "wua") {
    combos <- switch(species_lifestage,
                     WR_spawn = c("WR_spawn_wua", "FR_spawn_wua", "SR_spawn_wua", 
                                  "ST_spawn_wua"),
                     SR_spawn = c("SR_spawn_wua", "FR_spawn_wua", "ST_spawn_wua"),
                     FR_spawn = c("FR_spawn_wua", "SR_spawn_wua", "ST_spawn_wua"),
                     LFR_spawn = c("LFR_spawn_wua", "FR_spawn_wua", "SR_spawn_wua", 
                                   "ST_spawn_wua"),
                     ST_spawn = c("ST_spawn_wua", "SR_spawn_wua", "FR_spawn_wua"),
                     WR_juv = c("WR_juv_wua", "SR_juv_wua", "FR_juv_wua", "WR_fry_wua",
                                "SR_fry_wua", "FR_fry_wua", "ST_juv_wua", "ST_fry_wua"),
                     WR_fry = c("WR_fry_wua", "SR_fry_wua",  "FR_fry_wua", "WR_juv_wua", 
                                "SR_juv_wua", "FR_juv_wua", "ST_fry_wua", "ST_juv_wua"),
                     SR_juv = c("SR_juv_wua", "FR_juv_wua", "SR_fry_wua",
                                "FR_fry_wua", "ST_juv_wua", "ST_fry_wua"),
                     SR_fry = c("SR_fry_wua", "FR_fry_wua", "SR_juv_wua",
                                "FR_juv_wua", "ST_fry_wua", "ST_juv_wua"),
                     FR_juv = c("FR_juv_wua", "SR_juv_wua", "FR_fry_wua",
                                "SR_fry_wua", "ST_juv_wua", "ST_fry_wua"),
                     FR_fry = c("FR_fry_wua", "FR_juv_wua", "SR_fry_wua",
                                "ST_fry_wua", "SR_juv_wua", "ST_juv_wua"),
                     LFR_juv = c("LFR_juv_wua", "FR_juv_wua", "SR_juv_wua", 
                                 "FR_fry_wua", "SR_fry_wua", "ST_juv_wua", "ST_fry_wua"),
                     LFR_fry = c("LFR_fry_wua", "FR_fry_wua", "FR_juv_wua", 
                                 "SR_fry_wua", "ST_fry_wua", "SR_juv_wua", "ST_juv_wua"),
                     ST_adult = c("ST_adult_wua"),
                     ST_juv = c("ST_juv_wua", "ST_fry_wua", "SR_juv_wua",
                                "SR_fry_wua", "FR_juv_wua", "FR_fry_wua"),
                     ST_fry = c("ST_fry_wua", "ST_juv_wua", "SR_fry_wua",
                                "SR_juv_wua", "FR_fry_wua", "FR_juv_wua"))
  } else if (mode == "hsi") {
    combos <- switch(species_lifestage,
                     WR_spawn = c("WR_spawn_sqm", "FR_spawn_sqm", "SR_spawn_sqm", 
                                  "ST_spawn_sqm"),
                     SR_spawn = c("SR_spawn_sqm", "FR_spawn_sqm", "ST_spawn_sqm"),
                     FR_spawn = c("FR_spawn_sqm", "SR_spawn_sqm", "ST_spawn_sqm"),
                     LFR_spawn = c("LFR_spawn_sqm", "FR_spawn_sqm", "SR_spawn_sqm", 
                                   "ST_spawn_sqm"),
                     ST_spawn = c("ST_spawn_sqm", "SR_spawn_sqm", "FR_spawn_sqm"),
                     WR_juv = c("WR_juv_sqm", "SR_juv_sqm", "FR_juv_sqm", "WR_fry_sqm",
                                "SR_fry_sqm", "FR_fry_sqm", "ST_juv_sqm", "ST_fry_sqm"),
                     WR_fry = c("WR_fry_sqm", "SR_fry_sqm",  "FR_fry_sqm", "WR_juv_sqm", 
                                "SR_juv_sqm", "FR_juv_sqm", "ST_fry_sqm", "ST_juv_sqm"),
                     SR_juv = c("SR_juv_sqm", "FR_juv_sqm", "SR_fry_sqm",
                                "FR_fry_sqm", "ST_juv_sqm", "ST_fry_sqm"),
                     SR_fry = c("SR_fry_sqm", "FR_fry_sqm", "SR_juv_sqm",
                                "FR_juv_sqm", "ST_fry_sqm", "ST_juv_sqm"),
                     FR_juv = c("FR_juv_sqm", "SR_juv_sqm", "FR_fry_sqm",
                                "SR_fry_sqm", "ST_juv_sqm", "ST_fry_sqm"),
                     FR_fry = c("FR_fry_sqm", "FR_juv_sqm", "SR_fry_sqm",
                                "ST_fry_sqm", "SR_juv_sqm", "ST_juv_sqm"),
                     LFR_juv = c("LFR_juv_sqm", "FR_juv_sqm", "SR_juv_sqm", 
                                 "FR_fry_sqm", "SR_fry_sqm", "ST_juv_sqm", "ST_fry_sqm"),
                     LFR_fry = c("LFR_fry_sqm", "FR_fry_sqm", "FR_juv_sqm", 
                                 "SR_fry_sqm", "ST_fry_sqm", "SR_juv_sqm", "ST_juv_sqm"),
                     ST_adult = c("ST_adult_sqm"),
                     ST_juv = c("ST_juv_sqm", "ST_fry_sqm", "SR_juv_sqm",
                                "SR_fry_sqm", "FR_juv_sqm", "FR_fry_sqm"),
                     ST_fry = c("ST_fry_sqm", "ST_juv_sqm", "SR_fry_sqm",
                                "SR_juv_sqm", "FR_fry_sqm", "FR_juv_sqm"))
  }
  
  
  return(combos[which(combos %in% species_wuas)[[1]]])
  
}

#' Square Meters to Acres
#' @description Function converts area in square meters to area in acres
#' @param sq_meters area in square meters
#' @examples
#' hab_sq_meters <- set_instream_habitat('Merced River', 'fr', 'fry', 425)
#' square_meters_to_acres(hab_sq_meters)
#'
#' @export
square_meters_to_acres <- function(sq_meters) {
  sq_meters * 0.000247105
}

#' Acres to Square Meters
#' @description Function converts area in acres to area in square meters
#' @param acres area in square meters
#' @export
acres_to_square_meters <- function(acres) {
  acres / 0.000247105
}

#' Duration of Floodplain Inundation
#' @description Function returns number of weeks of floodplain inundation given a flow for a watershed
#' @param ws name of CVPIA watershed
#' @param flow_cfs monthly mean flow in cubic feet per second
#' @return integer value for number of weeks
#' @export
#'
#' @details
#' These relationships between number of days inundated and the mean monthly flow
#' are stored in \code{\link{weeks_inundated}} and referenced by this function.
#' For watersheds without a defined relationship, a two week inundation duration is assumed.
#'
#' @examples
#' get_weeks_flooded('Yuba River', 900)
#'
get_weeks_flooded <- function(ws, flow_cfs) {
  
  flow_thresholds <- DSMhabitat::weeks_inundated[DSMhabitat::weeks_inundated$watershed == ws, 'flow_threshold']
  if (length(flow_thresholds) == 0) {
    return(0)
  }
  
  # select closest number of weeks inundated given flow without going over
  v <- flow_thresholds[!is.na(flow_thresholds)]
  number_of_weeks <- (0:4)[!is.na(flow_thresholds)]
  i <- which.min(abs(v - flow_cfs))
  closest_threshold_index <- ifelse(flow_cfs < v[i], i - 1, i)
  
  number_of_weeks[closest_threshold_index]
}

