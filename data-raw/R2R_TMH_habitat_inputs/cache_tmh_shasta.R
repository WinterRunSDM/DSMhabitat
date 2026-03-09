# file to cache Theoretical Max Habitat data objects 
# and some exploratory plots to compare SIT existing 
# to TMH. 

library(tidyverse)
library(DSMhabitat)
library(lubridate)

source('data-raw/R2R_TMH_habitat_inputs/tmh_helper_functions.R')

calsim_run <- "action_5"

# Shasta Data Object Definitions: 
# 1. `action_5_upper_sac_tmh...` -- Upper Sac - redding to redbluff and above dam extent mapped by Yoshiyama 
# 2. `action_5_upper_sac_pit_tmh...` -- Upper Sac (above def) + Pit River
# 3. `action_5_upper_sac_mccloud_tmh...` -- Upper Sac (above def) + McCloud River
# 4. `action_5_upper_sac_pit_mccloud_tmh...` -- Upper Sac (above def) + Pit River + McCloud River

# FALL RUN: does not extend past reservoirs -------------------------------
# Winter and Spring Run: extend past reservoirs ---------------------------

# update DSMhabitat values ------------------------------------------------
watersheds_trunc <- DSMscenario::watershed_labels[!(DSMscenario::watershed_labels %in%  c('North Delta', "South Delta", "Sutter Bypass", "Yolo Bypass"))]

action_5_upper_sac_tmh_wr_spawn <- spawn_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = calsim_run, area = "Shasta", extent = 1)
action_5_upper_sac_pit_tmh_wr_spawn <- spawn_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = calsim_run, area = "Shasta", extent = 2)
action_5_upper_sac_mccloud_tmh_wr_spawn <- spawn_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = calsim_run, area = "Shasta", extent = 3)
action_5_upper_sac_pit_mccloud_tmh_wr_spawn <- spawn_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = calsim_run, area = "Shasta", extent = 4)

#action_5_upper_sac_tmh_wr_spawn["Upper Sacramento River",,] < action_5_upper_sac_pit_tmh_wr_spawn["Upper Sacramento River",,]

## inchannel habitat to both fry and juvenile habitat objects ---------------
action_5_upper_sac_tmh_wr_fry <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = calsim_run, area = "Shasta", extent = 1)$fry
action_5_upper_sac_tmh_wr_juv <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = calsim_run, area = "Shasta", extent = 1)$juv

action_5_upper_sac_pit_tmh_wr_fry <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = calsim_run, area = "Shasta", extent = 2)$fry
action_5_upper_sac_pit_tmh_wr_juv <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = calsim_run, area = "Shasta", extent = 2)$juv

action_5_upper_sac_mccloud_tmh_wr_fry <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = calsim_run, area = "Shasta", extent = 3)$fry
action_5_upper_sac_mccloud_tmh_wr_juv <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = calsim_run, area = "Shasta", extent = 3)$juv

action_5_upper_sac_pit_mccloud_tmh_wr_fry <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = calsim_run, area = "Shasta", extent = 4)$fry
action_5_upper_sac_pit_mccloud_tmh_wr_juv <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = calsim_run, area = "Shasta", extent = 4)$juv

#action_5_upper_sac_pit_mccloud_tmh_wr_juv["Upper Sacramento River",,] == action_5_upper_sac_mccloud_tmh_wr_juv["Upper Sacramento River",,]

##floodplain: -------------------------------------------------------------
action_5_upper_sac_tmh_wr_flood <- floodplain_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = calsim_run, area = "Shasta", extent = 1)
action_5_upper_sac_pit_tmh_wr_flood <- floodplain_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = calsim_run, area = "Shasta", extent = 2)
action_5_upper_sac_mccloud_tmh_wr_flood <- floodplain_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = calsim_run, area = "Shasta", extent = 3)
action_5_upper_sac_pit_mccloud_tmh_wr_flood <- floodplain_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = calsim_run, area = "Shasta", extent = 4)

action_5_upper_sac_pit_mccloud_tmh_wr_flood["Upper Sacramento River",,] == action_5_upper_sac_mccloud_tmh_wr_flood["Upper Sacramento River",,]

##delta: -------------------------------------------------------------------
# r_to_r_tmh_delta <- delta_tmh_processing()

# save data objects -------------------------------------------------------

# Save as data object to DSMhabitat
## FLOODPLAIN:
#### Winter Run:
wr_fp <- modifyList(DSMhabitat::wr_fp, list(action_5_upper_sac_tmh = action_5_upper_sac_tmh_wr_flood,
                                            action_5_upper_sac_pit_tmh = action_5_upper_sac_pit_tmh_wr_flood,
                                            action_5_upper_sac_mccloud_tmh = action_5_upper_sac_mccloud_tmh_wr_flood,
                                            action_5_upper_sac_pit_mccloud_tmh = action_5_upper_sac_pit_mccloud_tmh_wr_flood))

wr_fp <- wr_fp[!names(wr_fp) %in% c(
  "action_5_tmh",
  "action_5_1_tmh",
  "action_5_2_tmh",
  "action_5_3_tmh",
  "action_5_4_tmh"
)]
usethis::use_data(wr_fp, overwrite = TRUE)

## IN CHANNEL REARING:
### Winter Run: 
wr_fry <- modifyList(DSMhabitat::wr_fry, list(action_5_upper_sac_tmh = action_5_upper_sac_tmh_wr_fry,
                                              action_5_upper_sac_pit_tmh = action_5_upper_sac_pit_tmh_wr_fry,
                                              action_5_upper_sac_mccloud_tmh = action_5_upper_sac_mccloud_tmh_wr_fry,
                                              action_5_upper_sac_pit_mccloud_tmh = action_5_upper_sac_pit_mccloud_tmh_wr_fry))
wr_fry <- wr_fry[!names(wr_fry) %in% c(
  "action_5_tmh",
  "action_5_1_tmh",
  "action_5_2_tmh",
  "action_5_3_tmh",
  "action_5_4_tmh"
)]

usethis::use_data(wr_fry, overwrite = TRUE)



wr_juv <- modifyList(DSMhabitat::wr_juv, list(action_5_upper_sac_tmh = action_5_upper_sac_tmh_wr_juv,
                                              action_5_upper_sac_pit_tmh = action_5_upper_sac_pit_tmh_wr_juv,
                                              action_5_upper_sac_mccloud_tmh = action_5_upper_sac_mccloud_tmh_wr_juv,
                                              action_5_upper_sac_pit_mccloud_tmh = action_5_upper_sac_pit_mccloud_tmh_wr_juv))
wr_juv <- wr_juv[!names(wr_juv) %in% c(
  "action_5_tmh",
  "action_5_1_tmh",
  "action_5_2_tmh",
  "action_5_3_tmh",
  "action_5_4_tmh"
)]

usethis::use_data(wr_juv, overwrite = TRUE)

## SPAWNING: 
### Winter Run:
wr_spawn <- modifyList(DSMhabitat::wr_spawn, list(action_5_upper_sac_tmh = action_5_upper_sac_tmh_wr_spawn,
                                                  action_5_upper_sac_pit_tmh = action_5_upper_sac_pit_tmh_wr_spawn,
                                                  action_5_upper_sac_mccloud_tmh = action_5_upper_sac_mccloud_tmh_wr_spawn,
                                                  action_5_upper_sac_pit_mccloud_tmh = action_5_upper_sac_pit_mccloud_tmh_wr_spawn))
wr_spawn <- wr_spawn[!names(wr_spawn) %in% c(
  "action_5_tmh",
  "action_5_1_tmh",
  "action_5_2_tmh",
  "action_5_3_tmh",
  "action_5_4_tmh"
)]

usethis::use_data(wr_spawn, overwrite = TRUE)

#delta_habitat <- c(DSMhabitat::delta_habitat, r_to_r_tmh = list(r_to_r_tmh_delta))
#usethis::use_data(delta_habitat, overwrite = TRUE)

# do some checks, but make sure you build library first 

# commented this out because it doesn't work unless you build, and so it causes an error in sourcing update_data.R

# comparingn to baseline: 
table(DSMhabitat::wr_spawn$action_5_baseline == DSMhabitat::wr_spawn$action_5_upper_sac_tmh)
table(DSMhabitat::wr_fp$action_5_baseline == DSMhabitat::wr_fp$action_5_upper_sac_tmh)
table(DSMhabitat::wr_juv$action_5_baseline == DSMhabitat::wr_juv$action_5_upper_sac_tmh)

# comparing extents: 
table(DSMhabitat::wr_spawn$action_5_upper_sac_mccloud_tmh == DSMhabitat::wr_spawn$action_5_upper_sac_tmh)
table(DSMhabitat::wr_fp$action_5_upper_sac_mccloud_tmh == DSMhabitat::wr_fp$action_5_upper_sac_tmh)
table(DSMhabitat::wr_juv$action_5_upper_sac_mccloud_tmh == DSMhabitat::wr_juv$action_5_upper_sac_tmh)

# Exploratory Plots:  -----------------------------------------------------
## spawning plot:  ---------------------------------------------------------
tmh_comparison_plot(tmh_data = DSMhabitat::wr_spawn$action_5_upper_sac_tmh,
                    baseline = DSMhabitat::wr_spawn$action_5_upper_sac_mccloud_tmh, "spawn",
                    legend_labels = c(
                      baseline = "action_5_upper_sac_mccloud_tmh",
                      r_to_r_max_habitat = "action_5_upper_sac_tmh"),
                    legend_colors = c(
                        baseline = "#1b9e77",
                        r_to_r_max_habitat = "#d95f02"
                      ),
                    title = "action_5_upper_sac_tmh compared with action_5_upper_sac_mccloud_tmh for spawning"
                    )

## fry and juv plots:  -----------------------------------------------------
tmh_comparison_plot(tmh_data = DSMhabitat::wr_fry$action_5_upper_sac_tmh,
                    baseline = DSMhabitat::wr_fry$action_5_upper_sac_pit_tmh, "fry",
                    legend_labels = c(
                      baseline = "action_5_upper_sac_tmh",
                      r_to_r_max_habitat = "action_5_upper_sac_pit_tmh"),
                    legend_colors = c(
                      baseline = "#1b9e77",
                      r_to_r_max_habitat = "#d95f02"
                    ),
                    title = "action_5_upper_sac_tmh compared with action_5_upper_sac_pit_tmh for fry"
)

tmh_comparison_plot(tmh_data = DSMhabitat::wr_fry$action_5_upper_sac_tmh,
                    baseline = DSMhabitat::wr_fry$action_5, "fry",
                    legend_labels = c(
                      baseline = "action_5",
                      r_to_r_max_habitat = "action_5_upper_sac_pit_tmh"),
                    legend_colors = c(
                      baseline = "#1b9e77",
                      r_to_r_max_habitat = "#d95f02"
                    ),
                    title = "action_5 compared with action_5_upper_sac_pit_tmh for fry"
)


## floodplain exploratory plot:  -------------------------------------------

# winter run:

tmh_comparison_plot(tmh_data = DSMhabitat::wr_fp$action_5_upper_sac_tmh,
                    baseline = DSMhabitat::wr_fp$action_5_upper_sac_pit_tmh, "flood",
                    legend_labels = c(
                      baseline = "action_5_upper_sac_tmh",
                      r_to_r_max_habitat = "action_5_upper_sac_pit_tmh"),
                    legend_colors = c(
                      baseline = "#1b9e77",
                      r_to_r_max_habitat = "#d95f02"
                    ),
                    title = "action_5_upper_sac_tmh compared with action_5_upper_sac_pit_tmh for floodplain"
)

tmh_comparison_plot(tmh_data = DSMhabitat::wr_fp$action_5_baseline,
                    baseline = DSMhabitat::wr_fp$action_5_upper_sac_pit_tmh, "flood",
                    legend_labels = c(
                      baseline = "action_5_baseline",
                      r_to_r_max_habitat = "action_5_upper_sac_pit_tmh"),
                    legend_colors = c(
                      baseline = "#1b9e77",
                      r_to_r_max_habitat = "#d95f02"
                    ),
                    title = "action_5_baseline compared with action_5_upper_sac_pit_tmh for floodplain"
)


