# run this script to update all the habitat data
# Base data
source("data-raw/cached-habitat.R")

# R2R baseline scaling on various hydrologies
source("data-raw/R2R_baseline_habitat_inputs/cache_baseline_hab.R")
source("data-raw/R2R_baseline_habitat_inputs/cache_action5_with_baseline_hab.R")

# Max Habitat scaling on various hydrologies
source("data-raw/R2R_TMH_habitat_inputs/cache_tmh_data.R")
source("data-raw/R2R_TMH_habitat_inputs/cache_run_of_river_tmh_data.R")
source("data-raw/above_shasta_habitat/cache_action_5_tmh_data.R")

# add BC-2 spawning and rearing acres 
source("data-raw/battle_creek_habitat/cache_battle_creek_habitat.R")
