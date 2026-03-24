library(tidyverse)
source("data-raw/R2R_baseline_habitat_inputs/generate_hab_helper_functions.R")

# FALL RUN ---------------------------------------------------------------------
# Update DSMhabitat values 
# set r_to_r_baseline_fr_spawn to 
action_5_baseline_fr_spawn <- DSMhabitat::fr_spawn$action_5

# Add american river spawning habitat 
add_project_habitat <- DSMhabitat::fr_spawn$action_5["American River" , , ] * 
  hab_prop_change_from_projects("spawning", "American River", "fr", "adult", "action_5")
updated_habitat <- DSMhabitat::fr_spawn$action_5["American River", , ] + add_project_habitat

action_5_baseline_fr_spawn["American River", , ] <- updated_habitat 

# add clear creek spawning habitat
add_project_habitat <- DSMhabitat::fr_spawn$action_5["Clear Creek" , , ] * 
  hab_prop_change_from_projects("spawning", "Clear Creek", "fr", "adult", "action_5")
updated_habitat <- DSMhabitat::fr_spawn$action_5["Clear Creek", , ] + add_project_habitat

action_5_baseline_fr_spawn["Clear Creek", , ] <- updated_habitat 

# add paynes creek spawning habitat
add_project_habitat <- DSMhabitat::fr_spawn$action_5["Paynes Creek" , , ] * 
  hab_prop_change_from_projects("spawning", "Paynes Creek", "fr", "adult", "action_5")
updated_habitat <- DSMhabitat::fr_spawn$action_5["Paynes Creek", , ] + add_project_habitat

action_5_baseline_fr_spawn["Paynes Creek", , ] <- updated_habitat 

# Add sacramento river spawning habitat 
add_project_habitat <- DSMhabitat::fr_spawn$action_5["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("spawning", "Upper Sacramento River", "fr", "adult", "action_5")
updated_habitat <- DSMhabitat::fr_spawn$action_5["Upper Sacramento River", , ] + add_project_habitat

action_5_baseline_fr_spawn["Upper Sacramento River", , ] <- updated_habitat 

# check habitat differences 
# upper sac and american river should be different (larger)
action_5_baseline_fr_spawn == DSMhabitat::fr_spawn$action_5

# Save as data object to DSMhabitat
fr_spawn <- modifyList(DSMhabitat::fr_spawn, list("action_5_baseline" = action_5_baseline_fr_spawn))
usethis::use_data(fr_spawn, overwrite = TRUE)

# Exploratory plot 
# r_to_r_baseline_action_5 <- action_5_baseline_fr_spawn |> 
#   DSMhabitat::square_meters_to_acres()
# 
# baseline <- DSMhabitat::fr_spawn$r_to_r_baseline |> DSMhabitat::square_meters_to_acres()
# 
# spawn <- expand_grid(
#   watershed = factor(DSMscenario::watershed_labels, 
#                      levels = DSMscenario::watershed_labels),
#   month = 1:12,
#   year = 1979:2000) |> 
#   arrange(year, month, watershed) |> 
#   mutate(
#     r_to_r_baseline = as.vector(baseline),
#     r_to_r_baseline_action_5 = as.vector(r_to_r_baseline_action_5)) |> 
#   filter(watershed %in% c("American River", 
#                           "Upper Sacramento River", 
#                           "Paynes Creek", 
#                           "Clear Creek"))
# 
# spawn |> 
#   transmute(watershed, date = ymd(paste(year, month, 1)), 
#             r_to_r_baseline, r_to_r_baseline_action_5) |> 
#   gather(version, acres, -watershed, -date)  |> 
#   ggplot(aes(date, acres, color = version)) +
#   geom_line(alpha = .75) + 
#   facet_wrap(~watershed, scales = 'free_y') + 
#   theme_minimal()




# Add inchannel habitat to both fry and juvenile habitat objects ---------------
# set r_to_r_baseline_fr_juv and fry 
action_5_baseline_fr_juv <- DSMhabitat::fr_juv$action_5
action_5_baseline_fr_fry <- DSMhabitat::fr_fry$action_5

# Add american river juv habitat 
add_project_habitat <- DSMhabitat::fr_juv$action_5["American River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "American River", "fr", "juv", "action_5")
updated_habitat <- DSMhabitat::fr_juv$action_5["American River", , ] + add_project_habitat

action_5_baseline_fr_juv["American River", , ] <- updated_habitat 

# Add american river fry habitat 
add_project_habitat <- DSMhabitat::fr_fry$action_5["American River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "American River", "fr", "fry", "action_5")
updated_habitat <- DSMhabitat::fr_fry$action_5["American River", , ] + add_project_habitat

action_5_baseline_fr_fry["American River", , ] <- updated_habitat 

# add tuolumne river juv habitat
add_project_habitat <- DSMhabitat::fr_juv$action_5["Tuolumne River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Tuolumne River", "fr", "juv", "action_5")
updated_habitat <- DSMhabitat::fr_juv$action_5["Tuolumne River", , ] + add_project_habitat

action_5_baseline_fr_juv["Tuolumne River", , ] <- updated_habitat 

# add tuolumne river fry habitat
add_project_habitat <- DSMhabitat::fr_fry$action_5["Tuolumne River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Tuolumne River", "fr", "fry", "action_5")
updated_habitat <- DSMhabitat::fr_fry$action_5["Tuolumne River", , ] + add_project_habitat

action_5_baseline_fr_fry["Tuolumne River", , ] <- updated_habitat 

# Upper sac river 
# Add upper sac river juv habitat 
add_project_habitat <- DSMhabitat::fr_juv$action_5["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "fr", "juv", "action_5")
updated_habitat <- DSMhabitat::fr_juv$action_5["Upper Sacramento River", , ] + add_project_habitat

action_5_baseline_fr_juv["Upper Sacramento River", , ] <- updated_habitat 

# Add upper sacramento  river fry habitat 
add_project_habitat <- DSMhabitat::fr_fry$action_5["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "fr", "fry", "action_5")

updated_habitat <- DSMhabitat::fr_fry$action_5["Upper Sacramento River", , ] + add_project_habitat

action_5_baseline_fr_fry["Upper Sacramento River", , ] <- updated_habitat 

# add Upper-mid Sacramento River
# Add upper mid sac river juv habitat 
add_project_habitat <- DSMhabitat::fr_juv$action_5["Upper-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper-mid Sacramento River", 
                                "fr", "juv", "action_5")
updated_habitat <- DSMhabitat::fr_juv$action_5["Upper-mid Sacramento River", , ] + add_project_habitat

action_5_baseline_fr_juv["Upper-mid Sacramento River", , ] <- updated_habitat 

# Add upper mid sacramento river fry habitat 
add_project_habitat <- DSMhabitat::fr_fry$action_5["Upper-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper-mid Sacramento River", 
                                "fr", "fry", "action_5")
updated_habitat <- DSMhabitat::fr_fry$action_5["Upper-mid Sacramento River", , ] + add_project_habitat

action_5_baseline_fr_fry["Upper-mid Sacramento River", , ] <- updated_habitat 

# check r to r vs dsm habitat 
# upper-mid sac, upper sac, and american river should be different 
action_5_baseline_fr_juv == DSMhabitat::fr_juv$action_5
action_5_baseline_fr_fry == DSMhabitat::fr_fry$action_5

# Save as data object to DSMhabitat
fr_juv <- modifyList(DSMhabitat::fr_juv, list("action_5_baseline" = action_5_baseline_fr_juv))
usethis::use_data(fr_juv, overwrite = TRUE)

# Save as data object to DSMhabitat
fr_fry <- modifyList(DSMhabitat::fr_fry, list("action_5_baseline" = action_5_baseline_fr_fry))
usethis::use_data(fr_fry, overwrite = TRUE)

# Exploratory plot 
r_to_r_baseline_action_5 <- action_5_baseline_fr_juv |> 
  DSMhabitat::square_meters_to_acres()

baseline <- DSMhabitat::fr_juv$r_to_r_baseline |> DSMhabitat::square_meters_to_acres()

ic_juv <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    r_to_r_baseline = as.vector(baseline),
    r_to_r_baseline_action_5 = as.vector(r_to_r_baseline_action_5)) |> 
  filter(watershed %in% c("American River", 
                          "Tuolumne River",
                          "Upper Sacramento River",
                          "Upper-mid Sacramento River"
  ))

ic_juv |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            r_to_r_baseline, r_to_r_baseline_action_5) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75) + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()

# Exploratory plot 
r_to_r_baseline_action_5 <- action_5_baseline_fr_fry |> 
  DSMhabitat::square_meters_to_acres()

baseline <- DSMhabitat::fr_fry$r_to_r_baseline |> DSMhabitat::square_meters_to_acres()

ic_fry <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    r_to_r_baseline = as.vector(baseline),
    r_to_r_baseline_action_5 = as.vector(r_to_r_baseline_action_5)) |> 
  filter(watershed %in% c("American River", 
                          "Tuolumne River",
                          "Upper Sacramento River",
                          "Upper-mid Sacramento River"))

ic_fry|> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            r_to_r_baseline, r_to_r_baseline_action_5) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75) + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()



# Add floodplain habitat -------------------------------------------------------
action_5_baseline_fr_fp <- DSMhabitat::fr_fp$action_5

# Add Lower-mid sacramento river floodplain habitat 
add_project_habitat <- DSMhabitat::fr_fp$action_5["Lower-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Lower-mid Sacramento River" , 
                                "fr", "juv", "action_5")
updated_habitat <- DSMhabitat::fr_fp$action_5["Lower-mid Sacramento River" , , ] + add_project_habitat

action_5_baseline_fr_fp["Lower-mid Sacramento River" , , ] <- updated_habitat 

# TUolumne river floodplain 
add_project_habitat <- DSMhabitat::fr_fp$action_5["Tuolumne River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Tuolumne River" , 
                                "fr", "juv", "action_5")
updated_habitat <- DSMhabitat::fr_fp$action_5["Tuolumne River" , , ] + add_project_habitat

action_5_baseline_fr_fp["Tuolumne River" , , ] <- updated_habitat 

# Add Yuba floodlplain habitat 
add_project_habitat <- DSMhabitat::fr_fp$action_5["Yuba River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Yuba River" , 
                                "fr", "juv", "action_5")
updated_habitat <- DSMhabitat::fr_fp$action_5["Yuba River" , , ] + add_project_habitat

action_5_baseline_fr_fp["Yuba River" , , ] <- updated_habitat 


# check hab differences 
# yuba and lower-mid sac should be different 
action_5_baseline_fr_fp == DSMhabitat::fr_fp$action_5

# Save as data object to DSMhabitat
fr_fp <- modifyList(DSMhabitat::fr_fp, list("action_5_baseline" = action_5_baseline_fr_fp))
usethis::use_data(fr_fp, overwrite = TRUE)

# Exploratory plot 
r_to_r_baseline_action_5 <- action_5_baseline_fr_fp |> 
  DSMhabitat::square_meters_to_acres()

baseline <- DSMhabitat::fr_fp$r_to_r_baseline |> DSMhabitat::square_meters_to_acres()

fp <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    r_to_r_baseline = as.vector(baseline),
    r_to_r_baseline_action_5 = as.vector(r_to_r_baseline_action_5)) |> 
  filter(watershed %in% c("Yuba River", "Lower-mid Sacramento River", "Tuolumne River"))

fp |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            r_to_r_baseline, r_to_r_baseline_action_5) |> 
  filter(!(watershed %in% c('Sutter Bypass', 'Yolo Bypass'))) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line() + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()

# SPRING RUN ---------------------------------------------------------------------
# Update DSMhabitat values 
# set r_to_r_baseline_fr_spawn to 
# TODO is this correct? should we be using eff here?
action_5_baseline_sr_spawn <- DSMhabitat::sr_spawn$biop_itp_2018_2019

# Add cottonwood creek spawning habitat 
add_project_habitat <- DSMhabitat::sr_spawn$biop_itp_2018_2019["Cottonwood Creek", , ] * 
  hab_prop_change_from_projects("spawning", "Cottonwood Creek", "sr", "adult", "action_5")
updated_habitat <- DSMhabitat::sr_spawn$biop_itp_2018_2019["Cottonwood Creek", , ] + add_project_habitat

action_5_baseline_sr_spawn["Cottonwood Creek", , ] <- updated_habitat 

# add deer creek spawning habitat
add_project_habitat <- DSMhabitat::sr_spawn$biop_itp_2018_2019["Deer Creek" , , ] * 
  hab_prop_change_from_projects("spawning", "Deer Creek", "sr", "adult", "action_5")
updated_habitat <- DSMhabitat::sr_spawn$biop_itp_2018_2019["Deer Creek", , ] + add_project_habitat

action_5_baseline_sr_spawn["Deer Creek", , ] <- updated_habitat 

# Add sacramento river spawning habitat 
add_project_habitat <- DSMhabitat::sr_spawn$biop_itp_2018_2019["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("spawning", "Upper Sacramento River", "sr", "adult", "action_5")
updated_habitat <- DSMhabitat::sr_spawn$biop_itp_2018_2019["Upper Sacramento River", , ] + add_project_habitat

action_5_baseline_sr_spawn["Upper Sacramento River", , ] <- updated_habitat 

# check habitat differences 
# upper sac and american river should be different (larger)
action_5_baseline_sr_spawn == DSMhabitat::sr_spawn$biop_itp_2018_2019

# Save as data object to DSMhabitat
sr_spawn <- modifyList(DSMhabitat::sr_spawn, list("action_5_baseline" = action_5_baseline_sr_spawn))
usethis::use_data(sr_spawn, overwrite = TRUE)

# Exploratory plot 
r_to_r_baseline_action_5 <- action_5_baseline_sr_spawn |> 
  DSMhabitat::square_meters_to_acres()

baseline <- DSMhabitat::sr_spawn$r_to_r_baseline |> DSMhabitat::square_meters_to_acres()

spawn <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1979:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    r_to_r_baseline = as.vector(baseline),
    r_to_r_baseline_action_5 = as.vector(r_to_r_baseline_action_5)) |> 
  filter(watershed %in% c("Cottonwood Creek", 
                          "Upper Sacramento River", 
                          "Deer Creek"))

spawn |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            r_to_r_baseline, r_to_r_baseline_action_5) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75) + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()




# Add inchannel habitat to both fry and juvenile habitat objects ---------------
# set r_to_r_baseline_fr_juv and fry 
action_5_baseline_sr_juv <- DSMhabitat::sr_juv$action_5
action_5_baseline_sr_fry <- DSMhabitat::sr_fry$action_5

# add tuolumne river juv habitat
add_project_habitat <- DSMhabitat::sr_juv$action_5["Tuolumne River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Tuolumne River", "sr", "juv", "action_5")
updated_habitat <- DSMhabitat::sr_juv$action_5["Tuolumne River", , ] + add_project_habitat

action_5_baseline_sr_juv["Tuolumne River", , ] <- updated_habitat 

# add tuolumne river fry habitat
add_project_habitat <- DSMhabitat::sr_fry$action_5["Tuolumne River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Tuolumne River", "sr", "fry", "action_5")
updated_habitat <- DSMhabitat::sr_fry$action_5["Tuolumne River", , ] + add_project_habitat

action_5_baseline_sr_fry["Tuolumne River", , ] <- updated_habitat 

# Upper sac river 
# Add upper sac river juv habitat 
add_project_habitat <- DSMhabitat::sr_juv$action_5["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "sr", "juv", "action_5")
updated_habitat <- DSMhabitat::sr_juv$action_5["Upper Sacramento River", , ] + add_project_habitat

action_5_baseline_sr_juv["Upper Sacramento River", , ] <- updated_habitat 

# Add upper sacramento  river fry habitat 
add_project_habitat <- DSMhabitat::sr_fry$action_5["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "sr", "fry", "action_5")

updated_habitat <- DSMhabitat::sr_fry$action_5["Upper Sacramento River", , ] + add_project_habitat

action_5_baseline_sr_fry["Upper Sacramento River", , ] <- updated_habitat 

# add Upper-mid Sacramento River
# Add upper mid sac river juv habitat 
add_project_habitat <- DSMhabitat::sr_juv$action_5["Upper-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper-mid Sacramento River", 
                                "sr", "juv", "action_5")
updated_habitat <- DSMhabitat::sr_juv$action_5["Upper-mid Sacramento River", , ] + add_project_habitat

action_5_baseline_sr_juv["Upper-mid Sacramento River", , ] <- updated_habitat 

# Add upper mid sacramento river fry habitat 
add_project_habitat <- DSMhabitat::sr_fry$action_5["Upper-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper-mid Sacramento River", 
                                "sr", "fry", "action_5")
updated_habitat <- DSMhabitat::sr_fry$action_5["Upper-mid Sacramento River", , ] + add_project_habitat

action_5_baseline_sr_fry["Upper-mid Sacramento River", , ] <- updated_habitat 

# check r to r vs dsm habitat 
# upper-mid sac, upper sac, and american river should be different 
action_5_baseline_sr_juv == DSMhabitat::sr_juv$action_5
action_5_baseline_sr_fry == DSMhabitat::sr_fry$action_5

# Save as data object to DSMhabitat
sr_juv <- modifyList(DSMhabitat::sr_juv, list("action_5_baseline" = action_5_baseline_sr_juv))
usethis::use_data(sr_juv, overwrite = TRUE)

# Save as data object to DSMhabitat
sr_fry <- modifyList(DSMhabitat::sr_fry, list("action_5_baseline" = action_5_baseline_sr_fry))
usethis::use_data(sr_fry, overwrite = TRUE)

# Exploratory plot 
r_to_r_baseline_action_5 <- action_5_baseline_sr_juv |> 
  DSMhabitat::square_meters_to_acres()

r_to_r_baseline <- DSMhabitat::sr_juv$r_to_r_baseline |> DSMhabitat::square_meters_to_acres()

ic_juv <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    r_to_r_baseline = as.vector(r_to_r_baseline),
    r_to_r_baseline_action_5 = as.vector(r_to_r_baseline_action_5)) |> 
  filter(watershed %in% c("Tuolumne River",
                          "Upper Sacramento River",
                          "Upper-mid Sacramento River"
  ))

ic_juv |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            r_to_r_baseline, r_to_r_baseline_action_5) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75) + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()

# # Exploratory plot 
# r_to_r_baseline_habitat <- action_5_baseline_sr_fry |> 
#   DSMhabitat::square_meters_to_acres()
# 
# sit_habitat <- DSMhabitat::sr_fry$action_5 |> DSMhabitat::square_meters_to_acres()
# 
# ic_fry <- expand_grid(
#   watershed = factor(DSMscenario::watershed_labels, 
#                      levels = DSMscenario::watershed_labels),
#   month = 1:12,
#   year = 1980:2000) |> 
#   arrange(year, month, watershed) |> 
#   mutate(
#     sit_habitat = as.vector(sit_habitat),
#     r_to_r_baseline_habitat = as.vector(r_to_r_baseline_habitat)) |> 
#   filter(watershed %in% c("Tuolumne River",
#                           "Upper Sacramento River",
#                           "Upper-mid Sacramento River"))
# 
# ic_fry|> 
#   transmute(watershed, date = ymd(paste(year, month, 1)), 
#             sit_habitat, r_to_r_baseline_habitat) |> 
#   gather(version, acres, -watershed, -date)  |> 
#   ggplot(aes(date, acres, color = version)) +
#   geom_line(alpha = .75) + 
#   # geom_col(position = 'dodge') +
#   facet_wrap(~watershed, scales = 'free_y') + 
#   theme_minimal()



# Add floodplain habitat -------------------------------------------------------
action_5_baseline_sr_fp <- DSMhabitat::sr_fp$action_5

# Add Lower-mid sacramento river floodplain habitat 
add_project_habitat <- DSMhabitat::sr_fp$action_5["Lower-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Lower-mid Sacramento River" , 
                                "sr", "juv", "action_5")
updated_habitat <- DSMhabitat::sr_fp$action_5["Lower-mid Sacramento River" , , ] + add_project_habitat

action_5_baseline_sr_fp["Lower-mid Sacramento River" , , ] <- updated_habitat 

# Tuolumne river floodplain 
add_project_habitat <- DSMhabitat::sr_fp$action_5["Tuolumne River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Tuolumne River" , 
                                "sr", "juv", "action_5")
updated_habitat <- DSMhabitat::sr_fp$action_5["Tuolumne River" , , ] + add_project_habitat

action_5_baseline_sr_fp["Tuolumne River" , , ] <- updated_habitat 

# Add Yuba floodlplain habitat 
add_project_habitat <- DSMhabitat::sr_fp$action_5["Yuba River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Yuba River" , 
                                "sr", "juv", "action_5")
updated_habitat <- DSMhabitat::sr_fp$action_5["Yuba River" , , ] + add_project_habitat

action_5_baseline_sr_fp["Yuba River" , , ] <- updated_habitat 


# check hab differences 
# yuba and lower-mid sac should be different 
action_5_baseline_sr_fp == DSMhabitat::sr_fp$action_5

# Save as data object to DSMhabitat
sr_fp <- modifyList(DSMhabitat::sr_fp, list("action_5_baseline" = action_5_baseline_sr_fp))
usethis::use_data(sr_fp, overwrite = TRUE)

# Exploratory plot 
# r_to_r_baseline_habitat <- action_5_baseline_sr_fp |> 
#   DSMhabitat::square_meters_to_acres()
# 
# sit_habitat <- DSMhabitat::sr_fp$action_5 |> DSMhabitat::square_meters_to_acres()
# 
# fp <- expand_grid(
#   watershed = factor(DSMscenario::watershed_labels, 
#                      levels = DSMscenario::watershed_labels),
#   month = 1:12,
#   year = 1980:2000) |> 
#   arrange(year, month, watershed) |> 
#   mutate(
#     sit_habitat = as.vector(sit_habitat),
#     r_to_r_baseline_habitat = as.vector(r_to_r_baseline_habitat)) |> 
#   filter(watershed %in% c("Yuba River", "Lower-mid Sacramento River", "Tuolumne River"))
# 
# fp |> 
#   transmute(watershed, date = ymd(paste(year, month, 1)), 
#             sit_habitat, r_to_r_baseline_habitat) |> 
#   filter(!(watershed %in% c('Sutter Bypass', 'Yolo Bypass'))) |> 
#   gather(version, acres, -watershed, -date)  |> 
#   ggplot(aes(date, acres, color = version)) +
#   geom_line() + 
#   # geom_col(position = 'dodge') +
#   facet_wrap(~watershed, scales = 'free_y') + 
#   theme_minimal()

# WINTER RUN ---------------------------------------------------------------------
# Update DSMhabitat values 
# set r_to_r_baseline_fr_spawn to 
action_5_baseline_wr_spawn <- DSMhabitat::wr_spawn$action_5

# Add sacramento river spawning habitat 
add_project_habitat <- DSMhabitat::wr_spawn$action_5["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("spawning", "Upper Sacramento River", "wr", "adult", "action_5")
updated_habitat <- DSMhabitat::wr_spawn$action_5["Upper Sacramento River", , ] + add_project_habitat

action_5_baseline_wr_spawn["Upper Sacramento River", , ] <- updated_habitat 

# check habitat differences 
# upper sac and american river should be different (larger)
action_5_baseline_wr_spawn == DSMhabitat::wr_spawn$action_5

# Save as data object to DSMhabitat
wr_spawn <- modifyList(DSMhabitat::wr_spawn, list("action_5_baseline" = action_5_baseline_wr_spawn))
usethis::use_data(wr_spawn, overwrite = TRUE)

# Exploratory plot 
# r_to_r_baseline_habitat <- action_5_baseline_wr_spawn |> 
#   DSMhabitat::square_meters_to_acres()
# 
# sit_habitat <- DSMhabitat::wr_spawn$action_5 |> DSMhabitat::square_meters_to_acres()
# 
# spawn <- expand_grid(
#   watershed = factor(DSMscenario::watershed_labels, 
#                      levels = DSMscenario::watershed_labels),
#   month = 1:12,
#   year = 1979:2000) |> 
#   arrange(year, month, watershed) |> 
#   mutate(
#     sit_habitat = as.vector(sit_habitat),
#     r_to_r_baseline_habitat = as.vector(r_to_r_baseline_habitat)) |> 
#   filter(watershed %in% c("Upper Sacramento River"))
# 
# spawn |> 
#   transmute(watershed, date = ymd(paste(year, month, 1)), 
#             sit_habitat, r_to_r_baseline_habitat) |> 
#   gather(version, acres, -watershed, -date)  |> 
#   ggplot(aes(date, acres, color = version)) +
#   geom_line(alpha = .75) + 
#   # geom_col(position = 'dodge') +
#   facet_wrap(~watershed, scales = 'free_y') + 
#   theme_minimal()

# Add inchannel habitat to both fry and juvenile habitat objects ---------------
# set r_to_r_baseline_fr_juv and fry 
action_5_baseline_wr_juv <- DSMhabitat::wr_juv$action_5
action_5_baseline_wr_fry <- DSMhabitat::wr_fry$action_5

# Upper sac river 
# Add upper sac river juv habitat 
add_project_habitat <- DSMhabitat::wr_juv$action_5["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "wr", "juv", "action_5")
updated_habitat <- DSMhabitat::wr_juv$action_5["Upper Sacramento River", , ] + add_project_habitat

action_5_baseline_wr_juv["Upper Sacramento River", , ] <- updated_habitat 

# Add upper sacramento  river fry habitat 
add_project_habitat <- DSMhabitat::wr_fry$action_5["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "wr", "fry", "action_5")

updated_habitat <- DSMhabitat::wr_fry$action_5["Upper Sacramento River", , ] + add_project_habitat

action_5_baseline_wr_fry["Upper Sacramento River", , ] <- updated_habitat 

# add Upper-mid Sacramento River
# Add upper mid sac river juv habitat 
add_project_habitat <- DSMhabitat::wr_juv$action_5["Upper-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper-mid Sacramento River", 
                                "wr", "juv", "action_5")
updated_habitat <- DSMhabitat::wr_juv$action_5["Upper-mid Sacramento River", , ] + add_project_habitat

action_5_baseline_wr_juv["Upper-mid Sacramento River", , ] <- updated_habitat 

# Add upper mid sacramento river fry habitat 
add_project_habitat <- DSMhabitat::wr_fry$action_5["Upper-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper-mid Sacramento River", 
                                "wr", "fry", "action_5")
updated_habitat <- DSMhabitat::wr_fry$action_5["Upper-mid Sacramento River", , ] + add_project_habitat

action_5_baseline_wr_fry["Upper-mid Sacramento River", , ] <- updated_habitat 

# check r to r vs dsm habitat 
# upper-mid sac, upper sac, and american river should be different 
action_5_baseline_wr_juv == DSMhabitat::wr_juv$action_5
action_5_baseline_wr_fry == DSMhabitat::wr_fry$action_5

# Save as data object to DSMhabitat
wr_juv <- modifyList(DSMhabitat::wr_juv, list("action_5_baseline" = action_5_baseline_wr_juv))
usethis::use_data(wr_juv, overwrite = TRUE)

# Save as data object to DSMhabitat
modifyList(DSMhabitat::wr_fry, list("action_5_baseline" = action_5_baseline_wr_fry))
usethis::use_data(wr_fry, overwrite = TRUE)

# Exploratory plot 
# # r_to_r_baseline_habitat <- action_5_baseline_wr_juv |> 
# #   DSMhabitat::square_meters_to_acres()
# # 
# # sit_habitat <- DSMhabitat::wr_juv$action_5 |> DSMhabitat::square_meters_to_acres()
# # 
# # ic_juv <- expand_grid(
# #   watershed = factor(DSMscenario::watershed_labels, 
# #                      levels = DSMscenario::watershed_labels),
# #   month = 1:12,
# #   year = 1980:2000) |> 
# #   arrange(year, month, watershed) |> 
# #   mutate(
# #     sit_habitat = as.vector(sit_habitat),
# #     r_to_r_baseline_habitat = as.vector(r_to_r_baseline_habitat)) |> 
# #   filter(watershed %in% c( "Upper Sacramento River",
# #                            "Upper-mid Sacramento River"
# #   ))
# # 
# # ic_juv |> 
# #   transmute(watershed, date = ymd(paste(year, month, 1)), 
# #             sit_habitat, r_to_r_baseline_habitat) |> 
# #   gather(version, acres, -watershed, -date)  |> 
# #   ggplot(aes(date, acres, color = version)) +
# #   geom_line(alpha = .75) + 
# #   # geom_col(position = 'dodge') +
# #   facet_wrap(~watershed, scales = 'free_y') + 
# #   theme_minimal()
# 
# # Exploratory plot 
# r_to_r_baseline_habitat <- action_5_baseline_wr_fry |> 
#   DSMhabitat::square_meters_to_acres()
# 
# sit_habitat <- DSMhabitat::wr_fry$action_5 |> DSMhabitat::square_meters_to_acres()
# 
# ic_fry <- expand_grid(
#   watershed = factor(DSMscenario::watershed_labels, 
#                      levels = DSMscenario::watershed_labels),
#   month = 1:12,
#   year = 1980:2000) |> 
#   arrange(year, month, watershed) |> 
#   mutate(
#     sit_habitat = as.vector(sit_habitat),
#     r_to_r_baseline_habitat = as.vector(r_to_r_baseline_habitat)) |> 
#   filter(watershed %in% c("Upper Sacramento River",
#                           "Upper-mid Sacramento River"))
# 
# ic_fry|> 
#   transmute(watershed, date = ymd(paste(year, month, 1)), 
#             sit_habitat, r_to_r_baseline_habitat) |> 
#   gather(version, acres, -watershed, -date)  |> 
#   ggplot(aes(date, acres, color = version)) +
#   geom_line(alpha = .75) + 
#   # geom_col(position = 'dodge') +
#   facet_wrap(~watershed, scales = 'free_y') + 
#   theme_minimal()



# Add floodplain habitat -------------------------------------------------------
action_5_baseline_wr_fp <- DSMhabitat::wr_fp$action_5

# Add Lower-mid sacramento river floodplain habitat 
add_project_habitat <- DSMhabitat::wr_fp$action_5["Lower-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Lower-mid Sacramento River" , 
                                "wr", "juv", "action_5")
updated_habitat <- DSMhabitat::wr_fp$action_5["Lower-mid Sacramento River" , , ] + add_project_habitat

action_5_baseline_wr_fp["Lower-mid Sacramento River" , , ] <- updated_habitat 

# check hab differences 
# yuba and lower-mid sac should be different 
action_5_baseline_wr_fp == DSMhabitat::wr_fp$action_5

# Save as data object to DSMhabitat
wr_fp <- modifyList(DSMhabitat::wr_fp, list("action_5_baseline" = action_5_baseline_wr_fp))
usethis::use_data(wr_fp, overwrite = TRUE)

# Exploratory plot 
# r_to_r_baseline_habitat <- action_5_baseline_wr_fp |> 
#   DSMhabitat::square_meters_to_acres()
# 
# sit_habitat <- DSMhabitat::wr_fp$action_5 |> DSMhabitat::square_meters_to_acres()
# 
# fp <- expand_grid(
#   watershed = factor(DSMscenario::watershed_labels, 
#                      levels = DSMscenario::watershed_labels),
#   month = 1:12,
#   year = 1980:2000) |> 
#   arrange(year, month, watershed) |> 
#   mutate(
#     sit_habitat = as.vector(sit_habitat),
#     r_to_r_baseline_habitat = as.vector(r_to_r_baseline_habitat)) |> 
#   filter(watershed %in% c("Lower-mid Sacramento River"))
# 
# fp |> 
#   transmute(watershed, date = ymd(paste(year, month, 1)), 
#             sit_habitat, r_to_r_baseline_habitat) |> 
#   filter(!(watershed %in% c('Sutter Bypass', 'Yolo Bypass'))) |> 
#   gather(version, acres, -watershed, -date)  |> 
#   ggplot(aes(date, acres, color = version)) +
#   geom_line() + 
#   # geom_col(position = 'dodge') +
#   facet_wrap(~watershed, scales = 'free_y') + 
#   theme_minimal()

# all runs - delta 
# 
# Add north delta habitat ------------------------------------------------------
r_to_r_baseline_delta_action_5 <- DSMhabitat::delta_habitat$sit_habitat
add_project_habitat <- DSMhabitat::delta_habitat$sit_habitat[ , ,"North Delta" ] * 
  hab_prop_change_from_projects("floodplain rearing", "North Delta" , 
                                "fr", "juv", "action_5")
updated_habitat <- DSMhabitat::delta_habitat$sit_habitat[ , ,"North Delta" ] + add_project_habitat

r_to_r_baseline_delta_action_5[ , , "North Delta"] <- updated_habitat 

# check hab differences 
# north delta should be different 
r_to_r_baseline_delta_action_5 == DSMhabitat::delta_habitat$sit_habitat
r_to_r_baseline_delta_action_5 == DSMhabitat::delta_habitat$r_to_r_baseline # the same values
# TODO: do we need to have a different object if the values are the same? 
# TODO: double check that the delta flows are the same for eff and action_5

# Exploratory plot 
baseline_habitat_action_5 <- r_to_r_baseline_delta_action_5[,, "North Delta"] |> 
  DSMhabitat::square_meters_to_acres()

baseline <- DSMhabitat::delta_habitat$r_to_r_baseline[,, "North Delta"] |> DSMhabitat::square_meters_to_acres()

delta <- expand_grid(
  watershed = "North Delta",
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    baseline = as.vector(baseline),
    baseline_habitat_action_5 = as.vector(baseline_habitat_action_5)) |> 
  filter(watershed %in% c("North Delta"))

delta |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            baseline, baseline_habitat_action_5) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line() + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()

# Save as data object to DSMhabitat
# delta_habitat = modifyList(DSMhabitat::delta_habitat, list(sit_habitat = DSMhabitat::delta_habitat$sit_habitat,
#                                                            r_to_r_baseline = r_to_r_baseline_delta))
# usethis::use_data(delta_habitat, overwrite = TRUE)
