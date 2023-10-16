#' Make a note of when the report was generated.
Sys.time()
knitr.table.format = "markdown"

#' As always, remove all objects from the work space before starting.
rm(list = ls(all = TRUE))

setwd("C:/Users/haowe/Desktop/COVID-forecasting")
# setwd("C:/Users/hw3616/Desktop/COVID-forecasting")

#' Pull in packages needed
source("./functions/load_packages.R")
source("./functions/data_process.R")

pkgs <- c("stringr", "dplyr", "ggplot2", "aweek",
          "lubridate", "ISOweek","surveillance", "sf",
          "tidycovid19", "plyr", "weathermetrics")
load_package(pkgs)

# remotes::install_github("joachim-gassen/tidycovid19")
nhs_ltla <- read.csv("./saved_objects/nhs_ltla_table.csv") %>%
  dplyr::rename(lad19cd = lacode)

df_weather_raw <- read.csv("./data/df_weather_raw.csv")
google_mobility_lad_lookup <- read.csv("./data/google_mobility_lad_lookup_200903.csv")

df_epi <- readRDS("./saved_objects/df_adj.rds")

df_subregion_uk <- read.csv("./saved_objects/df_subregion_uk.csv")

######## epi #########
df_epi <- df_epi %>%
  mutate(adj_newCasesBySpecimenDate = newCasesBySpecimenDate/population*100000,
         adj_newWeeklyNsoDeathsByRegDate = newWeeklyNsoDeathsByRegDate/population*100000,
         adj_newAdmissions = newAdmissions/population*100000) %>% 
  filter(!is.na(adj_newAdmissions))

####### mobility ##########
df_ltla_uk <- full_join(df_subregion_uk, google_mobility_lad_lookup, by = c("sub_region_1", "sub_region_2")) %>%
  filter(substr(lad19cd, 1, 1) == "E") %>%
  filter(!is.na(lad19cd)) %>%
  filter(!is.na(iso3c))

#' map nhs regions according to ltla code
df_ltla_uk <- df_ltla_uk %>%
  dplyr::select(-iso3c,-sub_region_1,-sub_region_2, 
                -place_id, -timestamp, -country_region_code,
                -la_name, -flag_2018) %>%
  mutate(Year = isoWeekYear(ymd(date))[[1]],
         Week = isoWeekYear(ymd(date))[[2]]) %>% 
  dplyr::rename(Date = date) %>% 
  relocate(Year, .after = 1) %>% 
  relocate(Week, .after = 2)

df_ltla_uk <- df_ltla_uk %>%
  group_by(Year, Week, lad19cd) %>% 
  dplyr::summarise(retail_recreation = mean(retail_recreation, na.rm = TRUE),
                   grocery_pharmacy = mean(grocery_pharmacy, na.rm = TRUE),
                   parks = mean(parks, na.rm = TRUE),
                   transit_stations = mean(transit_stations, na.rm = TRUE),
                   workplaces = mean(workplaces, na.rm = TRUE),
                   residential = mean(residential, na.rm = TRUE)) %>%
  mutate(Week = ifelse(Week < 10, paste0("0", Week), as.character(Week))) %>%
  mutate(Date = ISOweek::ISOweek2date(paste0(Year,"-W", Week, "-4"))) %>% 
  relocate(Date, .before = 1)

df_ltla_uk <- inner_join(df_ltla_uk, nhs_ltla, by = "lad19cd") %>%
  group_by(Year, Week, nhs_region) %>% 
  dplyr::summarise(retail_recreation = mean(retail_recreation, na.rm = TRUE),
                   grocery_pharmacy = mean(grocery_pharmacy, na.rm = TRUE),
                   parks = mean(parks, na.rm = TRUE),
                   transit_stations = mean(transit_stations, na.rm = TRUE),
                   workplaces = mean(workplaces, na.rm = TRUE),
                   residential = mean(residential, na.rm = TRUE)) %>%
  # mutate(Week = ifelse(Week < 10, paste0("0", Week), as.character(Week))) %>%
  mutate(Date = ISOweek::ISOweek2date(paste0(Year,"-W", Week, "-4"))) %>% 
  relocate(Date, .before = 1)

df_epi_mobi <- full_join(df_epi, df_ltla_uk, by = c("Date","Year", "Week", "nhs_region")) %>% 
  dplyr::filter(!is.na(adj_newAdmissions)) %>% 
  mutate(retail_recreation = ifelse(retail_recreation == "NaN", NA, retail_recreation),
         grocery_pharmacy = ifelse(grocery_pharmacy == "NaN", NA, grocery_pharmacy),
         parks = ifelse(parks == "NaN", NA, parks),
         transit_stations = ifelse(transit_stations == "NaN", NA, transit_stations),
         workplaces = ifelse(workplaces == "NaN", NA, workplaces),
         residential = ifelse(residential == "NaN", NA, residential))

########### weather ##########
df_weather_raw <- df_weather_raw %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(Year = isoWeekYear(ymd(Date))[[1]],
         Week = isoWeekYear(ymd(Date))[[2]]) %>% 
  relocate(Date, .before = 1) %>% 
  relocate(Year, .after = 1) %>% 
  relocate(Week, .after = 2)

df_weather_raw <- df_weather_raw %>% 
  group_by(Year, Week, lad19cd) %>% 
  dplyr::summarise(total.precipitation = mean(total.precipitation, na.rm = TRUE),
                   temperature = mean(temperature, na.rm = TRUE)) %>%
  mutate(Week = ifelse(Week < 10, paste0("0", Week), as.character(Week))) %>%
  mutate(Date = ISOweek::ISOweek2date(paste0(Year,"-W", Week, "-4"))) %>% 
  relocate(Date, .before = 1)

#' Mathch LTLA with NHS regions
df_weather_nhs <- inner_join(df_weather_raw, nhs_ltla, by = "lad19cd") %>%
  group_by(Year, Week, nhs_region) %>% 
  dplyr::summarise(total.precipitation = mean(total.precipitation, na.rm = TRUE),
                   temperature = mean(temperature, na.rm = TRUE)) %>%
  mutate(total.precipitation = total.precipitation*1000,
         temperature = kelvin.to.celsius(temperature, round = 2)) %>%
  # mutate(Week = ifelse(Week < 10, paste0("0", Week), as.character(Week))) %>%
  mutate(Date = ISOweek::ISOweek2date(paste0(Year,"-W", Week, "-4"))) %>% 
  relocate(Date, .before = 1)

df_epi_mobiwea <- full_join(df_epi_mobi, df_weather_nhs, by = c("Date","Year", "Week", "nhs_region")) %>% 
  dplyr::filter(!is.na(adj_newAdmissions)) 

saveRDS(df_epi_mobiwea, "./saved_objects/df_raw_epi_mobiwea.rds")
