#' Make a note of when the report was generated.
Sys.time()
knitr.table.format = "markdown"

#' As always, remove all objects from the work space before starting.
rm(list = ls(all = TRUE))

#' Set your own work path
# setwd()

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

#' Load df_model_mobibins data
df_model_mobi3bins <- readRDS("./saved_objects/df_model_mobi3bins.rds")
df_model_mobi5bins <- readRDS("./saved_objects/df_model_mobi5bins.rds")
df_model_mobi10bins <- readRDS("./saved_objects/df_model_mobi10bins.rds")

#' Add LTLA code to the weather data and save it,
#' so we can read it directly afterwards.
df_weather_raw <- read.csv("./data/df_weather_raw.csv")
# df_ltla_lookup <- read.csv("./data/Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_(December_2019)_Lookup_in_England_and_Wales.csv")

# df_ltla_lookup <- df_ltla_lookup %>%
#   dplyr::select(LTLA19CD, LTLA19NM) %>%
#   dplyr::rename(lad19cd = LTLA19CD,
#                 lad19nm = LTLA19NM)
# 
# df_weather_raw <- left_join(df_weather_raw, df_ltla_lookup, by = "lad19nm") %>%
#   filter(substr(lad19cd, 1, 1) == "E") %>%
#   filter(lad19cd %in% nhs_ltla$lad19cd) %>%
#   relocate(lad19cd, .before = 1) 
# 
# df_weather_raw <- df_weather_raw %>%
#   dplyr::rename(temperature = X2m.temperature,
#                 Date = time) %>%
#   dplyr::select(-bng_e, -bng_n, -long, -lat)
# 
# write.csv(df_weather_raw, "./data/df_weather_raw.csv", row.names = FALSE)

#' Reformat date in df_weather_raw

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

#' merge with df_model_mobibins
df_model_mobiwea3bins <- full_join(df_model_mobi3bins, df_weather_nhs, by = c("Date","Year", "Week", "nhs_region")) %>% 
  filter(!is.na(hosp_quan)) 

df_model_mobiwea5bins <- full_join(df_model_mobi5bins, df_weather_nhs, by = c("Date","Year", "Week", "nhs_region")) %>% 
  filter(!is.na(hosp_quan)) 

df_model_mobiwea10bins <- full_join(df_model_mobi10bins, df_weather_nhs, by = c("Date","Year", "Week", "nhs_region")) %>% 
  filter(!is.na(hosp_quan)) 

#' create lags for mobility variables
lg <- function(x)c(NA,x[1:length(x)-1])

df_model_mobiwea3bins <- ddply(df_model_mobiwea3bins, ~nhs_region, transform, total.precipitationlag1 = lg(total.precipitation))
df_model_mobiwea3bins <- ddply(df_model_mobiwea3bins, ~nhs_region, transform, total.precipitationlag2 = lg(total.precipitationlag1))
df_model_mobiwea3bins <- ddply(df_model_mobiwea3bins, ~nhs_region, transform, total.precipitationlag3 = lg(total.precipitationlag2))
df_model_mobiwea3bins <- ddply(df_model_mobiwea3bins, ~nhs_region, transform, total.precipitationlag4 = lg(total.precipitationlag3))

df_model_mobiwea3bins <- ddply(df_model_mobiwea3bins, ~nhs_region, transform, temperaturelag1 = lg(temperature))
df_model_mobiwea3bins <- ddply(df_model_mobiwea3bins, ~nhs_region, transform, temperaturelag2 = lg(temperaturelag1))
df_model_mobiwea3bins <- ddply(df_model_mobiwea3bins, ~nhs_region, transform, temperaturelag3 = lg(temperaturelag2))
df_model_mobiwea3bins <- ddply(df_model_mobiwea3bins, ~nhs_region, transform, temperaturelag4 = lg(temperaturelag3))

df_model_mobiwea3bins <- df_model_mobiwea3bins %>% 
  dplyr::select(-total.precipitation, -temperature)

df_model_mobiwea5bins <- ddply(df_model_mobiwea5bins, ~nhs_region, transform, total.precipitationlag1 = lg(total.precipitation))
df_model_mobiwea5bins <- ddply(df_model_mobiwea5bins, ~nhs_region, transform, total.precipitationlag2 = lg(total.precipitationlag1))
df_model_mobiwea5bins <- ddply(df_model_mobiwea5bins, ~nhs_region, transform, total.precipitationlag3 = lg(total.precipitationlag2))
df_model_mobiwea5bins <- ddply(df_model_mobiwea5bins, ~nhs_region, transform, total.precipitationlag4 = lg(total.precipitationlag3))

df_model_mobiwea5bins <- ddply(df_model_mobiwea5bins, ~nhs_region, transform, temperaturelag1 = lg(temperature))
df_model_mobiwea5bins <- ddply(df_model_mobiwea5bins, ~nhs_region, transform, temperaturelag2 = lg(temperaturelag1))
df_model_mobiwea5bins <- ddply(df_model_mobiwea5bins, ~nhs_region, transform, temperaturelag3 = lg(temperaturelag2))
df_model_mobiwea5bins <- ddply(df_model_mobiwea5bins, ~nhs_region, transform, temperaturelag4 = lg(temperaturelag3))

df_model_mobiwea5bins <- df_model_mobiwea5bins %>% 
  dplyr::select(-total.precipitation, -temperature)

df_model_mobiwea10bins <- ddply(df_model_mobiwea10bins, ~nhs_region, transform, total.precipitationlag1 = lg(total.precipitation))
df_model_mobiwea10bins <- ddply(df_model_mobiwea10bins, ~nhs_region, transform, total.precipitationlag2 = lg(total.precipitationlag1))
df_model_mobiwea10bins <- ddply(df_model_mobiwea10bins, ~nhs_region, transform, total.precipitationlag3 = lg(total.precipitationlag2))
df_model_mobiwea10bins <- ddply(df_model_mobiwea10bins, ~nhs_region, transform, total.precipitationlag4 = lg(total.precipitationlag3))

df_model_mobiwea10bins <- ddply(df_model_mobiwea10bins, ~nhs_region, transform, temperaturelag1 = lg(temperature))
df_model_mobiwea10bins <- ddply(df_model_mobiwea10bins, ~nhs_region, transform, temperaturelag2 = lg(temperaturelag1))
df_model_mobiwea10bins <- ddply(df_model_mobiwea10bins, ~nhs_region, transform, temperaturelag3 = lg(temperaturelag2))
df_model_mobiwea10bins <- ddply(df_model_mobiwea10bins, ~nhs_region, transform, temperaturelag4 = lg(temperaturelag3))

df_model_mobiwea10bins <- df_model_mobiwea10bins %>% 
  dplyr::select(-total.precipitation, -temperature)

saveRDS(df_model_mobiwea3bins, "./saved_objects/df_model_mobiwea3bins.rds")
saveRDS(df_model_mobiwea5bins, "./saved_objects/df_model_mobiwea5bins.rds")
saveRDS(df_model_mobiwea10bins, "./saved_objects/df_model_mobiwea10bins.rds")
