#' Make a note of when the report was generated.
Sys.time()
knitr.table.format = "markdown"

#' As always, remove all objects from the work space before starting.
rm(list = ls(all = TRUE))

setwd("C:/Users/haowe/Desktop/COVID-forecasting")
setwd("C:/Users/hw3616/Desktop/COVID-forecasting")

#' Pull in packages needed
source("./functions/load_packages.R")
source("./functions/data_process.R")

pkgs <- c("stringr", "dplyr", "ggplot2", "aweek",
          "lubridate", "ISOweek","surveillance", "sf",
          "tidycovid19", "plyr")
load_package(pkgs)

# remotes::install_github("joachim-gassen/tidycovid19")
nhs_ltla <- read.csv("./saved_objects/nhs_ltla_table.csv") %>%
  dplyr::rename(lad19cd = lacode)
google_mobility_lad_lookup <- read.csv("./data/google_mobility_lad_lookup_200903.csv")

#' Load df_model_bins data
df_model_epi3bins <- readRDS("./saved_objects/df_model_epi3bins.rds")
df_model_epi5bins <- readRDS("./saved_objects/df_model_epi5bins.rds")
df_model_epi10bins <- readRDS("./saved_objects/df_model_epi10bins.rds")

# df_subregion <- download_google_cmr_data(type = "country_sub_region", silent = FALSE, cached = FALSE)
# df_subregion_uk <- df_region[which(df_region$iso3c=="GBR"),]
# 
# write.csv(df_subregion_uk, "./saved_objects/df_subregion_uk.csv", row.names = FALSE)

df_subregion_uk <- read.csv("./saved_objects/df_subregion_uk.csv")

#'  join on both the sub_region_1 and sub_region_2 columns 
#'  and use the la_name or lad19cd column as the unique identifier,
#'  excluding the rows in the data which represent the coarser granularity 
#'  data and don't have a la_name.

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

#' merge with df_model_bins
df_model_3bins <- full_join(df_model_epi3bins, df_ltla_uk, by = c("Date","Year", "Week", "nhs_region")) %>% 
  filter(!is.na(hosp_quan)) %>% 
  mutate(retail_recreation = ifelse(retail_recreation == "NaN", NA, retail_recreation),
         grocery_pharmacy = ifelse(grocery_pharmacy == "NaN", NA, grocery_pharmacy),
         parks = ifelse(parks == "NaN", NA, parks),
         transit_stations = ifelse(transit_stations == "NaN", NA, transit_stations),
         workplaces = ifelse(workplaces == "NaN", NA, workplaces),
         residential = ifelse(residential == "NaN", NA, residential))

df_model_5bins <- full_join(df_model_epi5bins, df_ltla_uk, by = c("Date","Year", "Week", "nhs_region")) %>% 
  filter(!is.na(hosp_quan)) %>% 
  mutate(retail_recreation = ifelse(retail_recreation == "NaN", NA, retail_recreation),
         grocery_pharmacy = ifelse(grocery_pharmacy == "NaN", NA, grocery_pharmacy),
         parks = ifelse(parks == "NaN", NA, parks),
         transit_stations = ifelse(transit_stations == "NaN", NA, transit_stations),
         workplaces = ifelse(workplaces == "NaN", NA, workplaces),
         residential = ifelse(residential == "NaN", NA, residential))

df_model_10bins <- full_join(df_model_epi10bins, df_ltla_uk, by = c("Date","Year", "Week", "nhs_region")) %>% 
  filter(!is.na(hosp_quan)) %>% 
  mutate(retail_recreation = ifelse(retail_recreation == "NaN", NA, retail_recreation),
         grocery_pharmacy = ifelse(grocery_pharmacy == "NaN", NA, grocery_pharmacy),
         parks = ifelse(parks == "NaN", NA, parks),
         transit_stations = ifelse(transit_stations == "NaN", NA, transit_stations),
         workplaces = ifelse(workplaces == "NaN", NA, workplaces),
         residential = ifelse(residential == "NaN", NA, residential))

#' create lags for mobility variables
lg <- function(x)c(NA,x[1:length(x)-1])

df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, retail_recreationlag1 = lg(retail_recreation))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, retail_recreationlag2 = lg(retail_recreationlag1))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, retail_recreationlag3 = lg(retail_recreationlag2))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, retail_recreationlag4 = lg(retail_recreationlag3))

df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, grocery_pharmacylag1 = lg(grocery_pharmacy))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, grocery_pharmacylag2 = lg(grocery_pharmacylag1))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, grocery_pharmacylag3 = lg(grocery_pharmacylag2))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, grocery_pharmacylag4 = lg(grocery_pharmacylag3))

df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, parkslag1 = lg(parks))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, parkslag2 = lg(parkslag1))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, parkslag3 = lg(parkslag2))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, parkslag4 = lg(parkslag3))

df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, transit_stationslag1 = lg(transit_stations))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, transit_stationslag2 = lg(transit_stationslag1))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, transit_stationslag3 = lg(transit_stationslag2))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, transit_stationslag4 = lg(transit_stationslag3))

df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, workplaceslag1 = lg(workplaces))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, workplaceslag2 = lg(workplaceslag1))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, workplaceslag3 = lg(workplaceslag2))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, workplaceslag4 = lg(workplaceslag3))

df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, residentiallag1 = lg(residential))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, residentiallag2 = lg(residentiallag1))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, residentiallag3 = lg(residentiallag2))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, residentiallag4 = lg(residentiallag3))

df_model_3bins <- df_model_3bins %>% 
  dplyr::select(-retail_recreation, -grocery_pharmacy, -parks, 
                -transit_stations, -workplaces, -residential)

df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, retail_recreationlag1 = lg(retail_recreation))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, retail_recreationlag2 = lg(retail_recreationlag1))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, retail_recreationlag3 = lg(retail_recreationlag2))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, retail_recreationlag4 = lg(retail_recreationlag3))

df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, grocery_pharmacylag1 = lg(grocery_pharmacy))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, grocery_pharmacylag2 = lg(grocery_pharmacylag1))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, grocery_pharmacylag3 = lg(grocery_pharmacylag2))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, grocery_pharmacylag4 = lg(grocery_pharmacylag3))

df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, parkslag1 = lg(parks))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, parkslag2 = lg(parkslag1))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, parkslag3 = lg(parkslag2))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, parkslag4 = lg(parkslag3))

df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, transit_stationslag1 = lg(transit_stations))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, transit_stationslag2 = lg(transit_stationslag1))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, transit_stationslag3 = lg(transit_stationslag2))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, transit_stationslag4 = lg(transit_stationslag3))

df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, workplaceslag1 = lg(workplaces))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, workplaceslag2 = lg(workplaceslag1))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, workplaceslag3 = lg(workplaceslag2))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, workplaceslag4 = lg(workplaceslag3))

df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, residentiallag1 = lg(residential))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, residentiallag2 = lg(residentiallag1))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, residentiallag3 = lg(residentiallag2))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, residentiallag4 = lg(residentiallag3))

df_model_5bins <- df_model_5bins %>% 
  dplyr::select(-retail_recreation, -grocery_pharmacy, -parks, 
                -transit_stations, -workplaces, -residential)

df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, retail_recreationlag1 = lg(retail_recreation))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, retail_recreationlag2 = lg(retail_recreationlag1))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, retail_recreationlag3 = lg(retail_recreationlag2))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, retail_recreationlag4 = lg(retail_recreationlag3))

df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, grocery_pharmacylag1 = lg(grocery_pharmacy))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, grocery_pharmacylag2 = lg(grocery_pharmacylag1))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, grocery_pharmacylag3 = lg(grocery_pharmacylag2))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, grocery_pharmacylag4 = lg(grocery_pharmacylag3))

df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, parkslag1 = lg(parks))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, parkslag2 = lg(parkslag1))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, parkslag3 = lg(parkslag2))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, parkslag4 = lg(parkslag3))

df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, transit_stationslag1 = lg(transit_stations))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, transit_stationslag2 = lg(transit_stationslag1))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, transit_stationslag3 = lg(transit_stationslag2))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, transit_stationslag4 = lg(transit_stationslag3))

df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, workplaceslag1 = lg(workplaces))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, workplaceslag2 = lg(workplaceslag1))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, workplaceslag3 = lg(workplaceslag2))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, workplaceslag4 = lg(workplaceslag3))

df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, residentiallag1 = lg(residential))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, residentiallag2 = lg(residentiallag1))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, residentiallag3 = lg(residentiallag2))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, residentiallag4 = lg(residentiallag3))

df_model_10bins <- df_model_10bins %>% 
  dplyr::select(-retail_recreation, -grocery_pharmacy, -parks, 
                -transit_stations, -workplaces, -residential)

saveRDS(df_model_3bins, "./saved_objects/df_model_mobi3bins.rds")
saveRDS(df_model_5bins, "./saved_objects/df_model_mobi5bins.rds")
saveRDS(df_model_10bins, "./saved_objects/df_model_mobi10bins.rds")
