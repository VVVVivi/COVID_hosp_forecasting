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
          "lubridate", "ISOweek","surveillance")
load_package(pkgs)

df_cases_ltla <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=changeInNewCasesBySpecimenDate&metric=newCasesBySpecimenDate&metric=newCasesBySpecimenDateDirection&format=csv")
df_death_ltla <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newWeeklyNsoDeathsByRegDate&format=csv")
df_hosp_nhs <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&metric=newAdmissions&metric=newAdmissionsChange&metric=newAdmissionsDirection&metric=newAdmissionsChangePercentage&format=csv")
# df_vacc_ltla <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newPeopleVaccinatedFirstDoseByPublishDate&format=csv")

nhs_ltla <- read.csv("./saved_objects/nhs_ltla_table.csv")
pop <- read.csv("./data/ONS-population_2021-08-05.csv")

# match ltla to nhs region

############### Weekly cases ###########
#' There are 380 unique LTLAs in df_cases_ltla, 369 in df_death_ltla, and 347 in df_vacc_ltla, 
#' while some of them are for other nations (Scotland, etc). We only keep 315 LTLAs in England.
df_cases_ltla <- df_cases_ltla %>%
  filter(areaCode %in% nhs_ltla$lacode) %>%
  dplyr::rename(lacode = areaCode)

df_cases_nhs <- inner_join(df_cases_ltla, nhs_ltla, by= "lacode") %>%
  dplyr::select(date, newCasesBySpecimenDate, nhs_region) %>%
  relocate(nhs_region, .after = 1) %>%
  mutate(Year = isoWeekYear(ymd(date))[[1]],
         Week = isoWeekYear(ymd(date))[[2]])

#' Take the Thursday of each week as reference day.
#' This ensures that the whole week is assigned to the month to which the 
#' majority of the days of the week belong to.

df_cases_nhs <- df_cases_nhs %>%
  group_by(nhs_region, Year, Week) %>% 
  dplyr::summarise(newCasesBySpecimenDate = sum(newCasesBySpecimenDate)) %>%
  mutate(Week = ifelse(Week < 10, paste0("0", Week), as.character(Week))) %>%
  mutate(Date = ISOweek::ISOweek2date(paste0(Year,"-W", Week, "-4"))) %>%
  relocate(Date, .after = 1)

########## Weekly death ##########
#' Same operation on death data
df_death_ltla <- df_death_ltla %>%
  filter(areaCode %in% nhs_ltla$lacode) %>%
  dplyr::rename(lacode = areaCode)

#' There are 304 LTLAs reported weekly new death
df_death_nhs <- inner_join(df_death_ltla, nhs_ltla, by= "lacode") %>%
  dplyr::select(date, newWeeklyNsoDeathsByRegDate, nhs_region) %>%
  relocate(nhs_region, .after = 1) %>%
  mutate(Year = isoWeekYear(ymd(date))[[1]],
         Week = isoWeekYear(ymd(date))[[2]])

df_death_nhs <- df_death_nhs %>%
  group_by(nhs_region, Year, Week) %>% 
  dplyr::summarise(newWeeklyNsoDeathsByRegDate = sum(newWeeklyNsoDeathsByRegDate)) %>%
  mutate(Week = ifelse(Week < 10, paste0("0", Week), as.character(Week))) %>%
  mutate(Date = ISOweek::ISOweek2date(paste0(Year,"-W", Week, "-4"))) %>%
  relocate(Date, .after = 1)


#' Same operation on vaccination data
#' Vaccination data by LTLA only starts from 2022-03-07, which is not enough for our analysis.
#' In addition, there is no vaccination data by NHS region, only by region.
#' Therefore, we don't include vaccination data for now.

# df_vacc_ltla <- df_vacc_ltla %>%
#   filter(areaCode %in% nhs_ltla$lacode) %>%
#   rename(lacode = areaCode)
# 
# df_vacc_nhs <- inner_join(df_vacc_ltla, nhs_ltla, by= "lacode") %>%
#   dplyr::select(date, newPeopleVaccinatedFirstDoseByPublishDate, newPeopleVaccinatedSecondDoseByPublishDate,
#                 newPeopleVaccinatedThirdInjectionByPublishDate, nhs_region) %>%
#   relocate(nhs_region, .after = 1) %>%
  # mutate(Year = isoWeekYear(ymd(date))[[1]],
  #        Week = isoWeekYear(ymd(date))[[2]])

# df_vacc_nhs <- df_vacc_nhs %>%
#   group_by(nhs_region, Year, Week) %>% 
#   summarise(newPeopleVaccinatedFirstDoseByPublishDate = sum(newPeopleVaccinatedFirstDoseByPublishDate),
#             newPeopleVaccinatedSecondDoseByPublishDate = sum(newPeopleVaccinatedSecondDoseByPublishDate),
#             newPeopleVaccinatedThirdInjectionByPublishDate = sum(newPeopleVaccinatedThirdInjectionByPublishDate)) %>%
#   mutate(Week = ifelse(Week < 10, paste0("0", Week), as.character(Week))) %>%
#   mutate(Date = ISOweek::ISOweek2date(paste0(Year,"-W", Week, "-4"))) %>%
#   relocate(Date, .after = 1)

#' Process hospitalisation data
df_hosp_nhs <- df_hosp_nhs %>%
  dplyr::rename(nhs_region = areaName) %>%
  dplyr::select(date, newAdmissions, nhs_region) %>%
  relocate(nhs_region, .after = 1) %>%
  mutate(Year = isoWeekYear(ymd(date))[[1]],
         Week = isoWeekYear(ymd(date))[[2]])

df_hosp_nhs <- df_hosp_nhs %>%
  group_by(nhs_region, Year, Week) %>% 
  dplyr::summarise(newAdmissions = sum(newAdmissions)) %>%
  mutate(Week = ifelse(Week < 10, paste0("0", Week), as.character(Week))) %>%
  mutate(Date = ISOweek::ISOweek2date(paste0(Year,"-W", Week, "-4"))) %>%
  relocate(Date, .after = 1)

#' Merge cases data, death data and hospitalization data
df <- full_join(df_cases_nhs, df_death_nhs, by = c("Date", "Year", "Week", "nhs_region")) %>%
  full_join(., df_hosp_nhs, by = c("Date", "Year", "Week", "nhs_region"))

saveRDS(df, "./saved_objects/df_unadj.rds")

#' Adjust numbers by per capital
#' Calculate the population by NHS region
pop_ltla <- pop %>%
  filter(areaCode %in% nhs_ltla$lacode) %>%
  filter(age == "ALL") %>%
  dplyr::rename(lacode = areaCode)

pop_ltla <- inner_join(pop_ltla, nhs_ltla, by= "lacode") %>%
  relocate(nhs_region, .after = 1) %>%
  group_by(nhs_region) %>% 
  dplyr::summarise(population = sum(population))

df_adj <- inner_join(df, pop_ltla, by = "nhs_region")
saveRDS(df_adj, "./saved_objects/df_adj.rds")


#' While it's also accurate to divide deaths by population to find a per capita rate, 
#' those very small decimals would be difficult for most people to compare, 
#' so I multiply by 100,000 to present the results more clearly. 

############## 10 bins ########### 
df_adj_10bins <- df_adj %>%
  mutate(adj_newCasesBySpecimenDate = newCasesBySpecimenDate/population*100000,
         adj_newWeeklyNsoDeathsByRegDate = newWeeklyNsoDeathsByRegDate/population*100000,
         adj_newAdmissions = newAdmissions/population*100000)

#' Assign numeric hospitalization into categories


# a <- df_adj$adj_newAdmissions
# group <- quantile(a, probs = seq(.1, .9, by = .1), na.rm = TRUE)
# group2 <- ntile(a, 10)

#' create bind by decile
adj_newAdmissions_10group <- ntile(df_adj_10bins$adj_newAdmissions, 10)
df_adj_10bins$adj_newAdmissions_group <- adj_newAdmissions_10group 

#' creat uniform bins

df_adj_10bins$uniform_bins <- cut_interval(cut_interval_vec(df_adj_10bins$adj_newAdmissions),
                                           n = 10)
df_adj_10bins <- df_adj_10bins %>%
  mutate(adj_newAdmissions_uniformGroup = case_when(uniform_bins %in% levels(uniform_bins)[1]==1 ~ 1,
                                                    uniform_bins %in% levels(uniform_bins)[2]==1 ~ 2,
                                                    uniform_bins %in% levels(uniform_bins)[3]==1 ~ 3,
                                                    uniform_bins %in% levels(uniform_bins)[4]==1 ~ 4,
                                                    uniform_bins %in% levels(uniform_bins)[5]==1 ~ 5,
                                                    uniform_bins %in% levels(uniform_bins)[6]==1 ~ 6,
                                                    uniform_bins %in% levels(uniform_bins)[7]==1 ~ 7,
                                                    uniform_bins %in% levels(uniform_bins)[8]==1 ~ 8,
                                                    uniform_bins %in% levels(uniform_bins)[9]==1 ~ 9,
                                                    uniform_bins %in% levels(uniform_bins)[10]==1 ~ 10
                                                    )) %>% 
  mutate(adj_newAdmissions_uniformGroup = as.integer(adj_newAdmissions_uniformGroup)) %>% 
  dplyr::select(-uniform_bins)

saveRDS(df_adj_10bins, "./saved_objects/df_adj_10bins.rds")

############## 5 bins ###########
df_adj_5bins <- df_adj %>%
  mutate(adj_newCasesBySpecimenDate = newCasesBySpecimenDate/population*100000,
         adj_newWeeklyNsoDeathsByRegDate = newWeeklyNsoDeathsByRegDate/population*100000,
         adj_newAdmissions = newAdmissions/population*100000)

#' create bind by decile
adj_newAdmissions_5group <- ntile(df_adj_5bins$adj_newAdmissions, 5)
df_adj_5bins$adj_newAdmissions_group <- adj_newAdmissions_5group 

#' creat uniform bins

df_adj_5bins$uniform_bins <- cut_interval(cut_interval_vec(df_adj_5bins$adj_newAdmissions),
                                           n = 5)
df_adj_5bins <- df_adj_5bins %>%
  mutate(adj_newAdmissions_uniformGroup = case_when(uniform_bins %in% levels(uniform_bins)[1]==1 ~ 1,
                                                    uniform_bins %in% levels(uniform_bins)[2]==1 ~ 2,
                                                    uniform_bins %in% levels(uniform_bins)[3]==1 ~ 3,
                                                    uniform_bins %in% levels(uniform_bins)[4]==1 ~ 4,
                                                    uniform_bins %in% levels(uniform_bins)[5]==1 ~ 5
  )) %>% 
  mutate(adj_newAdmissions_uniformGroup = as.integer(adj_newAdmissions_uniformGroup)) %>% 
  dplyr::select(-uniform_bins)

saveRDS(df_adj_5bins, "./saved_objects/df_adj_5bins.rds")

############## 3 bins ###########
df_adj_3bins <- df_adj %>%
  mutate(adj_newCasesBySpecimenDate = newCasesBySpecimenDate/population*100000,
         adj_newWeeklyNsoDeathsByRegDate = newWeeklyNsoDeathsByRegDate/population*100000,
         adj_newAdmissions = newAdmissions/population*100000)

#' create bind by decile
adj_newAdmissions_3group <- ntile(df_adj_3bins$adj_newAdmissions, 3)
df_adj_3bins$adj_newAdmissions_group <- adj_newAdmissions_3group 

#' creat uniform bins

df_adj_3bins$uniform_bins <- cut_interval(cut_interval_vec(df_adj_3bins$adj_newAdmissions),
                                          n = 3)
df_adj_3bins <- df_adj_3bins %>%
  mutate(adj_newAdmissions_uniformGroup = case_when(uniform_bins %in% levels(uniform_bins)[1]==1 ~ 1,
                                                    uniform_bins %in% levels(uniform_bins)[2]==1 ~ 2,
                                                    uniform_bins %in% levels(uniform_bins)[3]==1 ~ 3
  )) %>% 
  mutate(adj_newAdmissions_uniformGroup = as.integer(adj_newAdmissions_uniformGroup)) %>% 
  dplyr::select(-uniform_bins)

saveRDS(df_adj_3bins, "./saved_objects/df_adj_3bins.rds")

############# adjustment for df_model_5bins ##########
df_3bins <- readRDS("./saved_objects/df_adj_3bins.rds")
df_5bins <- readRDS("./saved_objects/df_adj_5bins.rds")
df_10bins <- readRDS("./saved_objects/df_adj_10bins.rds")

df_model_5bins <- df_5bins %>%
  dplyr::select(nhs_region, Date, Year, Week, adj_newCasesBySpecimenDate,
                adj_newWeeklyNsoDeathsByRegDate, adj_newAdmissions_group,
                adj_newAdmissions_uniformGroup) %>%
  filter(!is.na(adj_newAdmissions_group))

#' create lags for data
lg <- function(x)c(NA,x[1:length(x)-1])

# ddply(dataframe, ~group, transform, lg(var))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_quan1 = lg(adj_newAdmissions_group))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_quan2 = lg(hosplag_quan1))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_quan3 = lg(hosplag_quan2))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_quan4 = lg(hosplag_quan3))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_quan5 = lg(hosplag_quan4))

df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_uni1 = lg(adj_newAdmissions_uniformGroup))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_uni2 = lg(hosplag_uni1))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_uni3 = lg(hosplag_uni2))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_uni4 = lg(hosplag_uni3))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_uni5 = lg(hosplag_uni4))

df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, caselag1 = lg(adj_newCasesBySpecimenDate))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, caselag2 = lg(caselag1))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, caselag3 = lg(caselag2))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, caselag4 = lg(caselag3))

df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, deathlag1 = lg(adj_newWeeklyNsoDeathsByRegDate))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, deathlag2 = lg(deathlag1))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, deathlag3 = lg(deathlag2))
df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, deathlag4 = lg(deathlag3))


df_model_5bins <- df_model_5bins %>%
  dplyr::select( -adj_newCasesBySpecimenDate,
                -adj_newWeeklyNsoDeathsByRegDate) %>%
  dplyr::rename(hosp_quan = adj_newAdmissions_group,
                hosp_uni = adj_newAdmissions_uniformGroup)

df_model_5bins <- df_model_5bins %>%
  mutate(hosp_quan = factor(hosp_quan, levels = c("1","2","3","4","5"),
                            ordered = TRUE),
         hosp_uni = factor(hosp_uni, levels = c("1","2","3","4","5"),
                          ordered = TRUE))

#' save df_model_5bins so I can load it directly next time
saveRDS(df_model_5bins, "./saved_objects/df_model_epi5bins.rds")


############# adjustment for df_model_10bins ##########
df_model_10bins <- df_10bins %>%
  dplyr::select(nhs_region, Date, Year, Week, adj_newCasesBySpecimenDate,
                adj_newWeeklyNsoDeathsByRegDate, adj_newAdmissions_group,
                adj_newAdmissions_uniformGroup) %>%
  filter(!is.na(adj_newAdmissions_group))

#' create lags for data
lg <- function(x)c(NA,x[1:length(x)-1])
#
# ddply(dataframe, ~group, transform, lg(var))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_quan1 = lg(adj_newAdmissions_group))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_quan2 = lg(hosplag_quan1))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_quan3 = lg(hosplag_quan2))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_quan4 = lg(hosplag_quan3))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_quan5 = lg(hosplag_quan4))

df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_uni1 = lg(adj_newAdmissions_uniformGroup))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_uni2 = lg(hosplag_uni1))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_uni3 = lg(hosplag_uni2))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_uni4 = lg(hosplag_uni3))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_uni5 = lg(hosplag_uni4))

df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, caselag1 = lg(adj_newCasesBySpecimenDate))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, caselag2 = lg(caselag1))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, caselag3 = lg(caselag2))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, caselag4 = lg(caselag3))

df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, deathlag1 = lg(adj_newWeeklyNsoDeathsByRegDate))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, deathlag2 = lg(deathlag1))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, deathlag3 = lg(deathlag2))
df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, deathlag4 = lg(deathlag3))


df_model_10bins <- df_model_10bins %>%
  dplyr::select( -adj_newCasesBySpecimenDate,
                -adj_newWeeklyNsoDeathsByRegDate) %>%
  dplyr::rename(hosp_quan = adj_newAdmissions_group,
                hosp_uni = adj_newAdmissions_uniformGroup)

df_model_10bins <- df_model_10bins %>%
  mutate(hosp_quan = factor(hosp_quan, levels = c("1","2","3","4","5","6","7","8","9","10"),
                            ordered = TRUE),
         hosp_uni = factor(hosp_uni, levels = c("1","2","3","4","5","6","7","8","9","10"),
                          ordered = TRUE))
#' save df_model_10bins so I can load it directly next time
saveRDS(df_model_10bins, "./saved_objects/df_model_epi10bins.rds")

df_model_3bins <- df_3bins %>%
  dplyr::select(nhs_region, Date, Year, Week, adj_newCasesBySpecimenDate,
                adj_newWeeklyNsoDeathsByRegDate, adj_newAdmissions_group,
                adj_newAdmissions_uniformGroup) %>%
  filter(!is.na(adj_newAdmissions_group))

df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, hosplag_quan1 = lg(adj_newAdmissions_group))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, hosplag_quan2 = lg(hosplag_quan1))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, hosplag_quan3 = lg(hosplag_quan2))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, hosplag_quan4 = lg(hosplag_quan3))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, hosplag_quan5 = lg(hosplag_quan4))

df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, hosplag_uni1 = lg(adj_newAdmissions_uniformGroup))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, hosplag_uni2 = lg(hosplag_uni1))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, hosplag_uni3 = lg(hosplag_uni2))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, hosplag_uni4 = lg(hosplag_uni3))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, hosplag_uni5 = lg(hosplag_uni4))

df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, caselag1 = lg(adj_newCasesBySpecimenDate))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, caselag2 = lg(caselag1))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, caselag3 = lg(caselag2))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, caselag4 = lg(caselag3))

df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, deathlag1 = lg(adj_newWeeklyNsoDeathsByRegDate))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, deathlag2 = lg(deathlag1))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, deathlag3 = lg(deathlag2))
df_model_3bins <- ddply(df_model_3bins, ~nhs_region, transform, deathlag4 = lg(deathlag3))

df_model_3bins <- df_model_3bins %>%
  dplyr::select( -adj_newCasesBySpecimenDate,
                 -adj_newWeeklyNsoDeathsByRegDate) %>%
  dplyr::rename(hosp_quan = adj_newAdmissions_group,
                hosp_uni = adj_newAdmissions_uniformGroup)

df_model_3bins <- df_model_3bins %>%
  mutate(hosp_quan = factor(hosp_quan, levels = c("1","2","3"),
                            ordered = TRUE),
         hosp_uni = factor(hosp_uni, levels = c("1","2","3"),
                           ordered = TRUE))

#' save df_model_3bins so I can load it directly next time
saveRDS(df_model_3bins, "./saved_objects/df_model_epi3bins.rds")
