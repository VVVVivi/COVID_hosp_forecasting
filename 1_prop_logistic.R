#' Make a note of when the report was generated.
Sys.time()
knitr.table.format = "markdown"

#' As always, remove all objects fromt the workspace before starting.
rm(list = ls(all = TRUE))

setwd("C:/Users/haowe/Desktop/COVID-forecasting")
setwd("C:/Users/hw3616/Desktop/COVID-forecasting")

#' Pull in packages needed
source("./functions/load_packages.R")
source("./functions/data_process.R")
source("./functions/fixed_polr.R")

pkgs <- c("stringr", "dplyr", "ggplot2", "aweek",
          "lubridate", "ISOweek", "plyr", "MASS",
          "ordinal", "brant", "fastDummies","rms")
load_package(pkgs)

#' Load data
#' df: wrangled raw data
#' df_model_10bins: further process data for running model
df_5bins <- readRDS("./saved_objects/df_adj_5bins.rds")
df_10bins <- readRDS("./saved_objects/df_adj_10bins.rds")

df_model_3bins <- readRDS("./saved_objects/df_model_mobi3bins.rds")
df_model_5bins <- readRDS("./saved_objects/df_model_mobi5bins.rds")
df_model_10bins <- readRDS("./saved_objects/df_model_mobi10bins.rds")

############# adjustment for df_model_5bins ##########
#' df_model_5bins <- df_5bins %>%
#'   dplyr::select(nhs_region, Date, Year, Week, adj_newCasesBySpecimenDate,
#'                 adj_newWeeklyNsoDeathsByRegDate, adj_newAdmissions_group,
#'                 adj_newAdmissions_uniformGroup) %>%
#'   filter(!is.na(adj_newAdmissions_group))
#' 
#' #' create lags for data
#' lg <- function(x)c(NA,x[1:length(x)-1])
#' 
#' # ddply(dataframe, ~group, transform, lg(var))
#' df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_quan1 = lg(adj_newAdmissions_group))
#' df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_quan2 = lg(hosplag_quan1))
#' df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_quan3 = lg(hosplag_quan2))
#' df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_quan4 = lg(hosplag_quan3))
#' df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_quan5 = lg(hosplag_quan4))
#' 
#' df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_uni1 = lg(adj_newAdmissions_uniformGroup))
#' df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_uni2 = lg(hosplag_uni1))
#' df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_uni3 = lg(hosplag_uni2))
#' df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_uni4 = lg(hosplag_uni3))
#' df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, hosplag_uni5 = lg(hosplag_uni4))
#' 
#' df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, caselag1 = lg(adj_newCasesBySpecimenDate))
#' df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, caselag2 = lg(caselag1))
#' df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, caselag3 = lg(caselag2))
#' df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, caselag4 = lg(caselag3))
#' 
#' df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, deathlag1 = lg(adj_newWeeklyNsoDeathsByRegDate))
#' df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, deathlag2 = lg(deathlag1))
#' df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, deathlag3 = lg(deathlag2))
#' df_model_5bins <- ddply(df_model_5bins, ~nhs_region, transform, deathlag4 = lg(deathlag3))
#' 
#' 
#' df_model_5bins <- df_model_5bins %>%
#'   dplyr::select( -adj_newCasesBySpecimenDate,
#'                 -adj_newWeeklyNsoDeathsByRegDate) %>%
#'   dplyr::rename(hosp_quan = adj_newAdmissions_group,
#'                 hosp_uni = adj_newAdmissions_uniformGroup)
#' 
#' df_model_5bins <- df_model_5bins %>%
#'   mutate(hosp_quan = factor(hosp_quan, levels = c("1","2","3","4","5"),
#'                             ordered = TRUE),
#'          hosp_uni = factor(hosp_uni, levels = c("1","2","3","4","5"),
#'                           ordered = TRUE))
#' 
#' #' save df_model_5bins so I can load it directly next time
#' saveRDS(df_model_5bins, "./saved_objects/df_model_5bins.rds")


############# adjustment for df_model_10bins ##########
#' df_model_10bins <- df_10bins %>%
#'   dplyr::select(nhs_region, Date, Year, Week, adj_newCasesBySpecimenDate,
#'                 adj_newWeeklyNsoDeathsByRegDate, adj_newAdmissions_group,
#'                 adj_newAdmissions_uniformGroup) %>%
#'   filter(!is.na(adj_newAdmissions_group)) 
#' 
#' #' create lags for data
#' lg <- function(x)c(NA,x[1:length(x)-1])
#' # 
#' # ddply(dataframe, ~group, transform, lg(var))
#' df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_quan1 = lg(adj_newAdmissions_group))
#' df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_quan2 = lg(hosplag_quan1))
#' df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_quan3 = lg(hosplag_quan2))
#' df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_quan4 = lg(hosplag_quan3))
#' df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_quan5 = lg(hosplag_quan4))
#' 
#' df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_uni1 = lg(adj_newAdmissions_uniformGroup))
#' df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_uni2 = lg(hosplag_uni1))
#' df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_uni3 = lg(hosplag_uni2))
#' df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_uni4 = lg(hosplag_uni3))
#' df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, hosplag_uni5 = lg(hosplag_uni4))
#' 
#' df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, caselag1 = lg(adj_newCasesBySpecimenDate))
#' df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, caselag2 = lg(caselag1))
#' df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, caselag3 = lg(caselag2))
#' df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, caselag4 = lg(caselag3))
#' 
#' df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, deathlag1 = lg(adj_newWeeklyNsoDeathsByRegDate))
#' df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, deathlag2 = lg(deathlag1))
#' df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, deathlag3 = lg(deathlag2))
#' df_model_10bins <- ddply(df_model_10bins, ~nhs_region, transform, deathlag4 = lg(deathlag3))
#' 
#' 
#' df_model_10bins <- df_model_10bins %>%
#'   dplyr::select( -adj_newCasesBySpecimenDate,
#'                 -adj_newWeeklyNsoDeathsByRegDate) %>%
#'   dplyr::rename(hosp_quan = adj_newAdmissions_group,
#'                 hosp_uni = adj_newAdmissions_uniformGroup)
#' 
#' df_model_10bins <- df_model_10bins %>%
#'   mutate(hosp_quan = factor(hosp_quan, levels = c("1","2","3","4","5","6","7","8","9","10"),
#'                             ordered = TRUE),
#'          hosp_uni = factor(hosp_uni, levels = c("1","2","3","4","5","6","7","8","9","10"),
#'                           ordered = TRUE))
#' save df_model_10bins so I can load it directly next time
# saveRDS(df_model_10bins, "./saved_objects/df_model_10bins.rds")


#' Description figure
# ggplot(df_model_10bins, aes(x = Date, y = as.numeric(adj_newAdmissions_group)))+
#   geom_point(aes(colour = nhs_region))+
#   geom_line(aes(colour = nhs_region))+
#   facet_grid(.~nhs_region)+
#   ylab(label = "Hospotalization level")+
#   scale_y_continuous(breaks = c(1:10), labels = c(1:10))+
#   theme_bw()+
#   theme(legend.position="bottom",
#         text = element_text(size = 24),
#         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
# 
# ggsave(filename = "./figures/hosp_ts.pdf", width = 24, height = 14, dpi = 320, scale = 1)
# ggsave(filename = "./figures/hosp_ts.png", width = 24, height = 14, dpi = 320, scale = 1)


## fit ordered logistic regression model
# train <- df_model_10bins[which(df_model_10bins$Year %in% c(2020,2021)),]
# 
# test <-  df_model_10bins[which(df_model_10bins$year22 ==1),] 
# 
# m <- polr(hosp ~ nhs_region + hosplag1 + hosplag2 + caselag1 + deathlag1, 
#           data = train, Hess=TRUE)
# 
# p <- predict(m, test[1,], type = "probs")



########### 1-week ahead #############

OLR_data <- function(data, nWeek_ahead, i, bins_type){
  require(dplyr)
  
  if(bins_type == "uniform"){
    data <- data %>%
      dplyr::select(nhs_region, Date, Year, Week, hosp_uni,
                    paste0("hosplag_uni", nWeek_ahead),
                    paste0("hosplag_uni", nWeek_ahead+1),
                    paste0("caselag", nWeek_ahead),
                    paste0("deathlag", nWeek_ahead),
                    paste0("retail_recreationlag", nWeek_ahead),
                    paste0("grocery_pharmacylag", nWeek_ahead),
                    paste0("parkslag", nWeek_ahead),
                    paste0("transit_stationslag", nWeek_ahead),
                    paste0("workplaceslag", nWeek_ahead),
                    paste0("residentiallag", nWeek_ahead)
      )
  }
  
  if(bins_type == "quantile"){
    data <- data %>%
      dplyr::select(nhs_region, Date, Year, Week, hosp_quan,
                    paste0("hosplag_quan", nWeek_ahead),
                    paste0("hosplag_quan", nWeek_ahead+1),
                    paste0("caselag", nWeek_ahead),
                    paste0("deathlag", nWeek_ahead),
                    paste0("retail_recreationlag", nWeek_ahead),
                    paste0("grocery_pharmacylag", nWeek_ahead),
                    paste0("parkslag", nWeek_ahead),
                    paste0("transit_stationslag", nWeek_ahead),
                    paste0("workplaceslag", nWeek_ahead),
                    paste0("residentiallag", nWeek_ahead)
      )
  }
  
  data_train <- data[which(data$Year %in% c(2020,2021)), ]
  data_test <- data[which(data$Year %in% c(2022)), ]
  
  yearWeek <- unique(data_test[,c("Year", "Week")])
  week_data <- unique(data_test[,"Week"])

  if(i != 0){
    new_train <- rbind(data_train, data_test[which(data_test[,"Week"] == week_data[i]),])
  }else{
    new_train <- data_train
  }
  
  new_test <- subset(data_test, data_test[,"Week"] == week_data[i+1])
  
  df <- list(new_train = new_train, 
             new_test = new_test)
}

# test OLR_data function 
test_1week <- OLR_data(df_model_10bins, 1, 1, "quantile")

OLR_pred <- function(data, nWeek_ahead, bins_type){
  data <- data %>%
    arrange(Year, Week) 
  data <- na.omit(data)
  
  train <- data[which(data$Year %in% c(2020, 2021)),]
  test <- data[which(data$Year %in% c(2022)), ]

  test_weeknum <- dim(unique(test[,c("Year", "Week")]))[1]
    
  model_list <- list()

  if(bins_type == "uniform"){
    pred_mat <- data.frame(Year = test$Year,
                           Week = test$Week,
                           hosp_uni = test$hosp_uni,
                           nhs_region = test$nhs_region,
                           stringsAsFactors=FALSE)
    pred <- NULL
    
    for (i in 0:(test_weeknum-1)){
      #define train and test data sets
      df <- OLR_data(data, nWeek_ahead, i, bins_type)
      
      data_train <- df$new_train
      data_test <- df$new_test
      
      if(nWeek_ahead == 1){
        model <- polr(hosp_uni ~ hosplag_uni1 + hosplag_uni2 + caselag1 + deathlag1, 
                      data = train, Hess=TRUE)
      }
      if(nWeek_ahead == 2){
        model <- polr(hosp_uni ~ hosplag_uni2 + hosplag_uni3 + caselag2 + deathlag2, 
                      data = train, Hess=TRUE)
      }
      if(nWeek_ahead == 3){
        model <- polr(hosp_uni ~ hosplag_uni3 + hosplag_uni4 + caselag3 + deathlag3, 
                      data = train, Hess=TRUE)
      }
      if(nWeek_ahead == 4){
        model <- polr(hosp_uni ~ hosplag_uni4 + hosplag_uni5 + caselag4 + deathlag4, 
                      data = train, Hess=TRUE)
      }
      model_list[[i+1]] <- model
      names(model_list)[i+1] <- paste0("model_week", i+1)
      olr_pred <- predict(model, data_test, type = "probs")
      # pred_cat <- which.max(olr_pred)
      
      pred <- rbind(pred, olr_pred)
    }
  }
  
  if(bins_type == "quantile"){
    pred_mat <- data.frame(Year = test$Year,
                           Week = test$Week,
                           hosp_quan = test$hosp_quan,
                           nhs_region = test$nhs_region,
                           stringsAsFactors=FALSE)
    pred <- NULL
    
    for (i in 0:(test_weeknum-1)){
      #define train and test data sets
      df <- OLR_data(data, nWeek_ahead, i, bins_type)
      
      data_train <- df$new_train
      data_test <- df$new_test
      
      if(nWeek_ahead == 1){
        model <- polr(hosp_quan ~ hosplag_quan1 + hosplag_quan2 + caselag1 + deathlag1, 
                      data = train, Hess=TRUE)
      }
      if(nWeek_ahead == 2){
        model <- polr(hosp_quan ~ hosplag_quan2 + hosplag_quan3 + caselag2 + deathlag2, 
                      data = train, Hess=TRUE)
      }
      if(nWeek_ahead == 3){
        model <- polr(hosp_quan ~ hosplag_quan3 + hosplag_quan4 + caselag3 + deathlag3, 
                      data = train, Hess=TRUE)
      }
      if(nWeek_ahead == 4){
        model <- polr(hosp_quan ~ hosplag_quan4 + hosplag_quan5 + caselag4 + deathlag4, 
                      data = train, Hess=TRUE)
      }
      model_list[[i+1]] <- model
      names(model_list)[i+1] <- paste0("model_week", i+1)
      olr_pred <- predict(model, data_test, type = "probs")
      # pred_cat <- which.max(olr_pred)
      
      pred <- rbind(pred, olr_pred)
    }
  }

  pred_level <- apply(pred, 1, which.max)
  pred_mat <- cbind(pred_mat, pred_level) 
  
  names(pred_mat)[names(pred_mat) == "pred_level"] <- paste0(nWeek_ahead,"week_pred")

  return(pred_mat)
}

#' Test on OLR_pred() function
test_OLR_pred <- OLR_pred(df_model_10bins, 2, "uniform")


############### 1,2,3,4-week ahead prediction ##############
#' decile bins prediction
OLR_pred_quan3bins_1week <- OLR_pred(df_model_3bins, 1, "quantile")
OLR_pred_quan3bins_2week <- OLR_pred(df_model_3bins, 2, "quantile")
OLR_pred_quan3bins_3week <- OLR_pred(df_model_3bins, 3, "quantile")
OLR_pred_quan3bins_4week <- OLR_pred(df_model_3bins, 4, "quantile")

OLR_pred_quan5bins_1week <- OLR_pred(df_model_5bins, 1, "quantile")
OLR_pred_quan5bins_2week <- OLR_pred(df_model_5bins, 2, "quantile")
OLR_pred_quan5bins_3week <- OLR_pred(df_model_5bins, 3, "quantile")
OLR_pred_quan5bins_4week <- OLR_pred(df_model_5bins, 4, "quantile")

OLR_pred_quan10bins_1week <- OLR_pred(df_model_10bins, 1, "quantile")
OLR_pred_quan10bins_2week <- OLR_pred(df_model_10bins, 2, "quantile")
OLR_pred_quan10bins_3week <- OLR_pred(df_model_10bins, 3, "quantile")
OLR_pred_quan10bins_4week <- OLR_pred(df_model_10bins, 4, "quantile")

#' uniform bins prediction
OLR_pred_uni3bins_1week <- OLR_pred(df_model_3bins, 1, "uniform")
OLR_pred_uni3bins_2week <- OLR_pred(df_model_3bins, 2, "uniform")
OLR_pred_uni3bins_3week <- OLR_pred(df_model_3bins, 3, "uniform")
OLR_pred_uni3bins_4week <- OLR_pred(df_model_3bins, 4, "uniform")

OLR_pred_uni5bins_1week <- OLR_pred(df_model_5bins, 1, "uniform")
OLR_pred_uni5bins_2week <- OLR_pred(df_model_5bins, 2, "uniform")
OLR_pred_uni5bins_3week <- OLR_pred(df_model_5bins, 3, "uniform")
OLR_pred_uni5bins_4week <- OLR_pred(df_model_5bins, 4, "uniform")

OLR_pred_uni10bins_1week <- OLR_pred(df_model_10bins, 1, "uniform")
OLR_pred_uni10bins_2week <- OLR_pred(df_model_10bins, 2, "uniform")
OLR_pred_uni10bins_3week <- OLR_pred(df_model_10bins, 3, "uniform")
OLR_pred_uni10bins_4week <- OLR_pred(df_model_10bins, 4, "uniform")

#' Merge 1-4 week ahead predictions into one data frame
OLR_pred_quan3bins_all <- full_join(OLR_pred_quan3bins_1week, OLR_pred_quan3bins_2week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., OLR_pred_quan3bins_3week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., OLR_pred_quan3bins_4week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

OLR_pred_quan5bins_all <- full_join(OLR_pred_quan5bins_1week, OLR_pred_quan5bins_2week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., OLR_pred_quan5bins_3week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., OLR_pred_quan5bins_4week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

OLR_pred_quan10bins_all <- full_join(OLR_pred_quan10bins_1week, OLR_pred_quan10bins_2week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., OLR_pred_quan10bins_3week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., OLR_pred_quan10bins_4week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

OLR_pred_uni3bins_all <- full_join(OLR_pred_uni3bins_1week, OLR_pred_uni3bins_2week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., OLR_pred_uni3bins_3week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., OLR_pred_uni3bins_4week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni , .after = 4)

OLR_pred_uni5bins_all <- full_join(OLR_pred_uni5bins_1week, OLR_pred_uni5bins_2week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., OLR_pred_uni5bins_3week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., OLR_pred_uni5bins_4week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni , .after = 4)

OLR_pred_uni10bins_all <- full_join(OLR_pred_uni10bins_1week, OLR_pred_uni10bins_2week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., OLR_pred_uni10bins_3week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., OLR_pred_uni10bins_4week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni , .after = 4)

OLR_pred_all <- list(OLR_pred_quan3bins_all = OLR_pred_quan3bins_all,
                     OLR_pred_uni3bins_all = OLR_pred_uni3bins_all,
                     OLR_pred_quan5bins_all = OLR_pred_quan5bins_all,
                     OLR_pred_quan10bins_all = OLR_pred_quan10bins_all,
                     OLR_pred_uni5bins_all = OLR_pred_uni5bins_all,
                     OLR_pred_uni10bins_all = OLR_pred_uni10bins_all)

saveRDS(OLR_pred_all, "./saved_objects/OLR_pred_all.rds")


######### test on pathological data #########
df_path <- readRDS("./saved_objects/df_path.rds")
df_path_3bins <- df_path$df_path_3bins
df_path_5bins <- df_path$df_path_5bins
df_path_10bins <- df_path$df_path_10bins

df_path_3bins <- df_path_3bins %>%
  mutate(nhs_region = as.factor(nhs_region))
df_path_5bins <- df_path_5bins %>%
  mutate(nhs_region = as.factor(nhs_region))
df_path_10bins <- df_path_10bins %>%
  mutate(nhs_region = as.factor(nhs_region))

df_path_3bins$hosp_quan <- as.factor(df_path_3bins$hosp_quan)

OLR_pred_path3bins_1week <- OLR_pred(df_path_3bins, 1, "quantile")
OLR_pred_path3bins_2week <- OLR_pred(df_path_3bins, 2, "quantile")
OLR_pred_path3bins_3week <- OLR_pred(df_path_3bins, 3, "quantile")
OLR_pred_path3bins_4week <- OLR_pred(df_path_3bins, 4, "quantile")

OLR_pred_path3bins_all <- full_join(OLR_pred_path3bins_1week, OLR_pred_path3bins_2week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., OLR_pred_path3bins_3week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., OLR_pred_path3bins_4week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)


##################################################################
# not used #
#################################################################
#' 1-week ahead plot
df_1week <- rbind(EE_1week[,c("nhs_region","weekmum", "Obs","Pred")], 
                  London_1week[,c("nhs_region","weekmum", "Obs","Pred")]) %>%
  rbind(., Midlands_1week[,c("nhs_region","weekmum", "Obs","Pred")])%>%
  rbind(., NEY_1week[,c("nhs_region","weekmum", "Obs","Pred")])%>%
  rbind(., NW_1week[,c("nhs_region","weekmum", "Obs","Pred")])%>%
  rbind(., SE_1week[,c("nhs_region","weekmum", "Obs","Pred")])%>%
  rbind(., SW_1week[,c("nhs_region","weekmum", "Obs","Pred")])

df_1week <- df_1week %>%
  mutate(nhs_region = factor(nhs_region, levels = c("North West", "North East and Yorkshire", 
                                                    "Midlands", "East of England", "South West",
                                                    "London", "South East"))) %>%
  mutate(nWeek_ahead = 1)

ggplot(df_1week, aes(x=weekmum)) +
  geom_point(aes(y=as.numeric(Pred), colour = "red"))+
  geom_point(aes(y=as.numeric(Obs), colour = "blue")) +  
  labs(title = "Observed vs Predicted per region - Rolling Forecast", x = "Weeks", y = "Hospitalization level") +
  scale_color_manual(labels = c("Prediction", "Observation"), values = c("red", "blue")) +
  facet_grid(.~nhs_region)+
  theme_bw()+
  ylab("Hospitalization level") +
  xlab("Weeks")

ggsave(filename = "./figures/1week_olr.pdf", width = 24, height = 14, dpi = 320, scale = 1)
ggsave(filename = "./figures/1week_olr.png", width = 24, height = 14, dpi = 320, scale = 1)


######### accuracy metric ##########
compare_accuracy_region <- function(pred_result, region, num_category, nWeek_ahead){
  require(Metrics)
  
  if(region == "All"){
    pred_res_region <- pred_result[which(pred_result$nWeek_ahead==nWeek_ahead), ]
  }else{
    pred_res_region <- pred_result[which(pred_result$nhs_region == region & pred_result$nWeek_ahead==nWeek_ahead), ]
  }
  browser()
  accuracy <- length(which(pred_res_region$Obs == pred_res_region$Pred))/nrow(pred_res_region)
  MZE <- 1- accuracy
  MAE <- mae(pred_res_region$Obs, pred_res_region$Pred)
  
  # macro_averaged MAE
  macroMAE_vec <- c()
  for (i in 1:num_category){
    if (i %in% pred_res_region$Obs){
      mat <- pred_res_region[which(pred_res_region$Obs == i),]
      macroMAE_tmp <- mae(mat$Obs, mat$Pred)
    }else{
      macroMAE_tmp <- 0 
    }
    macroMAE_vec <- c(macroMAE_vec, macroMAE_tmp)
  }
  macroMAE <- sum(macroMAE_vec)/num_category
  
  result <- data.frame(Region = region, nWeek_ahead = nWeek_ahead, Accuracy = accuracy, 
                       MZE = MZE, MAE = MAE, macroMAE = macroMAE)
  
  return(result)
}

EE_accuracy <- rbind(compare_accuracy_region(df_OLR_pred_all, "East of England", 10, nWeek_ahead = 1),
                     compare_accuracy_region(df_OLR_pred_all, "East of England", 10, nWeek_ahead = 2)) %>%
  rbind(., compare_accuracy_region(df_OLR_pred_all, "East of England", 10, nWeek_ahead = 3)) %>%
  rbind(., compare_accuracy_region(df_OLR_pred_all, "East of England", 10, nWeek_ahead = 4))

London_accuracy <- rbind(compare_accuracy_region(df_OLR_pred_all, "London", 10, nWeek_ahead = 1),
                     compare_accuracy_region(df_OLR_pred_all, "London", 10, nWeek_ahead = 2)) %>%
  rbind(., compare_accuracy_region(df_OLR_pred_all, "London", 10, nWeek_ahead = 3)) %>%
  rbind(., compare_accuracy_region(df_OLR_pred_all, "London", 10, nWeek_ahead = 4))

Midlands_accuracy <- rbind(compare_accuracy_region(df_OLR_pred_all, "Midlands", 10, nWeek_ahead = 1),
                         compare_accuracy_region(df_OLR_pred_all, "Midlands", 10, nWeek_ahead = 2)) %>%
  rbind(., compare_accuracy_region(df_OLR_pred_all, "Midlands", 10, nWeek_ahead = 3)) %>%
  rbind(., compare_accuracy_region(df_OLR_pred_all, "Midlands", 10, nWeek_ahead = 4))

NEY_accuracy <- rbind(compare_accuracy_region(df_OLR_pred_all, "North East and Yorkshire", 10, nWeek_ahead = 1),
                           compare_accuracy_region(df_OLR_pred_all, "North East and Yorkshire", 10, nWeek_ahead = 2)) %>%
  rbind(., compare_accuracy_region(df_OLR_pred_all, "North East and Yorkshire", 10, nWeek_ahead = 3)) %>%
  rbind(., compare_accuracy_region(df_OLR_pred_all, "North East and Yorkshire", 10, nWeek_ahead = 4))

NW_accuracy <- rbind(compare_accuracy_region(df_OLR_pred_all, "North West", 10, nWeek_ahead = 1),
                           compare_accuracy_region(df_OLR_pred_all, "North West", 10, nWeek_ahead = 2)) %>%
  rbind(., compare_accuracy_region(df_OLR_pred_all, "North West", 10, nWeek_ahead = 3)) %>%
  rbind(., compare_accuracy_region(df_OLR_pred_all, "North West", 10, nWeek_ahead = 4))

SE_accuracy <- rbind(compare_accuracy_region(df_OLR_pred_all, "South East", 10, nWeek_ahead = 1),
                     compare_accuracy_region(df_OLR_pred_all, "South East", 10, nWeek_ahead = 2)) %>%
  rbind(., compare_accuracy_region(df_OLR_pred_all, "South East", 10, nWeek_ahead = 3)) %>%
  rbind(., compare_accuracy_region(df_OLR_pred_all, "South East", 10, nWeek_ahead = 4))

sw_accuracy <- rbind(compare_accuracy_region(df_OLR_pred_all, "South West", 10, nWeek_ahead = 1),
                           compare_accuracy_region(df_OLR_pred_all, "South West", 10, nWeek_ahead = 2)) %>%
  rbind(., compare_accuracy_region(df_OLR_pred_all, "South West", 10, nWeek_ahead = 3)) %>%
  rbind(., compare_accuracy_region(df_OLR_pred_all, "South West", 10, nWeek_ahead = 4))

all_accuracy <- rbind(compare_accuracy_region(df_OLR_pred_all, "All", 10, nWeek_ahead = 1),
                      compare_accuracy_region(df_OLR_pred_all, "All", 10, nWeek_ahead = 2)) %>%
  rbind(., compare_accuracy_region(df_OLR_pred_all, "All", 10, nWeek_ahead = 3)) %>%
  rbind(., compare_accuracy_region(df_OLR_pred_all, "All", 10, nWeek_ahead = 4))

accuracy_OLR_allRegion <- rbind(EE_accuracy, London_accuracy) %>%
  rbind(., Midlands_accuracy) %>%
  rbind(., NEY_accuracy) %>%
  rbind(., NW_accuracy) %>%
  rbind(., SE_accuracy) %>%
  rbind(., SE_accuracy) 

b <- df_OLR_pred_all[which(df_OLR_pred_all$nWeek_ahead==1),]
