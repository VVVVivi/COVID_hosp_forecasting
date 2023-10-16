#' Make a note of when the report was generated.
Sys.time()
knitr.table.format = "markdown"

#' As always, remove all objects from the workspace before starting.
rm(list = ls(all = TRUE))

setwd("C:/Users/haowe/Desktop/COVID-forecasting")
# setwd("C:/Users/hw3616/Desktop/COVID-forecasting")

#' Pull in packages needed
source("./functions/load_packages.R")
source("./functions/data_process.R")
source("./functions/fixed_polr.R")

pkgs <- c("haven", "xgboost", "stringr", "rasterVis","hrbrthemes",
          "dplyr", "ggplot2", "aweek", "surveillance", "plyr", 
          "mltools", "data.table")
load_package(pkgs)

#' Load data
#' df_model: further process data for running model
# df <- readRDS("./saved_objects/df_adj.rds")
# df_model_3bins <- readRDS("./saved_objects/df_model_mobi3bins.rds")
# df_model_5bins <- readRDS("./saved_objects/df_model_mobi5bins.rds")
# df_model_10bins <- readRDS("./saved_objects/df_model_mobi10bins.rds")

df_model_3bins <- readRDS("./saved_objects/df_model_mobiwea3bins.rds")
df_model_5bins <- readRDS("./saved_objects/df_model_mobiwea5bins.rds")
df_model_10bins <- readRDS("./saved_objects/df_model_mobiwea10bins.rds")


df_model_3bins <- df_model_3bins %>%
  mutate(nhs_region = as.factor(nhs_region))

df_model_5bins <- df_model_5bins %>%
  mutate(nhs_region = as.factor(nhs_region))

df_model_10bins <- df_model_10bins %>%
  mutate(nhs_region = as.factor(nhs_region))

#' prepare data for xgboost model
xgboost_hosp_dat_epi <- function(data, nWeek_ahead, i, bins_type,
                                     train_start_yr,train_end_yr, 
                                     test_start_yr, test_end_yr){
  require(dplyr)
  require(xgboost)
  require(data.table)
  
  if(bins_type == "uniform"){
    data <- data %>%
      dplyr::select(nhs_region, Date, Year, Week, hosp_uni,
                    paste0("hosplag_uni", nWeek_ahead),
                    paste0("hosplag_uni", nWeek_ahead+1),
                    paste0("caselag", nWeek_ahead),
                    paste0("deathlag", nWeek_ahead)
      ) %>%
      relocate(nhs_region, .after = 5)
    
    data_train <- data[which(data$Year %in% c(train_start_yr:train_end_yr)), ]
    data_test <- data[which(data$Year %in% c(test_start_yr:test_end_yr)), ]
    
    # define predictor and response variables in training set
    # variables that I want to do one-hot encoding must be in factor class
    # -c(1:2,4): remove Date, Year, hosp
    train_x <- one_hot(dt = data.table::as.data.table(data_train[,-c(1:2,4)]))  %>%
      data.matrix()
    train_y <- data_train[,"hosp_uni"]
    
    # define predictor and response variables in testing set
    test_x <- one_hot(dt = data.table::as.data.table(data_test[,-c(1:2,4)])) %>%
      data.matrix()
    test_y <- data_test[,"hosp_uni"]
  }
  
  if(bins_type == "quantile"){
    data <- data %>%
      dplyr::select(nhs_region, Date, Year, Week, hosp_quan,
                    paste0("hosplag_quan", nWeek_ahead),
                    paste0("hosplag_quan", nWeek_ahead+1),
                    paste0("caselag", nWeek_ahead),
                    paste0("deathlag", nWeek_ahead)
      ) %>%
      relocate(nhs_region, .after = 5)
    
    data_train <- data[which(data$Year %in% c(train_start_yr:train_end_yr)), ]
    data_test <- data[which(data$Year %in% c(test_start_yr:test_end_yr)), ]
    
    # define predictor and response variables in training set
    # variables that I want to do one-hot encoding must be in factor class
    # -c(1:2,4): remove Date, Year, hosp
    train_x <- one_hot(dt = data.table::as.data.table(data_train[,-c(1:2,4)]))  %>%
      data.matrix()
    train_y <- data_train[,"hosp_quan"]
    
    # define predictor and response variables in testing set
    test_x <- one_hot(dt = data.table::as.data.table(data_test[,-c(1:2,4)])) %>%
      data.matrix()
    test_y <- data_test[,"hosp_quan"]
  }
  
  yearWeek <- unique(data_test[,c("Year", "Week")])
  week_data_matrix <- unique(test_x[,"Week"])
  
  if(i != 0){
    new_train_x <- rbind(train_x, test_x[which(test_x[,"Week"] == week_data_matrix[i]),]) %>%
      as.matrix()
    new_train_y <- append(train_y, test_y[which(test_x[,"Week"] == week_data_matrix[i])]) %>%
      as.numeric()
  }else{
    new_train_x <- train_x
    new_train_y <- train_y
  }
  
  # val_length <- nrow(new_train_x)*0.2-1
  # train_length <- nrow(new_train_x) - val_length
  # 
  # new_val_x <- tail(new_train_x, val_length) %>%
  #   as.matrix()
  # new_val_y <- tail(new_train_y, val_length)%>%
  #   as.matrix()
  # 
  # new_train_x2 <- head(new_train_x, train_length)%>%
  #   as.matrix()
  # new_train_y2 <- head(new_train_y, train_length) %>%
  #   as.matrix()
  
  new_test_x <- subset(test_x, test_x[,"Week"] == week_data_matrix[i+1]) %>%
    as.matrix()
  new_test_y <- subset(test_y, test_x[,"Week"] == week_data_matrix[i+1]) %>%
    as.numeric()
  
  # define final training and testing sets
  # -1: remove Week
  xgb_train <- xgb.DMatrix(data = new_train_x[,-1], label = new_train_y)
  # xgb_val <- xgb.DMatrix(data = new_val_x[,-1], label = new_val_y)
  xgb_test <- xgb.DMatrix(data = new_test_x[,-1], label = new_test_y)
  
  xgb_dat <- list(xgb_train,xgb_test)
  
  return(xgb_dat)
}

xgboost_hosp_dat_mobi <- function(data, nWeek_ahead, i, bins_type,
                                     train_start_yr,train_end_yr, 
                                     test_start_yr, test_end_yr){
  require(dplyr)
  require(xgboost)
  require(data.table)
  
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
      ) %>%
      relocate(nhs_region, .after = 5)
    
    data_train <- data[which(data$Year %in% c(train_start_yr:train_end_yr)), ]
    data_test <- data[which(data$Year %in% c(test_start_yr:test_end_yr)), ]
    
    # define predictor and response variables in training set
    # variables that I want to do one-hot encoding must be in factor class
    # -c(1:2,4): remove Date, Year, hosp
    train_x <- one_hot(dt = data.table::as.data.table(data_train[,-c(1:2,4)]))  %>%
      data.matrix()
    train_y <- data_train[,"hosp_uni"]
    
    # define predictor and response variables in testing set
    test_x <- one_hot(dt = data.table::as.data.table(data_test[,-c(1:2,4)])) %>%
      data.matrix()
    test_y <- data_test[,"hosp_uni"]
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
      ) %>%
      relocate(nhs_region, .after = 5)
    
    data_train <- data[which(data$Year %in% c(train_start_yr:train_end_yr)), ]
    data_test <- data[which(data$Year %in% c(test_start_yr:test_end_yr)), ]
    
    # define predictor and response variables in training set
    # variables that I want to do one-hot encoding must be in factor class
    # -c(1:2,4): remove Date, Year, hosp
    train_x <- one_hot(dt = data.table::as.data.table(data_train[,-c(1:2,4)]))  %>%
      data.matrix()
    train_y <- data_train[,"hosp_quan"]
    
    # define predictor and response variables in testing set
    test_x <- one_hot(dt = data.table::as.data.table(data_test[,-c(1:2,4)])) %>%
      data.matrix()
    test_y <- data_test[,"hosp_quan"]
  }
  
  yearWeek <- unique(data_test[,c("Year", "Week")])
  week_data_matrix <- unique(test_x[,"Week"])
  
  if(i != 0){
    new_train_x <- rbind(train_x, test_x[which(test_x[,"Week"] == week_data_matrix[i]),]) %>%
      as.matrix()
    new_train_y <- append(train_y, test_y[which(test_x[,"Week"] == week_data_matrix[i])]) %>%
      as.numeric()
  }else{
    new_train_x <- train_x
    new_train_y <- train_y
  }
  
  # val_length <- nrow(new_train_x)*0.2-1
  # train_length <- nrow(new_train_x) - val_length
  # 
  # new_val_x <- tail(new_train_x, val_length) %>%
  #   as.matrix()
  # new_val_y <- tail(new_train_y, val_length)%>%
  #   as.matrix()
  # 
  # new_train_x2 <- head(new_train_x, train_length)%>%
  #   as.matrix()
  # new_train_y2 <- head(new_train_y, train_length) %>%
  #   as.matrix()
  
  new_test_x <- subset(test_x, test_x[,"Week"] == week_data_matrix[i+1]) %>%
    as.matrix()
  new_test_y <- subset(test_y, test_x[,"Week"] == week_data_matrix[i+1]) %>%
    as.numeric()
  
  # define final training and testing sets
  # -1: remove Week
  xgb_train <- xgb.DMatrix(data = new_train_x[,-1], label = new_train_y)
  # xgb_val <- xgb.DMatrix(data = new_val_x[,-1], label = new_val_y)
  xgb_test <- xgb.DMatrix(data = new_test_x[,-1], label = new_test_y)
  
  xgb_dat <- list(xgb_train, xgb_test)
  
  return(xgb_dat)
}

xgboost_hosp_dat_weather <- function(data, nWeek_ahead, i, bins_type,
                                     train_start_yr,train_end_yr, 
                                     test_start_yr, test_end_yr){
  require(dplyr)
  require(xgboost)
  require(data.table)
  
  if(bins_type == "uniform"){
    data <- data %>%
      dplyr::select(nhs_region, Date, Year, Week, hosp_uni,
                    paste0("hosplag_uni", nWeek_ahead),
                    paste0("hosplag_uni", nWeek_ahead+1),
                    paste0("caselag", nWeek_ahead),
                    paste0("deathlag", nWeek_ahead),
                    paste0("total.precipitationlag", nWeek_ahead),
                    paste0("temperaturelag", nWeek_ahead)
      ) %>%
      relocate(nhs_region, .after = 5)
    
    data_train <- data[which(data$Year %in% c(train_start_yr:train_end_yr)), ]
    data_test <- data[which(data$Year %in% c(test_start_yr:test_end_yr)), ]
    
    # define predictor and response variables in training set
    # variables that I want to do one-hot encoding must be in factor class
    # -c(1:2,4): remove Date, Year, hosp
    train_x <- one_hot(dt = data.table::as.data.table(data_train[,-c(1:2,4)]))  %>%
      data.matrix()
    train_y <- data_train[,"hosp_uni"]
    
    # define predictor and response variables in testing set
    test_x <- one_hot(dt = data.table::as.data.table(data_test[,-c(1:2,4)])) %>%
      data.matrix()
    test_y <- data_test[,"hosp_uni"]
  }
  
  if(bins_type == "quantile"){
    data <- data %>%
      dplyr::select(nhs_region, Date, Year, Week, hosp_quan,
                    paste0("hosplag_quan", nWeek_ahead),
                    paste0("hosplag_quan", nWeek_ahead+1),
                    paste0("caselag", nWeek_ahead),
                    paste0("deathlag", nWeek_ahead),
                    paste0("total.precipitationlag", nWeek_ahead),
                    paste0("temperaturelag", nWeek_ahead)
      ) %>%
      relocate(nhs_region, .after = 5)
    
    data_train <- data[which(data$Year %in% c(train_start_yr:train_end_yr)), ]
    data_test <- data[which(data$Year %in% c(test_start_yr:test_end_yr)), ]
    
    # define predictor and response variables in training set
    # variables that I want to do one-hot encoding must be in factor class
    # -c(1:2,4): remove Date, Year, hosp
    train_x <- one_hot(dt = data.table::as.data.table(data_train[,-c(1:2,4)]))  %>%
      data.matrix()
    train_y <- data_train[,"hosp_quan"]
    
    # define predictor and response variables in testing set
    test_x <- one_hot(dt = data.table::as.data.table(data_test[,-c(1:2,4)])) %>%
      data.matrix()
    test_y <- data_test[,"hosp_quan"]
  }
  
  yearWeek <- unique(data_test[,c("Year", "Week")])
  week_data_matrix <- unique(test_x[,"Week"])
  
  if(i != 0){
    new_train_x <- rbind(train_x, test_x[which(test_x[,"Week"] == week_data_matrix[i]),]) %>%
      as.matrix()
    new_train_y <- append(train_y, test_y[which(test_x[,"Week"] == week_data_matrix[i])]) %>%
      as.numeric()
  }else{
    new_train_x <- train_x
    new_train_y <- train_y
  }
  
  # val_length <- nrow(new_train_x)*0.2-1
  # train_length <- nrow(new_train_x) - val_length
  # 
  # new_val_x <- tail(new_train_x, val_length) %>%
  #   as.matrix()
  # new_val_y <- tail(new_train_y, val_length)%>%
  #   as.matrix()
  # 
  # new_train_x2 <- head(new_train_x, train_length)%>%
  #   as.matrix()
  # new_train_y2 <- head(new_train_y, train_length) %>%
  #   as.matrix()
  
  new_test_x <- subset(test_x, test_x[,"Week"] == week_data_matrix[i+1]) %>%
    as.matrix()
  new_test_y <- subset(test_y, test_x[,"Week"] == week_data_matrix[i+1]) %>%
    as.numeric()
  
  # define final training and testing sets
  # -1: remove Week
  xgb_train <- xgb.DMatrix(data = new_train_x[,-1], label = new_train_y)
  # xgb_val <- xgb.DMatrix(data = new_val_x[,-1], label = new_val_y)
  xgb_test <- xgb.DMatrix(data = new_test_x[,-1], label = new_test_y)
  
  xgb_dat <- list(xgb_train, xgb_test)
  
  return(xgb_dat)
}

xgboost_hosp_dat_mobiwea <- function(data, nWeek_ahead, i, bins_type,
                           train_start_yr,train_end_yr, 
                           test_start_yr, test_end_yr){
  require(dplyr)
  require(xgboost)
  require(data.table)
  
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
                    paste0("residentiallag", nWeek_ahead),
                    paste0("total.precipitationlag", nWeek_ahead),
                    paste0("temperaturelag", nWeek_ahead)
      ) %>%
      relocate(nhs_region, .after = 5)
    
    data_train <- data[which(data$Year %in% c(train_start_yr:train_end_yr)), ]
    data_test <- data[which(data$Year %in% c(test_start_yr:test_end_yr)), ]
    
    # define predictor and response variables in training set
    # variables that I want to do one-hot encoding must be in factor class
    # -c(1:2,4): remove Date, Year, hosp
    train_x <- one_hot(dt = data.table::as.data.table(data_train[,-c(1:2,4)]))  %>%
      data.matrix()
    train_y <- data_train[,"hosp_uni"]
    
    # define predictor and response variables in testing set
    test_x <- one_hot(dt = data.table::as.data.table(data_test[,-c(1:2,4)])) %>%
      data.matrix()
    test_y <- data_test[,"hosp_uni"]
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
                    paste0("residentiallag", nWeek_ahead),
                    paste0("total.precipitationlag", nWeek_ahead),
                    paste0("temperaturelag", nWeek_ahead)
      ) %>%
      relocate(nhs_region, .after = 5)

    data_train <- data[which(data$Year %in% c(train_start_yr:train_end_yr)), ]
    data_test <- data[which(data$Year %in% c(test_start_yr:test_end_yr)), ]
    
    # define predictor and response variables in training set
    # variables that I want to do one-hot encoding must be in factor class
    # -c(1:2,4): remove Date, Year, hosp
    train_x <- one_hot(dt = data.table::as.data.table(data_train[,-c(1:2,4)]))  %>%
      data.matrix()
    train_y <- data_train[,"hosp_quan"]
    
    # define predictor and response variables in testing set
    test_x <- one_hot(dt = data.table::as.data.table(data_test[,-c(1:2,4)])) %>%
      data.matrix()
    test_y <- data_test[,"hosp_quan"]
  }
  
  yearWeek <- unique(data_test[,c("Year", "Week")])
  week_data_matrix <- unique(test_x[,"Week"])

  if(i != 0){
    new_train_x <- rbind(train_x, test_x[which(test_x[,"Week"] == week_data_matrix[i]),]) %>%
      as.matrix()
    new_train_y <- append(train_y, test_y[which(test_x[,"Week"] == week_data_matrix[i])]) %>%
      as.numeric()
  }else{
    new_train_x <- train_x
    new_train_y <- train_y
  }
  
  # val_length <- nrow(new_train_x)*0.2-1
  # train_length <- nrow(new_train_x) - val_length
  # 
  # new_val_x <- tail(new_train_x, val_length) %>%
  #   as.matrix()
  # new_val_y <- tail(new_train_y, val_length)%>%
  #   as.matrix()
  # 
  # new_train_x2 <- head(new_train_x, train_length)%>%
  #   as.matrix()
  # new_train_y2 <- head(new_train_y, train_length) %>%
  #   as.matrix()
  
  new_test_x <- subset(test_x, test_x[,"Week"] == week_data_matrix[i+1]) %>%
    as.matrix()
  new_test_y <- subset(test_y, test_x[,"Week"] == week_data_matrix[i+1]) %>%
    as.numeric()

  # define final training and testing sets
  # -1: remove Week
  xgb_train <- xgb.DMatrix(data = new_train_x[,-1], label = new_train_y)
  # xgb_val <- xgb.DMatrix(data = new_val_x[,-1], label = new_val_y)
  xgb_test <- xgb.DMatrix(data = new_test_x[,-1], label = new_test_y)
  
  xgb_dat <- list(xgb_train, xgb_test)
  
  return(xgb_dat)
}

#' test xgboost_hosp_dat function
test_xgb_hosp_dat <- xgboost_hosp_dat(df_model_10bins, 1, 1, "quantile",
                                      2020, 2021, 2022, 2022)


#' run xgboost model and make predictions

#' SoftmaxMultiClassObj: label must be in [0, num_class)
#' Therefore, I need to do level-1 to convert levels between 0-9 
xgboost_nhsRegion_pred <- function(data, nWeek_ahead,bins_type, 
                                   features, params_list, 
                                   train_start_yr,train_end_yr,
                                   test_start_yr, test_end_yr){
  data <- data %>%
    arrange(Year, Week)
  
  if(bins_type == "uniform"){
    data <- data %>%
      mutate(hosp_uni = as.integer(as.character(hosp_uni))-1,
             hosplag_uni1 = hosplag_uni1-1,
             hosplag_uni2 = hosplag_uni2-1,
             hosplag_uni3 = hosplag_uni3-1,
             hosplag_uni4 = hosplag_uni4-1,
             hosplag_uni5 = hosplag_uni5-1)
    
    data_train <- data[which(data$Year %in% c(train_start_yr:train_end_yr)), ]
    data_test <- data[which(data$Year %in% c(test_start_yr:test_end_yr)), ]
    
    test_weeknum <- dim(unique(data_test[,c("Year", "Week")]))[1]
    
    pred_mat <- data.frame(Year = data_test$Year,
                           Week = data_test$Week,
                           hosp_uni = data_test$hosp_uni+1,
                           nhs_region = data_test$nhs_region,
                           stringsAsFactors=FALSE)
    
  }
  
  if(bins_type == "quantile") {
    data <- data %>%
      mutate(hosp_quan = as.integer(as.character(hosp_quan))-1,
             hosplag_quan1 = hosplag_quan1-1,
             hosplag_quan2 = hosplag_quan2-1,
             hosplag_quan3 = hosplag_quan3-1,
             hosplag_quan4 = hosplag_quan4-1,
             hosplag_quan5 = hosplag_quan5-1)
    
    data_train <- data[which(data$Year %in% c(train_start_yr:train_end_yr)), ]
    data_test <- data[which(data$Year %in% c(test_start_yr:test_end_yr)), ]
    
    test_weeknum <- dim(unique(data_test[,c("Year", "Week")]))[1]
    
    pred_mat <- data.frame(Year = data_test$Year,
                           Week = data_test$Week,
                           hosp_quan = data_test$hosp_quan+1,
                           nhs_region = data_test$nhs_region,
                           stringsAsFactors=FALSE)
  }
  
  pred <- NULL

  for (i in 0:(test_weeknum-1)){
    if (features == "epi"){
      xgb_dat <- xgboost_hosp_dat_epi(data, nWeek_ahead, i, bins_type,
                                  train_start_yr,train_end_yr,
                                  test_start_yr, test_end_yr) 
    } else if(features == "mobility"){
      xgb_dat <- xgboost_hosp_dat_mobi(data, nWeek_ahead, i, bins_type,
                                      train_start_yr,train_end_yr,
                                      test_start_yr, test_end_yr)
    } else if (features == "weather"){
      xgb_dat <- xgboost_hosp_dat_weather(data, nWeek_ahead, i, bins_type,
                                       train_start_yr,train_end_yr,
                                       test_start_yr, test_end_yr)
    }else if(features == "mobi+weather"){
      xgb_dat <- xgboost_hosp_dat_mobiwea(data, nWeek_ahead, i, bins_type,
                                          train_start_yr,train_end_yr,
                                          test_start_yr, test_end_yr)
    }
    
    xgb_train <- xgb_dat[[1]]
    # xgb_val <- xgb_dat[[2]]
    xgb_test <- xgb_dat[[2]]
    
    # watchlist <- list(train = xgb_train, test = xgb_val)

    set.seed(999)
    xgb_model <- xgb.train(params = params_list, data = xgb_train, 
                           nrounds = 200)
    
    xgb_pred <- predict(xgb_model, newdata = xgb_test, reshape = TRUE)
    
    pred <- rbind(pred, xgb_pred)
  }

  # pred <- pred %>% as.data.frame()%>% mutate(pred_level = max.col(.,ties.method = "last"))
  # pred_mat <- cbind(pred_mat, pred$pred_level) 
  
  pred_level <- apply(pred, 1, which.max)
  pred_mat <- cbind(pred_mat, pred_level)
  names(pred_mat)[names(pred_mat) == "pred_level"] <- paste0(nWeek_ahead,"week_pred")
  
  return(pred_mat)
}


# xgb_null_params <-readRDS("./saved_objects/para_nullXGBoost_final.rds")

############### 1,2,3,4-week ahead prediction ##############
xgb_params_multi <- readRDS("./saved_objects/hyperpara_list_final.rds")

########## mobi+weather ##########
#' number of bins = 3
xgb_params_3bins <- xgb_params_multi
xgb_params_3bins$num_class <- 3

xgb_pred_quan3bins_1week <- xgboost_nhsRegion_pred(df_model_3bins, 1,"quantile", 
                                                   "mobi+weather",xgb_params_3bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan3bins_2week <- xgboost_nhsRegion_pred(df_model_3bins, 2,"quantile",
                                                   "mobi+weather",xgb_params_3bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan3bins_3week <- xgboost_nhsRegion_pred(df_model_3bins, 3,"quantile", 
                                                   "mobi+weather",xgb_params_3bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan3bins_4week <- xgboost_nhsRegion_pred(df_model_3bins, 4,"quantile",
                                                   "mobi+weather",xgb_params_3bins,
                                                   2020,2021,2022,2022)

xgb_pred_uni3bins_1week <- xgboost_nhsRegion_pred(df_model_3bins, 1,"uniform", 
                                                  "mobi+weather",xgb_params_3bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni3bins_2week <- xgboost_nhsRegion_pred(df_model_3bins, 2,"uniform", 
                                                  "mobi+weather",xgb_params_3bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni3bins_3week <- xgboost_nhsRegion_pred(df_model_3bins, 3,"uniform", 
                                                  "mobi+weather",xgb_params_3bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni3bins_4week <- xgboost_nhsRegion_pred(df_model_3bins, 4,"uniform", 
                                                  "mobi+weather",xgb_params_3bins,
                                                  2020,2021,2022,2022)


#' number of bins = 5
xgb_params_5bins <- xgb_params_multi
xgb_params_5bins$num_class <- 5

xgb_pred_quan5bins_1week <- xgboost_nhsRegion_pred(df_model_5bins, 1,"quantile",
                                                   "mobi+weather",xgb_params_5bins,
                                                  2020,2021,2022,2022)
xgb_pred_quan5bins_2week <- xgboost_nhsRegion_pred(df_model_5bins, 2,"quantile",
                                                   "mobi+weather",xgb_params_5bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan5bins_3week <- xgboost_nhsRegion_pred(df_model_5bins, 3,"quantile",
                                                   "mobi+weather",xgb_params_5bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan5bins_4week <- xgboost_nhsRegion_pred(df_model_5bins, 4,"quantile",
                                                   "mobi+weather",xgb_params_5bins,
                                                   2020,2021,2022,2022)

xgb_pred_uni5bins_1week <- xgboost_nhsRegion_pred(df_model_5bins, 1,"uniform", 
                                                  "mobi+weather",xgb_params_5bins,
                                                   2020,2021,2022,2022)
xgb_pred_uni5bins_2week <- xgboost_nhsRegion_pred(df_model_5bins, 2,"uniform",
                                                  "mobi+weather",xgb_params_5bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni5bins_3week <- xgboost_nhsRegion_pred(df_model_5bins, 3,"uniform", 
                                                  "mobi+weather",xgb_params_5bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni5bins_4week <- xgboost_nhsRegion_pred(df_model_5bins, 4,"uniform",
                                                  "mobi+weather",xgb_params_5bins,
                                                  2020,2021,2022,2022)

#' number of bins = 10
xgb_params_10bins <- xgb_params_multi
xgb_params_10bins$num_class <- 10

#' make predictions for 1 to 4-week ahead prediction
xgb_pred_quan10bins_1week <- xgboost_nhsRegion_pred(df_model_10bins, 1,"quantile", 
                                                    "mobi+weather",xgb_params_10bins,
                                                     2020,2021,2022,2022)
xgb_pred_quan10bins_2week <- xgboost_nhsRegion_pred(df_model_10bins, 2, "quantile",
                                                    "mobi+weather",xgb_params_10bins,
                                                     2020,2021,2022,2022)
xgb_pred_quan10bins_3week <- xgboost_nhsRegion_pred(df_model_10bins, 3,"quantile", 
                                                    "mobi+weather",xgb_params_10bins,
                                                     2020,2021,2022,2022)
xgb_pred_quan10bins_4week <- xgboost_nhsRegion_pred(df_model_10bins, 4,"quantile",
                                                    "mobi+weather",xgb_params_10bins,
                                                     2020,2021,2022,2022)

xgb_pred_uni10bins_1week <- xgboost_nhsRegion_pred(df_model_10bins, 1,"uniform", 
                                                   "mobi+weather",xgb_params_10bins,
                                                    2020,2021,2022,2022)
xgb_pred_uni10bins_2week <- xgboost_nhsRegion_pred(df_model_10bins, 2, "uniform",
                                                   "mobi+weather",xgb_params_10bins,
                                                    2020,2021,2022,2022)
xgb_pred_uni10bins_3week <- xgboost_nhsRegion_pred(df_model_10bins, 3,"uniform", 
                                                   "mobi+weather",xgb_params_10bins,
                                                    2020,2021,2022,2022)
xgb_pred_uni10bins_4week <- xgboost_nhsRegion_pred(df_model_10bins, 4,"uniform", 
                                                   "mobi+weather",xgb_params_10bins,
                                                   2020,2021,2022,2022)

#' Merge 1-4 week ahead predictions into one data frame
xgb_pred_quan3bins_all <- full_join(xgb_pred_quan3bins_1week, xgb_pred_quan3bins_2week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan3bins_3week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan3bins_4week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_pred_uni3bins_all <- full_join(xgb_pred_uni3bins_1week, xgb_pred_uni3bins_2week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni3bins_3week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni3bins_4week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_pred_quan5bins_all <- full_join(xgb_pred_quan5bins_1week, xgb_pred_quan5bins_2week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan5bins_3week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan5bins_4week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_pred_quan10bins_all <- full_join(xgb_pred_quan10bins_1week, xgb_pred_quan10bins_2week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan10bins_3week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan10bins_4week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_pred_uni5bins_all <- full_join(xgb_pred_uni5bins_1week, xgb_pred_uni5bins_2week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni5bins_3week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni5bins_4week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_pred_uni10bins_all <- full_join(xgb_pred_uni10bins_1week, xgb_pred_uni10bins_2week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni10bins_3week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni10bins_4week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_pred_all_null <- list(xgb_pred_quan3bins_all = xgb_pred_quan3bins_all,
                          xgb_pred_uni3bins_all = xgb_pred_uni3bins_all,
                          xgb_pred_quan5bins_all = xgb_pred_quan5bins_all,
                          xgb_pred_quan10bins_all = xgb_pred_quan10bins_all,
                          xgb_pred_uni5bins_all = xgb_pred_uni5bins_all,
                          xgb_pred_uni10bins_all = xgb_pred_uni10bins_all)

saveRDS(xgb_pred_all_null, "./saved_objects/xgb_pred_all_null_mobiwea.rds") # epi + mobility + weather features


########## epi ##########
#' number of bins = 3
xgb_params_3bins <- xgb_params_multi
xgb_params_3bins$num_class <- 3

xgb_pred_quan3bins_1week <- xgboost_nhsRegion_pred(df_model_3bins, 1,"quantile", 
                                                   "epi",xgb_params_3bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan3bins_2week <- xgboost_nhsRegion_pred(df_model_3bins, 2,"quantile",
                                                   "epi",xgb_params_3bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan3bins_3week <- xgboost_nhsRegion_pred(df_model_3bins, 3,"quantile", 
                                                   "epi",xgb_params_3bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan3bins_4week <- xgboost_nhsRegion_pred(df_model_3bins, 4,"quantile",
                                                   "epi",xgb_params_3bins,
                                                   2020,2021,2022,2022)

xgb_pred_uni3bins_1week <- xgboost_nhsRegion_pred(df_model_3bins, 1,"uniform", 
                                                  "epi",xgb_params_3bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni3bins_2week <- xgboost_nhsRegion_pred(df_model_3bins, 2,"uniform", 
                                                  "epi",xgb_params_3bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni3bins_3week <- xgboost_nhsRegion_pred(df_model_3bins, 3,"uniform", 
                                                  "epi",xgb_params_3bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni3bins_4week <- xgboost_nhsRegion_pred(df_model_3bins, 4,"uniform", 
                                                  "epi",xgb_params_3bins,
                                                  2020,2021,2022,2022)


#' number of bins = 5
xgb_params_5bins <- xgb_params_multi
xgb_params_5bins$num_class <- 5

xgb_pred_quan5bins_1week <- xgboost_nhsRegion_pred(df_model_5bins, 1,"quantile",
                                                   "epi",xgb_params_5bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan5bins_2week <- xgboost_nhsRegion_pred(df_model_5bins, 2,"quantile",
                                                   "epi",xgb_params_5bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan5bins_3week <- xgboost_nhsRegion_pred(df_model_5bins, 3,"quantile",
                                                   "epi",xgb_params_5bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan5bins_4week <- xgboost_nhsRegion_pred(df_model_5bins, 4,"quantile",
                                                   "epi",xgb_params_5bins,
                                                   2020,2021,2022,2022)

xgb_pred_uni5bins_1week <- xgboost_nhsRegion_pred(df_model_5bins, 1,"uniform", 
                                                  "epi",xgb_params_5bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni5bins_2week <- xgboost_nhsRegion_pred(df_model_5bins, 2,"uniform",
                                                  "epi",xgb_params_5bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni5bins_3week <- xgboost_nhsRegion_pred(df_model_5bins, 3,"uniform", 
                                                  "epi",xgb_params_5bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni5bins_4week <- xgboost_nhsRegion_pred(df_model_5bins, 4,"uniform",
                                                  "epi",xgb_params_5bins,
                                                  2020,2021,2022,2022)

#' number of bins = 10
xgb_params_10bins <- xgb_params_multi
xgb_params_10bins$num_class <- 10

#' make predictions for 1 to 4-week ahead prediction
xgb_pred_quan10bins_1week <- xgboost_nhsRegion_pred(df_model_10bins, 1,"quantile", 
                                                    "epi",xgb_params_10bins,
                                                    2020,2021,2022,2022)
xgb_pred_quan10bins_2week <- xgboost_nhsRegion_pred(df_model_10bins, 2, "quantile",
                                                    "epi",xgb_params_10bins,
                                                    2020,2021,2022,2022)
xgb_pred_quan10bins_3week <- xgboost_nhsRegion_pred(df_model_10bins, 3,"quantile", 
                                                    "epi",xgb_params_10bins,
                                                    2020,2021,2022,2022)
xgb_pred_quan10bins_4week <- xgboost_nhsRegion_pred(df_model_10bins, 4,"quantile",
                                                    "epi",xgb_params_10bins,
                                                    2020,2021,2022,2022)

xgb_pred_uni10bins_1week <- xgboost_nhsRegion_pred(df_model_10bins, 1,"uniform", 
                                                   "epi",xgb_params_10bins,
                                                   2020,2021,2022,2022)
xgb_pred_uni10bins_2week <- xgboost_nhsRegion_pred(df_model_10bins, 2, "uniform",
                                                   "epi",xgb_params_10bins,
                                                   2020,2021,2022,2022)
xgb_pred_uni10bins_3week <- xgboost_nhsRegion_pred(df_model_10bins, 3,"uniform", 
                                                   "epi",xgb_params_10bins,
                                                   2020,2021,2022,2022)
xgb_pred_uni10bins_4week <- xgboost_nhsRegion_pred(df_model_10bins, 4,"uniform", 
                                                   "epi",xgb_params_10bins,
                                                   2020,2021,2022,2022)

#' Merge 1-4 week ahead predictions into one data frame
xgb_pred_quan3bins_all <- full_join(xgb_pred_quan3bins_1week, xgb_pred_quan3bins_2week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan3bins_3week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan3bins_4week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_pred_uni3bins_all <- full_join(xgb_pred_uni3bins_1week, xgb_pred_uni3bins_2week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni3bins_3week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni3bins_4week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_pred_quan5bins_all <- full_join(xgb_pred_quan5bins_1week, xgb_pred_quan5bins_2week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan5bins_3week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan5bins_4week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_pred_quan10bins_all <- full_join(xgb_pred_quan10bins_1week, xgb_pred_quan10bins_2week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan10bins_3week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan10bins_4week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_pred_uni5bins_all <- full_join(xgb_pred_uni5bins_1week, xgb_pred_uni5bins_2week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni5bins_3week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni5bins_4week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_pred_uni10bins_all <- full_join(xgb_pred_uni10bins_1week, xgb_pred_uni10bins_2week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni10bins_3week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni10bins_4week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_pred_all_null <- list(xgb_pred_quan3bins_all = xgb_pred_quan3bins_all,
                          xgb_pred_uni3bins_all = xgb_pred_uni3bins_all,
                          xgb_pred_quan5bins_all = xgb_pred_quan5bins_all,
                          xgb_pred_quan10bins_all = xgb_pred_quan10bins_all,
                          xgb_pred_uni5bins_all = xgb_pred_uni5bins_all,
                          xgb_pred_uni10bins_all = xgb_pred_uni10bins_all)

saveRDS(xgb_pred_all_null, "./saved_objects/xgb_pred_all_null_epi.rds") # only epi features

######### mobi ###########
#' number of bins = 3
xgb_params_3bins <- xgb_params_multi
xgb_params_3bins$num_class <- 3

xgb_pred_quan3bins_1week <- xgboost_nhsRegion_pred(df_model_3bins, 1,"quantile", 
                                                   "weather",xgb_params_3bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan3bins_2week <- xgboost_nhsRegion_pred(df_model_3bins, 2,"quantile",
                                                   "mobility",xgb_params_3bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan3bins_3week <- xgboost_nhsRegion_pred(df_model_3bins, 3,"quantile", 
                                                   "mobility",xgb_params_3bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan3bins_4week <- xgboost_nhsRegion_pred(df_model_3bins, 4,"quantile",
                                                   "mobility",xgb_params_3bins,
                                                   2020,2021,2022,2022)

xgb_pred_uni3bins_1week <- xgboost_nhsRegion_pred(df_model_3bins, 1,"uniform", 
                                                  "mobility",xgb_params_3bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni3bins_2week <- xgboost_nhsRegion_pred(df_model_3bins, 2,"uniform", 
                                                  "mobility",xgb_params_3bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni3bins_3week <- xgboost_nhsRegion_pred(df_model_3bins, 3,"uniform", 
                                                  "mobility",xgb_params_3bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni3bins_4week <- xgboost_nhsRegion_pred(df_model_3bins, 4,"uniform", 
                                                  "mobility",xgb_params_3bins,
                                                  2020,2021,2022,2022)


#' number of bins = 5
xgb_params_5bins <- xgb_params_multi
xgb_params_5bins$num_class <- 5

xgb_pred_quan5bins_1week <- xgboost_nhsRegion_pred(df_model_5bins, 1,"quantile",
                                                   "mobility",xgb_params_5bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan5bins_2week <- xgboost_nhsRegion_pred(df_model_5bins, 2,"quantile",
                                                   "mobility",xgb_params_5bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan5bins_3week <- xgboost_nhsRegion_pred(df_model_5bins, 3,"quantile",
                                                   "mobility",xgb_params_5bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan5bins_4week <- xgboost_nhsRegion_pred(df_model_5bins, 4,"quantile",
                                                   "mobility",xgb_params_5bins,
                                                   2020,2021,2022,2022)

xgb_pred_uni5bins_1week <- xgboost_nhsRegion_pred(df_model_5bins, 1,"uniform", 
                                                  "mobility",xgb_params_5bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni5bins_2week <- xgboost_nhsRegion_pred(df_model_5bins, 2,"uniform",
                                                  "mobility",xgb_params_5bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni5bins_3week <- xgboost_nhsRegion_pred(df_model_5bins, 3,"uniform", 
                                                  "mobility",xgb_params_5bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni5bins_4week <- xgboost_nhsRegion_pred(df_model_5bins, 4,"uniform",
                                                  "mobility",xgb_params_5bins,
                                                  2020,2021,2022,2022)

#' number of bins = 10
xgb_params_10bins <- xgb_params_multi
xgb_params_10bins$num_class <- 10

#' make predictions for 1 to 4-week ahead prediction
xgb_pred_quan10bins_1week <- xgboost_nhsRegion_pred(df_model_10bins, 1,"quantile", 
                                                    "mobility",xgb_params_10bins,
                                                    2020,2021,2022,2022)
xgb_pred_quan10bins_2week <- xgboost_nhsRegion_pred(df_model_10bins, 2, "quantile",
                                                    "mobility",xgb_params_10bins,
                                                    2020,2021,2022,2022)
xgb_pred_quan10bins_3week <- xgboost_nhsRegion_pred(df_model_10bins, 3,"quantile", 
                                                    "mobility",xgb_params_10bins,
                                                    2020,2021,2022,2022)
xgb_pred_quan10bins_4week <- xgboost_nhsRegion_pred(df_model_10bins, 4,"quantile",
                                                    "mobility",xgb_params_10bins,
                                                    2020,2021,2022,2022)

xgb_pred_uni10bins_1week <- xgboost_nhsRegion_pred(df_model_10bins, 1,"uniform", 
                                                   "mobility",xgb_params_10bins,
                                                   2020,2021,2022,2022)
xgb_pred_uni10bins_2week <- xgboost_nhsRegion_pred(df_model_10bins, 2, "uniform",
                                                   "mobility",xgb_params_10bins,
                                                   2020,2021,2022,2022)
xgb_pred_uni10bins_3week <- xgboost_nhsRegion_pred(df_model_10bins, 3,"uniform", 
                                                   "mobility",xgb_params_10bins,
                                                   2020,2021,2022,2022)
xgb_pred_uni10bins_4week <- xgboost_nhsRegion_pred(df_model_10bins, 4,"uniform", 
                                                   "mobility",xgb_params_10bins,
                                                   2020,2021,2022,2022)

#' Merge 1-4 week ahead predictions into one data frame
xgb_pred_quan3bins_all <- full_join(xgb_pred_quan3bins_1week, xgb_pred_quan3bins_2week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan3bins_3week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan3bins_4week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_pred_uni3bins_all <- full_join(xgb_pred_uni3bins_1week, xgb_pred_uni3bins_2week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni3bins_3week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni3bins_4week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_pred_quan5bins_all <- full_join(xgb_pred_quan5bins_1week, xgb_pred_quan5bins_2week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan5bins_3week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan5bins_4week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_pred_quan10bins_all <- full_join(xgb_pred_quan10bins_1week, xgb_pred_quan10bins_2week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan10bins_3week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan10bins_4week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_pred_uni5bins_all <- full_join(xgb_pred_uni5bins_1week, xgb_pred_uni5bins_2week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni5bins_3week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni5bins_4week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_pred_uni10bins_all <- full_join(xgb_pred_uni10bins_1week, xgb_pred_uni10bins_2week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni10bins_3week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni10bins_4week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_pred_all_null <- list(xgb_pred_quan3bins_all = xgb_pred_quan3bins_all,
                          xgb_pred_uni3bins_all = xgb_pred_uni3bins_all,
                          xgb_pred_quan5bins_all = xgb_pred_quan5bins_all,
                          xgb_pred_quan10bins_all = xgb_pred_quan10bins_all,
                          xgb_pred_uni5bins_all = xgb_pred_uni5bins_all,
                          xgb_pred_uni10bins_all = xgb_pred_uni10bins_all)
saveRDS(xgb_pred_all_null, "./saved_objects/xgb_pred_all_null_mobi.rds") # epi + mobility features

########## weather ##########
#' number of bins = 3
xgb_params_3bins <- xgb_params_multi
xgb_params_3bins$num_class <- 3

xgb_pred_quan3bins_1week <- xgboost_nhsRegion_pred(df_model_3bins, 1,"quantile", 
                                                   "weather",xgb_params_3bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan3bins_2week <- xgboost_nhsRegion_pred(df_model_3bins, 2,"quantile",
                                                   "weather",xgb_params_3bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan3bins_3week <- xgboost_nhsRegion_pred(df_model_3bins, 3,"quantile", 
                                                   "weather",xgb_params_3bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan3bins_4week <- xgboost_nhsRegion_pred(df_model_3bins, 4,"quantile",
                                                   "weather",xgb_params_3bins,
                                                   2020,2021,2022,2022)

xgb_pred_uni3bins_1week <- xgboost_nhsRegion_pred(df_model_3bins, 1,"uniform", 
                                                  "weather",xgb_params_3bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni3bins_2week <- xgboost_nhsRegion_pred(df_model_3bins, 2,"uniform", 
                                                  "weather",xgb_params_3bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni3bins_3week <- xgboost_nhsRegion_pred(df_model_3bins, 3,"uniform", 
                                                  "weather",xgb_params_3bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni3bins_4week <- xgboost_nhsRegion_pred(df_model_3bins, 4,"uniform", 
                                                  "weather",xgb_params_3bins,
                                                  2020,2021,2022,2022)


#' number of bins = 5
xgb_params_5bins <- xgb_params_multi
xgb_params_5bins$num_class <- 5

xgb_pred_quan5bins_1week <- xgboost_nhsRegion_pred(df_model_5bins, 1,"quantile",
                                                   "weather",xgb_params_5bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan5bins_2week <- xgboost_nhsRegion_pred(df_model_5bins, 2,"quantile",
                                                   "weather",xgb_params_5bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan5bins_3week <- xgboost_nhsRegion_pred(df_model_5bins, 3,"quantile",
                                                   "weather",xgb_params_5bins,
                                                   2020,2021,2022,2022)
xgb_pred_quan5bins_4week <- xgboost_nhsRegion_pred(df_model_5bins, 4,"quantile",
                                                   "weather",xgb_params_5bins,
                                                   2020,2021,2022,2022)

xgb_pred_uni5bins_1week <- xgboost_nhsRegion_pred(df_model_5bins, 1,"uniform", 
                                                  "weather",xgb_params_5bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni5bins_2week <- xgboost_nhsRegion_pred(df_model_5bins, 2,"uniform",
                                                  "weather",xgb_params_5bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni5bins_3week <- xgboost_nhsRegion_pred(df_model_5bins, 3,"uniform", 
                                                  "weather",xgb_params_5bins,
                                                  2020,2021,2022,2022)
xgb_pred_uni5bins_4week <- xgboost_nhsRegion_pred(df_model_5bins, 4,"uniform",
                                                  "weather",xgb_params_5bins,
                                                  2020,2021,2022,2022)

#' number of bins = 10
xgb_params_10bins <- xgb_params_multi
xgb_params_10bins$num_class <- 10

#' make predictions for 1 to 4-week ahead prediction
xgb_pred_quan10bins_1week <- xgboost_nhsRegion_pred(df_model_10bins, 1,"quantile", 
                                                    "weather",xgb_params_10bins,
                                                    2020,2021,2022,2022)
xgb_pred_quan10bins_2week <- xgboost_nhsRegion_pred(df_model_10bins, 2, "quantile",
                                                    "weather",xgb_params_10bins,
                                                    2020,2021,2022,2022)
xgb_pred_quan10bins_3week <- xgboost_nhsRegion_pred(df_model_10bins, 3,"quantile", 
                                                    "weather",xgb_params_10bins,
                                                    2020,2021,2022,2022)
xgb_pred_quan10bins_4week <- xgboost_nhsRegion_pred(df_model_10bins, 4,"quantile",
                                                    "weather",xgb_params_10bins,
                                                    2020,2021,2022,2022)

xgb_pred_uni10bins_1week <- xgboost_nhsRegion_pred(df_model_10bins, 1,"uniform", 
                                                   "weather",xgb_params_10bins,
                                                   2020,2021,2022,2022)
xgb_pred_uni10bins_2week <- xgboost_nhsRegion_pred(df_model_10bins, 2, "uniform",
                                                   "weather",xgb_params_10bins,
                                                   2020,2021,2022,2022)
xgb_pred_uni10bins_3week <- xgboost_nhsRegion_pred(df_model_10bins, 3,"uniform", 
                                                   "weather",xgb_params_10bins,
                                                   2020,2021,2022,2022)
xgb_pred_uni10bins_4week <- xgboost_nhsRegion_pred(df_model_10bins, 4,"uniform", 
                                                   "weather",xgb_params_10bins,
                                                   2020,2021,2022,2022)

#' Merge 1-4 week ahead predictions into one data frame
xgb_pred_quan3bins_all <- full_join(xgb_pred_quan3bins_1week, xgb_pred_quan3bins_2week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan3bins_3week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan3bins_4week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_pred_uni3bins_all <- full_join(xgb_pred_uni3bins_1week, xgb_pred_uni3bins_2week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni3bins_3week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni3bins_4week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_pred_quan5bins_all <- full_join(xgb_pred_quan5bins_1week, xgb_pred_quan5bins_2week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan5bins_3week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan5bins_4week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_pred_quan10bins_all <- full_join(xgb_pred_quan10bins_1week, xgb_pred_quan10bins_2week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan10bins_3week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_pred_quan10bins_4week, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_pred_uni5bins_all <- full_join(xgb_pred_uni5bins_1week, xgb_pred_uni5bins_2week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni5bins_3week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni5bins_4week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_pred_uni10bins_all <- full_join(xgb_pred_uni10bins_1week, xgb_pred_uni10bins_2week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni10bins_3week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_pred_uni10bins_4week, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_pred_all_null <- list(xgb_pred_quan3bins_all = xgb_pred_quan3bins_all,
                          xgb_pred_uni3bins_all = xgb_pred_uni3bins_all,
                          xgb_pred_quan5bins_all = xgb_pred_quan5bins_all,
                          xgb_pred_quan10bins_all = xgb_pred_quan10bins_all,
                          xgb_pred_uni5bins_all = xgb_pred_uni5bins_all,
                          xgb_pred_uni10bins_all = xgb_pred_uni10bins_all)

saveRDS(xgb_pred_all_null, "./saved_objects/xgb_pred_all_null_wea.rds") # epi + weather features




####################### Null model #################
null_model <- function(data, bins_type,test_start_yr, test_end_yr){
  
  if(bins_type == "uniform"){
    pred <- data[which(data$Year %in% c(test_start_yr:test_end_yr)), ] %>%
      dplyr::select(nhs_region, Year, Week, hosp_uni,
                    hosplag_uni1, hosplag_uni2, hosplag_uni3, hosplag_uni4)
    pred <- pred %>%
      relocate(nhs_region, .after = 3)
    
    colnames(pred) <- c("Year", "Week", "nhs_region", "hosp_uni",
                        "1week_pred", "2week_pred", "3week_pred", "4week_pred")
  }
  
  if(bins_type == "quantile"){
    pred <- data[which(data$Year %in% c(test_start_yr:test_end_yr)), ] %>%
      dplyr::select(nhs_region, Year, Week, hosp_quan,
                    hosplag_quan1, hosplag_quan2, hosplag_quan3, hosplag_quan4)
    pred <- pred %>%
      relocate(nhs_region, .after = 3)
    
    colnames(pred) <- c("Year", "Week", "nhs_region", "hosp_quan",
                        "1week_pred", "2week_pred", "3week_pred", "4week_pred")
  }

  return(pred)
}

null_model_quan3bins_pred <- null_model(df_model_3bins, "quantile", 2022, 2022)
null_model_uni3bins_pred <- null_model(df_model_3bins, "uniform", 2022, 2022)

null_model_quan5bins_pred <- null_model(df_model_5bins, "quantile", 2022, 2022)
null_model_quan10bins_pred <- null_model(df_model_10bins, "quantile", 2022, 2022)

null_model_uni5bins_pred <- null_model(df_model_5bins, "uniform", 2022, 2022)
null_model_uni10bins_pred <- null_model(df_model_10bins, "uniform", 2022, 2022)

null_model_pred <- list(null_model_quan3bins_pred = null_model_quan3bins_pred,
                        null_model_uni3bins_pred = null_model_uni3bins_pred,
                        null_model_quan5bins_pred = null_model_quan5bins_pred,
                        null_model_quan10bins_pred = null_model_quan10bins_pred,
                        null_model_uni5bins_pred = null_model_uni5bins_pred,
                        null_model_uni10bins_pred = null_model_uni10bins_pred)

saveRDS(null_model_pred, "./saved_objects/null_model_pred.rds")


########## test on pathological data #####
null_model_path3bins_pred <- null_model(df_path_3bins, "quantile", 2022, 2022)
null_model_path5bins_pred <- null_model(df_path_5bins, "quantile", 2022, 2022)
null_model_path10bins_pred <- null_model(df_path_10bins, "quantile", 2022, 2022)

null_model_pred_path <- list(null_model_path3bins_pred = null_model_path3bins_pred,
                             null_model_path5bins_pred= null_model_path5bins_pred,
                             null_model_path10bins_pred = null_model_path10bins_pred)
saveRDS(null_model_pred_path, "./saved_objects/null_model_pred_path.rds")
