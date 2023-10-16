#' Make a note of when the report was generated.
Sys.time()
knitr.table.format = "markdown"

#' As always, remove all objects fromt the workspace before starting.
rm(list = ls(all = TRUE))

#' Set your own work path
# setwd()

#' Pull in packages needed
source("./functions/load_packages.R")
source("./functions/data_process.R")

pkgs <- c("haven", "xgboost", "stringr", "rasterVis","hrbrthemes",
          "tidyr", "dplyr", "ggplot2", "aweek", "surveillance", "plyr", 
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


######## ordinal classifier for XGBoost ##########

#' convert into binary outcome - 1: > class; 0: <= class
#' will return a list of data frames with binary outcome
xgb_binary_epi <- function(data, nWeek_ahead, bins_type, class_num){
  require(dplyr)
  require(data.table)
  
  binary_data <- list()
  class <- c(1:(class_num-1))
  
  if(bins_type == "uniform"){
    data <- data %>%
      dplyr::select(nhs_region, Date, Year, Week, hosp_uni,
                    paste0("hosplag_uni", nWeek_ahead),
                    paste0("hosplag_uni", nWeek_ahead+1),
                    paste0("caselag", nWeek_ahead),
                    paste0("deathlag", nWeek_ahead)
                    
      ) %>%
      relocate(nhs_region, .after = 5)
    
    for (i in 1:(length(class))){
      binary_y <- ifelse(data$hosp_uni > class[i], 1, 0)
      
      data_temp <- cbind(data, binary_y) %>%
        dplyr::select(-hosp_uni) 
      # colnames(data_temp)[which(colnames(data_temp) == "binary_y")] <- paste0("binary_y",i)
      
      binary_data[[i]] <- data_temp
    }
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
    
    for (i in 1:(length(class))){
      binary_y <- ifelse(data$hosp_quan > class[i], 1, 0)
      
      data_temp <- cbind(data, binary_y) %>%
        dplyr::select(-hosp_quan) 
      # colnames(data_temp)[which(colnames(data_temp) == "binary_y")] <- paste0("binary_y",i)
      
      binary_data[[i]] <- data_temp
    }
  }
  
  return(binary_data)
}

xgb_binary_mobi <- function(data, nWeek_ahead, bins_type, class_num){
  require(dplyr)
  require(data.table)
  
  binary_data <- list()
  class <- c(1:(class_num-1))
  
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
    
    for (i in 1:(length(class))){
      binary_y <- ifelse(data$hosp_uni > class[i], 1, 0)
      
      data_temp <- cbind(data, binary_y) %>%
        dplyr::select(-hosp_uni) 
      # colnames(data_temp)[which(colnames(data_temp) == "binary_y")] <- paste0("binary_y",i)
      
      binary_data[[i]] <- data_temp
    }
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
    
    for (i in 1:(length(class))){
      binary_y <- ifelse(data$hosp_quan > class[i], 1, 0)
      
      data_temp <- cbind(data, binary_y) %>%
        dplyr::select(-hosp_quan) 
      # colnames(data_temp)[which(colnames(data_temp) == "binary_y")] <- paste0("binary_y",i)
      
      binary_data[[i]] <- data_temp
    }
  }
  
  return(binary_data)
}

xgb_binary_mobiwea <- function(data, nWeek_ahead, bins_type, class_num){
  require(dplyr)
  require(data.table)
  
  binary_data <- list()
  class <- c(1:(class_num-1))
  
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
    
    for (i in 1:(length(class))){
      binary_y <- ifelse(data$hosp_uni > class[i], 1, 0)
      
      data_temp <- cbind(data, binary_y) %>%
        dplyr::select(-hosp_uni) 
      # colnames(data_temp)[which(colnames(data_temp) == "binary_y")] <- paste0("binary_y",i)
      
      binary_data[[i]] <- data_temp
    }
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
    
    for (i in 1:(length(class))){
      binary_y <- ifelse(data$hosp_quan > class[i], 1, 0)
      
      data_temp <- cbind(data, binary_y) %>%
        dplyr::select(-hosp_quan) 
      # colnames(data_temp)[which(colnames(data_temp) == "binary_y")] <- paste0("binary_y",i)
      
      binary_data[[i]] <- data_temp
    }
  }
  
  return(binary_data)
}

xgb_binary_weather <- function(data, nWeek_ahead, bins_type, class_num){
  require(dplyr)
  require(data.table)
  
  binary_data <- list()
  class <- c(1:(class_num-1))
  
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
    
    for (i in 1:(length(class))){
      binary_y <- ifelse(data$hosp_uni > class[i], 1, 0)
      
      data_temp <- cbind(data, binary_y) %>%
        dplyr::select(-hosp_uni) 
      # colnames(data_temp)[which(colnames(data_temp) == "binary_y")] <- paste0("binary_y",i)
      
      binary_data[[i]] <- data_temp
    }
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
    
    for (i in 1:(length(class))){
      binary_y <- ifelse(data$hosp_quan > class[i], 1, 0)
      
      data_temp <- cbind(data, binary_y) %>%
        dplyr::select(-hosp_quan) 
      # colnames(data_temp)[which(colnames(data_temp) == "binary_y")] <- paste0("binary_y",i)
      
      binary_data[[i]] <- data_temp
    }
  }
  
  return(binary_data)
}

# test_xgb_binary5bins <- xgb_binary(df_model_5bins, 1, "uniform")
# test_xgb_binary10bins <- xgb_binary(df_model_10bins, 1, "quantile", 10)

#' data here is a data frame of binary outcome for each class
#' need to extract each data frame from the list 
#' do not use the whole list as the input for data
xgb_binary_data <- function(data, i, 
                            train_start_yr, train_end_yr,
                            test_start_yr, test_end_yr){
  require(dplyr)
  require(xgboost)
  require(data.table)
  
  data_train <- data[which(data$Year %in% c(train_start_yr:train_end_yr)), ]
  data_test <- data[which(data$Year %in% c(test_start_yr:test_end_yr)), ]
  
  # define predictor and response variables in training set
  # variables that I want to do one-hot encoding must be in factor class
  train_x <- data_train %>% 
    dplyr::select(-Date, -Year, -binary_y)
  
  train_x <- one_hot(dt = data.table::as.data.table(train_x))  %>%
    data.matrix()
  train_y <- data_train[,"binary_y"]
  
  # define predictor and response variables in testing set
  test_x <- data_test %>% 
    dplyr::select(-Date, -Year, -binary_y)
  
  test_x <- one_hot(dt = data.table::as.data.table(test_x)) %>%
    data.matrix()
  test_y <- data_test[,"binary_y"]
  
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
  xgb_train <- xgb.DMatrix(data = new_train_x[,-1], label = new_train_y)
  # xgb_val <- xgb.DMatrix(data = new_val_x[,-1], label = new_val_y)
  xgb_test <- xgb.DMatrix(data = new_test_x[,-1], label = new_test_y)
  
  xgb_dat <- list(xgb_train, xgb_test)
  
  return(xgb_dat)
}

# test_xgb_binary_data <- xgb_binary_data(test_xgb_binary10bins[[1]], 1, 2020, 2021, 2022, 2022)

#' train the model and predict probabilities for each class
xgb_signle_class_pred <- function(data, nWeek_ahead, xgb_params,
                                   class,train_start_yr, train_end_yr,
                                   test_start_yr, test_end_yr){
  require(dplyr)
  require(xgboost)
  
  data_train <- data[which(data$Year %in% c(train_start_yr:train_end_yr)), ]
  data_test <- data[which(data$Year %in% c(test_start_yr:test_end_yr)), ]
  
  test_weeknum <- dim(unique(data_test[,c("Year", "Week")]))[1]

  # pred_mat <- data.frame(Year = data_test$Year,
  #                        Week = data_test$Week,
  #                        hosp = data_test$hosp+1,
  #                        nhs_region = data_test$nhs_region,
  #                        stringsAsFactors=FALSE)
  
  pred <- NULL

  for (i in 0:(test_weeknum-1)){
    xgb_dat <- xgb_binary_data(data, i, train_start_yr, train_end_yr,
                               test_start_yr, test_end_yr)
    
    xgb_train <- xgb_dat[[1]]
    # xgb_val <- xgb_dat[[2]]
    xgb_test <- xgb_dat[[2]]
    
    # watchlist <- list(train = xgb_train, test = xgb_val)
    
    set.seed(999)
    xgb_model <- xgb.train(params = xgb_params_order, data = xgb_train, 
                           nrounds = 200)
    
    xgb_pred <- predict(xgb_model, newdata = xgb_test, reshape = TRUE)
    
    pred <- rbind(pred, xgb_pred)
  }

  pred <- as.data.frame(pred)
  colnames(pred) <- levels(data_test$nhs_region)
  
  pred <- pred %>%
    mutate(Class = paste0("level > ", class),
           nWeek_ahead = nWeek_ahead,
           Year = unique(data_test[,"Year"]),
           Week = unique(data_test[,"Week"])) %>%
    relocate(Year, .before = 1) %>%
    relocate(Week, .before = 2)
  
  return(pred)
}

# xgb_params <- list(booster = "gbtree", objective = "reg:logistic")
# test_xgb_train_pred10bins <- xgb_signle_class_pred(test_xgb_binary10bins[[1]], 1, xgb_params,
#                                                   1, 2020, 2021, 2022, 2022)

#' calculate probabilities for each class
xgb_ordered3bins_pred <- function(data, raw_data, nWeek_ahead,bins_type, xgb_params,
                                  class,train_start_yr, train_end_yr,
                                  test_start_yr, test_end_yr){
  require(dplyr)
  
  test_df <- raw_data %>%
    arrange(Year, Week) %>%
    filter(Year == 2022)
  
  df_pred_all <- NULL
  
  for (i in 1:length(class)){
    df_pred_single <- xgb_signle_class_pred(data[[i]], nWeek_ahead, xgb_params,
                                            class[i],train_start_yr, train_end_yr,
                                            test_start_yr, test_end_yr)
    df_pred_all <- rbind(df_pred_all, df_pred_single)
  }
  
  df_pred_all <- as.data.frame(df_pred_all) 
  df_pred_all <- df_pred_all %>%
    pivot_longer(c(`East of England`,`London`, `Midlands`,`North East and Yorkshire`,
                   `North West`, `South East`, `South West`),
                 names_to = "nhs_region", values_to = "Probability") %>% 
    pivot_wider(names_from = Class, values_from = Probability)
  
  df_pred_all <- df_pred_all %>% 
    mutate(level_1 = 1-`level > 1`,
           level_2 = `level > 1` - `level > 2`,
           level_3 = `level > 2`) 

  # 7:9, probabilities for levels 1 to 3
  df_pred_all <- df_pred_all %>% 
    mutate(Class = colnames(df_pred_all)[7:9][max.col(df_pred_all[,7:9],ties.method="first")])
  
  if(bins_type == "uniform"){
    df_pred_final <- df_pred_all %>%
      mutate(hosp_uni = test_df[,"hosp_uni"]) %>%
      dplyr::select(Year, Week, nhs_region, hosp_uni, Class) %>%
      mutate(Pred = str_sub(Class, -1, -1)) %>% 
      mutate(Pred = as.numeric(Pred),
             hosp_uni = as.numeric(hosp_uni)) %>%
      dplyr::select(-Class)
  }
  
  if(bins_type == "quantile"){
    df_pred_final <- df_pred_all %>%
      mutate(hosp_quan = test_df[,"hosp_quan"]) %>%
      dplyr::select(Year, Week, nhs_region, hosp_quan, Class) %>%
      mutate(Pred = str_sub(Class, -1, -1)) %>% 
      mutate(Pred = as.numeric(Pred),
             hosp_quan = as.numeric(hosp_quan)) %>%
      dplyr::select(-Class)
  }
  
  names(df_pred_final)[names(df_pred_final) == "Pred"] <- paste0(nWeek_ahead,"week_pred")
  
  return(df_pred_final)
}

xgb_ordered5bins_pred <- function(data, raw_data, nWeek_ahead,bins_type, xgb_params,
                                  class,train_start_yr, train_end_yr,
                                  test_start_yr, test_end_yr){
  require(dplyr)
  
  test_df <- raw_data %>%
    arrange(Year, Week) %>%
    filter(Year == 2022)
  
  df_pred_all <- NULL
  
  for (i in 1:length(class)){
    df_pred_single <- xgb_signle_class_pred(data[[i]], nWeek_ahead, xgb_params,
                                            class[i],train_start_yr, train_end_yr,
                                            test_start_yr, test_end_yr)
    df_pred_all <- rbind(df_pred_all, df_pred_single)
  }
  
  df_pred_all <- as.data.frame(df_pred_all) 
  df_pred_all <- df_pred_all %>%
    pivot_longer(c(`East of England`,`London`, `Midlands`,`North East and Yorkshire`,
                   `North West`, `South East`, `South West`),
                 names_to = "nhs_region", values_to = "Probability") %>% 
    pivot_wider(names_from = Class, values_from = Probability)
  
  df_pred_all <- df_pred_all %>% 
    mutate(level_1 = 1-`level > 1`,
           level_2 = `level > 1` - `level > 2`,
           level_3 = `level > 2` - `level > 3`,
           level_4 = `level > 3` - `level > 4`,
           level_5 = `level > 4`) 
  
  # 9:13, probabilities for levels 1 to 5
  df_pred_all <- df_pred_all %>% 
    mutate(Class = colnames(df_pred_all)[9:13][max.col(df_pred_all[,9:13],ties.method="first")])
  
  if(bins_type == "uniform"){
    df_pred_final <- df_pred_all %>%
      mutate(hosp_uni = test_df[,"hosp_uni"]) %>%
      dplyr::select(Year, Week, nhs_region, hosp_uni, Class) %>%
      mutate(Pred = str_sub(Class, -1, -1)) %>% 
      mutate(Pred = as.numeric(Pred),
             hosp_uni = as.numeric(hosp_uni)) %>%
      dplyr::select(-Class)
  }
  
  if(bins_type == "quantile"){
    df_pred_final <- df_pred_all %>%
      mutate(hosp_quan = test_df[,"hosp_quan"]) %>%
      dplyr::select(Year, Week, nhs_region, hosp_quan, Class) %>%
      mutate(Pred = str_sub(Class, -1, -1)) %>% 
      mutate(Pred = as.numeric(Pred),
             hosp_quan = as.numeric(hosp_quan)) %>%
      dplyr::select(-Class)
  }
  
  names(df_pred_final)[names(df_pred_final) == "Pred"] <- paste0(nWeek_ahead,"week_pred")
  
  return(df_pred_final)
}

# test_xgb_ordered_pred5bins <- xgb_ordered5bins_pred(test_xgb_binary5bins,df_model_5bins, 1,
#                                                     "uniform",xgb_params,
#                                                     c(1:4), 2020, 2021, 2022, 2022)

xgb_ordered10bins_pred <- function(data, raw_data, nWeek_ahead, bins_type,
                                   xgb_params,class,train_start_yr, train_end_yr,
                                   test_start_yr, test_end_yr){
  require(dplyr)
  
  test_df <- raw_data %>%
    arrange(Year, Week) %>%
    filter(Year == 2022)
  
  df_pred_all <- NULL
  
  for (i in 1:length(class)){
    df_pred_single <- xgb_signle_class_pred(data[[i]], nWeek_ahead, xgb_params,
                                            class[i],train_start_yr, train_end_yr,
                                            test_start_yr, test_end_yr)
    df_pred_all <- rbind(df_pred_all, df_pred_single)
  }
  
  df_pred_all <- as.data.frame(df_pred_all) 
  df_pred_all <- df_pred_all %>%
    pivot_longer(c(`East of England`,`London`, `Midlands`,`North East and Yorkshire`,
                   `North West`, `South East`, `South West`),
                 names_to = "nhs_region", values_to = "Probability") %>% 
    pivot_wider(names_from = Class, values_from = Probability)

  df_pred_all <- df_pred_all %>% 
    mutate(level_1 = 1-`level > 1`,
           level_2 = `level > 1` - `level > 2`,
           level_3 = `level > 2` - `level > 3`,
           level_4 = `level > 3` - `level > 4`,
           level_5 = `level > 4` - `level > 5`,
           level_6 = `level > 5` - `level > 6`,
           level_7 = `level > 6` - `level > 7`,
           level_8 = `level > 7` - `level > 8`,
           level_9 = `level > 8` - `level > 9`,
           level_10 = `level > 9`) 
  
  # 14:23, probabilities for levels 1 to 10
  df_pred_all <- df_pred_all %>% 
    mutate(Class = colnames(df_pred_all)[14:23][max.col(df_pred_all[,14:23],ties.method="first")])
  
  if(bins_type == "uniform"){
    df_pred_final <- df_pred_all %>%
      mutate(hosp_uni = test_df[,"hosp_uni"]) %>%
      dplyr::select(Year, Week, nhs_region, hosp_uni, Class) %>%
      mutate(Pred = str_sub(Class, -1, -1)) %>% 
      mutate(Pred = as.numeric(Pred),
             hosp_uni = as.numeric(hosp_uni)) %>%
      dplyr::select(-Class)
  }

  if(bins_type == "quantile"){
    df_pred_final <- df_pred_all %>%
      mutate(hosp_quan = test_df[,"hosp_quan"]) %>%
      dplyr::select(Year, Week, nhs_region, hosp_quan, Class) %>%
      mutate(Pred = str_sub(Class, -1, -1)) %>% 
      mutate(Pred = as.numeric(Pred),
             hosp_quan = as.numeric(hosp_quan)) %>%
      dplyr::select(-Class)
  }
  df_pred_final <- df_pred_final %>%
    mutate(Pred = ifelse(Pred == 0, 10, Pred))

  names(df_pred_final)[names(df_pred_final) == "Pred"] <- paste0(nWeek_ahead,"week_pred")
  
  return(df_pred_final)
}

# test_xgb_ordered_pred <- xgb_ordered10bins_pred(xgb_binary_list_1week10bins_quan, df_model_10bins,
#                                                 1, "quantile",
#                                                 xgb_params,
#                                                 c(1:9), 2020, 2021, 2022, 2022)
# xgb_ordered_params <-readRDS("./saved_objects/para_orderedXGBoost_final.rds")

############## Forecast ############
#' make predictions for 1 to 4-week ahead prediction

######### Forecast - weather ######
#' prepare data lists
# xgb_params <- list(booster = "gbtree", objective = "reg:logistic")
xgb_params_multi <- readRDS("./saved_objects/hyperpara_list_final.rds")
xgb_params_order <- xgb_params_multi
xgb_params_order$booster <- "gbtree"
xgb_params_order$objective <- "reg:logistic"
xgb_params_order <- xgb_params_order[-which(names(xgb_params_order)=="num_class")]


xgb_binary_weather_list_1week3bins_quan <- xgb_binary_weather(df_model_3bins, 1, "quantile", 3)
xgb_binary_weather_list_2week3bins_quan <- xgb_binary_weather(df_model_3bins, 2, "quantile", 3)
xgb_binary_weather_list_3week3bins_quan <- xgb_binary_weather(df_model_3bins, 3, "quantile", 3)
xgb_binary_weather_list_4week3bins_quan <- xgb_binary_weather(df_model_3bins, 4, "quantile", 3)

xgb_binary_weather_list_1week3bins_uni <- xgb_binary_weather(df_model_3bins, 1, "uniform", 3)
xgb_binary_weather_list_2week3bins_uni <- xgb_binary_weather(df_model_3bins, 2, "uniform", 3)
xgb_binary_weather_list_3week3bins_uni <- xgb_binary_weather(df_model_3bins, 3, "uniform", 3)
xgb_binary_weather_list_4week3bins_uni <- xgb_binary_weather(df_model_3bins, 4, "uniform", 3)

xgb_binary_weather_list_1week5bins_uni <- xgb_binary_weather(df_model_5bins, 1, "uniform", 5)
xgb_binary_weather_list_2week5bins_uni <- xgb_binary_weather(df_model_5bins, 2, "uniform", 5)
xgb_binary_weather_list_3week5bins_uni <- xgb_binary_weather(df_model_5bins, 3, "uniform", 5)
xgb_binary_weather_list_4week5bins_uni <- xgb_binary_weather(df_model_5bins, 4, "uniform", 5)

xgb_binary_weather_list_1week5bins_quan <- xgb_binary_weather(df_model_5bins, 1, "quantile", 5)
xgb_binary_weather_list_2week5bins_quan <- xgb_binary_weather(df_model_5bins, 2, "quantile", 5)
xgb_binary_weather_list_3week5bins_quan <- xgb_binary_weather(df_model_5bins, 3, "quantile", 5)
xgb_binary_weather_list_4week5bins_quan <- xgb_binary_weather(df_model_5bins, 4, "quantile", 5)

xgb_binary_weather_list_1week10bins_uni <- xgb_binary_weather(df_model_10bins, 1, "uniform", 10)
xgb_binary_weather_list_2week10bins_uni <- xgb_binary_weather(df_model_10bins, 2, "uniform", 10)
xgb_binary_weather_list_3week10bins_uni <- xgb_binary_weather(df_model_10bins, 3, "uniform", 10)
xgb_binary_weather_list_4week10bins_uni <- xgb_binary_weather(df_model_10bins, 4, "uniform", 10)

xgb_binary_weather_list_1week10bins_quan <- xgb_binary_weather(df_model_10bins, 1, "quantile", 10)
xgb_binary_weather_list_2week10bins_quan <- xgb_binary_weather(df_model_10bins, 2, "quantile", 10)
xgb_binary_weather_list_3week10bins_quan <- xgb_binary_weather(df_model_10bins, 3, "quantile", 10)
xgb_binary_weather_list_4week10bins_quan <- xgb_binary_weather(df_model_10bins, 4, "quantile", 10)

#' number of bins = 3
xgb_wea_ordered_pred_1week3bins_quan <- xgb_ordered3bins_pred(xgb_binary_weather_list_1week3bins_quan, df_model_3bins,
                                                          1, "quantile", xgb_params_order, c(1:2), 
                                                          2020, 2021, 2022, 2022)
xgb_wea_ordered_pred_2week3bins_quan <- xgb_ordered3bins_pred(xgb_binary_weather_list_2week3bins_quan, df_model_3bins,
                                                          2, "quantile", xgb_params_order, c(1:2), 
                                                          2020, 2021, 2022, 2022)
xgb_wea_ordered_pred_3week3bins_quan <- xgb_ordered3bins_pred(xgb_binary_weather_list_3week3bins_quan, df_model_3bins,
                                                          3, "quantile", xgb_params_order, c(1:2), 
                                                          2020, 2021, 2022, 2022)
xgb_wea_ordered_pred_4week3bins_quan <- xgb_ordered3bins_pred(xgb_binary_weather_list_4week3bins_quan, df_model_3bins,
                                                          4, "quantile", xgb_params_order, c(1:2), 
                                                          2020, 2021, 2022, 2022)

xgb_wea_ordered_pred_1week3bins_uni <- xgb_ordered3bins_pred(xgb_binary_weather_list_1week3bins_uni, df_model_3bins,
                                                         1, "uniform", xgb_params_order, c(1:2), 
                                                         2020, 2021, 2022, 2022)
xgb_wea_ordered_pred_2week3bins_uni <- xgb_ordered3bins_pred(xgb_binary_weather_list_2week3bins_uni, df_model_3bins,
                                                         2, "uniform", xgb_params_order, c(1:2), 
                                                         2020, 2021, 2022, 2022)
xgb_wea_ordered_pred_3week3bins_uni <- xgb_ordered3bins_pred(xgb_binary_weather_list_3week3bins_uni, df_model_3bins,
                                                         3, "uniform", xgb_params_order, c(1:2), 
                                                         2020, 2021, 2022, 2022)
xgb_wea_ordered_pred_4week3bins_uni <- xgb_ordered3bins_pred(xgb_binary_weather_list_4week3bins_uni, df_model_3bins,
                                                         4, "uniform", xgb_params_order, c(1:2), 
                                                         2020, 2021, 2022, 2022)


#' number of bins = 5 
xgb_wea_ordered_pred_1week5bins_quan <- xgb_ordered5bins_pred(xgb_binary_weather_list_1week5bins_quan, df_model_5bins,
                                                          1, "quantile", xgb_params_order, c(1:4), 
                                                          2020, 2021, 2022, 2022)
xgb_wea_ordered_pred_2week5bins_quan <- xgb_ordered5bins_pred(xgb_binary_weather_list_2week5bins_quan, df_model_5bins,
                                                          2, "quantile", xgb_params_order, c(1:4), 
                                                          2020, 2021, 2022, 2022)
xgb_wea_ordered_pred_3week5bins_quan <- xgb_ordered5bins_pred(xgb_binary_weather_list_3week5bins_quan, df_model_5bins,
                                                          3, "quantile", xgb_params_order, c(1:4), 
                                                          2020, 2021, 2022, 2022)
xgb_wea_ordered_pred_4week5bins_quan <- xgb_ordered5bins_pred(xgb_binary_weather_list_4week5bins_quan, df_model_5bins,
                                                          4, "quantile", xgb_params_order, c(1:4), 
                                                          2020, 2021, 2022, 2022)

xgb_wea_ordered_pred_1week5bins_uni <- xgb_ordered5bins_pred(xgb_binary_weather_list_1week5bins_uni, df_model_5bins,
                                                         1, "uniform", xgb_params_order, c(1:4), 
                                                         2020, 2021, 2022, 2022)
xgb_wea_ordered_pred_2week5bins_uni <- xgb_ordered5bins_pred(xgb_binary_weather_list_2week5bins_uni, df_model_5bins,
                                                         2, "uniform", xgb_params_order, c(1:4), 
                                                         2020, 2021, 2022, 2022)
xgb_wea_ordered_pred_3week5bins_uni <- xgb_ordered5bins_pred(xgb_binary_weather_list_3week5bins_uni, df_model_5bins,
                                                         3, "uniform", xgb_params_order, c(1:4), 
                                                         2020, 2021, 2022, 2022)
xgb_wea_ordered_pred_4week5bins_uni <- xgb_ordered5bins_pred(xgb_binary_weather_list_4week5bins_uni, df_model_5bins,
                                                         4, "uniform", xgb_params_order, c(1:4), 
                                                         2020, 2021, 2022, 2022)

#' number of bins = 10
xgb_wea_ordered_pred_1week10bins_quan <- xgb_ordered10bins_pred(xgb_binary_weather_list_1week10bins_quan, df_model_10bins,
                                                          1, "quantile", xgb_params_order, c(1:9), 
                                                          2020, 2021, 2022, 2022)
xgb_wea_ordered_pred_2week10bins_quan <- xgb_ordered10bins_pred(xgb_binary_weather_list_2week10bins_quan, df_model_10bins,
                                                          2, "quantile", xgb_params_order, c(1:9), 
                                                          2020, 2021, 2022, 2022)
xgb_wea_ordered_pred_3week10bins_quan <- xgb_ordered10bins_pred(xgb_binary_weather_list_3week10bins_quan, df_model_10bins,
                                                          3, "quantile", xgb_params_order, c(1:9), 
                                                          2020, 2021, 2022, 2022)
xgb_wea_ordered_pred_4week10bins_quan <- xgb_ordered10bins_pred(xgb_binary_weather_list_4week10bins_quan, df_model_10bins,
                                                          4, "quantile", xgb_params_order, c(1:9), 
                                                          2020, 2021, 2022, 2022)

xgb_wea_ordered_pred_1week10bins_uni <- xgb_ordered10bins_pred(xgb_binary_weather_list_1week10bins_uni, df_model_10bins,
                                                         1, "uniform", xgb_params_order, c(1:9), 
                                                         2020, 2021, 2022, 2022)
xgb_wea_ordered_pred_2week10bins_uni <- xgb_ordered10bins_pred(xgb_binary_weather_list_2week10bins_uni, df_model_10bins,
                                                         2, "uniform", xgb_params_order, c(1:9), 
                                                         2020, 2021, 2022, 2022)
xgb_wea_ordered_pred_3week10bins_uni <- xgb_ordered10bins_pred(xgb_binary_weather_list_3week10bins_uni, df_model_10bins,
                                                         3, "uniform", xgb_params_order, c(1:9), 
                                                         2020, 2021, 2022, 2022)
xgb_wea_ordered_pred_4week10bins_uni <- xgb_ordered10bins_pred(xgb_binary_weather_list_4week10bins_uni, df_model_10bins,
                                                         4, "uniform", xgb_params_order, c(1:9), 
                                                         2020, 2021, 2022, 2022)

xgb_wea_pred_all_ordered_quan3bins <- full_join(xgb_wea_ordered_pred_1week3bins_quan, xgb_wea_ordered_pred_2week3bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_wea_ordered_pred_3week3bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_wea_ordered_pred_4week3bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_wea_pred_all_ordered_uni3bins <- full_join(xgb_wea_ordered_pred_1week3bins_uni, xgb_wea_ordered_pred_2week3bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_wea_ordered_pred_3week3bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_wea_ordered_pred_4week3bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_wea_pred_all_ordered_quan5bins <- full_join(xgb_wea_ordered_pred_1week5bins_quan, xgb_wea_ordered_pred_2week5bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_wea_ordered_pred_3week5bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_wea_ordered_pred_4week5bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_wea_pred_all_ordered_uni5bins <- full_join(xgb_wea_ordered_pred_1week5bins_uni, xgb_wea_ordered_pred_2week5bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_wea_ordered_pred_3week5bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_wea_ordered_pred_4week5bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_wea_pred_all_ordered_quan10bins <- full_join(xgb_wea_ordered_pred_1week10bins_quan, xgb_wea_ordered_pred_2week10bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_wea_ordered_pred_3week10bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_wea_ordered_pred_4week10bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_wea_pred_all_ordered_uni10bins <- full_join(xgb_wea_ordered_pred_1week10bins_uni, xgb_wea_ordered_pred_2week10bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_wea_ordered_pred_3week10bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_wea_ordered_pred_4week10bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_wea_pred_all_ordered <- list(xgb_wea_pred_all_ordered_quan3bins = xgb_wea_pred_all_ordered_quan3bins,
                             xgb_wea_pred_all_ordered_uni3bins = xgb_wea_pred_all_ordered_uni3bins,
                             xgb_wea_pred_all_ordered_quan5bins = xgb_wea_pred_all_ordered_quan5bins,
                             xgb_wea_pred_all_ordered_uni5bins = xgb_wea_pred_all_ordered_uni5bins,
                             xgb_wea_pred_all_ordered_quan10bins = xgb_wea_pred_all_ordered_quan10bins,
                             xgb_wea_pred_all_ordered_uni10bins = xgb_wea_pred_all_ordered_uni10bins)

saveRDS(xgb_wea_pred_all_ordered, "./saved_objects/xgb_pred_all_ordered_weather.rds")

######### Forecast - mobi ######
#' prepare data lists
# xgb_params <- list(booster = "gbtree", objective = "reg:logistic")

xgb_binary_mobi_list_1week3bins_quan <- xgb_binary_mobi(df_model_3bins, 1, "quantile", 3)
xgb_binary_mobi_list_2week3bins_quan <- xgb_binary_mobi(df_model_3bins, 2, "quantile", 3)
xgb_binary_mobi_list_3week3bins_quan <- xgb_binary_mobi(df_model_3bins, 3, "quantile", 3)
xgb_binary_mobi_list_4week3bins_quan <- xgb_binary_mobi(df_model_3bins, 4, "quantile", 3)

xgb_binary_mobi_list_1week3bins_uni <- xgb_binary_mobi(df_model_3bins, 1, "uniform", 3)
xgb_binary_mobi_list_2week3bins_uni <- xgb_binary_mobi(df_model_3bins, 2, "uniform", 3)
xgb_binary_mobi_list_3week3bins_uni <- xgb_binary_mobi(df_model_3bins, 3, "uniform", 3)
xgb_binary_mobi_list_4week3bins_uni <- xgb_binary_mobi(df_model_3bins, 4, "uniform", 3)

xgb_binary_mobi_list_1week5bins_uni <- xgb_binary_mobi(df_model_5bins, 1, "uniform", 5)
xgb_binary_mobi_list_2week5bins_uni <- xgb_binary_mobi(df_model_5bins, 2, "uniform", 5)
xgb_binary_mobi_list_3week5bins_uni <- xgb_binary_mobi(df_model_5bins, 3, "uniform", 5)
xgb_binary_mobi_list_4week5bins_uni <- xgb_binary_mobi(df_model_5bins, 4, "uniform", 5)

xgb_binary_mobi_list_1week5bins_quan <- xgb_binary_mobi(df_model_5bins, 1, "quantile", 5)
xgb_binary_mobi_list_2week5bins_quan <- xgb_binary_mobi(df_model_5bins, 2, "quantile", 5)
xgb_binary_mobi_list_3week5bins_quan <- xgb_binary_mobi(df_model_5bins, 3, "quantile", 5)
xgb_binary_mobi_list_4week5bins_quan <- xgb_binary_mobi(df_model_5bins, 4, "quantile", 5)

xgb_binary_mobi_list_1week10bins_uni <- xgb_binary_mobi(df_model_10bins, 1, "uniform", 10)
xgb_binary_mobi_list_2week10bins_uni <- xgb_binary_mobi(df_model_10bins, 2, "uniform", 10)
xgb_binary_mobi_list_3week10bins_uni <- xgb_binary_mobi(df_model_10bins, 3, "uniform", 10)
xgb_binary_mobi_list_4week10bins_uni <- xgb_binary_mobi(df_model_10bins, 4, "uniform", 10)

xgb_binary_mobi_list_1week10bins_quan <- xgb_binary_mobi(df_model_10bins, 1, "quantile", 10)
xgb_binary_mobi_list_2week10bins_quan <- xgb_binary_mobi(df_model_10bins, 2, "quantile", 10)
xgb_binary_mobi_list_3week10bins_quan <- xgb_binary_mobi(df_model_10bins, 3, "quantile", 10)
xgb_binary_mobi_list_4week10bins_quan <- xgb_binary_mobi(df_model_10bins, 4, "quantile", 10)

#' number of bins = 3
xgb_mobi_ordered_pred_1week3bins_quan <- xgb_ordered3bins_pred(xgb_binary_mobi_list_1week3bins_quan, df_model_3bins,
                                                              1, "quantile", xgb_params_order, c(1:2), 
                                                              2020, 2021, 2022, 2022)
xgb_mobi_ordered_pred_2week3bins_quan <- xgb_ordered3bins_pred(xgb_binary_mobi_list_2week3bins_quan, df_model_3bins,
                                                              2, "quantile", xgb_params_order, c(1:2), 
                                                              2020, 2021, 2022, 2022)
xgb_mobi_ordered_pred_3week3bins_quan <- xgb_ordered3bins_pred(xgb_binary_mobi_list_3week3bins_quan, df_model_3bins,
                                                              3, "quantile", xgb_params_order, c(1:2), 
                                                              2020, 2021, 2022, 2022)
xgb_mobi_ordered_pred_4week3bins_quan <- xgb_ordered3bins_pred(xgb_binary_mobi_list_4week3bins_quan, df_model_3bins,
                                                              4, "quantile", xgb_params_order, c(1:2), 
                                                              2020, 2021, 2022, 2022)

xgb_mobi_ordered_pred_1week3bins_uni <- xgb_ordered3bins_pred(xgb_binary_mobi_list_1week3bins_uni, df_model_3bins,
                                                             1, "uniform", xgb_params_order, c(1:2), 
                                                             2020, 2021, 2022, 2022)
xgb_mobi_ordered_pred_2week3bins_uni <- xgb_ordered3bins_pred(xgb_binary_mobi_list_2week3bins_uni, df_model_3bins,
                                                             2, "uniform", xgb_params_order, c(1:2), 
                                                             2020, 2021, 2022, 2022)
xgb_mobi_ordered_pred_3week3bins_uni <- xgb_ordered3bins_pred(xgb_binary_mobi_list_3week3bins_uni, df_model_3bins,
                                                             3, "uniform", xgb_params_order, c(1:2), 
                                                             2020, 2021, 2022, 2022)
xgb_mobi_ordered_pred_4week3bins_uni <- xgb_ordered3bins_pred(xgb_binary_mobi_list_4week3bins_uni, df_model_3bins,
                                                             4, "uniform", xgb_params_order, c(1:2), 
                                                             2020, 2021, 2022, 2022)


#' number of bins = 5 
xgb_mobi_ordered_pred_1week5bins_quan <- xgb_ordered5bins_pred(xgb_binary_mobi_list_1week5bins_quan, df_model_5bins,
                                                              1, "quantile", xgb_params_order, c(1:4), 
                                                              2020, 2021, 2022, 2022)
xgb_mobi_ordered_pred_2week5bins_quan <- xgb_ordered5bins_pred(xgb_binary_mobi_list_2week5bins_quan, df_model_5bins,
                                                              2, "quantile", xgb_params_order, c(1:4), 
                                                              2020, 2021, 2022, 2022)
xgb_mobi_ordered_pred_3week5bins_quan <- xgb_ordered5bins_pred(xgb_binary_mobi_list_3week5bins_quan, df_model_5bins,
                                                              3, "quantile", xgb_params_order, c(1:4), 
                                                              2020, 2021, 2022, 2022)
xgb_mobi_ordered_pred_4week5bins_quan <- xgb_ordered5bins_pred(xgb_binary_mobi_list_4week5bins_quan, df_model_5bins,
                                                              4, "quantile", xgb_params_order, c(1:4), 
                                                              2020, 2021, 2022, 2022)

xgb_mobi_ordered_pred_1week5bins_uni <- xgb_ordered5bins_pred(xgb_binary_mobi_list_1week5bins_uni, df_model_5bins,
                                                             1, "uniform", xgb_params_order, c(1:4), 
                                                             2020, 2021, 2022, 2022)
xgb_mobi_ordered_pred_2week5bins_uni <- xgb_ordered5bins_pred(xgb_binary_mobi_list_2week5bins_uni, df_model_5bins,
                                                             2, "uniform", xgb_params_order, c(1:4), 
                                                             2020, 2021, 2022, 2022)
xgb_mobi_ordered_pred_3week5bins_uni <- xgb_ordered5bins_pred(xgb_binary_mobi_list_3week5bins_uni, df_model_5bins,
                                                             3, "uniform", xgb_params_order, c(1:4), 
                                                             2020, 2021, 2022, 2022)
xgb_mobi_ordered_pred_4week5bins_uni <- xgb_ordered5bins_pred(xgb_binary_mobi_list_4week5bins_uni, df_model_5bins,
                                                             4, "uniform", xgb_params_order, c(1:4), 
                                                             2020, 2021, 2022, 2022)

#' number of bins = 10
xgb_mobi_ordered_pred_1week10bins_quan <- xgb_ordered10bins_pred(xgb_binary_mobi_list_1week10bins_quan, df_model_10bins,
                                                                1, "quantile", xgb_params_order, c(1:9), 
                                                                2020, 2021, 2022, 2022)
xgb_mobi_ordered_pred_2week10bins_quan <- xgb_ordered10bins_pred(xgb_binary_mobi_list_2week10bins_quan, df_model_10bins,
                                                                2, "quantile", xgb_params_order, c(1:9), 
                                                                2020, 2021, 2022, 2022)
xgb_mobi_ordered_pred_3week10bins_quan <- xgb_ordered10bins_pred(xgb_binary_mobi_list_3week10bins_quan, df_model_10bins,
                                                                3, "quantile", xgb_params_order, c(1:9), 
                                                                2020, 2021, 2022, 2022)
xgb_mobi_ordered_pred_4week10bins_quan <- xgb_ordered10bins_pred(xgb_binary_mobi_list_4week10bins_quan, df_model_10bins,
                                                                4, "quantile", xgb_params_order, c(1:9), 
                                                                2020, 2021, 2022, 2022)

xgb_mobi_ordered_pred_1week10bins_uni <- xgb_ordered10bins_pred(xgb_binary_mobi_list_1week10bins_uni, df_model_10bins,
                                                               1, "uniform", xgb_params_order, c(1:9), 
                                                               2020, 2021, 2022, 2022)
xgb_mobi_ordered_pred_2week10bins_uni <- xgb_ordered10bins_pred(xgb_binary_mobi_list_2week10bins_uni, df_model_10bins,
                                                               2, "uniform", xgb_params_order, c(1:9), 
                                                               2020, 2021, 2022, 2022)
xgb_mobi_ordered_pred_3week10bins_uni <- xgb_ordered10bins_pred(xgb_binary_mobi_list_3week10bins_uni, df_model_10bins,
                                                               3, "uniform", xgb_params_order, c(1:9), 
                                                               2020, 2021, 2022, 2022)
xgb_mobi_ordered_pred_4week10bins_uni <- xgb_ordered10bins_pred(xgb_binary_mobi_list_4week10bins_uni, df_model_10bins,
                                                               4, "uniform", xgb_params_order, c(1:9), 
                                                               2020, 2021, 2022, 2022)

xgb_mobi_pred_all_ordered_quan3bins <- full_join(xgb_mobi_ordered_pred_1week3bins_quan, xgb_mobi_ordered_pred_2week3bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_mobi_ordered_pred_3week3bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_mobi_ordered_pred_4week3bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_mobi_pred_all_ordered_uni3bins <- full_join(xgb_mobi_ordered_pred_1week3bins_uni, xgb_mobi_ordered_pred_2week3bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_mobi_ordered_pred_3week3bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_mobi_ordered_pred_4week3bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_mobi_pred_all_ordered_quan5bins <- full_join(xgb_mobi_ordered_pred_1week5bins_quan, xgb_mobi_ordered_pred_2week5bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_mobi_ordered_pred_3week5bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_mobi_ordered_pred_4week5bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_mobi_pred_all_ordered_uni5bins <- full_join(xgb_mobi_ordered_pred_1week5bins_uni, xgb_mobi_ordered_pred_2week5bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_mobi_ordered_pred_3week5bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_mobi_ordered_pred_4week5bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_mobi_pred_all_ordered_quan10bins <- full_join(xgb_mobi_ordered_pred_1week10bins_quan, xgb_mobi_ordered_pred_2week10bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_mobi_ordered_pred_3week10bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_mobi_ordered_pred_4week10bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_mobi_pred_all_ordered_uni10bins <- full_join(xgb_mobi_ordered_pred_1week10bins_uni, xgb_mobi_ordered_pred_2week10bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_mobi_ordered_pred_3week10bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_mobi_ordered_pred_4week10bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_mobi_pred_all_ordered <- list(xgb_mobi_pred_all_ordered_quan3bins = xgb_mobi_pred_all_ordered_quan3bins,
                                 xgb_mobi_pred_all_ordered_uni3bins = xgb_mobi_pred_all_ordered_uni3bins,
                                 xgb_mobi_pred_all_ordered_quan5bins = xgb_mobi_pred_all_ordered_quan5bins,
                                 xgb_mobi_pred_all_ordered_uni5bins = xgb_mobi_pred_all_ordered_uni5bins,
                                 xgb_mobi_pred_all_ordered_quan10bins = xgb_mobi_pred_all_ordered_quan10bins,
                                 xgb_mobi_pred_all_ordered_uni10bins = xgb_mobi_pred_all_ordered_uni10bins)
saveRDS(xgb_mobi_pred_all_ordered, "./saved_objects/xgb_pred_all_ordered_mobi.rds")

######### Forecast - epi ######
#' prepare data lists
# xgb_params <- list(booster = "gbtree", objective = "reg:logistic")

xgb_binary_epi_list_1week3bins_quan <- xgb_binary_epi(df_model_3bins, 1, "quantile", 3)
xgb_binary_epi_list_2week3bins_quan <- xgb_binary_epi(df_model_3bins, 2, "quantile", 3)
xgb_binary_epi_list_3week3bins_quan <- xgb_binary_epi(df_model_3bins, 3, "quantile", 3)
xgb_binary_epi_list_4week3bins_quan <- xgb_binary_epi(df_model_3bins, 4, "quantile", 3)

xgb_binary_epi_list_1week3bins_uni <- xgb_binary_epi(df_model_3bins, 1, "uniform", 3)
xgb_binary_epi_list_2week3bins_uni <- xgb_binary_epi(df_model_3bins, 2, "uniform", 3)
xgb_binary_epi_list_3week3bins_uni <- xgb_binary_epi(df_model_3bins, 3, "uniform", 3)
xgb_binary_epi_list_4week3bins_uni <- xgb_binary_epi(df_model_3bins, 4, "uniform", 3)

xgb_binary_epi_list_1week5bins_uni <- xgb_binary_epi(df_model_5bins, 1, "uniform", 5)
xgb_binary_epi_list_2week5bins_uni <- xgb_binary_epi(df_model_5bins, 2, "uniform", 5)
xgb_binary_epi_list_3week5bins_uni <- xgb_binary_epi(df_model_5bins, 3, "uniform", 5)
xgb_binary_epi_list_4week5bins_uni <- xgb_binary_epi(df_model_5bins, 4, "uniform", 5)

xgb_binary_epi_list_1week5bins_quan <- xgb_binary_epi(df_model_5bins, 1, "quantile", 5)
xgb_binary_epi_list_2week5bins_quan <- xgb_binary_epi(df_model_5bins, 2, "quantile", 5)
xgb_binary_epi_list_3week5bins_quan <- xgb_binary_epi(df_model_5bins, 3, "quantile", 5)
xgb_binary_epi_list_4week5bins_quan <- xgb_binary_epi(df_model_5bins, 4, "quantile", 5)

xgb_binary_epi_list_1week10bins_uni <- xgb_binary_epi(df_model_10bins, 1, "uniform", 10)
xgb_binary_epi_list_2week10bins_uni <- xgb_binary_epi(df_model_10bins, 2, "uniform", 10)
xgb_binary_epi_list_3week10bins_uni <- xgb_binary_epi(df_model_10bins, 3, "uniform", 10)
xgb_binary_epi_list_4week10bins_uni <- xgb_binary_epi(df_model_10bins, 4, "uniform", 10)

xgb_binary_epi_list_1week10bins_quan <- xgb_binary_epi(df_model_10bins, 1, "quantile", 10)
xgb_binary_epi_list_2week10bins_quan <- xgb_binary_epi(df_model_10bins, 2, "quantile", 10)
xgb_binary_epi_list_3week10bins_quan <- xgb_binary_epi(df_model_10bins, 3, "quantile", 10)
xgb_binary_epi_list_4week10bins_quan <- xgb_binary_epi(df_model_10bins, 4, "quantile", 10)

#' number of bins = 3
xgb_epi_ordered_pred_1week3bins_quan <- xgb_ordered3bins_pred(xgb_binary_epi_list_1week3bins_quan, df_model_3bins,
                                                              1, "quantile", xgb_params_order, c(1:2), 
                                                              2020, 2021, 2022, 2022)
xgb_epi_ordered_pred_2week3bins_quan <- xgb_ordered3bins_pred(xgb_binary_epi_list_2week3bins_quan, df_model_3bins,
                                                              2, "quantile", xgb_params_order, c(1:2), 
                                                              2020, 2021, 2022, 2022)
xgb_epi_ordered_pred_3week3bins_quan <- xgb_ordered3bins_pred(xgb_binary_epi_list_3week3bins_quan, df_model_3bins,
                                                              3, "quantile", xgb_params_order, c(1:2), 
                                                              2020, 2021, 2022, 2022)
xgb_epi_ordered_pred_4week3bins_quan <- xgb_ordered3bins_pred(xgb_binary_epi_list_4week3bins_quan, df_model_3bins,
                                                              4, "quantile", xgb_params_order, c(1:2), 
                                                              2020, 2021, 2022, 2022)

xgb_epi_ordered_pred_1week3bins_uni <- xgb_ordered3bins_pred(xgb_binary_epi_list_1week3bins_uni, df_model_3bins,
                                                             1, "uniform", xgb_params_order, c(1:2), 
                                                             2020, 2021, 2022, 2022)
xgb_epi_ordered_pred_2week3bins_uni <- xgb_ordered3bins_pred(xgb_binary_epi_list_2week3bins_uni, df_model_3bins,
                                                             2, "uniform", xgb_params_order, c(1:2), 
                                                             2020, 2021, 2022, 2022)
xgb_epi_ordered_pred_3week3bins_uni <- xgb_ordered3bins_pred(xgb_binary_epi_list_3week3bins_uni, df_model_3bins,
                                                             3, "uniform", xgb_params_order, c(1:2), 
                                                             2020, 2021, 2022, 2022)
xgb_epi_ordered_pred_4week3bins_uni <- xgb_ordered3bins_pred(xgb_binary_epi_list_4week3bins_uni, df_model_3bins,
                                                             4, "uniform", xgb_params_order, c(1:2), 
                                                             2020, 2021, 2022, 2022)


#' number of bins = 5 
xgb_epi_ordered_pred_1week5bins_quan <- xgb_ordered5bins_pred(xgb_binary_epi_list_1week5bins_quan, df_model_5bins,
                                                              1, "quantile", xgb_params_order, c(1:4), 
                                                              2020, 2021, 2022, 2022)
xgb_epi_ordered_pred_2week5bins_quan <- xgb_ordered5bins_pred(xgb_binary_epi_list_2week5bins_quan, df_model_5bins,
                                                              2, "quantile", xgb_params_order, c(1:4), 
                                                              2020, 2021, 2022, 2022)
xgb_epi_ordered_pred_3week5bins_quan <- xgb_ordered5bins_pred(xgb_binary_epi_list_3week5bins_quan, df_model_5bins,
                                                              3, "quantile", xgb_params_order, c(1:4), 
                                                              2020, 2021, 2022, 2022)
xgb_epi_ordered_pred_4week5bins_quan <- xgb_ordered5bins_pred(xgb_binary_epi_list_4week5bins_quan, df_model_5bins,
                                                              4, "quantile", xgb_params_order, c(1:4), 
                                                              2020, 2021, 2022, 2022)

xgb_epi_ordered_pred_1week5bins_uni <- xgb_ordered5bins_pred(xgb_binary_epi_list_1week5bins_uni, df_model_5bins,
                                                             1, "uniform", xgb_params_order, c(1:4), 
                                                             2020, 2021, 2022, 2022)
xgb_epi_ordered_pred_2week5bins_uni <- xgb_ordered5bins_pred(xgb_binary_epi_list_2week5bins_uni, df_model_5bins,
                                                             2, "uniform", xgb_params_order, c(1:4), 
                                                             2020, 2021, 2022, 2022)
xgb_epi_ordered_pred_3week5bins_uni <- xgb_ordered5bins_pred(xgb_binary_epi_list_3week5bins_uni, df_model_5bins,
                                                             3, "uniform", xgb_params_order, c(1:4), 
                                                             2020, 2021, 2022, 2022)
xgb_epi_ordered_pred_4week5bins_uni <- xgb_ordered5bins_pred(xgb_binary_epi_list_4week5bins_uni, df_model_5bins,
                                                             4, "uniform", xgb_params_order, c(1:4), 
                                                             2020, 2021, 2022, 2022)

#' number of bins = 10
xgb_epi_ordered_pred_1week10bins_quan <- xgb_ordered10bins_pred(xgb_binary_epi_list_1week10bins_quan, df_model_10bins,
                                                                1, "quantile", xgb_params_order, c(1:9), 
                                                                2020, 2021, 2022, 2022)
xgb_epi_ordered_pred_2week10bins_quan <- xgb_ordered10bins_pred(xgb_binary_epi_list_2week10bins_quan, df_model_10bins,
                                                                2, "quantile", xgb_params_order, c(1:9), 
                                                                2020, 2021, 2022, 2022)
xgb_epi_ordered_pred_3week10bins_quan <- xgb_ordered10bins_pred(xgb_binary_epi_list_3week10bins_quan, df_model_10bins,
                                                                3, "quantile", xgb_params_order, c(1:9), 
                                                                2020, 2021, 2022, 2022)
xgb_epi_ordered_pred_4week10bins_quan <- xgb_ordered10bins_pred(xgb_binary_epi_list_4week10bins_quan, df_model_10bins,
                                                                4, "quantile", xgb_params_order, c(1:9), 
                                                                2020, 2021, 2022, 2022)

xgb_epi_ordered_pred_1week10bins_uni <- xgb_ordered10bins_pred(xgb_binary_epi_list_1week10bins_uni, df_model_10bins,
                                                               1, "uniform", xgb_params_order, c(1:9), 
                                                               2020, 2021, 2022, 2022)
xgb_epi_ordered_pred_2week10bins_uni <- xgb_ordered10bins_pred(xgb_binary_epi_list_2week10bins_uni, df_model_10bins,
                                                               2, "uniform", xgb_params_order, c(1:9), 
                                                               2020, 2021, 2022, 2022)
xgb_epi_ordered_pred_3week10bins_uni <- xgb_ordered10bins_pred(xgb_binary_epi_list_3week10bins_uni, df_model_10bins,
                                                               3, "uniform", xgb_params_order, c(1:9), 
                                                               2020, 2021, 2022, 2022)
xgb_epi_ordered_pred_4week10bins_uni <- xgb_ordered10bins_pred(xgb_binary_epi_list_4week10bins_uni, df_model_10bins,
                                                               4, "uniform", xgb_params_order, c(1:9), 
                                                               2020, 2021, 2022, 2022)

xgb_epi_pred_all_ordered_quan3bins <- full_join(xgb_epi_ordered_pred_1week3bins_quan, xgb_epi_ordered_pred_2week3bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_epi_ordered_pred_3week3bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_epi_ordered_pred_4week3bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_epi_pred_all_ordered_uni3bins <- full_join(xgb_epi_ordered_pred_1week3bins_uni, xgb_epi_ordered_pred_2week3bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_epi_ordered_pred_3week3bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_epi_ordered_pred_4week3bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_epi_pred_all_ordered_quan5bins <- full_join(xgb_epi_ordered_pred_1week5bins_quan, xgb_epi_ordered_pred_2week5bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_epi_ordered_pred_3week5bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_epi_ordered_pred_4week5bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_epi_pred_all_ordered_uni5bins <- full_join(xgb_epi_ordered_pred_1week5bins_uni, xgb_epi_ordered_pred_2week5bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_epi_ordered_pred_3week5bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_epi_ordered_pred_4week5bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_epi_pred_all_ordered_quan10bins <- full_join(xgb_epi_ordered_pred_1week10bins_quan, xgb_epi_ordered_pred_2week10bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_epi_ordered_pred_3week10bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_epi_ordered_pred_4week10bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_epi_pred_all_ordered_uni10bins <- full_join(xgb_epi_ordered_pred_1week10bins_uni, xgb_epi_ordered_pred_2week10bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_epi_ordered_pred_3week10bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_epi_ordered_pred_4week10bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_epi_pred_all_ordered <- list(xgb_epi_pred_all_ordered_quan3bins = xgb_epi_pred_all_ordered_quan3bins,
                                 xgb_epi_pred_all_ordered_uni3bins = xgb_epi_pred_all_ordered_uni3bins,
                                 xgb_epi_pred_all_ordered_quan5bins = xgb_epi_pred_all_ordered_quan5bins,
                                 xgb_epi_pred_all_ordered_uni5bins = xgb_epi_pred_all_ordered_uni5bins,
                                 xgb_epi_pred_all_ordered_quan10bins = xgb_epi_pred_all_ordered_quan10bins,
                                 xgb_epi_pred_all_ordered_uni10bins = xgb_epi_pred_all_ordered_uni10bins)

saveRDS(xgb_epi_pred_all_ordered, "./saved_objects/xgb_pred_all_ordered_epi.rds")


######### Forecast - mobi+ weather ######
#' prepare data lists
# xgb_params <- list(booster = "gbtree", objective = "reg:logistic")

xgb_binary_mobiwea_list_1week3bins_quan <- xgb_binary_mobiwea(df_model_3bins, 1, "quantile", 3)
xgb_binary_mobiwea_list_2week3bins_quan <- xgb_binary_mobiwea(df_model_3bins, 2, "quantile", 3)
xgb_binary_mobiwea_list_3week3bins_quan <- xgb_binary_mobiwea(df_model_3bins, 3, "quantile", 3)
xgb_binary_mobiwea_list_4week3bins_quan <- xgb_binary_mobiwea(df_model_3bins, 4, "quantile", 3)

xgb_binary_mobiwea_list_1week3bins_uni <- xgb_binary_mobiwea(df_model_3bins, 1, "uniform", 3)
xgb_binary_mobiwea_list_2week3bins_uni <- xgb_binary_mobiwea(df_model_3bins, 2, "uniform", 3)
xgb_binary_mobiwea_list_3week3bins_uni <- xgb_binary_mobiwea(df_model_3bins, 3, "uniform", 3)
xgb_binary_mobiwea_list_4week3bins_uni <- xgb_binary_mobiwea(df_model_3bins, 4, "uniform", 3)

xgb_binary_mobiwea_list_1week5bins_uni <- xgb_binary_mobiwea(df_model_5bins, 1, "uniform", 5)
xgb_binary_mobiwea_list_2week5bins_uni <- xgb_binary_mobiwea(df_model_5bins, 2, "uniform", 5)
xgb_binary_mobiwea_list_3week5bins_uni <- xgb_binary_mobiwea(df_model_5bins, 3, "uniform", 5)
xgb_binary_mobiwea_list_4week5bins_uni <- xgb_binary_mobiwea(df_model_5bins, 4, "uniform", 5)

xgb_binary_mobiwea_list_1week5bins_quan <- xgb_binary_mobiwea(df_model_5bins, 1, "quantile", 5)
xgb_binary_mobiwea_list_2week5bins_quan <- xgb_binary_mobiwea(df_model_5bins, 2, "quantile", 5)
xgb_binary_mobiwea_list_3week5bins_quan <- xgb_binary_mobiwea(df_model_5bins, 3, "quantile", 5)
xgb_binary_mobiwea_list_4week5bins_quan <- xgb_binary_mobiwea(df_model_5bins, 4, "quantile", 5)

xgb_binary_mobiwea_list_1week10bins_uni <- xgb_binary_mobiwea(df_model_10bins, 1, "uniform", 10)
xgb_binary_mobiwea_list_2week10bins_uni <- xgb_binary_mobiwea(df_model_10bins, 2, "uniform", 10)
xgb_binary_mobiwea_list_3week10bins_uni <- xgb_binary_mobiwea(df_model_10bins, 3, "uniform", 10)
xgb_binary_mobiwea_list_4week10bins_uni <- xgb_binary_mobiwea(df_model_10bins, 4, "uniform", 10)

xgb_binary_mobiwea_list_1week10bins_quan <- xgb_binary_mobiwea(df_model_10bins, 1, "quantile", 10)
xgb_binary_mobiwea_list_2week10bins_quan <- xgb_binary_mobiwea(df_model_10bins, 2, "quantile", 10)
xgb_binary_mobiwea_list_3week10bins_quan <- xgb_binary_mobiwea(df_model_10bins, 3, "quantile", 10)
xgb_binary_mobiwea_list_4week10bins_quan <- xgb_binary_mobiwea(df_model_10bins, 4, "quantile", 10)

#' number of bins = 3
xgb_mobiwea_ordered_pred_1week3bins_quan <- xgb_ordered3bins_pred(xgb_binary_mobiwea_list_1week3bins_quan, df_model_3bins,
                                                              1, "quantile", xgb_params_order, c(1:2), 
                                                              2020, 2021, 2022, 2022)
xgb_mobiwea_ordered_pred_2week3bins_quan <- xgb_ordered3bins_pred(xgb_binary_mobiwea_list_2week3bins_quan, df_model_3bins,
                                                              2, "quantile", xgb_params_order, c(1:2), 
                                                              2020, 2021, 2022, 2022)
xgb_mobiwea_ordered_pred_3week3bins_quan <- xgb_ordered3bins_pred(xgb_binary_mobiwea_list_3week3bins_quan, df_model_3bins,
                                                              3, "quantile", xgb_params_order, c(1:2), 
                                                              2020, 2021, 2022, 2022)
xgb_mobiwea_ordered_pred_4week3bins_quan <- xgb_ordered3bins_pred(xgb_binary_mobiwea_list_4week3bins_quan, df_model_3bins,
                                                              4, "quantile", xgb_params_order, c(1:2), 
                                                              2020, 2021, 2022, 2022)

xgb_mobiwea_ordered_pred_1week3bins_uni <- xgb_ordered3bins_pred(xgb_binary_mobiwea_list_1week3bins_uni, df_model_3bins,
                                                             1, "uniform", xgb_params_order, c(1:2), 
                                                             2020, 2021, 2022, 2022)
xgb_mobiwea_ordered_pred_2week3bins_uni <- xgb_ordered3bins_pred(xgb_binary_mobiwea_list_2week3bins_uni, df_model_3bins,
                                                             2, "uniform", xgb_params_order, c(1:2), 
                                                             2020, 2021, 2022, 2022)
xgb_mobiwea_ordered_pred_3week3bins_uni <- xgb_ordered3bins_pred(xgb_binary_mobiwea_list_3week3bins_uni, df_model_3bins,
                                                             3, "uniform", xgb_params_order, c(1:2), 
                                                             2020, 2021, 2022, 2022)
xgb_mobiwea_ordered_pred_4week3bins_uni <- xgb_ordered3bins_pred(xgb_binary_mobiwea_list_4week3bins_uni, df_model_3bins,
                                                             4, "uniform", xgb_params_order, c(1:2), 
                                                             2020, 2021, 2022, 2022)


#' number of bins = 5 
xgb_mobiwea_ordered_pred_1week5bins_quan <- xgb_ordered5bins_pred(xgb_binary_mobiwea_list_1week5bins_quan, df_model_5bins,
                                                              1, "quantile", xgb_params_order, c(1:4), 
                                                              2020, 2021, 2022, 2022)
xgb_mobiwea_ordered_pred_2week5bins_quan <- xgb_ordered5bins_pred(xgb_binary_mobiwea_list_2week5bins_quan, df_model_5bins,
                                                              2, "quantile", xgb_params_order, c(1:4), 
                                                              2020, 2021, 2022, 2022)
xgb_mobiwea_ordered_pred_3week5bins_quan <- xgb_ordered5bins_pred(xgb_binary_mobiwea_list_3week5bins_quan, df_model_5bins,
                                                              3, "quantile", xgb_params_order, c(1:4), 
                                                              2020, 2021, 2022, 2022)
xgb_mobiwea_ordered_pred_4week5bins_quan <- xgb_ordered5bins_pred(xgb_binary_mobiwea_list_4week5bins_quan, df_model_5bins,
                                                              4, "quantile", xgb_params_order, c(1:4), 
                                                              2020, 2021, 2022, 2022)

xgb_mobiwea_ordered_pred_1week5bins_uni <- xgb_ordered5bins_pred(xgb_binary_mobiwea_list_1week5bins_uni, df_model_5bins,
                                                             1, "uniform", xgb_params_order, c(1:4), 
                                                             2020, 2021, 2022, 2022)
xgb_mobiwea_ordered_pred_2week5bins_uni <- xgb_ordered5bins_pred(xgb_binary_mobiwea_list_2week5bins_uni, df_model_5bins,
                                                             2, "uniform", xgb_params_order, c(1:4), 
                                                             2020, 2021, 2022, 2022)
xgb_mobiwea_ordered_pred_3week5bins_uni <- xgb_ordered5bins_pred(xgb_binary_mobiwea_list_3week5bins_uni, df_model_5bins,
                                                             3, "uniform", xgb_params_order, c(1:4), 
                                                             2020, 2021, 2022, 2022)
xgb_mobiwea_ordered_pred_4week5bins_uni <- xgb_ordered5bins_pred(xgb_binary_mobiwea_list_4week5bins_uni, df_model_5bins,
                                                             4, "uniform", xgb_params_order, c(1:4), 
                                                             2020, 2021, 2022, 2022)

#' number of bins = 10
xgb_mobiwea_ordered_pred_1week10bins_quan <- xgb_ordered10bins_pred(xgb_binary_mobiwea_list_1week10bins_quan, df_model_10bins,
                                                                1, "quantile", xgb_params_order, c(1:9), 
                                                                2020, 2021, 2022, 2022)
xgb_mobiwea_ordered_pred_2week10bins_quan <- xgb_ordered10bins_pred(xgb_binary_mobiwea_list_2week10bins_quan, df_model_10bins,
                                                                2, "quantile", xgb_params_order, c(1:9), 
                                                                2020, 2021, 2022, 2022)
xgb_mobiwea_ordered_pred_3week10bins_quan <- xgb_ordered10bins_pred(xgb_binary_mobiwea_list_3week10bins_quan, df_model_10bins,
                                                                3, "quantile", xgb_params_order, c(1:9), 
                                                                2020, 2021, 2022, 2022)
xgb_mobiwea_ordered_pred_4week10bins_quan <- xgb_ordered10bins_pred(xgb_binary_mobiwea_list_4week10bins_quan, df_model_10bins,
                                                                4, "quantile", xgb_params_order, c(1:9), 
                                                                2020, 2021, 2022, 2022)

xgb_mobiwea_ordered_pred_1week10bins_uni <- xgb_ordered10bins_pred(xgb_binary_mobiwea_list_1week10bins_uni, df_model_10bins,
                                                               1, "uniform", xgb_params_order, c(1:9), 
                                                               2020, 2021, 2022, 2022)
xgb_mobiwea_ordered_pred_2week10bins_uni <- xgb_ordered10bins_pred(xgb_binary_mobiwea_list_2week10bins_uni, df_model_10bins,
                                                               2, "uniform", xgb_params_order, c(1:9), 
                                                               2020, 2021, 2022, 2022)
xgb_mobiwea_ordered_pred_3week10bins_uni <- xgb_ordered10bins_pred(xgb_binary_mobiwea_list_3week10bins_uni, df_model_10bins,
                                                               3, "uniform", xgb_params_order, c(1:9), 
                                                               2020, 2021, 2022, 2022)
xgb_mobiwea_ordered_pred_4week10bins_uni <- xgb_ordered10bins_pred(xgb_binary_mobiwea_list_4week10bins_uni, df_model_10bins,
                                                               4, "uniform", xgb_params_order, c(1:9), 
                                                               2020, 2021, 2022, 2022)

xgb_mobiwea_pred_all_ordered_quan3bins <- full_join(xgb_mobiwea_ordered_pred_1week3bins_quan, xgb_mobiwea_ordered_pred_2week3bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_mobiwea_ordered_pred_3week3bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_mobiwea_ordered_pred_4week3bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_mobiwea_pred_all_ordered_uni3bins <- full_join(xgb_mobiwea_ordered_pred_1week3bins_uni, xgb_mobiwea_ordered_pred_2week3bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_mobiwea_ordered_pred_3week3bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_mobiwea_ordered_pred_4week3bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_mobiwea_pred_all_ordered_quan5bins <- full_join(xgb_mobiwea_ordered_pred_1week5bins_quan, xgb_mobiwea_ordered_pred_2week5bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_mobiwea_ordered_pred_3week5bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_mobiwea_ordered_pred_4week5bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_mobiwea_pred_all_ordered_uni5bins <- full_join(xgb_mobiwea_ordered_pred_1week5bins_uni, xgb_mobiwea_ordered_pred_2week5bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_mobiwea_ordered_pred_3week5bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_mobiwea_ordered_pred_4week5bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_mobiwea_pred_all_ordered_quan10bins <- full_join(xgb_mobiwea_ordered_pred_1week10bins_quan, xgb_mobiwea_ordered_pred_2week10bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_mobiwea_ordered_pred_3week10bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_mobiwea_ordered_pred_4week10bins_quan, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_mobiwea_pred_all_ordered_uni10bins <- full_join(xgb_mobiwea_ordered_pred_1week10bins_uni, xgb_mobiwea_ordered_pred_2week10bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_mobiwea_ordered_pred_3week10bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  full_join(., xgb_mobiwea_ordered_pred_4week10bins_uni, by = c("Year", "Week", "hosp_uni", "nhs_region")) %>%
  relocate(hosp_uni, .after = 4)

xgb_mobiwea_pred_all_ordered <- list(xgb_mobiwea_pred_all_ordered_quan3bins = xgb_mobiwea_pred_all_ordered_quan3bins,
                                 xgb_mobiwea_pred_all_ordered_uni3bins = xgb_mobiwea_pred_all_ordered_uni3bins,
                                 xgb_mobiwea_pred_all_ordered_quan5bins = xgb_mobiwea_pred_all_ordered_quan5bins,
                                 xgb_mobiwea_pred_all_ordered_uni5bins = xgb_mobiwea_pred_all_ordered_uni5bins,
                                 xgb_mobiwea_pred_all_ordered_quan10bins = xgb_mobiwea_pred_all_ordered_quan10bins,
                                 xgb_mobiwea_pred_all_ordered_uni10bins = xgb_mobiwea_pred_all_ordered_uni10bins)

saveRDS(xgb_mobiwea_pred_all_ordered, "./saved_objects/xgb_pred_all_ordered_mobiwea.rds")


############ feature importance #########
xgb_ordered_single_model <- function(data, nWeek_ahead, xgb_params,
                                     train_start_yr, train_end_yr,
                                     test_start_yr, test_end_yr){
  require(dplyr)
  require(xgboost)
  
  data_train <- data[which(data$Year %in% c(train_start_yr:train_end_yr)), ]
  data_test <- data[which(data$Year %in% c(test_start_yr:test_end_yr)), ]
  
  test_weeknum <- dim(unique(data_test[,c("Year", "Week")]))[1]
  
  single_class_model <- list()

  for (i in 0:(test_weeknum-1)){
    xgb_dat <- xgb_binary_data(data, i, train_start_yr, train_end_yr,
                               test_start_yr, test_end_yr)
    
    xgb_train <- xgb_dat[[1]]
    xgb_test <- xgb_dat[[2]]
    
    set.seed(999)
    xgb_model <- xgb.train(params = xgb_params, data = xgb_train, 
                           nrounds = 200)
    
    
    single_class_model[[i+1]] <- xgb_model
  }
  
  return(single_class_model)
}

test_xgb_ordered_single_model <- xgb_ordered_single_model(xgb_binary_mobiwea_list_1week10bins_quan[[1]],1,
                                                          xgb_params_order, 2020, 2021, 2022, 2022)

xgb_ordered_model <- function(data, nWeek_ahead, bins_type,
                              xgb_params,class,train_start_yr, train_end_yr,
                              test_start_yr, test_end_yr){
  require(dplyr)
  
  model_list <- NULL
  
  for (i in 1:length(class)){
    single_class_model <- xgb_ordered_single_model(data[[i]], nWeek_ahead, xgb_params,
                                                   train_start_yr, train_end_yr,
                                                   test_start_yr, test_end_yr)
    model_list[[i]] <- single_class_model
  }
  return(model_list)
}

feature_importance <- function(model_list){
  require(xgboost)
  require(dplyr)
  
  df_feature_imp <- NULL
  
  for (i in 1:length(model_list)){
    single_model_num <- length(model_list[[i]])
    for (j in 1:single_model_num){
      temp <- xgb.importance(colnames(model_list[[i]][[j]]), 
                             model = model_list[[i]][[j]])
      
      df_feature_imp <- rbind(df_feature_imp, temp)
    }
  }
  df_feature_imp <- df_feature_imp[order(-Gain),]
  df_feature <- df_feature_imp %>%
    group_by(Feature) %>%
    dplyr::summarise(Ave_gain = mean(Gain)) %>%
    arrange(desc(Ave_gain))
  
  return(df_feature)
}

xgb_mobiwea_ordered_pred_1week10bins_model <- xgb_ordered_model(xgb_binary_mobiwea_list_1week10bins_quan, 
                                                                1, "qunatile", xgb_params_order, c(1:9), 
                                                                2020, 2021, 2022, 2022)
xgb_feature_imp_1week10bins <- feature_importance(xgb_mobiwea_ordered_pred_1week10bins_model)

xgb_mobiwea_ordered_pred_2week10bins_model <- xgb_ordered_model(xgb_binary_mobiwea_list_2week10bins_quan, 
                                                                2, "qunatile", xgb_params_order, c(1:9), 
                                                                2020, 2021, 2022, 2022)
xgb_feature_imp_2week10bins <- feature_importance(xgb_mobiwea_ordered_pred_2week10bins_model)

xgb_mobiwea_ordered_pred_3week10bins_model <- xgb_ordered_model(xgb_binary_mobiwea_list_3week10bins_quan, 
                                                                3, "qunatile", xgb_params_order, c(1:9), 
                                                                2020, 2021, 2022, 2022)
xgb_feature_imp_3week10bins <- feature_importance(xgb_mobiwea_ordered_pred_3week10bins_model)

xgb_mobiwea_ordered_pred_4week10bins_model <- xgb_ordered_model(xgb_binary_mobiwea_list_4week10bins_quan, 
                                                                4, "qunatile", xgb_params_order, c(1:9), 
                                                                2020, 2021, 2022, 2022)
xgb_feature_imp_4week10bins <- feature_importance(xgb_mobiwea_ordered_pred_4week10bins_model)

xgb_feature_importance_quan10bins <- list(xgb_feature_imp_1week10bins = xgb_feature_imp_1week10bins,
                                    xgb_feature_imp_2week10bins = xgb_feature_imp_2week10bins,
                                    xgb_feature_imp_3week10bins = xgb_feature_imp_3week10bins,
                                    xgb_feature_imp_4week10bins = xgb_feature_imp_4week10bins)

saveRDS(xgb_feature_importance_quan10bins, "./saved_objects/xgb_feature_importance_quan10bins.rds")

xgb_feature_importance_quan10bins <- readRDS("./saved_objects/xgb_feature_importance_quan10bins.rds")

ave_gain_predHorizon <- rbind(xgb_feature_importance_quan10bins[["xgb_feature_imp_1week10bins"]],
                              xgb_feature_importance_quan10bins[["xgb_feature_imp_2week10bins"]]) %>% 
  rbind(.,xgb_feature_importance_quan10bins[["xgb_feature_imp_3week10bins"]]) %>% 
  rbind(.,xgb_feature_importance_quan10bins[["xgb_feature_imp_4week10bins"]])
 
pred_list <- c("hosplag_quan", "case", "death", "temperature",
               "parks", "grocery_pharmacy", "retail_recreation",
               "total.precipitation", "residential","workplaces",
               "transit_stations", "nhs_region")

ave_gain_predHorizon <- ave_gain_predHorizon %>% 
  mutate(Predictors = case_when(grepl("hosplag_quan", Feature)~"hosplag_quan",
                                grepl("case", Feature)~"case",
                                grepl("death", Feature)~"death",
                                grepl("temperature", Feature)~"temperature",
                                grepl("parks", Feature)~"parks",
                                grepl("grocery_pharmacy", Feature)~"grocery_pharmacy",
                                grepl("retail_recreation", Feature)~"retail_recreation",
                                grepl("total.precipitation", Feature)~"total.precipitation",
                                grepl("residential", Feature)~"residential",
                                grepl("workplaces", Feature)~"workplaces",
                                grepl("transit_stations", Feature)~"transit_stations",
                                grepl("nhs_region", Feature)~"nhs_region")) %>% 
  group_by(Predictors) %>%
  dplyr::summarise(Ave_gain = mean(Ave_gain)) %>%
  arrange(desc(Ave_gain))

saveRDS(ave_gain_predHorizon, "./saved_objects/ave_gain_predHorizon.rds")


########## test on pathological data #####
df_path_3bins <- df_path$df_path_3bins
df_path_5bins <- df_path$df_path_5bins
df_path_10bins <- df_path$df_path_10bins

df_path_3bins <- df_path_3bins %>%
  mutate(nhs_region = as.factor(nhs_region))
df_path_5bins <- df_path_5bins %>%
  mutate(nhs_region = as.factor(nhs_region))
df_path_10bins <- df_path_10bins %>%
  mutate(nhs_region = as.factor(nhs_region))

xgb_binary_list_1week3bins_path <- xgb_binary(df_path_3bins, 1, "quantile")
xgb_binary_list_2week3bins_path <- xgb_binary(df_path_3bins, 2, "quantile")
xgb_binary_list_3week3bins_path <- xgb_binary(df_path_3bins, 3, "quantile")
xgb_binary_list_4week3bins_path <- xgb_binary(df_path_3bins, 4, "quantile")

xgb_binary_list_1week5bins_path <- xgb_binary(df_path_5bins, 1, "quantile")
xgb_binary_list_2week5bins_path <- xgb_binary(df_path_5bins, 2, "quantile")
xgb_binary_list_3week5bins_path <- xgb_binary(df_path_5bins, 3, "quantile")
xgb_binary_list_4week5bins_path <- xgb_binary(df_path_5bins, 4, "quantile")

xgb_binary_list_1week10bins_path <- xgb_binary(df_path_10bins, 1, "quantile")
xgb_binary_list_2week10bins_path <- xgb_binary(df_path_10bins, 2, "quantile")
xgb_binary_list_3week10bins_path <- xgb_binary(df_path_10bins, 3, "quantile")
xgb_binary_list_4week10bins_path <- xgb_binary(df_path_10bins, 4, "quantile")

xgb_ordered_pred_1week3bins_path <- xgb_ordered3bins_pred(xgb_binary_list_1week3bins_path, df_path_3bins,
                                                          1, "quantile", xgb_params_order, c(1:2), 
                                                          2020, 2021, 2022, 2022)
xgb_ordered_pred_2week3bins_path <- xgb_ordered3bins_pred(xgb_binary_list_2week3bins_path, df_path_3bins,
                                                          2, "quantile", xgb_params_order, c(1:2), 
                                                          2020, 2021, 2022, 2022)
xgb_ordered_pred_3week3bins_path <- xgb_ordered3bins_pred(xgb_binary_list_3week3bins_path, df_path_3bins,
                                                          3, "quantile", xgb_params_order, c(1:2), 
                                                          2020, 2021, 2022, 2022)
xgb_ordered_pred_4week3bins_path <- xgb_ordered3bins_pred(xgb_binary_list_4week3bins_path, df_path_3bins,
                                                          4, "quantile", xgb_params_order, c(1:2), 
                                                          2020, 2021, 2022, 2022)

xgb_ordered_pred_1week5bins_path <- xgb_ordered5bins_pred(xgb_binary_list_1week5bins_path, df_path_5bins,
                                                          1, "quantile", xgb_params_order, c(1:4), 
                                                          2020, 2021, 2022, 2022)
xgb_ordered_pred_2week5bins_path <- xgb_ordered5bins_pred(xgb_binary_list_2week5bins_path, df_path_5bins,
                                                          2, "quantile", xgb_params_order, c(1:4), 
                                                          2020, 2021, 2022, 2022)
xgb_ordered_pred_3week5bins_path <- xgb_ordered5bins_pred(xgb_binary_list_3week5bins_path, df_path_5bins,
                                                          3, "quantile", xgb_params_order, c(1:4), 
                                                          2020, 2021, 2022, 2022)
xgb_ordered_pred_4week5bins_path <- xgb_ordered5bins_pred(xgb_binary_list_4week5bins_path, df_path_5bins,
                                                          4, "quantile", xgb_params_order, c(1:4), 
                                                          2020, 2021, 2022, 2022)

xgb_ordered_pred_1week10bins_path <- xgb_ordered10bins_pred(xgb_binary_list_1week10bins_path, df_path_10bins,
                                                            1, "quantile", xgb_params_order, c(1:9), 
                                                            2020, 2021, 2022, 2022)
xgb_ordered_pred_2week10bins_path <- xgb_ordered10bins_pred(xgb_binary_list_2week10bins_path, df_path_10bins,
                                                            2, "quantile", xgb_params_order, c(1:9), 
                                                            2020, 2021, 2022, 2022)
xgb_ordered_pred_3week10bins_path <- xgb_ordered10bins_pred(xgb_binary_list_3week10bins_path, df_path_10bins,
                                                            3, "quantile", xgb_params_order, c(1:9), 
                                                            2020, 2021, 2022, 2022)
xgb_ordered_pred_4week10bins_path <- xgb_ordered10bins_pred(xgb_binary_list_4week10bins_path, df_path_10bins,
                                                            4, "quantile", xgb_params_order, c(1:9), 
                                                            2020, 2021, 2022, 2022)

xgb_pred_all_ordered_path3bins <- full_join(xgb_ordered_pred_1week3bins_path, xgb_ordered_pred_2week3bins_path, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_ordered_pred_3week3bins_path, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_ordered_pred_4week3bins_path, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_pred_all_ordered_path5bins <- full_join(xgb_ordered_pred_1week5bins_path, xgb_ordered_pred_2week5bins_path, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_ordered_pred_3week5bins_path, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_ordered_pred_4week5bins_path, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_pred_all_ordered_path10bins <- full_join(xgb_ordered_pred_1week10bins_path, xgb_ordered_pred_2week10bins_path, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_ordered_pred_3week10bins_path, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  full_join(., xgb_ordered_pred_4week10bins_path, by = c("Year", "Week", "hosp_quan", "nhs_region")) %>%
  relocate(hosp_quan, .after = 4)

xgb_pred_all_ordered_path <- list(xgb_pred_all_ordered_path3bins = xgb_pred_all_ordered_path3bins,
                                  xgb_pred_all_ordered_path5bins = xgb_pred_all_ordered_path5bins,
                                  xgb_pred_all_ordered_path10bins = xgb_pred_all_ordered_path10bins)

saveRDS(xgb_pred_all_ordered_path, "./saved_objects/xgb_pred_all_ordered_path.rds")
