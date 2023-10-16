#' Make a note of when the report was generated.
Sys.time()
knitr.table.format = "markdown"

#' As always, remove all objects from the workspace before starting.
rm(list = ls(all = TRUE))

setwd("C:/Users/haowe/Desktop/COVID-forecasting")

#' Pull in packages needed
source("./functions/load_packages.R")
source("./functions/data_process.R")
source("./functions/fixed_polr.R")

pkgs <- c("haven", "xgboost", "stringr", "rasterVis","hrbrthemes",
          "dplyr", "ggplot2", "aweek", "surveillance", "plyr", 
          "mltools", "data.table")
load_package(pkgs)

#' Load original data
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

#' Load predictions
OLR_pred_all <- readRDS("./saved_objects/OLR_pred_all.rds")
null_model_pred <- readRDS("./saved_objects/null_model_pred.rds")

xgb_pred_all_null_epi <- readRDS("./saved_objects/xgb_pred_all_null_epi.rds")
xgb_pred_all_ordered_epi <- readRDS("./saved_objects/xgb_pred_all_ordered_epi.rds")

xgb_pred_all_null_mobi <- readRDS("./saved_objects/xgb_pred_all_null_mobi.rds")
xgb_pred_all_ordered_mobi <- readRDS("./saved_objects/xgb_pred_all_ordered_mobi.rds")

xgb_pred_all_null_mobiwea <- readRDS("./saved_objects/xgb_pred_all_null_mobiwea.rds")
xgb_pred_all_ordered_mobiwea <- readRDS("./saved_objects/xgb_pred_all_ordered_mobiwea.rds")

xgb_pred_all_null_weather <- readRDS("./saved_objects/xgb_pred_all_null_weather.rds")
xgb_pred_all_ordered_weather <- readRDS("./saved_objects/xgb_pred_all_ordered_weather.rds")

nhs_region <- unique(as.character(df_model_10bins$nhs_region))

accuracy_metric <- function(Obs, Pred, num_category){
  require(Metrics)
  Obs <- as.integer(Obs)
  Accuracy <- length(which(Obs == Pred))/length(Obs)
  MZE <- 1 - Accuracy
  MAE <- mae(Obs, Pred)

  # macro_averaged MAE
  macroMAE_vec <- c()
  for (i in 1:num_category){
    if (i %in% Obs){
      Obs_mmae <- Obs[which(Obs == i)]
      Pred_mmae <- Pred[which(Obs == i)]
      macroMAE_tmp <- mae(Obs_mmae, Pred_mmae)
    }else{
      macroMAE_tmp <- 0 
    }
    macroMAE_vec <- c(macroMAE_vec, macroMAE_tmp)
  }
  macroMAE <- sum(macroMAE_vec)/num_category
  
  metric <- c(MZE, Accuracy, MAE, macroMAE)
  return(metric)
}

#' test accuracy_metric()
# test_accuracy_metric <- accuracy_metric(OLR_pred_all$OLR_pred_quan5bins_all$hosp_quan, 
#                                         OLR_pred_all$OLR_pred_quan5bins_all$`1week_pred`, 5)
# accuracy_metric(xgb_pred_all_null$hosp, xgb_pred_all_null[,"1week_pred"], 10)
# accuracy_metric(xgb_pred_all_ordered$hosp, xgb_pred_all_ordered[,"1week_pred"], 10)

compare_accuracy_region <- function(pred_result, region, num_category,
                                    time_horizon, bins_type){
  require(Metrics)
  require(dplyr)
  
  pred_result <- as.data.frame(pred_result)
  pred_res_region <- pred_result %>%
    filter(nhs_region == region)

  # if(region == "All"){
  #   pred_res_region <- pred_result 
  # }else{
  #   pred_res_region <- pred_result %>%
  #     filter(nhs_region == region) 
  # }
  
  metric_tab <- NULL
  if(bins_type == "uniform"){
    for (i in 1:time_horizon){
      metric <- accuracy_metric(pred_res_region$hosp_uni, pred_res_region[,paste0(i, "week_pred")],
                                num_category)
      metric_tab <- rbind(metric_tab, metric)
    }
  }
  
  if(bins_type == "quantile"){
    for (i in 1:time_horizon){
      metric <- accuracy_metric(pred_res_region$hosp_quan, pred_res_region[,paste0(i, "week_pred")],
                                num_category)
      metric_tab <- rbind(metric_tab, metric)
    }
  }

  colnames(metric_tab) <- c("MZE", "Accuracy", "MAE", "macroMAE")

  metric_tab <- metric_tab %>%
    as.data.frame() %>%
    mutate(nhs_Region = region,
           nWeek_ahead = c("1-week ahead", "2-week ahead", "3-week ahead", "4-week ahead")) %>%
    relocate(nhs_Region, .before = 1) %>%
    relocate(nWeek_ahead, .before = 2)
  
  return(metric_tab)
}

# temp_xgb_ordered <- compare_accuracy_region(xgb_pred_all_ordered$xgb_pred_all_ordered_quan5bins, 
#                                             nhs_region[1], 5, 4, "quantile")

average_acc <- function(acc_byRegion, time_horizon){
  ave_acc <- NULL
  
  for (i in 1:time_horizon){
    ave_MZE <- mean(acc_byRegion$MZE[which(acc_byRegion$nWeek_ahead == paste0(i, "-week ahead"))])
    ave_Acc <- mean(acc_byRegion$Accuracy[which(acc_byRegion$nWeek_ahead == paste0(i, "-week ahead"))])
    acc_MAE <- mean(acc_byRegion$MAE[which(acc_byRegion$nWeek_ahead == paste0(i, "-week ahead"))])
    acc_mMAE <- mean(acc_byRegion$macroMAE[which(acc_byRegion$nWeek_ahead == paste0(i, "-week ahead"))])
    
    acc_temp <- c(ave_MZE, ave_Acc, acc_MAE,acc_mMAE)
    ave_acc <- rbind(ave_acc, acc_temp)
  }
  colnames(ave_acc) <- c("MZE", "Accuracy", "MAE", "macroMAE")

  ave_acc <- ave_acc %>%
    as.data.frame() %>%
    mutate(nhs_Region = "Average",
           nWeek_ahead = c("1-week ahead", "2-week ahead", "3-week ahead", "4-week ahead")) %>%
    relocate(nhs_Region, .before = 1) %>%
    relocate(nWeek_ahead, .before = 2)
  
  acc_byRegion <- rbind(acc_byRegion, ave_acc)
  
  return(acc_byRegion)
}

################ calculate accuracy metrics #######

########## Accuracy - ordered logistic regression ########
#' ordered logistic regression
#' number of bins = 3
OLR_acc_uni3bins <- NULL

for (i in 1:length(nhs_region)){
  temp_OLR <- compare_accuracy_region(OLR_pred_all$OLR_pred_uni3bins_all, 
                                      nhs_region[i], 3, 4, "uniform")
  OLR_acc_uni3bins <- rbind(OLR_acc_uni3bins, temp_OLR)
} 
OLR_acc_uni3bins <- average_acc(OLR_acc_uni3bins, 4)

OLR_acc_uni3bins <- OLR_acc_uni3bins%>% 
  mutate(Model = "OLR",
         Bins_type = "Uniform",
         Bins_num = 3)

OLR_acc_quan3bins <- NULL

for (i in 1:length(nhs_region)){
  temp_OLR <- compare_accuracy_region(OLR_pred_all$OLR_pred_quan3bins_all, 
                                      nhs_region[i], 3, 4, "quantile")
  OLR_acc_quan3bins <- rbind(OLR_acc_quan3bins, temp_OLR)
} 
OLR_acc_quan3bins <- average_acc(OLR_acc_quan3bins, 4)

OLR_acc_quan3bins <- OLR_acc_quan3bins%>% 
  mutate(Model = "OLR",
         Bins_type = "Quantile",
         Bins_num = 3)


#' number of bins = 5 
OLR_acc_uni5bins <- NULL

for (i in 1:length(nhs_region)){
  temp_OLR <- compare_accuracy_region(OLR_pred_all$OLR_pred_uni5bins_all, 
                                      nhs_region[i], 5, 4, "uniform")
  OLR_acc_uni5bins <- rbind(OLR_acc_uni5bins, temp_OLR)
} 
OLR_acc_uni5bins <- average_acc(OLR_acc_uni5bins, 4)

OLR_acc_uni5bins <- OLR_acc_uni5bins%>% 
  mutate(Model = "OLR",
         Bins_type = "Uniform",
         Bins_num = 5)

OLR_acc_quan5bins <- NULL

for (i in 1:length(nhs_region)){
  temp_OLR <- compare_accuracy_region(OLR_pred_all$OLR_pred_quan5bins_all, 
                                      nhs_region[i], 5, 4, "quantile")
  OLR_acc_quan5bins <- rbind(OLR_acc_quan5bins, temp_OLR)
} 
OLR_acc_quan5bins <- average_acc(OLR_acc_quan5bins, 4)
OLR_acc_quan5bins <- OLR_acc_quan5bins%>% 
  mutate(Model = "OLR",
         Bins_type = "Quantile",
         Bins_num = 5)

#' number of bins = 10
OLR_acc_uni10bins <- NULL

for (i in 1:length(nhs_region)){
  temp_OLR <- compare_accuracy_region(OLR_pred_all$OLR_pred_uni10bins_all, 
                                      nhs_region[i], 10, 4, "uniform")
  OLR_acc_uni10bins <- rbind(OLR_acc_uni10bins, temp_OLR)
} 
OLR_acc_uni10bins <- average_acc(OLR_acc_uni10bins, 4)

OLR_acc_uni10bins <- OLR_acc_uni10bins%>% 
  mutate(Model = "OLR",
         Bins_type = "Uniform",
         Bins_num = 10)

OLR_acc_quan10bins <- NULL

for (i in 1:length(nhs_region)){
  temp_OLR <- compare_accuracy_region(OLR_pred_all$OLR_pred_quan10bins_all, 
                                      nhs_region[i], 10, 4, "quantile")
  OLR_acc_quan10bins <- rbind(OLR_acc_quan10bins, temp_OLR)
} 
OLR_acc_quan10bins <- average_acc(OLR_acc_quan10bins, 4)
OLR_acc_quan10bins <- OLR_acc_quan10bins%>% 
  mutate(Model = "OLR",
         Bins_type = "Quantile",
         Bins_num = 10)

########## Accuracy - null model ########
#' number of bins = 3
null_acc_uni3bins <- NULL
for (i in 1:length(nhs_region)){
  temp_null <- compare_accuracy_region(null_model_pred$null_model_uni3bins_pred, 
                                       nhs_region[i], 3, 4,"uniform")
  null_acc_uni3bins <- rbind(null_acc_uni3bins, temp_null)
} 
null_acc_uni3bins <- average_acc(null_acc_uni3bins, 4)
null_acc_uni3bins <- null_acc_uni3bins%>% 
  mutate(Model = "Null Model",
         Bins_type = "Uniform",
         Bins_num = 3)

null_acc_quan3bins <- NULL
for (i in 1:length(nhs_region)){
  temp_null <- compare_accuracy_region(null_model_pred$null_model_quan3bins_pred, 
                                       nhs_region[i], 3, 4,"quantile")
  null_acc_quan3bins <- rbind(null_acc_quan3bins, temp_null)
} 
null_acc_quan3bins <- average_acc(null_acc_quan3bins, 4)
null_acc_quan3bins <- null_acc_quan3bins%>% 
  mutate(Model = "Null Model",
         Bins_type = "Quantile",
         Bins_num = 3)

#' number of bins = 5
null_acc_uni5bins <- NULL
for (i in 1:length(nhs_region)){
  temp_null <- compare_accuracy_region(null_model_pred$null_model_uni5bins_pred, 
                                       nhs_region[i], 5, 4,"uniform")
  null_acc_uni5bins <- rbind(null_acc_uni5bins, temp_null)
} 
null_acc_uni5bins <- average_acc(null_acc_uni5bins, 4)
null_acc_uni5bins <- null_acc_uni5bins%>% 
  mutate(Model = "Null Model",
         Bins_type = "Uniform",
         Bins_num = 5)

null_acc_quan5bins <- NULL
for (i in 1:length(nhs_region)){
  temp_null <- compare_accuracy_region(null_model_pred$null_model_quan5bins_pred, 
                                       nhs_region[i], 5, 4,"quantile")
  null_acc_quan5bins <- rbind(null_acc_quan5bins, temp_null)
} 
null_acc_quan5bins <- average_acc(null_acc_quan5bins, 4)
null_acc_quan5bins <- null_acc_quan5bins%>% 
  mutate(Model = "Null Model",
         Bins_type = "Quantile",
         Bins_num = 5)

#' number of bins = 10
null_acc_uni10bins <- NULL
for (i in 1:length(nhs_region)){
  temp_null <- compare_accuracy_region(null_model_pred$null_model_uni10bins_pred, 
                                       nhs_region[i], 10, 4,"uniform")
  null_acc_uni10bins <- rbind(null_acc_uni10bins, temp_null)
} 
null_acc_uni10bins <- average_acc(null_acc_uni10bins, 4)
null_acc_uni10bins <- null_acc_uni10bins%>% 
  mutate(Model = "Null Model",
         Bins_type = "Uniform",
         Bins_num = 10)

null_acc_quan10bins <- NULL
for (i in 1:length(nhs_region)){
  temp_null <- compare_accuracy_region(null_model_pred$null_model_quan10bins_pred, 
                                       nhs_region[i], 10, 4,"quantile")
  null_acc_quan10bins <- rbind(null_acc_quan10bins, temp_null)
} 
null_acc_quan10bins <- average_acc(null_acc_quan10bins, 4)
null_acc_quan10bins <- null_acc_quan10bins%>% 
  mutate(Model = "Null Model",
         Bins_type = "Quantile",
         Bins_num = 10)

########### Accuracy - categorical XGBoost ############
#' XGBoost no order
xgb_null_pred_acc <- function(pred,nhs_region){
  # number of bins = 5
  xgboost_null_acc_quan3bins <- NULL
  for (i in 1:length(nhs_region)){
    temp_xgb_null <- compare_accuracy_region(pred$xgb_pred_quan3bins_all, 
                                             nhs_region[i], 3, 4, "quantile")
    xgboost_null_acc_quan3bins <- rbind(xgboost_null_acc_quan3bins, temp_xgb_null)
  } 
  xgboost_null_acc_quan3bins <- average_acc(xgboost_null_acc_quan3bins, 4)
  xgboost_null_acc_quan3bins <- xgboost_null_acc_quan3bins%>% 
    mutate(Model = "XGBoost category",
           Bins_type = "Quantile",
           Bins_num = 3)
  
  xgboost_null_acc_uni3bins <- NULL
  for (i in 1:length(nhs_region)){
    temp_xgb_null <- compare_accuracy_region(pred$xgb_pred_uni3bins_all, 
                                             nhs_region[i], 3, 4, "uniform")
    xgboost_null_acc_uni3bins <- rbind(xgboost_null_acc_uni3bins, temp_xgb_null)
  } 
  xgboost_null_acc_uni3bins <- average_acc(xgboost_null_acc_uni3bins, 4)
  xgboost_null_acc_uni3bins <- xgboost_null_acc_uni3bins%>% 
    mutate(Model = "XGBoost category",
           Bins_type = "Uniform",
           Bins_num = 3)
  # number of bins = 5
  xgboost_null_acc_uni5bins <- NULL
  for (i in 1:length(nhs_region)){
    temp_xgb_null <- compare_accuracy_region(pred$xgb_pred_uni5bins_all, 
                                             nhs_region[i], 5, 4, "uniform")
    xgboost_null_acc_uni5bins <- rbind(xgboost_null_acc_uni5bins, temp_xgb_null)
  } 
  xgboost_null_acc_uni5bins <- average_acc(xgboost_null_acc_uni5bins, 4)
  xgboost_null_acc_uni5bins <- xgboost_null_acc_uni5bins%>% 
    mutate(Model = "XGBoost category",
           Bins_type = "Uniform",
           Bins_num = 5)
  
  xgboost_null_acc_quan5bins <- NULL
  for (i in 1:length(nhs_region)){
    temp_xgb_null <- compare_accuracy_region(pred$xgb_pred_quan5bins_all, 
                                             nhs_region[i], 5, 4, "quantile")
    xgboost_null_acc_quan5bins <- rbind(xgboost_null_acc_quan5bins, temp_xgb_null)
  } 
  xgboost_null_acc_quan5bins <- average_acc(xgboost_null_acc_quan5bins, 4)
  xgboost_null_acc_quan5bins <- xgboost_null_acc_quan5bins%>% 
    mutate(Model = "XGBoost category",
           Bins_type = "Quantile",
           Bins_num = 5)
  
  # number of bins = 10
  xgboost_null_acc_uni10bins <- NULL
  for (i in 1:length(nhs_region)){
    temp_xgb_null <- compare_accuracy_region(pred$xgb_pred_uni10bins_all, 
                                             nhs_region[i], 10, 4, "uniform")
    xgboost_null_acc_uni10bins <- rbind(xgboost_null_acc_uni10bins, temp_xgb_null)
  } 
  xgboost_null_acc_uni10bins <- average_acc(xgboost_null_acc_uni10bins, 4)
  xgboost_null_acc_uni10bins <- xgboost_null_acc_uni10bins%>% 
    mutate(Model = "XGBoost category",
           Bins_type = "Uniform",
           Bins_num = 10)
  
  xgboost_null_acc_quan10bins <- NULL
  for (i in 1:length(nhs_region)){
    temp_xgb_null <- compare_accuracy_region(pred$xgb_pred_quan10bins_all, 
                                             nhs_region[i], 10, 4, "quantile")
    xgboost_null_acc_quan10bins <- rbind(xgboost_null_acc_quan10bins, temp_xgb_null)
  } 
  xgboost_null_acc_quan10bins <- average_acc(xgboost_null_acc_quan10bins, 4)
  xgboost_null_acc_quan10bins <- xgboost_null_acc_quan10bins%>% 
    mutate(Model = "XGBoost category",
           Bins_type = "Quantile",
           Bins_num = 10)
  acc_null_all <- list(xgboost_null_acc_quan3bins = xgboost_null_acc_quan3bins,
                       xgboost_null_acc_quan5bins = xgboost_null_acc_quan5bins,
                       xgboost_null_acc_quan10bins = xgboost_null_acc_quan10bins,
                       xgboost_null_acc_uni3bins = xgboost_null_acc_uni3bins,
                       xgboost_null_acc_uni5bins = xgboost_null_acc_uni5bins,
                       xgboost_null_acc_uni10bins = xgboost_null_acc_uni10bins)
  return(acc_null_all)
}

xgboost_null_acc_epi <- xgb_null_pred_acc(xgb_pred_all_null_epi,nhs_region)
xgboost_null_acc_mobi <- xgb_null_pred_acc(xgb_pred_all_null_mobi,nhs_region)
xgboost_null_acc_mobiwea <- xgb_null_pred_acc(xgb_pred_all_null_mobiwea,nhs_region)
xgboost_null_acc_weather <- xgb_null_pred_acc(xgb_pred_all_null_weather,nhs_region)


########### Accuracy - ordered XGBoost ############
#' ordered XGBoost 
xgb_ordered_pred_acc <- function(pred, features, 
                                 nhs_region){
  
  #' number of bins = 3
  xgboost_ordered_acc_quan3bins <- NULL

  for (i in 1:length(nhs_region)){
    temp_xgb_ordered <- compare_accuracy_region(pred[[paste0("xgb_",features,"_pred_all_ordered_quan3bins")]],
                                                nhs_region[i], 3, 4, "quantile")
    xgboost_ordered_acc_quan3bins <- rbind(xgboost_ordered_acc_quan3bins, temp_xgb_ordered)
  } 

  xgboost_ordered_acc_quan3bins <- average_acc(xgboost_ordered_acc_quan3bins, 4)
  xgboost_ordered_acc_quan3bins <- xgboost_ordered_acc_quan3bins%>% 
    mutate(Model = "XGBoost ordered",
           Bins_type = "Quantile",
           Bins_num = 3)

  xgboost_ordered_acc_uni3bins <- NULL
  for (i in 1:length(nhs_region)){
    temp_xgb_ordered <- compare_accuracy_region(pred[[paste0("xgb_",features,"_pred_all_ordered_uni3bins")]],
                                                nhs_region[i], 3, 4, "uniform")
    xgboost_ordered_acc_uni3bins <- rbind(xgboost_ordered_acc_uni3bins, temp_xgb_ordered)
  } 
  xgboost_ordered_acc_uni3bins <- average_acc(xgboost_ordered_acc_uni3bins, 4)
  xgboost_ordered_acc_uni3bins <- xgboost_ordered_acc_uni3bins%>% 
    mutate(Model = "XGBoost ordered",
           Bins_type = "Uniform",
           Bins_num = 3)
  
  #' number of bins = 5
  xgboost_ordered_acc_uni5bins <- NULL
  for (i in 1:length(nhs_region)){
    temp_xgb_ordered <- compare_accuracy_region(pred[[paste0("xgb_",features,"_pred_all_ordered_uni5bins")]], 
                                                nhs_region[i], 5, 4, "uniform")
    xgboost_ordered_acc_uni5bins <- rbind(xgboost_ordered_acc_uni5bins, temp_xgb_ordered)
  } 
  xgboost_ordered_acc_uni5bins <- average_acc(xgboost_ordered_acc_uni5bins, 4)
  xgboost_ordered_acc_uni5bins <- xgboost_ordered_acc_uni5bins%>% 
    mutate(Model = "XGBoost ordered",
           Bins_type = "Uniform",
           Bins_num = 5)
  
  xgboost_ordered_acc_quan5bins <- NULL
  for (i in 1:length(nhs_region)){
    temp_xgb_ordered <- compare_accuracy_region(pred[[paste0("xgb_",features,"_pred_all_ordered_quan5bins")]],
                                                nhs_region[i], 5, 4, "quantile")
    xgboost_ordered_acc_quan5bins <- rbind(xgboost_ordered_acc_quan5bins, temp_xgb_ordered)
  } 
  xgboost_ordered_acc_quan5bins <- average_acc(xgboost_ordered_acc_quan5bins, 4)
  xgboost_ordered_acc_quan5bins <- xgboost_ordered_acc_quan5bins%>% 
    mutate(Model = "XGBoost ordered",
           Bins_type = "Quantile",
           Bins_num = 5)
  
  #' number of bins = 10
  xgboost_ordered_acc_uni10bins <- NULL
  for (i in 1:length(nhs_region)){
    temp_xgb_ordered <- compare_accuracy_region(pred[[paste0("xgb_",features,"_pred_all_ordered_uni10bins")]], 
                                                nhs_region[i], 10, 4, "uniform")
    xgboost_ordered_acc_uni10bins <- rbind(xgboost_ordered_acc_uni10bins, temp_xgb_ordered)
  } 
  xgboost_ordered_acc_uni10bins <- average_acc(xgboost_ordered_acc_uni10bins, 4)
  xgboost_ordered_acc_uni10bins <- xgboost_ordered_acc_uni10bins%>% 
    mutate(Model = "XGBoost ordered",
           Bins_type = "Uniform",
           Bins_num = 10)
  
  xgboost_ordered_acc_quan10bins <- NULL
  for (i in 1:length(nhs_region)){
    temp_xgb_ordered <- compare_accuracy_region(pred[[paste0("xgb_",features,"_pred_all_ordered_quan10bins")]],
                                                nhs_region[i], 10, 4, "quantile")
    xgboost_ordered_acc_quan10bins <- rbind(xgboost_ordered_acc_quan10bins, temp_xgb_ordered)
  } 
  xgboost_ordered_acc_quan10bins <- average_acc(xgboost_ordered_acc_quan10bins, 4)
  xgboost_ordered_acc_quan10bins <- xgboost_ordered_acc_quan10bins%>% 
    mutate(Model = "XGBoost ordered",
           Bins_type = "Quantile",
           Bins_num = 10)
  
  acc_order_all <- list(xgboost_ordered_acc_quan3bins = xgboost_ordered_acc_quan3bins,
                        xgboost_ordered_acc_quan5bins = xgboost_ordered_acc_quan5bins,
                        xgboost_ordered_acc_quan10bins = xgboost_ordered_acc_quan10bins,
                        xgboost_ordered_acc_uni3bins = xgboost_ordered_acc_uni3bins,
                        xgboost_ordered_acc_uni5bins = xgboost_ordered_acc_uni5bins,
                        xgboost_ordered_acc_uni10bins = xgboost_ordered_acc_uni10bins)
  return(acc_order_all)
  
}


xgboost_ordered_acc_epi <- xgb_ordered_pred_acc(xgb_pred_all_ordered_epi, "epi",nhs_region)
xgboost_ordered_acc_mobi <- xgb_ordered_pred_acc(xgb_pred_all_ordered_mobi, "mobi",nhs_region)
xgboost_ordered_acc_mobiwea <- xgb_ordered_pred_acc(xgb_pred_all_ordered_mobiwea, "mobiwea",nhs_region)
xgboost_ordered_acc_weather <- xgb_ordered_pred_acc(xgb_pred_all_ordered_weather,"wea",nhs_region)


acc_all_epi <- rbind(OLR_acc_uni3bins, OLR_acc_quan3bins) %>% 
  rbind(., OLR_acc_uni5bins) %>% 
  rbind(., OLR_acc_quan5bins) %>%
  rbind(., OLR_acc_uni10bins) %>%
  rbind(., OLR_acc_quan10bins) %>%
  rbind(., null_acc_uni3bins) %>% 
  rbind(., null_acc_quan3bins) %>% 
  rbind(., null_acc_uni5bins) %>%
  rbind(., null_acc_quan5bins) %>%
  rbind(., null_acc_uni10bins) %>%
  rbind(., null_acc_quan10bins) %>%
  rbind(., xgboost_null_acc_epi$xgboost_null_acc_uni3bins) %>% 
  rbind(., xgboost_null_acc_epi$xgboost_null_acc_quan3bins) %>% 
  rbind(., xgboost_null_acc_epi$xgboost_null_acc_uni5bins) %>%
  rbind(., xgboost_null_acc_epi$xgboost_null_acc_quan5bins) %>%
  rbind(., xgboost_null_acc_epi$xgboost_null_acc_uni10bins) %>%
  rbind(., xgboost_null_acc_epi$xgboost_null_acc_quan10bins) %>%
  rbind(., xgboost_ordered_acc_epi$xgboost_ordered_acc_uni3bins) %>% 
  rbind(., xgboost_ordered_acc_epi$xgboost_ordered_acc_quan3bins) %>% 
  rbind(., xgboost_ordered_acc_epi$xgboost_ordered_acc_uni5bins) %>%
  rbind(., xgboost_ordered_acc_epi$xgboost_ordered_acc_quan5bins) %>%
  rbind(., xgboost_ordered_acc_epi$xgboost_ordered_acc_uni10bins) %>%
  rbind(., xgboost_ordered_acc_epi$xgboost_ordered_acc_quan10bins)
saveRDS(acc_all_epi, "./saved_objects/all_models_accuracy_metrics_epi.rds")
  
acc_all_mobi <- rbind(OLR_acc_uni3bins, OLR_acc_quan3bins) %>% 
  rbind(., OLR_acc_uni5bins) %>% 
  rbind(., OLR_acc_quan5bins) %>%
  rbind(., OLR_acc_uni10bins) %>%
  rbind(., OLR_acc_quan10bins) %>%
  rbind(., null_acc_uni3bins) %>% 
  rbind(., null_acc_quan3bins) %>% 
  rbind(., null_acc_uni5bins) %>%
  rbind(., null_acc_quan5bins) %>%
  rbind(., null_acc_uni10bins) %>%
  rbind(., null_acc_quan10bins) %>%
  rbind(., xgboost_null_acc_mobi$xgboost_null_acc_uni3bins) %>% 
  rbind(., xgboost_null_acc_mobi$xgboost_null_acc_quan3bins) %>% 
  rbind(., xgboost_null_acc_mobi$xgboost_null_acc_uni5bins) %>%
  rbind(., xgboost_null_acc_mobi$xgboost_null_acc_quan5bins) %>%
  rbind(., xgboost_null_acc_mobi$xgboost_null_acc_uni10bins) %>%
  rbind(., xgboost_null_acc_mobi$xgboost_null_acc_quan10bins) %>%
  rbind(., xgboost_ordered_acc_mobi$xgboost_ordered_acc_uni3bins) %>% 
  rbind(., xgboost_ordered_acc_mobi$xgboost_ordered_acc_quan3bins) %>% 
  rbind(., xgboost_ordered_acc_mobi$xgboost_ordered_acc_uni5bins) %>%
  rbind(., xgboost_ordered_acc_mobi$xgboost_ordered_acc_quan5bins) %>%
  rbind(., xgboost_ordered_acc_mobi$xgboost_ordered_acc_uni10bins) %>%
  rbind(., xgboost_ordered_acc_mobi$xgboost_ordered_acc_quan10bins)

saveRDS(acc_all_mobi, "./saved_objects/all_models_accuracy_metrics_mobi.rds")

acc_all_mobiwea <- rbind(OLR_acc_uni3bins, OLR_acc_quan3bins) %>% 
  rbind(., OLR_acc_uni5bins) %>% 
  rbind(., OLR_acc_quan5bins) %>%
  rbind(., OLR_acc_uni10bins) %>%
  rbind(., OLR_acc_quan10bins) %>%
  rbind(., null_acc_uni3bins) %>% 
  rbind(., null_acc_quan3bins) %>% 
  rbind(., null_acc_uni5bins) %>%
  rbind(., null_acc_quan5bins) %>%
  rbind(., null_acc_uni10bins) %>%
  rbind(., null_acc_quan10bins) %>%
  rbind(., xgboost_null_acc_mobiwea$xgboost_null_acc_uni3bins) %>% 
  rbind(., xgboost_null_acc_mobiwea$xgboost_null_acc_quan3bins) %>% 
  rbind(., xgboost_null_acc_mobiwea$xgboost_null_acc_uni5bins) %>%
  rbind(., xgboost_null_acc_mobiwea$xgboost_null_acc_quan5bins) %>%
  rbind(., xgboost_null_acc_mobiwea$xgboost_null_acc_uni10bins) %>%
  rbind(., xgboost_null_acc_mobiwea$xgboost_null_acc_quan10bins) %>%
  rbind(., xgboost_ordered_acc_mobiwea$xgboost_ordered_acc_uni3bins) %>% 
  rbind(., xgboost_ordered_acc_mobiwea$xgboost_ordered_acc_quan3bins) %>% 
  rbind(., xgboost_ordered_acc_mobiwea$xgboost_ordered_acc_uni5bins) %>%
  rbind(., xgboost_ordered_acc_mobiwea$xgboost_ordered_acc_quan5bins) %>%
  rbind(., xgboost_ordered_acc_mobiwea$xgboost_ordered_acc_uni10bins) %>%
  rbind(., xgboost_ordered_acc_mobiwea$xgboost_ordered_acc_quan10bins)

saveRDS(acc_all_mobiwea, "./saved_objects/all_models_accuracy_metrics_mobiwea.rds")

acc_all_weather <- rbind(OLR_acc_uni3bins, OLR_acc_quan3bins) %>% 
  rbind(., OLR_acc_uni5bins) %>% 
  rbind(., OLR_acc_quan5bins) %>%
  rbind(., OLR_acc_uni10bins) %>%
  rbind(., OLR_acc_quan10bins) %>%
  rbind(., null_acc_uni3bins) %>% 
  rbind(., null_acc_quan3bins) %>% 
  rbind(., null_acc_uni5bins) %>%
  rbind(., null_acc_quan5bins) %>%
  rbind(., null_acc_uni10bins) %>%
  rbind(., null_acc_quan10bins) %>%
  rbind(., xgboost_null_acc_weather$xgboost_null_acc_uni3bins) %>% 
  rbind(., xgboost_null_acc_weather$xgboost_null_acc_quan3bins) %>% 
  rbind(., xgboost_null_acc_weather$xgboost_null_acc_uni5bins) %>%
  rbind(., xgboost_null_acc_weather$xgboost_null_acc_quan5bins) %>%
  rbind(., xgboost_null_acc_weather$xgboost_null_acc_uni10bins) %>%
  rbind(., xgboost_null_acc_weather$xgboost_null_acc_quan10bins) %>%
  rbind(., xgboost_ordered_acc_weather$xgboost_ordered_acc_uni3bins) %>% 
  rbind(., xgboost_ordered_acc_weather$xgboost_ordered_acc_quan3bins) %>% 
  rbind(., xgboost_ordered_acc_weather$xgboost_ordered_acc_uni5bins) %>%
  rbind(., xgboost_ordered_acc_weather$xgboost_ordered_acc_quan5bins) %>%
  rbind(., xgboost_ordered_acc_weather$xgboost_ordered_acc_uni10bins) %>%
  rbind(., xgboost_ordered_acc_weather$xgboost_ordered_acc_quan10bins)

saveRDS(acc_all_weather, "./saved_objects/all_models_accuracy_metrics_weather.rds")


######### pathological data ###########
null_model_pred_path <- readRDS( "./saved_objects/null_model_pred_path.rds")
xgb_pred_all_null_path <- readRDS("./saved_objects/xgb_pred_all_null_path.rds")
xgb_pred_all_ordered_path <- readRDS("./saved_objects/xgb_pred_all_ordered_path.rds")

xgboost_null_acc_path3bins <- NULL
for (i in 1:length(nhs_region)){
  temp_xgb_null <- compare_accuracy_region(xgb_pred_all_null_path$xgb_pred_path3bins_all, 
                                           nhs_region[i], 3, 4, "quantile")
  xgboost_null_acc_path3bins <- rbind(xgboost_null_acc_path3bins, temp_xgb_null)
} 
xgboost_null_acc_path3bins <- average_acc(xgboost_null_acc_path3bins, 4)
xgboost_null_acc_path3bins <- xgboost_null_acc_path3bins%>% 
  mutate(Model = "XGBoost category",
         Bins_type = "Quantile",
         Bins_num = 3)

xgboost_null_acc_path5bins <- NULL
for (i in 1:length(nhs_region)){
  temp_xgb_null <- compare_accuracy_region(xgb_pred_all_null_path$xgb_pred_path5bins_all, 
                                           nhs_region[i], 5, 4, "quantile")
  xgboost_null_acc_path5bins <- rbind(xgboost_null_acc_path5bins, temp_xgb_null)
} 
xgboost_null_acc_path5bins <- average_acc(xgboost_null_acc_path5bins, 4)
xgboost_null_acc_path5bins <- xgboost_null_acc_path5bins%>% 
  mutate(Model = "XGBoost category",
         Bins_type = "Quantile",
         Bins_num = 5)

xgboost_null_acc_path10bins <- NULL
for (i in 1:length(nhs_region)){
  temp_xgb_null <- compare_accuracy_region(xgb_pred_all_null_path$xgb_pred_path10bins_all, 
                                           nhs_region[i], 10, 4, "quantile")
  xgboost_null_acc_path10bins <- rbind(xgboost_null_acc_path10bins, temp_xgb_null)
} 
xgboost_null_acc_path10bins <- average_acc(xgboost_null_acc_path10bins, 4)
xgboost_null_acc_path10bins <- xgboost_null_acc_path10bins%>% 
  mutate(Model = "XGBoost category",
         Bins_type = "Quantile",
         Bins_num = 10)

xgboost_ordered_acc_path3bins <- NULL
for (i in 1:length(nhs_region)){
  temp_xgb_ordered <- compare_accuracy_region(xgb_pred_all_ordered_path$xgb_pred_all_ordered_path3bins,
                                              nhs_region[i], 3, 4, "quantile")
  xgboost_ordered_acc_path3bins <- rbind(xgboost_ordered_acc_path3bins, temp_xgb_ordered)
} 
xgboost_ordered_acc_path3bins <- average_acc(xgboost_ordered_acc_path3bins, 4)
xgboost_ordered_acc_path3bins <- xgboost_ordered_acc_path3bins%>% 
  mutate(Model = "XGBoost ordered",
         Bins_type = "Quantile",
         Bins_num = 3)

xgboost_ordered_acc_path5bins <- NULL
for (i in 1:length(nhs_region)){
  temp_xgb_ordered <- compare_accuracy_region(xgb_pred_all_ordered_path$xgb_pred_all_ordered_path5bins,
                                              nhs_region[i], 5, 4, "quantile")
  xgboost_ordered_acc_path5bins <- rbind(xgboost_ordered_acc_path5bins, temp_xgb_ordered)
} 
xgboost_ordered_acc_path5bins <- average_acc(xgboost_ordered_acc_path5bins, 4)
xgboost_ordered_acc_path5bins <- xgboost_ordered_acc_path5bins%>% 
  mutate(Model = "XGBoost ordered",
         Bins_type = "Quantile",
         Bins_num = 5)

xgboost_ordered_acc_path10bins <- NULL
for (i in 1:length(nhs_region)){
  temp_xgb_ordered <- compare_accuracy_region(xgb_pred_all_ordered_path$xgb_pred_all_ordered_path10bins,
                                              nhs_region[i], 10, 4, "quantile")
  xgboost_ordered_acc_path10bins <- rbind(xgboost_ordered_acc_path10bins, temp_xgb_ordered)
} 
xgboost_ordered_acc_path10bins <- average_acc(xgboost_ordered_acc_path10bins, 4)
xgboost_ordered_acc_path10bins <- xgboost_ordered_acc_path10bins%>% 
  mutate(Model = "XGBoost ordered",
         Bins_type = "Quantile",
         Bins_num = 10)

null_acc_path3bins <- NULL
for (i in 1:length(nhs_region)){
  temp_null <- compare_accuracy_region(null_model_pred_path$null_model_path3bins_pred, 
                                       nhs_region[i], 3, 4,"quantile")
  null_acc_path3bins <- rbind(null_acc_path3bins, temp_null)
} 
null_acc_path3bins <- average_acc(null_acc_path3bins, 4)
null_acc_path3bins <- null_acc_path3bins%>% 
  mutate(Model = "Null Model",
         Bins_type = "Quantile",
         Bins_num = 3)

null_acc_path5bins <- NULL
for (i in 1:length(nhs_region)){
  temp_null <- compare_accuracy_region(null_model_pred_path$null_model_path5bins_pred, 
                                       nhs_region[i], 5, 4,"quantile")
  null_acc_path5bins <- rbind(null_acc_path5bins, temp_null)
} 
null_acc_path5bins <- average_acc(null_acc_path5bins, 4)
null_acc_path5bins <- null_acc_path5bins%>% 
  mutate(Model = "Null Model",
         Bins_type = "Quantile",
         Bins_num = 5)

null_acc_path10bins <- NULL
for (i in 1:length(nhs_region)){
  temp_null <- compare_accuracy_region(null_model_pred_path$null_model_path10bins_pred, 
                                       nhs_region[i],10, 4,"quantile")
  null_acc_path10bins <- rbind(null_acc_path10bins, temp_null)
} 
null_acc_path10bins <- average_acc(null_acc_path10bins, 4)
null_acc_path10bins <- null_acc_path10bins%>% 
  mutate(Model = "Null Model",
         Bins_type = "Quantile",
         Bins_num = 10)

acc_all_path <- rbind(xgboost_null_acc_path3bins, xgboost_null_acc_path5bins) %>% 
  rbind(., xgboost_null_acc_path10bins) %>% 
  rbind(., xgboost_ordered_acc_path3bins) %>% 
  rbind(., xgboost_ordered_acc_path5bins) %>% 
  rbind(., xgboost_ordered_acc_path10bins) %>% 
  rbind(., null_acc_path3bins) %>% 
  rbind(., null_acc_path5bins) %>%
  rbind(., null_acc_path10bins)
saveRDS(acc_all_path, "./saved_objects/all_models_accuracy_metrics_path.rds")
