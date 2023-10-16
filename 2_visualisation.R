#' Make a note of when the report was generated.
Sys.time()
knitr.table.format = "markdown"

#' As always, remove all objects from the workspace before starting.
rm(list = ls(all = TRUE))

#' Set your own work path
# setwd()

#' Pull in packages needed
source("./functions/load_packages.R")
source("./functions/data_process.R")

pkgs <- c("xgboost", "stringr", "rasterVis", "hrbrthemes",
          "dplyr", "ggplot2", "aweek", "maps", "scales", "ggpubr", 
          "cowplot", "viridis","RColorBrewer","wesanderson")
load_package(pkgs)

#' Load original data
df_model_3bins <- readRDS("./saved_objects/df_model_epi3bins.rds")
df_model_5bins <- readRDS("./saved_objects/df_model_epi5bins.rds")
df_model_10bins <- readRDS("./saved_objects/df_model_epi10bins.rds")
df_epi_mobiwea <- readRDS("./saved_objects/df_raw_epi_mobiwea.rds")
df_raw <- readRDS("./saved_objects/df_adj.rds")

df_model_3bins <- df_model_3bins %>%
  mutate(nhs_region = as.factor(nhs_region)) %>% 
  dplyr::filter(!is.na(hosp_quan)) %>% 
  dplyr::filter(Year %in% c(2020:2022))

df_model_5bins <- df_model_5bins %>%
  mutate(nhs_region = as.factor(nhs_region))%>% 
  dplyr::filter(!is.na(hosp_quan))%>% 
  dplyr::filter(Year %in% c(2020:2022))

df_model_10bins <- df_model_10bins %>%
  mutate(nhs_region = as.factor(nhs_region))%>% 
  dplyr::filter(!is.na(hosp_quan))%>% 
  dplyr::filter(Year %in% c(2020:2022))

df_raw <- df_raw %>%
  dplyr::select(nhs_region, Date, Year, Week, newAdmissions) %>%
  dplyr::filter(!is.na(newAdmissions))

df_model_3bins <- dplyr::full_join(df_model_3bins, df_raw, by = c("nhs_region", "Date", "Year", "Week"))
df_model_5bins <- dplyr::full_join(df_model_5bins, df_raw, by = c("nhs_region", "Date", "Year", "Week"))
df_model_10bins <- dplyr::full_join(df_model_10bins, df_raw, by = c("nhs_region", "Date", "Year", "Week"))

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

#' Load accuracy metrics table
acc_metrics_tab_epi <- readRDS("./saved_objects/all_models_accuracy_metrics_epi.rds")
acc_metrics_tab_mobi <- readRDS("./saved_objects/all_models_accuracy_metrics_mobi.rds")
acc_metrics_tab_mobiwea <- readRDS("./saved_objects/all_models_accuracy_metrics_mobiwea.rds")
acc_metrics_tab_weather <- readRDS("./saved_objects/all_models_accuracy_metrics_weather.rds")

############### Figure 1 ###############
epi_curve_plot <- function(data, class_num){
  # East of England
  df_EE <- data[which(data$nhs_region=="East of England"), ]
  # browser()
  EE <- ggplot(df_EE, aes(x = Date)) +
    geom_line(aes(y = newAdmissions, color = "Hospital admission cases"),
              linewidth = 1.5) +
    geom_line(aes(y = as.numeric(hosp_quan)*(3000/class_num), color = "Hospital admission level (n-tile)"),
              linewidth = 1.5) +
    geom_point(aes(y = as.numeric(hosp_quan)*(3000/class_num), color= "Hospital admission level (n-tile)"),
               size= 3)+
    geom_line(aes(y = as.numeric(hosp_uni)*(3000/class_num), color = "Hospital admission level (uniform)"),
              linewidth = 1.5) +
    geom_point(aes(y = as.numeric(hosp_uni)*(3000/class_num), color= "Hospital admission level (uniform)"),
               size= 3)+
    labs(title = "East of England",
         #colour = "Hospitalisations"
    )+
    scale_y_continuous(
      # Features of the first axis
      name = "Hospital admission cases",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~./(3000/class_num), name="Hospital admission level",
                          breaks = c(1:class_num))
    ) + 
    scale_color_manual(name = "Hospitalisation",
                       values = c("Hospital admission cases" = "#D6604D",
                                  "Hospital admission level (n-tile)" = "#4393C3",
                                  "Hospital admission level (uniform)" = "#E69F00"))+
    theme_bw()+ 
    theme(text = element_text(size = 22),
          axis.text=element_text(size=20),
          legend.text = element_text(size=24),
          legend.title = element_text(size=24))
  
  # London
  df_L <- data[which(data$nhs_region=="London"), ]
  LON <- ggplot(df_L, aes(x = Date)) +
    geom_line(aes(y = newAdmissions, color = "Hospital admission cases"),
              linewidth = 1.5) +
    geom_line(aes(y = as.numeric(hosp_quan)*(6000/class_num), color = "Hospital admission level (n-tile)"),
              linewidth = 1.5) +
    geom_point(aes(y = as.numeric(hosp_quan)*(6000/class_num), color= "Hospital admission level (n-tile)"),
               size= 3)+
    geom_line(aes(y = as.numeric(hosp_uni)*(6000/class_num), color = "Hospital admission level (uniform)"),
              linewidth = 1.5) +
    geom_point(aes(y = as.numeric(hosp_uni)*(6000/class_num), color= "Hospital admission level (uniform)"),
               size= 3)+
    labs(title = "London",
         # colour = "Hospitalisations"
    )+
    scale_y_continuous(
      # Features of the first axis
      name = "Hospital admission cases",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~./(6000/class_num), name="Hospital admission level",
                          breaks = c(1:class_num))) + 
    scale_color_manual(name = "Hospitalisation",
                       values = c("Hospital admission cases" = "#D6604D",
                                  "Hospital admission level (n-tile)" = "#4393C3",
                                  "Hospital admission level (uniform)" = "#E69F00"))+
    theme_bw()+ 
    theme(text = element_text(size = 22),
          axis.text=element_text(size=20),
          legend.text = element_text(size=20),
          legend.title = element_text(size=22))
  
  # Midland
  df_Mid <- data[which(data$nhs_region=="Midlands"), ]
  MID <- ggplot(df_Mid, aes(x = Date)) +
    geom_line(aes(y = newAdmissions, color = "Hospital admission cases"),
              linewidth = 1.5) +
    geom_line(aes(y = as.numeric(hosp_quan)*(5000/class_num), color = "Hospital admission level (n-tile)"),
              linewidth = 1.5) +
    geom_point(aes(y = as.numeric(hosp_quan)*(5000/class_num), color= "Hospital admission level (n-tile)"),
               size= 3)+
    geom_line(aes(y = as.numeric(hosp_uni)*(5000/class_num), color = "Hospital admission level (uniform)"),
              linewidth = 1.5) +
    geom_point(aes(y = as.numeric(hosp_uni)*(5000/class_num), color= "Hospital admission level (uniform)"),
               size= 3)+
    labs(title = "Midlands",
         # colour = "Hospitalisations"
    )+
    scale_y_continuous(
      # Features of the first axis
      name = "Hospital admission cases",
      breaks = c(0,1000, 2000, 3000, 4000, 5000),
      # Add a second axis and specify its features
      sec.axis = sec_axis(~./(5000/class_num), name="Hospital admission level",
                          breaks = c(1:class_num))) + 
    scale_color_manual(name = "Hospitalisation",
                       values = c("Hospital admission cases" = "#D6604D",
                                  "Hospital admission level (n-tile)" = "#4393C3",
                                  "Hospital admission level (uniform)" = "#E69F00"))+
    theme_bw()+ 
    theme(text = element_text(size = 22),
          axis.text=element_text(size=20),
          legend.text = element_text(size=20),
          legend.title = element_text(size=22))
  
  # North East and Yorkshire
  df_NEY <- data[which(data$nhs_region=="North East and Yorkshire"), ]
  NEY <- ggplot(df_NEY, aes(x = Date)) +
    geom_line(aes(y = newAdmissions, color = "Hospital admission cases"),
              linewidth = 1.5) +
    geom_line(aes(y = as.numeric(hosp_quan)*(3000/class_num), color = "Hospital admission level (n-tile)"),
              linewidth = 1.5) +
    geom_point(aes(y = as.numeric(hosp_quan)*(3000/class_num), color= "Hospital admission level (n-tile)"),
               size= 3)+
    geom_line(aes(y = as.numeric(hosp_uni)*(3000/class_num), color = "Hospital admission level (uniform)"),
              linewidth = 1.5) +
    geom_point(aes(y = as.numeric(hosp_uni)*(3000/class_num), color= "Hospital admission level (uniform)"),
               size= 3)+
    labs(title = "North East and Yorkshire",
         # colour = "Hospitalisations"
    )+
    scale_y_continuous(
      # Features of the first axis
      name = "Hospital admission cases",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~./(3000/class_num), name="Hospital admission level",
                          breaks = c(1:class_num))) + 
    scale_color_manual(name = "Hospitalisation",
                       values = c("Hospital admission cases" = "#D6604D",
                                  "Hospital admission level (n-tile)" = "#4393C3",
                                  "Hospital admission level (uniform)" = "#E69F00"))+
    theme_bw()+ 
    theme(text = element_text(size = 22),
          axis.text=element_text(size=20),
          legend.text = element_text(size=20),
          legend.title = element_text(size=22))
  
  # North West
  df_NW <- data[which(data$nhs_region=="North West"), ]
  NW <- ggplot(df_NW, aes(x = Date)) +
    geom_line(aes(y = newAdmissions, color = "Hospital admission cases"),
              linewidth = 1.5) +
    geom_line(aes(y = as.numeric(hosp_quan)*(3000/class_num), color = "Hospital admission level (n-tile)"),
              linewidth = 1.5) +
    geom_point(aes(y = as.numeric(hosp_quan)*(3000/class_num), color= "Hospital admission level (n-tile)"),
               size= 3)+
    geom_line(aes(y = as.numeric(hosp_uni)*(3000/class_num), color = "Hospital admission level (uniform)"),
              linewidth = 1.5) +
    geom_point(aes(y = as.numeric(hosp_uni)*(3000/class_num), color= "Hospital admission level (uniform)"),
               size= 3)+
    labs(title = "North West",
         # colour = "Hospitalisations"
    )+
    scale_y_continuous(
      # Features of the first axis
      name = "Hospital admission cases",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~./(3000/class_num), name="Hospital admission level",
                          breaks = c(1:class_num))) + 
    scale_color_manual(name = "Hospitalisation",
                       values = c("Hospital admission cases" = "#D6604D",
                                  "Hospital admission level (n-tile)" = "#4393C3",
                                  "Hospital admission level (uniform)" = "#E69F00"))+
    theme_bw()+ 
    theme(text = element_text(size = 22),
          axis.text=element_text(size=20),
          legend.text = element_text(size=20),
          legend.title = element_text(size=22))
  
  # South East
  df_SE <- data[which(data$nhs_region=="South East"), ]
  SE <- ggplot(df_SE, aes(x = Date)) +
    geom_line(aes(y = newAdmissions, color = "Hospital admission cases"),
              linewidth = 1.5) +
    geom_line(aes(y = as.numeric(hosp_quan)*(4500/class_num), color = "Hospital admission level (n-tile)"),
              linewidth = 1.5) +
    geom_point(aes(y = as.numeric(hosp_quan)*(4500/class_num), color= "Hospital admission level (n-tile)"),
               size= 3)+
    geom_line(aes(y = as.numeric(hosp_uni)*(4500/class_num), color = "Hospital admission level (uniform)"),
              linewidth = 1.5) +
    geom_point(aes(y = as.numeric(hosp_uni)*(4500/class_num), color= "Hospital admission level (uniform)"),
               size= 3)+
    labs(title = "South East",
         # colour = "Hospitalisations"
    )+
    scale_y_continuous(
      # Features of the first axis
      name = "Hospital admission cases",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~./(4500/class_num), name="Hospital admission level",
                          breaks = c(1:class_num))) + 
    scale_color_manual(name = "Hospitalisation",
                       values = c("Hospital admission cases" = "#D6604D",
                                  "Hospital admission level (n-tile)" = "#4393C3",
                                  "Hospital admission level (uniform)" = "#E69F00"))+
    theme_bw()+ 
    theme(text = element_text(size = 22),
          axis.text=element_text(size=20),
          legend.text = element_text(size=20),
          legend.title = element_text(size=22))
  
  # South West
  df_SW <- data[which(data$nhs_region=="South West"), ]
  SW <- ggplot(df_SW, aes(x = Date)) +
    geom_line(aes(y = newAdmissions, color = "Hospital admission cases"),
              linewidth = 1.5) +
    geom_line(aes(y = as.numeric(hosp_quan)*(2000/class_num), color = "Hospital admission level (n-tile)"),
              linewidth = 1.5) +
    geom_point(aes(y = as.numeric(hosp_quan)*(2000/class_num), color= "Hospital admission level (n-tile)"),
               size= 3)+
    geom_line(aes(y = as.numeric(hosp_uni)*(2000/class_num), color = "Hospital admission level (uniform)"),
              linewidth = 1.5) +
    geom_point(aes(y = as.numeric(hosp_uni)*(2000/class_num), color= "Hospital admission level (uniform)"),
               size= 3)+
    labs(title = "South West",
         # colour = "Hospitalisations"
    )+
    scale_y_continuous(
      # Features of the first axis
      name = "Hospital admission cases",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~./(2000/class_num), name="Hospital admission level",
                          breaks = c(1:class_num))) + 
    scale_color_manual(name = "Hospitalisation",
                       values = c("Hospital admission cases" = "#D6604D",
                                  "Hospital admission level (n-tile)" = "#4393C3",
                                  "Hospital admission level (uniform)" = "#E69F00"))+
    theme_bw()+ 
    theme(text = element_text(size = 22),
          axis.text=element_text(size=20),
          legend.text = element_text(size=20),
          legend.title = element_text(size=22))
  
  # put A-G into one figure
  final_plot <- ggarrange(EE,LON,MID, NEY,NW,SE,SW,
                          font.label = list(size = 22),
                          ncol = 2, nrow = 4, align = "v", 
                          # labels = c("A.","","","","","",""),
                          common.legend = TRUE, legend = "bottom")
  
  return(final_plot)
}

epi_curve <- epi_curve_plot(df_model_10bins,10)
epi_curve

ggsave(filename = "./figures/Figure1.pdf", width = 18, height = 20, dpi = 300, scale = 1)
ggsave(filename = "./figures/Figure1.png", width = 18, height = 20, dpi = 300, scale = 1)

# ggsave(filename = "./figures/epi_curve_byRegion_10bins.pdf", width = 18, height = 20, dpi = 300, scale = 1)
# ggsave(filename = "./figures/epi_curve_byRegion_10bins.png", width = 18, height = 20, dpi = 300, scale = 1)


##################################################################
region_names <- c(
  # "All" = "All",
  "East of England" = "East of \n England",
  "London" = "London",
  "Midlands" = "Midlands",
  "North East and Yorkshire" = "North East \nand Yorkshire",
  "North West" =  "North West",
  "South East" = "South East",
  "South West" = "South West")

ave_acc_metric_allFea <- read.csv("./results/ave_acc_metric_allFea.csv")

acc_metrics_tab_epi <- acc_metrics_tab_epi %>%
  mutate(Data = "Epi* features")%>%
  mutate(Bins_num = factor(Bins_num, levels = c("3", "5","10")),
         Bins_type = factor(Bins_type, levels = c("Uniform", "Quantile")))

ave_acc_metric_epi <- acc_metrics_tab_epi %>%
  filter(nhs_Region=="Average")

acc_metrics_tab_mobi <- acc_metrics_tab_mobi %>%
  mutate(Data = "Epi*+Mobility features")%>%
  mutate(Bins_num = factor(Bins_num, levels = c("3", "5","10")),
         Bins_type = factor(Bins_type, levels = c("Uniform", "Quantile")))

ave_acc_metric_mobi <- acc_metrics_tab_mobi %>%
  filter(nhs_Region=="Average")

acc_metrics_tab_mobiwea <- acc_metrics_tab_mobiwea %>%
  mutate(Data = "Epi*+Mobility+Weather features")%>%
  mutate(Bins_num = factor(Bins_num, levels = c("3", "5","10")),
         Bins_type = factor(Bins_type, levels = c("Uniform", "Quantile")))

ave_acc_metric_mobiwea <- acc_metrics_tab_mobiwea %>%
  filter(nhs_Region=="Average")

acc_metrics_tab_weather <- acc_metrics_tab_weather %>%
  mutate(Data = "Epi*+Weather features")%>%
  mutate(Bins_num = factor(Bins_num, levels = c("3", "5","10")),
         Bins_type = factor(Bins_type, levels = c("Uniform", "Quantile")))

ave_acc_metric_weather <- acc_metrics_tab_weather %>%
  filter(nhs_Region=="Average")

ave_acc_metric_allFea <- rbind(ave_acc_metric_epi, ave_acc_metric_mobi) %>%
  rbind(.,ave_acc_metric_mobiwea) %>%
  rbind(.,ave_acc_metric_weather)

write.csv(ave_acc_metric_allFea, "./results/ave_acc_metric_allFea.csv",
          row.names = FALSE)

#' for making tables in the paper
ave_acc_metric_epiweamob <- ave_acc_metric_allFea %>%
  filter(Data == "Epi*+Mobility+Weather features")

write.csv(ave_acc_metric_epiweamob, "./results/ave_acc_metric_epiweamob.csv",
          row.names = FALSE)

ave_acc_metric_allFea <- ave_acc_metric_allFea %>%
  mutate(Bins_type = ifelse(Bins_type == "Quantile", "n-tile", Bins_type))

ave_acc_metric_allFea <- ave_acc_metric_allFea %>% 
  filter(nWeek_ahead == "4-week ahead") %>%
  mutate(Data = factor(Data, levels = c("Epi* features", "Epi*+Weather features",
                                        "Epi*+Mobility features", 
                                        "Epi*+Mobility+Weather features")),
         Model = factor(Model, levels = c("Null Model", "OLR", 
                                          "XGBoost category", "XGBoost ordered")))


bins_num <- c("3 levels", "5 levels", "10 levels")
names(bins_num) <- c("3", "5", "10")


############### Figure 2 ################
ave_mMAE <- ggplot(ave_acc_metric_allFea[which(ave_acc_metric_allFea$Bins_type=="n-tile"),], 
                   aes(x = Model, y = macroMAE, fill = Data)) +
  geom_bar(stat="identity",position=position_dodge())+
  facet_grid(Bins_num ~ ., labeller = labeller(Bins_num = bins_num))+
  scale_x_discrete(# guide = guide_axis(n.dodge=2),
    labels = c("Null Model" = "Null Model","OLR" = "OLR",
               "XGBoost category" = "XGBoost\ncategory",
               "XGBoost ordered" = "XGBoost\nordered"))+
  labs(y = "macro-average Mean Absolute Error",
       x = "Model")+
  scale_fill_manual(values = wes_palette(name="Darjeeling1",4), 
                    labels = c("Epi* features","Epi*+Weather features",
                               "Epi*+Mobility\nfeatures",
                               "Epi*+Mobility+\nWeather features"))+
  theme_bw()+
  theme(text = element_text(size = 22),
        axis.text=element_text(size=20),
        # axis.text.x = element_text(angle = 45,hjust=1),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22),
        legend.position="bottom")

ave_acc <- ggplot(ave_acc_metric_allFea[which(ave_acc_metric_allFea$Bins_type=="n-tile"),], 
                  aes(x = Model, y = Accuracy, fill = Data)) +
  geom_bar(stat="identity",position=position_dodge())+
  facet_grid(Bins_num ~ ., labeller = labeller(Bins_num = bins_num))+
  scale_x_discrete(# guide = guide_axis(n.dodge=2),
    labels = c("Null Model" = "Null Model","OLR" = "OLR",
               "XGBoost category" = "XGBoost\ncategory",
               "XGBoost ordered" = "XGBoost\nordered"))+
  labs(y = "Accuracy",
       x = "Model")+
  scale_fill_manual(values = wes_palette(name="Darjeeling1",4), 
                    labels = c("Epi* features","Epi*+Weather features",
                               "Epi*+Mobility\nfeatures",
                               "Epi*+Mobility+\nWeather features"))+
  theme_bw()+
  theme(text = element_text(size = 22),
        axis.text=element_text(size=20),
        # axis.text.x = element_text(angle = 45,hjust=1),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22),
        legend.position="bottom")

ggarrange(ave_mMAE,ave_acc,
          ncol = 1, nrow = 2, align = "v",
          labels = c("A.", "B."), font.label = list(size = 22),
          vjust = 0.9, hjust = -0.2,
          common.legend = TRUE, legend = "bottom")
ggsave(filename = "./figures/Figure3.pdf", width = 12, height = 16, dpi = 300, scale = 1)
ggsave(filename = "./figures/Figure3.png", width = 12, height = 16, dpi = 300, scale = 1)

############### Figure 2 ###############

#' Compare mMAE of XGBoost models trained by different set of features
mMAE_compare_XGB <- rbind(ave_acc_metric_epi[which(ave_acc_metric_epi$Model %in% c("XGBoost category","XGBoost ordered")),],
                          ave_acc_metric_mobi[which(ave_acc_metric_mobi$Model %in% c("XGBoost category","XGBoost ordered")),],
                          ave_acc_metric_mobiwea[which(ave_acc_metric_mobiwea$Model %in% c("XGBoost category","XGBoost ordered")),],
                          ave_acc_metric_weather[which(ave_acc_metric_weather$Model %in% c("XGBoost category","XGBoost ordered")),]) %>% 
  mutate(Bins_type = ifelse(Bins_type == "Quantile", "n-tile", "Uniform")) %>%
  mutate(Bins_num = factor(Bins_num, levels = c("3","5","10")),
         Bins_type = factor(Bins_type, levels = c("Uniform", "n-tile")),
         Model = factor(Model, levels = c("XGBoost category","XGBoost ordered")),
         Data = factor(Data, levels = c("Epi* features", "Epi*+Weather features",
                                        "Epi*+Mobility features", 
                                        "Epi*+Mobility+Weather features"))) 

xgb_mMAE_compare <- ggplot(mMAE_compare_XGB[which(mMAE_compare_XGB$Bins_type=="n-tile"), ], 
                                aes(x = nWeek_ahead, y = macroMAE, group=Data))+
  geom_point(aes(color = Data), size = 3)+
  geom_line(aes(color = Data), linewidth = 1.5)+
  scale_x_discrete(labels=c("1","2" ,"3", "4"))+ 
  labs(y = "macro-average Mean Absolute Error",
       x = "n-week ahead")+
  scale_color_manual(values = c(wes_palette(name="Darjeeling1",2),
                                wes_palette(name="Cavalcanti1",1),
                                wes_palette(name="Moonrise3",1)), 
                     labels = c("Epi* features", "Epi*+Weather features",
                                "Epi*+Mobility features",
                                "Epi*+Mobility+Weather features"))+
  facet_grid(Bins_num ~ Model, labeller = labeller(Bins_num = bins_num))+
  theme_bw()+
  theme(text = element_text(size = 22),
        axis.text=element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22),
        legend.position="bottom") 

xgb_acc_compare <- ggplot(mMAE_compare_XGB[which(mMAE_compare_XGB$Bins_type=="n-tile"),], 
                              aes(x = nWeek_ahead, y = Accuracy, group=Data))+
  geom_point(aes(color = Data), size = 3)+
  geom_line(aes(color = Data), linewidth = 1.5)+
  scale_x_discrete(labels=c("1","2" ,"3", "4"))+ 
  labs(y = "Accuracy",
       x = "n-week ahead")+
  scale_color_manual(values = c(wes_palette(name="Darjeeling1",2),
                                wes_palette(name="Cavalcanti1",1),
                                wes_palette(name="Moonrise3",1)), 
                     labels = c("Epi* features", "Epi*+Weather features",
                                "Epi*+Mobility features",
                                "Epi*+Mobility+Weather features"))+
  facet_grid(Bins_num ~ Model, labeller = labeller(Bins_num = bins_num))+
  theme_bw()+
  theme(text = element_text(size = 22),
        axis.text=element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22),
        legend.position="bottom")

ggarrange(xgb_mMAE_compare,xgb_acc_compare,
          ncol = 1, nrow = 2, align = "v",
          labels = c("A.", "B."), font.label = list(size = 22),
          vjust = 1, hjust = -0.8,
          common.legend = TRUE, legend = "bottom")

ggsave(filename = "./figures/Figure2.pdf", width = 14, height = 16, dpi = 300, scale = 1)
ggsave(filename = "./figures/Figure2.png", width = 14, height = 16, dpi = 300, scale = 1)

############### Figure S1 ################

#' Distribution of levels (uniform interval bins)
df_model_3bins <- df_model_3bins %>%
  dplyr::filter(Year != 2023)

prop_uni3bins <- with(df_model_3bins, table(hosp_uni)) %>% 
  prop.table() %>% 
  as.data.frame() %>% 
  dplyr::rename(Proportion = Freq,
                Hospitalisation_level = hosp_uni) %>% 
  mutate(Prop_percent = scales::percent(Proportion, accuracy = 0.01))

df_model_5bins <- df_model_5bins %>%
  filter(Year != 2023)

prop_uni5bins <- with(df_model_5bins, table(hosp_uni)) %>% 
  prop.table() %>% 
  as.data.frame() %>% 
  dplyr::rename(Proportion = Freq,
                Hospitalisation_level = hosp_uni) %>% 
  mutate(Prop_percent = scales::percent(Proportion, accuracy = 0.01))

df_model_10bins <- df_model_10bins %>%
  filter(Year != 2023)

prop_uni10bins <- with(df_model_10bins, table(hosp_uni)) %>% 
  prop.table() %>% 
  as.data.frame() %>% 
  dplyr::rename(Proportion = Freq,
                Hospitalisation_level = hosp_uni) %>% 
  mutate(Prop_percent = scales::percent(Proportion, accuracy = 0.01))

prop_bar1 <- ggplot(data=prop_uni3bins, aes(x=Hospitalisation_level, y=Proportion)) +
  geom_bar(stat="identity", width=1, colour = "white", fill="steelblue")+
  scale_x_discrete("Hospitalisation level", breaks = c(1:5))+
  coord_cartesian(ylim = c(0, 1))+
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1),
                     labels = c(0, 25, 50, 75, 100))+
  ylab("Proportion (%)")+
  labs(title = "3 levels")+
  # scale_y_continuous("Proportion",expand = c(0, 0),limits = c(0,1.00))+
  geom_text(aes(label=Prop_percent), vjust=-0.2, size=6)+
  theme_bw()+
  theme(text = element_text(size = 22),
        axis.text=element_text(size=20),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24))

prop_bar2 <- ggplot(data=prop_uni5bins, aes(x=Hospitalisation_level, y=Proportion)) +
  geom_bar(stat="identity", width=1, colour = "white", fill="steelblue")+
  scale_x_discrete("Hospitalisation level", breaks = c(1:5))+
  coord_cartesian(ylim = c(0, 1))+
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1),
                     labels = c(0, 25, 50, 75, 100))+
  ylab("Proportion (%)")+
  labs(title = "5 levels")+
  # scale_y_continuous("Proportion",expand = c(0, 0),limits = c(0,1.00))+
  geom_text(aes(label=Prop_percent), vjust=-0.2, hjust = 0.4, size=6)+
  theme_bw()+
  theme(text = element_text(size = 22),
        axis.text=element_text(size=20),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24))

prop_bar3 <- ggplot(data=prop_uni10bins, aes(x=Hospitalisation_level, y=Proportion)) +
  geom_bar(stat="identity", width=1, colour = "white", fill="steelblue")+
  scale_x_discrete("Hospitalisation level", breaks = c(1:10))+
  coord_cartesian(ylim = c(0, 1))+
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1),
                     labels = c(0, 25, 50, 75, 100))+
  ylab("Proportion (%)")+
  labs(title = "10 levels")+
  # scale_y_continuous("Proportion",expand = c(0, 0),limits = c(0,1.00))+
  geom_text(aes(label=Prop_percent),vjust= -0.5, hjust=0.1,size=4.5, angle = 55)+
  theme_bw()+
  theme(text = element_text(size = 22),
        axis.text=element_text(size=20),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24))

prop_bar <- ggarrange(prop_bar1, prop_bar2, prop_bar3,
                      labels = c("A.", "B.", "C."),
                      font.label = list(size = 22),
                      ncol = 3, nrow = 1, align = "v")

# prop_bar <- ggarrange(prop_bar1, prop_bar2, prop_bar3,
#                       labels = c("B.", "", ""),
#                       font.label = list(size = 22),
#                       ncol = 3, nrow = 1, align = "v")
prop_bar

ggsave(filename = "./figures/FigureS1B.pdf", width = 14, height = 8, dpi = 300, scale = 1)
ggsave(filename = "./figures/FigureS1B.png", width = 14, height = 8, dpi = 300, scale = 1)

# ggsave(filename = "./figures/prop_of_bins.pdf", width = 14, height = 8, dpi = 300, scale = 1)
# ggsave(filename = "./figures/prop_of_bins.png", width = 14, height = 8, dpi = 300, scale = 1)

# ggarrange(epi_curve, prop_bar, heights = c(2, 0.7),
#           ncol = 1, nrow = 2, align = "v")


############### Figure S2 ################
epi_curve_3bins <- epi_curve_plot(df_model_3bins,3)

ggarrange(epi_curve_3bins,
          ncol = 1, nrow = 1, align = "v",
          labels = c("A."), font.label = list(size = 22),
          vjust = 1, hjust = -1.2,
          common.legend = TRUE, legend = "bottom")  

 
ggsave(filename = "./figures/FigS2A.pdf", width = 18, height = 20, dpi = 300, scale = 1)
ggsave(filename = "./figures/FigS2A.png", width = 18, height = 20, dpi = 300, scale = 1)

epi_curve_5bins <- epi_curve_plot(df_model_5bins,5)
ggarrange(epi_curve_5bins,
          ncol = 1, nrow = 1, align = "v",
          labels = c("B."), font.label = list(size = 22),
          vjust = 1, hjust = -1.2,
          common.legend = TRUE, legend = "bottom")
ggsave(filename = "./figures/FigS2B.pdf", width = 18, height = 20, dpi = 300, scale = 1)
ggsave(filename = "./figures/FigS2B.png", width = 18, height = 20, dpi = 300, scale = 1)


############### Figure S3 ################
mMAE_compare_XGB_epi <- ave_acc_metric_epi %>% 
  mutate(Bins_type = ifelse(Bins_type == "Quantile", "n-tile", Bins_type)) %>%
  mutate(Bins_num = factor(Bins_num, levels = c("3","5","10")), 
         Model = factor(Model, levels = c("Null Model", "OLR", 
                                          "XGBoost category", "XGBoost ordered")))

xgbOrder_mMAE_compare_epi <- ggplot(mMAE_compare_XGB_epi[which(mMAE_compare_XGB_epi$Bins_type=="n-tile"),], 
                                     aes(x = nWeek_ahead, y = macroMAE, group=Model))+
  geom_point(aes(color = Model), size = 3)+
  geom_line(aes(color = Model), linewidth = 1.5)+
  scale_x_discrete(labels=c("1","2" ,"3", "4"))+ 
  labs(y = "macro-average Mean Absolute Error",
       x = "n-week ahead")+
  scale_color_manual(values = c(wes_palette(name="Royal1",2),
                                wes_palette(name="Cavalcanti1",1),
                                wes_palette(name="GrandBudapest2",1)), 
                     labels = c("Null Model" = "Null Model","OLR" = "OLR",
                                "XGBoost category" = "XGBoost\ncategory",
                                "XGBoost ordered" = "XGBoost\nordered"))+
  facet_grid(Bins_num ~ ., labeller = labeller(Bins_num = bins_num))+
  theme_bw()+
  theme(text = element_text(size = 22),
        axis.text=element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22),
        legend.position="bottom") 

xgbOrder_acc_compare_epi <- ggplot(mMAE_compare_XGB_epi[which(mMAE_compare_XGB_epi$Bins_type=="n-tile"),], 
                                   aes(x = nWeek_ahead, y = Accuracy, group=Model))+
  geom_point(aes(color = Model), size = 3)+
  geom_line(aes(color = Model), linewidth = 1.5)+
  scale_x_discrete(labels=c("1","2" ,"3", "4"))+ 
  labs(y = "Accuracy",
       x = "n-week ahead")+
  scale_color_manual(values = c(wes_palette(name="Royal1",2),
                                wes_palette(name="Cavalcanti1",1),
                                wes_palette(name="GrandBudapest2",1)), 
                     labels = c("Null Model" = "Null Model","OLR" = "OLR",
                                "XGBoost category" = "XGBoost\ncategory",
                                "XGBoost ordered" = "XGBoost\nordered"))+
  facet_grid(Bins_num ~ ., labeller = labeller(Bins_num = bins_num))+
  theme_bw()+
  theme(text = element_text(size = 22),
        axis.text=element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22),
        legend.position="bottom")

ggarrange(xgbOrder_mMAE_compare_epi,xgbOrder_acc_compare_epi,
          ncol = 1, nrow = 2, align = "v",
          labels = c("A.", "B."), font.label = list(size = 22),
          vjust = 0.9, hjust = -0.2,
          common.legend = TRUE, legend = "bottom")

ggsave(filename = "./figures/FigureS4.pdf", width = 12, height = 16, dpi = 300, scale = 1)
ggsave(filename = "./figures/FigureS4.png", width = 12, height = 16, dpi = 300, scale = 1)

############### Figure S4 ################
#' Compare mMAE of the XGBoost models trained by weather features
mMAE_compare_XGB_wea <- rbind(ave_acc_metric_epi[which(ave_acc_metric_epi$Model %in% c("XGBoost category","XGBoost ordered")),],
                              ave_acc_metric_weather[which(ave_acc_metric_weather$Model %in% c("XGBoost category","XGBoost ordered")),]) %>% 
  mutate(Bins_num = factor(Bins_num, levels = c("3","5","10")),
         # Bins_type = factor(Bins_type, levels = c("Uniform", "Quantile")),
         Model = factor(Model, levels = c("XGBoost category","XGBoost ordered")),
         Data = factor(Data, levels = c("Epi* features", "Epi*+Weather features"))) %>%
  mutate(Bins_type = ifelse(Bins_type == "Quantile", "n-tile", Bins_type))

xgb_mMAE_compare_wea <- ggplot(mMAE_compare_XGB_wea[which(mMAE_compare_XGB_wea$Bins_type=="n-tile"),], 
                                    aes(x = nWeek_ahead, y = macroMAE, group=Data))+
  geom_point(aes(color = Data), size = 3)+
  geom_line(aes(color = Data), linewidth = 1.5)+
  scale_x_discrete(labels=c("1","2" ,"3", "4"))+ 
  labs(y = "macro-average Mean Absolute Error",
       x = "n-week ahead")+
  scale_color_manual(values = c(wes_palette(name ="Darjeeling1",1),
                                wes_palette(name = "Zissou1",1)), 
                     labels = c("Epi* features", "Epi*+Weather features"))+
  facet_grid(Bins_num ~ Model, labeller = labeller(Bins_num = bins_num))+
  theme_bw()+
  theme(text = element_text(size = 22),
        axis.text=element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22),
        legend.position="bottom") 
xgb_mMAE_compare_wea

xgb_acc_compare_wea <- ggplot(mMAE_compare_XGB_wea[which(mMAE_compare_XGB_wea$Bins_type=="n-tile"),], 
                                  aes(x = nWeek_ahead, y = Accuracy, group=Data))+
  geom_point(aes(color = Data), size = 3)+
  geom_line(aes(color = Data), linewidth = 1.5)+
  scale_x_discrete(labels=c("1","2" ,"3", "4"))+ 
  labs(y = "Accuracy",
       x = "n-week ahead")+
  scale_color_manual(values = c(wes_palette(name ="Darjeeling1",1),
                                wes_palette(name = "Zissou1",1)), 
                     labels = c("Epi* features", "Epi*+Weather features"))+
  facet_grid(Bins_num ~ Model, labeller = labeller(Bins_num = bins_num))+
  theme_bw()+
  theme(text = element_text(size = 22),
        axis.text=element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22),
        legend.position="bottom")
xgb_acc_compare_wea

ggarrange(xgb_mMAE_compare_wea,xgb_acc_compare_wea,
          ncol = 1, nrow = 2, align = "v",
          labels = c("A.", "B."), font.label = list(size = 22),
          vjust = 0.9, hjust = -0.2,
          common.legend = TRUE, legend = "bottom")

ggsave(filename = "./figures/FigureS5.pdf", width = 12, height = 16, dpi = 300, scale = 1)
ggsave(filename = "./figures/FigureS5.png", width = 12, height = 16, dpi = 300, scale = 1)


############### Figure S5 ################
mMAE_compare_XGB_mobi <- rbind(ave_acc_metric_epi[which(ave_acc_metric_epi$Model %in% c("XGBoost category","XGBoost ordered")),],
                               ave_acc_metric_mobi[which(ave_acc_metric_mobi$Model %in% c("XGBoost category","XGBoost ordered")),]) %>% 
  mutate(Bins_num = factor(Bins_num, levels = c("3","5","10")),
         # Bins_type = factor(Bins_type, levels = c("Uniform", "Quantile")),
         Model = factor(Model, levels = c("XGBoost category","XGBoost ordered")),
         Data = factor(Data, levels = c("Epi* features", "Epi*+Mobility features")))%>%
  mutate(Bins_type = ifelse(Bins_type == "Quantile", "n-tile", Bins_type))

xgb_mMAE_compare_mobi <- ggplot(mMAE_compare_XGB_mobi[which(mMAE_compare_XGB_mobi$Bins_type=="n-tile"),], 
                                     aes(x = nWeek_ahead, y = macroMAE, group=Data))+
  geom_point(aes(color = Data), size = 3)+
  geom_line(aes(color = Data), linewidth = 1.5)+
  scale_x_discrete(labels=c("1","2" ,"3", "4"))+ 
  labs(y = "macro-average Mean Absolute Error",
       x = "n-week ahead")+
  scale_color_manual(values = wes_palette(name="Darjeeling1",2), 
                     labels = c("Epi* features", "Epi*+Mobility features"))+
  facet_grid(Bins_num ~ Model, labeller = labeller(Bins_num = bins_num))+
  theme_bw()+
  theme(text = element_text(size = 22),
        axis.text=element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22),
        legend.position="bottom") 

xgb_acc_compare_mobi <- ggplot(mMAE_compare_XGB_mobi[which(mMAE_compare_XGB_mobi$Bins_type=="n-tile"),], 
                                   aes(x = nWeek_ahead, y = Accuracy, group=Data))+
  geom_point(aes(color = Data), size = 3)+
  geom_line(aes(color = Data), linewidth = 1.5)+
  scale_x_discrete(labels=c("1","2" ,"3", "4"))+ 
  labs(y = "Accuracy",
       x = "n-week ahead")+
  scale_color_manual(values = wes_palette(name="Darjeeling1",2), 
                     labels = c("Epi* features", "Epi*+Mobility features"))+
  facet_grid(Bins_num ~ Model, labeller = labeller(Bins_num = bins_num))+
  theme_bw()+
  theme(text = element_text(size = 22),
        axis.text=element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22),
        legend.position="bottom")

ggarrange(xgb_mMAE_compare_mobi, xgb_acc_compare_mobi,
          ncol = 1, nrow = 2, align = "v",
          labels = c("A.", "B."), font.label = list(size = 22),
          vjust = 0.9, hjust = -0.2,
          common.legend = TRUE, legend = "bottom")
ggsave(filename = "./figures/FigureS6.pdf", width = 12, height = 16, dpi = 300, scale = 1)
ggsave(filename = "./figures/FigureS6.png", width = 12, height = 16, dpi = 300, scale = 1)
