# figure 4 code
# wateRtemp project
# Moritz Feigl, Nov 2020


library(ggplot2)
setwd("C:/Users/morit/Dropbox/WT_Project/Modelling/VSC_results/final_models")
years <- "2015"
catchment <- "Inn"

# get the best models for the given catchment
scores <- read.csv("../../../Manuscript/Paper 1 - ML focus/results_data/all_results_FINAL.csv")
scores <- scores[-grep("noFmon", scores$model_name), ]
names(scores)[3] <- "Catchment"
names(scores)[4] <- "Model"
catchments <- c("Kleine_Muehl", "Aschach", "Erlauf", "Traisen", "Ybbs", "Saalach", "Enns", "Inn", "Salzach", "Donau")
# air2stream_results <- data.frame("Catchment" = catchments,
#                                  Model = "air2stream",
#                                  "test_RMSE" = c(0.908, 1.147, 0.911, 0.948, 0.948, 0.802,
#                                                  1.168, 1.097, 0.743, 1.099),
#                                  "test_MAE"  = c(0.714, 0.882, 0.726, 0.747, 0.756, 0.646,
#                                                  0.671, 0.949, 0.595, 0.910)
# )
count <- 1
for(catchment in catchments){
  catch_data <- scores[scores$Catchment == catchment, ]
  catch_data <- catch_data[!is.na(catch_data$test_MAE), ]
  sub_data <- merge(aggregate(cv_or_validation_RMSE ~ Model , catch_data, min), catch_data,
                    by = c("Model", "cv_or_validation_RMSE"), all.x = TRUE, all.y = FALSE)
  # in case a model is double -> choose with cv MAE
  for(model in unique(sub_data$Model)){
    if(sum(sub_data$Model == model) > 1){
      remove_row <- which.max(sub_data[sub_data$Model == model, "cv_or_validation_MAE"])
      sub_data <- sub_data[-which(sub_data$Model == model)[remove_row], ]
    }
  }
  sub_data <- rbind(sub_data,
                    catch_data[catch_data$Model %in% c("LM", "air2stream"),])
  sub_data$diff <- sub_data$test_RMSE - min(sub_data$test_RMSE)
  
  cat(catchment, ":", sub_data$Model[which.min(sub_data$test_RMSE)], "\n")
  print(sub_data)
  cat("\n")
  
  # LM 
  LM <- read.csv(paste0(catchment, "/LM/LM/test_data_prediction.csv"))
  # stepLM 
  model_inputs <- sub_data[sub_data$Model == "stepLM", "model_name"]
  cv_mode <- sub_data[sub_data$Model == "stepLM", "cv_mode"]
  steptry <- try({stepLM <- read.csv(paste0(catchment, "/LM/stepLM/", model_inputs, "_", cv_mode,
                            "/test_data_prediction.csv"))}, silent = TRUE)
  if(class(steptry) == "try-error"){
    stepLM <- read.csv(paste0(catchment, "/LM/stepLM/", model_inputs,
                            "/test_data_prediction.csv"))
  }
  # RF
  model_inputs <- sub_data[sub_data$Model == "RF", "model_name"]
  cv_mode <- sub_data[sub_data$Model == "RF", "cv_mode"]
  RF <- read.csv(paste0(catchment, "/RF/", model_inputs, "_", cv_mode,
                        "/test_data_prediction.csv"))
  # XGBoost
  model_inputs <- sub_data[sub_data$Model == "XGBoost", "model_name"]
  cv_mode <- sub_data[sub_data$Model == "XGBoost", "cv_mode"]
  XGBoost <- read.csv(paste0(catchment, "/XGBoost/", model_inputs, "_", cv_mode,
                             "/test_data_prediction.csv"))
  # FNN 
  model_inputs <- sub_data[sub_data$Model == "FNN", "model_name"]
  FNN <- read.csv(paste0(catchment, "/FNN/", model_inputs,
                         "/test_data_prediction.csv"))
  # GRU 
  model_inputs <- sub_data[sub_data$Model == "RNN-GRU", "model_name"]
  GRU <- read.csv(paste0(catchment, "/RNN/GRU/", model_inputs,
                         "/test_data_prediction.csv"))
  # LSTM 
  model_inputs <- sub_data[sub_data$Model == "RNN-LSTM", "model_name"]
  LSTM <- read.csv(paste0(catchment, "/RNN/LSTM/", model_inputs,
                          "/test_data_prediction.csv"))
  # air2stream -----------------------------------------------------------------------------
  air2stream <- feather::read_feather(
    paste0("C:/Users/morit/Dropbox/WT_Project/Modelling/data_final/", catchment, 
           "/air2stream/predicted_values.feather"))
  
  # Plot data ------------------------------------------------------------------------------
  test_data <- feather::read_feather(paste0(catchment, "/test_radiation_data.feather"))
  test_length <- nrow(test_data)
  test_data$year <- format(test_data$date, format="%Y")
  plot_data <- data.frame(test_data[, c("date", "year", "wt")],
                          "LM" = tail(LM$predicted_wt, test_length),
                          "air2stream" = tail(air2stream$predicted_values, test_length),
                          "step LM" = tail(stepLM$predicted_wt, test_length),
                          "RF" = tail(RF$predicted_wt, test_length),
                          "XGBoost" = tail(XGBoost$predicted_wt, test_length),
                          "FNN" = tail(FNN$predicted_wt, test_length),
                          "RNN-GRU" = tail(GRU$predicted_wt, test_length),
                          "RNN-LSTM" = tail(LSTM$predicted_wt, test_length)
  )
  names(plot_data)[names(plot_data) == "wt"] <- "Observation"
  plot_xts <- xts::xts(plot_data[, -c(1, 2)],
                       order.by = plot_data$date)
  plot_xts <- plot_xts[years]
  names(plot_xts) <- c("Observation", "LM", "air2stream",
                       "stepLM", "RF","XGBoost",
                       "FNN", "RNN-GRU", "RNN-LSTM")
  
  
  sqrt(mean((tail(RF$predicted_wt, test_length) - test_data$wt)^2))
  # Plot -----------------------------------------------------------------------------------
  # Plot parameter
  line_width <- 0.75
  grey_alpha <- 0.65
  
  mscore <- sub_data[, c("Model", "test_RMSE", "test_MAE")]
  
  
  
  
  RMSE <- function(prediction, observation){
    round(sqrt(mean((prediction - observation)^2, na.rm = TRUE)), 3)
  }
  MAE <- function(prediction, observation){
    round(mean(abs(observation - prediction)), 3)
  }
  
  for(col in 4:11){
    mscore[mscore$Model == names(plot_data)[col], "test_RMSE"] <- RMSE(
      plot_data[, col], plot_data[, "Observation"])
    mscore[mscore$Model == names(plot_data)[col], "test_MAE"] <- MAE(
      plot_data[, col], plot_data[, "Observation"])
  }
  
  
  
  # observation plot
  ts_plot_data <- broom::tidy(plot_xts)
  colors <- c("Observation" = "deepskyblue3", "LM" = "grey", "air2stream" = "grey",
              "stepLM" = "grey", "RF" = "grey", "XGBoost" = "grey",
              "FNN" ="grey", "RNN-GRU" = "grey", "RNN-LSTM" = "grey")
  alphas <- rep(grey_alpha, 9)
  names(alphas) <- names(colors)
  plots <- list()
  for(model in names(colors)[-1]){
    colors[model] <- c("tomato2")
    alphas[model] <- 1
    ts_plot_data$series <- as.character(ts_plot_data$series)
    ts_plot_data$series <- factor(ts_plot_data$series,
                                  levels = c(
                                    names(colors)[!(names(colors) %in%
                                                      c("Observation", model))],
                                    "Observation", model))
    plots[[model]] <- ggplot(ts_plot_data,
                             aes(x = index, y = value, color = series)) +
      geom_line(lwd = line_width) +
      xlab("") + ylab("") +
      scale_color_manual("", values = colors) +
      scale_alpha_manual(values = alphas) +
      ggtitle(paste0(model, " | RMSE = ", mscore$test_RMSE[mscore$Model == model], " °C",
                     " | MAE = ", mscore$test_MAE[mscore$Model == model], " °C")) +
      #theme_classic() +
      theme(panel.background = element_rect(colour = "black"),
            axis.title=element_text(size=12), axis.text = element_text(size = 12),
            plot.title = element_text(hjust = 0.5), legend.position = "none")
    colors[model] <- c("grey")
    alphas[model] <- grey_alpha
  }
  
  if(catchment == "Inn"){
    ggpubr::ggarrange(plotlist = plots, ncol = 2, nrow = 4) +
      ggsave("../../../Manuscript/Paper 1 - ML focus/Figures/fig05_raw1.png",
             height = 20, width = 35, units = "cm")
  } else {
    count <- count + 1
    ggpubr::ggarrange(plotlist = plots, ncol = 2, nrow = 4) +
      ggsave(paste0("../../../Manuscript/Paper 1 - ML focus/Figures/S", count, "_", catchment, ".png"),
             height = 20, width = 35, units = "cm")
    
  }
  
  # Legend
  ts_plot_data$series <- as.character(ts_plot_data$series)
  legend_data <- ts_plot_data[ts_plot_data$series %in% c("Observation", "LM"), ]
  legend_data$series[legend_data$series == "LM"] <- "Prediction"
  legend_data$series <- factor(legend_data$series, labels = c("Observation", "Prediction"))
  colors <- c("Observation" = "deepskyblue3", "Prediction" = "tomato2")
  
  if(catchment == "Inn"){
    ggplot(legend_data, aes(x = index, y = value, color = series)) +
      geom_line(lwd = 1 ) +
      xlab("") + ylab("") +
      scale_color_manual("", values = colors) +
      #theme_classic() +
      theme(panel.background = element_rect(colour = "black"),
            axis.title=element_text(size=12), axis.text = element_text(size = 12),
            legend.text = element_text(size = 14),
            plot.title = element_text(hjust = 0.5)) +
      ggsave("../../../Manuscript/Paper 1 - ML focus/Figures/fig05_raw2.png",
             height = 20, width = 35, units = "cm")
  }
  
  
  
}








