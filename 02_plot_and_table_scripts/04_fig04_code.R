# figure 3 code
# wateRtemp project
# Moritz Feigl, Nov 2020

library(ggplot2)
setwd("C:/Users/morit/Dropbox/WT_Project/Manuscript/Paper 1 - ML focus") # Mo laptop
score <- read.csv("results_data/all_results_FINAL.csv")
catchments <- c("Kleine_Muehl", "Aschach", "Erlauf", "Traisen", "Ybbs", "Saalach",
                "Enns", "Inn", "Salzach", "Donau")
experiments <- c("experiment0_Tmean",
                 "experiment1_T",
                 "experiment2_TP",
                 "experiment3_TQ",
                 "experiment4_TQP",
                 "experiment5_TPGL",
                 "experiment6_TQPGL")

# ml performance -------------------------------------------------------------------------
best_models_data <- score[!(score$model %in% "LM"),]
best_models_data <- best_models_data[!is.na(best_models_data$test_MAE),]
best_models_data <- best_models_data[best_models_data$model_name %in% c(experiments, "LM", "air2stream"), ]
# remove timeseriesCV results for plotting (except if no repcv exists)
rm_cv_mode <- which(best_models_data$model %in% c("RF", "XGBoost") & best_models_data$cv_mode == "timeseriesCV")
best_models_data <- best_models_data[-rm_cv_mode, ]
rm_cv_mode2 <- which(best_models_data$model %in% "stepLM" & best_models_data$cv_mode == "timeseriesCV")
for(catchment in catchments){
  for(experiment in experiments){
    criteria <- best_models_data$catchment == catchment &
                best_models_data$model %in% "stepLM" &
                best_models_data$model_name == experiment
    #print(best_models_data[criteria, 3:8])
    if(nrow(best_models_data[criteria, ]) > 1){
      remove_row <- which(criteria & best_models_data$cv_mode == "timeseriesCV")
      print(best_models_data[remove_row, ])
      best_models_data <- best_models_data[-remove_row, ]
    }
  }
}

best_models_with_air2stream <- best_models_data
best_models_data <- best_models_data[best_models_data$model != "air2stream", ]

best_models_data$Model <- factor(best_models_data$model,
                                 levels = c("stepLM", "RF", "XGBoost", "FNN", "RNN-LSTM", "RNN-GRU"),
                                 labels = c("step LM", "RF", "XGBoost", "FNN", "RNN-LSTM", "RNN-GRU"))
best_models_data$experiments <- factor(best_models_data$model_name, levels = experiments,
                                       labels = c("Experiment 0\n(Tmean)",
                                                  "Experiment 1\n(T)",
                                                  "Experiment 2\n(TP)",
                                                  "Experiment 3\n(TQ)",
                                                  "Experiment 4\n(TQP)",
                                                  "Experiment 5\n(TPGL)",
                                                  "Experiment 6\n(TQPGL)"))

# ML models vs performance
best_models_with_air2stream$Model <- factor(best_models_with_air2stream$model,
                                 levels = c("air2stream", "stepLM", "RF", "XGBoost", "FNN", "RNN-LSTM", "RNN-GRU"),
                                 labels = c("air2stream", "step LM", "RF", "XGBoost", "FNN", "RNN-LSTM", "RNN-GRU"))
ggplot(best_models_with_air2stream, aes(x = Model, y = test_RMSE)) + geom_boxplot() +
  xlab("") + ylab("Test RMSE") +
  geom_jitter(width = 0.1, col = "grey41") +
  ggsave("Figures/fig04_raw1.png",
         height = 6, width = 10, units = "in")

# data inputs vs performance
ggplot(best_models_data, aes(x = experiments, y = test_RMSE)) + geom_boxplot() +
  xlab("") + ylab("Test RMSE")  +
  geom_jitter(width = 0.1, col = "grey41") +
  ggsave("Figures/fig04_raw2.png",
         height = 6, width = 10, units = "in")

aggregate(test_RMSE~experiments, best_models_data, min)
aggregate(test_RMSE~experiments, best_models_data, median)
aggregate(test_RMSE~Model, best_models_with_air2stream, median)

# Performance vs. catchments -------------------------------------------------------------
best_models_data$catchment[best_models_data$catchment == "Kleine_Muehl"] <- "Kleine M?hl"
best_models_data$catchment <- factor(best_models_data$catchment,
                                     levels = c("Kleine M?hl", "Aschach", "Erlauf", "Traisen",
                                                "Ybbs", "Saalach", "Enns", "Inn", "Salzach", "Donau")
)
best_models_data$experiments_nice <- factor(best_models_data$model_name, levels = experiments,
                                            labels = c("0 (Tmean)",
                                                       "1 (T)",
                                                       "2 (TP)",
                                                       "3 (TQ)",
                                                       "4 (TQP)",
                                                       "5 (TPGL)",
                                                       "6 (TQPGL)"))

# air2stream horizontal lines
a2s <- score[score$model == "air2stream", c("catchment", "test_RMSE")]
a2s$catchment[a2s$catchment == "Kleine_Muehl"] <- "Kleine M?hl"

a2s$catchment <- factor(a2s$catchment,
                        levels = c("Kleine M?hl", "Aschach", "Erlauf", "Traisen",
                                   "Ybbs", "Saalach", "Enns", "Inn", "Salzach", "Donau"))
a2s$boxplot.nr <- c(1:10)


ggplot(best_models_data,
       aes(x = catchment, y = test_RMSE)) + geom_boxplot() +
  geom_point(aes(col = Model, shape = experiments_nice), size = 3,
             alpha = 0.7, position = position_dodge(width=0.5)) +#position_jitterdodge()) +
  #geom_beeswarm(aes(col = Model, shape = experiments_nice), size = 3,
  #          alpha = 0.7, dodge.width = 0.4) +
  scale_shape_manual(values = c(4, 3, 8, 17, 18, 19, 15), name = "Experiment") +
  theme(legend.text=element_text(size=12)) +
  xlab("") + ylab("Test RMSE") +
  geom_segment(data=a2s,aes(x=boxplot.nr-0.4,xend=boxplot.nr+0.4,
                                  y=test_RMSE,yend=test_RMSE, linetype="air2stream"),
               inherit.aes=FALSE, color="grey69", size=1) +
  scale_linetype_manual("Benchmark model",values=c("air2stream"=1)) +
  ggsave("Figures/fig04_raw3.png", height = 6, width = 15, units = "in")


ggplot(best_models_data,
       aes(x = catchment, y = test_MAE)) + geom_boxplot() +
  geom_point(aes(col = Model, shape = experiments_nice), size = 3,
             alpha = 0.7, position = position_dodge(width=0.5)) +
  scale_shape_manual(values = c(4, 3, 8, 17, 18, 19, 15), name = "Experiment") +
  theme(legend.text=element_text(size=12)) +
  xlab("") + ylab("Test MAE") +
  geom_segment(data=a2s,aes(x=boxplot.nr-0.4,xend=boxplot.nr+0.4,
                            y=test_RMSE,yend=test_RMSE, linetype="air2stream"),
               inherit.aes=FALSE, color="grey69", size=1) +
  scale_linetype_manual("Benchmark model",values=c("air2stream"=1)) +
  ggsave("Figures/S1_raw.png", height = 6, width = 15, units = "in")

aggregate(test_RMSE ~ catchment, best_models_data, median)


# difference ML models vs. air2stream
dif_dat <- merge(best_models_data, a2s[, 1:2], by = "catchment", suffixes = c("", ".air2s"))
dif_dat$diff_to_air2s  <- dif_dat$train_RMSE - dif_dat$test_RMSE.air2s
summary(dif_dat$diff_to_air2s)
unique(dif_dat$model)


