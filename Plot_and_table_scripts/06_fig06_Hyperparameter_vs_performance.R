# figure 5
# wateRtemp project
# Moritz Feigl, Nov 2020


library(ggplot2)
setwd("C:/Users/morit/Dropbox/WT_Project/Manuscript/Paper 1 - ML focus") # Mo laptop
catchments <- c("Kleine_Muehl", "Aschach", "Erlauf", "Traisen", "Ybbs", "Saalach", "Enns", "Inn", "Salzach", "Donau")

# Hyperparameter vs. RMSE 
scores <- read.csv("results_data/all_results_FINAL.csv")
sub_scores <- scores[!(scores$model %in% c("LM", "step LM")) & 
                       is.na(scores$test_RMSE) & scores$cv_mode != "timeseriesCV", ]
sub_scores$model <- factor(sub_scores$model,
                           levels = c("RF", "XGBoost", "FNN", "RNN-LSTM", "RNN-GRU"))
sub_scores$catchment[sub_scores$catchment == "Kleine_Muehl"] <- "Kleine Mühl"
sub_scores$catchment <-  factor(sub_scores$catchment,
                                levels = c("Kleine Mühl", "Aschach", "Erlauf", "Traisen",
                                           "Ybbs", "Saalach", "Enns", "Inn", "Salzach", "Donau"))

ggplot(sub_scores, aes(catchment, cv_or_validation_RMSE, fill = model)) + geom_boxplot() +
  xlab("") + ylab("Validation RMSE (°C)") + coord_cartesian(ylim = c(5, NA)) +
  scale_y_continuous(breaks = c(5, 10, 15)) +
  ggsave("Figures/fig06_raw1.png", width = 12, height = 2, units = "in")
ggplot(sub_scores, aes(catchment, cv_or_validation_RMSE, fill = model)) + geom_boxplot() +
  xlab("") + ylab("Validation RMSE (°C)") + 
  scale_y_continuous(breaks = c(0.5, 1:5), limits = c(0.4, 5)) + 
  ggsave("Figures/fig06_raw2.png", width = 12, height = 7, units = "in")


summary(lm(cv_or_validation_RMSE ~ - 1 + catchment + model + model_name, sub_scores))
plot(lm(cv_or_validation_RMSE ~ - 1 + catchment + model + model_name, sub_scores))

aggregate(cv_or_validation_RMSE ~ model, sub_scores, sd)
aggregate(cv_or_validation_RMSE ~ model, sub_scores, mad)
aggregate(cv_or_validation_RMSE ~ model, sub_scores, mean)
aggregate(cv_or_validation_RMSE ~ model, sub_scores, min)
aggregate(cv_or_validation_RMSE ~ model, sub_scores, max)
aggregate(cv_or_validation_RMSE ~ model, sub_scores, summary)

# median RMSE
aggregate(cv_or_validation_RMSE ~ model,
          aggregate(cv_or_validation_RMSE ~ model + catchment, sub_scores, median),
          mean)
# RMSE variance
aggregate(cv_or_validation_RMSE ~ model,
          aggregate(cv_or_validation_RMSE ~ model + catchment, sub_scores, var),
          mean)
# mean RMSE
aggregate(cv_or_validation_RMSE ~ model,
          aggregate(cv_or_validation_RMSE ~ model + catchment, sub_scores, max), mean)


min(sub_scores$cv_or_validation_RMSE, na.rm = TRUE)
    