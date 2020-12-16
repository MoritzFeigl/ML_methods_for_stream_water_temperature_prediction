# statistical tests for results
# wateRtemp project
# Moritz Feigl, Nov 2020



library(ggplot2)
setwd("C:/Users/morit/Dropbox/WT_Project/Manuscript/Paper 1 - ML focus") # Mo laptop
scores <- read.csv("results_data/all_results_FINAL.csv")
catchments <- c("Kleine_Muehl", "Aschach", "Erlauf", "Traisen", "Ybbs", "Saalach",
                "Enns", "Inn", "Salzach", "Donau")
experiments <- c("experiment0_Tmean", 
                 "experiment1_T",
                 "experiment2_TP",
                 "experiment3_TQ",
                 "experiment4_TQP",
                 "experiment5_TPGL",
                 "experiment6_TQPGL")
# remove hyperpar results
scores <- scores[!is.na(scores$test_RMSE), ]
# sub data for testing model results per: basin, data inputs, model
data <- scores[scores$model_name %in% experiments,]
data <- data[data$cv_mode != "timeseriesCV", ]
data$model <- factor(data$model, 
                     levels = c("stepLM", "RF", "XGBoost", "FNN", "RNN-LSTM", "RNN-GRU"),
                     labels = c("step LM", "RF", "XGBoost", "FNN", "RNN-LSTM", "RNN-GRU"))
data$model_name <- factor(data$model_name, levels = experiments)

# 1. test overall differences -> not significant
kruskal.test(test_RMSE ~ model, data) # kruskal test -> significant -> test
dunn_all_models <- FSA::dunnTest(test_RMSE ~ model,
                                 data=data,
                                 method="bh")
p_vals_dunn <- data.frame("comparison" = dunn_all_models$res$Comparison,
                          "p_val" = format(dunn_all_models$res$P.unadj, scientific = FALSE))
p_vals_dunn
p_vals_dunn[p_vals_dunn$p_val < 0.005, "comparison"]

# 3. test overall differences with MAE-> not significant
kruskal.test(test_MAE ~ model, data) # kruskal test -> significant -> test
dunn_all_models_MAE <- FSA::dunnTest(test_MAE ~ model,
                                 data=data,
                                 method="bh")
p_vals_dunn_MAE <- data.frame("comparison" = dunn_all_models_MAE$res$Comparison,
                          "p_val" = format(dunn_all_models_MAE$res$P.unadj, scientific = FALSE))
p_vals_dunn_MAE
p_vals_dunn_MAE[p_vals_dunn_MAE$p_val < 0.005, "comparison"] # only FNNvsstepLM are now not significant anymore


# 4. data inputs influence on RMSE result -> significant
kruskal.test(test_RMSE ~ model_name, data)
dunn_all_models <- FSA::dunnTest(test_RMSE ~ model_name,
                                 data=data,
                                 method="bh")
p_vals_dunn <- data.frame("comparison" = dunn_all_models$res$Comparison,
                          "p_val" = format(dunn_all_models$res$P.unadj, scientific = FALSE))
p_vals_dunn
p_vals_dunn[p_vals_dunn$p_val < 0.005, "comparison"]


# 5. Median performance per cacthment
aggregate(test_RMSE ~ catchment, data, median)
aggregate(test_RMSE ~ catchment, data, mean)


# 6. median model performance difference to best model -----------------------------------

best_models <- aggregate(test_RMSE ~ catchment, data, min)
names(best_models)[2] <- "best_rmse"
data_best <- merge(data, best_models, by = "catchment", all.x=TRUE)
head(data_best)
data_best$best_diff <- abs(data_best$test_RMSE - data_best$best_rmse)
aggregate(best_diff ~ model, data_best, median)


# 7. LM of model -------------------------------------------------------------------------
mod <- lm(test_RMSE ~ -1 + catchment + model + model_name, data)
summary(mod)
# plot pred vs obs
test_pred <- lm(test_RMSE ~ catchment + model + model_name - 1, data)
tes_pred2 <- predict(test_pred, data)
plot(tes_pred2, data$test_RMSE, xlim=c(0.4, 1.2), ylim=c(0.4, 1.2))

# effect size
effectsize::standardize_parameters(mod)

# catchments
summary(mod$coefficients[1:10])
# models
summary(mod$coefficients[11:15])
# data inputs
summary(mod$coefficients[16:21])

# mod
# 
# # time and variables test
# catch_time <- data.frame(catchment = c("Kleine_Muehl", "Aschach", "Erlauf", "Traisen", "Ybbs", "Saalach",
#                                        "Enns", "Inn", "Salzach", "Donau"),
#                          time = 0.8*c(14, 11.9, 35.3, 17.7, 34.7, 16, 10, 18.8, 39, 11))
# data <- merge(data, catch_time, by = "catchment", suffixes = c("", ""))
# data$time[data$model_name %in% c("experiment5_TPGL", "experiment6_TQPGL")] <- 0.8*9
# data$Q <- FALSE
# data$P <- FALSE
# data$T <- 
# data$
# mod <- lm(test_RMSE ~ -1 + catchment + model + model_name + time, data)
# summary(mod)
# 










