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

cv_data <- score[score$model %in% c("stepLM", "RF", "XGBoost"), ]
cv_data <- cv_data[!is.na(cv_data$test_RMSE),]

ggplot(cv_data, aes(cv_mode, test_RMSE)) + geom_boxplot()

# only pairs of repCV and timesliceCV
counts <- aggregate(cv_mode ~ catchment + model_name + model, cv_data, length)
full_counts <- counts[counts$cv_mode == 2, ]
pair_cv_data <- merge(full_counts, cv_data, 
                      by = c("catchment", "model_name", "model"),
                      all.x = TRUE, all.y = FALSE, suffixes = c(".x", ""))
# test for differences in RMSE of CV types
kruskal.test(data.frame(pair_cv_data$test_RMSE[pair_cv_data$cv_mode == "repCV"],
             pair_cv_data$test_RMSE[pair_cv_data$cv_mode == "timeseriesCV"]))


compare_df <- merge(pair_cv_data[pair_cv_data$cv_mode == "repCV", ],
                    pair_cv_data[pair_cv_data$cv_mode == "timeseriesCV", ],
      by = c("catchment", "model", "model_name"), suffixes = c(".repCV", ".timeslice"),
      all.x=TRUE)
# asb diff
compare_df$diff <- abs(compare_df$test_RMSE.repCV - compare_df$test_RMSE.timeslice)
compare_df$mae_diff <- abs(compare_df$test_MAE.repCV - compare_df$test_MAE.timeslice)
summary(compare_df$diff)
summary(compare_df$mae_diff)

# diff
compare_df$diff <- compare_df$test_RMSE.repCV - compare_df$test_RMSE.timeslice
compare_df$mae_diff <- compare_df$test_MAE.repCV - compare_df$test_MAE.timeslice
summary(compare_df$diff)
summary(compare_df$mae_diff)

# statistical influence of cv_mode on results?
summary(lm(test_RMSE ~ catchment + model + cv_mode, cv_data))
plot(lm(test_RMSE ~ catchment + model + cv_mode, cv_data))

# run time difference
pair_cv_data$run_time <- as.numeric(gsub(" minutes", "", pair_cv_data$run_time))
aggregate(run_time ~ cv_mode, pair_cv_data, summary)
kruskal.test(data.frame(pair_cv_data$run_time[pair_cv_data$cv_mode == "repCV"],
                        pair_cv_data$run_time[pair_cv_data$cv_mode == "timeseriesCV"]))


