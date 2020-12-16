# figure 3 code
# wateRtemp project
# Moritz Feigl, Nov 2020

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
# remove hyperpar results and noFmon results
scores <- scores[!is.na(scores$test_RMSE), ]
scores <- scores[scores$model_name %in% c(experiments, "LM", "air2stream"), ]

# get best model results for each basin -> Table 3
basin_best_model <- NULL
for(catchment in catchments){
  catch_data <- scores[scores$catchment == catchment, ]
  sub_data <- merge(aggregate(cv_or_validation_RMSE ~ model, catch_data, min), catch_data,
                    by = c("model", "cv_or_validation_RMSE"), all.x = TRUE, all.y = FALSE)
  num_cols <- which(sapply(sub_data, class)  == "numeric")
  sub_data[, num_cols] <- round(sub_data[, num_cols], 3)
  basin_best_model <- rbind(basin_best_model,
                            cbind("catchment" = catchment, sub_data[which.min(sub_data$test_RMSE),
                                                                    c("model", "model_name",
                                                                      "test_RMSE", "test_MAE")]))
}
basin_best_model
mean(basin_best_model$test_RMSE)
mean(basin_best_model$test_MAE)

#aggregate(test_RMSE ~ model_type, scores[scores$model_type %in% c("LSTM", "GRU"), ], summary)
# Best model for all ML model per catchment
diff <- numeric()
secbest_bestdiff <- numeric()
for(catchment in catchments){
  catch_data <- scores[scores$catchment == catchment, ]

  sub_data <- merge(aggregate(cv_or_validation_RMSE ~ model, catch_data, min), catch_data,
                    by = c("model", "cv_or_validation_RMSE"), all.x = TRUE, all.y = FALSE)
  sub_data$diff <- sub_data$test_RMSE - min(sub_data$test_RMSE)

  cat(catchment, ":", sub_data$model[which.min(sub_data$test_RMSE)], "\n")
  print(sub_data[, c("model", "model_name", "test_RMSE", "test_MAE", "diff")])
  cat("\n")
  # get the second best performing model diff
  secbest_bestdiff <- c(secbest_bestdiff, sort(sub_data$diff)[2])
  diff <- c(diff, sub_data$diff)
}
summary(diff)
summary(secbest_bestdiff)
# Get LM model results
lm_results <- NULL
for(catchment in catchments){
  catch_data <- scores[scores$catchment == catchment, ]
  num_cols <- which(sapply(catch_data, class)  == "numeric")
  catch_data[, num_cols] <- round(catch_data[, num_cols], 3)
  lm_results <- rbind(lm_results,
                      cbind("catchment" = catchment,
                            catch_data[catch_data$model == "LM", c("model", "test_RMSE", "test_MAE")]))
}
lm_results
mean(lm_results$test_RMSE)
mean(lm_results$test_MAE)
median(lm_results$test_RMSE)

# Get air2stream results
air_results <- NULL
for(catchment in catchments){
  catch_data <- scores[scores$catchment == catchment, ]
  num_cols <- which(sapply(catch_data, class)  == "numeric")
  catch_data[, num_cols] <- round(catch_data[, num_cols], 3)
  air_results <- rbind(air_results,
                      cbind("catchment" = catchment,
                            catch_data[catch_data$model == "air2stream", c("model", "test_RMSE", "test_MAE")]))

}
air_results
mean(air_results$test_RMSE)
mean(air_results$test_MAE)
median(air_results$test_RMSE)


# Statistical testing
kruskal.test(data.frame(a = lm_results$test_RMSE, b = air_results$test_RMSE))
kruskal.test(data.frame(a = basin_best_model$test_RMSE, b = air_results$test_RMSE))


