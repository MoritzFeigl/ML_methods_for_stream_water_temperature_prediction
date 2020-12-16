# Aggregate wateRtemp results
# wateRtemp project
# Moritz Feigl, Nov 2020


# script for loading all wateRtemp results into a single data frame for all given catchments
setwd("C:/Users/morit/Dropbox/WT_Project/Modelling/VSC_results/final_models")
catchments <- c("Kleine_Muehl", "Aschach", "Erlauf", "Traisen", "Ybbs", "Saalach",
                "Enns", "Inn", "Salzach", "Donau")
experiments <- c("experiment0_Tmean", 
                 "experiment1_T",
                 "experiment2_TP",
                 "experiment3_TQ",
                 "experiment4_TQP",
                 "experiment5_TPGL",
                 "experiment6_TQPGL")
models <- c("LM", "RF", "XGBoost", "FNN", "RNN")
# 1. Model scores
file_nr <- 0
for(catchment in catchments){
  for(model in models){
    file_nr <- file_nr +1
    if(file_nr == 1){
      model_scores <- read.csv(paste0(catchment, "/", model, "/model_scores.csv"))    
    } else {
      try(model_scores <- rbind(model_scores,
                                read.csv(paste0(catchment, "/", model, "/model_scores.csv")))
      )
    }
  }
}
# remove old version
rm_lm_results <- which(model_scores$model == "LM" & is.na(model_scores$model_type))
if(length(rm_lm_results) > 0) model_scores <- model_scores[-rm_lm_results, ]
rm_cvless_results <- which(model_scores$model %in% c("RF", "XGBoost") & model_scores$model_name %in% experiments)
if(length(rm_cvless_results) > 0) model_scores <- model_scores[-rm_cvless_results, ]

no_time_data <- model_scores[, 3:13]
which_duplicated <- which(duplicated(no_time_data))
if(length(which_duplicated) > 0) model_scores <- model_scores[-which_duplicated, ]
# remove cv_mode from model name
model_scores$model_name <- gsub("_repCV", "", model_scores$model_name, fixed = TRUE)
model_scores$model_name <- gsub("_timeseriesCV", "", model_scores$model_name, fixed = TRUE)



air2stream_scores <- data.frame("start_time" = NA,
                                "run_time" = NA,
                                "catchment" = catchments,
                                "model" = "air2stream",
                                "model_type" = NA,
                                "model_name" = "air2stream",
                                "cv_mode" = "train/val split",
                                "train_RMSE" = NA,
                                "train_MAE" = NA,
                                "cv_or_validation_RMSE" = NA,
                                "cv_or_validation_MAE" = NA,
                                "test_RMSE" = c(0.908, 1.147, 0.911, 0.948, 0.948, 0.802, 1.168, 1.097, 0.743, 1.099),
                                "test_MAE"  = c(0.714, 0.882, 0.726, 0.747, 0.756, 0.646, 0.671, 0.949, 0.595, 0.910)
)


# get hyperparopt scores
opt_models <- c("RF", "XGBoost", "FNN", "RNN")
missing_exp <- NULL
hyper_par_scores <- NULL
for(catchment in catchments){
  for(model in opt_models){
    for(experiment in experiments){
      # get types and cv_modes to loop over
      types <- switch(model,
                      "RF" = "",
                      "XGBoost" = "",
                      "FNN" = "",
                      "RNN" = c("LSTM", "GRU"))
      cv_modes <- switch(model,
                         "LM" = c("repCV", "timeseriesCV"),
                         "RF" = c("repCV", "timeseriesCV"),
                         "XGBoost" = c("repCV", "timeseriesCV"),
                         "FNN" = "train/val split",
                         "RNN" = "train/val split")
      for(type in types){
        for(cv_mode in cv_modes){
          type_path <- ifelse(type != "", paste0(type, "/"), "")
          cv_mode_path <- ifelse(model %in% c("LM", "RF", "XGBoost"), paste0("_", cv_mode), "")
          
          test_read <- try({
            opt_score <- read.csv(paste0(catchment, "/", model, "/", type_path, 
                                         experiment, cv_mode_path, 
                                         "/hyperpar_opt_scores.csv"))
            if(sum(names(opt_score) %in% c("RMSE", "MAE")) != 0){
             score_cols <- which(names(opt_score) %in% c("RMSE", "MAE"))
             names(opt_score)[score_cols] <- c("cv_or_validation_RMSE", "cv_or_validation_MAE")
            }
            hyper_par_scores <- rbind(hyper_par_scores,
                                      data.frame(
                                        "start_time" = NA,
                                        "run_time" = NA,
                                        "catchment" = catchment,
                                        "model" = model,
                                        "model_type" = type,
                                        "model_name" = experiment,
                                        "cv_mode" = cv_mode,
                                        "train_RMSE" = NA,
                                        "train_MAE" = NA,
                                        "cv_or_validation_RMSE" = opt_score$cv_or_validation_RMSE,
                                        "cv_or_validation_MAE" = opt_score$cv_or_validation_MAE,
                                        "test_RMSE" = NA,
                                        "test_MAE"  = NA)
            )
          })
          if(class(test_read) == "try-error"){
            missing_exp <- rbind(missing_exp, 
                                 data.frame("catchment" = catchment,
                                            "model" = model,
                                            "model_name" = experiment,
                                            "cv_mode" = cv_mode))
            
            
          }
        }
      }
    }
  }
}

#write.csv(missing_exp, "missing_runs.csv", row.names = FALSE)

model_scores <- rbind(model_scores, air2stream_scores, hyper_par_scores)

model_scores$model[model_scores$model_type == "LSTM"] <- "RNN-LSTM"
model_scores$model[model_scores$model_type == "GRU"] <- "RNN-GRU"
model_scores$model[model_scores$model_type == "stepLM"] <- "stepLM"


# save results
write.csv(model_scores, "../../../Manuscript/Paper 1 - ML focus/results_data/all_results_FINAL.csv",
          row.names = FALSE)

