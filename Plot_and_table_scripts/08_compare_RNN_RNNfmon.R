#
# RNNs vs. fmon_RNNs
#

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

# scores
fmon_data <- score[grep("RNN", score$model), ]
fmon_data$noFmon <- grepl("Fmon", fmon_data$model_name)
fmon_data <- fmon_data[!is.na(fmon_data$test_RMSE),]
fmon_best <- aggregate(test_RMSE ~ catchment + model_type + model_name, fmon_data, min)

# Hyperparameters
path <- "C:/Users/morit/Dropbox/WT_Project/Modelling/VSC_results/final_models/"
fmon_experiments <- c(experiments, paste0( c("experiment0_Tmean", 
                                        "experiment1_T",
                                        "experiment2_TP",
                                        "experiment3_TQ",
                                        "experiment4_TQP",
                                        "experiment5_TPGL",
                                        "experiment6_TQPGL"), "_noFmon"))
rnn_paras <- NULL
for(catchment in catchments){
  for(experiment in fmon_experiments){
    # get types and cv_modes to loop over
    types <- c("LSTM", "GRU")
    for(type in types){
      noFmon <- ifelse(grepl("noFmon", experiment), TRUE, FALSE)
      type_path <- ifelse(type != "", paste0(type, "/"), "")
      test_read <- try({
        opt_score <- cbind("noFmon" = noFmon, "catchment" = catchment, "model_type" = type, 
                           "model_name" = experiment,
                           experiment = gsub("_noFmon", "", experiment),
                           read.csv(paste0(path, catchment, "/", "RNN", "/", type_path, 
                                     experiment, 
                                     "/hyperpar_opt_scores.csv")))
        opt_score <- opt_score[which.min(opt_score$cv_or_validation_RMSE), ]
        rnn_paras <- rbind(rnn_paras,
                           opt_score)
      })
    }
  }
}

small_rnn_paras <- rnn_paras[, c("noFmon", "catchment", "model_type", "model_name", 
                                 "experiment", "timesteps", "cv_or_validation_RMSE")]
rnn_df <- merge(small_rnn_paras, fmon_best, by = c("catchment", "model_type", "model_name"), 
                all.x = TRUE, all.y = TRUE)

all <- merge(rnn_df[!rnn_df$noFmon, c("catchment", "model_type", "experiment", 
                                      "test_RMSE", "cv_or_validation_RMSE", "timesteps")],
             rnn_df[rnn_df$noFmon, c("catchment", "model_type","experiment", 
                                     "test_RMSE", "cv_or_validation_RMSE", "timesteps")],
             by = c("catchment", "model_type","experiment"), suffixes = c(".fmon", ".nofmon"), all = TRUE)

table(all$catchment)

all$diff <- all$test_RMSE.nofmon - all$test_RMSE.fmon
all$diff_ts <- all$timesteps.nofmon - all$timesteps.fmon
summary(all$diff)
summary(all$diff_ts)

kruskal.test(data.frame(all$test_RMSE.nofmon, all$test_RMSE.fmon))
kruskal.test(data.frame(all$timesteps.fmon, all$timesteps.nofmon))


# Computation time difference
fmon_data$run_time <- as.numeric(gsub(" minutes", "", fmon_data$run_time))
aggregate(run_time ~ noFmon, fmon_data, summary)
Fmon_times <- fmon_data[!fmon_data$noFmon, ]
noFmon_times <- fmon_data[fmon_data$noFmon, ]
noFmon_times$model_name <- gsub("_noFmon", "", noFmon_times$model_name)
rm_rows <- NULL
for(row in 1:nrow(Fmon_times)){
  if(!(
    paste0(Fmon_times[row, 3:6], collapse = "") %in% 
    apply(noFmon_times[, 3:6], 1, paste0, collapse = ""))){
    rm_rows <- c(rm_rows, row)
  }
}
Fmon_times <- Fmon_times[- rm_rows, ]
noFmon_times <- noFmon_times[!duplicated(noFmon_times[, 3:6]), ]
Fmon_times <- Fmon_times[!duplicated(Fmon_times[, 3:6])]

kruskal.test(data.frame(Fmon_times$run_time, noFmon_times$run_time))
mean(Fmon_times$run_time) - mean(noFmon_times$run_time)
ks.test(noFmon_times$run_time, "pnorm")









