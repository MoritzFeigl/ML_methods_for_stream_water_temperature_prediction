# Run wateRtemp models on VSC
# Project: wateRtemp
# Moritz Feigl, Sep 2020

# settings
.libPaths("/home/lv71468/mfeigl/R/x86_64-pc-linux-gnu-library/4.0")
setwd("/home/lv71468/mfeigl/WT_Project/Modelling/final_models")
catchment <- "Aschach"
type <- "NULL"
cv_mode <- "NULL"
data_inputs <- "experiment2_TP"
model <- "FNN"

# source functions
source("wt_lm.R")
source("wt_randomforest.R")
source("utils.R")
source("wt_xgboost.R")
source("wt_fnn.R")
source("wt_rnn.R")
source("wt_nn.R")
library(ggplot2)
library(dplyr)
library(magrittr)
library(tensorflow)
library(keras)
# data prefix
data_prefix <- ifelse(data_inputs %in% c("experiment6_TQPGL", 
                                         "experiment5_TPGL",
                                         "experiment6_TQPGL_noFmon",
                                         "experiment5_TPGL_noFmon"), "radiation_", "")

# run
train_data <- feather::read_feather(paste0(catchment, "/train_", data_prefix, "data.feather"))
test_data <- feather::read_feather(paste0(catchment, "/test_", data_prefix, "data.feather"))

type_check <- ifelse(is.null(type), "nope", type)
if(type_check == "LM"){
  relevant_data <- c("date", "Q", "Ta", "wt")
} else {
  if(data_inputs == "experiment0_Tmean"){
    relevant_data <- c("date", "wt", paste0("Fmon.", 1:12),
                       "Ta", "Ta_lag1", "Ta_lag2", "Ta_lag3", "Ta_lag4")
  }
  if(data_inputs == "experiment1_T"){
    relevant_data <- c("date", "wt", paste0("Fmon.", 1:12),
                       "Ta", "Ta_lag1", "Ta_lag2", "Ta_lag3", "Ta_lag4",
                       "Ta_max", "Ta_max_lag1", "Ta_max_lag2", "Ta_max_lag3", "Ta_max_lag4",
                       "Ta_min", "Ta_min_lag1", "Ta_min_lag2", "Ta_min_lag3", "Ta_min_lag4")
  }
  if(data_inputs == "experiment2_TP"){
    relevant_data <- c("date", "wt", paste0("Fmon.", 1:12),
                       "Ta", "Ta_lag1", "Ta_lag2", "Ta_lag3", "Ta_lag4",
                       "Ta_max", "Ta_max_lag1", "Ta_max_lag2", "Ta_max_lag3", "Ta_max_lag4",
                       "Ta_min", "Ta_min_lag1", "Ta_min_lag2", "Ta_min_lag3", "Ta_min_lag4",
                       "P", "P_lag1", "P_lag2", "P_lag3", "P_lag4")
  }
  if(data_inputs == "experiment3_TQ"){
    relevant_data <- c("date", "wt", paste0("Fmon.", 1:12),
                       "Ta", "Ta_lag1", "Ta_lag2", "Ta_lag3", "Ta_lag4",
                       "Ta_max", "Ta_max_lag1", "Ta_max_lag2", "Ta_max_lag3", "Ta_max_lag4",
                       "Ta_min", "Ta_min_lag1", "Ta_min_lag2", "Ta_min_lag3", "Ta_min_lag4",
                       "Q", "Q_lag1", "Q_lag2", "Q_lag3", "Q_lag4")
  }
  if(data_inputs == "experiment4_TQP"){
    relevant_data <- c("date", "wt", paste0("Fmon.", 1:12),
                       "Ta", "Ta_lag1", "Ta_lag2", "Ta_lag3", "Ta_lag4",
                       "Ta_max", "Ta_max_lag1", "Ta_max_lag2", "Ta_max_lag3", "Ta_max_lag4",
                       "Ta_min", "Ta_min_lag1", "Ta_min_lag2", "Ta_min_lag3", "Ta_min_lag4",
                       "Q", "Q_lag1", "Q_lag2", "Q_lag3", "Q_lag4",
                       "P", "P_lag1", "P_lag2", "P_lag3", "P_lag4")
    
  }
  if(data_inputs == "experiment5_TPGL"){
    relevant_data <- c("date", "wt", paste0("Fmon.", 1:12),
                       "Ta", "Ta_lag1", "Ta_lag2", "Ta_lag3", "Ta_lag4",
                       "Ta_max", "Ta_max_lag1", "Ta_max_lag2", "Ta_max_lag3", "Ta_max_lag4",
                       "Ta_min", "Ta_min_lag1", "Ta_min_lag2", "Ta_min_lag3", "Ta_min_lag4",
                       "P", "P_lag1", "P_lag2", "P_lag3", "P_lag4",
                       "GL", "GL_lag1", "GL_lag2", "GL_lag3", "GL_lag4")
  }
  if(data_inputs == "experiment6_TQPGL"){
    relevant_data <- c("date", "wt", paste0("Fmon.", 1:12),
                       "Ta", "Ta_lag1", "Ta_lag2", "Ta_lag3", "Ta_lag4",
                       "Ta_max", "Ta_max_lag1", "Ta_max_lag2", "Ta_max_lag3", "Ta_max_lag4",
                       "Ta_min", "Ta_min_lag1", "Ta_min_lag2", "Ta_min_lag3", "Ta_min_lag4",
                       "Q", "Q_lag1", "Q_lag2", "Q_lag3", "Q_lag4",
                       "P", "P_lag1", "P_lag2", "P_lag3", "P_lag4",
                       "GL", "GL_lag1", "GL_lag2", "GL_lag3", "GL_lag4")
  }
  if(data_inputs == "experiment0_Tmean_noFmon"){
    relevant_data <- c("date", "wt", 
                       "Ta", "Ta_lag1", "Ta_lag2", "Ta_lag3", "Ta_lag4")
  }
  if(data_inputs == "experiment1_T_noFmon"){
    relevant_data <- c("date", "wt", 
                       "Ta", "Ta_lag1", "Ta_lag2", "Ta_lag3", "Ta_lag4",
                       "Ta_max", "Ta_max_lag1", "Ta_max_lag2", "Ta_max_lag3", "Ta_max_lag4",
                       "Ta_min", "Ta_min_lag1", "Ta_min_lag2", "Ta_min_lag3", "Ta_min_lag4")
  }
  if(data_inputs == "experiment2_TP_noFmon"){
    relevant_data <- c("date", "wt",
                       "Ta", "Ta_lag1", "Ta_lag2", "Ta_lag3", "Ta_lag4",
                       "Ta_max", "Ta_max_lag1", "Ta_max_lag2", "Ta_max_lag3", "Ta_max_lag4",
                       "Ta_min", "Ta_min_lag1", "Ta_min_lag2", "Ta_min_lag3", "Ta_min_lag4",
                       "P", "P_lag1", "P_lag2", "P_lag3", "P_lag4")
  }
  if(data_inputs == "experiment3_TQ_noFmon"){
    relevant_data <- c("date", "wt",
                       "Ta", "Ta_lag1", "Ta_lag2", "Ta_lag3", "Ta_lag4",
                       "Ta_max", "Ta_max_lag1", "Ta_max_lag2", "Ta_max_lag3", "Ta_max_lag4",
                       "Ta_min", "Ta_min_lag1", "Ta_min_lag2", "Ta_min_lag3", "Ta_min_lag4",
                       "Q", "Q_lag1", "Q_lag2", "Q_lag3", "Q_lag4")
  }
  if(data_inputs == "experiment4_TQP_noFmon"){
    relevant_data <- c("date", "wt",
                       "Ta", "Ta_lag1", "Ta_lag2", "Ta_lag3", "Ta_lag4",
                       "Ta_max", "Ta_max_lag1", "Ta_max_lag2", "Ta_max_lag3", "Ta_max_lag4",
                       "Ta_min", "Ta_min_lag1", "Ta_min_lag2", "Ta_min_lag3", "Ta_min_lag4",
                       "Q", "Q_lag1", "Q_lag2", "Q_lag3", "Q_lag4",
                       "P", "P_lag1", "P_lag2", "P_lag3", "P_lag4")
    
  }
  if(data_inputs == "experiment5_TPGL_noFmon"){
    relevant_data <- c("date", "wt",
                       "Ta", "Ta_lag1", "Ta_lag2", "Ta_lag3", "Ta_lag4",
                       "Ta_max", "Ta_max_lag1", "Ta_max_lag2", "Ta_max_lag3", "Ta_max_lag4",
                       "Ta_min", "Ta_min_lag1", "Ta_min_lag2", "Ta_min_lag3", "Ta_min_lag4",
                       "P", "P_lag1", "P_lag2", "P_lag3", "P_lag4",
                       "GL", "GL_lag1", "GL_lag2", "GL_lag3", "GL_lag4")
  }
  if(data_inputs == "experiment6_TQPGL_noFmon"){
    relevant_data <- c("date", "wt",
                       "Ta", "Ta_lag1", "Ta_lag2", "Ta_lag3", "Ta_lag4",
                       "Ta_max", "Ta_max_lag1", "Ta_max_lag2", "Ta_max_lag3", "Ta_max_lag4",
                       "Ta_min", "Ta_min_lag1", "Ta_min_lag2", "Ta_min_lag3", "Ta_min_lag4",
                       "Q", "Q_lag1", "Q_lag2", "Q_lag3", "Q_lag4",
                       "P", "P_lag1", "P_lag2", "P_lag3", "P_lag4",
                       "GL", "GL_lag1", "GL_lag2", "GL_lag3", "GL_lag4")
  }
}

train_data <- train_data[, relevant_data]
test_data <- test_data[, relevant_data]

if(model == "LM"){
  wt_lm(train_data = train_data,
        test_data = test_data,
        catchment = catchment,
        type = type,
        cv_mode = cv_mode,
        model_name = paste0(data_inputs, "_", cv_mode),
        seed = 42,
        no_cores = 32)
}
if(model == "RF"){
  wt_randomforest(train_data = train_data,
                  test_data = test_data,
                  catchment = catchment,
                  cv_mode = cv_mode,
                  model_name = paste0(data_inputs, "_", cv_mode),
                  seed = 42,
                  no_cores = 32)
}
if(model == "XGBoost"){
  wt_xgboost(train_data = train_data,
             test_data = test_data,
             catchment = catchment,
             cv_mode = cv_mode,
             model_name = paste0(data_inputs, "_", cv_mode),
             seed = 42,
             no_cores = 32,
             n_iter = 40,
             n_random_initial_points = 20)
}
if(model == "FNN"){
  wt_fnn(train_data = train_data,
         test_data = test_data,
         catchment = catchment,
         model_name = data_inputs,
         seed = 42,
         n_iter = 40,
         n_random_initial_points = 20)
}
if(model == "RNN"){
  wt_rnn(train_data = train_data,
         test_data = test_data,
         catchment = catchment,
         model_name = data_inputs,
         seed = 42,
         n_iter = 40,
         ensemble_runs = 5,
         n_random_initial_points = 20,
         type = type)
}
