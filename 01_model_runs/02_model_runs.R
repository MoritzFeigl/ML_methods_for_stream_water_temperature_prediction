# Run wateRtemp models on VSC
# Project: wateRtemp
# Moritz Feigl, Sep 2020

# settings
setwd("/home/lv71468/mfeigl/WT_Project/Modelling/final_models")

# Template, folder, loop variables
lines <- readLines("02_model_runs_template.R")
if(!dir.exists("02_scripts")) dir.create("02_scripts")
all_catchments <- c("Donau", "Ybbs", "Inn", "Aschach", "Enns",
                    "Erlauf", "Kleine_Muehl", "Saalach", "Salzach", "Traisen")
all_data_inputs <- c("experiment1_T",
                     "experiment2_TP",
                     "experiment3_TQ",
                     "experiment4_TQP",
                     "experiment5_TPGL",
                     "experiment6_TQPGL")

# create scripts in a loop
for(model in c("RF", "XGBoost")){ # "LM", "RF", "XGBoost", "FNN", "RNN")){
  # model script folder
  if(!dir.exists(paste0("02_scripts/", model, "_scripts"))){
    dir.create(paste0("02_scripts/", model, "_scripts"))
  }
  # type switch
  types <- switch(model,
                  "LM" = c("LM", "stepLM"),
                  "RF" = "NULL",
                  "XGBoost" = "NULL",
                  "FNN" = "NULL",
                  "RNN" = c("LSTM", "GRU"))
  cv_modes <- switch(model,
                     "LM" = c("repCV", "timeseriesCV"),
                     "RF" = c("repCV", "timeseriesCV"),
                     "XGBoost" = c("repCV", "timeseriesCV"),
                     "FNN" = "NULL",
                     "RNN" = "NULL")
  # loop over all properties
  batch <- 0
  for(catchment in all_catchments){
    for(cv_mode in cv_modes){
      for(type in types){
        if(type == "LM" & cv_mode == "timeseriesCV") next
        for(data_inputs in all_data_inputs){
          if(type == "LM" & data_inputs != "ungauged") next
          batch <- batch + 1
          lines[8] <- paste0('catchment <- "', catchment, '"')
          lines[9] <- paste0('type <- "', type, '"')
          lines[10] <- paste0('cv_mode <- "', cv_mode, '"')
          lines[11] <- paste0('data_inputs <- "', data_inputs, '"')
          lines[12] <- paste0('model <- "', model, '"')
          writeLines(lines, paste0("02_scripts/", model, "_scripts/model_run", batch, ".R"))
        }
      }
    }
  }
  # write bash script to start slurm jobs
  setwd(paste0("02_scripts/", model, "_scripts"))
  fileConn <- file(paste0(model, "_runs.sh"))
  writeLines(
    c("#!/bin/sh",
      paste0("#SBATCH -J ", model, "_runs"),
      "#SBATCH -N 1",
      "#SBATCH --qos=normal_0064",
      "#SBATCH --partition=mem_0064",
      "#SBATCH --mem=64000M",
      paste0("#SBATCH --array=1-", batch, "%", min(c(40, batch))),
      "#SBATCH --mail-user=moritz.feigl@boku.ac.at",
      "#SBATCH --mail-type=BEGIN,END,FAIL",
      "#SBATCH --output=model_run%a.out",
      "module purge",
      "module load intel/18 intel-mkl/2018 pcre2/10.35 R/4.0.2 gcc",
      "Rscript model_run${SLURM_ARRAY_TASK_ID}.R"),
    fileConn)
  close(fileConn)
  # start slurm job
  system(paste0("sbatch ", model, "_runs.sh"))
  setwd("../..")
}
