# Preprocessing catchment data for wateRtemp models
# wateRtemp project
# Moritz Feigl, Sep 2020

setwd("C:/Users/morit/Dropbox/WT_Project/Modelling/final_models")
library(wateRtemp)
# Get year ranges
year_ranges <- readRDS("year_ranges_list_for_all_catchments.rds")
all_catchments <- c("Donau", "Ybbs", "Inn", "Aschach", "Enns",
                    "Erlauf", "Kleine_Muehl", "Saalach", "Salzach", "Traisen")

# Data loader function
data.loader <- function(catchment){
  old_wd <- getwd()
  wrong_folder_catcher <- tryCatch({
    setwd(catchment)},
    error = function(e) {
      message(paste0("ERROR: There is no folder named ",
                     catchment, " in your current working directory."))
      return(NA)
    })
  if(is.na(wrong_folder_catcher)) return(NA)
  # Load data
  file <- list.files(pattern = "\\.dat")
  lines <- readLines(file, n = 100)
  skip_ind <- which(lines == "Werte:")
  wt_raw <- read.table(file, skip = skip_ind, stringsAsFactors = FALSE)
  names(wt_raw) <- c("date", "time", "wt")
  wt_raw[wt_raw$wt == "L\xfccke", "wt"] <- NA
  wt_raw$wt <- as.numeric(wt_raw$wt)
  wt_raw_d <- aggregate(wt ~ date, wt_raw, FUN = mean)
  wt_raw_d$year <- as.integer(substr(wt_raw_d$date, 7, 10))
  wt_raw_d$mon <- as.integer(substr(wt_raw_d$date, 4, 5))
  wt_raw_d$day <- as.integer(substr(wt_raw_d$date, 1, 2))
  wt_raw_d <- wt_raw_d[order(wt_raw_d$day), ]
  wt_raw_d <- wt_raw_d[order(wt_raw_d$mon), ]
  wt_raw_d <- wt_raw_d[order(wt_raw_d$year), ]

  # SPARTACUS DATA
  sparta_files <- list.files("../../../Daten/Spartacus_NB", pattern = "csv")
  catchment_number <- as.numeric(
    gsub("SB", "",
         strsplit(sparta_files[grep(catchment, sparta_files)], "_")[[1]][1]
    ))
  spartacus <- read.csv(paste0("../../../Daten/Spartacus_NB/SB",
                               catchment_number, "_", catchment, ".csv"), sep = ";",
                        dec = ",", stringsAsFactors = FALSE)
  spartacus <- spartacus[, -c(4, 5)]
  # Gesamter Datensatz
  data <- merge(spartacus, wt_raw_d, by = c("year", "mon", "day"))
  #data <- data[data$year > year_range[1] & data$year < year_range[2], ]
  data <- data[order(data$day), ]
  data <- data[order(data$mon), ]
  data <- data[order(data$year), ]
  data <- data[, -9]
  names(data) <- c("year", "mon", "day", "Q", "RR", "Tmin", "Tmax", "Tmean", "wt")

  # Strahlung
  gl <- read.csv2("../../../Daten/INCA_Catchment_Averages/GL_C2P_NB_24h_ASCII.csv")
  gl <- gl[, c(1:3, catchment_number + 5)]
  names(gl) <- c("year", "mon", "day", "GL")
  data <- merge(data, gl, all.x = TRUE)
  names(data)[names(data) == "RR"] <- "P"
  names(data)[names(data) == "Tmean"] <- "Ta"
  names(data)[names(data) == "Tmax"] <- "Ta_max"
  names(data)[names(data) == "Tmin"] <- "Ta_min"
  names(data)[names(data) == "mon"] <- "month"
  setwd(old_wd)
  return(data)
}

for(catchment in all_catchments){
  data <- data.loader(catchment)
  year_range <- year_ranges[[catchment]]
  wt_preprocess(data, year_range = year_range, catchment = catchment)
}
