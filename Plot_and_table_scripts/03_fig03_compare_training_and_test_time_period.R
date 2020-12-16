# training vs test period
# wateRtemp project
# Moritz Feigl, Nov 2020

setwd("C:/Users/morit/Dropbox/WT_Project/Modelling/VSC_results/final_models/Inn")
library(ggplot2)
inputs <- feather::read_feather("input_data.feather")
test <- feather::read_feather("test_data.feather")

all <- rbind(inputs, test)
all$year <- as.integer(format(all$date, "%Y"))
all$mon <- as.integer(format(all$date, "%m"))
all$is_last <- 0
all$is_last[all$year == 2015] <- 1
means <- aggregate(wt~year, all[all$mon %in% c(6, 7, 8), ], mean)
# mean water temperature 2015 vs rest
aggregate(wt~is_last, all[all$mon %in% c(6, 7, 8), ], mean)

# distribution summery of water temperature of 2015 vs rest
aggregate(wt~is_last, all[all$mon %in% c(6, 7, 8), ], summary)
aggregate(wt~is_last, all, summary)

# number of events larger than 11 degrees
ev11 <- aggregate(wt~year, all, function(x) sum(x > 11))
ev11
mean(ev11[-19, 2])
mean(ev11[19, 2])


catchments <- c("Kleine_Muehl", "Aschach", "Erlauf", "Traisen", "Ybbs", "Saalach",
                "Enns", "Inn", "Salzach", "Donau")
extreme_days <- NULL
mean_wts <- NULL
days_above_thrs_all <- NULL

for(catchment in catchments){
  setwd(paste0("C:/Users/morit/Dropbox/WT_Project/Modelling/VSC_results/final_models/", catchment))

  inputs <- feather::read_feather("input_data.feather")
  inputs <- data.frame(inputs, "is_test" = FALSE)
  test <- feather::read_feather("test_data.feather")
  test <- data.frame(test, "is_test" = TRUE)
  all <- rbind(inputs, test)
  all$year <- as.integer(format(all$date, "%Y"))
  all$mon <- as.integer(format(all$date, "%m"))
  all$is_last <- 0
  all$is_last[all$year == 2015] <- 1
  print(catchment)
  print(unique(all$year[all$is_test]))
  print(unique(all$year[!all$is_test]))
  # number of events larger than 90% quantile
  thrs <- quantile(all$wt, 0.9)
  days_above_thrs <- aggregate(wt~year, all, function(x) sum(x > thrs))
  test_years <- unique(all$year[all$is_test])

  # Plot
  days_above_thrs <- cbind(days_above_thrs, is_test = FALSE, catchment = catchment)
  days_above_thrs$is_test[days_above_thrs$year %in% test_years] <- TRUE
  days_above_thrs_all <- rbind(days_above_thrs_all, days_above_thrs)

  # Table results
  median_90days_train <- round(median(days_above_thrs[!(days_above_thrs$year %in% test_years), 2]), 2)
  median_90days_test <- round(median(days_above_thrs[days_above_thrs$year %in% test_years, 2]), 2)
  median_90days_not2015 <- round(median(days_above_thrs[days_above_thrs$year != 2015, 2]), 2)
  median_90days_2015 <- round(median(days_above_thrs[days_above_thrs$year == 2015, 2]), 2)
  extreme_days <- rbind(extreme_days,
                        data.frame("catchment" = catchment,
                                   "median_90days_train" = median_90days_train,
                                   "median_90days_test" = median_90days_test,
                                   "train_test_increase" = round(median_90days_test/median_90days_train, 2),
                                   "train_test_increase_median" = NA,
                                   "median_90days_not2015" = median_90days_not2015,
                                   "median_90days_2015" = median_90days_2015,
                                   "increase_2015" = round(median_90days_2015 / median_90days_not2015, 2),
                                   "increase_2015_median" = NA

                        ))

  # means of time periods
  days_above_thrs <- aggregate(wt~year, all, mean)

  test_years <- unique(all$year[all$is_test])
  means_train <- round(mean(days_above_thrs[!(days_above_thrs$year %in% test_years), 2]), 2)
  means_test <- round(mean(days_above_thrs[days_above_thrs$year %in% test_years, 2]), 2)
  means_not2015 <- round(mean(days_above_thrs[days_above_thrs$year != 2015, 2]), 2)
  means_2015 <- round(mean(days_above_thrs[days_above_thrs$year == 2015, 2]), 2)
  mean_wts <- rbind(mean_wts,
                        data.frame("catchment" = catchment,
                                   "means_train" = means_train,
                                   "means_test" = means_test,
                                   "train_test_diff" = round(means_test - means_train, 2),
                                   "train_test_diff_median" = NA,
                                   "means_not2015" = means_not2015,
                                   "means_2015" = means_2015,
                                   "diff_2015" = round(means_2015 - means_not2015, 2),
                                   "diff_2015_median" = NA
                        ))

}

extreme_days$train_test_increase_median <- round(median(extreme_days$median_90days_test/extreme_days$median_90days_train), 2) * 100
extreme_days$increase_2015_median <- round(median(extreme_days$median_90days_2015 / extreme_days$median_90days_not2015), 2) * 100
mean_wts$train_test_diff_median <- round(median(mean_wts$means_test - mean_wts$means_train), 2)
mean_wts$diff_2015_median <- round(median(mean_wts$means_2015 - mean_wts$means_not2015), 2)
extreme_days
mean_wts

# train test increase
cat("Median percentage increase of days above the 90% quantile in training/test time period:", extreme_days$train_test_increase_median[1], "%\n",
"Median percentage increase of days above the 90% quantile in 2015:", extreme_days$increase_2015_median[1], "%\n",
"Median increase of mean yearly temperature in training/test time period:", mean_wts$train_test_diff_median[1], "?C\n",
"Median increase of mean yearly temperature in 2015:", mean_wts$diff_2015_median[1], "?C\n"
)


# Plots
setwd("C:/Users/morit/Dropbox/WT_Project/Manuscript/Paper 1 - ML focus")
days_above_thrs_all$is_test[days_above_thrs_all$is_test] <- "Test time period"
days_above_thrs_all$is_test[days_above_thrs_all$is_test == "FALSE"] <- "Training/validation time period"
days_above_thrs_all$is_test <- factor(days_above_thrs_all$is_test,
                                   levels = c("Training/validation time period", "Test time period"))
days_above_thrs_all$catchment[days_above_thrs_all$catchment == "Kleine_Muehl"] <- "Kleine Mühl"
ggplot(days_above_thrs_all, aes(x = is_test, y = wt, fill = catchment)) +
  geom_boxplot() + labs(x = "",
                        y = "Number of days with stream temperatures\nabove the 90% quantile per year",
                        fill = "Catchment")  +
  theme(legend.text=element_text(size=12),
        legend.title = element_text(size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12)) +
  ggsave("Figures/fig03.png", height = 6, width = 12, units = "in")


# Salzach Plot
catchment <- "Salzach"
setwd(paste0("C:/Users/morit/Dropbox/WT_Project/Modelling/VSC_results/final_models/", catchment))

inputs <- feather::read_feather("input_data.feather")
inputs <- data.frame(inputs, "is_test" = FALSE)
test <- feather::read_feather("test_data.feather")
test <- data.frame(test, "is_test" = TRUE)
all <- rbind(inputs, test)
all$year <- as.integer(format(all$date, "%Y"))
all$mon <- as.integer(format(all$date, "%m"))
all$is_last <- 0
all$is_last[all$year == 2015] <- 1
# number of events larger than 90% quantile
thrs <- quantile(all$wt, 0.9)
days_above_thrs <- aggregate(wt~year, all, function(x) sum(x > thrs))
test_years <- unique(all$year[all$is_test])
head(all)
ggplot(all[all$year == 2015,], aes(x = date, y = wt)) + geom_line() + geom_hline(yintercept = thrs)

