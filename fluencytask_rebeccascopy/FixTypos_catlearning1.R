if (getwd()!= "~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/logs"){
  setwd("~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/")
}




dat <- data.table(read.csv("results_cleaned.csv"))
                          


cat_names <- unique(dat[, category])
instruments_clean <- sort(unique(dat[category== cat_names[1],item]))

vegetables <- sort(unique(dat[category== cat_names[2],item]))

furniture <- sort(unique(dat[category== cat_names[3],item]))

clothing <- sort(unique(dat[category== cat_names[4],item]))

cities <- sort(unique(dat[category== cat_names[5],item]))

countries <- sort(unique(dat[category== cat_names[6],item]))

fruits <- sort(unique(dat[category== cat_names[7],item]))

animals <- sort(unique(dat[category== cat_names[8],item]))

transportation <- sort(unique(dat[category== cat_names[9],item]))

measurements <- sort(unique(dat[category== cat_names[10],item]))

sports <- sort(unique(dat[category== cat_names[11],item]))

kitchen <- sort(unique(dat[category== cat_names[12],item]))



