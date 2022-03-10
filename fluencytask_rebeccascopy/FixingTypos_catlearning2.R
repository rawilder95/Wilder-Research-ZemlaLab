if (getwd()!= "~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/logs"){
  setwd("~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/")
}

###Relevant Libraries & Packages###
# install.packages("data.table")
# library(data.table)
# install.packages("googlesheets4")
# library(googlesheets4)




# Read in Data From RA Google Sheets
ra_sheet<- data.table(read_sheet('https://docs.google.com/spreadsheets/d/14YJ7IpvEyFVRSqr3zo3SAqgyR6g0QYAbI5xzxh3rl_A/edit?usp=sharing'))

# Sort Alphabetically 
ra_sheet <- ra_sheet[order(-rank(Unchecked), Category)]

# Read in New Data

dat <- data.table(read.csv("results_cleaned.csv"))

# Parse by category and sort alphabetically
at_names <- unique(dat[, category])
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

spellcheck <- c(instruments_clean, vegetables, furniture, clothing, cities, countries, fruits, animals, transportation, measurements, sports, kitchen)

# write an arbitrary data table so that you can append in sorted data that was *not* in last batch
to_import <- data.table(spellcheck[!spellcheck %in% ra_sheet$Unchecked])

sheet_append(ss= 'https://docs.google.com/spreadsheets/d/14YJ7IpvEyFVRSqr3zo3SAqgyR6g0QYAbI5xzxh3rl_A/edit?usp=sharing', to_import, 1)



