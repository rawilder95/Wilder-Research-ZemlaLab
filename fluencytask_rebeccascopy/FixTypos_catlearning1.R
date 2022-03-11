if (getwd()!= "~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/logs"){
  setwd("~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/")
}

# install.packages("data.table")
# library(data.table)


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

spellcheck <- c(instruments_clean, vegetables, furniture, clothing, cities, countries, fruits, animals, transportation, measurements, sports, kitchen)






write.csv(spellcheck, "spellcheckdata.csv")

nsubj <- unique(dat$id)

# install.packages("googlesheets4")
# library(googlesheets4)

access_sheets<- data.table(read_sheet('https://docs.google.com/spreadsheets/d/1AAPf2miO7cbkWZROg7BS8gpE-nXgK7xexbjLMkJ4mv0/edit?usp=sharing')
)



ra_sheet<- data.table(read_sheet('https://docs.google.com/spreadsheets/d/1AAPf2miO7cbkWZROg7BS8gpE-nXgK7xexbjLMkJ4mv0/edit?usp=sharing'))

ra_sheet <- ra_sheet[order(-rank(Unchecked), Category)]



write_sheet(data.frame(my_sheet), ss= 'https://docs.google.com/spreadsheets/d/1AAPf2miO7cbkWZROg7BS8gpE-nXgK7xexbjLMkJ4mv0/edit?usp=sharing', sheet= 1)

trial <- unique(c(ra_sheet$Unchecked, ra_sheet$SpellChecked))

my_sheet <- data.table(c(unique(spellcheck[!spellcheck %in% trial])))

sheet_append(ss= 'https://docs.google.com/spreadsheets/d/1AAPf2miO7cbkWZROg7BS8gpE-nXgK7xexbjLMkJ4mv0/edit?usp=sharing', data.frame(ra_sheet$Checked), 1)
                      


sheet_append(ss= 'https://docs.google.com/spreadsheets/d/1AAPf2miO7cbkWZROg7BS8gpE-nXgK7xexbjLMkJ4mv0/edit?usp=sharing', data.frame(to_import), 1)

sort(unique(c(trial,spellcheck[!spellcheck %in% trial])))

my_sheet <- dat[order(-rank(item), category)]

my_sheet <- my_sheet[order(-rank(item),category)]
to_import <- my_sheet$item[!my_sheet$item %in% trial]




import_fr <- my_sheet[order(-rank(item)%in%to_import,category)]



sheet_append(ss= 'https://docs.google.com/spreadsheets/d/1AAPf2miO7cbkWZROg7BS8gpE-nXgK7xexbjLMkJ4mv0/edit?usp=sharing', data.frame(to_import)


             
             
ra_sheet[ra_sheet== "NONSENSE"] <- print(paste("SENSE"))    

sheet_append(ss= "https://docs.google.com/spreadsheets/d/1AAPf2miO7cbkWZROg7BS8gpE-nXgK7xexbjLMkJ4mv0/edit?usp=sharing", data.frame(ra_sheet), 1)


