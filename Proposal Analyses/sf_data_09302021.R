if (getwd()!= "/Users/rebeccawilder/Desktop/Research 2021-2022"){
  setwd("~/Desktop//Research 2021-2022")
}

# install.packages("data.table")
library(data.table)
library(ggplot2)

dat <- data.table(read.csv("snafu_sample.csv"))

dat= dat[dat[, group== "Experiment1" & category=="fruits"]]
nsubj= unique(dat[,id])
# Create Col Ranking List Reps 1-3
dat[listnum<3, listrank:= 1]
dat[listnum>=3 & listnum<6, listrank := 2]
dat[listnum>=6, listrank:= 3]

# Get unique items for list 1
(items1 <- unique(dat[listrank== 1, item]))
(items2 <- unique(dat[listrank== 2, item]))
(items3 <- unique(dat[listrank== 3, item]))

pfruit1 <- vector()
pfruit2 <- vector()
pfruit21 <- vector()

for (i in 1:length(items1)){
  pfruit1[i]  <- sum(dat[listrank==1 & item== items1[i], .N, by= id]$N>0)/length(nsubj)
   pfruit2[i]  <- (sum(dat[listrank==2 & item== items1[i], .N, by= id]$N>0))/length(nsubj)
    pfruit21[i]  <- sum(dat[listrank<3 & item== items1[i], .N, by= id]$N>1)/sum(dat[listrank==1 & item== items1[i], .N, by= id]$N>0)
}

  tot_fruit <- data.table()
  
  tot_fruit[,l1:= pfruit1]
  tot_fruit[, l2:= pfruit2]
  tot_fruit[, l21:= pfruit21]
library("tidyverse")

distinct(tot_fruit)  
  
  
tot_fruit %>% select(c('l2','l21',)) %>% unique()

##############################################################################################################
dat2 <- data.table(read.csv("snafu_sample.csv"))
dat2 <- dat2[dat2[, group== "Experiment1" & category=="vegetables"]]
dat2[listnum < 3,listrank := 1]
dat2[listnum>=3 & listnum < 6,listrank := 2]
dat2[listnum>=6,listrank := 3]

# Get unique items for list 1
(vitems1 <- unique(dat2[listrank== 1, item]))
(vitems2 <- unique(dat2[listrank== 2, item]))
(vitems3 <- unique(dat2[listrank== 3, item]))


pveg1 <- vector()
pveg2 <- vector()
pveg21 <- vector()


for (i in 1:length(items1)){
  pveg1[i]  <- sum(dat2[listrank==1 & item== vitems1[i], .N, by= id]$N>0)/length(nsubj)
   pveg2[i]  <- (sum(dat2[listrank==2 & item== vitems1[i], .N, by= id]$N>0))/length(nsubj)
    pveg21[i]  <- sum(dat2[listrank<3 & item== vitems1[i], .N, by= id]$N>1)/sum(dat2[listrank==1 & item== vitems1[i], .N, by= id]$N)
}

tot_veg <- data.table()
tot_veg[, l1:= pveg1]
tot_veg[, l2:= pveg2]
tot_veg[, l21:= pveg21]

library(ggplot2)

(get_freq <- table(tot_fruit[,l2],tot_fruit[,l1]))

# tot_fruit[, freq:= ]

ggplot() + geom_point(aes(x= tot_fruit$l2, y= tot_fruit$l21, color= "Fruits", size= ), alpha= 0.5, show.legend = TRUE)+geom_point(aes(x= tot_veg$l2, y= tot_veg$l21, color= "Vegatables"), alpha= 0.5, show.legend = TRUE)+ labs(x= "p(Item in SF2)", y= "p(Item in SF2 | Item in SF1)", color= "Category", title= "Conditional Probability of Repeated Semantic Retrievals")
ggsave('probability_fruitsandvegetables', device= 'png', dpi= 300)
  
tf1 <- vector()
tf2 <- vector()
tf3 <- vector() 

for (i in 1:length(items2)){
  tf1[i]  = sum(dat[listrank==1 & item== items1[i], .N, by= id]$N)
  tf2[i]  = sum(dat[listrank==2 & item== items2[i], .N, by= id]$N)-1
}



tf1
tf2

