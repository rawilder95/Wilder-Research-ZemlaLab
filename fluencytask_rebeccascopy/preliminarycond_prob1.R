if (getwd()!= "~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/logs"){
  setwd("~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/")
}

# ##Relevant Libraries & Packages###
# install.packages("data.table")
# library(data.table)
# install.packages("googlesheets4")
# library(googlesheets4)
# install.packages("ggplot2")
# library(ggplot2)


## Pull data back to original dataframe ##
ra_sheet<- data.table(read_sheet('https://docs.google.com/spreadsheets/d/14YJ7IpvEyFVRSqr3zo3SAqgyR6g0QYAbI5xzxh3rl_A/edit?usp=sharing'))


dat <- data.table(read.csv("results_cleaned.csv"))

if(sum(colnames(dat)%in% "X")>0 | sum(colnames(dat%in% "X.1"))>0){
  dat<- subset(dat,select=-c(X,X.1))
}

nsubj <- unique(dat$id)
dat

# write.csv(dat, "results_cleaned.csv")



##Actual Comparison##
cat_names <- unique(dat$category)
#Set up data with listrank
for (i in 1:length(nsubj)){
  for (j in 1:length(cat_names)){
    this_game <- dat[id== nsubj[i] & category== cat_names[j], game]
    dat[id== nsubj[i] & category== cat_names[j], listnum:= this_game== max(this_game)]
  }
}

dat[listnum== "FALSE", listrank:= 1]
dat[listnum == "TRUE", listrank:= 2]

dat<- subset(dat,select=-c(listnum))

second_trials <- dat[listrank== 2,]
first_trials <-dat[listrank == 1,]

#practice on a category you're familiar with; generalize it afterwards


# Old analysis from last semester
p_trial2p_trial1 <- vector()
for i in 1:length N Categories
for (cat_i in 1:length(cat_names)){
  # Grab all unique items from the second trial of category[i]
  this_category<- dat[category== cat_names[cat_i],]
  item_trial2= unique(this_category[listrank== 2,item])
  p_trial2 <- vector()
  # loop through all of the unique trial 2 items
  for (n2items in 1:length(item_trial2)){
    both_trials <- intersect(this_category[listrank==1 & item== item_trial2[i]]$id, this_category[listrank==2 & item== item_trial2[i]])
    p_trial2[i] <- sum(this_category[listrank== 2 & item== item_trial2[i], .N, by= id]$N)-1
    if (length(both_trials)>0){
      p_trial2p_trial1[i] <- length(both_trials)/sum(this_category[listrank== 1 & item== item_trial2[i], .N, by= id]$N)
    }else{
      p_trial2p_trial1[i] <- 0
    }
  }
}








for (i in 1:length(item_trial2)){
  both_trials <- intersect(this_category[listrank==1 & item== item_trial2[i]]$id, this_category[listrank==2 & item== item_trial2[i]]$id)
  p_trial2[i] <- sum(this_category[listrank== 2 & item== item_trial2[i], .N, by= id]$N)-1
  if (length(both_trials)>0){
    p_trial2p_trial1[i] <- length(both_trials)/sum(this_category[listrank== 1 & item== item_trial2[i], .N, by= id]$N)
  }else{
    p_trial2p_trial1[i] <- 0
  }
}

pthis_category <- data.table()
pthis_category[,p_trial2:= p_trial2/length(nsubj)]
pthis_category[,p_trial2p_trial1:= p_trial2p_trial1]
ggplot(data= data.frame(pthis_category), aes(x= p_trial2, y= p_trial2p_trial1, color= "this_category"))+ geom_count(alpha= 0.3)+ geom_abline() + labs(x= "Item Typicality", y= "p(B|A)", color= "Category", size= "Frequency") + xlim(0,1)







