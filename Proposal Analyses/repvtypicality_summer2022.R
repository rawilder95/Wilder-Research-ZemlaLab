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

# set up data file
dat <- data.table(read.csv("final_results.csv"))
nsubj= unique(dat$id)
ncat= unique(dat$category)
for (i in 1:length(nsubj)){
  for (j in 1:length(ncat)){
    this_game <- dat[id== nsubj[i] & category== ncat[j], game]
    dat[id== nsubj[i] & category== ncat[j], listnum:= this_game== max(this_game)]
  }
}
dat[listnum== "FALSE", listrank:= 1]
dat[listnum == "TRUE", listrank:= 2]
dat<- subset(dat,select=-c(listnum,X))
dat[, both_trials := 0]

# get items that were listed in both trials 
for (i in 1:length(nsubj)){
  this_subj <- dat[id==nsubj[i],]
  repeated_words <- intersect(this_subj[listrank==2,item], this_subj[listrank==1,item])
  this_subj[, both_trials:= 0]
  this_subj[item %in% repeated_words, both_trials:=1]
  dat[id==nsubj[i]]$both_trials <- this_subj$both_trials
}

# check for perseverative errors
# how to get indices of perseverations
# this_subj[listrank== 1,.N, by= item]

# This loop looks up and gets rid of perseverative erros by setting to NaN
check4err= data.table()
for (subject in nsubj){
  for (cats in ncat){
    this_subj <- dat[listrank== 1 & id== subject & category== cats]
    if(any(this_subj[listrank== 1,.N, by= item]$N> 1)){
      check4err <- this_subj[listrank== 1,.N, by= item]
      get_err <- this_subj[item %in% check4err[check4err$N>1]$item]
      smallest_val <- min(get_err$itemnum)
      # larger_val <- get_err[itemnum != smallest_val]
      this_subj[item %in% unique(get_err$item) & itemnum %in% smallest_val]$item = NaN
      dat[id== subject & category == cats & listrank== 1] <- this_subj
    }
  }
}




# isolate items listed in repeated fluency trial
trial2= dat[listrank==2]
trial1= dat[listrank==1]
# p(listing a word on list2| that it was listed on list 1)
pba1= vector()
for (i in 1:length(unique(trial2[both_trials==1 & !is.na(item)]$item))){
  pba1[i]= sum(trial1$item %in% trial2[both_trials==1 & !is.na(item)]$item[i])/length(nsubj)
}

typicality1= (trial2[item %in% unique(trial2[both_trials==1 & !is.na(item)]$item), .N, by= .(item)]$N-1)/length(nsubj)

# get typicality score
sus_words= vector()
typicality2= vector()
pba1= vector()
for (i in 1:length(unique(trial2[both_trials==1 & !is.na(item)]$item))){
  word2= unique(trial2[both_trials==1 & !is.na(item)]$item)
  typicality2[i]= (sum(trial2[item %in% word2[i], .N, by= id]$N)-1)/length(nsubj)
  pba1= trial2[item%in% unique(trial1[both_trials==1]$item), .N, by= item]$N/length(nsubj)
  if(typicality2[i]>1){
    sus_words[i]= word2[i]
  }
}

word2= word2[1:length(typicality2)]

# put it all in a data table for legibility
word2_dt= data.table(word2, pba1, typicality2)








colnames(word2_dt) = c("Item", "p(B|A)", "Typicality") 




ggplot() + geom_count(aes(x= typicality2[1:975], y= pba1))

for (subject in nsubj){
  for (cats in ncat){
    trial2= dat[id== subject & category== cats & listrank==2]
    trial1= dat[id== subject & category== cats & listrank==1]
    uwords2= unique(trial2$item)
    uwords1= unique(trial2[both_trials==1]$item)
    
  }
}



k= dat[, .N, by= .(both_trials, item)]
bt= vector()
for (i = 1:length(unique(k$item))){
  bt[i]= k[item== item[i] & both_trials== 1]
}



words2= dat[listrank2]

