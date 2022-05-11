library(data.table)
if (getwd()!= "~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/logs"){
  setwd("~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/")
}
dat <- data.table(read.csv("final_results.csv"))

for (i in 1:length(nsubj)){
  for (j in 1:length(ncat)){
    this_game <- dat[id== nsubj[i] & category== ncat[j], game]
    dat[id== nsubj[i] & category== ncat[j], listnum:= this_game== max(this_game)]
  }
}

dat[listnum== "FALSE", listrank:= 1]
dat[listnum == "TRUE", listrank:= 2]

dat<- subset(dat,select=-c(listnum))


trial2 <- dat[listrank==2]
# Set up everything for function
dat[, both_trials := 0]
set_reptrials <- function(i) {
  this_subj <- dat[id==nsubj[i],]
  repeated_words <- intersect(this_subj[listrank==2,item], this_subj[listrank==1,item])
  this_subj[, both_trials:= 0]
  this_subj[item %in% repeated_words, both_trials:=1]
  dat[id==nsubj[i]]$both_trials <- this_subj$both_trials
}

#Now all trials have both trials
set_reptrials(length(nsubj))



for (subject in nsubj){
  for (cats in unique(dat$category)){
    this_subj <- dat[id== subject & category== cats,]
    repeated_words <- intersect(this_subj[listrank==1,item], this_subj[listrank==2,item])
    this_subj[item %in% repeated_words, both_trials:= 1]
    this_subj[!item %in% repeated_words, both_trials:=0]
    dat[id== subject & category== cats, both_trials:= this_subj$both_trials]
  }
}




