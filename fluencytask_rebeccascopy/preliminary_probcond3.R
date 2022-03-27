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

dat <- data.table(read.csv("snafu_sample.csv"))

dat<- dat[group== "Experiment1"]
dat[listnum<3, listrank:= 1]
dat[listnum>=3 & listnum <6, listrank := 2]
dat[listnum>=6, listrank := 3]

dat<- dat[listrank<3,]
nsubj <- unique(dat$id)
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




old_old <- 0


  for (i in 1:(length(trial2$item)-1)){
    if(sum(trial2[i]$rep_before== "TRUE",trial2[i+1]$rep_before== "TRUE")>1){
      old_old= old_old+1
    }else{
      old_old=old_old+0
    }
  }
# if you need to clear this var
# transition_probabilites <- data.table() 
old_old <-0 
old_new <- 0
new_old <-0
new_new <-0
transition_probabilities <- data.table(id=character(), category=character(), pOldOld=numeric(), pOldNew=numeric(), pNewNew=numeric(), pNewOld=numeric())
for (subject in nsubj){
  for (cats in unique(dat$category)){
    trial2 <- dat[id== subject & category== cats & listrank==2,]
    for (i in 1:(nrow(trial2)-1)){
      if(sum(trial2[i]$both_trials== 1,trial2[i+1]$both_trials== 1)>1){
        old_old= old_old+1
      }else{
        old_old=old_old+0
      }
      if(sum(trial2[i]$both_trials== 0,trial2[i+1]$both_trials== 1)>1){
        new_old= new_old+1
      }else{
        new_old=new_old+0
      }
      if(sum(trial2[i]$both_trials== 1,trial2[i+1]$both_trials== 0)>1){
        old_new= old_new+1
      }else{
        old_new=old_new+0
      }
      if(sum(trial2[i]$both_trials== 0,trial2[i+1]$both_trials== 1)>1){
        new_new= new_new+1
      }else{
        new_new=new_new+0
      }
    }
    
  }
  newrow <- list(id=subject, category=cats, pOldOld=old_old/(old_new+old_old), pOldNew=old_new/(old_new+old_old), pNewNew= new_new/(new_old+new_new), pNewOld= new_old/(new_old+new_new))
  transition_probabilites <- rbind(transition_probabilities, newrow)
  return(transition_probabilites)
}
  




