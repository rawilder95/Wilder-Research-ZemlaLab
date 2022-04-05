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

bsoldold <- vector()
bsoldnew <- vector()
bsnewold <- vector()
bsnewnew <- vector()

### This is where the bootstrapping comes in #####
bts_probabilities <- data.table(bsoldold= numeric(), bsoldnew= numeric(), bsnewold= numeric(), bsoldold)




for (subject in nsubj){
  for (cats in unique(dat$category)){
    trial2 <- sample(dat[id== subject & category== cats & listrank==2,])
    for (i in 1:(nrow(trial2)-1)){
      if(trial2[i]$both_trials== 1 & trial2[i+1]$both_trials== 1){
        bsoldold[i] <- mean(sample(dat[id== subject & category== cats,both_trials], 1000, replace= TRUE))
      } else if (trial2[i]$both_trials== 0 & trial2[i+1]$both_trials== 1){
        bsoldnew[i] <- mean(sample(dat[id== subject & category== cats,both_trials], 1000, replace= TRUE))
      } else if(trial2[i]$both_trials== 1 & trial2[i+1]$both_trials== 0){
        bsnewold[i] <- mean(sample(dat[id== subject & category== cats,both_trials], 1000, replace= TRUE))
      } else if(trial2[i]$both_trials== 0 & trial2[i+1]$both_trials== 0){
        bsnewnew[i] <- mean(sample(dat[id== subject & category== cats,both_trials], 1000, replace= TRUE))
      }
    }
    newrow <- list(bsoldold, bsoldnew, bsnewold, bsnewnew)
    bts_probabilities <- rbind(bts_probabilities, newrow, fill= TRUE)
  }
}


 for (subject in nsubj){
  for (cats in unique(dat$category)){
    old <- 0
    new <- 0
    trial2 <- sample(dat[id== subject & category== cats & listrank==2,])
    for (i in 1:nrow(trial2)){
      if (trial2$both_trials[i]==1){
        old= old +1
      } else {
        new = new + 1
      }
    }
    bts_probabilities[id== subject & category== cats]$old= old
    bts_probabilities[id== subject & category== cats]$new= new
  }
}



for (subject in 1: nsubj)
# bts_probs<- data.table()
# bts_probs[,old_old:= t$OldOld/(t$OldOld+t$OldNew)]
# bts_probs[,old_new:= t$OldNew/(t$OldOld+t$OldNew)]
# bts_probs[,new_old:= t$NewOld/(t$NewOld+t$NewNew)]
# bts_probs[,new_new:= t$NewNew/(t$NewOld+t$NewNew)]

bts_p1 <- sample(dat[id== nsubj[1] & category== category[1],both_trials], 1000, replace= TRUE)

btw_p1 <- data.table()
get_probs <- vector()

for (subject in nsubj){
  for (cats in unique(dat$category)){
    this_subj <- dat[id== subject & category== cats &listrank== 2,]
    shuffled_subj <- this_subj
    shuffled_subj$item<- sample(this_subj$item, category[])
  }
}

