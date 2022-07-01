if (getwd()!= "~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/logs"){
  setwd("~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/")
}
#clear environment and load relevant packages
rm(list= ls())
library(lme4)
library(cowplot)
library(ggplot2)
library(data.table)
# setup dt and indexing vectors
dat <- data.table(read.csv("final_results.csv"))
dat<- subset(dat,select=-c(X))
nsubj= unique(dat$id)
ncat= unique(dat$category)
# get rid of category sequence error
bad_cats= dat[, .N, by= .(category, game, id)]
badcat_table= bad_cats[, .N, by= .(id, category)]
badcat_table= badcat_table[N==2]
dat= merge(dat, badcat_table)
dat= dat[N== 2]
dat= subset(dat, select= -c(N))
dat[, both_trials:= 0]
# set both_trials col for items repeated across both trials of cat[i]
#both_trials== 1 -> subject listed item on trial 1 and trial 2
#both_trials == 0 -> subject only listed the item on either trial 1 *or* trial2
for (subject in nsubj){
  for(cats in ncat){
    if(nrow(dat[id== subject & category== cats])){
      this_subj = dat[id== subject & category== cats]
      this_subj[, both_trials:= 0]
      rep_item= intersect(this_subj[listrank== 2,item], this_subj[listrank== 1, item])
      this_subj[item %in% rep_item]$both_trials= 1
      dat[id== subject & category== cats]= this_subj
    }
  }
}



