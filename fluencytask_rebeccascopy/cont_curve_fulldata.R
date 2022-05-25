library(data.table)
if (getwd()!= "~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/logs"){
  setwd("~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/")
  
  
}

# install.packages("ggplot2")
# library(ggplot2)



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


# glmer(rep ~ typicality * temporal-interval + (1|category) + (1|id), data=dat, family="binomial")
96/5

#Now all trials have both trials
set_reptrials(length(nsubj))
# this_subj[, oldold:= 0]


# Transitions that were contiguous


list1= this_subj[listrank==1]
list2= this_subj[listrank==2]

list1[which(list1$item %in% list2[both_trials==1]$item)]

cont_transitions= vector()
# Go through old old transitions
# for (i in 1:(length(this_subj$both_trials)-1)){
#   this_subj[dat]
#   subj_idx= this_subj[idx== i]
#   word= this_subj[i]$item
#   if(this_subj$both_trials[i]==1 & this_subj$both_trials[i+1]==1){
#     list1[which(list1$item %in% word)]
#   }
# }
# 










cont_transitions <- data.table(id= character(), category= character(), word= character(), contiguous= numeric())
for (subject in nsubj){
  for (cats in ncat){
    idx_oldold <- 0
    this_subj <- dat[id== subject & category== cats & listrank== 2,]
    if(length(this_subj$item > 5)){
      for (item in 1:(length(this_subj$item)-1)){
        if(this_subj$both_trials[item]== 1 & this_subj$both_trials[item+1]== 1){
          idx_oldold <- 1
          print(idx_oldold)
        }else{
          idx_oldold <- 0
        }
        newlist <- list(subject, cats, this_subj$item[item], idx_oldold)
        cont_transitions <- rbind(cont_transitions, newlist)
      }
    }
  }
  
  word_dist <- vector()
  for (subject in nsubj){
    for (cats in unique(dat$category)){
      this_subj <- dat[id== subject & category== cats & listrank== 1]
      for (i in 1:length(unique(cont_transitions[id== subject, category== cats]$word))){
      }
    }
  }
}
this_subj <- dat[id== subject & category== cats,]
repeated_words <- intersect(this_subj[listrank==1,item], this_subj[listrank==2,item])
this_subj[item %in% repeated_words, both_trials:= 1]
this_subj[!item %in% repeated_words, both_trials:=0]
dat[id== subject & category== cats, both_trials:= this_subj$both_trials]


dat <- subset(dat,select=-c(X))
# subject= nsubj[1]
# cats= ncat[2]


lag_crp= data.table()
for (subject in nsubj){
  for (cats in ncat){
    this_subj= dat[id== subject & category== cats]
    word1 = this_subj[listrank== 1]
    word2= unique(this_subj[listrank== 2 & both_trials==1]$item)
    sp= word1[which(item %in% word2),itemnum]
    transitions= vector()
    for (i in 2:length(sp)-1){
      transitions[i]= sp[i+1] - sp[i]
    }
    # transitions returning NA for some reason
    if(!is.na(all(transitions))){
      transition_names= c(min(transitions):max(transitions))
    }
    get_counts= vector()
    for (j in 1:length(transition_names)){
      get_counts[j]= length(which(transitions== transition_names[j]))
    }
    possible_transitions= vector()
    for (i in 1:length(transitions)){
      possible_transitions[i]=  
    }
     new_row= list(get_counts, transition_names, sp)
    lag_crp = rbind(lag_crp, new_row, use.names= FALSE)
  }
}



# colnames(lag_crp) <- c("Transition Counts", "Transition Names", "Serial Position")



ggplot(data= lag_crp)+ geom_count(aes(x= ))




