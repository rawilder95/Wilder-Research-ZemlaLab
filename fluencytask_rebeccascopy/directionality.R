library(data.table)
if (getwd()!= "~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/logs"){
  setwd("~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/")
}

# install.packages("data.table")
# library(data.table)
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
dat<- subset(dat,select=-c(X))


randsubj= dat[id== nsubj[1] & category== ncat[4]]

trial1= randsubj[listrank==1]
trial2= randsubj[listrank==2]
trial1[, sp:= 1:length(trial1$item)]
trial2[, sp:= NaN]

trial2$item[1] %in% trial1

for (i in 1:length(trial2$item)){
  if(trial2[i]$item %in% trial1$item){
    trial2$sp[i]= which( trial1$item %in% trial2$item[i])
  } else{
    trial2$sp[i]= NaN
  }
}

# Get transitions
transitions = vector()
for (j in 1:(length(trial2$item)-1)){
  transitions[j]= trial2$sp[j]- trial2$sp[j+1]
}

get_counts= vector()

transitions[is.nan(transitions)]=0

transition_range= c(min(transitions): max(transition))

transitions= subset(transitions, select(transition_range==0))


# Get actual transition tally
for (i in 1:length(transitions)){
  get_counts[i]= sum(transitions %in% transitions[i])
}

# Bind everything together as a table
get_counts[is.nan(transitions)]= NaN
cbind(transitions, get_counts)

