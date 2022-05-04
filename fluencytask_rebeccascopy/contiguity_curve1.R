dat <- data.table(read.csv("results_cleaned.csv"))

# dat<- dat[group== "Experiment1"]
# dat[listnum<3, listrank:= 1]
# dat[listnum>=3 & listnum <6, listrank := 2]
# dat[listnum>=6, listrank := 3]

# dat<- dat[listrank<3,]
nsubj <- unique(dat$id)
ncat <- unique(dat$category)
dat[, listrank:= 0]

for (i in nsubj){
  for (j in ncat){
    this_game <- dat[id== nsubj[i] & category== cat_names[j], game]
    dat[id== nsubj[i] & category== cat_names[j], listnum:= this_game== max(this_game)]
  }
}

dat[listnum== "FALSE", listrank:= 1]
dat[listnum == "TRUE", listrank:= 2]

dat<- subset(dat,select=-c(listnum))


trial2 <- dat[listrank==2]
# Set up everything for function
dat[, both_trials := 0]




  for (subject in nsubj){
    for (cats in unique(dat$category)){
      this_subj <- dat[id== subject & category== cats,]
      repeated_words <- intersect(this_subj[listrank==2,item], this_subj[listrank==1,item])
      this_subj[, both_trials:= 0]
      this_subj[item %in% repeated_words, both_trials:=1]
      dat[id== subject & category == cats]$both_trials <- this_subj$both_trials

    }
  }


cont_transitions <- data.table(id= character(), category= character(), word= character(), contiguous= numeric())
for (subject in nsubj){
  for (cats in (unique(dat$category))){
    idx_oldold <- 0
    this_subj <- dat[id== subject & category== cats & listrank== 2,]
    for (item in 2:length(this_subj$item)-1){
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


this_subj <- dat[id== subject & category == cats & listrank == 2]

this_subj
sp = 1:length(this_subj$both_trials)
bt= this_subj$both_trials
words= this_subj$item
pt = dat[id== subject & category == cats & listrank == 1]
sp1 = 1:length(pt$both_trials)

bt1 = pt$both_trials
words1 = pt$words

sp[bt== 0] <- NaN
words[bt== 0] <- 0


# which(a== b)
# > a
# [1] 4 5 6 7
# > b
# [1] 1 6 4 3 9
# >
#   > which(a==6)
# [1] 3
# > which(a==4)
# [1] 1
# > a<-c("apple)
# +
# > a<-c("apple", "banana", "kiwi")
# > b<-c("kiwi", "banana", "mango")
# > which(a == "kiwi")
# [1] 3
# > which(a == "kiwi") Gives you index- if there are any repetitions 

for (i in 2:length(bt)-1){
  
  
}

that_subj <- dat[id== subject & category== cats & listrank== 1]


that_subj[which(this_subj$item[1]== that_subj$item)]


for (subject in nsubj)
  




