if (getwd()!= "~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/logs"){
  setwd("~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/")
}

rm()
library(lme4)
library(cowplot)
library(ggplot2)
library(data.table)
library(dplyr)
# set up data file
dat <- data.table(read.csv("final_results.csv"))
dat<- subset(dat,select=-c(X))
nsubj= unique(dat$id)
ncat= unique(dat$category)
# get indices for categories that were !repeated twice
k= dat[, .N, by= .(category, game, id)]
cat_table= k[, .N, by= .(id, category)]
cat_table= cat_table[N==2]
dat= merge(dat, cat_table)
dat= dat[N== 2]
dat= subset(dat, select= -c(N))
# Drop games 23-24
ncat= unique(dat$category)
for (i in 1:length(nsubj)){
  for (j in 1:length(ncat)){
    this_game <- dat[id== nsubj[i] & category== ncat[j], game]
    dat[id== nsubj[i] & category== ncat[j], listnum:= max(game)]
  }
}
dat= subset(dat, select= -c(listnum))
# This loop looks up and gets rid of perseverative erros by setting to NaN
check4err= data.table()
for (subject in nsubj){
  for (cats in ncat){
    subj <- dat[listrank== 1 & id== subject & category== cats]
    if(any(subj[listrank== 1,.N, by= item]$N> 1)){
      for (game_id in unique(subj$game)){
        this_subj= subj[game== game_id]
        check4err <- this_subj[listrank== 1,.N, by= item]
        get_err <- this_subj[item %in% check4err[check4err$N>1]$item]
        smallest_val <- min(get_err$itemnum)
        # larger_val <- get_err[itemnum != smallest_val]
        this_subj[item %in% unique(get_err$item) & itemnum %in% smallest_val] = NaN
        dat[id== subject & category == cats & listrank== 1] <- this_subj
      }
    }
  }
}
dat[, both_trials:= 0]
for (subject in nsubj){
  for (cats in unique(dat$category)){
    this_subj <- dat[id== subject & category== cats,]
    repeated_words <- intersect(this_subj[listrank==2,item], this_subj[listrank==1,item])
    this_subj[, both_trials:= 0]
    this_subj[item %in% repeated_words, both_trials:=1]
    dat[id== subject & category == cats]$both_trials <- this_subj$both_trials
  }
}



all_transitions= data.table()
for (subject in nsubj){
  for(cats in ncat){
    this_subj= dat[id== subject & category== cats]
    thisword= unique(this_subj$item)
    for (word in 1:(length(thisword)-1)){
      if(sum(this_subj$both_trials[word], this_subj$both_trials[word+1])==2){
        all_transitions[id== subject & category== cats & item %in% item[word] <- which(thatword %in% thisword[word])]
      } else {
        all_transitions[id== subject & category== cats & item %in% item[word]] <- NaN
        print(sum(this_subj$both_trials[word], this_subj$both_trials[word+1])==2)
      }
      if(length(transition_dist[is.na(transition_dist)%in%length(transition_dist)])){
        print(sum(this_subj$both_trials[word], this_subj$both_trials[word+1])==2)
      }
    }
  }
}











for (subject in nsubj){
  for(cats in ncat){
    this_subj= dat[id== subject & category== cats]
    thisword= unique(this_subj$word)
    for (word in 2:length(thisword)-1){
      if(sum(this_subj[item %in% thisword[word]]$both_trials, this_subj[item %in% thisword[word]]$both_trials)==2){
        all_transitions[id== subject & category== cats & item== item[word] <- which(thatword %in% thisword[word])]
      } else {
        all_transitions[id== subject & category== cats & item== item[word]] <- NaN
        print(sum(this_subj$both_trials[word], this_subj$both_trials[word+1])==2)
      }
      if(length(transition_dist[is.na(transition_dist)== length(transition_dist)])){
        print(sum(this_subj$both_trials[word], this_subj$both_trials[word+1])==2)
      }
    }
  }
}

sum(is.na(all_transitions$x))
sum(dat[listrank== 2]$both_trials==0)

all_transitions

# change transition dist to sp
all_transitions <- dat
all_transitions[, sp:= NaN]
transition_dist <- vector()

s_df= data.table(id= character(), category= character(), item= character(), sp1= numeric(), sp2= numeric(), dist= numeric(), range= numeric())
for (subject in nsubj){
  for(cats in ncat){
    idx= nrow(dat[id== subject & category== cats & both_trials== 1 & listrank==2])>1 & nrow(dat[id== subject & category== cats & both_trials== 1 & listrank==2])
    if (idx== TRUE){
      l2= dat[id== subject & category== cats & both_trials== 1 & listrank== 2]
      l1= dat[id== subject & category== cats & listrank== 1]
      s_vec= vector()
      id_vec= vector()
      cat_vec= vector()
      item_vec= vector()
      sp_vec2= vector()
      sp_vec1= vector()
      for(i in 1:(nrow(l2)-1)){
        s_vec[i]= max(l1[item %in% l2[i]$item]$itemnum)- min(l1[item %in% l2[i+1]$item]$itemnum)
        id_vec[i]= subject
        cat_vec[i]= cats
        item_vec[i]= l2[i]$item
        sp_vec1[i]= min(l1[l1$item %in% l2[i]$item]$itemnum)
        sp_vec2[i]= l1[i]$itemnum
      }
      max_range= rep(max(abs(s_vec)),length(s_vec))
      newrow= list(id_vec, cat_vec, item_vec, sp_vec1, sp_vec2, s_vec, max_range)
      s_df= rbindlist(list(s_df, newrow))
    }
  }
}

### Sanity Check Calculations ###
#Forwards
s_df[dist>0, mean(dist), by= .(id, category)]
s_df[dist>0, mean(dist), by=  id]
s_df[dist>0, mean(dist), by= category]

#Backwards
s_df[dist<0, mean(dist), by= .(id, category)]
s_df[dist<0, mean(dist), by=  id]
s_df[dist<0, mean(dist), by= category]
#all about the same

#Get counts for how many forward v backward order transitions by subj
bckwd= s_df[dist<0, .N, by= .(id, category)]
fwd= s_df[dist>0, .N, by= .(id, category)]
mean(fwd$N) #3.905405
mean(bckwd$N) #5.665198



transition_range= 1:(max(this_transition$dist))
this_subj= dat[id== nsubj[1] & category== ncat[1]]
this_transition= s_df[id== nsubj[1] & category== ncat[1]]
transition_range= data.table(possible_transitions= c(min(this_transition$dist):max(this_transition$dist)), counts= 0)


# This is where I am getting the transition counts
for(i in 1:length(transition_range$possible_transitions)){
  idx= transition_range$possible_transitions[i]
  transition_range[i]$counts= sum(this_transition$dist %in% idx)
}
log_table= data.table(transition= numeric())
this_log= vector()
counter= vector()
tvec= vector()
for (i in 1:max(this_transition$sp1)){
  if(nrow(this_transition[sp1==i])>0){
    this_log[i]= this_transition[sp1== i]$sp1
    fordir= c(1:i)
    fordir[fordir%in% this_log]= NaN
    backdir= c(i:1)
    backdir[backdir%in% this_log]= NaN
    backdir[backdir %in% fordir]= NaN
    newrow= c(fordir, backdir)
    log_table= rbindlist(list(log_table, newrow))
    print(i)
  } else{
    print(this_transition$sp)
  }
}
s_df
# 
# k= s_df[id== nsubj[1] & category== ncat[1]]
# t_counts= vector()
# d= vector()
# a_counts= unique(s_df)
# for (i in 1:max(k$dist)){
#  d[i]= max(k$dist)- i+1
# }






#### Me mapping out exactly how to get the dist of possible transition values#### 
this_transition= s_df[id== nsubj[1]& category== ncat[1]]
thislog= vector()
# possible transitions
idx= this_transition[1]$sp1 #= 2
# vector= [1, 3, 4, 5, 6, ..... to 25]
k = c(1:idx, idx:(max(this_transition$sp1)))
thislog[1]= idx
k= k[!k%in% thislog]
k
length(k)

idx= this_transition[2]$sp1 #= 2
# vector= [1, 3, 4, 5, 6, ..... to 25]
k = c(1:idx, idx:(max(this_transition$sp1)))
thislog[2]= idx
k= k[!k%in% thislog]
k
length(k)

idx= this_transition[3]$sp1 #= 2
# vector= [1, 3, 4, 5, 6, ..... to 25]
k = c(1:idx, idx:(max(this_transition$sp1)))
thislog[3]= idx
k= k[!k%in% thislog]
k
length(k)

idx= this_transition[4]$sp1 #= 2
# vector= [1, 3, 4, 5, 6, ..... to 25]
k = c(1:idx, idx:(max(this_transition$sp1)))
thislog[4]= idx
k= k[!k%in% thislog]
k
length(k)

idx= this_transition[5]$sp1 #= 2
# vector= [1, 3, 4, 5, 6, ..... to 25]
k = c(1:idx, idx:(max(this_transition$sp1)))
thislog[5]= idx
k= k[!k%in% thislog]
k
length(k)



### Then trying to do it exactly in the vector### 
#New weird error is that ptr_count is now, for whatever reason, not adding rows.
#Even though it was earlier.
thislog= vector()
ptr_count= data.table(sp= numeric(), npossible= numeric())
getvals= vector()
for (i in 1:max(this_transition$sp1)){
  # print(i)
  if(nrow(this_transition[sp1 %in% i])>0){
    idx= i
    k = c(1:idx, idx:(max(this_transition$sp1)))
    thislog[i]= idx
    k= k[!k%in% thislog]
    k= k[!is.na(k)]
    print(k)
    
  }else {
    k = 0
    thislog[i]= idx
  }
  ptr_count[i]$npossible= length(k) #N possible transitions that could be made up till this point
}
ptr_count


