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



nitem <- unique(dat[,item])
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



trial1 <- dat[listrank== 1,]
trial2 <- dat[listrank== 2,]
p_trial <- vector()


# Find all of the listrank 3 which represent repeated items
# Do this in one subject first
intersect(dat[listrank==1 & item== nitems[i]]$id, dat[listrank==2 & item== nitems[i]]$id)

this_subject <- dat[id== nsubj[1] & category== "Fruits",]
# second_trial <- this_subj[listrank== 2]

# listrank type 3 is going to need to be specified to subject and category.  You can manually generalize... but idk how to streamline this in R but in matlab you would use a cell array. Ask Jeff tomorrow.

# find overlap between 
this_subject[item%in% intersect(this_subject[listrank==2,item], this_subject[listrank==1, item]),]


this_subject[listrank==2, rep_before:= item %in% intersect(this_subject[listrank==2]$item, this_subject[listrank==1]$item)]


trial2 <- this_subject[listrank==2,]


# Change rep_before to FALSE
# Get all of the repeat repeat trials
old_old= 0
for (i in 1:(length(trial2$item)-1)){
  if(sum(trial2[i]$rep_before== "TRUE",trial2[i+1]$rep_before== "TRUE")>1){
      old_old= old_old+1
      }else{
      old_old=old_old+0
    }
  }

# if new|old --> only should include things following old 
# Get all of the new new trials 
# p ( new | old) = old_new / (old_new + old_old)
# p (old | old ) = old_old/ (old_new + old_old)
# p (old | new ) = new_old/ (new_old + new_new)
new_new= 0
for (i in 1:(length(trial2$item)-1)){
  if(sum(trial2[i]$rep_before== "FALSE",trial2[i+1]$rep_before== "FALSE")>1){
    new_new= new_new+1
  }else{
    new_new=new_new+0
  }
}

# Get all of the new old trials
# 2= only said on the second trial
# 3= said on the second and the first trial
new_old= 0
for (i in 1:(length(trial2$item)-1)){
  if(sum(trial2[i]$rep_before== "FALSE",trial2[i+1]$rep_before== "TRUE")>1){
    new_old= new_old+1
  }else{
    new_old=new_old+0
  }
}


old_new= 0
for (i in 1:(length(trial2$item)-1)){
  if(sum(trial2[i]$rep_before== "TRUE",trial2[i+1]$rep_before== "FALSE")>1){
    old_new= old_new+1
  }else{
    old_new=old_new+0
  }
}



# Randomly sample iterations that can be indexed by list rank.  
# set.seed(1)
(rand_trial <- sample(trial2$rep_before))



# Repeat previous loops with resampled trials
# just write a function 
rold_old= 0
for (i in 1:(length(rand_trial)-1)){
  if(sum(rand_trial[i]=="TRUE",rand_trial[i+1]=="TRUE")>1){
    rold_old= rold_old+1
  }else{
    rold_old=rold_old+0
  }
}

rnew_new=0 
for (i in 1:(length(rand_trial)-1)){
  if(sum(rand_trial[i]=="FALSE",rand_trial[i+1]=="FALSE")>1){
    rnew_new= rnew_new+1
  }else{
    rnew_new=rnew_new+0
  }
}


rold_new= 0
for (i in 1:(length(rand_trial)-1)){
  if(sum(rand_trial[i]=="TRUE",rand_trial[i+1]=="FALSE")>1){
    rold_new= rold_new+1
  }else{
    rold_new=rold_new+0
  }
}


rnew_old= 0
for (i in 1:(length(rand_trial)-1)){
  if(sum(rand_trial[i]=="FALSE",rand_trial[i+1]=="TRUE")>1){
    rnew_old= rnew_old+1
  }else{
    rnew_old=rnew_old+0
  }
}

# p ( new | old) = old_new / (old_new + old_old)
# p (old | old ) = old_old/ (old_new + old_old)
# p (old | new ) = new_old/ (new_old + new_new)

# t.test(old_old/(old_new+old_old),rold_old/(rold_new+rold_old))

# when scaling up be careful to do it one list at a time --> collapse 
# permutations create distribution 

sum(rold_old/length(this_subject$id),rnew_old/length(this_subject$id),rold_new/length(this_subject$id),rnew_new/length(this_subject$id))

# get distributions (Null Hypothesis)
counter1 <- vector()
for (i in 1:1000){
  rand_trial <- sample(trial2)
  #old_old_dist[i] <- pf(old|old)
  # new_old_dist [i] <- pf(old|new)
  # new_new_dist [i] <- pf(new|new)
  # old_new_dist [i] <- pf(new|old)
}

# sample e.g. 1000 per trial (which means subj/cat/trial) and add col that says samplei for each iteration ?  Turn it into a function and pass it through 
for (i in 1:1000){
  
}

# Look at resample dist and probability of old-old in Null resampled dist
#Box plot graph: two bars for each combinations --> bar would be mean, confidence intervals for range of vals 
# For plotting, one data point that collapses across everything.  You cana also do it within subject, where you just collapse across category. 



# dat[item %in% intersect(dat[listrank==2,item], dat[listrank==1, item])]$listrank <- 3

