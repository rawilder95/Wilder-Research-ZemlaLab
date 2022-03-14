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

t1 <- unique(dat[listrank== 1,item])
t2 <- unique(dat[listrank== 2, item])
t3 <- intersect(dat[listrank== 1 & listrank== 2, item], nitem)

t2 <- vector()
for (i in 1:length(nitems)){
  check_foroverlap <- intersect(dat[listrank==1 & item== nitem[i]]$id, dat[listrank==2 & item== nitem[i]]$id)
  t2[i] <- sum(dat[listrank== 2 & item== nitem[i], .N, by= id]$N)-1
}



pt2 <- vector()
pt2pt1 <- vector()
for (i in 1:length(nitems)){
  check_foroverlap <- intersect(dat[listrank==1 & item== nitems[i]]$id, dat[listrank==2 & item== nitems[i]]$id)
  pt2[i] <- sum(dat[listrank== 2 & item== nitems[i], .N, by= id]$N)-1
  if (length(check_foroverlap)>0){
    pt2pt1[i] <- length(check_foroverlap)/sum(dat[listrank== 1 & item== nitems[i], .N, by= id]$N)
  }else{
    pt2pt1[i] <- 0
  }
}


dat_reps <- data.table()

dat_reps[, pt2 := pt2/length(nsubj)]
dat_reps[, pt2pt1 := pt2pt1]

# 
# ggplot() + geom_count(aes(x= dat_reps$pt2, y= dat_reps$pt2pt1)) + geom_abline(slope= 1, intercept= 0)+ xlim(0,1)


t1
t2

trial1 <- dat[listrank== 1,]
trial2 <- dat[listrank== 2,]
p_trial <- vector()

# for (i in 1:length(dat[subj==nsubj[1],id])){
#   if (intersect(dat[listrank==1 & item== nitems[i]]$id, dat[listrank==2 & item== nitems[i]]$id))
# }



# Find all of the listrank 3 which represent repeated items
# Do this in one subject first
intersect(dat[listrank==1 & item== nitems[i]]$id, dat[listrank==2 & item== nitems[i]]$id)

this_subject <- dat[id== nsubj[1] & category== "Fruits",]
second_trial <- this_subj[listrank== 2]

# listrank type 3 is going to need to be specified to subject and category.  You can manually generalize... but idk how to streamline this in R but in matlab you would use a cell array. Ask Jeff tomorrow.
this_subject[item%in% intersect(this_subject[listrank==2,item], this_subject[listrank==1, item])]$listrank <- 3 
counter= 0 #set counter to 0
for (i in 1:(length(this_subject$item)-1)){
  if (this_subject[i]$listrank==3 & this_subject[i+1]$listrank==3){
    counter= counter+1
  } else {
    counter= counter+0
  }
}


# Get all of the repeat repeat trials
old_old= 0
trial2 <- this_subject[listrank> 1,]
for (i in 1:(length(this_subject$item)-1)){
  if(sum(this_subject[i]$listrank==3,this_subject[i+1]$listrank==3)>1){
    
      old_old= old_old+1
      }else{
      old_old=old_old+0
    }
  }


# Get all of the new new trials 
new_new= 0
for (i in 1:(length(this_subject$item)-1)){
  if(sum(this_subject[i]$listrank==2,this_subject[i+1]$listrank==2)>1){
    if(this_subject[i+1]$listrank==2){
      new_new= new_new+1
    } else{
      new_new=new_new+0
    }
  }
}

# Get all of the new old trials
# 2= only said on the second trial
# 3= said on the second and the first trial
new_old= 0
for (i in 1:(length(this_subject$item)-1)){
  if(sum(this_subject[i]$listrank==2,this_subject[i+1]$listrank==3)>1){
      new_old= new_old+1
    } else{
      new_old=new_old+0
    }
  }


old_new= 0
for (i in 1:(length(this_subject$item)-1)){
  if(sum(this_subject[i]$listrank==3,this_subject[i+1]$listrank==2)>1){
    old_new= old_new+1
  } else{
    old_new=old_new+0
  }
}

# dat[item %in% intersect(dat[listrank==2,item], dat[listrank==1, item])]$listrank <- 3
set.seed(1)
(rand_trial <- sample(this_subject$listrank))


rold_old= 0
for (i in 1:(length(rand_trial)-1)){
  if(sum(rand_trial[i]==3,rand_trial[i+1]==3)>1){
    rold_old= rold_old+1
  }else{
    rold_old=rold_old+0
  }
}

rnew_new=0 
for (i in 1:(length(rand_trial)-1)){
  if(sum(rand_trial[i]==2,rand_trial[i+1]==2)>1){
    rnew_new= rnew_new+1
  }else{
    rnew_new=rnew_new+0
  }
}


rold_new= 0
for (i in 1:(length(rand_trial)-1)){
  if(sum(rand_trial[i]==2,rand_trial[i+1]==2)>1){
    rnew_new= rnew_new+1
  }else{
    rnew_new=rnew_new+0
  }
}

