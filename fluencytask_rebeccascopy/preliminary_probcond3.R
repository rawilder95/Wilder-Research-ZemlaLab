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





# if you need to clear this var
# transition_probabilites <- data.table() 

transition_probabilities <- data.table(id=character(), category=character(), OldOld=numeric(), OldNew=numeric(), NewNew=numeric(), NewOld=numeric(), old= numeric(), new= numeric())
for (subject in nsubj){
  for (cats in unique(dat$category)){
    old_old <-0 
    old_new <- 0
    new_old <-0
    new_new <-0
    old <- 0
    new <-0 
    trial2 <- dat[id== subject & category== cats & listrank==2,]
    for (i in 1:(nrow(trial2)-1)){
      if(trial2[i]$both_trials== 1 & trial2[i+1]$both_trials== 1){
        old_old= old_old+1
      } else if (trial2[i]$both_trials== 0 & trial2[i+1]$both_trials== 1){
        new_old= new_old+1
        
      } else if(trial2[i]$both_trials== 1 & trial2[i+1]$both_trials== 0){
        old_new= old_new +1
      } else if(trial2[i]$both_trials== 0 & trial2[i+1]$both_trials== 0){
        new_new= new_new+1
      }
    }
    newrow <- list(id=subject, category=cats, OldOld=old_old, OldNew=old_new, NewNew=new_new, NewOld=new_old)
    transition_probabilities <- rbind(transition_probabilities, newrow, fill= TRUE)
    }
}

  


# newrow <- list(id="A101", category="fruits", pOldOld=.123, pOldNew=.5, pNewNew=.76, pNewOld=.3)
# 
# transition_probabilites <- rbind(transition_probabilities, newrow)
# pOldOld=old_old/(old_new+old_old), pOldNew=old_new/(old_new+old_old), pNewOld= new_old/(new_old + new_new), pNewNew/


# pNewNew= new_new/(new_old+new_new), pNewOld= new_old/(new_old+new_new))
#       pNewNew= new_new/(new_old+new_new), pNewOld= new_old/(new_old+new_new))

# a <- data.table(rep(0,100),rep(0,100), rep(0,100), rep(0,100))
# 
# 
# for (i in 100){
#   a[i,] <- rbind(c(sample(100,4,1)))
# }
# 
for (subject in nsubj){
  for (cats in unique(dat$category)){
    old <- 0
    new <- 0
    trial2 <- dat[id== subject & category== cats & listrank==2,]
    for (i in 1:nrow(trial2)){
      if (trial2$both_trials[i]==1){
        old= old +1
      } else {
        new = new + 1
      }
    }
    transition_probabilities[id== subject & category== cats]$old= old
    transition_probabilities[id== subject & category== cats]$new= new
  }
}

t <- transition_probabilities
# p ( new | old) = old_new / (old_new + old_old)
# p (old | old ) = old_old/ (old_new + old_old)
# p (old | new ) = new_old/ (new_old + new_new)
all_probs<- data.table()
all_probs[,old_old:= t$OldOld/(t$OldOld+t$OldNew)]
all_probs[,old_new:= t$OldNew/(t$OldOld+t$OldNew)]
all_probs[,new_old:= t$NewOld/(t$NewOld+t$NewNew)]
all_probs[,new_new:= t$NewNew/(t$NewOld+t$NewNew)]



# 
# all_probs[, old_new:= transition_probabilities$NewNew/transition_probabilities$OldNew]
# all_probs[, new_old:= transition_probabilities$OldNew/transition_probabilities$NewNew]
# all_probs[, new_new:= transition_probabilities$OldNew/transition_probabilities$NewNew]


# 
ggplot()+ geom_boxplot(aes( y= c(all_probs$old_old, all_probs$old_new, all_probs$new_old, all_probs$new_new)))

new_dat <- data.table()
new_dat[, row1:= c(all_probs$old_old,all_probs$old_new, all_probs$new_old, all_probs$new_new)]

new_dat[, dat_labs:= c(rep("p(Old|Old)", length(all_probs$old_old)), rep("p(New|Old)", length(all_probs$old_old)), rep("p(Old|New)", length(all_probs$old_old)), rep("p(New|New)", length(all_probs$old_old)))]



ggplot() + geom_boxplot(aes(x= new_dat$dat_labs, y= new_dat$row1))+ labs(x= "Retrieval Type", y= "Conditional Probability of Retrieval Type")
ggsave("conditional_probability_repeat_type.png", device= "png", dpi= 300)




resamp_probs <- data.table()
resamp_probs[, oldold:= sample(all_probs$old_old, 1000, replace= TRUE)]
resamp_probs[, oldnew:= sample(all_probs$new_old, 1000, replace= TRUE)]
resamp_probs[, newold:= sample(all_probs$old_new, 1000, replace= TRUE)]
resamp_probs[, newnew:= sample(all_probs$new_new, 1000, replace= TRUE)]
new_tab <- data.table()
new_tab[, probs:= c(resamp_probs$oldold, resamp_probs$oldnew, resamp_probs$newold, resamp_probs$newnew)]
prob_labs <- c(rep("p(Old|Old) Resample", 1000), rep("p(Old|New)", 1000), rep("p(New|Old)", 1000), rep("p(New|New)", 1000))
new_tab[,resamp_labs:= prob_labs]
new_tab <- new_tab[!is.nan(new_tab$probs)]
new_dat <- new_dat[!is.nan(new_dat$row1)]
# new_tab[!is.na(new_tab$probs),prob_labs]
ggplot() + geom_boxplot(aes(x= new_tab[resamp_labs== "p(Old|Old) Resample"]$resamp_labs, y= new_tab[resamp_labs== "p(Old|Old) Resample"]$probs*0.71), color= "blue")+ geom_boxplot(aes(y= new_dat[dat_labs== "p(Old|Old)"]$row1, x= new_dat[dat_labs== "p(Old|Old)"]$dat_labs), color= "red")+ ylim(0,1)+ labs(x= "Data Versus Resample", y= "p(Old|Old)")







mean(new_tab[resamp_labs== "p(Old|Old) Resample"]$probs)
mean(new_dat[dat_labs== "p(Old|Old)"]$row1)
ggsave("poldold.png", device= png, dpi= 300)


rpoldold <- sample(transition_probabilities$OldOld, 1000, replace= TRUE)/nrow(transition_probabilities)

rpnewnew <- sample(transition_probabilities$NewNew, 1000, replace= TRUE)/nrow(transition_probabilities)


# leave out (old|new) (new|old)
# put id and cat name in prob data table




nsubj[2]
nsubj[7]

all_probs[new_old== 0]
all_probs[new_new== 0]

all_probs[old_old== 0]
all_probs[old_new== 0]



# treat the lists as shuffled generated those probabilities do it on a shuffled list.  Shuffle the probabilities.  Recompute average over 3 distinct categories.  shuffle that 1000 times

sample(trial2[,item])

# for subject ()
# for category ()
# for 1000x sample ()


# Do people cluster old things together more than you would expect by chance 
# If you have 20 data points --> boxplot dist, average them together, single value for p(old/new|old/new) actual data points 

# Generate a density distribution for cond_probs, make an entirely resampled dataset.  Shuffle that lists and redo the old_trials and old new.  Make sure you have an entirely fabricated data set of the same size.  Participants recalled the same words as they did, but a bag of words approach.  Shuffle 1x by this point- you want 1000 fake datasets.

# Plot those as dist, how for probabilities old|old as a group, and point estimate 

# When you write it up, make sure you talk about shuffling the words.