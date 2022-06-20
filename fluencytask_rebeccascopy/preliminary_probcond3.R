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
# install.packages("cowplot")
# library(cowplot)




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

  
# when taking averages collapse across cat first --> then subject 

# when taking averages of bootstrapped data collapse across cat first --> subject--> not iteration 

# transition_probabilities[mean(oldold), by = .(iter, subject)]
transition_probabilities[mean(OldOld)]

# for data just do mean(df[mean(oldold), by= .(subject)]) should have 100 (or however many iterations) data points


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

tp<- transition_probabilities




# p ( new | old) = old_new / (old_new + old_old)
# p (old | old ) = old_old/ (old_new + old_old)
# p (old | new ) = new_old/ (new_old + new_new)
all_probs<- data.table()
all_probs[,old_old:= tp$OldOld/(tp$OldOld+tp$OldNew)]
all_probs[,old_new:= tp$OldNew/(tp$OldOld+tp$OldNew)]
all_probs[,new_old:= tp$NewOld/(tp$NewOld+tp$NewNew)]
all_probs[,new_new:= tp$NewNew/(tp$NewOld+tp$NewNew)]

tp[, pOldOld:= OldOld/(OldOld+OldNew)]
tp[, pOldNew := (OldNew/(OldOld+OldNew))]
tp[, pNewOld := (NewOld/(NewOld+NewNew))]
tp[, pNewNew := (NewNew/(NewOld+NewNew))]
tp= tp[!is.na(oldold)]


# 
# all_probs[, old_new:= transition_probabilities$NewNew/transition_probabilities$OldNew]
# all_probs[, new_old:= transition_probabilities$OldNew/transition_probabilities$NewNew]
# all_probs[, new_new:= transition_probabilities$OldNew/transition_probabilities$NewNew]


# 

new_dat <- data.table()
new_dat[, row1:= c(all_probs$old_old,all_probs$old_new, all_probs$new_old, all_probs$new_new)]

new_dat[, dat_labs:= c(rep("p(Old|Old)", length(all_probs$old_old)), rep("p(New|Old)", length(all_probs$old_old)), rep("p(Old|New)", length(all_probs$old_old)), rep("p(New|New)", length(all_probs$old_old)))]

# Bootstrapped Data 



btransition_probabilities= data.table(id= character(), category= character(), iteration= numeric(), OldOld= numeric(), OldNew= numeric(), NewOld= numeric(), NewNew= numeric())
  for (bts in 1:1000){
    for (subject in nsubj){
      for (cats in unique(dat$category)){
        bold_old <-0 
        bold_new <- 0
        bnew_old <-0
        bnew_new <-0
        bold <- 0
        bnew <-0 
        trial2 <- dat[id== subject & category== cats & listrank==2,]
        trial2$both_trials <- sample(trial2$both_trials)
        for (i in 1:(nrow(trial2)-1)){
          if(trial2[i]$both_trials== 1 & trial2[i+1]$both_trials== 1){
            bold_old= bold_old+1
          } else if (trial2[i]$both_trials== 0 & trial2[i+1]$both_trials== 1){
            bnew_old= bnew_old+1
          } else if(trial2[i]$both_trials== 1 & trial2[i+1]$both_trials== 0){
            bold_new= bold_new +1
          } else if(trial2[i]$both_trials== 0 & trial2[i+1]$both_trials== 0){
            bnew_new= bnew_new+1
          }
        }
        newrow <- data.table()
        newrow[,id:= subject]
        newrow[, category:= cats]
        newrow[, iteration:= bts]
        newrow[, OldOld:= bold_old]
        newrow[, OldNew:= bold_new]
        newrow[, NewOld:= bnew_old]
        newrow[, NewNew:= bnew_new]
        l = list(btransition_probabilities, newrow)
        btransition_probabilities <- rbind(btransition_probabilities, list(subject, cats, bts, newrow$OldOld, newrow$OldNew, newrow$NewOld, newrow$NewNew))
        # btransition_probabilities <- rbind(btransition_probabilities, newrow)
      }
    }
    # print(bts)
  }





bt <- btransition_probabilities
# newrow <- data.table()
# newrow[,id:= subject]
# newrow[, category:= cats]
# newrow[, iteration:= bts]
# newrow[, oldold:= bold_old]
# newrow[, oldnew:= bold_new]
# newrow[, newold:= bnew_old]
# newrow[, newnew:= bnew_new]


# l = list(bt, newrow)
# rbindlist(l)

bt[, pOldOld := (OldOld/(OldOld+OldNew))]
bt[, pOldNew := (OldNew/(OldOld+OldNew))]
bt[, pNewOld := (NewOld/(NewOld+NewNew))]
bt[, pNewNew := (NewNew/(NewOld+NewNew))]


bnewdat= data.table()

bnewdat[, bt_labs:= c(rep("p(Old|Old)", length(bt$OldOld)), rep("p(New|Old)", length(bt$OldNew)), rep("p(Old|New)", length(bt$NewOld)), rep("p(New|New)", length(bt$NewNew)))]


# mean(new_dat[dat_labs== "p(Old|Old)"]$row1)
# mean(new_dat[dat_labs== "p(Old|New)"]$row1)
# mean(new_dat[dat_labs== "p(New|Old)"]$row1)
# mean(new_dat[dat_labs== "p(New|New)"]$row1)

bt <- bt[!is.nan(pOldOld) & !is.nan(pNewOld) & !is.nan(pNewNew) & !is.nan(pOldNew)]

avg_oldold <- bt[, mean(pOldOld), by= iteration]
avg_oldnew <- bt[, mean(pOldNew), by= iteration]
avg_newold <- bt[, mean(pNewOld), by= iteration]
avg_newnew <- bt[, mean(pNewNew), by= iteration]

bt_vals <- data.table()

bt_vals[,oldold:= avg_oldold$V1]
bt_vals[,newold:= avg_newold$V1]
bt_vals[,oldnew:= avg_oldnew$V1]
bt_vals[,newnew:= avg_newnew$V1]

quantile(bt_vals$oldold)
quantile(bt_vals$oldnew)
quantile(bt_vals$newold)
quantile(bt_vals$newnew)

new_dat= new_dat[!is.na(row1)]

p1 <- ggplot() + geom_density(aes(x= bt_vals$oldold, fill= "Bootstrapped Data"), fill= "white") + geom_vline(aes(xintercept= mean(new_dat[dat_labs== "p(Old|Old)"]$row1), color= "Subject Average"))+ xlim(0,1)+ labs(x= "p(B|A)", y= "Density", title= "Probability of Repeated Item Given a Repeated Item")

p2 <- ggplot() + geom_density(aes(x= bt_vals$newold, fill= "Bootstrapped Data"), fill= "white") + geom_vline(aes(xintercept= mean(new_dat[dat_labs== "p(Old|New)"]$row1), color= "Subject Average"))+ xlim(0,1)+ labs(x= "p(B|A)", y= "Density", title= "Probability of Repeated Item Given New Item")




p3 <- ggplot() + geom_density(aes(x= bt_vals$oldnew, fill= "Bootstrapped Data"), fill= "white") + geom_vline(aes(xintercept= mean(new_dat[dat_labs== "p(New|Old)"]$row1), color= "Subject Average"))+ xlim(0,1)+ labs(x= "p(B|A)", y= "Density", title= "Probability of New Item Given Repeated Item", colour= "Simulated Distribution")
p4 <- ggplot() + geom_density(aes(x= bt_vals$newnew, fill= "Bootstrapped Data", color= "Simulated Permutation"), fill= "white", color= "black") + geom_vline(aes(xintercept= mean(new_dat[dat_labs== "p(New|New)"]$row1), color= "Subject Average"))+ xlim(0,1)+ labs(x= "p(B|A)", y= "Density", title= "Probability of New Item Given New Item", colour= "Simulated Distribution")

cowplot::plot_grid(p1, p2, p3, p4)
ggsave("ConditionalProbabilities_04112022.png", device = "png", dpi= 300)




# p1 <- ggplot() + geom_density(aes(x= rep("p(Old|Old)", y= 100))+ geom_vline(aes(xintercept= mean(new_dat[dat_labs== "p(Old|Old)"]$row1), color= "Subject Average"))+ labs(x= "Retrieval Type", y= "p(A|B)", title= "p(Old|Old)")
# 
# p2 <- ggplot() + geom_density(aes(x= bnew_dat[bt_labs== "p(Old|New)"]$bt_vals))+ geom_vline(aes(xintercept= mean(new_dat[dat_labs== "p(Old|New)"]$row1), color= "Subject Average"))+ labs(x= "Retrieval Type", y= "p(A|B)", title= "p(Old|New)")
# 
# p3 <- ggplot() + geom_density(aes(x= bnew_dat[bt_labs== "p(New|Old)"]$bt_vals))+ geom_vline(aes(xintercept= mean(new_dat[dat_labs== "p(New|Old)"]$row1), color= "Subject Average"))+ labs(x= "Retrieval Type", y= "p(A|B)", title= "p(New|Old)")
# 
# p4 <- ggplot() + geom_density(aes(x= bnew_dat[bt_labs== "p(New|New)"]$bt_vals))+ geom_vline(aes(xintercept= mean(new_dat[dat_labs== "p(New|New)"]$row1), color= "Subject Average"))+ labs(x= "Retrieval Type", y= "p(A|B)", title= "p(New|New)")
# 
# 
# 
# 
# cowplot::plot_grid(p1,p2,p3, p4)





# leave out (old|new) (new|old)
# put id and cat name in prob data table







# treat the lists as shuffled generated those probabilities do it on a shuffled list.  Shuffle the probabilities.  Recompute average over 3 distinct categories.  shuffle that 1000 times


# for subject ()
# for category ()
# for 1000x sample ()

# rbindlist instead of rbind

# rbindlist(btransition_probabilities, list(newrow), use.names= TRUE)
# Do people cluster old things together more than you would expect by chance 
# If you have 20 data points --> boxplot dist, average them together, single value for p(old/new|old/new) actual data points 

# Generate a density distribution for cond_probs, make an entirely resampled dataset.  Shuffle that lists and redo the old_trials and old new.  Make sure you have an entirely fabricated data set of the same size.  Participants recalled the same words as they did, but a bag of words approach.  Shuffle 1x by this point- you want 1000 fake datasets.

# Plot those as dist, how for probabilities old|old as a group, and point estimate 

# When you write it up, make sure you talk about shuffling the words.