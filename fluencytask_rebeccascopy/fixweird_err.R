if (getwd()!= "~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/logs"){
  setwd("~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/")
}

# ##Relevant Libraries & Packages###
# install.packages("data.table")
library(data.table)
# install.packages("googlesheets4")
library(googlesheets4)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("lme4")
library(lme4)


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
# Drop games 23-24
ncat= unique(dat$category)
for (i in 1:length(nsubj)){
  for (j in 1:length(ncat)){
    this_game <- dat[id== nsubj[i] & category== ncat[j], game]
    dat[id== nsubj[i] & category== ncat[j], listnum:= max(game)]
  }
}
dat[listnum== "FALSE", listrank:= 1]
dat[listnum == "TRUE", listrank:= 2]
dat[, both_trials := 0]
dat<- subset(dat, select=-c(listnum))
# get items that were listed in both trials 
for (i in 1:length(nsubj)){
  for (cats in ncat){
    this_subj <- dat[id==nsubj[i] & category== cats,]
    repeated_words <- intersect(this_subj[listrank==2,item], this_subj[listrank==1,item])
    this_subj[, both_trials:= 0]
    this_subj[item %in% repeated_words, both_trials:=1]
    dat[id==nsubj[i] & category== cats]$both_trials <- this_subj$both_trials
  }
}
dat[, temp_int:= 0]
dat[, temp_int:= ((max(game)-min(game))-1), by= .(id, category)]
# Spotcheck
dat[id== "Sy2yTzrhP6e" & category== "Vegetables"]
mod= glmer(data= dat, both_trials~ temp_int + (1|id)+ (1|category), family= "binomial")


typicality= vector()
pba= vector()
all_cats= data.table(item= character(), category= character(), typicality= numeric(), pba= numeric())
dat[, typicality:= NaN]
dat[, pba:= NaN]
### Rep v typicality ###
for (cats in ncat){
  this_cat= dat[category== cats]
  words= unique(this_cat[listrank== 2, item])
  for ( i in 1:length(words)){
    check_foroverlap <- intersect(this_cat[listrank==1 & item == words[i]]$id, this_cat[listrank==2 & item == words[i]]$id)
    typicality[i]= (sum(this_cat[listrank==2 & item == words[i], .N, by= id]$N)-1)/length(nsubj)
    if (length(check_foroverlap)>0){
      pba[i] = length(check_foroverlap)/sum(this_cat[listrank== 1 & item == words[i], .N, by = id]$N)
      dat[category== cats & item== words[i]]$typicality= typicality[i]
      dat[category== cats & item== words[i]]$pba= pba[i]
    } else{
      pba[i]= 0
    }
  }
  # newlist= list(words, pba, typicality, rep(cats,length(typicality)))
  # all_cats= rbind(all_cats, list(rep(words[i], length(typicality)), rep(cats, length(typicality)), typicality, pba))
}

### Rep V Typicality + Temporal Interval ###
ggplot() + geom_count(data= dat, aes(x= typicality, y= pba, alpha= 0.5)) + geom_abline(intercept= 0, slope= 1)+ ylim(0,1) + xlim(0,1)
mod= glmer(data= dat, both_trials~ temp_int + typicality + pba + (1|id)+ (1|category), family= "binomial")
summary(mod)

### Rep V Typicality without temporal interval
mod2= glmer(data= dat, both_trials ~ typicality + pba + (1|id)+ (1|category), family= "binomial", index= c("id", "category"))
summary(mod2)

mod2fit= fitted.values(mod2)
dat2= dat[!is.na(typicality),]
dat2[,mod2:= mod2fit]
dat2[, repeated:= both_trials==1]


# Plot fitted values of model 2 (both_trials ~ rep + typicality)
ggplot() + geom_jitter(aes(x= dat2$both_trials, y= dat2$mod2), alpha= 0.15) + geom_violin(aes(x= dat2[repeated== "FALSE"]$repeated, dat2[repeated== FALSE]$mod2), alpha= 0.5)

##### Prob Repeat-Repeat ####
for (subject in nsubj){
  for (cats in ncat) {
    oldold= 0
    oldnew= 0
    newold= 0
    newnew= 0
  }
}
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
    for(i in 1:(nrow(trial2)-1)){
      # print(c(i, i+1, nrow(trial2))) make sure no weird indexing issue
      if(trial2[i,both_trials]==1){
        old= old+1
        new= new+0
      } else{
        new= new+1
        old= old+0
      }
    }
    # for (i in 1:(nrow(trial2)-1)){
    #   if(trial2[i]$both_trials== 1 & trial2[i+1]$both_trials== 1){
    #     old_old= old_old+1
    #   } else if (trial2[i]$both_trials== 0 & trial2[i+1]$both_trials== 1){
    #     new_old= new_old+1
    #   } else if(trial2[i]$both_trials== 1 & trial2[i+1]$both_trials== 0){
    #     old_new= old_new +1
    #   } else if(trial2[i]$both_trials== 0 & trial2[i+1]$both_trials== 0){
    #     new_new= new_new+1
    #   }
    # }
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

transition_probabilities <- data.table(id=character(), category=character(), OldOld=numeric(), OldNew=numeric(), NewNew=numeric(), NewOld=numeric(), old= numeric(), new= numeric())
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
for (subject in nsubj){
  for (cats in ncat){
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


transition_probabilities$OldOld/(transition_probabilities$OldOld+ transition_probabilities$OldNew)

###### CODE THAT WASNT USED BUT MAY BE GOOD TO REFERENCE LATER ######
# ### CONDITIONAL PROBABILITY (REPEAT|REPEAT)####
# transitions = vector()
# 
# transition_table= data.table(id= character(), category= character(), oldold= numeric(), oldnew= numeric(), newold= numeric(), newnew= numeric(), old= numeric(), new= numeric())
# # Loop through all transitions per trial
# for (subject in nsubj){
#   for (cats in ncat){
#     this_subj= dat[id== subject & category== cats]
#     this_subj= this_subj[listrank==2]
#     # Check to make sure that trial is not empty
#     if(nrow(this_subj)>1){
#       transitions= vector()
#       oldold= vector()
#       oldnew= vector()
#       newold= vector()
#       newnew= vector()
#       # changed these names because i'm lazy and don't like pressing shift key
#       olditem= vector()
#       newitem= vector()
#       for(i in 1:(nrow(this_subj)-1)){
#         if(this_subj[i,both_trials]==1 & this_subj[i+1,both_trials]==1){
#           oldold[i]= 1
#           oldnew[i]= 0
#           newold[i]= 0
#           newnew[i]= 0
#           olditem[i]= 1
#           newitem[i]= 0
#         } else if(this_subj[i,both_trials]==1 & this_subj[i+1,both_trials]==0){
#           oldold[i]= 0
#           oldnew[i]= 1
#           newold[i]= 0
#           newnew[i]= 0
#           olditem[i]= 1
#           newitem[i]= 0
#         } else if(this_subj[i,both_trials]==0 & this_subj[i+1,both_trials]==1){
#           oldold[i]= 0
#           oldnew[i]= 0
#           newold[i]= 1
#           newnew[i]= 0
#           olditem[i]= 0
#           newitem[i]= 1
#         } else if(this_subj[i,both_trials]==0 & this_subj[i+1,both_trials]==0){
#           oldold[i]= 0
#           oldnew[i]= 0
#           newold[i]= 0
#           newnew[i]= 1
#           olditem[i]= 0
#           newitem[i]= 1
#         } else{
#       oldold[i]= NaN
#       oldnew[i]= NaN
#       newold[i]= NaN
#       newnew[i]= NaN
#       olditem[i]= NaN
#       newitem[i]= NaN
#     }
#       }
#     }
#     newrow= list(this_subj[1:length(oldold)]$id,this_subj[1:(length(oldold))]$category, oldold, oldnew, newold, newnew, olditem, newitem)
#     transition_table= rbind(transition_table, newrow)
#   }
# }
# 
# 
# # Clear out all of the NaNs from things that were not recalled 
# transition_table= transition_table[!is.na(transition_table$oldold) & !is.na(category),]
# 
# p_oldold= transition_table[, sum(oldold)/(sum(oldold)+sum(oldnew))]
# p_oldnew= transition_table[, sum(oldnew)/(sum(oldold)+sum(oldnew))]
# p_newold= transition_table[, sum(newold)/(sum(newnew)+sum(newold))]
# p_newnew= transition_table[, sum(newnew)/(sum(newnew)+sum(newold))]
# 
# # # Get Bootstrapped values 
# # btransition_probabilities= data.table(id= character(), category= character(), oldold= numeric(), oldnew= numeric(), newold= numeric(), newnew= numeric(), old= numeric(), new= numeric(), p_oldold= numeric(), p_oldnew= numeric(), p_newold= numeric(), p_newnew= numeric())
# # for(sim in 500000){
# #   for (subject in nsubj){
# #     for (cats in ncat){
# #       this_subj= dat[id== subject & category== cats & listrank== 2]
# #       this_subj$both_trials = sample(this_subj$both_trials, replace= FALSE)
# #       # Check to make sure that trial is not empty
# #       if(nrow(this_subj)>1){
# #         transitions= vector()
# #         oldold= 0
# #         oldnew= 0
# #         newold= 0
# #         newnew= 0
# #         # changed these names because i'm lazy and don't like pressing shift key
# #         olditem= 0
# #         newitem= 0
# #         for(i in 1:(nrow(this_subj)-1)){
# #           if(this_subj[i,both_trials]==1 & this_subj[i+1,both_trials]==1){
# #             oldold= oldold+ 1
# #             oldnew= oldnew+ 0
# #             newold= newold + 0
# #             newnew= newnew+ 0
# #             olditem= olditem+ 1
# #             newitem= newitem+ 0
# #           } else if(this_subj[i,both_trials]==1 & this_subj[i+1,both_trials]==0){
# #             oldold= oldold + 0
# #             oldnew= oldnew+ 1
# #             newold= newold+ 0
# #             newnew= newnew+ 0
# #             olditem= olditem+ 1
# #             newitem= newitem+ 0
# #           } else if(this_subj[i,both_trials]==0 & this_subj[i+1,both_trials]==1){
# #             oldold= oldold+ 0
# #             oldnew= oldnew+ 0
# #             newold= newold+ 1
# #             newnew= newnew+ 0
# #             olditem= old+ 0
# #             newitem= new+ 1
# #           } else if(this_subj[i,both_trials]==0 & this_subj[i+1,both_trials]==0){
# #             oldold= oldold+ 0
# #             oldnew= oldnew+ 0
# #             newold= newold+ 0
# #             newnew= newnew+ 1
# #             olditem= olditem+ 0
# #             newitem= newitem+ 1
# #         }
# #       }
# #       newrow= list(subject, cats, oldold, oldnew, newold, newnew, olditem, newitem, sum(oldold)/sum(oldold, oldnew), sum(oldnew)/sum(oldold,oldnew), sum(newold)/sum(newold, newnew), sum(newnew)/sum(newold, newnew))
# #       btransition_probabilities= rbind(btransition_probabilities, newrow)
# #     }
# #   }
# #   }
# #   }
# 

