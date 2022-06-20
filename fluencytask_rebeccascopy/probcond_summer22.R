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
# install.packages("lme4")
# library(lme4)

# library(lme4)
# library(cowplot)
# library(ggplot2)
# library(data.table)

if (getwd()!= "~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/logs"){
setwd("~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/")}

# # ##Relevant Libraries & Packages###
# # install.packages("data.table")
# # library(data.table)
# # install.packages("googlesheets4")
# # library(googlesheets4)
# # install.packages("ggplot2")
# # library(ggplot2)
# # install.packages("lme4")
# # library(lme4)
# 
# library(lme4)
# library(cowplot)
# library(ggplot2)
# library(data.table)

# 
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

# 
bt= data.table()
btransition_probabilities= data.table(id= character(), category= character(), iteration= numeric(), OldOld= numeric(), OldNew= numeric(), NewOld= numeric(), NewNew= numeric(), Old= numeric(), New= numeric())
for (bts in 1:50){
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
      if(nrow(trial2)>1){
        for (i in 1:(nrow(trial2)-1)){
          if(trial2[i]$both_trials== 1 & trial2[i+1]$both_trials== 1){
            bold_old= bold_old+1
            bold= bold+1
          } else if (trial2[i]$both_trials== 0 & trial2[i+1]$both_trials== 1){
            bnew_old= bnew_old+1
            bnew= bnew+1
          } else if(trial2[i]$both_trials== 1 & trial2[i+1]$both_trials== 0){
            bold_new= bold_new +1
            bold= bold+1
          } else if(trial2[i]$both_trials== 0 & trial2[i+1]$both_trials== 0){
            bnew_new= bnew_new+1
            bnew= bnew+1
          }
        }
        # # newrow[,id:= subject]
        # # newrow[, category:= cats]
        # # newrow[, iteration:= bts]
        # # newrow[, OldOld:= bold_old]
        # # newrow[, OldNew:= bold_new]
        # # newrow[, NewOld:= bnew_old]
        # # newrow[, NewNew:= bnew_new]
        # # l = list(btransition_probabilities, newrow)
        # btransition_probabilities <- rbind(list(btransition_probabilities, newrow))
        # # btransition_probabilities <- rbind(btransition_probabilities, newrow)
      }
    }
    newrow <- list(id= subject, category= cats, iteration= bts, OldOld= bold_old, OldNew= bold_new, NewOld= bnew_old, NewNew= bnew_new, Old= bold, New= bnew)
    btransition_probabilities= rbindlist(list(btransition_probabilities, newrow))
  }
}





bt <- btransition_probabilities[!is.na(btransition_probabilities$oldold) & !is.na(category),]

bt[, pOldOld := (OldOld/(OldOld+OldNew))]
bt[, pOldNew := (OldNew/(OldOld+OldNew))]
bt[, pNewOld := (NewOld/(NewOld+NewNew))]
bt[, pNewNew := (NewNew/(NewOld+NewNew))]

# btransition_probabilities= btransition_probabilities[!is.na(btransition_probabilities$oldold) & !is.na(category),]
# New Data Table: Coerce cols into single column for vals and one for labels.
####Harder and dumber way to do what I just did in the for loop####
btransition_probabilities[, p_oldold:= sum(OldOld)/sum(OldOld,OldNew)]
btransition_probabilities[, p_oldnew:= sum(OldNew)/sum(OldOld,OldNew)]
btransition_probabilities[, p_newold:= sum(NewOld)/sum(NewOld,NewNew)]
btransition_probabilities[, p_newnew:= sum(NewNew)/sum(NewOld,NewNew)]
btransition_probabilities[, p_old:= sum(Old)/(sum(Old,New))]
btransition_probabilities[, p_new:= sum(New)/(sum(Old,New))]


bnewdat= data.table()
# Coerce into two cols for labs and vals
bnewdat[, bt_labs:= c(rep("p(Old|Old)", length(btransition_probabilities$oldold)), rep("p(Old|New)", length(btransition_probabilities$oldnew)), rep("p(New|Old)", length(btransition_probabilities$newold)), rep("p(New|New)", length(btransition_probabilities$newnew)))]
bnewdat[, bt_probs:= c(btransition_probabilities$p_oldold, btransition_probabilities$p_oldnew, btransition_probabilities$p_newold, btransition_probabilities$p_newnew)]

# bnewdat[, simvals:= c(btransition_probabilities$oldold, btransition_probabilities$oldnew, btransition_probabilities$newold, btransition_probabilities$newnew)]

ggplot()+ geom_boxplot(aes(x= bnewdat$bt_labs, y= bnewdat$bt_probs))+ labs(x= "Transition Type", y= "Probability of Transition Type")
ggsave("probcond_boxplots_summer22.png", device= "png", dpi = 300)

p1= ggplot()+ geom_density(aes(y= btransition_probabilities$oldold), fill= "white") + geom_density(aes(x= 1, y= p_oldold))



p1 <- ggplot() + geom_density(aes(x= bt_vals$oldold, fill= "Bootstrapped Data"), fill= "white") + geom_vline(aes(xintercept= mean(new_dat[dat_labs== "p(Old|Old)"]$row1), color= "Subject Average"))+ xlim(0,1)+ labs(x= "p(B|A)", y= "Density", title= "Probability of Repeated Item Given a Repeated Item")



p1= ggplot() + geom_density(aes(x= bnewdat[bt_labs== "p(Old|Old)"]$bt_probs, fill= "Bootstrapped Data"), fill= "white") + geom_vline(aes(xintercept= p_oldold, color= "Observed Probability"))+ xlim(0,1)+ labs(x= "Transition Probability", y= "Density", title= "p(Repeat|Repeat)")
p2= ggplot() + geom_density(aes(x= bnewdat[bt_labs== "p(Old|New)"]$bt_probs, fill= "Bootstrapped Data"), fill= "white") + geom_vline(aes(xintercept= p_oldnew, color= "Observed Probability"))+ xlim(0,1)+ labs(x= "Transition Probability", y= "Density", title= "p(Repeat|New)")
p3= ggplot() + geom_density(aes(x= bnewdat[bt_labs== "p(New|Old)"]$bt_probs, fill= "Bootstrapped Data"), fill= "white") + geom_vline(aes(xintercept= p_newold, color= "Observed Probability"))+ xlim(0,1)+ labs(x= "Transition Probability", y= "Density", title= "p(New|Repeat)")
p4= ggplot() + geom_density(aes(x= bnewdat[bt_labs== "p(New|New)"]$bt_probs, fill= "Bootstrapped Data"), fill= "white") + geom_vline(aes(xintercept= p_newnew, color= "Observed Probability"))+ xlim(0,1)+ labs(x= "Transition Probability", y= "Density", title= "p(New|New)")
cowplot::plot_grid(p1,p2,p3,p4)







# install.packages("forcats")
# library(forcats)

thislist= data.table(r1= numeric())
for (i in 1:10){
  thisnum= list(sample(100, 100, replace= TRUE))
  thislist= rbindlist(list(thislist, thisnum), use.names = FALSE, fill= FALSE, idcol= FALSE)
}

