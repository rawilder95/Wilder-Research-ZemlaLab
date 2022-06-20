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


############### REPETITION SINGLE CATEGORY TRIAL #################
# cats= ncat[1]
# 
# this_cat= dat[category== cats]




# words= unique(this_cat[listrank== 2, item])
sf2 <- vector()
both_trials <- vector()
all_cats= data.table(typicality= numeric(), pba= numeric())
# all_cats= data.table(item= character(), pba= numeric(), typicality= numeric(), category= character())

# this works DO NOT TOUCH IT. Do NOT try to do anything with aesthetics for now, you absolute gaffe machine.
# now I have to leave reminders bc you did it again. stop.
for (cats in ncat){
  this_cat= dat[category== cats]
  words= unique(this_cat[listrank== 2, item])
  for ( i in 1:length(words)){
    check_foroverlap <- intersect(this_cat[listrank==1 & item == words[i]]$id, this_cat[listrank==2 & item == words[i]]$id)
    sf2[i]= (sum(this_cat[listrank==2 & item == words[i], .N, by= id]$N)-1)/length(nsubj)
    if (length(check_foroverlap)>0){
      both_trials[i] = length(check_foroverlap)/sum(this_cat[listrank== 1 & item == words[i], .N, by = id]$N)
    } else{
      both_trials[i]= 0
    }
  }
newlist= list(words, both_trials, sf2, rep(cats,length(sf2)))
all_cats= rbind(all_cats, list(sf2, both_trials))
}

ggplot() + geom_count(data= all_cats, aes(x= all_cats$typicality, y= all_cats$pba, alpha= 0.5)) + geom_abline(intercept= 0, slope= 1)+ ylim(0,1) + xlim(0,1)




ggsave("repvtypicality_summer22.png", device= "png", dpi= 300)

#################### Temporal Interval ######################


dat[, temp_int:=0]
cat_spacing= vector()
for (subject in 1:length(nsubj)){
  for (cats in 1:length(ncat)){
    this_subj= dat[id== nsubj[subject] & category==ncat[cats]]
    if(length(this_subj$game)>0){
      cat_spacing= max(this_subj$game)- min(this_subj$game)-1
      dat[id== nsubj[subject] & category== ncat[cats]]$temp_int= cat_spacing
    }
  }
}

tmp_spc= dat[, max(game)- min(game), by= .(id, category)]



temp_int= data.table(id= character(), category= character(), temp_spc= numeric())
for (cats in ncat){
  print(temp_spc[category== cats])
}

tmp_spc= dat[, max(game)- min(game), by= .(id, category)]

mean(dat[temp_int==2]$both_trials)
mean(dat[temp_int==4]$both_trials)
mean(dat[temp_int==6]$both_trials)


# Get rid of the one 4
odd_subj= dat[id== unique(tmp_spc[V1== 4]$id) & category== unique(tmp_spc[V1== 4]$category)]
odd_subj[,]= NaN
dat[id== unique(tmp_spc[V1== 4]$id) & category== unique(tmp_spc[V1== 4]$category)]= odd_subj$game
# Get rid of the one 6
odd_subj= dat[id== unique(tmp_spc[V1== 6]$id) & category== unique(tmp_spc[V1== 6]$category)]
odd_subj[,]= NaN
dat[id== unique(tmp_spc[V1== 6]$id) & category== unique(tmp_spc[V1== 6]$category)]= odd_subj$game
dat[id== unique(tmp_spc[V1== 6]$id) & category== unique(tmp_spc[V1== 6]$category)]
tmp_spc= dat[, max(game)- min(game), by= .(id, category)]
unique(tmp_spc$V1)



dat[, temp_int:= (max(game)- min(game))-1, by= .(id, category)]
mod= glmer(data= dat, both_trials~ temp_int +(1|id) + (1|category), family= "binomial")
### No uneven temporal intervals now! ####







mean(dat[temp_int== 2 & !is.na(both_trials)]$both_trials)
mean(dat[temp_int== 4 & !is.na(both_trials)]$both_trials)
mean(dat[temp_int== 6 & !is.na(both_trials)]$both_trials)



mean(!is.na(dat[temp_int==3]$both_trials))
mean(dat[temp_int==5]$both_trials)
mean(dat[temp_int==7]$both_trials)


# = dat[id== unique(tmp_spc[V1== 4]$id) & category== unique(tmp_spc[V1== 4]$category), game]+1

tmp_spc= dat[, max(game)- min(game), by= .(id, category)]

k= vector()
j= vector()
for(cats in ncat){
  this_cat= dat[category== cats & listrank== 2]
  that_cat= dat[category== cats & listrank== 1]
  l1word= unique(this_cat[listrank==1]$item)
  l2word= unique(this_cat[listrank==2]$item)
  for (word in 1:length(l2word)){
    # k[word]= (length(dat[category== cats & item== l2word[word]]$game)-1)/length(nsubj)
    k[word]= (sum(this_cat$item %in% l2word[word])-1)/length(nsubj)
    dat[category== cats & listrank== 2 & item== l2word[word]]$typicality= (sum(this_cat$item %in% l2word[word])-1)/length(nsubj)
  }
  for (item in 1:length(l1word)){
    dat[category== cats & listrank== 1 & item == l1word[item]]$typicality = (sum(that_cat$item %in% l1word[word]-1)/length(nsubj))
  }
}



# glmer(rep ~ typicality * temporal-interval + (1|category) + (1|id), data=dat, family="binomial")
modreptyp= glmer(both_trials ~ typicality * temp_int + (1|id) + (1|category), data= dat, family= "binomial")

summary(modreptyp)





sf2[i]= (sum(this_cat[listrank==2 & item == words[i], .N, by= id]$N)-1)/length(nsubj)




summary(glmer(data= dat, both_trials~ temp_int + typicality + (1|id)+ (1|category), family= "binomial"))


dat[listrank== 2 & item %in% l2word[length(l2word)]]



lmer(data= dat, sf2~ typicality+ temp_int)

dat[, temp_int]
get_counts= data.table(id= character(), category= character(), temp_int= numeric())
for (subject in nsubj){
  for (cats in ncat){
    if(any(unique(dat[id== subject, category]) %in% cats)){
      this_subj= dat[id== subject & category == cats]
      tmpint= max(this_subj$game)-min(this_subj$game)
      if(tmpint != 4 & tmpint != 6){
        get_counts= rbind(get_counts, list(subject, cats, tmpint))
        dat[id== subject & category== cats]$temp_int= tmpint
      }
    }
  }
}








k = dat[id== nsubj[1]]
max(k$game)

