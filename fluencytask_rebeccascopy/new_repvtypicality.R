# if (getwd()!= "~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/logs"){
#   setwd("~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/")
# }
# 
# # Clear workspace line here###
# # load wikipedia2vec on lab computer -->  run python script to get all similarities --> save csv --> nevr touch wikipedia2vec again. 
# # Install xCode --> xcode commandline tools --> option to do that + google
# 
# 
# 
# 
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
# # set up data file
# dat <- data.table(read.csv("final_results.csv"))
# dat<- subset(dat,select=-c(X))
# nsubj= unique(dat$id)
# ncat= unique(dat$category)
# # get indices for categories that were !repeated twice
# k= dat[, .N, by= .(category, game, id)]
# cat_table= k[, .N, by= .(id, category)]
# cat_table= cat_table[N==2]
# dat= merge(dat, cat_table)
# dat= dat[N== 2]
# # Drop games 23-24
# ncat= unique(dat$category)
# for (i in 1:length(nsubj)){
#   for (j in 1:length(ncat)){
#     this_game <- dat[id== nsubj[i] & category== ncat[j], game]
#     dat[id== nsubj[i] & category== ncat[j], listnum:= max(game)]
#   }
# }
# dat[listnum== "FALSE", listrank:= 1]
# dat[listnum == "TRUE", listrank:= 2]
# dat[, both_trials := 0]
# dat<- subset(dat, select=-c(listnum))
# # get items that were listed in both trials 
# for (i in 1:length(nsubj)){
#   for (cats in ncat){
#     this_subj <- dat[id==nsubj[i] & category== cats,]
#     repeated_words <- intersect(this_subj[listrank==2,item], this_subj[listrank==1,item])
#     this_subj[, both_trials:= 0]
#     this_subj[item %in% repeated_words, both_trials:=1]
#     dat[id==nsubj[i] & category== cats]$both_trials <- this_subj$both_trials
#   }
# }

rm(list= ls())
library(lme4)
library(cowplot)
library(ggplot2)
library(data.table)
dat <- data.table(read.csv("results_cleaned.csv"))
dat<- subset(dat,select=-c(X))
nsubj= unique(dat$id)
ncat= unique(dat$category)
dat[, listrank:= NaN]
for (subject in nsubj){
  for (cats in ncat){
    this_subj= dat[id== subject & category== cats]
    if(length(unique(this_subj$game))==2){
      this_subj[game %in% min(game)]$listrank= 1
      this_subj[game %in% max(game)]$listrank= 2
    } else if(length(unique(this_subj$game))>2){
      this_subj[game %in% min(game)]$listrank= 1
      this_subj[game> min(game) && game< max(game)]$listrank= 2
      this_subj[game== max(game)]$listrank= 3
    } else if(length(unique(this_subj$game))<2){
      this_subj[game %in% min(game)]$listrank= 1
    }
    dat[id== subject & category== cats,]= this_subj
  }
}
# Take out last extra repetition(listrank== 3) to make sure spacing stays consistent and valid
dat= dat[listrank<3]
# get rid of categories that only have a listrank== 1
for (subject in nsubj){
  this_idx= dat[id== subject]
  for(cats in unique(this_idx$category)){
    this_subj= this_idx[category== cats]
    if(sum(unique(this_subj$listrank))<3)
      dat[id== subject & category== cats,]= NaN
  }
}


for (subject in nsubj){
  this_idx= dat[id== subject]
  for (cats in unique(this_idx$category)){
    l1= this_idx[category== cats & listrank== 1]
    l2= this_idx[category== cats & listrank== 2]
    for(i in 1:length(unique(l1$item))){
      word1_idx= unique(l1[listrank== 1]$item)
      word1= word1_idx[i]
      get_itemnum1= l1[item %in% word1]$itemnum
      it_idx= min(get_itemnum1)
      l1[item %in% word1 & itemnum%in% get_itemnum1[get_itemnum1>it_idx]]= NaN
    }
    dat[id== subject & category== cats & listrank==1]= l1
    for(i in 1:length(unique(l2$item))){
      word2_idx= unique(l2[listrank== 2]$item)
      word2= word2_idx[i]
      get_itemnum2= l2[item %in% word2]$itemnum
      it_idx= min(get_itemnum2)
      l2[item %in% word2 & itemnum%in% get_itemnum2[get_itemnum2>it_idx]]= NaN
    }
    dat[id== subject & category== cats & listrank==2]= l2
  }
}
#Remove all of the trials that were set to NaN
dat= dat[!is.na(id),]
# #Yay no perseverative errors!
# Add both trials col to dat data.table
# get items that were listed in both trials
dat[, both_trials:= NaN]
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
all_cats= data.table(item= character(), pba= numeric(), typicality= numeric(), category= character())
# all_cats= data.table(item= character(), pba= numeric(), typicality= numeric(), category= character())

# this works DO NOT TOUCH IT. Do NOT try to do anything with aesthetics for now, you absolute gaffe machine.
# now I have to leave reminders bc you did it again. stop
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
all_cats= rbind(all_cats, newlist)
}

iggplot() + geom_count(data= all_cats, aes(x= all_cats$typicality, y= all_cats$pba, alpha= 0.5)) + geom_abline(intercept= 0, slope= 1)+ ylim(0,1) + xlim(0,1)+ labs(x= "Typicality", y= "p(Repetition)")


###USE THIS MERGE FOR GET TYPICALITY#####
# merge(dat, all_cats, by=c("item","category"))

ggsave("repvtypicality_summer22.png", device= "png", dpi= 300)


k= dat[listrank==2, .N, by= .(id,category)][, .N, by= category]
# merge(dat, k)



merge(dat, j, by= c("item", "category"))

j= dat[listrank==2, (.N-1)/length(nsubj), by=.(category, item)]


mod= glmer(data= dat[listrank==1], both_trials~ V1 )
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
dat[, typicality:= NaN]
for(cats in ncat){
  this_cat= dat[category== cats & listrank== 2]
  that_cat= dat[category== cats & listrank== 1]
  l1word= unique(this_cat[listrank==1]$item)
  l2word= unique(this_cat[listrank==2]$item)
  for (word in 1:length(l2word)){
    # k[word]= (length(dat[category== cats & item== l2word[word]]$game)-1)/length(nsubj)
    k[word]= (sum(this_cat$item %in% l2word[word])-1)/length(nsubj)
    dat[category== cats & listrank== 2 & item== l2word[word]]$typicality== (sum(this_cat$item %in% l2word[word])-1)/length(nsubj)
  }
  for (item in 1:length(l1word)){
    dat[category== cats & listrank== 1 & item == l1word[item]]$typicality== (sum(that_cat$item %in% l1word[word]-1)/length(nsubj))
  }
}



# glmer(rep ~ typicality * temporal-interval + (1|category) + (1|id), data=dat, family="binomial")
modreptyp= glmer(both_trials ~ typicality * temp_int + (1|id) + (1|category), data= dat, family= "binomial")

summary(modreptyp)


glmer(both_trials ~typicality + temp_int + (1|id) + (1|category), data= dat, family= "binomial")











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

