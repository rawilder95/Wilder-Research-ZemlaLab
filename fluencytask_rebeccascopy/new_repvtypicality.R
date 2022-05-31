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

# get indices for categories that were !repeated twice
k= dat[, .N, by= .(category, game, id)]
cat_table= k[, .N, by= .(id, category)]

dat= merge(dat, cat_table)
dat= dat[N== 2]


# Drop games 23-24
ncat= unique(dat$category)
for (i in 1:length(nsubj)){
  for (j in 1:length(ncat)){
    this_game <- dat[id== nsubj[i] & category== ncat[j], game]
    dat[id== nsubj[i] & category== ncat[j], listnum:= this_game== max(this_game)]
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
cats= ncat[1]

this_cat= dat[category== cats]
words= unique(this_cat[listrank== 2, item])
sf2 <- vector()
both_trials <- vector()
all_cats= data.table(item= character(), pba= numeric(), typicality= numeric(), category= character())
for (cats in ncat){
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

  
mean(dat[temp_int==2]$both_trials)
mean(dat[temp_int==4]$both_trials)
mean(dat[temp_int==6]$both_trials)

ggplot() + geom_count(aes(x= all_cats$typicality, y= all_cats$pba), alpha= 0.5)+ geom_abline(intercept= 0, slope= 1)+ labs(y= "p(Repetition)", x= "Item Typicality")
ggsave("repvtypicality_summer22.png", device= "png", dpi= 300)

#################### Temporal Interval ######################



cat_spacing= vector()
for (subject in 1:length(nsubj)){
  for (cats in 1:length(ncat)){
    this_subj= dat[id== nsubj[subject] & category==ncat[cats]]
    cat_spacing= max(this_subj$game)- min(this_subj$game)-1
    dat[id== nsubj[subject] & category== ncat[cats], temp_int:= cat_spacing]
  }
}

dat[temp_int<0]= NaN
dat[, typicality:= 0]


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



sf2[i]= (sum(this_cat[listrank==2 & item == words[i], .N, by= id]$N)-1)/length(nsubj)




summary(glmer(data= dat, both_trials~ temp_int + typicality + (1|id)+ (1|category), family= "binomial"))


dat[listrank== 2 & item %in% l2word[length(l2word)]]



