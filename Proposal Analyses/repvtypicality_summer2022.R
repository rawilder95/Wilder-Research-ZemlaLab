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

# set up data file
dat <- data.table(read.csv("final_results.csv"))
dat<- subset(dat,select=-c(X))
nsubj= unique(dat$id)


ncat= unique(dat$category)
for (i in 1:length(nsubj)){
  for (j in 1:length(ncat)){
    this_game <- dat[id== nsubj[i] & category== ncat[j], game]
    dat[id== nsubj[i] & category== ncat[j], listnum:= this_game== max(this_game)]
  }
}
dat[listnum== "FALSE", listrank:= 1]
dat[listnum == "TRUE", listrank:= 2]
dat<- subset(dat, game<23)
dat[, both_trials := 0]


# get items that were listed in both trials 
for (i in 1:length(nsubj)){
  this_subj <- dat[id==nsubj[i],]
  repeated_words <- intersect(this_subj[listrank==2,item], this_subj[listrank==1,item])
  this_subj[, both_trials:= 0]
  this_subj[item %in% repeated_words, both_trials:=1]
  dat[id==nsubj[i]]$both_trials <- this_subj$both_trials
}

# check for perseverative errors
# how to get indices of perseverations
# this_subj[listrank== 1,.N, by= item]

# This loop looks up and gets rid of perseverative erros by setting to NaN
check4err= data.table()
for (subject in nsubj){
  for (cats in ncat){
    this_subj <- dat[listrank== 1 & id== subject & category== cats]
    if(any(this_subj[listrank== 1,.N, by= item]$N> 1)){
      check4err <- this_subj[listrank== 1,.N, by= item]
      get_err <- this_subj[item %in% check4err[check4err$N>1]$item]
      smallest_val <- min(get_err$itemnum)
      # larger_val <- get_err[itemnum != smallest_val]
      this_subj[item %in% unique(get_err$item) & itemnum %in% smallest_val]$item = NaN
      dat[id== subject & category == cats & listrank== 1] <- this_subj
    }
  }
}






# isolate items listed in repeated fluency trial
trial2= dat[listrank==2]
trial1= dat[listrank==1]
# p(listing a word on list2| that it was listed on list 1)
pba1= vector()
for (subject in nsubj){
  for(cats in ncat){
    l1= dat[id== subject & category== cats & listrank== 1]
    l2= dat[id== subject & category== cats & listrank== 2]
  }
}


# Right here 
typicality= data.frame()
pba= dat[listrank==2, mean(both_trials), by= .(unique(item))]



typicality2= dat[both_trials==1, .N, by= (item)]
typicality= dat[, .N, by= (item)]
typicality1=typicality[item %in% typicality2$item,]


typicality2$N/length(nsubj)*2

typicality1= (trial2[item %in% unique(trial2[both_trials==1 & !is.na(item)]$item), .N, by= (item)]$N-1)/length(nsubj)



# get typicality score
sus_words= vector()
typicality2= vector()
pba1= vector()
for (i in 1:length(unique(trial2[both_trials==1 & !is.na(item)]$item))){
  word2= unique(trial2[both_trials==1 & !is.na(item)]$item)
  typicality2[i]= (sum(trial2[item %in% word2[i], .N, by= id]$N)-1)/length(nsubj)
  pba1= trial2[item%in% unique(trial1[both_trials==1]$item), .N, by= item]$N/length(nsubj)
  if(typicality2[i]>1){
    sus_words[i]= word2[i]
  }
}

word2= word2[1:length(typicality2)]

# put it all in a data table for legibility
word2_dt= data.table(word2, pba1, typicality2)








colnames(word2_dt) = c("Item", "p(B|A)", "Typicality") 




ggplot() + geom_count(aes(x= typicality2[1:975], y= pba1))

for (subject in nsubj){
  for (cats in ncat){
    trial2= dat[id== subject & category== cats & listrank==2]
    trial1= dat[id== subject & category== cats & listrank==1]
    uwords2= unique(trial2$item)
    uwords1= unique(trial2[both_trials==1]$item)
    
  }
}



k= dat[, .N, by= .(both_trials, item)]
bt= vector()
# for (i = 1:length(unique(k$item))){
#   bt[i]= k[item== item[i] & both_trials== 1]
# }



words2= data.table(item= unique(dat[listrank==2, item]), count= numeric())
# dat[item %in% words2[1], .N, by= id]


  for (subject in nsubj){
    for (cats in ncat){
      this_subj= dat[id== subject & category== cats]
      for (i in 1:length(unique(this_subj$item))){
        word= this_subj$item[i]
        if(any(words2$item %in% word)){
      }
    }
    }}
# dat[id== "Sj1Fs4GWRuX" & category== "Clothing Articles",]

this_subj= dat[id== nsubj[2]]
ext_cat = this_subj[, .N, by= .(game, category)]
ext_cat[, .N, by= (category)]
# last 2 games need to be removed

word_counts= data.table(item= sfboth$item, count= 0, category= character())

sfboth= dat[listrank==2 & both_trials==1, .N, by= .(category,item)]
sf1= dat[listrank==1, .N, by= .(category,item)]
for(i in 1:length(sfboth$item)){
  words= sfboth$item
  for (cats in ncat){
    newrow= list(sf1[category== cats & item== words[i]]$item, sf1[category== cats & item== words[i]]$N, sf1[category==cats]$category)
    word_counts= rbind(word_counts,newrow)
  }
  word_counts[i]$count= sf1[item %in% words[i]]$N
}




k= dat[listrank==2 & both_trials==1, .N, by= .(item, id, category)]
dat[item== "cupboard" & category != "Objects You Would Find in The Kitchen"]





























############### CLEAN CODE FOR JEFF #######################
##########################  COMPARISON FOR CATEGORY 2 ##########################
this_subj= dat[id== nsubj[1] & category== ncat[2]]
# Grab all items generated from list 2
# words2= unique(fruit[listrank== 2,item])
words2= unique(dat[listrank== 2 & category==ncat[1], item])

sf2 <- vector()
sf2sf1 <- vector()
for (i in 1:length(words2)){
  check_foroverlap <- intersect(dat[listrank==1 & item== words2[i]]$id, dat[listrank==2 & item== words2[i]]$id)
  sf2[i] <- sum(dat[listrank== 2 & item== words2[i], .N, by= id]$N)-1
  if (length(check_foroverlap)>0){
    sf2sf1[i] <- length(check_foroverlap)/sum(dat[listrank== 1 & item== words2[i], .N, by= id]$N)
  }else{
    sf2sf1[i] <- 0
  }
}

pcat <- data.table()
pcat[,sf2:= sf2/length(nsubj)]
pcat[,sf2sf1:= sf2sf1]

ggplot() + geom_count(aes(x= pcat$sf2, y= pcat$sf2sf1))

