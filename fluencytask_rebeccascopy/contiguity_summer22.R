rm(list= ls())
library(lme4)
library(cowplot)
library(ggplot2)
library(data.table)
dat <- data.table(read.csv("final_results.csv"))
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
dat[, temp_int:= 0]
dat[, temp_int:= ((max(game)-min(game))-1), by= .(id, category)]
### CONTIGUITY CURVE ####
lag_df= data.table()
lag0= vector()
s_df= data.table(id= character(), category= character(), item= character(), sp1= numeric(), dist= numeric(), range= numeric())
for (subject in nsubj){
  for(cats in ncat){
    idx= nrow(dat[id== subject & category== cats & both_trials== 1 & listrank==2])>1 & nrow(dat[id== subject & category== cats & both_trials== 1 & listrank==2])
    if (idx== TRUE){
      l2= dat[id== subject & category== cats & both_trials== 1 & listrank== 2]
      l1= dat[id== subject & category== cats & listrank== 1]
      s_vec= vector()
      id_vec= vector()
      cat_vec= vector()
      item_vec= vector()
      sp_vec2= vector()
      sp_vec1= vector()
      for(i in 1:(nrow(l2)-1)){
        s_vec[i]= l1[item %in% l2[i+1]$item]$itemnum - l1[item %in% l2[i]$item]$itemnum
        # if(s_vec[i]==0){
        #   newlag= s_vec[i]
        # }
        # lag_df= rbindlist(list(lag_df, newlag))
        id_vec[i]= subject
        cat_vec[i]= cats
        item_vec[i]= l2[i]$item
        sp_vec1[i]= min(l1[l1$item %in% l2[i]$item]$itemnum)
        # sp_vec2[i]= l1[i]$itemnum
      }
      max_range= rep(max(abs(s_vec)),length(s_vec))
      newrow= list(id_vec, cat_vec, item_vec, sp_vec1, s_vec, max_range)
      s_df= rbindlist(list(s_df, newrow))
    }
  }
}

s_df[dist== 0]= NaN
### Sanity Check Calculations ###
#Forwards
s_df[dist>0, mean(dist), by= .(id, category)]
s_df[dist>0, mean(dist), by=  id]
s_df[dist>0, mean(dist), by= category]
#Backwards
s_df[dist<0, mean(dist), by= .(id, category)]
s_df[dist<0, mean(dist), by=  id]
s_df[dist<0, mean(dist), by= category]
#more distance going backwards than forwards
#Get counts for how many forward v backward order transitions by subj
bckwd= s_df[dist<0, .N, by= .(id, category)]
fwd= s_df[dist>0, .N, by= .(id, category)]
mean(fwd$N) #5.601322
mean(bckwd$N) #3.815315
# Replicate Jeff's histogram
ggplot()+ geom_histogram(aes(x= s_df[!is.nan(dist)]$dist))+ labs(x= "Lag", y= "Frequency")+ xlim(min(s_df$dist), max(s_df$dist))
min(s_df$dist)#max backwards lag (-25)
max(s_df$dist)#max forwards lag (29)

# Do counts
#don't worry about listrank bc you're only looking at listrank 2 output
get_counts= data.table(range= c(1:(max(s_df$range))), actual= rep(0,max(s_df$range)))

k= s_df[, .N, by= .(id, category, dist)]

ggplot() + geom_point(aes(x= k$dist, y= k$N))

back_idx= vector()
# p_counts= data.table(possible= c(min(s_df$dist):max(s_df$dist)), counts= rep(0, length(min(s_df$dist):max(s_df$dist)))) #subject index
# possible_counts= data.table(possible= numeric(), counts= numeric()) #full set using rbindlist
all_transitions= data.table(possible= numeric())
for(subject in nsubj){
  this_idx= s_df[id== subject]
  for(cats in unique(this_idx$category)){
    # forward backwards possible transition vectors
    this_subj= this_idx[category== cats]
    get_transitions= data.table(possible= numeric())
    for (i in 1:nrow(this_subj)){
      back_idx[i]= this_subj[i]$sp1
      btr= c(min(this_subj$sp1):this_subj[i]$sp1)
      btr= btr[!btr %in% this_subj[i]$sp1 & !btr %in% back_idx]
      #There's probably a cleaner way to do this- but doing it out fully to make sure that I have it right
      if(this_subj[i]$dist== -25){break}
      b1= btr- this_subj[i]$sp1
      ftr= c(this_subj[i]$sp1: max(this_subj$sp1))
      ftr= ftr[!ftr %in% this_subj[i]$sp1 & ! ftr %in% back_idx]
      f1= ftr- this_subj[i]$sp1
      # How many possible transitions?
      newlist= list(c(b1, f1))
      get_transitions= rbindlist(list(get_transitions,newlist))
      # idk if it goes here
      all_transitions= rbindlist(list(all_transitions,get_transitions))
    }
  }
}

# fuck it's because each subject has a different SP length- the kahana CRP tutorial assumes that SP range will be fixed- so range is is out of 1:length(N items presented)
get_counts= vector()
range_vec= c(min(s_df$dist):max(s_df$dist))
p_vec= data.table(possible= range_vec, counts= NaN)
for (i in 1:length(range_vec)){
  p_vec[i]$counts= sum(all_transitions$possible %in% range_vec[i])
}
k= s_df[, .N, by= dist]
k= k[order(dist)]
colnames(k)= colnames(p_vec)


all_counts= merge(k, p_vec, by= "possible")
colnames(all_counts) <- c("lag", "observed", "possible")
all_counts[, CRP:= observed/possible]
# Add rows for the impossible transitions e.g. 0
k= all_counts
for (i in 1:length(range_vec)){
  idx= range_vec[i]
  if(!any(all_counts$lag %in% range_vec[i])){
    print(range_vec[i])
    k[i]$lag= range_vec[i]
    k[i]$possible= 0
    k[i]$observed= 0
    k[i]$CRP= NaN
  }
}
all_counts= k

ggplot(data= all_counts, aes(x= lag, y= CRP))+ geom_line() + geom_point()+ xlim(-20,20)+ labs(title= "Lag CRP", x= "Lag")
ggsave("lagcrp_summer22.png", device= png, dpi= 300)

# get forwards and backwards transitions
all_counts[, directionality:= as.numeric(lag>0)]
sum(all_counts[directionality==0]$observed)
sum(all_counts[directionality==1]$observed)
# plot that into a binomial test

# # just get the table counts for each instance of value
# j= all_transitions[, .N, by= possible]
# k= s_df[, .N, by= dist]
# k= k[dist>= -13]
# k= k[dist<= max(j$possible)]
# colnames(k) <- c("dist", "N")
# colnames(j) <- c("dist", "N")
# # compare possible versus actual counts
# comp_pcounts= merge(j,k, by= "dist")
# colnames(comp_pcounts) <- c("dist", "possible", "actual")

write.csv(s_df, file= "contiguityfor_python.csv")


