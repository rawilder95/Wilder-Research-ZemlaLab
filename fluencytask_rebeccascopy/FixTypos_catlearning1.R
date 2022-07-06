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
      l2[item %in% word2 & itemnum%in% get_itemnum1[get_itemnum2>it_idx]]= NaN
      }
    dat[id== subject & category== cats & listrank==2]= l2
    }
  }
#Remove all of the trials that were set to NaN
dat= dat[!is.na(id),]


# Check for perseverative errors
k= dat[, .N, by= .(id, game, category, item)]
k[N>2]
#No perseverative errors :)
