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
# for(subject in nsubj){
#   for(cats in ncat){
#     if(any(dat[id== subject & category == cats & listrank==1, .N, by= .(item, game)]$N>1)){
#       this_subj= dat[id== subject & category == cats & listrank==1]
#       get_err= this_subj[, .N, by= .(id, category, game, item)]
#       this_subj= merge(this_subj, get_err, by= c("id", "category", "game", "item"))
#       }
#   }
# }
idx_dt= data.table(id= character(), category= character())
for(subject in nsubj){
  for(cats in ncat){
    if(any(dat[id== subject & category == cats & listrank==1, .N, by= .(item, game)]$N>1)){
      newlist= list(subject, cats)
      idx_dt= rbindlist(list(idx_dt,newlist))
      this_subj= dat[id== subject & category == cats & listrank==1]
    }
  }
}


for (subject in idx_dt$id){
  this_subj= dat[id== idx_dt[i]$id & category== idx_dt[i]$category]
}

this_subj[, get_counts:= .N, by= .(id, category, game, item)]
k= this_subj[get_counts>1]
k

