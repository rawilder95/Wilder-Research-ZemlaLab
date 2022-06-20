#### CORRECT DIRECTORY CODE ######
if (getwd()!= "~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/logs"){
  setwd("~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/")
}
###Relevant Libraries & Packages###
# install.packages("data.table")
# library(data.table)
# install.packages("googlesheets4")
# library(googlesheets4)
# install.packages("ggplot2")
# library(ggplot2)
# install.packages("lme4")
# library(lme4)
### Read in Data ###
dat <- data.table(read.csv("final_results.csv"))
#For some reason, final_results loading in with both_trials and listrank
# Even after cleaning environment and history...? No idea.
dat<- subset(dat,select=-c(X))
nsubj= unique(dat$id) #unique subject vector
ncat= unique(dat$category) #unique category vector
# Get indicies for categories repeated more or less than twice
counts_idgamecat= dat[, .N, by= .(category, game, id)]
idgamecat_byidcat= counts_idgamecat[, .N, by= .(id, category)]
cat_counts= idgamecat_byidcat[N==2]
dat= merge(dat, cat_counts)
# Calculate listrank variable
for (i in 1:length(nsubj)){
  for (j in 1:length(ncat)){
    this_game <- dat[id== nsubj[i] & category== ncat[j], game]
    dat[id== nsubj[i] & category== ncat[j], listnum:= max(game)]
  }
}
dat[listnum== FALSE,listrank:=1]
dat[listnum== TRUE,listrank:=2]
dat[, both_trials := 0]
dat<- subset(dat, select=-c(listnum))

### Get rid of perseverative errors###
#Check for perseverative errors
for (subject in nsubj) {
  for (cats in ncat) {
    if(nrow(dat[id== subject & category== cats])>0){
      this_subj= dat[id== subject & category== cats]
      if(any(this_subj[, .N, by= .(item, id, category, game, itemnum)]$N)>1)
        print(this_subj[, .N, by= .(item, id, category, game, itemnum)]$N)
    } 
  }
}
#If so, remove them.  If not, just leave this commented out
# for (subject in nsubj){
#   for (cats in ncat){
#     if(nrow(dat[id== subject & category== cats])>0){
#       this_subj= dat[id== subject & category== cats]
#       words= unique(this_subj$item[(!is.na(this_subj$item))])
#       for (i in 1:length(words)){
#         p_err = this_subj[item== words[i], .N, by= .(id, category, game, item)]
#         if(max(p_err[, .N, by= .(game, id, item)]$N)>1 & !is.na(words[i])){
#           this_subj[item== p_err[max(game), item] & game== p_err[max(game),game]]= NaN
#         }
#         dat[id== subject & category== cats]= this_subj
#       }
#     }
#   }
# }


this_subj

# p_err= dat[item== "mango" & category== "Fruits", .N, by= .(id, game, item)]
# p_err= p_err[N>1]
# dat[id %in% p_err[1,id] & game %in% p_err[1, game] & item %in% p_err[1,item], max(itemnum)]= NaN

for (i in unique(dat$item)){
  words= unique(dat$item)
  p_err= dat[item== words[i], .N, by= .(id, game, category)]
  if(nrow(p_err)>0){
    for (j in nrow(p_err)){
      dat[id== p_err[j]$id & game %in% p_err[j, game] & item %in% p_err[j,item], max(itemnum)]= NaN
    }
  }
}


k= dat[item %in% "mango", .N, by= .(id, category, game, itemnum, item)]
g=k[, .N, by= .(id, item, game)]


# dat[id %in% g[N>1]$id & item %in% g[N>1]$item & game %in% g[N>1]$game, max(itemnum), by = . (id, game)]=
merge(dat, g, by.x= "id", by.y= "id", allow.cartesian=TRUE)


g= dat[id %in% k[N>1, unique(id)] & item %in% k[N>1, unique(item)], .N, by= .(id, itemnum, item)]
p= g[,max(itemnum), by= id]
dat[id== p$id]


dat


