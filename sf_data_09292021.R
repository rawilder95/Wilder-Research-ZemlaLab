if (getwd()!= "/Users/rebeccawilder/Desktop/Research 2021-2022"){
  setwd("~/Desktop//Research 2021-2022")
}

# install.packages("data.table")
# library(data.table)
library(ggplot2)

sf1 <- data.table(read.csv("snafu_sample.csv"))
sf1= sf1[sf1[, group== "Experiment1" & category=="fruits"]]
sf1= sf1[,1:7-1]


nsubj <- unique(sf1$id)
for (i in 1:length(nsubj)){
  
}


this_subj <- sf1[id==id[1] & category== "fruits",]





unique(this_subj$listnum)

l1 <- sum(this_subj[listnum== listnum[1], item]== "strawberry")

l2 <- sum(this_subj[listnum== listnum[2], item]== "strawberry")

l3 <- sum(this_subj[listnum== listnum[1], item]== "strawberry")


rec_straw <- data.table()
rec_straw[, l1:= 1:length(nsubj)]
rec_straw[, l1:= 1:length(nsubj)]
rec_straw[, l1:= 1:length(nsubj)]
pstraw <- vector()

for (i in 1:length(nsubj)){
  this_subj <- sf1[id==nsubj[i] & category== "fruits",]
  pstraw[i] <- sum(this_subj[listnum== unique(listnum[1]), item]== "strawberry")
}
# Account for 1 perseverative intrusion
pstraw[pstraw>1] <-1

psf2 <- vector()
(sum(pstraw)-1)/length(nsubj)
for (i in 1:length(nsubj)){
    this_subj<-  sf1[id==nsubj[i] & category== "fruits",]
    psf2[i] <- sum(this_subj[listnum== unique(listnum[2]), item]== "strawberry")
    
}

# Account for 1 perseverative intrusion
psf2[psf2>0]= 1

sum(pstraw)/sum(psf2)

nitems <- vector()

# for (i in 1:length(unique(this_subj$item))){
#   items = unique(this_subj$item)
#   nitems[i] <- sum(this_subj$item== items[i])
# }

# You're going to do an initial for loop for the subject
#A second for loop for the category

# Set 'this_subj' to the current subject and current category (subj, ses) respectively
(this_subj <- sf1[id== id[1] & category==category[1]])

# Set a variable 'this_list' for grabbing the current subject's initial and repeated fluency trials for the given category
this_list= unique(this_trial[,listnum])

# New for loop to iterate through the unique list trials.
  # Within all of these loops, set a new loop to iterate through each listed item in that given trial.
(this_trial= this_subj[listnum== this_list[1]])
(this_trial= this_subj[listnum== this_list[2]])
(this_trial= this_subj[listnum== this_list[3]])

samp_lists <- data.table()
samp_lists[, l1:= rep(0, length(this_trial$id))]
samp_lists[, l2:= rep(0, length(this_trial$id))]
samp_lists[, l3:= rep(0, length(this_trial$id))]

# To account for intrusions, we just want to know whether the item[x] was listed in trial[i] for category[y].  If (sum(this_trial[,item]== this_trial[it_idx, item])>0){ vec1[it_idx]= 1} else{ vec1[it_idx]= 0}.  The var vec1[0 0 1 1 0 1 0 1] etc 
target_item= this_trial[1, item]
samp_lists[1,l3] <- sum(this_trial[,item] == this_trial[1, item])>0
samp_lists[1,l2] <- sum(this_trial[,item] == this_trial[1, item])>0
samp_lists[1,l1] <- sum(this_trial[,item] == this_trial[1, item])>0

all_subj <- rep(0, length(nsubj))



for (subj in 1:length(nsubj)){
  # set current subj. I set the category to fruits only for now, but I'll change that later.
  this_subj= sf1[id== id[subj] & category== unique(category)[1],]
  # set a var to index the unique values of list num, representing each repetition
  list_reps= unique(this_subj$listnum)
  #loop through each repetition
  for (list_idx in 1:length(unique(this_subj[,listnum]))){
    #Set var this_trial to represent the current subject, category, and current initial/repeated trial
    # Setting this to the first trial only, because typicality isn't evaluated during repeated trials
    this_trial= this_subj[listnum== list_reps[list_idx[1]]]
    samp_idx1 <- rep(0,length(this_trial$item))
    for (it_idx in 1:length(this_trial$item)){
      this_item <- this_trial[it_idx, item] #var representing current item
      if (sum(this_trial[,item]== this_item)>0){
        samp_idx1[it_idx]= 1
      } else {
        samp_idx1[it_idx]= 0
      }
    }
  }

}
  
# Operationalizing a single trial for typicality 
this_subj <- sf1[id== id[1] & category== category[1] & listnum== unique(listnum)[1],]
# Get unique items for this category in trial 1
items <- unique(sf1[category== category[1] & listnum== unique(listnum[1]), item])


unique(sf1[id==ids[1] & category== unique(category)[1],listnum])   


items <- vector()

for (i in 1:length(nsubj)){
  this_list= sf1[id== nsubj[i], list]
}



items <- unique(sf1[category== "fruits", item])
bgMat <- matrix(nrow= length(nsubj), ncol= length(items))

bgMat[1:length(nsubj),] <- 0

v1 <- vector()

colnames(bgMat)<- items
rownames(bgMat)<- nsubj


for (subj in 1:length(nsubj)){
  this_list= unique(sf1[id== nsubj[subj] & category== "fruits", listnum])
  # Take the first index of 'this_list' to exclude repeated trials
  this_subj= sf1[id== nsubj[subj] & category== "fruits" & listnum==this_list[1],]
  for (it_idx in 1:length(items)){
    if (sum(this_subj$item== items[it_idx])>0){
      v1[it_idx] <- 1
      bgMat[subj,it_idx] <- 1
    } else{
      v1[it_idx] <- 0
      bgMat[subj, it_idx] <- 0
    }
  }
  bgMat[subj,]= v1
}

# For each person add col that sets repeat trial to 1 2 3 instead of absolute list number
#Interim solution for masking out words that only appeared on repeat trials 
data.table(bgMat)
get_totalcounts <- colSums(bgMat)
get_totalcounts= get_totalcounts[get_totalcounts>0]

# P(j in SF2) = (\sum_i F_{ij2} - 1) /N
fruit_item_typicality <- (get_totalcounts-1)/length(nsubj)


##################################################################
bgMat2 <- matrix(nrow= length(nsubj), ncol= length(items))

bgMat2[1:length(nsubj),] <- 0

for (subj in 1:length(nsubj)){
  this_list= unique(sf1[id== nsubj[subj] & category== "fruits", listnum])
  # Take the first index of 'this_list' to exclude repeated trials
  
  # Items still refers to items from list 1
  this_subj= sf1[id== nsubj[subj] & category== "fruits" & listnum==this_list[2],]
  for (it_idx in 1:length(items)){
    if (sum(this_subj$item== items[it_idx])>0){
      v1[it_idx] <- 1
      bgMat2[subj,it_idx] <- 1
    } else{
      v1[it_idx] <- 0
      bgMat2[subj, it_idx] <- 0
    }
  }
  bgMat2[subj,]= v1
}

data.table(bgMat2)

colnames(bgMat2) <-items
rownames(bgMat2) <- nsubj

bgMat2[bgMat[,]!= bgMat2[,]]= 0 



#Interim solution for masking out words that only appeared on repeat trials 
data.table(bgMat2)
get_totalcounts2 <- colSums(bgMat2)
get_totalcounts2 = get_totalcounts2[get_totalcounts2>0]
get_totalcounts_adj <- c(get_totalcounts, rep(0, length(get_totalcounts2)-length(get_totalcounts)))

#Set words that weren't recalled during first fluency trial but were recalled on second fluency trial to 0 --> will return NaN at the end.  
get_totalcounts2[get_totalcounts_adj== 0 & get_totalcounts2>0]= 0

get_totalcounts2[get_totalcounts_adj> get_totalcounts2]
get_totalcounts_adj[get_totalcounts_adj> get_totalcounts2]

# P(j in SF2 | j in SF1) = (\sum_i F_{ij1} & \sum_i F_{ij2}) / (\sum_i F_{ij1})
get_totalcounts2/get_totalcounts_adj

# NaNs are items that were 

##########################################################################################################

sf1[, .N, by= .(listnum,id)]

sf1[,.N, by= id]

sf1[listnum<3, listrank:= 1]
sf1[listnum>3 & listnum<6, listrank := 2]
sf1[listnum>6, listrank:= 3]



length(sf1[listrank==2 & item== "kiwi", item])/length(nsubj)
length(sf1[listrank==1 & item== "kiwi", item])/length(nsubj)


# This is probability of item[i] appearing on list 2 if it was recalled on list 1
pba <- sf1[listrank< 3 & item== "kiwi", .N,by= id]$N/length(nsubj)/((sum(sf1[listrank== 2, item]== "kiwi")/length(nsubj)))

(pkiwi <- sum(sf1[listrank== 1, item]== "kiwi")/length(nsubj))




sfkiwi21 <- sf1[listrank <3 & item == "kiwi", .N, by= id]

sfkiwi21[N>2]$N=2

sfkiwi2 <- sf1[listrank ==2 & item == "kiwi", .N, by= id]
sfkiwi1[N>2]$N= 2



(sfkiwi21$N-1)/length(nsubj)/(sfkiwi1$N)/length(nsubj)



pab <- (sfkiwi21$N/length(nsubj))/sum(sfkiwi2$N)/length(nsubj)

ggplot() + geom_point(aes(x= 1:14, y= pab))

