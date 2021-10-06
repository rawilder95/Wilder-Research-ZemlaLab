if (getwd()!= "/Users/rebeccawilder/Documents/GitHub/Wilder-Research-ZemlaLab"){
  setwd("~/Documents/GitHub/Wilder-Research-ZemlaLab")
}
# Install packages and libraries
# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("cowplot")
# library("cowplot")



library(data.table)
library(ggplot2)
# Load Data
dat <- data.table(read.csv("snafu_sample.csv"))

(dat <- dat[dat[,group == "Experiment1"]]) #Only looking at Experiment 1

# Data Setup
dat[listnum<3, listrank:= 1]
dat[listnum>=3 & listnum <6, listrank := 2]
dat[listnum>=6, listrank := 3]
fruit <- dat[category== "fruits"]
veg <- dat[category== "vegetables"]
animal <- dat[category== "animals"]

# Just looking at Fruit right now but the rest can be added later
fruit[listnum<3, listrank:= 1]
fruit[listnum>=3 & listnum <6, listrank := 2]
fruit[listnum>=6, listrank := 3]

nsubj= unique(dat$id) #Get number of subjects in Experiment 1



############# SPOTCHECK EXAMPLE  SINGLE ITEM ################
#  # Get counts for rp fluency trial 1
# idl1 <- fruit[listrank== 1 & item== "kiwi"]$id
#  # Get counts for rp fluency trial 2
# idl2 <- fruit[listrank== 2 & item== "kiwi"]$id
# # 
# # # Find overlapping id's between trial 1 and 2 for item i
# length(intersect(idl1,idl2))

# This works and adds up correctly

##########################  COMPARISON FOR CATEGORY 2 ##########################

# Grab all items generated from list 2
fruit_items= unique(fruit[listrank== 2,item])


sf2 <- vector()
sf2sf1 <- vector()
  for (i in 1:length(fruit_items)){
    check_foroverlap <- intersect(fruit[listrank==1 & item== fruit_items[i]]$id, fruit[listrank==2 & item== fruit_items[i]]$id)
    sf2[i] <- sum(fruit[listrank== 2 & item== fruit_items[i], .N, by= id]$N)-1
    if (length(check_foroverlap)>0){
      sf2sf1[i] <- length(check_foroverlap)/sum(fruit[listrank== 1 & item== fruit_items[i], .N, by= id]$N)
    }else{
      sf2sf1[i] <- 0
    }
  }
    
pfruit <- data.table()
pfruit[,sf2:= sf2/length(nsubj)]
 pfruit[,sf2sf1:= sf2sf1]
 
 ##################  REPEAT COMPARISON FOR CATEGORY 2 ####################
 veg[listnum<3, listrank:= 1]
 veg[listnum>=3 & listnum <6, listrank := 2]
 veg[listnum>=6, listrank := 3]
 
 veg_items= unique(veg[listrank== 2,item]) # Get unique items in repeated fluency trial
 
 
 vf2 <- vector()
 vf2vf1 <- vector()
 for (i in 1:length(veg_items)){
   check_foroverlap <- intersect(veg[listrank==1 & item== veg_items[i]]$id, veg[listrank==2 & item== veg_items[i]]$id)
   vf2[i] <- sum(veg[listrank== 2 & item== veg_items[i], .N, by= id]$N)-1
   if (length(check_foroverlap)>0){
     vf2vf1[i] <- length(check_foroverlap)/sum(veg[listrank== 1 & item== veg_items[i], .N, by= id]$N)
   }else{
     vf2vf1[i] <- 0
   }
 }
 
 pveg= data.table()
 pveg[, vf2:= vf2/length(nsubj)]
 pveg[, vf2vf1:= vf2vf1]
 
 ##################  REPEAT COMPARISON FOR CATEGORY 3 ####################
 animal[listnum<3, listrank:= 1]
 animal[listnum>=3 & listnum <6, listrank := 2]
 animal[listnum>=6, listrank := 3]
 
 animal_items= unique(animal[listrank== 2,item]) # Get unique items in repeated fluency trial
 
 
 af2 <- vector()
 af2af1 <- vector()
 for (i in 1:length(animal_items)){
   check_foroverlap <- intersect(animal[listrank==1 & item== animal_items[i]]$id, animal[listrank==2 & item== animal_items[i]]$id)
   af2[i] <- sum(animal[listrank== 2 & item== animal_items[i], .N, by= id]$N)-1
   if (length(check_foroverlap)>0){
     af2af1[i] <- length(check_foroverlap)/sum(animal[listrank== 1 & item== animal_items[i], .N, by= id]$N)
   }else{
     af2af1[i] <- 0
   }
 }
 
 panimal= data.table()
 panimal[, af2:= af2/length(nsubj)]
 panimal[, af2af1:= af2af1]
 

 

 #############################  PLOT DATA ################################  
ggplot(data= data.frame(pfruit), aes(x= sf2, y= sf2sf1, color= "Fruits"))+ geom_count(alpha= 0.3)+ geom_abline() + labs(x= "Sum(SF2)-1/length(N Subjects)", y= "Sum(SF2 & SF1)/ Sum(SF1)", color= "Category", size= "Frequency")+ geom_count(data= data.frame(pveg), aes(x= vf2, y= vf2vf1, color= "Vegetables"), alpha= 0.3)+ geom_count(data= data.frame(panimal), aes(x= af2, y= af2af1, color= "Animals"), alpha= 0.3)
ggsave("fixed_sfcomparison10032021", device= "png", dpi= 300)


################### Something like an SPC? ###################



s1 <- fruit[, sp:= 0]
s2 <- vector()

for (i in 1:length(unique(s1[,id]))){
  s1[id== unique(id)[i] & listrank== 1]$sp <- 1:length(fruit[id== unique(id)[i] & listrank== 1]$id)
  s1[id== unique(id)[i] & listrank== 2]$sp <- 1:length(fruit[id== unique(id)[i] & listrank== 2]$id)
  s1[id== unique(id)[i] & listrank== 3]$sp <- 1:length(fruit[id== unique(id)[i] & listrank== 3]$id)
}

sp1 <- vector()
for (i in 1:length(unique(s1[listrank== 1,item]))){
  sp1[i]= sum(s1[listrank>1,item]== unique(s1[listrank== 1,item])[i])/(length(nsubj)*2)
}

sp3 <- matrix()


for (i in 1:length(unique(nsubj))){
  this_subj <- s1[id== nsubj[i],]
  for (j in 1:10){
    sp3[i,j:j+length(j)] <- sum(this_subj[,sp]==j)
  }
}


ggplot()+ geom_line(aes(x= 1:length(unique(s1[listrank== 1,item])), y= sp1))
# s1[id== unique(id)[1]]$sp <- 1:length(fruit[id== unique(id)[1]]$id)
# s1[id== unique(id)[2]]$sp <- 1:length(fruit[id== unique(id)[2]]$id)
# s1[id== unique(id)[3]]$sp <- 1:length(fruit[id== unique(id)[3]]$id)
# s1[id== unique(id)[4]]$sp <- 1:length(fruit[id== unique(id)[4]]$id)

(s1 <- s1[sp>0,])

# Get shortest Output length
(k <- s1[, .N, by= id])
k[N==min(N)]$id
sp1 <- s1[item== s1[1, item] & listrank== 1]
sp2 <- s1[item== s1[1, item] & listrank== 2]

s2[1] <- length(sp2$sp)/length(sp1$sp) 
s2[2]<- length(s1[item== unique(s1$item)[2] & listrank== 2]$sp)/length(s1[item== unique(s1$item)[2] & listrank== 1]$sp) 


s2[3]<- length(s1[item== unique(s1$item)[3] & listrank== 2]$sp)/length(s1[item== unique(s1$item)[3] & listrank== 1]$sp) 

s2[4]<- length(s1[item== unique(s1$item)[4] & listrank== 2]$sp)/length(s1[item== unique(s1$item)[4] & listrank== 1]$sp)

nfruit <- unique(fruit$item)

length(unique(s1[id== unique(id)[1] & item== item[1],]$listrank))/3 #repeated trials
s1[id== unique(id)[2] & item== item[1],]

sum(s1[,sp]==1)








# For loop through item listed in first trial (i)
ses_1 <- s1[id== id[1] & listrank== 1]$item
# Find index of item listed in list 2
ses_2 <- s1[id== id[1] & listrank== 2]$item
# Find repeated items for subject[i]
common_items <- intersect(ses_1, ses_2)
# New for loop through common items within other for loop (j)
# Assign "sp/rp" from item in first trial to second trial 
s1[id== unique(id)[1] & item== common_items[1] & listrank == 2]$sp = s1[item== common_items[1] & id== unique(id)[1] & listrank == 1, sp]

# Set the second trial sp to 0 for now
s1[listrank== 2]$sp<- 0
for (subj in 1:length(nsubj)){
    # 'This Subject' Var.  Indexing variable for a specific subject
  this_subj1 <- s1[id== unique(id)[subj] & listrank == 1]
  this_subj2 <- s1[id== unique(id)[subj] & listrank == 2]
  common_items <- intersect(this_subj1$item, this_subj2$item)
  for (i in 1:length(common_items)){
    # This is a super annoying work around, but I'm basically going through and finding the repeated items and matching them to the repeated trial.  Then I'm going in and assigning the RP from the first trial to the second trial.  
    s1[id== unique(this_subj1$id) & listrank== 2 & item== common_items[i]]$sp= first(s1[id== unique(this_subj1$id) & listrank== 1 & item== common_items[i]]$sp)
  }
}
# We don't really care about the third repetition ATM so just take it out. It's getting confusing
s1 <- s1[listrank<3,]
########################### REPEATED TRANSITIONS NOT FINISHED ##################################
# s1[id== "A101"]
# (full_transitions= matrix(rep(NaN, 200), nrow= 20, ncol= 100))
# 
# 
# transitions <- vector()
# transitions <- NaN
# for (subj in 1:length(nsubj)){
#   transitions <- vector() #clear transitions var
#   for (i in 1:length(s1[id== nsubj[subj] & listrank== 2,sp])-1){
#     this_subj= s1[id== nsubj[subj] & listrank== 2,]
#     if (sum(this_subj$sp[i+1], this_subj$sp[i])==0){
#       transitions[i]= NaN
#     }else{
#       transitions[i]= this_subj$sp[i+1] - this_subj$sp[i]
#     }
#   }
#   full_transitions[subj,1:i] <- transitions
# }
# (n_transitions <- as.vector(sort(unique(full_transitions[!is.nan(full_transitions[,])]))))
# 
# rownames(full_transitions) <- nsubj
# 
# # full_transitions <- data.table(full_transitions)
# (full_transitions <- full_transitions[,1:30])
# 
# nonnan_transitions <- full_transitions[!is.nan(full_transitions)]
# 
# sum(nonnan_transitions== n_transitions[26])/length(nonnan_transitions)
# p_transitions <- vector()
# for (i in 1:length(n_transitions)){
#   p_transitions[i] <- sum(nonnan_transitions== n_transitions[i])/length(nonnan_transitions)
# }
# 
# 
# ggplot()+ geom_line(aes(x= n_transitions, y= p_transitions))+ geom_point(aes(x= n_transitions, y= p_transitions))+ labs(x= "Transition", y= "p(Transition)")
  

