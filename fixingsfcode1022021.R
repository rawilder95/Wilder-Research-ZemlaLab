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

