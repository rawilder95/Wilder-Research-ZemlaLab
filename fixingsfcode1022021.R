if (getwd()!= "/Users/rebeccawilder/Desktop/Research 2021-2022"){
  setwd("~/Desktop//Research 2021-2022")
}

# install.packages("data.table")
library(data.table)
library(ggplot2)

dat <- data.table(read.csv("snafu_sample.csv"))

(dat <- dat[dat[,group == "Experiment1"]])


dat[listnum<3, listrank:= 1]
dat[listnum>=3 & listnum <6, listrank := 2]
dat[listnum>=6, listrank := 3]

fruit <- dat[category== "fruits"]
veg <- dat[category== "vegetables"]
animal <- dat[category== "animals"]

fruit[listnum<3, listrank:= 1]
fruit[listnum>=3 & listnum <6, listrank := 2]
fruit[listnum>=6, listrank := 3]


t1 = vector(mode= "logical", length= 67)
t2 = vector(mode= "logical", length= 67)
int1t2= vector(mode= "logical", length= 67)

############# HERE IS THE IMPORTANT CORRECTION ################
# What I was doing before and what we did during meeting was just getting the counts for item[i] in listrank== 1 and listrank== 2.  This didn't actually specify that subject[j] was recalling item[i] in *both* trial 1 and 2.  By listing the ids for listranks 1 and 2 respectively, you can use intersect to find the overlapping id's and just grab the length for items recalled in both the first and second fluency trial.  

############# SPOTCHECK EXAMPLE  SINGLE ITEM ################
#  # Get counts for rp fluency trial 1
# idl1 <- fruit[listrank== 1 & item== "kiwi"]$id
#  # Get counts for rp fluency trial 2
# idl2 <- fruit[listrank== 2 & item== "kiwi"]$id
# # 
# # # Find overlapping id's between trial 1 and 2 for item i
# length(intersect(idl1,idl2))

# This works and adds up correctly

# Grab all items generated from list 2
fruit_items= unique(fruit[listrank== 2,item])
 t2t1= vector()
for (i in 1:length(fruit_items)){
  # Get counts for rp fluency trial 1
  idl1 <- fruit[listrank== 1 & item== fruit_items[i]]$id
  # Get counts for rp fluency trial 2
  idl2 <- fruit[listrank== 2 & item== fruit_items[i]]$id
  # Take the length of overlapping id's between trials 1 & 2 for item i
  int1t2[i] <- length(intersect(idl1,idl2))
  t2[i] <- sum(fruit[listrank== 2 & item== fruit_items[i], .N, by= id]$N>0)
  t2t1[i]= idl2
}

# Super messy, will clean up
sf2= (t2-1)/length(nsubj)
pfruit= data.table()
pfruit[, sf2:= sf2]

pfruit[, sf2sf1:= int1t2/length(t2t1)]




ggplot(data= data.frame(pfruit),aes(x= sf2, y=sf2sf1))+ geom_count(alpha= 0.5)+ geom_abline(slope= 1, intercept= 0)+ geom_smooth(method= "lm")


ggplot(data= data.frame(pfruit), aes(x= sf2, y= sf2sf1))+ geom_point()+ stat_identity(geom= "line" )



############################ Do calculation for vegetables
vt2 = vector()
veg_items= unique(veg[listrank== 2,item])
for (i in 1:length(veg_items)){
  # Get counts for rp fluency trial 1
  vidl1 <- veg[listrank== 1 & item== veg_items[i]]$id
  # Get counts for rp fluency trial 2
  vidl2 <- veg[listrank== 2 & item== veg_items[i]]$id
  # Take the length of overlapping id's between trials 1 & 2 for item i
  int1t2[i] <- length(intersect(vidl1,vidl2))
  vt2[i] <- sum(veg[listrank== 2 & item== veg_items[i], .N, by= id]$N>0)
}


vf2= (vt2-1)/length(nsubj)
pveg= data.table()
pveg[, sf2:= vf2]
pveg[, sf2sf1:= int1t2/length(vidl1)]

############################ Do calculation for Animals
at2 = vector()
animal_items= unique(animal[listrank== 2,item])
for (i in 1:length(animal_items)){
  # Get counts for rp fluency trial 1
  vidl1 <- animal[listrank== 1 & item== animal_items[i]]$id
  # Get counts for rp fluency trial 2
  vidl2 <- animal[listrank== 2 & item== animal_items[i]]$id
  # Take the length of overlapping id's between trials 1 & 2 for item i
  int1t2[i] <- length(intersect(vidl1,vidl2))
  at2[i] <- sum(animal[listrank== 2 & item== animal_items[i], .N, by= id]$N>0)
}


af2= (at2-1)/length(nsubj)
panimal= data.table()
panimal[, sf2:= af2]
panimal[, sf2sf1:= int1t2/length(vidl1)]

p_all = data.table()
p_all[, sf2: c(pfruit$sf2, pveg$sf2, panimal$sf2)]

lm()

# Plot Probabilities
ggplot(data= pfruit) +geom_count(aes(x= pfruit$sf2, y= pfruit$sf2sf1, color= "Fruit"), alpha= 0.5)+ labs(x= "Typicality in SF 2", y= "p(SF2|SF1)", size= "Frequency", color= "Category")+ geom_smooth(aes(x= pfruit$sf2, y= pfruit$sf2sf1), method= "lm") 


rownames(pfruit, do.NULL = TRUE, prefix= "row")
row.names(nsubj)

+ geom_count(aes(x= pveg$sf2, y= pveg$sf2sf1, color= "Vegetable"), alpha= 0.5) + geom_count(aes(x= panimal$sf2, y= panimal$sf2sf1, color= "Animal"), alpha= 0.5)+ geom_abline()+ labs(x= "Typicality in SF 2", y= "p(SF2|SF1)", size= "Frequency", color= "Category")+ geom_smooth(aes(x= pfruit$sf2, y= pfruit$sf2sf1), method= "lm")

ggplot(data= pfruit, aes(x= sf2, y= sf2sf1))

