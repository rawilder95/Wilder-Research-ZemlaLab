if (getwd()!= "/Users/rebeccawilder/Desktop/Research 2021-2022"){
  setwd("~/Desktop//Research 2021-2022")
}

# install.packages("data.table")
library(data.table)
library(ggplot2)

dat <- data.table(read.csv("snafu_sample.csv"))

(dat <- dat[dat[,group == "Experiment1"]])

fruit <- dat[category== "fruits"]
veg <- dat[category== "vegetables"]
animal <- dat[category== "animals"]


fruit[listnum<3, listrank:= 1]
fruit[listnum>=3 & listnum <6, listrank := 2]
fruit[listnum>=6, listrank := 3]

# Get unique values for items that appeared on list 2
items2 <- unique(fruit[listrank== 2,item])
items1 <- unique(fruit[listrank== 1, item])
items3 <- vector()

# Mask out items only recalled in list 2 but not in list 1
for (i in 1:length(items2)){
  if (sum(items1== items2[i])>0){
    items3[i]= items2[i]} else{
      items3[i] = NaN
    }
  }

items1= unique(items1)







items3= unique(items3[items3 != NaN])



# items1and2 <- 
t1 <- vector()
t2 <- vector()
for (i in 1:length(unique(items3))){
    # Take the sum of instances where item[i] was listed in the repeated fluency trial and also generated on first fluency trial.  Because we're sampling from 
  # -1 to express typicality of item appearing on *other subjects'* lists
  t1[i] <- (sum(fruit[listrank==2 & item==items3[i], .N, by= id]$N>0)-1)/length(nsubj)
  # Take the sum of instances where item[i] was generated on second fluency trial
  # -1 to express typicality of item appearing on *other subjects'* lists
  t2[i] <- (sum(fruit[listrank==2 & item==items2[i], .N, by= id]$N>0)-1)/length(nsubj)
}

# Create new col for p(item in SF2| item in SF1)

pfruits <- data.table(rep(0,length(t1)))
pfruits[1:length(t1), sf2:= t2]
pfruits[1:length(t1), sf2sf1:= t1]
pfruits= pfruits[,2:length(pfruits)]



ggplot()+ geom_point(data= pfruits, aes(x= sf2, y = sf2sf1, color= "Fruits"),alpha= 0.5)+ xlim(0,1)+ ylim(0,1)

# ggplot()+ geom_count(data= pfruits, aes(x= sf2, y = sf2sf1, color= "Fruits"),alpha= 0.5)


################## DO THE SAME THING FOR VEGETABLES #################################

veg[listnum<3, listrank:= 1]
veg[listnum>=3 & listnum <6, listrank := 2]
veg[listnum>=6, listrank := 3]

# Get unique values for items that appeared on list 2
items2 <- unique(veg[listrank== 2,item])
items1 <- unique(veg[listrank== 1, item])
items3 <- vector()


# items1and2 <- 
t1 <- vector()
t2 <- vector()
t3 <- vector()



for (i in 1:length(items2)){
  if (sum(items2[i]== veg[listrank==1, item])>0 & sum(items2[i]== veg[listrank==2, item])>0){
    t3[i]= (items2[i]== veg[listrank==1]) + sum(items2[i]== veg[listrank==2])
  } else {
    t3[i]=0
  }
}


sf1= (veg[item== "carrot", .N, by= listrank]$N[1]-1)
sf2= (veg[item== "carrot", .N, by= listrank]$N[2]-1)/length(nsubj)
sf3= sf2/sf1


for (i in 1:length(items2)){
  if (sum(items2[i]== veg[listrank==1, item])>0){
    t1[i]= (sum(items2[i] == veg[listrank== 1, item])-1)
  } else {
    t1[i]=0
  }
}

for (i in 1:length(items2)){
  if (sum(items2[i]== veg[listrank==2, item])>0){
    t2[i]= sum(items2[i] == veg[listrank== 2, item])-1
  } else {
    t2[i]=0
  }
}


for (i in 1:length(unique(items2))){
# Take the sum of instances where item[i] was listed in the repeated fluency trial and also generated on first fluency trial. Listrank< 3 samples from repeated fluency trials 1-2 and saying N> 1 means that the item;   Because we're sampling from 
  # -1 to express typicality of item appearing on *other subjects'* lists
  t1[i] <- sum(veg[listrank<3 & item==items1[i], .N, by= id]$N>1)/sum(veg[listrank==1 & item== items1[i], .N, by=id]$N>0)
  # Take the sum of instances where item[i] was generated on second fluency trial
  # -1 to express typicality of item appearing on *other subjects'* lists
  t2[i] <- (sum(veg[listrank== 2 & item==items2[i], .N, by= id]$N>0)-1)/length(nsubj)
}

# Create new col for p(item in SF2| item in SF1)

pveg <- data.table(rep(0,length(t1)))
pveg[1:length(t1), sf2:= t2]
pveg[1:length(t1), sf2sf1:= t1]
pveg= pveg[,2:length(pveg)]


################################# PLOT IT #################################

ggplot() +geom_count(data= pveg, aes(x= sf2, y= sf2sf1, color= "Vegetables"),alpha= 0.5)+ labs(x= "Item Typicality in SF List 2", y= "p(SF2|SF1)", size= "Frequency", color= "Category")



