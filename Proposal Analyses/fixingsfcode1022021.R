if (getwd()!= "~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/Proposal Analyses"){
  setwd("~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/Proposal Analyses")
}
# Install packages and libraries
# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("cowplot")
# library("cowplot")

# library("data.table")
# library("ggplot2")



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
ggplot(data= data.frame(pfruit), aes(x= sf2, y= sf2sf1, color= "Fruits"))+ geom_count(alpha= 0.3)+ geom_abline() + labs(x= "Item Typicality", y= "p(B|A)", color= "Category", size= "Frequency")+ geom_count(data= data.frame(pveg), aes(x= vf2, y= vf2vf1, color= "Vegetables"), alpha= 0.3)+ geom_count(data= data.frame(panimal), aes(x= af2, y= af2af1, color= "Animals"), alpha= 0.3)+ xlim(0,1)+theme(text=element_text(size= 15))

ggsave("repvtypicalityII.png", device= "png", dpi= 400, height= 5, width= 7)

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
########################### Transitions. It's a Mess.  ##################################
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
  

# LAG CRP Generated Plot


ll= length(l1)
library(ggplot2)
l1 <- c(0.14, 0.145, 0.125, 0.14, 0.21, 0.35, NaN, 0.42, 0.19, 0.18, 0.178, 0.17, 0.18)

list1 <- data.table(l1= l1, x1= seq(round(ll/2)*-1,ll/2), h0= rep(0.1756667, ll))
list1[l1>1]$l1 <- 1
ggplot(data= list1, aes(x= x1, y= l1))+ geom_point(aes(y= l1, color= "CRP"), color= "blue")+ geom_line(aes(y= l1))+ geom_line(aes(y= h0), color= "red")+ labs(x= "Item Lag", y= "CRP")+ ylim(0.1,1)
ggsave("new_contiguity_SF.png", device= "png", dpi= 300)
# p2 <- ggplot(data= list1, aes(x= x1, y= l1))+ geom_point(aes(x= x1, l3))+ geom_line(aes(x= x1, y= l3), color= "blue")+ ylim(0,1)
# # install.packages("cowplot")
# cowplot::plot_grid(p1,p2)

# ll= length(l1)
# library(ggplot2)
# l1 <- c(0.4, 0.46, 0.5, 0.61, 0.8, 0.95, NaN, 0.91, 0.75, 0.55, 0.43, 0.32, 0.33)
# l1 <- l1*0.95
# l2 <- rep(0.5, length(l1))
# l3 <- c(0.250, 0.205, 0.220, 0.383, 0.460, 0.600,  NaN, 0.915, 0.775, 0.510, 0.465, 0.335, 0.350)
# list1 <- data.table(l1= l1*0.8, x1= seq(round(ll/2)*-1,ll/2), h0= l2)
# ggplot(data= list1, aes(x= x1, y= l1))+ geom_point(aes(y= l1, color= "Temporal Proximity"), color= "blue")+ geom_line(aes(y= l1, na.rm= TRUE))+ geom_line(aes(y= h0), color= "red", linejoin= "bevel")+ labs(x= "Item Lag", y= "CRP") + geom_point(aes(x= x1, l3))+ geom_line(aes(x= x1, l3), color= "blue")




ggsave("new_contiguity_SF.png", device= "png", dpi= 300)


 # + scale_x_discrete(labels= c("1"= "-6", "2"= "-5", "3"= "-4", "4"= "-3", "5"= "-2", "6"= "-1", "7"="0", "8"= "1", "9"= "2", "10"= "3", "11"= "4", "12"= "5"))+ labs(y= "CRP", x= "Item Lag")))
                         
                         
                         
#                          scale_x_discrete(breaks= c("-1", "1" , floor(length(l1)/2)), labels= c(-1:floor(length(l1)/2)))
# ))

###### TEMPORAL CLUSTERING FACTOR #########
k <- data.table(1:100)
k[,tf:= sample(100)/100]
k[,sf:= sample(100)/100]
k[,colors:= rep(0, length(k$sf))]
k[sf<0.5 & tf<= 0.5]$colors <- 1
k[sf<0.5 & tf> 0.5]$colors <- 2 
k[sf>=0.5 & tf> 0.5]$colors <- 3 
k[sf>0.5 & tf< 0.5]$colors <- 4

ggplot(data= k, aes(x= sf, y= tf))+ geom_jitter(aes(color= colors), alpha= 0.5, )+ geom_hline(yintercept = 0.5)+geom_vline(xintercept= 0.5)+ labs(y= "Temporal Distance", x= "Semantic Distance")


ggplot()+ geom_point(aes(x= 0.22, y= 1), alpha= 0.5, size= 3)+geom_point(aes(x= 0.39, y= 4), alpha= 0.5, size= 3)+geom_point(aes(x= 0.77, y= 2), alpha= 0.5, size= 3)+ geom_point(aes(x= 0.9, y= 5), alpha= 0.5, size= 3)+ geom_hline(yintercept = 3)+geom_vline(xintercept= 0.5)+ labs(y= "Temporal Distance", x= "Semantic Distance")+ xlim(0,1)+ylim(0,6)+ geom_text(aes(x= c(0.25, 0.75, 0.25, 0.75+0.1), y= c(1, 2, 5, 5)-0.5, label= c("DOG-CAT", "FERRET-GIRAFFE", "WOLF-FOX", "TURTLE-ELEPHANT")), size= 6)+ theme(text=element_text(size= 18))
ggsave("temp_sem1.png", device= "png", dpi= 400, width= 8, height= 6)





y <- c(NaN, 0.1, 0.12, 0.1, 0.12, 0.18, 0.4, NaN, 0.7, 0.3, 0.2, 0.15, 0.13, 0.12, 0.1,0.08)
y2 <- c(0.12, 0.1, 0.11, 0.12, 0.134, 0.19, 0.24, NaN, NaN, 0.22, 0.219, 0.23, 0.2, 0.13, 0.13, 0.1)
ll= length(y)
x <- seq(round(ll/2)*-1+1,round(ll/2))
lagcrp <- data.table(x,y)
ggplot(data= lagcrp, aes(x= x, y= y))+geom_point(shape= 1)+ geom_line()+ ylim(0,0.75)+ xlim(-6,6)+ labs(x= "Item Lag", y= "CRP")+theme(text=element_text(size= 18))
ggsave("contiguity_both.png", dpi= 450, height= 5, width= 7)  

geom_line(aes(x= x, y= y2), color= "red", size= 0.85)+geom_point(aes(x= x, y= y2, color= "SF"), color= "black", shape= 16, size= 2)+ 

# + geom_line(aes(x= x, y= y2), color= "red", size= 0.85)+geom_point(aes(x= x, y= y2, color= "SF"), color= "black", shape= 16, size= 2)+ 
  
  
write.csv(sort(unique(animal_items)), "animal_data1.csv")

