library(data.table)
if (getwd()!= "~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/logs"){
  setwd("~/Desktop/Desktop - Rebecca’s MacBook Air/Research 2021-2022/GitHub/Wilder-Research-ZemlaLab/fluencytask_rebeccascopy/")
}
dat <- data.table(read.csv("final_results.csv"))


# dat<- dat[group== "Experiment1"]
# dat[listnum<3, listrank:= 1]
# dat[listnum>=3 & listnum <6, listrank := 2]
# dat[listnum>=6, listrank := 3]

# dat<- dat[listrank<3,]
nsubj <- unique(dat$id)
ncat <- unique(dat$category)
dat[, listrank:= 0]

for (subject in nsubj){
  for (cats in ncat){
    this_game <- dat[id== subject & category== cats, game]
    dat[id== subject & category== cats, listnum:= this_game== max(this_game)]
  }
}

dat[listnum== "FALSE", listrank:= 1]
dat[listnum == "TRUE", listrank:= 2]
subset(dat,  select = -c(X))

dat<- subset(dat,select=-c(listnum))



# Set up everything for function
dat[, both_trials := 0]




  for (subject in nsubj){
    for (cats in unique(dat$category)){
      this_subj <- dat[id== subject & category== cats,]
      repeated_words <- intersect(this_subj[listrank==2,item], this_subj[listrank==1,item])
      this_subj[, both_trials:= 0]
      this_subj[item %in% repeated_words, both_trials:=1]
      dat[id== subject & category == cats]$both_trials <- this_subj$both_trials

    }
  }


cont_transitions <- data.table(id= character(), category= character(), word= character(), contiguous= numeric())
for (subject in nsubj){
  for (cats in (unique(dat$category))){
    idx_oldold <- 0
    this_subj <- dat[id== subject & category== cats & listrank== 2,]
    for (item in 2:length(this_subj$item)-1){
      if(this_subj$both_trials[item]== 1 & this_subj$both_trials[item+1]== 1){
        idx_oldold <- 1
        print(idx_oldold)
        }else{
          idx_oldold <- 0
          }
            newlist <- list(subject, cats, this_subj$item[item], idx_oldold)
            cont_transitions <- rbind(cont_transitions, newlist)
            }
          }
        }

word_dist <- vector()
for (subject in nsubj){
  for (cats in unique(dat$category)){
    this_subj <- dat[id== subject & category== cats & listrank== 1]
    for (i in 1:length(unique(cont_transitions[id== subject, category== cats]$word))){
      
    }
  }
}


this_subj <- dat[id== subject & category == cats & listrank == 2]

this_subj
sp = 1:length(this_subj$both_trials)
bt= this_subj$both_trials
words= this_subj$item
pt = dat[id== subject & category == cats & listrank == 1]
sp1 = 1:length(pt$both_trials)

bt1 = pt$both_trials
words1 = pt$words

sp[bt== 0] <- NaN
words[bt== 0] <- 0


# which(a== b)
# > a
# [1] 4 5 6 7
# > b
# [1] 1 6 4 3 9
# >
#   > which(a==6)
# [1] 3
# > which(a==4)
# [1] 1
# > a<-c("apple)
# +
# > a<-c("apple", "banana", "kiwi")
# > b<-c("kiwi", "banana", "mango")
# > which(a == "kiwi")
# [1] 3
# > which(a == "kiwi") Gives you index- if there are any repetitions 

for (i in 2:length(bt)-1){
  
  
}

that_subj <- dat[id== subject & category== cats & listrank== 1]


that_subj[which(this_subj$item[1]== that_subj$item)]

perseverative <- data.table(id= character(), category= character(), errors= numeric())
for (subject in nsubj){
  for (cats in ncat){
    getcount <- 0
    this_subj <- dat[id== subject & category == cats & listrank== 1]
    for (word in unique(this_subj$item)){
      if (sum(this_subj$item %in% word)>1){
        getcount= getcount+ 1
      }
      newrow= list(subject, cats, getcount)
      perseverative= rbind(perseverative, newrow)
    }
  }
}
  

that_subj <- dat[id== subject & category== cats & listrank== 1]
this_subj <- dat[id== subject & category== cats & listrank== 2]
thatword <- unique(that_subj$item)
thisword <- unique(this_subj$item)


all_transitions <- dat
all_transitions[, transition_dist:= NaN]
transition_dist <- vector()

this


for (subject in nsubj){
  for(cats in ncat){
    for (word in 2:length(thisword)-1){
      if(sum(this_subj$both_trials[word], this_subj$both_trials[word+1])==2){
        all_transitions[id== subject & category== cats & item== item[word] <- which(thatword %in% thisword[word])]
      } else {
        all_transitions[id== subject & category== cats & item== item[word]] <- NaN
        print(sum(this_subj$both_trials[word], this_subj$both_trials[word+1])==2)
      }
      if(length(transition_dist[is.na(transition_dist)== length(transition_dist)])){
        print(sum(this_subj$both_trials[word], this_subj$both_trials[word+1])==2)
      }
    }
  }
}

    
sum(is.na(all_transitions$x))
sum(dat[listrank== 2]$both_trials==0)

all_transitions

# change transition dist to sp
all_transitions <- dat
all_transitions[, transition_dist:= NaN]
transition_dist <- vector()


for (subject in nsubj){
  for (cats in ncat){
    this_subj <- dat[id== subject & category== cats & listrank == 2]
    that_subj <- dat[id== subject & category== cats & listrank== 1]
    that_word <- unique(that_subj$item)
    this_word <- unique(this_subj$item)
    for (word in unique(this_subj[both_trials==1]$item)){
      if (subject== nsubj[26] & cats== ncat[12]){
        print(which(that_word %in% word))
      }
      all_transitions[id== subject & category== cats & item== word]$transition_dist <- which(that_word %in% word)
    }
  }
}

# Alternative way to do this ^ 
# dat[word==word, itemnum]
all_transitions

# sanity check with small example
# vector of serial positions serial positions
transition_dist <- all_transitions[id== subject & category == cats & listrank== 2]$transition_dist



a_transition_list <- data.table()
for (subject in nsubj){
  for(cats in ncat){
    sample_vector <- vector()
    this_subject <- all_transitions[id== subject & category == cats & listrank== 2 & both_trials==1]
    # get transitions
    for (i in 2:length(this_subject$transition_dist)-1){
      sample_vector[i] <- this_subject$transition_dist[i] - this_subject$transition_dist[i+1]
    }
    newlist <- list(rep(subject,length(sample_vector)),rep(cats,length(sample_vector)), sample_vector)
    a_transition_list<- rbind(a_transition_list, newlist, use.names= FALSE)
  }
}

colnames(a_transition_list) <- c("id", "category", "transition")

all_counts <- data.table(id= character(), category= character(), words= character(), transition_type= numeric(), counts= numeric(), get_length= numeric())


# Standardizes length of N possible transitions across subjects (because largest smallest transitions are going to vary from person to person)
transition_type <- unique(a_transition_list$transition)

#Get probability of transitions
for (subject in nsubj){
  for (cats in ncat){
    this_subj <- a_transition_list[id== subject & category== cats]
    get_counts <- vector()
    get_length <- vector()
    get_words <- vector()
    transition_types <- vector()
    for (i in 1:length(transition_type)){
      this_transition <- unique(a_transition_list$transition[i])
      get_counts[i] <- sum(this_subj$transition %in% this_transition)
      get_length[i] <- length(this_subj$transition)
      # get_words[i] <- this_subj[i]$item
    }
    newrow <- list(rep(subject, length(get_counts)), rep(cats, length(get_counts)), transition_type, get_counts, get_length)
    all_counts <- rbind(all_counts, newrow)
  }
}


# Sort by category and transition type to make things easier
all_counts <- all_counts[order(category, id)]
all_counts[, lag_crp:= counts/get_length]

new_counts = all_counts[, mean(lag_crp), by= transition_type]

ggplot(data= new_counts, aes(x= transition_type, y= V1)) + geom_line()+ ylim(0,1)



# go through one or 2 lists on UPenn Page do the CRP tutorial on the website
# Do it as a for loop and by hand 
# go through and use their numbers as well just to make sure that you don't have any mistakes
# List comprehensions in python-- more intuitive to work with in python


# from wikipedia2vec import Wikipedia2Vec
# vec= Wikipedia2Vec.load('enwiki_2018_100d.pk1')
# def similarity(a,b,vec):
# return 1-spatial.distrance 




# set up trials 1 and 2 

sf2= dat[id== subject & category== cats & listrank==2]
words2= unique(sf2$item)
sf1= dat[id== subject & category== cats & listrank==1]
words1= unique(sf1$item)
sp <- vector()

l2 = sf2$item
l1= sf1$item
lag= vector()
for (i in 2:length(l2)-1){
  idx1 = which(l1== l2[i])
  idx2= which(l1== l2[i+1])
  if(!is.na(idx1) & !is.na(idx2)){
    lag <- c(lag, (idx2-idx1))
  }
}
  
  
  
  
  
for (i in 1:length(words2)){
  if(words2[i] %in% words1){
    sp[i] = which(words1 %in% words2[i])
  }else{
    sp[i] = 0
    }
}


transitions= vector()

for (i in 2:length(sp)-1){
  transitions[i] = sp[i+1]- sp[i]
}

get_counts= vector()
transitions_range = min(transitions):max(transitions)
for (i in 1:length(transitions_range)){
  get_counts[i]= sum(transitions %in% transitions_range[i])
  # print(get_counts)
}
# table(transitions)
dat_transitions <- data.table(transition_range= transitions_range, transition_counts= get_counts)
possible_counts = vector()
# temporary serial positions that you can change in the for loop 
temp_sp= sp
prior_transitions = vector()
for (i in 2:length(transition_range)-1){
  transition_range[i]- length(transition_range)
}








