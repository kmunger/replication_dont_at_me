##load in anonymized tweets and code them for different thresholds of aggression
setwd("C:/Users/kevin/Documents/GitHub/replication_dont_at_me/")


##read in subject information
load(file = "subjects_active_anonymized.RData")

library(dplyr)


##read in tweet data

load(file = "data_active_anonymized.RData")


###set the threshold for aggressive tweet 


agg_thresholds<-quantile(data_active_anonymized$aggression[data_active_anonymized$days_after <0], 
                         probs = c(.7, .75, .8, .85, .9, .95))



##plot distribution of aggression scores and the 70th percentile line
pdf(file = "results/distribution_aggression_scores_subjects.pdf", 8 , 5)

hist(data_active_anonymized$aggression, main = "Aggression Scores in Subject Tweets (@-replies only)",
     ylab = "Number of Tweets", xlab = "Aggression Score" )
abline( v= agg_thresholds[ 1], col = "red")
dev.off()





###this is for the number of tweets above a threshold
subjects_active_anonymized$pre_total<-rep(0, length(subjects_active_anonymized$treatment))
subjects_active_anonymized$post_total<-rep(0, length(subjects_active_anonymized$treatment))

subjects_active_anonymized$pre_aggtweets<-rep(0, length(subjects_active_anonymized$treatment))
subjects_active_anonymized$post_aggtweets_1<-rep(0, length(subjects_active_anonymized$treatment))
subjects_active_anonymized$post_aggtweets_3<-rep(0, length(subjects_active_anonymized$treatment))
subjects_active_anonymized$post_aggtweets_7<-rep(0, length(subjects_active_anonymized$treatment))
subjects_active_anonymized$post_aggtweets_14<-rep(0, length(subjects_active_anonymized$treatment))
subjects_active_anonymized$post_aggtweets_28<-rep(0, length(subjects_active_anonymized$treatment))
subjects_active_anonymized$post_aggtweets_43<-rep(0, length(subjects_active_anonymized$treatment))

##non-overlapping time periods
subjects_active_anonymized$post_aggtweets_7_ex<-rep(0, length(subjects_active_anonymized$treatment))
subjects_active_anonymized$post_aggtweets_14_ex<-rep(0, length(subjects_active_anonymized$treatment))
subjects_active_anonymized$post_aggtweets_28_ex<-rep(0, length(subjects_active_anonymized$treatment))
subjects_active_anonymized$post_aggtweets_43_ex<-rep(0, length(subjects_active_anonymized$treatment))

#####70th percentile 

##check to make sure there aren't any ppl who don't have any relevant tweets
  zero_replies_strong<-character()
  zero_replies_strong_num<-vector()
  for(i in 1:length(subjects_active_anonymized$anon_id)){
    
    apropos<-filter(data_active_anonymized, anon_id==subjects_active_anonymized$anon_id[i])
    if(length(apropos$anon_id)==0){
      ##listing ppl with no relevant tweets 
      zero_replies_strong<-c(zero_replies_strong, subjects_active_anonymized$anon_id[i])
      zero_replies_strong_num<-c(zero_replies_strong_num, i)
      
      next
    }
    ##############################################counts of aggressive tweets -- 70th percentile
    #before
    pre<-filter(apropos, days_after <0 & days_after > -90)
    subjects_active_anonymized$pre_aggtweets[i]<- length(which(pre$aggression > agg_thresholds[1]))
    subjects_active_anonymized$pre_total[i]<- length((pre$aggression))
    
    #1 day after
    post1<-filter(apropos, days_after ==1)
    subjects_active_anonymized$post_aggtweets_1[i]<-length(which(post1$aggression > agg_thresholds[1]))
    
    #3 days after
    post3<-filter(apropos, days_after > 0 & days_after < 4)
    subjects_active_anonymized$post_aggtweets_3[i]<-length(which(post3$aggression > agg_thresholds[1]))
    
    #7 days after
    post7<-filter(apropos, days_after > 0 & days_after< 8)
    subjects_active_anonymized$post_aggtweets_7[i]<-length(which(post7$aggression > agg_thresholds[1]))
    
    #7 days after exclusive
    post7_ex<-filter(apropos, days_after > 1 & days_after< 8)
    subjects_active_anonymized$post_aggtweets_7_ex[i]<-length(which(post7_ex$aggression > agg_thresholds[1]))
    
    #14 days after
    post14<-filter(apropos, days_after > 0 & days_after< 15)
    subjects_active_anonymized$post_aggtweets_14[i]<-length(which(post14$aggression > agg_thresholds[1]))
    
    #14 days after exclusive
    post14_ex<-filter(apropos, days_after > 7 & days_after< 15)
    subjects_active_anonymized$post_aggtweets_14_ex[i]<-length(which(post14_ex$aggression > agg_thresholds[1]))
    
    #28 days after
    post28<-filter(apropos, days_after > 0 & days_after< 29)
    subjects_active_anonymized$post_aggtweets_28[i]<-length(which(post28$aggression > agg_thresholds[1]))
    
    #43 days after
    post43<-filter(apropos, days_after > 0 & days_after< 44)
    subjects_active_anonymized$post_aggtweets_43[i]<-length(which(post43$aggression > agg_thresholds[1]))
    subjects_active_anonymized$post_total[i]<- length((post43$aggression))
    
    
    #28 days after exclusive
    post28_ex<-filter(apropos, days_after > 14 & days_after< 29)
    subjects_active_anonymized$post_aggtweets_28_ex[i]<-length(which(post28_ex$aggression > agg_thresholds[1]))
    
    #43 days after exclusive
    post43_ex<-filter(apropos, days_after > 28 & days_after< 44)
    subjects_active_anonymized$post_aggtweets_43_ex[i]<-length(which(post43_ex$aggression > agg_thresholds[1]))
    
    
    
  
  }
  
  
  ##make these things into factors
  
  subjects_active_anonymized$f.treatment<-factor(subjects_active_anonymized$treatment)
  subjects_active_anonymized$f.candidate<-factor(subjects_active_anonymized$candidate)
  


  ##save subjects files 
  
  
  save(subjects_active_anonymized, file = "subjects_70th.RData")
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###################  ###################  ###################  ###################  ###################
  ####calculate proporations in Table 1
  ###################  ###################  ###################  ###################  ###################
  
  
  #pre-treatment total
  summary(subjects_active_anonymized$pre_total)
  
  
  
  # pre-treatment uncivil
  summary(subjects_active_anonymized$pre_aggtweets)
  
  
  #post-treatment total
  summary(subjects_active_anonymized$post_total)
  
  #post-treatment uncivil
  summary(subjects_active_anonymized$post_aggtweets_43)
  
  #pre-treatment uncivil by partisanship
  summary(subjects_active_anonymized$pre_aggtweets[subjects_active_anonymized$rightist==1])
  summary(subjects_active_anonymized$pre_aggtweets[subjects_active_anonymized$leftist==1])  
  
  
  

  ###################  ###################  ###################  ###################  ###################
  ### civil tweets
  
  ###################  ###################  ###################  ###################  ###################
  
  ###this is for the number of tweets above a threshold
  subjects_active_anonymized$pre_civiltweets<-rep(0, length(subjects_active_anonymized$treatment))
  subjects_active_anonymized$post_civiltweets_1<-rep(0, length(subjects_active_anonymized$treatment))
  subjects_active_anonymized$post_civiltweets_3<-rep(0, length(subjects_active_anonymized$treatment))
  subjects_active_anonymized$post_civiltweets_7<-rep(0, length(subjects_active_anonymized$treatment))
  subjects_active_anonymized$post_civiltweets_14<-rep(0, length(subjects_active_anonymized$treatment))
  subjects_active_anonymized$post_civiltweets_28<-rep(0, length(subjects_active_anonymized$treatment))
  subjects_active_anonymized$post_civiltweets_43<-rep(0, length(subjects_active_anonymized$treatment))
  
  ##non-overlapping time periods
  subjects_active_anonymized$post_civiltweets_7_ex<-rep(0, length(subjects_active_anonymized$treatment))
  subjects_active_anonymized$post_civiltweets_14_ex<-rep(0, length(subjects_active_anonymized$treatment))
  subjects_active_anonymized$post_civiltweets_28_ex<-rep(0, length(subjects_active_anonymized$treatment))
  subjects_active_anonymized$post_civiltweets_43_ex<-rep(0, length(subjects_active_anonymized$treatment))
  

  
  ##check to make sure there aren't any ppl who don't have any relevant tweets
  zero_replies_strong<-character()
  zero_replies_strong_num<-vector()
  for(i in 1:length(subjects_active_anonymized$anon_id)){
    
    apropos<-filter(data_active_anonymized, anon_id==subjects_active_anonymized$anon_id[i])
    if(length(apropos$anon_id)==0){
      ##listing ppl with no relevant tweets 
      zero_replies_strong<-c(zero_replies_strong, subjects_active_anonymized$anon_id[i])
      zero_replies_strong_num<-c(zero_replies_strong_num, i)
      
      next
    }
    
    ##############################################counts of aggressive tweets -- 70th percentile
    #before
    pre<-filter(apropos, days_after <0 & days_after > -90)
    subjects_active_anonymized$pre_civiltweets[i]<- length(which(pre$aggression < agg_thresholds[1]))
    
    #1 day after
    post1<-filter(apropos, days_after ==1)
    subjects_active_anonymized$post_civiltweets_1[i]<-length(which(post1$aggression < agg_thresholds[1]))
    
    #3 days after
    post3<-filter(apropos, days_after > 0 & days_after < 4)
    subjects_active_anonymized$post_civiltweets_3[i]<-length(which(post3$aggression < agg_thresholds[1]))
    
    #7 days after
    post7<-filter(apropos, days_after > 0 & days_after< 8)
    subjects_active_anonymized$post_civiltweets_7[i]<-length(which(post7$aggression < agg_thresholds[1]))
    
    #7 days after exclusive
    post7_ex<-filter(apropos, days_after > 1 & days_after< 8)
    subjects_active_anonymized$post_civiltweets_7_ex[i]<-length(which(post7_ex$aggression < agg_thresholds[1]))
    
    #14 days after
    post14<-filter(apropos, days_after > 0 & days_after< 15)
    subjects_active_anonymized$post_civiltweets_14[i]<-length(which(post14$aggression < agg_thresholds[1]))
    
    #14 days after exclusive
    post14_ex<-filter(apropos, days_after > 7 & days_after< 15)
    subjects_active_anonymized$post_civiltweets_14_ex[i]<-length(which(post14_ex$aggression < agg_thresholds[1]))
    
    #28 days after
    post28<-filter(apropos, days_after > 0 & days_after< 29)
    subjects_active_anonymized$post_civiltweets_28[i]<-length(which(post28$aggression < agg_thresholds[1]))
    
    #43 days after
    post43<-filter(apropos, days_after > 0 & days_after< 44)
    subjects_active_anonymized$post_civiltweets_43[i]<-length(which(post43$aggression < agg_thresholds[1]))
    
    #28 days after exclusive
    post28_ex<-filter(apropos, days_after > 14 & days_after< 29)
    subjects_active_anonymized$post_civiltweets_28_ex[i]<-length(which(post28_ex$aggression < agg_thresholds[1]))
    
    #43 days after exclusive
    post43_ex<-filter(apropos, days_after > 28 & days_after< 44)
    subjects_active_anonymized$post_civiltweets_43_ex[i]<-length(which(post43_ex$aggression < agg_thresholds[1]))
    
    
    
    
  }
  
  
  ##make these things into factors
  
  subjects_active_anonymized$f.treatment<-factor(subjects_active_anonymized$treatment)
  subjects_active_anonymized$f.candidate<-factor(subjects_active_anonymized$candidate)
  
  
  
  ##save subjects files 
  
  
  save(subjects_active_anonymized, file = "subjects_civil.RData")
  
  

  
  
  
  
  
  
  
  
  ###################  ###################  ###################  ###################  ###################
  ###################for appendix data
  
  ###this is for the number of tweets above a threshold
  subjects_active_anonymized$pre_aggtweets<-rep(0, length(subjects_active_anonymized$treatment))
  subjects_active_anonymized$post_aggtweets_1<-rep(0, length(subjects_active_anonymized$treatment))
  subjects_active_anonymized$post_aggtweets_3<-rep(0, length(subjects_active_anonymized$treatment))
  subjects_active_anonymized$post_aggtweets_7<-rep(0, length(subjects_active_anonymized$treatment))
  subjects_active_anonymized$post_aggtweets_14<-rep(0, length(subjects_active_anonymized$treatment))
  subjects_active_anonymized$post_aggtweets_28<-rep(0, length(subjects_active_anonymized$treatment))
  subjects_active_anonymized$post_aggtweets_43<-rep(0, length(subjects_active_anonymized$treatment))
  
  ##non-overlapping time periods
  subjects_active_anonymized$post_aggtweets_7_ex<-rep(0, length(subjects_active_anonymized$treatment))
  subjects_active_anonymized$post_aggtweets_14_ex<-rep(0, length(subjects_active_anonymized$treatment))
  subjects_active_anonymized$post_aggtweets_28_ex<-rep(0, length(subjects_active_anonymized$treatment))
  subjects_active_anonymized$post_aggtweets_43_ex<-rep(0, length(subjects_active_anonymized$treatment))
  
  #####90th percentile 
  
  
  ##check to make sure there aren't any ppl who don't have any relevant tweets
  zero_replies_strong<-character()
  zero_replies_strong_num<-vector()
  for(i in 1:length(subjects_active_anonymized$anon_id)){
    
    apropos<-filter(data_active_anonymized, anon_id==subjects_active_anonymized$anon_id[i])
    if(length(apropos$anon_id)==0){
      ##listing ppl with no relevant tweets 
      zero_replies_strong<-c(zero_replies_strong, subjects_active_anonymized$anon_id[i])
      zero_replies_strong_num<-c(zero_replies_strong_num, i)
      
      next
    }
    
    ##############################################counts of aggressive tweets -- 90th percentile
    #before
    pre<-filter(apropos, days_after <0 & days_after > -90)
    subjects_active_anonymized$pre_aggtweets[i]<- length(which(pre$aggression > agg_thresholds[5]))
    
    #1 day after
    post1<-filter(apropos, days_after ==1)
    subjects_active_anonymized$post_aggtweets_1[i]<-length(which(post1$aggression > agg_thresholds[5]))
    
    #3 days after
    post3<-filter(apropos, days_after > 0 & days_after < 4)
    subjects_active_anonymized$post_aggtweets_3[i]<-length(which(post3$aggression > agg_thresholds[5]))
    
    #7 days after
    post7<-filter(apropos, days_after > 0 & days_after< 8)
    subjects_active_anonymized$post_aggtweets_7[i]<-length(which(post7$aggression > agg_thresholds[5]))
    
    #7 days after exclusive
    post7_ex<-filter(apropos, days_after > 1 & days_after< 8)
    subjects_active_anonymized$post_aggtweets_7_ex[i]<-length(which(post7_ex$aggression > agg_thresholds[5]))
    
    #14 days after
    post14<-filter(apropos, days_after > 0 & days_after< 15)
    subjects_active_anonymized$post_aggtweets_14[i]<-length(which(post14$aggression > agg_thresholds[5]))
    
    #14 days after exclusive
    post14_ex<-filter(apropos, days_after > 7 & days_after< 15)
    subjects_active_anonymized$post_aggtweets_14_ex[i]<-length(which(post14_ex$aggression > agg_thresholds[5]))
    
    #28 days after
    post28<-filter(apropos, days_after > 0 & days_after< 29)
    subjects_active_anonymized$post_aggtweets_28[i]<-length(which(post28$aggression > agg_thresholds[5]))
    
    #43 days after
    post43<-filter(apropos, days_after > 0 & days_after< 44)
    subjects_active_anonymized$post_aggtweets_43[i]<-length(which(post43$aggression > agg_thresholds[5]))
    
    #28 days after exclusive
    post28_ex<-filter(apropos, days_after > 14 & days_after< 29)
    subjects_active_anonymized$post_aggtweets_28_ex[i]<-length(which(post28_ex$aggression > agg_thresholds[5]))
    
    #43 days after exclusive
    post43_ex<-filter(apropos, days_after > 28 & days_after< 44)
    subjects_active_anonymized$post_aggtweets_43_ex[i]<-length(which(post43_ex$aggression > agg_thresholds[5]))
    
    
    
    
  }
  
  
  ##make these things into factors
  
  subjects_active_anonymized$f.treatment<-factor(subjects_active_anonymized$treatment)
  subjects_active_anonymized$f.candidate<-factor(subjects_active_anonymized$candidate)
  
  
  
  ##save subjects files 
  
  
  save(subjects_active_anonymized, file = "subjects_90th.RData")
  
  
  
  