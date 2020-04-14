##figures 8
setwd("C:/Users/kevin/Documents/GitHub/replication_dont_at_me/")


##read in subject information
load(file = "subjects_70th.RData")
library(plyr)

library(dplyr)


###create logs of all variables
subjects_active_anonymized$pre_aggtweets<- log(subjects_active_anonymized$pre_aggtweets +.5)
subjects_active_anonymized$post_aggtweets_1<- log(subjects_active_anonymized$post_aggtweets_1 +.5)
subjects_active_anonymized$post_aggtweets_3<- log(subjects_active_anonymized$post_aggtweets_3 +.5)
subjects_active_anonymized$post_aggtweets_7<- log(subjects_active_anonymized$post_aggtweets_7 +.5)
subjects_active_anonymized$post_aggtweets_14<- log(subjects_active_anonymized$post_aggtweets_14 +.5)
subjects_active_anonymized$post_aggtweets_28<- log(subjects_active_anonymized$post_aggtweets_28 +.5)
subjects_active_anonymized$post_aggtweets_43<- log(subjects_active_anonymized$post_aggtweets_43 +.5)

subjects_active_anonymized$post_aggtweets_7_ex<- log(subjects_active_anonymized$post_aggtweets_7_ex +.5)
subjects_active_anonymized$post_aggtweets_14_ex<- log(subjects_active_anonymized$post_aggtweets_14_ex +.5)
subjects_active_anonymized$post_aggtweets_28_ex<- log(subjects_active_anonymized$post_aggtweets_28_ex +.5)
subjects_active_anonymized$post_aggtweets_43_ex<- log(subjects_active_anonymized$post_aggtweets_43_ex +.5)




subjects_active_anonymized$treatment_<- revalue(subjects_active_anonymized$f.treatment, c("0"="Control", "1"="Authority", "2" = "Care", "3" = "Public"))



##divide Republicans and Democrats 
republicans<-filter(subjects_active_anonymized, rightist == 1)
democrats<-filter(subjects_active_anonymized, leftist == 1)



##democrats first
full_1<-(lm(post_aggtweets_1 ~ pre_aggtweets + treatment_*anonymity, data = subjects_active_anonymized))

full_7<-(lm(post_aggtweets_7_ex ~ pre_aggtweets + treatment_*anonymity, data = subjects_active_anonymized))

full_14<-(lm(post_aggtweets_14_ex ~ pre_aggtweets + treatment_*anonymity, data = subjects_active_anonymized))

full_28<-(lm(post_aggtweets_28_ex ~ pre_aggtweets + treatment_*anonymity, data = subjects_active_anonymized))

library(interplot)


  p <- interplot(m = full_7, var1 = "treatment_" , var2 = "anonymity") +
  
              #theme(panel.grid.major = element_blank(),
              # panel.grid.minor = element_blank(),
              #panel.background = element_blank(),
              #axis.line = element_line(colour = "black")) +

    #geom_smooth(alpha=0.2, linetype=1,  method = "loess", span = .34) +
 geom_hline(yintercept = 0, linetype = 2) +
    
    xlab("Subject Anonymity (0 = Full Bio, 2 = Fully Anonymous)") +
    ylab("Change in Incivil Tweets, Relative to Control") +

    ggtitle("Treatment Effects By Subject Anonymity, Week 1") +
    theme(plot.title = element_text(hjust=.5, size = 20), legend.text=element_text(size=20), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title=element_text(size=20),
          axis.line = element_line(colour = "black")) 

  print(p)
  
  pdf(file="results/interplot_week1.pdf", height=7, width=12)
  print(p)
  dev.off()
  
  
  p <- interplot(m = full_1, var1 = "treatment_" , var2 = "anonymity") +
    
    #theme(panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    #axis.line = element_line(colour = "black")) +
    
    #geom_smooth(alpha=0.2, linetype=1,  method = "loess", span = .34) +
    geom_hline(yintercept = 0, linetype = 2) +
    
    xlab("Subject Anonymity (0 = Full Bio, 2 = Fully Anonymous)") +
    ylab("Change in Incivil Tweets, Relative to Control") +
    
    ggtitle("Treatment Effects By Subject Anonymity, Day 1") +
    theme(plot.title = element_text(hjust=.5, size = 20), legend.text=element_text(size=20), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title=element_text(size=20),
          axis.line = element_line(colour = "black")) 
  
  print(p)
  
  pdf(file="results/interplot_day1.pdf", height=7, width=12)
  print(p)
  dev.off()
  
  