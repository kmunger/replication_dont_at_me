

##figures 12 and 13
setwd("C:/Users/kevin/Documents/GitHub/replication_dont_at_me/")


##read in subject information
load(file = "subjects_70th.RData")

library(dplyr)
library(MASS)


##divide Republicans and Democrats and true Democrats
republicans<-filter(subjects_active_anonymized, rightist == 1)
democrats<-filter(subjects_active_anonymized, leftist == 1)


######################################################################################

## full sample
full_1<-(glm.nb(post_aggtweets_1 ~ pre_aggtweets + f.treatment, data = subjects_active_anonymized))

full_7<-(glm.nb(post_aggtweets_7_ex ~ pre_aggtweets + f.treatment, data = subjects_active_anonymized))

full_14<-(glm.nb(post_aggtweets_14_ex ~ pre_aggtweets + f.treatment, data = subjects_active_anonymized))

full_28<-(glm.nb(post_aggtweets_28_ex ~ pre_aggtweets + f.treatment, data = subjects_active_anonymized))



est <-  coef(full_1)
summ <-  summary(full_1)

##to calculate treatment  

irr1_1<-exp(est[3])
irr2_1<-exp(est[4])
irr3_1<-exp(est[5])

se1<-coef(summ)[3, "Std. Error"]
se2<-coef(summ)[4, "Std. Error"]
se3<-coef(summ)[5, "Std. Error"]



##calculate standard errors

  ##calculate CIs
  
  lb1_1<-exp(est[3]  - qnorm(.975) * se1)
  lb2_1<-exp(est[4] - qnorm(.975) * se2)
  lb3_1<-exp(est[5] - qnorm(.975) * se3)
  ub1_1<-exp(est[3]  + qnorm(.975) * se1)
  ub2_1<-exp(est[4] + qnorm(.975) * se2)
  ub3_1<-exp(est[5] + qnorm(.975) * se3)
  
  ##calculate treatment effects on the first week of treatment
  
  est <-  coef(full_7)
  summ <-  summary(full_7)
  
  ##to calculate treatment  
  
  irr1_7<-exp(est[3])
  irr2_7<-exp(est[4])
  irr3_7<-exp(est[5])
  
  se1<-coef(summ)[3, "Std. Error"]
  se2<-coef(summ)[4, "Std. Error"]
  se3<-coef(summ)[5, "Std. Error"]
  
  
  
  ##calculate standard errors
  
  ##calculate CIs
  
  lb1_7<-exp(est[3]  - qnorm(.975) * se1)
  lb2_7<-exp(est[4] - qnorm(.975) * se2)
  lb3_7<-exp(est[5] - qnorm(.975) * se3)
  ub1_7<-exp(est[3]  + qnorm(.975) * se1)
  ub2_7<-exp(est[4] + qnorm(.975) * se2)
  ub3_7<-exp(est[5] + qnorm(.975) * se3)  
  ##calculate treatment effects on the first two weeks of treatment
  
  est <-  coef(full_14)
  summ <-  summary(full_14)
  
  ##to calculate treatment  
  
  irr1_14<-exp(est[3])
  irr2_14<-exp(est[4])
  irr3_14<-exp(est[5])
  
  se1<-coef(summ)[3, "Std. Error"]
  se2<-coef(summ)[4, "Std. Error"]
  se3<-coef(summ)[5, "Std. Error"]
  
  
  
  ##calculate standard errors
  
  ##calculate CIs
  
  lb1_14<-exp(est[3]  - qnorm(.975) * se1)
  lb2_14<-exp(est[4] - qnorm(.975) * se2)
  lb3_14<-exp(est[5] - qnorm(.975) * se3)
  ub1_14<-exp(est[3]  + qnorm(.975) * se1)
  ub2_14<-exp(est[4] + qnorm(.975) * se2)
  ub3_14<-exp(est[5] + qnorm(.975) * se3)
  ##calculate treatment effects on the first month of treatment
  
  est <-  coef(full_28)
  summ <-  summary(full_28)
  
  ##to calculate treatment  
  
  irr1_28<-exp(est[3])
  irr2_28<-exp(est[4])
  irr3_28<-exp(est[5])
  
  se1<-coef(summ)[3, "Std. Error"]
  se2<-coef(summ)[4, "Std. Error"]
  se3<-coef(summ)[5, "Std. Error"]
  
  
  
  ##calculate standard errors
  
  ##calculate CIs
  
  lb1_28<-exp(est[3]  - qnorm(.975) * se1)
  lb2_28<-exp(est[4] - qnorm(.975) * se2)
  lb3_28<-exp(est[5] - qnorm(.975) * se3)
  ub1_28<-exp(est[3]  + qnorm(.975) * se1)
  ub2_28<-exp(est[4] + qnorm(.975) * se2)
  ub3_28<-exp(est[5] + qnorm(.975) * se3)


  
  ##create plots
  ##feelings 1, rules 2, public 3
  
  

  df_full<-data.frame(coefs = c(irr1_1, irr2_1, irr3_1,
                                irr1_7, irr2_7, irr3_7,
                                irr1_14, irr2_14, irr3_14,
                                irr1_28, irr2_28, irr3_28),
                      lbs_95 = c(lb1_1, lb2_1, lb3_1,
                                lb1_7, lb2_7, lb3_7,
                                lb1_14, lb2_14, lb3_14,
                                lb1_28, lb2_28, lb3_28),
                      ubs_95 = c(ub1_1, ub2_1, ub3_1,
                                ub1_7, ub2_7, ub3_7,
                                ub1_14, ub2_14, ub3_14,
                                ub1_28, ub2_28, ub3_28),
                     week = (as.factor(c(1,1,1,2,2,2,3,3,3,4,4,4))),
                      treatment = (as.factor(c("Care","Authority","Public",
                                                                     "Care","Authority","Public",
                                                                     "Care","Authority","Public",
                                                                     "Care","Authority","Public"))   ))                                            
  
  require(ggplot2)
  require(reshape2)
  cols <- c("purple", "forestgreen", "black")
  ##

  p <- ggplot(data = df_full,  
              #theme(panel.grid.major = element_blank(),
              # panel.grid.minor = element_blank(),
              #panel.background = element_blank(),
              #axis.line = element_line(colour = "black")) +
              aes(x = week, group=treatment ,  colour = treatment)) + #, group = delivery)) +
    
    #geom_smooth(alpha=0.2, linetype=1,  method = "loess", span = .34) +
    geom_point( aes(y = coefs), position = position_dodge(width = .5), size = 4) +
    geom_errorbar(aes(ymax = ubs_95,ymin=lbs_95),position=position_dodge(width = .5),width = 0.5, size = 2) +
    geom_hline(yintercept = 1, linetype = 2) +
    
    scale_colour_manual(values = cols, guide = guide_legend(title = "Treatment"))+
    xlab("Weeks Post-Treatment, Non-Overlapping") +
    ylab("Ratio of Number of Incivil Tweets, Relative to Control") +
    coord_cartesian(ylim = c(.1, 2)) +
    
    ggtitle("Effects on All Subjects") +
    theme(plot.title = element_text(hjust=.5, size = 20), legend.text=element_text(size=20), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_text(size=20),
          
          
          axis.title.y = element_text(size=20),
          axis.title.x = element_text(size=20),
          
          axis.line = element_line(colour = "black")) +
    scale_y_continuous(labels=c("0", "50%", "100%", "150%", "200%"))+
    annotate("text", x=1, y=.1, label= "Day 1", size = 8) +
    annotate("text", x=2, y=.1, label= "Week 1", size = 8) +
    annotate("text", x=3, y=.1, label= "Week 2", size = 8) +
    annotate("text", x=4, y=.1, label= "Weeks 3/4", size = 8) 
  
  print(p)

  pdf(file="results/nb_plot.pdf", height=7, width=12)
  print(p)
  
  
  dev.off()
  
  
  ######################################################################################
  
  ## dems
  full_1<-(glm.nb(post_aggtweets_1 ~ pre_aggtweets + f.treatment, data = democrats))
  
  full_7<-(glm.nb(post_aggtweets_7_ex ~ pre_aggtweets + f.treatment, data = democrats))
  
  full_14<-(glm.nb(post_aggtweets_14_ex ~ pre_aggtweets + f.treatment, data = democrats))
  
  full_28<-(glm.nb(post_aggtweets_28_ex ~ pre_aggtweets + f.treatment, data = democrats))
  
  
  
  est <-  coef(full_1)
  summ <-  summary(full_1)
  
  ##to calculate treatment  
  
  irr1_1<-exp(est[3])
  irr2_1<-exp(est[4])
  irr3_1<-exp(est[5])
  
  se1<-coef(summ)[3, "Std. Error"]
  se2<-coef(summ)[4, "Std. Error"]
  se3<-coef(summ)[5, "Std. Error"]
  
  
  
  ##calculate standard errors
  
  ##calculate CIs
  
  lb1_1<-exp(est[3]  - qnorm(.975) * se1)
  lb2_1<-exp(est[4] - qnorm(.975) * se2)
  lb3_1<-exp(est[5] - qnorm(.975) * se3)
  ub1_1<-exp(est[3]  + qnorm(.975) * se1)
  ub2_1<-exp(est[4] + qnorm(.975) * se2)
  ub3_1<-exp(est[5] + qnorm(.975) * se3)
  
  ##calculate treatment effects on the first week of treatment
  
  est <-  coef(full_7)
  summ <-  summary(full_7)
  
  ##to calculate treatment  
  
  irr1_7<-exp(est[3])
  irr2_7<-exp(est[4])
  irr3_7<-exp(est[5])
  
  se1<-coef(summ)[3, "Std. Error"]
  se2<-coef(summ)[4, "Std. Error"]
  se3<-coef(summ)[5, "Std. Error"]
  
  
  
  ##calculate standard errors
  
  ##calculate CIs
  
  lb1_7<-exp(est[3]  - qnorm(.975) * se1)
  lb2_7<-exp(est[4] - qnorm(.975) * se2)
  lb3_7<-exp(est[5] - qnorm(.975) * se3)
  ub1_7<-exp(est[3]  + qnorm(.975) * se1)
  ub2_7<-exp(est[4] + qnorm(.975) * se2)
  ub3_7<-exp(est[5] + qnorm(.975) * se3)  
  ##calculate treatment effects on the first two weeks of treatment
  
  est <-  coef(full_14)
  summ <-  summary(full_14)
  
  ##to calculate treatment  
  
  irr1_14<-exp(est[3])
  irr2_14<-exp(est[4])
  irr3_14<-exp(est[5])
  
  se1<-coef(summ)[3, "Std. Error"]
  se2<-coef(summ)[4, "Std. Error"]
  se3<-coef(summ)[5, "Std. Error"]
  
  
  
  ##calculate standard errors
  
  ##calculate CIs
  
  lb1_14<-exp(est[3]  - qnorm(.975) * se1)
  lb2_14<-exp(est[4] - qnorm(.975) * se2)
  lb3_14<-exp(est[5] - qnorm(.975) * se3)
  ub1_14<-exp(est[3]  + qnorm(.975) * se1)
  ub2_14<-exp(est[4] + qnorm(.975) * se2)
  ub3_14<-exp(est[5] + qnorm(.975) * se3)
  ##calculate treatment effects on the first month of treatment
  
  est <-  coef(full_28)
  summ <-  summary(full_28)
  
  ##to calculate treatment  
  
  irr1_28<-exp(est[3])
  irr2_28<-exp(est[4])
  irr3_28<-exp(est[5])
  
  se1<-coef(summ)[3, "Std. Error"]
  se2<-coef(summ)[4, "Std. Error"]
  se3<-coef(summ)[5, "Std. Error"]
  
  
  
  ##calculate standard errors
  
  ##calculate CIs
  
  lb1_28<-exp(est[3]  - qnorm(.975) * se1)
  lb2_28<-exp(est[4] - qnorm(.975) * se2)
  lb3_28<-exp(est[5] - qnorm(.975) * se3)
  ub1_28<-exp(est[3]  + qnorm(.975) * se1)
  ub2_28<-exp(est[4] + qnorm(.975) * se2)
  ub3_28<-exp(est[5] + qnorm(.975) * se3)
  
  
  
  ##create plots
  ##feelings 1, rules 2, public 3
  
  
  
  df_full<-data.frame(coefs = c(irr1_1, irr2_1, irr3_1,
                                irr1_7, irr2_7, irr3_7,
                                irr1_14, irr2_14, irr3_14,
                                irr1_28, irr2_28, irr3_28),
                      lbs_95 = c(lb1_1, lb2_1, lb3_1,
                                 lb1_7, lb2_7, lb3_7,
                                 lb1_14, lb2_14, lb3_14,
                                 lb1_28, lb2_28, lb3_28),
                      ubs_95 = c(ub1_1, ub2_1, ub3_1,
                                 ub1_7, ub2_7, ub3_7,
                                 ub1_14, ub2_14, ub3_14,
                                 ub1_28, ub2_28, ub3_28),
                      week = (as.factor(c(1,1,1,2,2,2,3,3,3,4,4,4))),
                      treatment = (as.factor(c("Care","Authority","Public",
                                               "Care","Authority","Public",
                                               "Care","Authority","Public",
                                               "Care","Authority","Public"))   ))                                            
  
  require(ggplot2)
  require(reshape2)
  cols <- c("purple", "forestgreen", "black")
  ##
  
  p <- ggplot(data = df_full,  
              #theme(panel.grid.major = element_blank(),
              # panel.grid.minor = element_blank(),
              #panel.background = element_blank(),
              #axis.line = element_line(colour = "black")) +
              aes(x = week, group=treatment ,  colour = treatment)) + #, group = delivery)) +
    
    #geom_smooth(alpha=0.2, linetype=1,  method = "loess", span = .34) +
    geom_point( aes(y = coefs), position = position_dodge(width = .5), size = 4) +
    geom_errorbar(aes(ymax = ubs_95,ymin=lbs_95),position=position_dodge(width = .5),width = 0.5, size = 2) +
    geom_hline(yintercept = 1, linetype = 2) +
    
    scale_colour_manual(values = cols, guide = guide_legend(title = "Treatment"))+
    xlab("Weeks Post-Treatment, Non-Overlapping") +
    ylab("Ratio of Number of Incivil Tweets, Relative to Control") +
    coord_cartesian(ylim = c(.1, 2)) +
    
    ggtitle("Effects on Democrat Subjects") +
    theme(plot.title = element_text(hjust=.5, size = 20), legend.text=element_text(size=20), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_text(size=20),
          
          
          axis.title.y = element_text(size=20),
          axis.title.x = element_text(size=20),
          
          axis.line = element_line(colour = "black")) +
    scale_y_continuous(labels=c("0", "50%", "100%", "150%", "200%"))+
    annotate("text", x=1, y=.1, label= "Day 1", size = 8) +
    annotate("text", x=2, y=.1, label= "Week 1", size = 8) +
    annotate("text", x=3, y=.1, label= "Week 2", size = 8) +
    annotate("text", x=4, y=.1, label= "Weeks 3/4", size = 8) 
  
  print(p)
  
  pdf(file="results/nb_plot_dems.pdf", height=7, width=12)
  print(p)
  
  
  dev.off()  
  
  
  ######################################################################################
  
  ## reps
  full_1<-(glm.nb(post_aggtweets_1 ~ pre_aggtweets + f.treatment, data = republicans))
  
  full_7<-(glm.nb(post_aggtweets_7_ex ~ pre_aggtweets + f.treatment, data = republicans))
  
  full_14<-(glm.nb(post_aggtweets_14_ex ~ pre_aggtweets + f.treatment, data = republicans))
  
  full_28<-(glm.nb(post_aggtweets_28_ex ~ pre_aggtweets + f.treatment, data = republicans))
  
  
  
  est <-  coef(full_1)
  summ <-  summary(full_1)
  
  ##to calculate treatment  
  
  irr1_1<-exp(est[3])
  irr2_1<-exp(est[4])
  irr3_1<-exp(est[5])
  
  se1<-coef(summ)[3, "Std. Error"]
  se2<-coef(summ)[4, "Std. Error"]
  se3<-coef(summ)[5, "Std. Error"]
  
  
  
  ##calculate standard errors
  
  ##calculate CIs
  
  lb1_1<-exp(est[3]  - qnorm(.975) * se1)
  lb2_1<-exp(est[4] - qnorm(.975) * se2)
  lb3_1<-exp(est[5] - qnorm(.975) * se3)
  ub1_1<-exp(est[3]  + qnorm(.975) * se1)
  ub2_1<-exp(est[4] + qnorm(.975) * se2)
  ub3_1<-exp(est[5] + qnorm(.975) * se3)
  
  ##calculate treatment effects on the first week of treatment
  
  est <-  coef(full_7)
  summ <-  summary(full_7)
  
  ##to calculate treatment  
  
  irr1_7<-exp(est[3])
  irr2_7<-exp(est[4])
  irr3_7<-exp(est[5])
  
  se1<-coef(summ)[3, "Std. Error"]
  se2<-coef(summ)[4, "Std. Error"]
  se3<-coef(summ)[5, "Std. Error"]
  
  
  
  ##calculate standard errors
  
  ##calculate CIs
  
  lb1_7<-exp(est[3]  - qnorm(.975) * se1)
  lb2_7<-exp(est[4] - qnorm(.975) * se2)
  lb3_7<-exp(est[5] - qnorm(.975) * se3)
  ub1_7<-exp(est[3]  + qnorm(.975) * se1)
  ub2_7<-exp(est[4] + qnorm(.975) * se2)
  ub3_7<-exp(est[5] + qnorm(.975) * se3)  
  ##calculate treatment effects on the first two weeks of treatment
  
  est <-  coef(full_14)
  summ <-  summary(full_14)
  
  ##to calculate treatment  
  
  irr1_14<-exp(est[3])
  irr2_14<-exp(est[4])
  irr3_14<-exp(est[5])
  
  se1<-coef(summ)[3, "Std. Error"]
  se2<-coef(summ)[4, "Std. Error"]
  se3<-coef(summ)[5, "Std. Error"]
  
  
  
  ##calculate standard errors
  
  ##calculate CIs
  
  lb1_14<-exp(est[3]  - qnorm(.975) * se1)
  lb2_14<-exp(est[4] - qnorm(.975) * se2)
  lb3_14<-exp(est[5] - qnorm(.975) * se3)
  ub1_14<-exp(est[3]  + qnorm(.975) * se1)
  ub2_14<-exp(est[4] + qnorm(.975) * se2)
  ub3_14<-exp(est[5] + qnorm(.975) * se3)
  ##calculate treatment effects on the first month of treatment
  
  est <-  coef(full_28)
  summ <-  summary(full_28)
  
  ##to calculate treatment  
  
  irr1_28<-exp(est[3])
  irr2_28<-exp(est[4])
  irr3_28<-exp(est[5])
  
  se1<-coef(summ)[3, "Std. Error"]
  se2<-coef(summ)[4, "Std. Error"]
  se3<-coef(summ)[5, "Std. Error"]
  
  
  
  ##calculate standard errors
  
  ##calculate CIs
  
  lb1_28<-exp(est[3]  - qnorm(.975) * se1)
  lb2_28<-exp(est[4] - qnorm(.975) * se2)
  lb3_28<-exp(est[5] - qnorm(.975) * se3)
  ub1_28<-exp(est[3]  + qnorm(.975) * se1)
  ub2_28<-exp(est[4] + qnorm(.975) * se2)
  ub3_28<-exp(est[5] + qnorm(.975) * se3)
  
  
  
  ##create plots
  ##feelings 1, rules 2, public 3
  
  
  
  df_full<-data.frame(coefs = c(irr1_1, irr2_1, irr3_1,
                                irr1_7, irr2_7, irr3_7,
                                irr1_14, irr2_14, irr3_14,
                                irr1_28, irr2_28, irr3_28),
                      lbs_95 = c(lb1_1, lb2_1, lb3_1,
                                 lb1_7, lb2_7, lb3_7,
                                 lb1_14, lb2_14, lb3_14,
                                 lb1_28, lb2_28, lb3_28),
                      ubs_95 = c(ub1_1, ub2_1, ub3_1,
                                 ub1_7, ub2_7, ub3_7,
                                 ub1_14, ub2_14, ub3_14,
                                 ub1_28, ub2_28, ub3_28),
                      week = (as.factor(c(1,1,1,2,2,2,3,3,3,4,4,4))),
                      treatment = (as.factor(c("Care","Authority","Public",
                                               "Care","Authority","Public",
                                               "Care","Authority","Public",
                                               "Care","Authority","Public"))   ))                                            
  
  require(ggplot2)
  require(reshape2)
  cols <- c("purple", "forestgreen", "black")
  ##
  
  p <- ggplot(data = df_full,  
              #theme(panel.grid.major = element_blank(),
              # panel.grid.minor = element_blank(),
              #panel.background = element_blank(),
              #axis.line = element_line(colour = "black")) +
              aes(x = week, group=treatment ,  colour = treatment)) + #, group = delivery)) +
    
    #geom_smooth(alpha=0.2, linetype=1,  method = "loess", span = .34) +
    geom_point( aes(y = coefs), position = position_dodge(width = .5), size = 4) +
    geom_errorbar(aes(ymax = ubs_95,ymin=lbs_95),position=position_dodge(width = .5),width = 0.5, size = 2) +
    geom_hline(yintercept = 1, linetype = 2) +
    
    scale_colour_manual(values = cols, guide = guide_legend(title = "Treatment"))+
    xlab("Weeks Post-Treatment, Non-Overlapping") +
    ylab("Ratio of Number of Incivil Tweets, Relative to Control") +
    coord_cartesian(ylim = c(.1, 2)) +
    
    ggtitle("Effects on Republican Subjects") +
    theme(plot.title = element_text(hjust=.5, size = 20), legend.text=element_text(size=20), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_text(size=20),
          
          
          axis.title.y = element_text(size=20),
          axis.title.x = element_text(size=20),
          
          axis.line = element_line(colour = "black")) +
    scale_y_continuous(labels=c("0", "50%", "100%", "150%", "200%"))+
    annotate("text", x=1, y=.1, label= "Day 1", size = 8) +
    annotate("text", x=2, y=.1, label= "Week 1", size = 8) +
    annotate("text", x=3, y=.1, label= "Week 2", size = 8) +
    annotate("text", x=4, y=.1, label= "Weeks 3/4", size = 8) 
  
  print(p)
  
  pdf(file="results/nb_plot_reps.pdf", height=7, width=12)
  print(p)
  
  
  dev.off()    