##figure 6 and 11
setwd("C:/Users/kevin/Documents/GitHub/replication_dont_at_me/")


##read in subject information --70th
load(file = "subjects_70th.RData")

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








full_1<-(lm(post_aggtweets_1 ~ pre_aggtweets + f.treatment, data = subjects_active_anonymized))

full_7<-(lm(post_aggtweets_7_ex ~ pre_aggtweets + f.treatment, data = subjects_active_anonymized))

full_14<-(lm(post_aggtweets_14_ex ~ pre_aggtweets + f.treatment, data = subjects_active_anonymized))

full_28<-(lm(post_aggtweets_28_ex ~ pre_aggtweets + f.treatment, data = subjects_active_anonymized))




###formal test of different treatment effects 


#####run a t-test in coefficients 
summary(full_1)
library(car)

# tests if the coefficient of Sepal.Width = Petal.Length
linearHypothesis(full_7, "treatment_binary1 = treatment_binary2")
linearHypothesis(full_1, "treatment_binary1 = treatment_binary2")








est <-  coef(full_1)
summ <-  summary(full_1)

##to calculate treatment  

irr1_1<-(est[3])
irr2_1<-(est[4])
irr3_1<-(est[5])

se1<-coef(summ)[3, "Std. Error"]
se2<-coef(summ)[4, "Std. Error"]
se3<-coef(summ)[5, "Std. Error"]



##calculate standard errors

##calculate CIs

lb1_1<-(est[3]  - qnorm(.975) * se1)
lb2_1<-(est[4] - qnorm(.975) * se2)
lb3_1<-(est[5] - qnorm(.975) * se3)
ub1_1<-(est[3]  + qnorm(.975) * se1)
ub2_1<-(est[4] + qnorm(.975) * se2)
ub3_1<-(est[5] + qnorm(.975) * se3)

##calculate treatment effects on the first week of treatment

est <-  coef(full_7)
summ <-  summary(full_7)

##to calculate treatment  

irr1_7<-(est[3])
irr2_7<-(est[4])
irr3_7<-(est[5])

se1<-coef(summ)[3, "Std. Error"]
se2<-coef(summ)[4, "Std. Error"]
se3<-coef(summ)[5, "Std. Error"]



##calculate standard errors

##calculate CIs

lb1_7<-(est[3]  - qnorm(.975) * se1)
lb2_7<-(est[4] - qnorm(.975) * se2)
lb3_7<-(est[5] - qnorm(.975) * se3)
ub1_7<-(est[3]  + qnorm(.975) * se1)
ub2_7<-(est[4] + qnorm(.975) * se2)
ub3_7<-(est[5] + qnorm(.975) * se3)  
##calculate treatment effects on the first two weeks of treatment

est <-  coef(full_14)
summ <-  summary(full_14)

##to calculate treatment  

irr1_14<-(est[3])
irr2_14<-(est[4])
irr3_14<-(est[5])

se1<-coef(summ)[3, "Std. Error"]
se2<-coef(summ)[4, "Std. Error"]
se3<-coef(summ)[5, "Std. Error"]



##calculate standard errors

##calculate CIs

lb1_14<-(est[3]  - qnorm(.975) * se1)
lb2_14<-(est[4] - qnorm(.975) * se2)
lb3_14<-(est[5] - qnorm(.975) * se3)
ub1_14<-(est[3]  + qnorm(.975) * se1)
ub2_14<-(est[4] + qnorm(.975) * se2)
ub3_14<-(est[5] + qnorm(.975) * se3)
##calculate treatment effects on the first month of treatment

est <-  coef(full_28)
summ <-  summary(full_28)

##to calculate treatment  

irr1_28<-(est[3])
irr2_28<-(est[4])
irr3_28<-(est[5])

se1<-coef(summ)[3, "Std. Error"]
se2<-coef(summ)[4, "Std. Error"]
se3<-coef(summ)[5, "Std. Error"]



##calculate standard errors

##calculate CIs

lb1_28<-(est[3]  - qnorm(.975) * se1)
lb2_28<-(est[4] - qnorm(.975) * se2)
lb3_28<-(est[5] - qnorm(.975) * se3)
ub1_28<-(est[3]  + qnorm(.975) * se1)
ub2_28<-(est[4] + qnorm(.975) * se2)
ub3_28<-(est[5] + qnorm(.975) * se3)



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
cols <- c("solid", "dotted", "twodash")
##

p <- ggplot(data = df_full,  
            #theme(panel.grid.major = element_blank(),
            # panel.grid.minor = element_blank(),
            #panel.background = element_blank(),
            #axis.line = element_line(colour = "black")) +
            aes(x = week, group=treatment ,  linetype = treatment)) + #, group = delivery)) +
  
  #geom_smooth(alpha=0.2, linetype=1,  method = "loess", span = .34) +
  geom_point( aes(y = coefs), position = position_dodge(width = .5), size = 4) +
  geom_errorbar(aes(ymax = ubs_95,ymin=lbs_95),position=position_dodge(width = .5),width = 0.5, size = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  
  scale_linetype_manual(values = cols, guide = guide_legend(title = "Treatment"))+
  xlab("Weeks Post-Treatment, Non-Overlapping") +
  ylab("Log Change in Incivil Tweets, Relative to Control") +
  #coord_cartesian(ylim = c(-.7, .3)) +
  
  ggtitle("Treatment Effects on All Subjects") +
  theme(plot.title = element_text(hjust=.5, size = 20), legend.text=element_text(size=20), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(size=20),
        
        
        axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20),
        
        axis.line = element_line(colour = "black")) +
  # scale_y_continuous(labels=c("0", "50%", "100%", "150%", "200%"))+
  annotate("text", x=1, y=-1.2, label= "Day 1", size = 8) +
  annotate("text", x=2, y=-1.2, label= "Week 1", size = 8) +
  annotate("text", x=3, y=-1.2, label= "Week 2", size = 8) +
  annotate("text", x=4, y=-1.2, label= "Weeks 3/4", size = 8) 

print(p)


pdf(file="results/OLS_plot.pdf", height=7, width=12)
print(p)


dev.off()


##################################################################################
##read in subject information --90th
load(file = "subjects_90th.RData")

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








full_1<-(lm(post_aggtweets_1 ~ pre_aggtweets + f.treatment, data = subjects_active_anonymized))

full_7<-(lm(post_aggtweets_7_ex ~ pre_aggtweets + f.treatment, data = subjects_active_anonymized))

full_14<-(lm(post_aggtweets_14_ex ~ pre_aggtweets + f.treatment, data = subjects_active_anonymized))

full_28<-(lm(post_aggtweets_28_ex ~ pre_aggtweets + f.treatment, data = subjects_active_anonymized))








est <-  coef(full_1)
summ <-  summary(full_1)

##to calculate treatment  

irr1_1<-(est[3])
irr2_1<-(est[4])
irr3_1<-(est[5])

se1<-coef(summ)[3, "Std. Error"]
se2<-coef(summ)[4, "Std. Error"]
se3<-coef(summ)[5, "Std. Error"]



##calculate standard errors

##calculate CIs

lb1_1<-(est[3]  - qnorm(.975) * se1)
lb2_1<-(est[4] - qnorm(.975) * se2)
lb3_1<-(est[5] - qnorm(.975) * se3)
ub1_1<-(est[3]  + qnorm(.975) * se1)
ub2_1<-(est[4] + qnorm(.975) * se2)
ub3_1<-(est[5] + qnorm(.975) * se3)

##calculate treatment effects on the first week of treatment

est <-  coef(full_7)
summ <-  summary(full_7)

##to calculate treatment  

irr1_7<-(est[3])
irr2_7<-(est[4])
irr3_7<-(est[5])

se1<-coef(summ)[3, "Std. Error"]
se2<-coef(summ)[4, "Std. Error"]
se3<-coef(summ)[5, "Std. Error"]



##calculate standard errors

##calculate CIs

lb1_7<-(est[3]  - qnorm(.975) * se1)
lb2_7<-(est[4] - qnorm(.975) * se2)
lb3_7<-(est[5] - qnorm(.975) * se3)
ub1_7<-(est[3]  + qnorm(.975) * se1)
ub2_7<-(est[4] + qnorm(.975) * se2)
ub3_7<-(est[5] + qnorm(.975) * se3)  
##calculate treatment effects on the first two weeks of treatment

est <-  coef(full_14)
summ <-  summary(full_14)

##to calculate treatment  

irr1_14<-(est[3])
irr2_14<-(est[4])
irr3_14<-(est[5])

se1<-coef(summ)[3, "Std. Error"]
se2<-coef(summ)[4, "Std. Error"]
se3<-coef(summ)[5, "Std. Error"]



##calculate standard errors

##calculate CIs

lb1_14<-(est[3]  - qnorm(.975) * se1)
lb2_14<-(est[4] - qnorm(.975) * se2)
lb3_14<-(est[5] - qnorm(.975) * se3)
ub1_14<-(est[3]  + qnorm(.975) * se1)
ub2_14<-(est[4] + qnorm(.975) * se2)
ub3_14<-(est[5] + qnorm(.975) * se3)
##calculate treatment effects on the first month of treatment

est <-  coef(full_28)
summ <-  summary(full_28)

##to calculate treatment  

irr1_28<-(est[3])
irr2_28<-(est[4])
irr3_28<-(est[5])

se1<-coef(summ)[3, "Std. Error"]
se2<-coef(summ)[4, "Std. Error"]
se3<-coef(summ)[5, "Std. Error"]



##calculate standard errors

##calculate CIs

lb1_28<-(est[3]  - qnorm(.975) * se1)
lb2_28<-(est[4] - qnorm(.975) * se2)
lb3_28<-(est[5] - qnorm(.975) * se3)
ub1_28<-(est[3]  + qnorm(.975) * se1)
ub2_28<-(est[4] + qnorm(.975) * se2)
ub3_28<-(est[5] + qnorm(.975) * se3)



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
cols <- c("solid", "dotted", "twodash")
##

p <- ggplot(data = df_full,  
            #theme(panel.grid.major = element_blank(),
            # panel.grid.minor = element_blank(),
            #panel.background = element_blank(),
            #axis.line = element_line(colour = "black")) +
            aes(x = week, group=treatment ,  linetype = treatment)) + #, group = delivery)) +
  
  #geom_smooth(alpha=0.2, linetype=1,  method = "loess", span = .34) +
  geom_point( aes(y = coefs), position = position_dodge(width = .5), size = 4) +
  geom_errorbar(aes(ymax = ubs_95,ymin=lbs_95),position=position_dodge(width = .5),width = 0.5, size = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  
  scale_linetype_manual(values = cols, guide = guide_legend(title = "Treatment"))+
  xlab("Weeks Post-Treatment, Non-Overlapping") +
  ylab("Log Change in Incivil Tweets, Relative to Control") +
  #coord_cartesian(ylim = c(-.7, .3)) +
  
  ggtitle("Treatment Effects on All Subjects, 90th Percentile Threshold") +
  theme(plot.title = element_text(hjust=.5, size = 20), legend.text=element_text(size=20), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(size=20),
        
        
        axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20),
        
        axis.line = element_line(colour = "black")) +
  # scale_y_continuous(labels=c("0", "50%", "100%", "150%", "200%"))+
  annotate("text", x=1, y=-1.2, label= "Day 1", size = 8) +
  annotate("text", x=2, y=-1.2, label= "Week 1", size = 8) +
  annotate("text", x=3, y=-1.2, label= "Week 2", size = 8) +
  annotate("text", x=4, y=-1.2, label= "Weeks 3/4", size = 8) 

print(p)


pdf(file="results/OLS_plot_90.pdf", height=7, width=12)
print(p)


dev.off()

