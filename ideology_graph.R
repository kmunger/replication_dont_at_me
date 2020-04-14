##figure 16
setwd("C:/Users/kevin/Documents/GitHub/replication_dont_at_me/")

load("subjects_active_anonymized.Rdata")

library(ggplot2)
###use continuous ideology
summary(lm(response~ rightist + ideology, data=subjects_active_anonymized))




subjects_active_anonymized$estimated_rightist<-subjects_active_anonymized$ideology

subjects_active_anonymized$estimated_rightist[subjects_active_anonymized$ideology<0]<-0

subjects_active_anonymized$estimated_rightist[subjects_active_anonymized$ideology>0]<-1



subjects_active_anonymized$Subject<-as.character(subjects_active_anonymized$rightist)

pdf("results/ideology_scores_full.pdf", 9, 6)

#Plot.
ggplot(subjects_active_anonymized, aes(x = ideology, fill = Subject)) + 
  geom_density(alpha = 0.5 )+
  scale_fill_manual( values = c("blue","red"), labels = c("Anti-Trump", "Anti-Clinton"))+
  xlab("Subject Ideology (Left to Right) Estimated By Twitter Networks") +
  ylab("Number of Subjects") +

  ggtitle("Anti-Trump Subjects Were Ideologically Diverse") +
  theme(plot.title = element_text(hjust=.5), legend.text=element_text(size=12)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_line(colour = "black")) 

dev.off()



