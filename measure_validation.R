
##Figure 10
setwd("C:/Users/kevin/Documents/GitHub/replication_dont_at_me/")

library(dplyr)


validation_tweets<-read.csv(file = "validation_tweets.csv", stringsAsFactors = T)

agg_thresholds<-c(.295, .405)


##filter to where there's agreement 

agreement<-filter(validation_tweets, Agreement == "Yes")



  ###validate for real
acc<-NULL
for(i in 1:100){
  acc[i] <-(length(agreement$Answer1[(agreement$Answer1 == "Civil" & agreement$scores < i/100)] ) + 
              length(agreement$Answer1[(agreement$Answer1 == "Incivil" & agreement$scores > i/100)] )) /
    length(agreement$Answer1)
  
  
}

x<-seq(1:100)

pdf("results/validation_accuracy.pdf", 8 , 5)
plot(x = x, y= acc, ylab = "Validation Accuracy", xlab = "Civil/Incivil Cutoff" )
abline(v = agg_thresholds[1]*100, col = "red")
abline(v = agg_thresholds[2]*100, col = "black")

dev.off()



