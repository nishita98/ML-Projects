#setting the working directory
setwd("~/R/QDA_2021")

#importing the assignment 1 dataset
MMI_data = read.csv ("MMIdata.csv")

#checking head and tail data
head(MMI_data)
tail(MMI_data)

#descriptive statistics for column MMI Score
mean(MMI_data$MMI.Score)
sd(MMI_data$MMI.Score)
max(MMI_data$MMI.Score)
min(MMI_data$MMI.Score)
IQR(MMI_data$MMI.Score)

#as MMI score ranges from 13 to 65 and as the maximum score >65 so searching all the scores >65
which(MMI_data$MMI.Score>65)

#replacing score 67 at row 13th, 22nd with score 65
MMI_data.replace = MMI_data
MMI_data.replace[13,1] = 65
MMI_data.replace[22,1] = 65

#view MMI_data.replace score
MMI_data.replace$MMI.Score

#descriptive statistics of MMI Score replaced
mean(MMI_data.replace$MMI.Score)
sd(MMI_data.replace$MMI.Score)
max(MMI_data.replace$MMI.Score)
min(MMI_data.replace$MMI.Score)
IQR(MMI_data.replace$MMI.Score)
median(MMI_data.replace$MMI.Score)

#hist and box plot functions on MMI replaced score
hist (MMI_data.replace$MMI.Score, main="Histogram of MMI Score", xlab='MMI Score', breaks=c (20,30,40,50,60,70))
boxplot(MMI_data.replace$MMI.Score, main="Boxplot of MMI Score", ylab='MMI Score', ylim=c(20,70))

#replacing the outlier=34 by mean - two standard deviation as it doesn't affect the value much and removes the outlier too.
MMI_data.replace[71,1] = (mean(MMI_data.replace$MMI.Score)-2*sd(MMI_data.replace$MMI.Score))
boxplot(MMI_data.replace$MMI.Score, main="Boxplot of MMI Score", ylab='MMI Score', ylim=c(20,70))

boxplot.stats(MMI_data.replace$MMI.Score)


#descriptive statistics of MMI Score replaced with no outliers
mean(MMI_data.replace$MMI.Score)
sd(MMI_data.replace$MMI.Score)
max(MMI_data.replace$MMI.Score)
min(MMI_data.replace$MMI.Score)
IQR(MMI_data.replace$MMI.Score)

#descriptive statistics for column WEMWBS Score
mean(MMI_data.replace$WEMWBS.Score)
sd(MMI_data.replace$WEMWBS.Score)
max(MMI_data.replace$WEMWBS.Score)
min(MMI_data.replace$WEMWBS.Score)
IQR(MMI_data.replace$WEMWBS.Score)

#histogram and box plot functions on WEMWBS score
hist (MMI_data.replace$WEMWBS.Score, main="Histogram of WEMWBS Score", xlab='WEMWBS Score', breaks=c (10,20,30,40,50,60,70))
boxplot(MMI_data.replace$WEMWBS.Score, main="Boxplot of WEMWBS Score", ylab='WEMWBS Score', ylim=c(10,70))

#Scatter plot between MMI and WEMWBS score
plot (MMI_data.replace$MMI.Score, MMI_data.replace$WEMWBS.Score,pch = 16, col="blue", xlab="MMI Score", ylab="WEMWBS Score", main="Scatterplot of MMI and WEMWBS score")

#Testing the correlation
cor.test(MMI_data.replace$MMI.Score, MMI_data.replace$WEMWBS.Score)

#As cor = -0.602 so evident enough to construct a linear model and best fit line
lm(MMI_data.replace$MMI.Score~MMI_data.replace$WEMWBS.Score)
abline(65.7699,-.3285,col="red")
