#setting the working directory
setwd("~/R/QDA_2021")

#importing the assignment 2 dataset
driving  = read.csv ("driving(1).csv")

#removing the participant numbering column as not required
driving$participant = NULL

#checking head and tail data
head(driving)
tail(driving)

#setting the condition column as the factor
driving$condition = factor(driving$condition)

#checking the levels present in the factor: condition
levels (driving$condition)

#dividing both the conditions into different subsets
control = subset (driving$score, driving$condition=="control")
control

text = subset (driving$score, driving$condition=="text")
text

#using summary stats package, running the statistics for driving scores for control and text conditions
source ('summarystats.R')
summary.stats (driving$score)
summary.stats (control)
summary.stats (text)

#using plotrix package to study standard error of the mean for both conditions
install.packages("plotrix")
library("plotrix")  
std.error(text)
std.error(control)

#checking for impossible scores
which(driving$score<0)
which(driving$score>10)
#no impossible scores

#box plot for driving score
boxplot (driving$score,main="Boxplot of Driving scores", ylim= c(0,8), ylab = "Score")

#box plot for both conditions and found no outlier
boxplot(driving$score~driving$condition, main="Boxplot of Driving scores by condition", ylim= c(0,8),xlab = "Condition", ylab = "Score")

#histogram to check the spread of data and normality for both the conditions
hist (control, xlab="Driving Scores", main="Driving Quality scores for text (green) and control (orange) conditions", xlim=c(0,8), breaks=seq(0,8,1), col=rgb(1,.5,0,1/3))
hist (text, breaks=seq(0,8,1), col=rgb(0,1,0,1/3), add=TRUE)

#running Shapiro Wilk test to check if the subsets driving scores are normal enough to perform t-test or not
shapiro.test (control)
shapiro.test (text)

#using car package to run the levene test before t-test to check homogeneity of variance
library ('car')
leveneTest(driving$score, driving$condition)

#running independent t-test between control and text conditions
t.test (control, text, paired=FALSE)

#To create a bar plot to visualize the data storing mean and sd for both conditions respectively
driving.mean = c( mean(control), mean(text) )
driving.sd = c( sd(control), sd(text) )

#providing labels to means
names(driving.mean) = c("Control", "Texting")

#running the bar plot for mean data
barplot(driving.mean, main = "Graph of Condition Means", xlab= "Driving Condition", ylab="Driving Score", ylim=c(0,8),col=c(col=rgb(1,.5,0,1),rgb(0,1,0,1)))

#function to show error bars
se.bar = function(x, y, sds, n, upper=sds/sqrt(n), lower=upper, length=0.1,...)
{
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

#storing the bar plot in variable br to use it in the calling of the function
br = barplot (driving.mean, main = "Graph of Condition Means", xlab= "Driving Condition", ylab="Driving Score", ylim=c(0,8),border="black",col=c("orange","green"),density=60)

#make a call to error bar function
se.bar(br,driving.mean,driving.sd,16, col="red", lwd=2)
legend("topright", c("Control","Texting"),fill=c("orange","green"),density=60)



