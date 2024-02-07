> #100 participants took part in a study on the relationship between how much people multitask with media and their mental well being.
> #Participants were asked to complete two questionnaires, the Media Multitasking Index(MMI) and the Warwick-Edinburgh Mental Well-being Scale (WEMWBS).
> #The MMI is a 13 item 5 point Likert scale questionnaire, where people are asked questions about how frequently they conduct multitasking behaviours.
> #The scale ranges from a minimum of 13 to a maximum score of 65. The WEMWBS is a 14 item 5 point Likert scale questionnaire measuring positive feelings and behaviours related to mental health. 
> 
>  # *H1: There will be a statistically significant relationship between MMI score and WEMWBS*
> 
> 
> #setting the working directory
> setwd("~/R/QDA_2021")
> 
> #importing the assignment 1 dataset
> MMI_data = read.csv ("MMIdata.csv")
> 
> #checking head and tail data
> head(MMI_data)
  MMI.Score WEMWBS.Score
1        47           28
2        58           43
3        51           43
4        46           51
5        53           45
6        55           34
> tail(MMI_data)
    MMI.Score WEMWBS.Score
95         56           20
96         54           39
97         43           65
98         44           42
99         52           37
100        46           35
> 
> #descriptive statistics for column MMI Score
> mean(MMI_data$MMI.Score)
[1] 51.21
> sd(MMI_data$MMI.Score)
[1] 6.236573
> max(MMI_data$MMI.Score)
[1] 67
> min(MMI_data$MMI.Score)
[1] 34
> IQR(MMI_data$MMI.Score)
[1] 8.25
> 
> #as MMI score ranges from 13 to 65 and as the maximum score >65 so searching all the scores >65
> which(MMI_data$MMI.Score>65)
[1] 13 22
> 
> #replacing score 67 at row 13th, 22nd with score 65
> MMI_data.replace = MMI_data
> MMI_data.replace[13,1] = 65
> MMI_data.replace[22,1] = 65
> 
> #view MMI_data.replace score
> MMI_data.replace$MMI.Score
  [1] 47 58 51 46 53 55 49 51 50 46 63 52 65 64 48 39 50 48 45 54 54 65 56 50 53 47 45 47 53 50 57 49 49 51 65 52 50 51
 [39] 56 57 44 53 45 52 54 61 53 64 48 55 58 49 43 50 57 58 49 58 47 45 49 56 47 50 52 56 43 55 42 51 34 52 48 51 56 56
 [77] 51 44 62 50 41 49 51 45 59 58 53 40 58 44 44 41 48 52 56 54 43 44 52 46
> 
> #descriptive statistics of MMI Score replaced
> mean(MMI_data.replace$MMI.Score)
[1] 51.17
> sd(MMI_data.replace$MMI.Score)
[1] 6.139876
> max(MMI_data.replace$MMI.Score)
[1] 65
> min(MMI_data.replace$MMI.Score)
[1] 34
> IQR(MMI_data.replace$MMI.Score)
[1] 8.25
> median(MMI_data.replace$MMI.Score)
[1] 51
> 
> #hist and box plot functions on MMI replaced score
> hist (MMI_data.replace$MMI.Score, main="Histogram of MMI Score", xlab='MMI Score', breaks=c (20,30,40,50,60,70))
> boxplot(MMI_data.replace$MMI.Score, main="Boxplot of MMI Score", ylab='MMI Score', ylim=c(20,70))
> 
> #replacing the outlier=34 by mean - two standard deviation as it doesn't affect the value much and removes the outlier too.
> MMI_data.replace[71,1] = (mean(MMI_data.replace$MMI.Score)-2*sd(MMI_data.replace$MMI.Score))
> boxplot(MMI_data.replace$MMI.Score, main="Boxplot of MMI Score", ylab='MMI Score', ylim=c(20,70))
> 
> boxplot.stats(MMI_data.replace$MMI.Score)
$stats
[1] 38.89025 47.00000 51.00000 55.50000 65.00000

$n
[1] 100

$conf
[1] 49.657 52.343

$out
numeric(0)

> 
> 
> #descriptive statistics of MMI Score replaced with no outliers
> mean(MMI_data.replace$MMI.Score)
[1] 51.2189
> sd(MMI_data.replace$MMI.Score)
[1] 6.020046
> max(MMI_data.replace$MMI.Score)
[1] 65
> min(MMI_data.replace$MMI.Score)
[1] 38.89025
> IQR(MMI_data.replace$MMI.Score)
[1] 8.25
> 
> #descriptive statistics for column WEMWBS Score
> mean(MMI_data.replace$WEMWBS.Score)
[1] 44.29
> sd(MMI_data.replace$WEMWBS.Score)
[1] 11.03282
> max(MMI_data.replace$WEMWBS.Score)
[1] 70
> min(MMI_data.replace$WEMWBS.Score)
[1] 18
> IQR(MMI_data.replace$WEMWBS.Score)
[1] 15
> 
> #histogram and box plot functions on WEMWBS score
> hist (MMI_data.replace$WEMWBS.Score, main="Histogram of WEMWBS Score", xlab='WEMWBS Score', breaks=c (10,20,30,40,50,60,70))
> boxplot(MMI_data.replace$WEMWBS.Score, main="Boxplot of WEMWBS Score", ylab='WEMWBS Score', ylim=c(10,70))
> 
> #Scatter plot between MMI and WEMWBS score
> plot (MMI_data.replace$MMI.Score, MMI_data.replace$WEMWBS.Score,pch = 16, col="blue", xlab="MMI Score", ylab="WEMWBS Score", main="Scatterplot of MMI and WEMWBS score")
> 
> #Testing the correlation
> cor.test(MMI_data.replace$MMI.Score, MMI_data.replace$WEMWBS.Score)

	Pearson's product-moment correlation

data:  MMI_data.replace$MMI.Score and MMI_data.replace$WEMWBS.Score
t = -7.4655, df = 98, p-value = 3.441e-11
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.7140756 -0.4601034
sample estimates:
       cor 
-0.6021075 

> 
> #As cor = -0.602 so evident enough to construct a linear model and best fit line
> lm(MMI_data.replace$MMI.Score~MMI_data.replace$WEMWBS.Score)

Call:
lm(formula = MMI_data.replace$MMI.Score ~ MMI_data.replace$WEMWBS.Score)

Coefficients:
                  (Intercept)  MMI_data.replace$WEMWBS.Score  
                      65.7699                        -0.3285  

> abline(65.7699,-.3285,col="red")
