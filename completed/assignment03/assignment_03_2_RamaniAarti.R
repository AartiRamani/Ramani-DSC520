---
output:
  pdf_document: default
  html_document: default
---
# Assignment: ASSIGNMENT 3
# Name: Ramani, Aarti
# Date: 2022-12-14
```{r}
# Assignment: ASSIGNMENT 3.2
# Name: Ramani, Aarti
# Date: 2022-12-14

## Load the ggplot2 package
library(ggplot2)

## Set the working directory to the root of DSC 520 directory
setwd("C:/Masters/GitHub/Winter2022/Ramani-DSC520")

#List the name of each field and what you believe the data type and intent is of
#the data included in each field (Example: Id - Data Type: varchar 
#(contains text and numbers) Intent: unique identifier for each row)

## Load the `data/r4ds/heights.csv` to
heights_df <- read.csv("data/acs-14-1yr-s0201.csv")


colnames(heights_df)
#Run the following functions and provide the results: str(); nrow(); ncol()
str(heights_df)
nrow(heights_df)
ncol(heights_df)

#Create a Histogram of the HSDegree variable using the ggplot2 package.
#Set a bin size for the Histogram that you think best visuals the data 
#(the bin size will determine how many bars display and how wide they are)
#Include a Title and appropriate X/Y axis labels on your Histogram Plot.
library(ggplot2)
ggplot(heights_df, aes(HSDegree)) + geom_histogram(bins=50) + ggtitle("HS Degree vs. Count") + xlab("HS Degree (%)") + ylab("Count")

#Answer the following questions based on the Histogram produced:
#Based on what you see in this histogram, is the data distribution unimodal?
# > No, this is not a unimodal distribution since more that one HSdegree percentage is recurring
 
# > Standard deviation = 5.117941
# sd(heights_df$HSDegree)

#  Is it approximately symmetrical?
# > No, the histogram is not symmetrical. The left and right sides are not symmetrical.

#  Is it approximately bell-shaped?
# > Yes, it is approximately bell-shaped.  

#  Is it approximately normal?
# > shapiro.test(heights_df$HSDegree) W = 0.87736, p-value = 3.194e-09. p<.001 - Not normal
shapiro.test(heights_df$HSDegree)

#  If not normal, is the distribution skewed? If so, in which direction?
# > The histogram is left or negatively skewed since the mean is lower than the median and most of the data lies to the left of the mean

#  Include a normal curve to the Histogram that you plotted.
#Explain whether a normal distribution can accurately be used as a model for this data.
# > The graph is skewed and does not qualify for normal distribution. 
# > For a normal distribution, 1 to 100% of area of the plot should be under the normal curve. 
ggplot(heights_df, aes(HSDegree))+ geom_histogram(aes(y=..density..),color = "blue",bins=50,fill="light green") +
  stat_function(fun = dnorm,args = list(mean = mean(heights_df$HSDegree),sd = sd(heights_df$HSDegree)),col = "black")+
  ggtitle("HS Degree vs. Count") + xlab("HS Degree (%)") + ylab("Count")

#Create a Probability Plot of the HSDegree variable.
library(qqplotr)
ggplot(data = heights_df, aes(sample = HSDegree)) + stat_qq(colour="blue")  
 
#Answer the following questions based on the Probability Plot:
#Based on what you see in this probability plot, is the distribution approximately normal? Explain how you know.
ggplot(data = heights_df, aes(sample = HSDegree)) + stat_qq(colour="blue") + stat_qq_line(colour="red")
# > The distribution is not normal. Plot is away from the normal line. 

#If not normal, is the distribution skewed? If so, in which direction? Explain how you know.
# > Plot is way away from the normal line and since data is away from the X-axis, it is left skewed.

# Now that you have looked at this data visually for normality, you will now quantify normality with numbers using the stat.desc() function. Include a screen capture of the results produced.
#library(pastecs)
#library(psych) 
describe(heights_df)
stat.desc(heights_df$HSDegree,basic = TRUE, norm = TRUE) 
z-score <- (heights_df$HSDegree - mean(heights_df$HSDegree))/sd(heights_df$HSDegree)
z-score
# In several sentences provide an explanation of the result produced for skew, kurtosis, and z-scores.
# > For a normal distribution, skew and kurtosis should be 0. In this case, the skew is -1.674767 and kurtosis is 4.352856
# > A negative skew represents a left skew.
# > A positive kurtosis represents a pointy and heavy-tailed distribution.
# > Data in a left skew, positive kurtosis will be concentrated on the right side of the distribution graph.
# > Mean = 87.63    Standard Deviation = 5.117941.
# > Z-score helps measure the standard deviation from the mean.
# > A positive z-score implies the individual value is greater than the mean, negative z-score implies value is lesser than the mean. A 0 implies zscale equals the mean.

# In addition, explain how a change in the sample size may change your explanation?
Sample size and 
```
