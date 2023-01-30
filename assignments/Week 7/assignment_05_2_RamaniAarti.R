# Assignment: ASSIGNMENT 5
# Name: Ramani, Aarti
# Date: 2021-01-27

## Set the working directory to the root of your DSC 520 directory
setwd("C:/Masters/GitHub/Winter2022/Ramani-DSC520")

## Load the `data/r4ds/heights.csv` to
student_survey <- read.csv("data/student-survey.csv")
nrow(student_survey)
## i. Use R to calculate the covariance of the Survey variables and provide an 
## explanation of why you would use this calculation and what the results indicate.
## TimeReading vs. TimeTV
cor(student_survey$TimeReading,student_survey$TimeTV, method="pearson")
# -0.8830677 -> There is a negative correlation between the time spent reading and watching TV

## TimeReading vs. Happiness
cor(student_survey$TimeReading,student_survey$Happiness, method="pearson")
# -0.4348663 -> Time spent reading and Happiness are also negatively correlated

## TimeReading vs. Gender
cor(student_survey$TimeReading,student_survey$Gender, method="pearson")
# -0.08964215 -> Time spent reading and Gender are also negatively correlated.
# The value is close to 0.

## TimeTV vs. Happiness
cor(student_survey$TimeTV,student_survey$Happiness, method="pearson")
# 0.636556 -> There is a positive correlation between the time spent watching TV and happiness

## TimeTV vs. Gender
cor(student_survey$TimeTV,student_survey$Gender, method="pearson")
# 0.006596673 -> Time spent watching TV and Gender are also positively correlated

## Happiness vs. Gender
cor(student_survey$Happiness,student_survey$Gender, method="pearson")
# 0.1570118 -> Happiness and Gender are positively correlated


## ii. Examine the Survey data variables. 
##What measurement is being used for the variables? 

#TimeReading - In hours
#TimeTV - In minutes
#Happiness - Numeric 0-100 (O being lowest and 100 being highest)
#Gender - Binary 0 and 1

##Explain what effect changing the measurement being used for the variables 
##would have on the covariance calculation. Would this be a problem? 
##Explain and provide a better alternative if needed.

#All variables except Gender are numeric. I would like to change the gender to a string. 
#In the current times gender is represented in different forms such as queer, male, female, fluid etc.
#I would avoid computing Covariance between Gender and the other variables 
#since its not relevant to the research question.
 

## iii. Choose the type of correlation test to perform, explain why you chose this test, 
## and make a prediction if the test yields a positive or negative correlation?
cor(student_survey$TimeReading,student_survey$TimeTV, method="pearson")
#Answer:
#I choose to calculate correlation between Time spent Reading and Time spent watching TV 
#with One-Sample T-Test since they are both in measures of time.
#The variables have a negative correlation and the value is closer to -1 implying both are inversely related.
#The more time a person spends in watching TV, the lesser time spent in reading and vice-versa.

## iv. Perform a correlation analysis of:
##1. All variables
cor(student_survey, use="complete.obs", method = "pearson") 
#cor(student_survey, use="everything") 

##2. A single correlation between two a pair of the variables
cor(student_survey[,c(1:2,3)], method = "pearson")
cor(student_survey, use="pairwise.complete", method = "pearson")

##3. Repeat your correlation test in step 2 but set the confidence interval at 99%

## TimeReading vs. TimeTV
cor.test(student_survey$TimeReading,student_survey$TimeTV, method="pearson", conf.level = .99) 
# p value is low (<0.05), implying there is a strong correlation between the 2 variables.
# cor is -0.883 which is closer to a -1 implying a strong negative correlation.
# alternative hypothesis: true correlation is not equal to 0. 
#  - If 0, implies there is no relation between the variables.
# Negative correlation -> variables are inversely related,
#                         if one variable goes up the other goes down.
# As the time spent reading goes up less time is spent watching TV.

## TimeReading vs. Happiness
cor.test(student_survey$TimeReading,student_survey$Happiness, method="pearson", conf.level = .99)
# p value is high (>0.05), implying the correlation between the 2 variables is not too strong.
# cor is -0.4348663 which is negative but also closer to 0 implying a negative correlation.
# alternative hypothesis: true correlation is not equal to 0. 
#  - If 0, implies there is no relation between the variables.
# Negative correlation -> variables are inversely related,
#                         if one variable goes up the other goes down.
# As the time spent reading goes up lesser happy.

## TimeReading vs. Gender
cor.test(student_survey$TimeReading,student_survey$Gender, method="pearson", conf.level = .99)
# p value is high (>0.05), implying the correlation between the 2 variables is not too strong.
# cor is -0.08964215 which is negative but almost 0 implying a negative but weak correlation.
# alternative hypothesis: true correlation is not equal to 0. 
#  - If 0, implies there is no relation between the variables.
# Negative correlation -> variables are inversely related,
#                         if one variable goes up the other goes down.
# There is not much relevance of gender on the time spent reading.

## TimeTV vs. Happiness
cor.test(student_survey$TimeTV,student_survey$Happiness, method="pearson", conf.level=.99)
# p value is low (<0.05), implying a strong correlation between the 2 variables.
# cor is 0.636556 which is positive and closer to 1 => positive correlation.
# alternative hypothesis: true correlation is not equal to 0. 
#  - If 0, implies there is no relation between the variables.
# Positive correlation -> variables are directly related,
#                         if one variable goes up the other goes up too.
# As the time spent watching TV goes up more happier.

## TimeTV vs. Gender
cor.test(student_survey$TimeTV,student_survey$Gender, method="pearson", conf.level=.99)
# p value is high (>0.05), implying a weak correlation between the 2 variables.
# cor is 0.006596673 which is positive but almost 0 implying a positive and very weak correlation.
# alternative hypothesis: true correlation is not equal to 0. 
#  - If 0, implies there is no relation between the variables.
# Positive correlation -> variables are directly related,
#                         if one variable goes up the other goes up too.
# There is not much relevance of gender on the time spent reading.

## Happiness vs. Gender
cor.test(student_survey$Happiness,student_survey$Gender, method="pearson", conf.level=.99)
# p value is high (>0.05), implying a weak correlation between the 2 variables.
# cor is 0.1570118 which is positive but very close to 0 implying a positive and weak correlation.
# alternative hypothesis: true correlation is not equal to 0. 
#  - If 0, implies there is no relation between the variables.
# Positive correlation -> variables are directly related,
#                         if one variable goes up the other goes up too.
# There is not much relevance of gender and happiness.

## 4.Describe what the calculations in the correlation matrix suggest about the 
## relationship between the variables. Be specific with your explanation.
cor(student_survey, use="complete.obs", method = "pearson") 
#ANSWER:
# The correlation matrix explains the relationship between variables.
# A positive correlation implies the variables are directly related
# A negative correlation implies the variables are inversely related.
# More Time spent reading implies lesser time spent watching TV. 
# More time spent reading implies being lesser happier  
# No specific relevance to gender and time spent reading (almost 0)

# More Time spent watching TV implies lesser time spent reading. 
# More Time spent watching TV implies being more happier 
# No specific relevance to gender and time spent watching TV (almost 0)

# Lesser happier when more time spent reading
# More Happier when more time spent watching TV
# Lesser significance of happiness and gender

## v. Calculate the correlation coefficient and the coefficient of determination, 
## describe what you conclude about the results.
student_survey <- student_survey[, c("TimeReading", "TimeTV", "Happiness")]
cor(student_survey)
# Time spent in Reading and watching TV have a strong negative correlation
# Time spent in Reading and happiness have a moderate negative correlation
