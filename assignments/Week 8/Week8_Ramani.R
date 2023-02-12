#title: "Assignment 8.2"
#author: "Aarti Ramani"
#date: "2023-02-11"
#output:
#  pdf_document: default
#html_document: default 
  
 
library(readxl)
## Set the working directory to the root of your DSC 520 directory
setwd("C:/Masters/GitHub/Winter2022/Ramani-DSC520")

## Load housing data 
housing_df <- read_excel(path = "C:/Masters/GitHub/Winter2022/Ramani-DSC520/data/week-6-housing.xlsx",
                         .name_repair = function(col){ gsub(" ", "_", col) }) 
names(housing_df)

#housing_df <- na.omit(housing_df)

#i. Explain any transformations or modifications you made to the dataset
 
library(dplyr)
test <- housing_df %>% 
  mutate(ctyname = case_when((is.na(ctyname) & zip5 == 98052) ~ 'REDMOND',
                             (is.na(ctyname) & zip5 == 98053) ~ 'REDMOND',
                             (is.na(ctyname) & zip5 == 98074) ~ 'SAMMAMISH',
                             (is.na(ctyname) & zip5 == 98059) ~ 'RENTON',
                             TRUE~ctyname))
 
#Updated missing city names based on zipcode.


#ii.Create two variables; one that will contain the variables Sale Price and Square Foot of Lot (same variables used from previous assignment on simple regression) and one that will contain Sale Price and several additional predictors of your choice. Explain the basis for your additional predictor selections.
 
price_vs_sqf_lm <- lm(Sale_Price  ~ square_feet_total_living, data = housing_df)
price_vs_sqf_lm


price_and_more <- lm(Sale_Price~square_feet_total_living + bedrooms + 
                       bath_half_count + bath_full_count, data=housing_df)
price_and_more
 
#As additional predictors, I'm using bedrooms and bathrooms as these 
#fields make a significant impact on the sale price.

#iii. Execute a summary() function on two variables defined in the previous step
#to compare the model results. What are the R2 and Adjusted R2 statistics? 
#Explain what these results tell you about the overall model. 
#Did the inclusion of the additional predictors help explain any large variations 
#found in Sale Price?
 
summary(price_vs_sqf_lm)

summary(price_and_more)
 
#R-squared:  0.2066,	Adjusted R-squared:  0.2066 for price and sq feet total size.
#This tells us there is a sale price accounts to around 20.6% for sq feet total size.
 
#R-squared:  0.2123,	Adjusted R-squared:  0.2121  
#This tells us there is a sale price accounts to around 21% for the factors 
#- sq feet total size,bedroom and bathroom. 
#Which means 79% of the sale cannot be explained by these predictors alone.
#This correlation is however is slightly better than the 20.6%% for price and sq feet total size.

#iv. Considering the parameters of the multiple regression model you have created. 
#What are the standardized betas for each parameter and what do the values indicate?

library(QuantPsyc)
lm.beta(price_vs_sqf_lm)
lm.beta(price_and_more)

##coefficients(price_and_more)
 
#square_feet_total_living has a significant relation to the sale price, 
#implying they have a comparable degree of importance.

#The standardized betas for the linear model "price_and_more" indicates the 
#sale price increases by 0.443831708 standard deviations when there is 
#an increase in standard deviations for the property's size by 
#total living square feet.  

#v. Calculate the confidence intervals for the parameters in your model and 
#explain what the results indicate.
 
confint(price_vs_sqf_lm)
confint(price_and_more) 
#The confidence intervals calculated for "price_vs_sqf_lm" have a small range. 
#The confidence intervals calculated for "price_and_more" have a larger range. 
#In addition, these values cross zero and include negative values. 
#This indicates that the sale price can increase or decrease depending on the 
#number of bedrooms. This makes the output for sale price not consistent. 
#However, the other variables do have better consistency and shorter range.


#vi. Assess the improvement of the new model compared to your original model 
#(simple regression model) by testing whether this change is significant by 
#performing an analysis of variance.

anova(price_vs_sqf_lm)
anova(price_and_more)
anova(price_vs_sqf_lm, price_and_more)
#A non zero F statistic means significant coefficients. 
#P value < 0.05 also implies a significant model. In this case, all predictors 
#have a p value less than 0.05.

#vii. Perform casewise diagnostics to identify outliers and/or influential cases, 
#storing each function's output in a dataframe assigned to a unique variable name.

housing_df_outliers <-  housing_df
names(housing_df_outliers)
#housing_df_outliers <- na.omit(housing_df_outliers)
nrow(housing_df_outliers)
nrow(housing_df)
#rsid  
housing_df_outliers$residuals <- resid(price_and_more) 

#standardized residuals  
housing_df_outliers$stand_residuals <-  rstandard(price_and_more)
#studentized residuals   
housing_df_outliers$studentized_residuals <- rstudent(price_and_more) 
#cooks distance
housing_df_outliers$cooks_distance <- cooks.distance(price_and_more) 
#DFBeta   
housing_df_outliers$dfbeta <- dfbeta(price_and_more) 
#DFFit  
housing_df_outliers$dffits <- dffits(price_and_more) 
#hat values (leverage)  
housing_df_outliers$leverage <- hatvalues(price_and_more) 
#covariance ratio  
housing_df_outliers$covariance_ratio <- covratio(price_and_more)  

names(housing_df_outliers)
#nrow(housing_df_outliers)
#head(housing_df_outliers) 

#viii. Calculate the standardized residuals using the appropriate command, 
#specifying those that are +-2, storing the results of large residuals in a 
#variable you create.
housing_df_outliers$residual_flag <-  
  housing_df_outliers$stand_residuals > 2|housing_df_outliers$stand_residuals < -2

#ix. Use the appropriate function to show the sum of large residuals.
sum(housing_df_outliers$residual_flag)

#320 cases have a large residual (>2 and <-2)

#x.  Which specific variables have large residuals (only cases that evaluate as TRUE)?
housing_df_outliers[housing_df_outliers$residual_flag==TRUE,
                      c("Sale_Price","square_feet_total_living","bedrooms",
                        "bath_full_count","bath_half_count","stand_residuals")]

nrow(housing_df_outliers[housing_df_outliers$residual_flag==TRUE,
                         c("Sale_Price","square_feet_total_living","bedrooms",
                           "bath_full_count","bath_half_count","stand_residuals")])
names(housing_df_outliers)
#320 rows out of 12865

#xi. Investigate further by calculating the leverage, cooks distance, and covariance rations. 
#Comment on all cases that are problematics.
 
nrow(housing_df_outliers[ (housing_df_outliers$residual_flag==TRUE &
                           housing_df_outliers$cooks_distance > 1),])

#No rows have cooks distance > 1,  so none of the cases is having an undue
#influence on the model. 

#Average leverage -  (3(k + 1)/n) a
threshold <- (3*(4 + 1)/12865)  
#threshold - 0.001165954
nrow(housing_df_outliers[housing_df_outliers$residual_flag==TRUE & 
                           housing_df_outliers$leverage > threshold,])
#56 rows have leverage greater than the threshold  


#Covariance ratio 
positive_boundary <- 1 + (3*(4 + 1)/12865)
negative_boundary <- 1 - (3*(4 + 1)/12865)  

nrow(housing_df_outliers[housing_df_outliers$residual_flag==TRUE & 
                           (housing_df_outliers$covariance_ratio > positive_boundary & 
                              housing_df_outliers$covariance_ratio > negative_boundary),])

#10 rows deviate from substantially from the covariance_ratio boundaries. 
 

#xii. Perform the necessary calculations to assess the assumption of independence 
#and state if the condition is met or not.
library(car) 

durbinWatsonTest(price_vs_sqf_lm)
#The value is0.5433798 which is less than 1. This means assumption  of independence is not met.

durbinWatsonTest(price_and_more)
#The value is 0.5437297 which is less than 1. This means assumption  of independence is not met.

#xiii. Perform the necessary calculations to assess the assumption of no 
#multicollinearity and state if the condition is met or not.
library(regclass)
 
vif(price_and_more)
#Largest VIF is not greater than 10. 

1/vif(price_and_more)
#Tolerance is not below 0.1 or 0.2. There is no collinearity within the data.

mean(vif(price_and_more))
#Average Vif (1.48) is greater than 1, the regression may be biased. 

#xiv. Visually check the assumptions related to the residuals using the plot() 
#and hist() functions. Summarize what each graph is informing you of and if any
#anomalies are present.
library(ggplot2)
plot(price_and_more) 
hist(rstudent(price_and_more))

#Histogram distributions is not normal. Q-Q plot does not looks like a diagonal line.
#The plot standardized residuals plotted against the fitted (predicted) values is not
#scattered, which means this is probably a violation of the assumption of homogeneity of variance

 
#xv. Overall, is this regression model unbiased? If an unbiased regression model,
#what does this tell us about the sample vs. the entire population model?


This regression is not entirely unbiased. Based on the vif avaerage, it is possible
the model may be biased.


##FOR OUTLIERS

boxplot(housing_df$Sale_Price)
housing_df_out <- housing_df[!housing_df %in% boxplot.stats(housing_df$Sale_Price)$out]

boxplot(housing_df$Sale_Price)                # Create boxplot of all data
x_out_rm <- housing_df$Sale_Price[!housing_df$Sale_Price %in% boxplot.stats(housing_df$Sale_Price)$out]  # Remove outliers

length(housing_df$Sale_Price) - length(x_out_rm) # Count removed observations

boxplot(x_out_rm)                              # Create boxplot without outliers
