#title: "Assignment 10.2"
#author: "Aarti Ramani"
#date: "2023-02-11"
#output:
#  pdf_document: default
#html_document: default 

## Set the working directory to the root of your DSC 520 directory
setwd("C:/Masters/GitHub/Winter2022/Ramani-DSC520")

library(foreign)
thoraric_df <- read.arff("C:/Masters/GitHub/Winter2022/Ramani-DSC520/data/ThoraricSurgery.arff")
nrow(thoraric_df)
head(thoraric_df)
names(thoraric_df)

thoraric_survive <-  thoraric_df[thoraric_df$Risk1Yr == "F",]
head(thoraric_survive)
nrow(thoraric_survive)

thoraric_nonsurvive <-  thoraric_df[thoraric_df$Risk1Yr == "T",]
head(thoraric_nonsurvive)
nrow(thoraric_nonsurvive)


#library(ggplot2)
#library(useful)
#ggplot(thoraric_df, aes(x=Risk1Yr)) + geom_density(fill="grey", color="grey")   

#1. DGN: Diagnosis - specific combination of ICD-10 codes for primary and secondary as well multiple tumours if any (DGN3,DGN2,DGN4,DGN6,DGN5,DGN8,DGN1)
#2. PRE4: Forced vital capacity - FVC (numeric)
#3. PRE5: Volume that has been exhaled at the end of the first second of forced expiration - FEV1 (numeric)
#4. PRE6: Performance status - Zubrod scale (PRZ2,PRZ1,PRZ0)
#5. PRE7: Pain before surgery (T,F)
#6. PRE8: Haemoptysis before surgery (T,F)
#7. PRE9: Dyspnoea before surgery (T,F)
#8. PRE10: Cough before surgery (T,F)
#9. PRE11: Weakness before surgery (T,F)
#10.PRE14: T in clinical TNM - size of the original tumour, from OC11 (smallest) to OC14 (largest) (OC11,OC14,OC12,OC13)
#11.PRE17: Type 2 DM - diabetes mellitus (T,F)
#12.PRE19: MI up to 6 months (T,F)
#13.PRE25: PAD - peripheral arterial diseases (T,F)
#14.PRE30: Smoking (T,F)
#15.PRE32: Asthma (T,F)
#16.AGE: Age at surgery (numeric)
#17.Risk1Y: 1 year survival period - (T)rue value if died (T,F)
 

#Fit a binary logistic regression model to the data set that predicts whether or 
#not the patient survived for one year (the Risk1Y variable) after the surgery. 
#Use the glm() function to perform the logistic regression. 
#See Generalized Linear Models for an example. 
#Include a summary using the summary() function in your results.

result.0 <- glm(Risk1Yr ~ 1, data = thoraric_df, family = binomial())
summary(result.0)

result.1 <- glm(Risk1Yr ~ DGN + PRE4 +PRE5 +PRE6 +PRE7 +PRE8 +PRE9 +PRE10+
                PRE11 +PRE14 +PRE17 +PRE19 +PRE25 +PRE30 +PRE32+
                AGE ,data = thoraric_df, family=binomial(link="logit"))
summary(result.1)

#According to the summary, which variables had the greatest effect on the survival rate? 
PRE9T, PRE14OC14, PRE17, PRE30 have p-value < 0.05. 
These variables have the greatest effect on the survival rate


#To compute the accuracy of your model, use the dataset to predict the outcome variable. 
#The percent of correct predictions is the accuracy of your model. 
#What is the accuracy of your model?

library(coefplot)
coefplot(result)

invlogit(result$coefficients)

As the age goes up the risk value goes up by 0.003956 
Smoking increases the risk of survival increase by 0.879

modelChi <- result.1$null.deviance - result.1$deviance
modelChi

#Degrees of freedom for the model result.1 subtracted from 
#the degrees of freedom for the null model
chidf <- result.1$df.null - result.1$df.residual
chidf


chisq.prob <- 1 - pchisq(modelChi, chidf)
chisq.prob
#Value is <0.002 (<0.05)
thoraric_df$model_prob <- predict(result, thoraric_df, type = "response")
thoraric_df$model_prob 


chisquare statistic = Null Deviance - Residual Deviance
395.61 - 341.19 = 54.42
The difference is fairly high which tells us the model is a good fit.

 
