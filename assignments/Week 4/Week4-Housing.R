---
output:
  pdf_document: default
  html_document: default
---

# Assignment: Week 4 - Housing
# Name: Ramani, Aarti
# Date: 2023-01-05
```{r}
library(readxl)
setwd("C:/Masters/GitHub/Winter2022/Ramani-DSC520")  
housing_df <- read_excel(path = "C:/Masters/GitHub/Winter2022/Ramani-DSC520/data/week-6-housing.xlsx",
                         .name_repair = function(col){ gsub(" ", "_", col) }) 
names(housing_df)

#Use the apply function on a variable in your dataset
apply(housing_df,2,range)

#Use the aggregate function on a variable in your dataset
aggregate(cbind(Sale_Price, bedrooms) ~ ctyname + zip5, housing_df, mean)

#Use the plyr function on a variable in your dataset – more specifically, 
#I want to see you split some data, perform a modification to the data, 
#and then bring it back together
library(plyr)
zip_df = subset(housing_df, zip5==98052)
nonzip_df = subset(housing_df, zip5!=98052) 
#zip_df
# Requires dplyr library for case_when -> zip_df <- ddply(zip_df, .(zip5), mutate, ctyname = case_when(is.na(ctyname)&zip5==98052 ~ "REDMOND", TRUE~ctyname))
zip_df <- ddply(zip_df, .(zip5), mutate, ctyname = ifelse(is.na(ctyname)&zip5==98052,"REDMOND", ctyname))
housing_df <- merge(zip_df, nonzip_df, all=TRUE)
 

#Check distributions of the data
library(fitdistrplus)
descdist(housing_df$Sale_Price)
#plotdist(housing_df$Sale_Price, histo = TRUE, demp = TRUE,pch = 19)
plot(fitdist(housing_df$Sale_Price, "norm"))


#Identify if there are any outliers
library(ggplot2)
#ggplot(housing_df, aes(x=Sale_Price, y=prop_type))+ geom_point()
ggplot(housing_df, aes(x=Sale_Price))+ geom_histogram(fill="gray",bins=10, color="black")
#ggplot(housing_df, aes(x=Sale_Price))+ geom_boxplot()
#Answer: There are no outliers in the data

#Create at least 2 new variables
State <- rep("California",12865)
Index <- c(1:12865)

housing <- data.frame(housing_df, State, Index)
colnames(housing)
