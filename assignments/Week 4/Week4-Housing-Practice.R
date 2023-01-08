# Assignment: Week 4 - Housing
# Name: Ramani, Aarti
# Date: 2023-01-05
library(readxl)
setwd("C:/Masters/GitHub/Winter2022/Ramani-DSC520")  
housing_df <- read_excel(path = "C:/Masters/GitHub/Winter2022/Ramani-DSC520/data/week-6-housing.xlsx",
                         .name_repair = function(col){ gsub(" ", "_", col) }) 
names(housing_df)

#survey_df <- read.csv(file="C:/Masters/GitHub/Winter2022/Ramani-DSC520/data/acs-14-1yr-s0201.csv")
#survey_df

#Use the apply function on a variable in your dataset
apply(housing_df,2,range)

#Use the aggregate function on a variable in your dataset
aggregate(cbind(Sale_Price, bedrooms) ~ ctyname + zip5, housing_df, mean)
#PRACTICE
#list(housing_df$ctyname)
#aggregate(cbind(housing_df$Sale.Price, housing_df$bedrooms), list(housing_df$ctyname, housing_df$zip5), mean)
#Validate mean of sale price from aggregrate function for the city REDMOND
#aggregate(Sale.Price ~ ctyname, housing_df, mean)
#mean(subset(housing_df, housing_df$ctyname=="REDMOND")$Sale.Price) 
#mean(housing_df[housing_df$ctyname=="REDMOND",]$Sale.Price)
#aggregate(cbind(Sale.Price, bedrooms) ~ ctyname + zip5, upd_housing_df, mean)
#aggregate(cbind(Sale.Price, bedrooms) ~ ctyname + zip5, housing_df, mean)

#Use the plyr function on a variable in your dataset – more specifically, I want to see you split some data, perform a modification to the data, and then bring it back together
#library(dplyr)
#library(tidyr)
#upd2_houseing_df <- housing_df %>% separate(sale_warning , c("sale_warning_1", "sale_warning_2","sale_warning_3", "sale_warning_4", "sale_warning_5"))
#housing_df %>% filter(ctyname=='' & zip5==98052)
#upd_housing_df <- housing_df %>% mutate(ctyname = replace(ctyname, zip5==98052 & ctyname=='', "REDMOND"))
library(plyr)
zip_df = subset(housing_df, zip5==98052)
nonzip_df = subset(housing_df, zip5!=98052)
#zip_df[zip_df$zip5==98052 & is.na(zip_df$ctyname),]
zip_df <- ddply(zip_df, .(zip5), mutate, ctyname = case_when(is.na(ctyname)&zip5==98052 ~ "REDMOND", TRUE~ctyname))
housing_df <- full_join(zip_df, nonzip_df)

#Check distributions of the data
library(fitdistrplus)
#descdist(housing_df$Sale_Price)
#plotdist(housing_df$Sale_Price, histo = TRUE, demp = TRUE,pch = 19)
plot(fitdist(housing_df$Sale_Price, "norm"))


#Identify if there are any outliers
library(ggplot2)
#ggplot(housing_df, aes(x=Sale_Price, y=prop_type))+ geom_point()
ggplot(housing_df, aes(x=Sale_Price))+ geom_histogram(fill="gray",bins=20, color="black")
#ggplot(housing_df, aes(x=Sale_Price))+ geom_boxplot()



#Create at least 2 new variables
State <- rep("California",12865)
Index <- c(1:12865)

housing <- data.frame(housing_df, State, Index)
colnames(housing)
