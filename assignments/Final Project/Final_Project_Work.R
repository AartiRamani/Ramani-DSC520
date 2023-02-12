library(readxl)
## Set the working directory to the root of your DSC 520 directory
setwd("C:/Masters/Data/AirlineDelayData/")

## Load housing data 
housing_df <- read.csv(path = "C:/Masters/Data/AirlineDelayData/airlines.csv") 
names(housing_df)

setwd("C:/Masters/Data/AirlineDelayData/") 
airlines_df <- read.csv("airlines.csv")
flights_df <- read.csv("flights.csv")
airports_df <- read.csv("airports.csv")

names(airlines_df)
names(flights_df)
names(airports_df)
nrow(airlines_df)
nrow(flights_df)
nrow(airports_df)