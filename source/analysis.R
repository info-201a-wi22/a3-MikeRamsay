library("dplyr")
library("ggplot2")
library("plotly")
library("readr")

#Loading dataset into R:
urlfile="https://raw.githubusercontent.com/vera-institute/incarceration-trends/ae6a18945c23b56aa8d4b00ff83090d83a171fac/incarceration_trends.csv"

incarceration_dataset <- read_csv(url(urlfile))
spec(incarceration_dataset)
View(incarceration_dataset)
dim(incarceration_dataset)

#-------------------------------------------------------------------------------
#My 5 values of interest:

#Question #1: What state has the highest portion of its residents in jails?
#Use total_pop_15to64 for this - incorporate race?


#Question #2: Does commuting zone have an affect on jail population?


#Question #3: How has total jail population changed from 1970 to 2018, by race?


#Question #4: What what race has the largest jail population, proportional to its own population?


#Question #5: Which state has the largest proportion of (prev answer) people in jails?


#-------------------------------------------------------------------------------
#Trends Over Time Chart:
#(Use Q 3- yr vs pop)


#-------------------------------------------------------------------------------
#Variable Comparison Chart:
#(Use Q 2)


#-------------------------------------------------------------------------------
#Map:
#(Use Q 1)


