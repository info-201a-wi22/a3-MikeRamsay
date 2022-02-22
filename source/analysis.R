#Code goes here
library("dplyr")
library("ggplot2")
library("plotly")
library("readr")

urlfile="https://raw.githubusercontent.com/vera-institute/incarceration-trends/ae6a18945c23b56aa8d4b00ff83090d83a171fac/incarceration_trends.csv"

incarceration_dataset <- read_csv(url(urlfile))
spec(incarceration_dataset)
View(incarceration_dataset)
dim(incarceration_dataset)



