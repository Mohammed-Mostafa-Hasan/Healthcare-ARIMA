library(tidyverse)
#read data
flu<-read.csv("Data/french_flu.csv")
head(flu)

#find out all missing value in all columns

map(flu,~sum(is.na(.)))

