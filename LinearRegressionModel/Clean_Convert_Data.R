#import library
library(dplyr) #data manipulation
library(ggplot2) #data visualization
library(DT) #beautify the data table
library(plotly) #make a pie chart
library(Metrics) #find error value
library(Hmisc)
library(cowplot)
library(WVPlots)

#Load file
data_insurance <- read.csv("./DATA/insurance.csv")

#Understanding data
describe(data_insurance)

#EDA
#Correlation age and charge

#Correlation bmi and charge

#Correlation charge and sex

#Correlation charge and Children

#Correlation charge and smoker

#Correlation charge and Region

