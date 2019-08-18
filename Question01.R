###############################################################################
# Description     : CA for B8IT109 Advanced Data Analytics
# Lecturer        : Dr Shahram Azizi
# Author          : Barry Sheppard - Student Number 10387786
# Date            : 2019/08/18
# Notes           : Question 1
###############################################################################

#######################################
# Normal prep code                    #
#######################################

# This clears the workspace environment
rm(list = ls())
# This sets the working directory to the same as the file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# This installs all the packages needed if not already loaded
if (!require("pacman")) install.packages("pacman")
pacman::p_load("psych")


# Question 1
# Use in-built dataset ‘airquality’,
# a)explore the general feature of dataset using appropriate R functions.
# (5 Marks)
# b)perform data cleansing if required.
# (5 Marks)
# c)consider ‘Temp’ attributes and compute the central and variational 
# measures.
# (10 Marks)
# d)apply boxplot technique to detect outlier of ‘wind’ attribute if any.
# (10 Marks)

data("airquality")
df <- airquality

# Part a)

# First we look at the dataset structure
str(df)
# 'data.frame':	153 obs. of  6 variables:
# $ Ozone  : int  41 36 12 18 NA 28 23 19 8 NA ...
# $ Solar.R: int  190 118 149 313 NA NA 299 99 19 194 ...
# $ Wind   : num  7.4 8 12.6 11.5 14.3 14.9 8.6 13.8 20.1 8.6 ...
# $ Temp   : int  67 72 74 62 56 66 65 59 61 69 ...
# $ Month  : int  5 5 5 5 5 5 5 5 5 5 ...
# $ Day    : int  1 2 3 4 5 6 7 8 9 10 ...

# we can look at some of the variable values using the psych package describe
describe(df)

# vars   n   mean    sd median trimmed   mad  min   max range  skew kurtosis
# Ozone      1 116  42.13 32.99   31.5   37.80 25.95  1.0 168.0   167  1.21     1.11
# Solar.R    2 146 185.93 90.06  205.0  190.34 98.59  7.0 334.0   327 -0.42    -1.00
# Wind       3 153   9.96  3.52    9.7    9.87  3.41  1.7  20.7    19  0.34     0.03
# Temp       4 153  77.88  9.47   79.0   78.28  8.90 56.0  97.0    41 -0.37    -0.46
# Month      5 153   6.99  1.42    7.0    6.99  1.48  5.0   9.0     4  0.00    -1.32
# Day        6 153  15.80  8.86   16.0   15.80 11.86  1.0  31.0    30  0.00    -1.22
# se
# Ozone   3.06
# Solar.R 7.45
# Wind    0.28
# Temp    0.77
# Month   0.11
# Day     0.72

# Part b

# Lets see if there are any missing values
df[!complete.cases(df),]
# So yeah, we can see there are rows with missing data
# Lets look at it by column and get some counts.
sum(is.na(df$Ozone)) # 37 missing items
sum(is.na(df$Solar.R)) # 7 missing items
sum(is.na(df$Wind)) # 0 missing items
sum(is.na(df$Temp)) # 0 missing items
sum(is.na(df$Month)) # 0 missing items
sum(is.na(df$Day)) # 0 missing items

# As we are going to be looking at the Temp and Wind attributes which have
# no missing items, we can conclude there is no need to have concern
# about the missing balues for Ozone and Solar.R

# Part c)

describe(df$Temp)
#     vars   n  mean   sd median trimmed mad min max range  skew kurtosis   se
# X1    1  153 77.88 9.47     79   78.28 8.9  56  97    41 -0.37    -0.46 0.77
mean(df$Temp)
sd(df$Temp)
# the mean temperature is 77.88 with a standard deviation of 9.47

# Part d) 
boxplot(df$Wind)
# From the pot we can there are 3 outliers at the top of the range
tail(sort(df$Wind), 3)
# 18.4 20.1 20.7

