
#######################################
# Normal prep code                    #
#######################################

# This clears the workspace environment
rm(list = ls())
# This sets the working directory to the same as the file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# This installs all the packages needed if not already loaded
if (!require("pacman")) install.packages("pacman")
pacman::p_load('psych', 'MASS', 'car')

# Reference code from
# https://rpubs.com/ifn1411/LDA

# Question 4
# Use dataset available on
# http://users.stat.ufl.edu/~winner/data/nfl2008_fga.csv

df <- read.csv('http://users.stat.ufl.edu/~winner/data/nfl2008_fga.csv')
names(df)
head(df)
describe(df)

# 1. Use LDA to classify the dataset into few classes so that
#    at least 90% of information of dataset is explained through
#    new classification. (Hint: model the variable “qtr” to
#    variables “togo”, “kicker”, and “ydline”). How many LDs do
#    you choose? Explain the reason.

qtrlda <- lda(qtr ~ togo + kicker + ydline, data = df)
qtrlda

# Call:
#   lda(qtr ~ togo + kicker + ydline, data = df)
# 
# Prior probabilities of groups:
#   1          2          3          4          5 
# 0.20636451 0.35969142 0.17550627 0.24590164 0.01253616 
# 
# Group means:
#   togo   kicker   ydline
# 1 6.481308 19.64486 17.22897
# 2 6.973190 18.77212 19.30027
# 3 6.543956 19.96703 19.03297
# 4 6.792157 20.20000 18.53725
# 5 5.923077 22.61538 19.53846
# 
# Coefficients of linear discriminants:
#   LD1         LD2         LD3
# togo    0.06665269  0.12498308  0.20996464
# kicker -0.04134867 -0.06009657  0.05013225
# ydline  0.07726467 -0.07173243 -0.02257770
# 
# Proportion of trace:
#   LD1   LD2   LD3 
# 0.615 0.322 0.063 


qtrldavalues <- predict(qtrlda)

ldahist(qtrldavalues$x[,1], g = qtr)


newdata <- data.frame(type = df[,1], lda = qtrldavalues$x)
library(ggplot2)
ggplot(newdata) + geom_point(aes(lda.LD1, lda.LD2, colour = type), size = 2.5)


# 2. Apply PCA, and identify the important principle components
#    involving at least 90% of dataset variation. Explain your
#    decision strategy? Plot principle components versus their
#    variance (Hint: to sketch the plot use the Scree plot).

# 3. Split the dataset into two sets of variables so that
#    X=( togo,kicker,ydline) and Y=( distance, homekick). Apply
#    canonical correlation analysis to find the cross-correlation
#    between X and Y. What is the correlation between ydline and
#    distance?
#    Use K-means clustering analysis to identify the most
#    important classes. How many classes do you select? Why?
