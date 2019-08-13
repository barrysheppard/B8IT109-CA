
#######################################
# Normal prep code                    #
#######################################

# This clears the workspace environment
rm(list = ls())
# This sets the working directory to the same as the file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# This installs all the packages needed if not already loaded
if (!require("pacman")) install.packages("pacman")
pacman::p_load('psych', 'MASS', 'car', 'ggplot2', 'GGally', 'CCA', 'sem', 'cluster')


# Question 4
# Use dataset available on
# http://users.stat.ufl.edu/~winner/data/nfl2008_fga.csv

df <- read.csv('http://users.stat.ufl.edu/~winner/data/nfl2008_fga.csv')
names(df)
head(df)
describe(df)
str(df)

#There are two rows incomplete
df[!complete.cases(data),]
# As this is only 2 out of 1039 observations, we choose to exclude these
data <- df[complete.cases(df),]

# 1. Use LDA to classify the dataset into few classes so that
#    at least 90% of information of dataset is explained through
#    new classification. (Hint: model the variable “qtr” to
#    variables “togo”, “kicker”, and “ydline”). How many LDs do
#    you choose? Explain the reason.

qtrlda <- lda(qtr ~ togo + kicker + ydline, data = data)
qtrlda

# Call:
#   lda(qtr ~ togo + kicker + ydline, data = data)
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

0.615 + 0.322 # 0.937
# LD1 and LD2 combined explain 93.7% of the information and are selected


# 2. Apply PCA, and identify the important principle components
#    involving at least 90% of dataset variation. Explain your
#    decision strategy? Plot principle components versus their
#    variance (Hint: to sketch the plot use the Scree plot).

# We start by removing any of the non-continuous data
# We also remove season as it has 0 variance
keep_columns <- c('qtr', 'min', 'sec', 'down', 'togo','kicker','ydline','distance',
                  'homekick', 'kickdiff', 'timerem', 'offscore', 'defscore',
                  'GOOD', 'Missed', 'Blocked')
data <- data[,keep_columns]

names(data)
describe(data)
fit <- princomp(data, cor = TRUE)

summary(fit)
# Importance of components:
#                           Comp.1    Comp.2    Comp.3     Comp.4     Comp.5     Comp.6
# Standard deviation     2.0048406 1.6721636 1.3083384 1.22081818 1.11060318 1.00083350
# Proportion of Variance 0.2512116 0.1747582 0.1069843 0.09314981 0.07708996 0.06260423
# Cumulative Proportion  0.2512116 0.4259698 0.5329542 0.62610397 0.70319393 0.76579816
#                            Comp.7     Comp.8     Comp.9    Comp.10    Comp.11     Comp.12
# Standard deviation     0.97841761 0.95110310 0.88207039 0.82066687 0.62552037 0.203651798
# Proportion of Variance 0.05983131 0.05653732 0.04862801 0.04209338 0.02445473 0.002592128
# Cumulative Proportion  0.82562948 0.88216680 0.93079481 0.97288819 0.99734293 0.999935053
#                             Comp.13      Comp.14      Comp.15      Comp.16
# Standard deviation     3.223577e-02 3.041048e-08 2.446684e-08 8.914025e-09
# Proportion of Variance 6.494657e-05 5.779982e-17 3.741415e-17 4.966240e-18
# Cumulative Proportion  1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00

loadings(fit)
plot(fit, type = 'lines')
biplot(fit)
# From the screeplot we have a couple of options we could choose from
# Comp1 + Comp2 + Comp3 explains 53% of the data and the increase to 4 is only 9%
# Another possibile point of inflection could be 6 which explains 76% and the inrease
# to 7 is only 6%

# in this quesiton however, we're being asked to explain 90% of the dataset variation
# for that we need a total of 9 components which explain 93%



# 3. Split the dataset into two sets of variables so that
#    X=( togo,kicker,ydline) and Y=( distance, homekick).

names(data)
x <- data[,5:7]
y <- data[,8:9]
names(x)
# [1] "togo"   "kicker" "ydline"
names(y)
# [1] "distance" "homekick"
  
#    Apply
#    canonical correlation analysis to find the cross-correlation
#    between X and Y. What is the correlation between ydline and
#    distance?


# display the canonical correlations
cc1 <- cc(x, y)
cc1$cor
# [1] 0.99894989 0.06975549

cor(x, y)  # correlation between two set of variables
#            distance    homekick
# togo    0.315641454 -0.04838438
# kicker -0.001951722 -0.02363159
# ydline  0.998947222  0.04295427


# The correlation betweel ydline and distance is 0.998947222
# So positively correlated and almost 1!

#    Use K-means clustering analysis to identify the most
#    important classes. How many classes do you select? Why?


# We use this wssplot function is to work out the lowest number of clusters
# with the highest amount of variation (information)
wssplot <- function(data, nc=10, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)}
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")}

wssplot(data, nc = 10) 
# This scree plot gives us an indication of the number of clusters we should be
# looking for. In this example, after 4 there is no major change.
# We could also make a case for 3, but 4 appears to be a better point of inflection

# generate the clusters
k.means.fit <- kmeans(data, 4) # the 4 indicates the number of groups previous selected

# We can also plot this to visualise the 4 groups
library(cluster)
clusplot(data, k.means.fit$cluster, main = '2D representation of the Cluster solution',
         color = TRUE, shade = TRUE,
         labels = 2, lines = 0)

