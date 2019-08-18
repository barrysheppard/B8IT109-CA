###############################################################################
# Description     : CA for B8IT109 Advanced Data Analytics
# Lecturer        : Dr Shahram Azizi
# Author          : Barry Sheppard - Student Number 10387786
# Date            : 2019/08/18
# Notes           : Question 2
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
pacman::p_load('psych', 'caret', 'e1071')

# Question 2
# Use dataset available on http://users.stat.ufl.edu/~winner/data/nfl2008_fga.csv , then:
# (a) Train the model using 80% of this dataset and suggest an appropriate GLM to model homekick to togo, ydline and kicker variables.
# (b) Specify the significant variables on homekick at the level of
# 𝛼=0.05, and estimate the parameters of your model.
# (c) Predict the test dataset using the trained model.
# (d) Provide the confusion matrix and obtain the probability of correctness of predictions.

# First we load and review the dataset
df <- read.csv("http://users.stat.ufl.edu/~winner/data/nfl2008_fga.csv")

head(df)
names(df)
str(df)
describe(df)

# (a) Train the model using 80% of this dataset and suggest an
# appropriate GLM to model homekick to togo, ydline and kicker
# variables.

df <- na.omit(df)

# Split the dataset into 80 train and 20 test.
n <- nrow(df)
indexes <- sample(n,n*(80/100))
trainset <- df[indexes,]
testset <- df[-indexes,]


# To determine what #type of model to use we need to look
# at the ourcome variable homekick
str(df$homekick)
max(df$homekick) #1
min(df$homekick) #0


# As the outcome is binary, we create a glm model using binomial
# family
model <- glm(homekick ~ togo + ydline + kicker, data = trainset, family = 'binomial')

# (b) Specify the significant variables on homekick at the level
# of 𝛼=0.05, and estimate the parameters of your model

summary(model)

# Call:
#   glm(formula = homekick ~ togo + ydline + kicker, family = "binomial", 
#       data = trainset)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.3384  -1.1532  -0.9887   1.1876   1.4596  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)  0.13064    0.20628   0.633   0.5265  
# togo        -0.04219    0.01795  -2.351   0.0187 *
# ydline       0.01156    0.00753   1.535   0.1247  
# kicker      -0.00550    0.00620  -0.887   0.3750  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1150.1  on 829  degrees of freedom
# Residual deviance: 1143.2  on 826  degrees of freedom
# (1 observation deleted due to missingness)
# AIC: 1151.2
# 
# Number of Fisher Scoring iterations: 


# at alpha of 0.05 only togo is significant
# Using this we can estimate the parameters as
# Intercept = 0 (as non-sig), beta for togo is - 0.044219
# all other non-sigificant parameters are set to 0
# homekick =  0 - 0.044219 * togo


# Let's rerun the model removing the other non-signifcant items

model2 <- glm(homekick ~ togo, data=trainset, family='binomial')
summary(model2)
# 
# Call:
#   glm(formula = homekick ~ togo, family = "binomial", data = trainset)
# 
# Deviance Residuals: 
#   Min      1Q  Median      3Q     Max  
# -1.237  -1.166  -1.025   1.188   1.419  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)  0.17100    0.13233   1.292   0.1963  
# togo        -0.03283    0.01691  -1.942   0.0522 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1150.1  on 829  degrees of freedom
# Residual deviance: 1146.3  on 828  degrees of freedom
# (1 observation deleted due to missingness)
# AIC: 1150.3
# 
# Number of Fisher Scoring iterations: 3

# This model ends up as
# homekick = 0.17100 - 0.03283 * togo
# although, interestingly in this summary togo is no longer
# significant at the 0.05 level!


# (c) Predict the test dataset using the trained model.

predicted_data <- predict(model2,testset, type='response')
# convert that to 1s and 0s
p_data <- as.integer(predicted_data > 0.5)

# (d) Provide the confusion matrix and obtain the probability
# of correctness of predictions.

# We will us use caret to compute a confusion matrix
confusionMatrix(data = as.factor(p_data),
                reference = as.factor(testset$homekick))

# # Confusion Matrix and Statistics
# 
# Reference
# Prediction  0  1
#          0 52 60
#          1 48 47
# 
# Accuracy : 0.4783          
# 95% CI : (0.4085, 0.5486)
# No Information Rate : 0.5169          
# P-Value [Acc > NIR] : 0.8814          
# 
# Kappa : -0.0406         
# 
# Mcnemar's Test P-Value : 0.2898          
#                                           
#             Sensitivity : 0.5200          
#             Specificity : 0.4393          
#          Pos Pred Value : 0.4643          
#          Neg Pred Value : 0.4947          
#              Prevalence : 0.4831          
#          Detection Rate : 0.2512          
#    Detection Prevalence : 0.5411          
#       Balanced Accuracy : 0.4796          
#                                           
#        'Positive' Class : 0     

# So this model has a 0.4783 prediction rate.

# The below is another way to do accuracy
tab <- table(p_data, testset$homekick) # Confusion matrix
tab

# p_data  0  1
#      0 52 60
#      1 48 47

model_accuracy <- sum( tab[row(tab) == col(tab)] ) / sum(tab)
model_accuracy # 0.4783

