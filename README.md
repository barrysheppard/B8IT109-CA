# B8IT109-CA
Coursework for the B8IT109 Advanced Data Analytics course


###Question 1
Use in-built dataset ‘airquality’,
1. Explore the general feature of dataset using appropriate R functions. (5 Marks)
2. Perform data cleansing if required. (5 Marks)
3. Consider ‘Temp’ attributes and compute the central and variational measures. (10 Marks)
4. Apply boxplot technique to detect outlier of ‘wind’ attribute if any. (10 Marks)
(Total: 30 Marks)

###Question 2
Use dataset available on http://users.stat.ufl.edu/~winner/data/nfl2008_fga.csv, then:
1. Train the model using 80% of this dataset and suggest an appropriate GLM to model homekick to togo, ydline and kicker variables. (5 Marks)
2. Specify the significant variables on homekick at the level of alpha=0.05, and estimate the parameters of your model. (5 Marks)
3. Predict the test dataset using the trained model. (5 Marks)
4. Provide the confusion matrix and obtain the probability of correctness of predictions.(10 Marks)
(Total: 25 Marks)

###Question 3
Using Yahoo Finance API, select a specific stock market price, apply time series analysis, consider ‘close price' as your time series variable:
1. Validate the assumptions using graphical visualization. (5 Marks)
2. Fit the optimized model for ‘close price’ and provide the coefficient estimates for the fitted model. (5 Marks)
3. What is the estimated order for AR and MA? (5 Marks)
4. Forecast h=10 step ahead prediction of wage on the plot of the original time series. (10 Marks)
(Total: 25 Marks)

###Question 4
Use dataset available on http://users.stat.ufl.edu/~winner/data/nfl2008_fga.csv
1. Use LDA to classify the dataset into few classes so that at least 90% of information of dataset is explained through new classification. (Hint: model the variable "qtr"to variables "togo", "kicker", and " "ydline"). How many LDs do you choose? Explain the reason. (5 Marks)
2. Apply PCA, and identify the important principle components involving at least 90% of dataset variation. Explain your decision strategy?  Plot principle components versus their variance (Hint: to sketch the plot use the Scree plot) (5 Marks)
3. Split the dataset into two sets of variables so that X=(togo,kicker,ydline) and Y=(distance, homekick). Apply canonical correlation analysis to find the cross-correlation between Xand Y. What is the correlation between ydlineand distance? (5 Marks)
4. Use K-means clustering analysis to identify the most important classes. How many classes do you select? Why? (6 Marks)
(Total: 20 Marks)
