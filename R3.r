##testing error-performance of model on prev seen data
##train error-performance of model on unseen data.
#for linear regression-- variance increases with flexibility.
#bias decreases with flexibility. flexibilty is high if graph ia able to touch as 
#many points as possible.
##TESTING AND TRAINING DATASETS
install.packages("caTools")
##set.seed() at aparticular value will give same split for all.
##observation output will also be same for all working on the program.
##so, to avoid random input-output we use set.seed().
library(caTools)
set.seed(0)
split=sample.split(df,SplitRatio = 0.8)
training_set = subset(df,split == TRUE)
test_set = subset(df,split == FALSE)
lm_a = lm(price~.,data=training_set)
###linear regression model on training dataset.
train_a = predict(lm_a,training_set)
##above command will take all independent variables from training_set, put it 
##into lm_a model and predict the value of independent variable and store it into train_a.
test_a = predict(lm_a,test_set)
mean((training_set$price - train_a)^2)
###above is command to get mean squared error on training data. which is 20.66384
mean((test_set$price - test_a)^2)
##the model is not performing well on the data as means of the 2 are very far from each other.
#################################################################
#Use linear regression to find the relation between mpg and other variables
#Questions for this assignment
#The file mtcars provides the following information for 392 different car models:
#??? Cylinders
#??? Displacement
#??? Horsepower
#??? Weight
#??? Acceleration
#??? Miles per gallon (MPG)
#Determine an equation that can predict MPG.
set.seed(0)
split=sample.split(mtcars,SplitRatio=0.8)
training_set=subset(mtcars,split==TRUE)
test_set=subset(mtcars,split==FALSE)
lm_a=lm(mpg~.,data=training_set)
train_a=predict(lm_a,training_set)
test_a=predict(lm_a,test_set)
mean((training_set$wt-train_a)^2) 
mean((test_set$wt-test_a)^2)
summary(split)
summary(train_a)
summary(test_a)
summary(lm_a)
multiple_model <- lm(mpg~.,data=mtcars)
summary(multiple_model)
library(readxl)
df1 <- read_excel("C:/Cardata.xlsx")
df1
multiple_model <- lm(mpg~.,data=df1)
summary(multiple_model)
#The file Cardata.xlsx provides the following information for 392 different car models:
#??? Cylinders
#??? Displacement
#??? Horsepower
#??? Weight
#??? Acceleration
#??? Miles per gallon (MPG)
#Determine an equation that can predict MPG.
library(readxl)
df1 <- read_excel("C:/Cardata.xlsx")
df1
multiple_model <- lm(mpg~.,data=df1)
summary(multiple_model)
#output screen--
  
  #Call:
  
  #lm(formula = mpg ~ ., data = df1)

#Residuals:
  
 # Min       1Q   Median       3Q      Max 

#-11.5816  -2.8618  -0.3404   2.2438  16.3416 

#Coefficients:
  
 # Estimate Std. Error t value Pr(>|t|)    

#(Intercept)  4.626e+01  2.669e+00  17.331   <2e-16 ***
  
 # cyl         -3.979e-01  4.105e-01  -0.969   0.3330    

#disp        -8.313e-05  9.072e-03  -0.009   0.9927    

#HP          -4.526e-02  1.666e-02  -2.716   0.0069 ** 
  
 # wt          -5.187e-03  8.167e-04  -6.351    6e-10 ***
  
  #accel       -2.910e-02  1.258e-01  -0.231   0.8171    

#---
  
  #Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 4.247 on 386 degrees of freedom

#Multiple R-squared:  0.7077,	Adjusted R-squared:  0.7039 

#F-statistic: 186.9 on 5 and 386 DF,  p-value: < 2.2e-16

#answer---
  
 # mpg = 4.626e+01 - 3.979e-01 * cyl - 8.313e-05 * disp - 4.526e-02 * HP - 5.187e-03 * 
 #   wt - 2.910e-02 * accel
#IN LINEAR REGRESSION IF NO. OF OBSERVATION >> NO. OF VARIABLES THEN NO ALTERNATIVE APPROACH IS REQ
#IF NO. OF VARIABLES > NO. OF OBSERVATIONS THERE WILL BE VARIABLITY AND INFINITE
#NO. OF SOLUTIONS AVAILABLE.
#Other linear models of linear regression 2 types of methods--
#Subset Selection-instead of training model on all variables we will be using a subset of 
#available variables.
#Shrinkage method- try to shrink the coefficient of some variables towards zero.
#Subset Selection In R:
install.packages("leaps")
library(leaps)
#BEST SUBSET SELECTION
lm_best=regsubsets(price~.,data=df)##this command runs for 8 variables only.
#But we req for all our 15 variables,nvmax=15.
lm_best= regsubsets(df$price~.,data= df,nvmax= 15)
summary(lm_best)
summary(lm_best)$adjr2 ##adjusted R^2 FOR all 15 columns
##to get maximum values out of above values
which.max(summary(lm_best)$adjr2)
#to get coefficients for the model
coef(lm_best,8)# we get the intercept as well as beta values for R^2.
##forward stepwise selection
lm_forward = regsubsets(df$price~.,data= df,nvmax= 15 , method = "forward")
summary(lm_forward)
##backward stepwise selection
lm_backward = regsubsets(df$price~.,data= df,nvmax= 15 , method = "backward")
summary(lm_backward)
#Ridge AND Lasso Regression--try to shrink coefficients of variable towards zero by adding shrinkage penalty
install.packages("glmnet")
library(glmnet)
x=model.matrix(price~.,data=df)[,-1]
#the above does not have 16th variable i.e. price.
y=df$price
##the above is dependent variable.
grid=10^seq(10,-2,length=100)
grid
lm_ridge = glmnet(x,y,alpha=0,lambda = grid)
##alpha=0 for ridge regression and 1 for lasso regression
summary(lm_ridge)
cv_fit=cv.glmnet(x,y,alpha=0,lambda = grid)
plot(cv_fit)
opt_lambda = cv_fit$lambda.min
###above is value of lambda for which mean square error is min
tss = sum((y-mean(y))^2)
y_a = predict(lm_ridge,s=opt_lambda,newx=x)
rss=sum((y_a-y)^2)
rsq= 1-rss/tss
#lasso regression is done in exactly same way as above but alpha=1.
lm_lasso = glmnet(x,y,alpha=1,lambda = grid)
#other steps are same as above .
#calc rsq i.e. R^2 from lasso and ridge and check which gives more prediction
#accuracy.