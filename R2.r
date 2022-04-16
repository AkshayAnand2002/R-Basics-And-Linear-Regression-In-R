df <-read.csv("C:/House_Price.csv",header = TRUE)
View(df)
str(df)
summary(df)##EDD(EXPLORATORY DATA DICTIONARY IN R FOR EACH COLUMN)
hist(df$crime_rate)
##freq vs crime_rate
pairs(~price+crime_rate+n_hot_rooms+rainfall,data=df)
##above command draws scatterplot for 4 different axes written above
barplot(table(df$airport))
barplot(table(df$waterbody))
barplot(table(df$bus_ter))
##from above drawn barplots for diff columns
##observations
#n_hot_rooms and rainfall has outliers.
#n_hos_beds have missing values.
#bus_ter is a useless variable.
#crime_rate has some other functional relationship with price.
##TO HANDLE OUTLIERS, we will capping and flooring technique 
##Cap upper value to 3*(99th percentile)
##Cap lower value to 0.3*(1st percentile)
##use above step for data having outliers i.e n_hot_rooms and rainfall
##Below command to get req perceentile value.
quantile(df$n_hot_rooms,0.99)##to get 99th percentile value
uv=3*quantile(df$n_hot_rooms,0.99)##calculating upper value
uv
df$n_hot_rooms[df$n_hot_rooms > uv] <- uv ##all the values in n_hot_rooms columns
## which are greater than uv are asigned value uv.
summary(df$n_hot_rooms)##EDD on n_hot_rooms
## for n_hot_romms we calc uv as by data visualization plots above we saw it has outliers on 
## positive side i.e. some values were very much greater than other values.
##for rainfall we saw that it had outlier data on negative side i.e. some values are very much
##smaller than other values. so we calc lv.
lv = 0.3 * quantile(df$rainfall,0.01)
lv
df$rainfall[df$rainfall < lv] <- lv##values < lv changed to lv
summary(df$rainfall)##EDD on rainfall
##For 2 outliers above we got mean very near to median.
###if we want minimum very near to 1st quantile we should use other multiplier 
## than 0.3 i.e. value lower than 0.3
##Now starting missing value imputation
## in data we have missing values in n_hos_beds so using mean from 
##available values in column to fill the empty column.
#mean(df$n_hos_beds) will not give mean as data has NA values
mean(df$n_hos_beds,na.rm=TRUE)##na.rm removes na values in the data column.
which(is.na(df$n_hos_beds))## to get cell no. which is na in column.
df$n_hos_beds[is.na(df$n_hos_beds)] <- mean(df$n_hos_beds,na.rm=TRUE)
###To replace null values in the data column by mean. 
summary(df$n_hos_beds)
## we don't get NA.
which(is.na(df$n_hos_beds))
##check if any NA or not.
##DONE MISSING VALUE IMPUTATION ABOVE.
##crime_rate to be made for more linear relationship with price.
pairs(~price+crime_rate,data=df)
plot(df$price,df$crime_rate)
##from graph we predict nature to be logarithmic.
df$crime_rate = log(1+df$crime_rate)
plot(df$price,df$crime_rate)
##some linear relationship now visible from graph.
df$avg_dist = (df$dist1+df$dist2+df$dist3+df$dist4)/4
###New column avg_dist created as average distance.
##to delete a variable see position in data.
##in R index starts from1.
##removing dist1,2,3,4.
df2 <-df[,-7:-10]##1st parameter before comma is for rows we left it blank to keep 
##all rows and selected columns 7,8,9,10 with minus sign which represents deletion.
#assign df2 to df
df <-df2
rm(df2)##removes df2
###bus_ter has only 'Yes' in it . not useful so delete it.
##bus_ter is 14th column
df <- df[,-14]##deleting 14th column
##Dummy variable creation for all variables or columns in data by library dummies.
install.packages("dummies")
library(dummies)
df <- dummy.data.frame(df)
View(df)
##we get all columns of subtypes i.e for waterbody we get waterbody lake,
#waterbodyriver,waterbodyLakeandRiver,waterbodyNone.
##no. of dummy variables = total no. of variables -1.
##so 0ne column can be deleted for eg waterbodyNone.
##similarly for other columns also
##airport has 2 columns airportYes and airportNo
#deleting airportNo
df <-df[,-9]
df <-df[,-14]##deleting waterbodyNone
View(df)
##Correlation Matrix
cor(df)
round(cor(df),2)##roundoff required data to 2 decimal places
df <-df[,-16] ##removing parks.
#to identify highly correlated variables. If more than 1 is highly correlated like in above 
##parks and air_qual air_qual has higher correlation in magnitude.
##so we keep air_qual and remove parks.
###This is done to avoid multi-collinearity.
##How many dummy columns will be required to replace a column containing qualitative 
##data with 5 categories?
##answer-- 5-1 = 4 dummy columns req to replace a column containing qualitative
##data with 5 categories.
##simple linear regression predicts (independent variable)Y based on single predictor(dependent variable) X.
##y=a0+a1x
##linear regression minimizes sum of squared errors.
##we found summary,estimate, std.error,t value and p value which is
##probability of any value greater than absolute value of t.
##a small p value means there exists a relationship .
##p value should be typically < 5% or 1%.
##RSE OR Residual standard error is the average amount that response will deviate from true regression
##line. It is also considered as lack of fit of model to data.
##RSS OR RESIDUAL SUM OF SQUARES is the measure of variability that is not explained by
##model after regression.
##R^2 is the proportion of variance explained. R^2 =(Total sum of squares(TSS SHORT FORM)-RSS)/TSS
simple_model <- lm(price~room_num,data=df) ##linear regression 
#lm(predicted or dependent variable ~ independent variable, data=dataset name given)
summary(simple_model)
#y=b0+b1x
#intercept =b0 = -34.6592
#b1=9.0997(=room_num)
##We also get standard error and t value formula can be seen from ppt.
##Probability of values > abs(t) has 99.9% sure.
##If we increase room_num value by 1 unit house price increase by b1 = 9.0997 units.
#also we got RSE on n-2 degrees of freedom.
#r-squared is 0.4848 means nearly 48% of variance in the values of house price is explained by this linear 
#regression model.
plot(df$room_num,df$price)##plots scatterplot to see relationship
abline(simple_model)##line to show relationship among x and y.
###multiple Linear regression-- more than 1 predictor or dependent values.
##y=b0+b1x1+b2x2+b3x3+...
##p value of f statistic should be < 5%.
multiple_model <- lm(price~.,data=df)
##lm(dependent~independent,data=datasetname)
##'.' is for all column variables except price
summary(multiple_model)
#degree of freedom = 506-no. of variables=506-16 =490
##from data of summary, if we increase room)num price will increase by 4.019017 units.