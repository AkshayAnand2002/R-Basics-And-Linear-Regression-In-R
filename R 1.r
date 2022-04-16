#ls()-->to see all variables on workspace
#rm(variable-name)--> to delete a variable
#remove(variable-name)-->to delete a variable.
#rm(list = ls())--> to delete all variables from workspace.
#browseURL("website-url") --> to browse a url .
install.packages("##package-name")
##library() --> to get list of all packages installed.
##search() --> to get list of packages which are currently loaded.
##require("package-name")##to add a new package to currently loaded packages.
##detach("package:LiblineaR",unload=TRUE)## to remove a package from running currently.
##remove.packages("package-name")##to uninstall a package .
## ?ggplot2 --> to know more about a package.
library(datasets)### to call all inbuilt datasets
data() ##to get list of all inbuilt datasets and small description.
library(help = "datasets") ### gives list with description
? iris  ### to know abt inbuilt data
str(iris) ##to get structure of data
iris
data("iris")
###manual data entry
x1 <- 1:10
x2 <- c(2,5,7,4)
x3 <- seq(5,50, by = 5)
x4 <-scan()## for user input once we don't want to enter new values just press enter 2 times
Product <- read.table("C:/Product.txt",header=TRUE , sep ="\t")
## to read a text file in r , separator attribute for file has what type of separator
## and header for file has header or not
str(Product)
Customer <- read.csv("C:/Customer.csv",header = TRUE)##DEFAULT SEPARATOR FOR CSV FILE IS COMMA.
Customer
View(Customer)
## above command to view whole dataset.
y <- table(Customer$Region) ## to select a particular column of dataset
y ## gives no of times each name comes in data column.
View(y)  ###view in table format
barplot(y)
barplot(y[order(y)]) ## to order the bars in ascending order
barplot(y[order(-y)]) ## order descending order
barplot(y[order(y)],horiz=TRUE)
barplot(y[order(y)],horiz=TRUE , col= "red")
barplot(y[order(y)],horiz=TRUE , col= c("red","green","blue","beige"))
colors()
barplot(y[order(y)],horiz=TRUE , col= c("red","green","blue","beige"),border = NA)
##ABOVE COMMAND TO REMOVE OUTSIDE BORDER OF BARS.
## BELOW COMMAND TO ADD TITLE TO CHART
barplot(y[order(y)],horiz=TRUE , col= c("red","green","blue","beige"),border = NA,main= "FREQUENCY OF REGIONS")
barplot(y[order(y)],horiz=TRUE , col= c("red","green","blue","beige"),border = NA,main= "FREQUENCY OF \n REGIONS")
## printing some words in next line.
barplot(y[order(y)],horiz=TRUE , col= c("red","green","blue","beige"),border = NA,main= "FREQUENCY OF \n REGIONS",xlab="NO. OF CUSTOMERS")
## saving image of visualizations here barplot
png(filename="C:/FREQ.png",width = 888,height= 571 )
barplot(y[order(y)],horiz = TRUE , col=c("red","green","blue","beige"),border = NA,main= "FREQUENCY OF \n REGIONS",xlab="NO. OF CUSTOMERS")
dev.off()
hist(Customer$Age)
hist(Customer$Age,breaks = 5) ## to provide no of bars we require .
## R may plot 5 bars or more or less bars also .
hist(Customer$Age,breaks = c(0,40,60,100))## to specify bar widths.
## the above plots density on y axis but we want frequency.
hist(Customer$Age,breaks = c(0,40,60,100),freq = TRUE)
##THE ABOVE HAS FREQ ON Y-AXIS.
hist(Customer$Age,breaks = c(0,40,60,100),freq = TRUE,col='blue')
### the above plots have default title 
hist(Customer$Age,breaks = c(0,40,60,100),freq = TRUE,col='blue', main="HISTOGRAM OF AGE")
