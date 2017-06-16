# Project Title:  Prediction of house price in King County, USA
# NAME: Aditya Sharma
# EMAIL: adityathc@gmail.com
# COLLEGE / COMPANY: NIT Jaipur

# Setting up working directory
setwd("C:/Users/aditya/Downloads/Internship 2017/R directory")

# Reading csv file into R and creating a data frame 'hprice'
hprice <- read.csv("kc_house_data.csv")

# Checking data frame
dim(hprice)
class(hprice)

# Formatting date as date format from string
hprice$date <- as.Date(as.Date(as.character(hprice$date),"%Y%m%d"))

# Checking for NA values in dataset
table(is.na(hprice))

# Creating a variable column name 'age' , because year built doesn't make sense. Age at selling the home (after built ) matters
hprice$age <- as.numeric(format(hprice$date, "%Y")) - hprice$yr_built

# Same as done above, column 'renage', but renage has zero values which do not make sense, so making 0 values to null before
hprice$yr_renovated[hprice$yr_renovated == 0] <- NA
hprice$renage <- as.numeric(format(hprice$date, "%Y")) - hprice$yr_renovated

# Checking how many NA are there
table(is.na(hprice$renage))   # only approax 5% houses are renovated)
table(hprice$waterfront)      # less than 0.5% have waterfront
table(hprice$view)            # approax 10% has other than zero views 1,2,3,4
table(hprice$bedrooms)        # mostly bedrooms are between 1-6
table(hprice$bathrooms)      # mostly accounts for 1,1.5,1.75,2,2.25,2.5,2.5,3,3.5 total 30
table(hprice$condition)      # mostly 3 then 4 then 5 then 2 then 1
table(hprice$grade)          # mostly 5-9 out of 1-12
table(hprice$floors)         # mostly 1 and 2 then 1.5


# Column 'rate' is created which is selling price per square feet
hprice$rate <- hprice$price/hprice$sqft_living

# Checking structure of dataset
str(hprice)

# Checking summary of 'hprice'
summary(hprice)
#( bedrooms, floors, waterfront, view, condition, grade - feel like catagorical variables)
#( bathrooms can be said as catagorical variable, in multiple of 0.25)
#( zipcode is directly connected to latitude and longitude, so only zipcode will be fine. But as catagorical variable(factored, better clustered))
#( sqft_lot15 and sqft_living15 is average of nearby 15 houses, will be usefull in foresting)

####  DATA VISUALIZATION

## Boxplots
library(lattice)

# Of each variable indivisually
bwplot(~bedrooms, data = hprice)    #( few outliers but one big notable outlier on 33)
bwplot(~price, data = hprice)       #( high price outliers)
bwplot(~bathrooms, data = hprice)   #( both side outliers but few noticable outliers near 8)
bwplot(~sqft_living, data = hprice) #( big area outliers)
bwplot(~sqft_lot, data = hprice)    #( mostly not much lot area but few outliers are there)
bwplot(~floors, data = hprice)      
bwplot(~waterfront, data = hprice)  #( mostly no waterfront)
bwplot(~view, data = hprice)        #( mostly zero view)
bwplot(~condition, data = hprice)   #( very little low condition outlier)
bwplot(~grade, data = hprice)       #( both side outliers, mostly 6-8)
bwplot(~sqft_above, data = hprice)  #( high area outliers)
bwplot(~sqft_basement, data = hprice) #( high area outliers)
bwplot(~age, data = hprice)         #( mostly between 20-60)
bwplot(~renage, data = hprice)      #( mostly 10-30, some big outliers after 60)
bwplot(~rate, data = hprice)

# Mostly catagorical type variable with comparision to price (in case of catagorical- zero counted as one and increasing)
bwplot(bedrooms~price, data = hprice, horizontal= TRUE)   #( mostly price incremen in 4-8 which is between)
bwplot(bathrooms~price, data = hprice)          #( price vary with bathrooms)
bwplot(sqft_living~price, data = hprice)         
bwplot(floors~price, data = hprice)             #( price not depending on floors, mostly on 1,3)
bwplot(waterfront~price, data = hprice)         #( mostly 0)         
bwplot(view~price, data = hprice)               #( mostly 0)
bwplot(grade~price, data = hprice)              #( price increasing with grade)

# density plot
densityplot(hprice$price)

## Histograms
library(ggplot2)

# of mostly continous variables
hist(hprice$price, breaks = 100)
ggplot(hprice, aes(hprice$price)) + stat_bin(bins = 100, colour="black", fill="green") + labs(x= "Price",y= "Frequency" , title = "Histogram of Price") + xlim(0,4000000) + scale_fill_discrete()
ggplot(hprice, aes(hprice$sqft_living)) + stat_bin(bins = 100, colour="black", fill="green") + labs(x= "square feet living",y= "Frequency" , title = "Histogram of sqft_living") + scale_fill_discrete()
ggplot(hprice, aes(hprice$sqft_lot)) + stat_bin(bins = 100, colour="black", fill="green")
ggplot(hprice, aes(hprice$sqft_basement)) + stat_bin(bins = 100, colour="black", fill="green")
ggplot(hprice, aes(hprice$sqft_above)) + stat_bin(bins = 100, colour="black", fill="green")
ggplot(hprice, aes(hprice$zipcode)) + stat_bin(bins = 100, colour="black", fill="green")
ggplot(hprice, aes(hprice$age)) + stat_bin(bins = 100, colour="black", fill="green")

# of catagorical variables with price
qplot(hprice$view, main = "Plot of House View" , xlab = "View" , ylab = "Frequency")
qplot(hprice$bathrooms)
qplot(hprice$bedrooms, bins=10)
qplot(hprice$bathrooms)
qplot(hprice$grade)
qplot(hprice$condition)
qplot(hprice$waterfront)
qplot(hprice$floors)

# Scatterplots    ( should be more informative for both continous variables)
library(car)     # red line affected by outlier, green ignoring the outlier (robust)
scatterplot(x =hprice$price, y=hprice$bedrooms)       #( not much dependent)
scatterplot(x =hprice$bedrooms, y=hprice$bathrooms)   #( dependent excluding the 33 outlier)
scatterplot(x =hprice$price, y=hprice$bathrooms)      #( dependent)
scatterplot(x =hprice$price, y=hprice$sqft_living)    #( dependent)
scatterplot(x =hprice$price, y=hprice$sqft_lot)       #( not dependent)
scatterplot(x =hprice$price, y=hprice$view)           #( dependent but mostly view is 0 so not dependent)
scatterplot(x =hprice$price, y=hprice$grade)          #( dependent)
scatterplot(x =hprice$price, y=hprice$floors)         #( not dependent from boxplot)
scatterplot(x =hprice$price, y=hprice$condition)      #( nearly not dependent)
scatterplot(x =hprice$price, y=hprice$waterfront)     #( not dependent as nearly no waterfront)
scatterplot(x =hprice$price, y=hprice$bedrooms)       #( not much dependent)
scatterplot(x =hprice$price, y=hprice$sqft_above)     #( dependent)
scatterplot(x =hprice$price, y=hprice$sqft_basement)  #( dependent)
scatterplot(x =hprice$price, y=hprice$age)            #( low negative dependent)
scatterplot(x =hprice$price, y=hprice$zipcode)        
scatterplot(x =hprice$price, y=hprice$renage)         #( not dependent as very less houses are renovated)
# scatterplot using ggplot
ggplot(data = hprice, mapping = aes(x = sqft_living, y = price)) + geom_point(colour = 'skyblue') + geom_smooth(method = 'lm')


# creating a data frame excluding id,date,sqft_living15,sqft_lot15, lat,long,yr_built,yr_renovated,rate
hpricecor <- hprice[ ,c(3:14,17,22,23)]

# Correlation matric
cor(hpricecor)

# Corrplot
library(corrplot)
corrplot(cor(hpricecor))

# Corrgram
library(corrgram)
corrgram(hpricecor, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)

# Hypothesis
# Price is correlated with sqft_living, bathrooms, sqft_above, grade 
# Correlated little with bedroom, (sqft_basement is correlated with sqft_above) (view is discarded))

## Hypothesis testing
# t-Test
cor.test(hprice$price,hprice$sqft_living)
t.test(hprice$price,hprice$sqft_living)
t.test(hprice$price,hprice$bathrooms)
t.test(hprice$price,hprice$sqft_above)
t.test(hprice$price,hprice$grade)
t.test(hprice$price,hprice$bedrooms)
t.test(hprice$price,hprice$sqft_basement)

#### DATA MODELLING

# converting catagorical variables from numeric variables ( bedroom, bathroom, grade ,zipcode)
hprice$bedrooms <- as.factor(hprice$bedrooms)
hprice$bathrooms <- as.factor(hprice$bathrooms)
hprice$grade <- as.factor(hprice$grade)
hprice$zipcode <- as.factor(hprice$zipcode)

# Checking structure now
str(hprice)

# Writing multiple linear regression model without zipcode
model1 <- lm(price~ sqft_living + bedrooms + bathrooms + grade + sqft_above,data = hprice)
summary(model1)

## Writing multiple linear regression model without zipcode
model1A <- lm(price~ sqft_living + bedrooms + bathrooms + grade + sqft_above + zipcode,data = hprice)
summary(model1A)

# adjusted R squared value = 0.791 ( model 1A)

# Plotting fit model1A
plot(model1A)

# Predicting price with model
price_predict <- predict(model1A)

# Adding selling price and predicted price in a new data frame
Prediction <-cbind(hprice$price, price_predict)

# Setting column names for 'Prediction'
colnames(Prediction) <- c("selling_price","price_predict")

# Writing a csv file containing difference between price
write.csv(Prediction, file = "price comparision.csv", row.names=FALSE)









#### Alternatively Clustering can be done according geo-mapping ( Only Visualization for now )
# Clustering
# Visualizing zipcode and lat and long
ggplot(data = hprice, mapping = aes(x = zipcode, y = price)) + geom_boxplot()

ggplot(hprice, mapping = aes(x=lat, y=long)) + geom_point(colour= "green") + geom_smooth(method = 'lm')


hcluster <- kmeans(scale(hprice[,c(18,19,24)]),25,100)  #( creating 25 clusters,100 random start)
 
# factoring clusters
hprice$cluster<-factor(hcluster$cluster)

# plotting clusters
ggplot(data= hprice, aes(x = long, y = lat)) + geom_point(aes(color=cluster))
ggplot(hprice, aes(rate, fill = cluster)) + geom_density(position = "stack")


