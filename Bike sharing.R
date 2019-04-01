# Kaggle bike sharing program
# The training data set is for the first 19 days of each month. 
# The test dataset is from 20th day to month's end. 
#We are required to predict the total count of bikes rented during each hour covered by the test set.

#****************************************************************************
#Hypothesis generation
#1.Hourly trend : There must be high demand in the office hours.
#2.Daily trend : Registered users use more bikes on weekdays then weekend
#3. Rain - Demand is inversely proposional to rain
#4. Temperature - seems to be directly propotional for the data
#5. Time - Total demand should have higher contribution of registered users compared to casual users
#*****************************************************************************

# import the data sets.

train = read.csv(file.choose())
test = read.csv(file.choose())

str(train)
str(test)

#Combine both the datasets
test$casual = 0
test$registered = 0
test$count = 0

data = rbind(train,test)
str(data)
summary(data)

# Find the missing values
table(is.na(data))
sapply(data, function(df)
  {
    sum(is.na(df)==T)
  })

# Find the distribution of the numerical variables.
par(mfrow=c(4,2))
#par(mar = rep(2,4))

hist(data$season)
hist(data$holiday)
hist(data$workingday)
hist(data$weather)
hist(data$temp)
hist(data$atemp)
hist(data$humidity)
hist(data$windspeed)

# Inferences :
# 1. Season has 4 categories and are amlost equally distributed.
# 2. Holiday and working day variables share similar inference. 
# 3. Weather 1 has higher contribution, ie, clear sky
# 4. Variables temp,atemp,humidity and windspeed look naturally distributed.

#Converting discrete variables to factors.
data$season = as.factor(data$season)
data$holiday = as.factor(data$holiday)
data$workingday = as.factor(data$workingday)
data$weather = as.factor(data$weather)

#Testing hypothesis 1 : Hourly trend.
# We do not have hour variable, hence extracting hour from datetime variable

data$hour = substr(data$datetime,12,13)
data$hour = as.factor(data$hour)

train = data[as.integer(substr(data$datetime,9,10))<20,]
test = data[as.integer(substr(data$datetime,9,10))>19,]

boxplot(train$count ~ train$hour,xlab='hour',ylab='# of users')

# The boxplot proves the hypothesis on hourly trend.
# Based on the boxplot, we can categorize users as
# High = 7 to 9 & 17-19 hrs
# Average = 1 to 16 hrs
# Low = 0 to 6 & 20 to 24hrs

# Testing hypothesis 2 - Daily trend
boxplot(train$registered ~ train$hour,xlab='hour',ylab='registered users')
boxplot(train$casual ~ train$hour,xlab='hour',ylab='casual users')

#from the boxplot, its clear that registered users have similar pattern as of count.
# Whereas casual users have different trend. Thus we can prove that
#Hour is a significant variable and the hypothesis is true.

# Testing hypothesis 2 - Daily trend
# Extract the variable day from datetime variable
date = substr(data$datetime,1,10)
days = weekdays(as.Date(date))
data$days = days

train = data[as.integer(substr(data$datetime,9,10))<20,]
test = data[as.integer(substr(data$datetime,9,10))>19,]

boxplot(train$registered ~ train$days,xlab = 'days',ylab= 'registered users')
boxplot(train$casual ~ train$days, xlab ='days', ylab = 'Casual Users')
# From boxplot we can prove that demand for casual users are more during weekends

# Testing hypothesis 3 - Rain
boxplot(train$registered ~ train$weather, xlab = 'weather', ylab = 'Registered Users')
boxplot(train$casual ~ train$weather, xlab = 'weather', ylab = 'Casual Users')
# Working as per the hypothesis

# Testing hypothesis 4 - Temperature
boxplot(train$registered ~ train$temp, xlab = 'weather', ylab = 'Registered Users')
boxplot(train$casual ~ train$temp, xlab = 'weather', ylab = 'Casual Users')
str(train)

# Temperature, Windspeed and Humidity: These are continuous variables so we
# can look at the correlation factor to validate hypothesis. 
cont = data.frame(train$registered,train$casual,train$count,train$temp,
                  train$atemp,train$humidity,train$windspeed)
cor(cont)

# Variable temp is positively correlated with dependent variables (casual is more compare to registered)
# Variable atemp is highly correlated with temp.
# Windspeed has lower correlation as compared to temp and humidity

# Testing hypothesis for time
data$year = substr(data$datetime,1,4)
data$year = as.factor(data$year)

train = data[as.integer(substr(data$datetime,9,10))<20,]
test = data[as.integer(substr(data$datetime,9,10))>19,]

boxplot(train$count ~ train$year, xlab = 'year', ylab = 'Count')
# 2012 has more deomand than 2011.

# We have already added new variables - hour, month, day, year
# we have broadly categorize the hour into three categories. 
# Let's create bins for the hour variable separately for casual and registered users

train$hour = as.integer(train$hour)
test$hour = as.integer(test$hour)

# Using decision tree for binning some variables
library(rpart)
library(rattle) # For better visual plot
library(rpart.plot)
library(RColorBrewer)
dtree = rpart(train$registered ~ train$hour)
fancyRpartPlot(dtree)

#Creating buckets for registered users
data = rbind(train, test)
data$dp_reg = 0
data$dp_reg[data$hour < 8] = 1
data$dp_reg[data$hour > 22] = 2
data$dp_reg[data$hour > 9 & data$dp_reg < 18] = 3
data$dp_reg[data$hour == 8] = 4
data$dp_reg[data$hour == 9] = 5
data$dp_reg[data$hour == 20 | data$dp_reg == 21] = 6
data$dp_reg[data$hour == 19 | data$dp_reg == 18] = 7

data$dp_cas = 0
data$dp_cas[data$hour <=8] = 1
data$dp_cas[data$hour ==9] = 2
data$dp_cas[data$hour >=10 & data$hour <= 19] = 3
data$dp_cas[data$hour > 19 ] = 4

# Similarly create bins for temp
f = rpart(train$registered ~ train$temp)
fancyRpartPlot(f)

data$temp_reg = 0
data$temp_reg[data$temp_reg < 13] = 1
data$temp_reg[data$temp_reg >= 13 & data$temp <23] = 2
data$temp_reg[data$temp_reg >= 23 & data$temp <30] = 3
data$temp_reg[data$temp_reg >= 30] = 4

data$temp_cas = 0
data$temp_cas[data$temp < 15] = 1
data$temp_cas[data$temp >= 15 & data$temp <23] = 2
data$temp_cas[data$temp >=23 & data$temp < 30] = 3
data$temp_cas[data$temp > 30] = 4

data$year_part[data$year=='2011']=1
data$year_part[data$year=='2011' & data$month>3]=2
data$year_part[data$year=='2011' & data$month>6]=3
data$year_part[data$year=='2011' & data$month>9]=4
data$year_part[data$year=='2012']=5
data$year_part[data$year=='2012' & data$month>3]=6
data$year_part[data$year=='2012' & data$month>6]=7
data$year_part[data$year=='2012' & data$month>9]=8
table(data$year_part)

data$day_type = ''
data$day_type[data$holiday ==0 & data$workingday == 0] = 'weekend'
data$day_type[data$holiday ==1] = 'holiday'
data$day_type[data$holiday ==0 & data$workingday ==1] = 'working day'

data$weekend = 0
data$weekend[data$days =='Sunday' | data$days == 'Saturday'] = 1

train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]
plot(train$temp, train$count)

data=rbind(train,test)
data$month=substr(data$datetime,6,7)
data$month=as.integer(data$month)

# dividing total data depending on windspeed to impute/predict the missing values
table(data$windspeed==0)
k=data$windspeed==0
wind_0=subset(data,k)
wind_1=subset(data,!k)

# predicting missing values in windspeed using a random forest model
# this is a different approach to impute missing values rather than just using the mean or median or some other statistic for imputation
library(randomForest)
set.seed(415)
fit <- randomForest(windspeed ~ season+weather +humidity +month+temp+ year+atemp, data=wind_1,importance=TRUE, ntree=250)
pred=predict(fit,wind_0)
wind_0$windspeed=pred

data=rbind(wind_0,wind_1)

data$weekend=0
data$weekend[data$day=="Sunday" | data$day=="Saturday"]=1

str(data)

# converting all relevant categorical variables into factors to feed to our random forest model
data$season=as.factor(data$season)
data$holiday=as.factor(data$holiday)
data$workingday=as.factor(data$workingday)
data$weather=as.factor(data$weather)
data$hour=as.factor(data$hour)
data$month=as.factor(data$month)
data$day_part=as.factor(data$dp_cas)
data$day_type=as.factor(data$dp_reg)
data$day=as.factor(data$days)
data$temp_cas=as.factor(data$temp_cas)
data$temp_reg=as.factor(data$temp_reg)

train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

# log transformation for some skewed variables, which can be seen from their distribution
train$reg1=train$registered+1
train$cas1=train$casual+1
train$logcas=log(train$cas1)
train$logreg=log(train$reg1)
test$logreg=0
test$logcas=0

boxplot(train$logreg~train$weather,xlab="weather", ylab="registered users")
boxplot(train$logreg~train$season,xlab="season", ylab="registered users")

# final model building using random forest
# note that we build different models for predicting for registered and casual users
set.seed(415)

fit1 <- randomForest(logreg ~ hour +workingday+day+holiday+ day_type +
                       temp_reg+humidity+atemp+windspeed+season+weather+
                       dp_reg+weekend+year+year_part, data=train,
                     importance=TRUE, ntree=250)

pred1=predict(fit1,test)
test$logreg=pred1

#Predicting the log of casual users
set.seed(200)
fit2 <- randomForest(logcas ~ hour + day_type + day + humidity + atemp + temp_cas +windspeed + season + weather + holiday +
                     workingday + dp_cas + weekend + year + year_part, data = train, importance = TRUE, ntree = 250)
pred2 = predict(fit2,test)
test$logcas = pred2












