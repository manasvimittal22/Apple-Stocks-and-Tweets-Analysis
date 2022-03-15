#installing libraries
library(forecast)
library(zoo)
library(fBasics)
library(rugarch)
library(ggplot2)
#reading the data set
apple_price <- read.csv("AAPL.csv")



#### Exploratory data analysis ####
#list of all the column names
colnames(apple_price)

#finding out all the column data types
#all the columns ahve the right class except for date which has the character class instead of the date
sapply(apple_price, class)

#finding out all the null values in the relevant closing price which we shall use for time series regression
#hence we come to the conclusion that zero null values are present in the price open column.
sum(is.na(apple_price$Open))


#finding out if any date column values are missing
#Hence we can conclude that zero null values are present in the date column
sum(is.na(apple_price$Date))


#### Data cleaning ####

#converting the date column from date to character type 
apple_price$Date <- as.Date(apple_price$Date,"%m/%d/%Y")

#subsetting data according to relevant dates
apple_price_relevant_before <- apple_price[apple_price$Date > "2014-12-31" & apple_price$Date < "2020-12-31" , ]


#creating a timely ordered data set to perform time series regression
apple_price_relevant = zoo(apple_price_relevant_before$Close, apple_price_relevant_before$Date)
head(apple_price_relevant)



#### Final Data explorartion forregression ####  
# basic stats for apple stock price
basicStats(apple_price_relevant)

#The apple stock price between the
par(mfrow=c(1,1))
plot(apple_price_relevant, type='l', ylab = "Closing Price", xlab = '2015-2020' , main=" Apple stock price over the years")


#to calculate returns for each day
returns = 1- (apple_price_relevant/lag(apple_price_relevant, -1))

#displaying the absolute returns over the years
par(mfrow=c(1,1))
plot(returns, type='l', ylab = "Returns", main="Perentage returns of apple stock over the years")

# creates time plot of log returns
returns_log = log(apple_price_relevant/lag(apple_price_relevant, -1))
par(mfrow=c(1,1))
plot(returns_log, type='l', ylab = "Log Returns", main="Log perentage returns of apple stock over the years")


#### Data testing for selecting the right parameters ######

## Check ARC effect
# removing dates from the returns_log data apply the ACF function to help us understand the strength of relationship of past data with present data 
returns_ACF_log = coredata(returns_log);

# Checking the ARC effect using Plots ACF function 
par(mfrow=c(1,1))
acf(returns_ACF)

# Checking the ARC effect using Plots ACF function 
returns_ACF = coredata(returns);
acf(returns_ACF)

#### testing of independence

#Ljung Box test on returns
Box.test(returns_ACF, lag=1, type="Ljung")
Box.test(returns_ACF, lag=3, type="Ljung")
Box.test(returns_ACF, lag=5, type="Ljung")


#Ljung Box test on alog returns
Box.test(returns_ACF_log, lag=1, type="Ljung")
Box.test(returns_ACF_log, lag=3, type="Ljung")
Box.test(returns_ACF_log, lag=5, type="Ljung")

#based on the test we select log returns as our input data due to very small p values  
#therefore showing significant autocorrelation necessary for predicting future stock values
#therefore we select log returns as our inout


#### Model fitting ######

#Fitting the ARMA(0,0)-eGARCH(1,1) model with t-distribution
egarch11.t.spec=ugarchspec(variance.model=list(model = "eGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")
#estimate model 
egarch11.t.fit=ugarchfit(spec=egarch11.t.spec, data=returns_log)
egarch11.t.fit
plot(egarch11.t.fit)


rff=ugarchfit(spec=egarch11.t.spec, data=returns_log, out.sample=500)
rf=ugarchforecast(rff, n.ahead=20, n.roll=450)
rf
plot(rf)


#backtesting
mod_egarch = ugarchroll(egarch11.t.spec, data = returns_log, n.ahead = 1,
                        n.start = 1000, refit.every = 200, refit.window = "recursive")
mod_egarch
report(mod_egarch, type="VaR", VaR.alpha = 0.01, conf.level = 0.95)


###### Probability questions ######

#Q1)
h <- hist(returns, breaks = 20, plot=FALSE)
h$counts=h$counts/sum(h$counts)
plot(h,xlab='Returns',ylab="Probablity")


#Q2)
quantile(returns,1:80/80)
po <- ggplot(data = returns, aes(x=returns))
po +stat_ecdf()
