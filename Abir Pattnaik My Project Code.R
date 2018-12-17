# Analysis on Room Rent in hotels of various cities in India
# NAME: Abir Pattnaik
# EMAIL: abir.pattanaik@gmail.com
# COLLEGE: Maharaja Agrasen Institute of Technology

# Reading the data onto the HotelData.df
# Please change the destination on where the dataset is stored
# Displaying the variables and formats that are present

HotelData.df<-read.csv("C:/Users/DRDO HQ/Desktop/DATA ANALYTICS INTERNSHIP/CAPSTONE PROJECT/Cities42.csv")
str(HotelData.df)

#Converting Date to one format

table(HotelData.df$Date)
HotelData.df$Date<-gsub("18-Dec-16","Dec 18 2016",HotelData.df$Date)
HotelData.df$Date<-gsub("21-Dec-16","Dec 21 2016",HotelData.df$Date)
HotelData.df$Date<-gsub("24-Dec-16","Dec 24 2016",HotelData.df$Date)
HotelData.df$Date<-gsub("25-Dec-16","Dec 25 2016",HotelData.df$Date)
HotelData.df$Date<-gsub("28-Dec-16","Dec 28 2016",HotelData.df$Date)
HotelData.df$Date<-gsub("31-Dec-16","Dec 31 2016",HotelData.df$Date)
HotelData.df$Date<-gsub("4-Jan-16","Jan 04 2017",HotelData.df$Date)
HotelData.df$Date<-gsub("4-Jan-17","Jan 04 2017",HotelData.df$Date)
HotelData.df$Date<-gsub("8-Jan-16","Jan 08 2017",HotelData.df$Date)
HotelData.df$Date<-gsub("8-Jan-17","Jan 08 2017",HotelData.df$Date)
HotelData.df$Date<-gsub("Jan 4 2017","Jan 04 2017",HotelData.df$Date)
HotelData.df$Date<-gsub("Jan 8 2017","Jan 08 2017",HotelData.df$Date)

table(HotelData.df$Date)

#Converting Data into different formats

str(HotelData.df)
HotelData.df$Date<-as.Date(c("Dec 18 2016","Dec 21 2016","Dec 24 2016","Dec 25 2016","Dec 28 2016","Dec 31 2016","Jan 04 2017","Jan 08 2017"),"%b %d %Y")
HotelData.df$HotelAddress<-as.character(HotelData.df$HotelAddress)
HotelData.df$HotelDescription<-as.character(HotelData.df$HotelDescription)
HotelData.df$HotelName<-as.character(HotelData.df$HotelName)

# Summary of the data 
# (a)Population inference:Min.-8096 Mean-4416837 Median-3046163 Max.-12442373
# (b)RoomRent:Min.-299 Median-4000 Mean-5474 Max.-322500
#    RoomRent outliers- 3rd quantile+IQR*1.5= 6299+5794.5=12093.5
#    RoomRent outliers- 1st quantile-IQR*1.5= 2436-5794.5=-3358.5
# (c)Star Rating=3.46
# (d)Airport Distance Min.-0.20 Median-15 Mean-21.16 Max.-124
#    Airport Distance outliers- 3rd quantile+IQR*1.5= 24+23.4=47.4
#    Airport Distance outliers- 1st quantile-IQR*1.5= 8.4-23.4=-15
# (e)Hotel Capacity Min.0 Median-34 Mean-62.5 Max.-600

summary(HotelData.df)

# Individual Variables
# (a)Histogram of Population 
#    Most cities have population in the range of 0-4000 and only few cities have population over 12000---These could be metropolitan cities 
# (b)Histogram of Airport Distance
#    Positively skewed graph with most of the hotels in the range of 0-40 km
# (c)Histogram of HotelCapacity
#    Positively skewed graph with capacity of hotels in the range of 0-300
# (d)Histogram of StarRating of hotels
#    Most of hotel ratings are in the range of 2.5-3.5 

attach(HotelData.df)
hist(Population,col="blue",border="red",main="Population of cities",ylab="Count")
hist(Airport,col="yellow",border="red",ylab="Count")
hist(HotelCapacity,col="red",border="yellow",main="Histogram of Hotel Capacity of various hotels",xlab="Capacity of Hotel",ylab="Count") 
hist(StarRating,col="green",border="yellow",main="Histogram for Star Rating of hotels")

# Comparison of Function Variable to other variables
# (a)Boxplot of Room Rent and Star Rating of Hotel
#    Room Rent Increases with Increase in Star Rating
# (b)Box Plot of Room Rent and whether city is a Metro City
#    Non Metro cities have a higher room rent than Metro Cities
# (c)Boxplot of Room Rent and whether the city is a tourist destination 
#    Those with tourist destinations have higher room rents than those which are not tourist destination
# (d)Boxplot of Room Rent and whether it was New Year's Eve
#    No considerable difference in room rent and New Year's Eve
# (e)Boxplot of Room Rent and whether hotel has a free Wi-fi
#    Room Rent are higher when Free Wi-fi is available
# (f)Boxplot of Room Rent and Whether hotel has swimming pool
#    Room Rents are considerably high when Swimming pools are available
# (g)Boxplot of Room Rent and Whether hotel provides breakfast
#    There is no considerable difference in hotel rent and breakfast barring one or two outliers

boxplot(RoomRent~StarRating,log="x",las=1,col=c("red","sienna","palevioletred1","royalblue2"),horizontal=TRUE,xlab="Room Rent of Hotels",ylab="Star Rating of Hotel",main="Boxplot of Room Rent and Star Rating")
boxplot(RoomRent~IsMetroCity,log="x",col=c("cyan","magenta"),horizontal=TRUE,xlab="Room Rent of Hotels",ylab="Metro City",main="Comparison of Room Rent with Metro City")
boxplot(RoomRent~IsTouristDestination,log="x",horizontal=TRUE,col=c("gold","green"),xlab="Room Rent of Hotels",ylab="Tourist Destination",main="Comparison of Room Rent with Tourist Destination")
boxplot(RoomRent~IsNewYearEve,log="x",horizontal=TRUE,col=c("lightblue","red"),main="Comparison of Room Rent with 31st Dec",xlab="Room Rent of hotels",ylab="New Years eve")

boxplot(RoomRent~FreeWifi,log="x",horizontal=TRUE,col=c("dodgerblue1","darkorange2"),main="Comparison of Room Rent with whether hotel has wi-fi",xlab="Room Rent of hotels",ylab="Wifi ?")
boxplot(RoomRent~HasSwimmingPool,log="x",horizontal=TRUE,col=c("ivory","lightgoldenrod1"),main="Comparison of Room Rent with whether hotel has a swimming pool",xlab="Room Rent of hotels",ylab="Swimming Pool")
boxplot(RoomRent~FreeBreakfast,log="x",horizontal=TRUE,col=c("violetred2","lightgoldenrod1"),main="Comparison of Room Rent with whether hotel provides BreakFast",xlab="Room Rent of hotels",ylab="Breakfast")

# Plots for continuos variables
# (a)Plot for Room Rent and Hotel Capacity
#    Most of the hotel room rents are higher when hotel capacity is less than 300.Slow Increase in room rent 
# (b)Plot for Room Rent and Star Rating
#    Considerable Increase in the rates of Hotel rent as the ratings are increased
# (c)Plot for Room Rent and Airport Distance
#    Slight Increase in the difference of Room rents and distance of hotel from airport

plot(~HotelCapacity+RoomRent,log="y",type="p",col=c("yellow","red"),main="Comparison between Room Rent and Hotel Capacity",xlab="Hotel Capacity",ylab="Room Rent of Hotels")
lines(lowess(RoomRent))
lines(lowess(RoomRent~HotelCapacity),col="navy")

plot(~StarRating+RoomRent,log="y",col=c("gold","navy"),type="p",main="Comparison between Room Rent and Star Rating",xlab="Star Rating of Hotels",ylab="Room Rent of Hotels")
lines(lowess(RoomRent~StarRating))

plot(RoomRent~Airport,log="y",col=c("darkgreen","yellow"),type="p",main="Comparison between Room Rent and Airport Distance",xlab="Airport Distance",ylab="Room Rent of Hotels")
lines(lowess(RoomRent~Airport))


# Drawing the correlation matrix that specifies which objects are highly correlated.
# A value that reaches near correlation value 1 has positive correlation while that has value going
# towards -1 has negative value.
# (a)Star Rating has a correlation value of 0.60 with Hotel Rating 
# (b)Star Rating has a correlation value of 0.62 with HasSwimmingPool
# (c)Hotel Capacity has a correlation value of 0.51 with HasSwimmingPool
# (d)Rest have low correlation values

library(corrplot)
corrplot(cor(HotelData.df[,-c(2,9,10,14,15,16)]),method="number")

# Variables taken
# StarRating,IsTouristDestination,Airport
# Experimenting the variables with the RoomRent
# Inferences: (a)All the correlations were not too high as they reduced homoscedasticity
# 		  (b)The pairs function showed interesting plots with respect to the RoomRent as only IsTouristDestination
#		     contained the categorical variable rest both of them were continuos
library(corrgram)
corrgram.data=data.frame(RoomRent,StarRating,IsTouristDestination,Airport)
corrgram(corrgram.data, order=TRUE, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt,main="Corrgram of Hotel Prices In India")

library(car)
pairs(RoomRent~StarRating+IsTouristDestination+Airport,cex=0.6,data=HotelData.df)

library(car)
pairs(RoomRent~StarRating+IsTouristDestination+FreeWifi,cex=0.6,data=HotelData.df)


# t.tests
# T test for when the average of Room Rent is higher when It is a tourist Destination
# T test for when the average of Room Rent is higher when Hotel has a swimming pool

t.test(RoomRent~IsTouristDestination,alternative="greater")
t.test(RoomRent~HasSwimmingPool,alternative="greater")

# Aggregate command for Room Rent and variables-Airport,Star Rating and IsTouristDestination
# (a)The Room Rent doesn't quite depend on the StarRating linearly but a 4.8 rating starred hotel has average of 46000 Room Rent
# (b)Rates are quite high when it is closer ranging from 3000-24000
# (c)Room Rents are higher when it is a tourist destination
# (d)On New Years Eve the rates of Room Rent is higher as compared to a regular day

aggregate(RoomRent,by=list(StarRating),mean)
aggregate(RoomRent,by=list(Airport),mean)

aggregate(RoomRent,by=list(IsTouristDestination),mean)
aggregate(RoomRent,by=list(Date),mean)

# Creating correlation and covariance
# These factors although not correlated but show high variances between them

x<-HotelData.df[,c("IsTouristDestination","StarRating", "Airport")]
y<-HotelData.df[,c("RoomRent")]
cor(x,y)
cov(x,y)

# Creating a linear model_1
# INTERPRETATIONS-The model that was created had high significance factors, this meant that 
# each model created has its own significance and cannot be removed as they help in getting a R^2 
# value although it was low
# R^2 and adjusted R^2 value are respectively 0.1575 and 0.1574
# However due to huge RSE value this model is not a correct adaptation of the RoomRent 
# but these factors shouldn't be eliminated also.
# The outlier test for the model provided the outliers that may affect the model and
# its overall value but it wasn't much so.
# VIF stands for Variation Inflation Factor.This shows the variation.
# Any Variation that is greater than 10 creates a problem  but as displayed most 
# of them were 1
# gvlma model stands for Global Validation of Linear Models Assumptions
# This model has been created for checking the validation of the model
# with respect to GlobalStat,Skewness,Kurtosis,Link Function,Heteroscedasticity
# Plotting a linear model with respect to the values 4 graphs were created.
# In Residuals Vs Fitted shows a linear relationship that means this is not a correct model
# The Normal Q-Q model shows that a linear model upto a value but huge outliers create a problem
# The fourth plot has the cook's distance where the outliers that surpass the 0.5 range are rejected.
# However it is shown that these outliers didn't matter.

 
lm.fit1=lm(RoomRent~StarRating+Airport+IsTouristDestination,data=HotelData.df)
summary(lm.fit1)
par(mfrow=c(2,2))
plot(lm.fit1)
par(mfrow=c(1,1))

library(car)
outlierTest(lm.fit1)
vif(lm.fit1)

library(gvlma)
gvmodel <- gvlma(lm.fit1) 
summary(gvmodel)

# Creating a linear model_2
# This model was created containing variables that are not descriptive but are both continuos and categorical variable
# INTERPRETATIONS-The model that was created had high significance factors, this meant that 
# each model created has its own significance and cannot be removed as they help in getting a R^2 
# value although it was low
# R^2 and adjusted R^2 value are respectively 0.1904 and 0.1899
# However due to huge RSE value this model is not a correct adaptation of the RoomRent 
# but these factors shouldn't be eliminated also.
# The outlier test for the model provided the outliers that may affect the model and
# its overall value but it wasn't much so.
# VIF stands for Variation Inflation Factor.This shows the variation.
# Any Variation that is greater than 10 creates a problem  but as displayed most 
# of them were 1
# gvlma model stands for Global Validation of Linear Models Assumptions
# This model has been created for checking the validation of the model
# with respect to GlobalStat,Skewness,Kurtosis,Link Function,Heteroscedasticity
# Plotting a linear model with respect to the values 4 graphs were created.
# In Residuals Vs Fitted shows a linear relationship that means this is not a correct model
# The Normal Q-Q model shows that a linear model upto a value but huge outliers create a problem
# The fourth plot has the cook's distance where the outliers that surpass the 0.5 range are rejected.
# However it is shown that these outliers didn't matter.

lm.fit2=lm(RoomRent~Population+IsMetroCity+IsTouristDestination+IsNewYearEve+StarRating+Airport+FreeWifi+HotelCapacity+HasSwimmingPool,data=HotelData.df)
summary(lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)
par(mfrow=c(1,1))

library(car)
outlierTest(lm.fit2)
vif(lm.fit2)

library(gvlma)
gvmodel <- gvlma(lm.fit2) 
summary(gvmodel)

# Anova stands for Analysis of Variation Model
# It takes in a full model and the model that has been created by the user
# and then comparison is made
# For this model,A high RSS is achieved which clearly shows that this is not a correct predictive model


anova(lm.fit2,lm.fit1)
detach(HotelData.df)

#Detaching the model 
#--------------THE END-----------------#







