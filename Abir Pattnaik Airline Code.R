# Analysis of Airline Ticket Pricing
# NAME: ABIR PATTNAIK
# EMAIL: abir.pattanaik@gmail.com
# COLLEGE : Maharaja Agrasen Institute of Technology

# NOTE:The dataset that has been used is SixAirlinesData.csv updated on W4D1
# PriceRelative=(PricePremium-PriceEconomy)/ (PriceEconomy)

# a)Load the dataset into RStudio and Viewing the dataframe for checking whether the 
#   data set uploaded is correct or not.
#
# Choose your own location of where you have saved the dataset SixAirlinesData.csv

AirlineAnalysis.df<-read.csv("C:/Users/DRDO HQ/Desktop/DATA ANALYTICS INTERNSHIP/AIRLINE PROJECT/SixAirlinesData.csv")
View(AirlineAnalysis.df)

# b)To summarize the data with the help of Summary function.
#   A detailed version of the spread can be understood by describe() function
#   'psych' package was used for accessing the function

summary(AirlineAnalysis.df)
library(psych)
describe(AirlineAnalysis.df)

# c)Visualising each variable independently and understanding the spread of each data
# Histogram was used to depict the distribution of PriceEconomy and PricePremium

attach(AirlineAnalysis.df)
par(mfrow=c(1,2))
hist(PriceEconomy,xlab="Economy Price of the Airlines",ylab="Count",main="Economy Price of Six Airlines",col="red")
hist(PricePremium,xlab="Premium Price of the Airlines",ylab="Count",main="Premium Price of Six Airlines",col="yellow")


# c1)Boxplots to see the price of economy seats and premium seats in each airline

par(mfrow=c(2,1))
boxplot(PriceEconomy~Airline,data=AirlineAnalysis.df,horizontal=TRUE,xlab="Price of economy seats",ytab="Airline",main="price of economy seats in each airline",las=1,col="lightblue")
boxplot(PricePremium~Airline,data=AirlineAnalysis.df,horizontal=TRUE,xlab="Price of PREMIUM seats",ytab="Airline",main="price of Premium seats in each airline",las=1,col="lightgreen")

# c2)Boxplots to see the no of total,economy and premium seats in each airline

par(mfrow=c(3,1))
boxplot(SeatsTotal~Airline,data=AirlineAnalysis.df,horizontal=TRUE,xlab="No.of seats",ytab="Airline",main="No.of seats in each airline seats in each airline",las=1,col="cyan")
boxplot(SeatsEconomy~Airline,data=AirlineAnalysis.df,horizontal=TRUE,xlab="No.of seats",ytab="Airline",main="N0.of economy seats in each airline seats in each airline",las=1,col="lightpink")
boxplot(SeatsPremium~Airline,data=AirlineAnalysis.df,horizontal=TRUE,xlab="No.of seats",ytab="Airline",main="No.of premium seats in each airline seats in each airline",las=1,col="grey")

# c3)Visualising other variables to the PriceRelative column

par(mfrow=c(1,1))
boxplot(PriceRelative~MONTH,xlab="Relative price",ylab="Month of travel",main="Boxplot between Relative Price and month of travel",col="yellow") 
boxplot(PriceRelative~IsInternational,xlab="Relative price",ylab="International",main="Boxplot between Relative Price and the travel is domestic or international",col="yellow")   
boxplot(PriceRelative~SeatsPremium,xlab="Relative price",ylab="Premium Seats",main="Boxplot between Relative Price and Premium Seats",col="yellow") 
boxplot(PriceRelative~FlightDuration,xlab="Relative price",ylab="Travel Duration",main="Boxplot between Relative Price and travel duration",col="yellow") 

# d)Finding relations between the variables through the scatterplotMatrix.It was suggested that the PriceRelative depended on some variables.The high predictor variables were chosen in this
#   'car' package was called

library(car)
scatterplotMatrix(formula = ~PriceRelative+SeatsEconomy+PitchEconomy+WidthEconomy+PriceEconomy, cex=0.6,data=AirlineAnalysis.df, diagonal="histogram")
scatterplotMatrix(formula = ~PriceRelative+SeatsPremium+PitchPremium+WidthPremium+PricePremium, cex=0.6,data=AirlineAnalysis.df, diagonal="histogram")
# e) Attempt to to find correlation between the each Premium and Economy
#     Calculating correlations between Prices of Economy and Premium in correlation to other factors
cor.test(PriceEconomy, PitchEconomy)
cor.test(PriceEconomy, WidthEconomy)
cor.test(PricePremium, PitchPremium)
cor.test(PricePremium, WidthPremium)

library(corrgram)
corrgram(AirlineAnalysis.df, main = "Corrgram of Six airplane variables", lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt,order=TRUE)                

# f)Creating a regression model -In this following section 2 models were created one that is a full linear model and second the model created is leaving out of variables
#   that do not create significant difference in the model.
#   'QuantPsyc' package contains the lm.beta that involves in the making standardized coefficients   
#   lm.fit is the full model containing all the variables that affect the PriceRelative
#   lm.fit5 is the reduced model containing the variables that create or give significant effect on the variable  
#   anova() involves the calculating the variance from the actual model
#   'visreg' package was used to create regression lines on given linear model

lm.fit=lm(PriceRelative~., data=AirlineAnalysis.df)
summary(lm.fit)

library(QuantPsyc)
lm.beta(lm.fit)

lm.fit5<-lm(PriceRelative~.-Airline-TravelMonth-Aircraft-IsInternational-SeatsTotal-PitchDifference-WidthDifference-FractionPremiumSeats-SeatsEconomy,data=AirlineAnalysis.df)
summary(lm.fit5)
anova(lm.fit5,lm.fit)

library(visreg)
par(mfrow=c(4,5))
visreg(lm.fit5)
    
# Result:Keeping all the variables it is shown that the lm.fit model has R^2 of 0.78 which is quite good but it does
# contain multilinearity and other overlapping variables.Hence,for calculation of the most significant oncs
# lm.fit5 model was created.This model had R^2 of 0.76 and Adjusted R^2 of 0.7606.
# On applying the anova analysis in these 2 models the F-statistic came out to be 4.1992 and P<0.05
# which implies that the variables that were removed were less significant and had no impact on tha analysis
# visreg function plots function of each variable.