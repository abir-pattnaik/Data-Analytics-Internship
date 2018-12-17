# Analysis of MBA Starting Salaries
# NAME: ABIR PATTNAIK
# EMAIL: abir.pattanaik@gmail.com
# COLLEGE : Maharaja Agrasen Institute of Technology

# NOTE:2 datasets have been used
# Dataset1:StartingSalary.csv
# Dataset2:StartingSalary_job.csv

# a) Load the dataset into RStudio and Viewing the dataframe for checking whether the 
#   data set uploaded is correct or not.
#
# Choose your own location of where you have saved the dataset StartingSalary.csv

StartingSalary.df<-read.csv("C:/Users/DRDO HQ/Desktop/DATA ANALYTICS INTERNSHIP/MBA SALARIES PROJECT/StartingSalary.csv")
View(StartingSalary.df)

# a1) Viewing the summary for the first 11 columns as the last 2 columns contain coded symbols which affect the data 

summary(StartingSalary.df[1:11])

# This would ensure that data that has been collected is for both placed and unplaced students
# All these data are collected before and is not required for people to fill data

StartingSalary_TotalStudents.df<-StartingSalary.df[1:11]

# b) Visualising each variable and understanding the graph
#    Age has a positive skewed distribution with median at age 27 and mean at 27.36
#    Table for Male and Female shows that the Male:Female ratio is high
#    A normal distribution curve was made with respect to students count with mean at 619.5
#    and going high as 790.0 
#    A positively distribution curve was established with most of the work experiences being with range of 0-5 yrs exp.

attach(StartingSalary_TotalStudents.df)
hist(age,col="yellow",main="Variation of age",xlab="Age")
hist(sex,col="red",main="Sex of the total students",xlab="Sex")
addmargins(table(sex))
hist(gmat_tot,col="blue",main="Total gmat score of students",xlab="GMAT Score")
hist(work_yrs,col="red",main="Work experience of students",xlab="Work exp")
addmargins(table(frstlang))
detach(StartingSalary_TotalStudents.df)

# c) Segregating the databases into 2 databases
#    PlacedStudents--Students who were placed and mentioned their salary
#    NotPlacedStudents--Students who weren't placed but took part in the survey
#    PlacedStudents--Students who were placed but some of them didn't disclose their salaries

PlacedStudents.df<-StartingSalary.df[which(StartingSalary.df$salary!=999&StartingSalary.df$salary!=998&StartingSalary.df$salary!=0),]
View(PlacedStudents.df)
NotPlacedStudents.df<-StartingSalary.df[which(StartingSalary.df$salary!=999&StartingSalary.df$salary!=998&StartingSalary.df$salary==0),]
View(NotPlacedStudents.df)
PlacedStudents_999.df<-StartingSalary.df[which(StartingSalary.df$salary!=998&StartingSalary.df$salary!=0),]
View(PlacedStudents_999.df)

# d) Creating boxplots to understand the some basic relations of salaries and other factors
#    such as Gender or age or ranking in their college
#    Inferring from the boxplot it is clear that the male salary is higher than female salary 
#    although it the mean is approximately same but it might not be true as there is an outlier in the 
#    female salary that can affect the mean
#    On the basis of age it is clear that with age the salaries do increase.Further analysis should be done in this area
#    There is a fairly linear relationship in the comparison between the Salary and work exp
#    A negative linear relationship was established between the percentile and Salary 

attach(PlacedStudents.df)
boxplot(salary~sex,main="Comparison between the genders and their salary variance",xlab="Gender",ylab="Salary",xaxt="n",col="cyan")
axis(side=1, at=c(1,2), labels=c("Males", "Females"))
boxplot(salary~age,main="Comparison between the salary and age",xlab="Age",ylab="Salary",col="magenta")
boxplot(salary~work_yrs,main="Comparison between the Work exp and Salary",xlab="Work Exp",ylab="Salary",col="magenta")
boxplot(salary~quarter,main="Comparison between the position in college and Salary",xlab="Quarter percentile",ylab="Salary",col="green")
hist(NotPlacedStudents.df$quarter,main="Unplaced students with respect to their percentile",xlab="Quarter",col="yellow")
boxplot(salary~gmat_tot,main="Comparison between the GMAT total and Salary",xlab="GMAT score",ylab="Salary",col="green")
detach(PlacedStudents.df)

# d) Creating a scatterplot matrix and understanding the pattern it makes with salary
#    'car' package was used
#    The relationship between salary and age are linear relationship
#    Same is for the work_exp and salary
#    However due to strong linear relationship between work_exp and age chances are correlated


library(car)
scatterplotMatrix(formula=~work_yrs+age+salary+sex+gmat_tot,cex=0.6,diagonal="histogram",data=PlacedStudents.df)

# e) For checking correlation corrplot was used
#    'corrplot' package was used
#    'corrgram' package was used
#    corrplot provides some very interesting insights of the correlation
#    work_yrs and age have value of 0.88 that is almost close to 1
#    It is evident that gmat_tot and gmat_tpc would be correlated
#    Quarter percentile has a negative correlation of 0.84
#    corrgram depicts the same plots to understand correlationship better in 

library(corrplot)
corrplot(cor(PlacedStudents.df),method="number")
library(corrgram)
corrgram(PlacedStudents.df, main = "Corrgram of Six airplane variables", lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt,order=TRUE)                

# d) Creating a linear model using all factors 
#    Three models are created.Details are given as following:-
#    i)lm.fit_full:This model has all factors for the model
#      Residuals are high and Residual Standard error is too high(15430)
#      r-squared:0.3422 and adjusted r-squared:0.2545
#    ii)lm.fit:This model has factors as frstlang+age+sex+work_yrs+work_yrs:gmat_tot
#      Residuals are high and similarly for RSE(15570)
#      r-squared:0.2702 and adjusted r-squared:0.2409
#    iii)lm.fit2:This factors considers only age as a element
#      Residuals are high and so is the RSE(15550)
#      r-squared:0.2496 and adjusted r-squared:0.2422 
#      vif stands for variance inflation factor
#      'visreg' to realise visualisations of regression made
#      anova was used as a comparison method

lm.fit_full=lm(salary~.,data=PlacedStudents.df)
lm.fit=lm(salary~work_yrs+frstlang+sex+work_yrs:gmat_tot,data=PlacedStudents.df)
lm.fit2=lm(salary~age,data=PlacedStudents.df)
summary(lm.fit_full)
summary(lm.fit)
summary(lm.fit2)
anova(lm.fit,lm.fit_full)
vif(lm.fit)
par(mfrow=c(2,2))
library(visreg)
visreg(lm.fit)

# e) Linear model for people having job and satisfaction from college
#    i)lm.fit3:For Y as a job parameter it is evident that salary would be a factor and so is satisfaction from college 
#    r^2=95.29,adjusted r^2=95.23
#    RSE=0.106
#    ii)lm.fit4:Not considering salary as one of the factors
#    r^2=0.1898 and adjusted r^2=0.176
#    RSE=0.4407


StartingSalary_job.df<-read.csv("C:/Users/DRDO HQ/Desktop/DATA ANALYTICS INTERNSHIP/MBA SALARIES PROJECT/StartingSalary_job.csv")
View(StartingSalary_job.df)
PlacedStudents_job.df<-StartingSalary_job.df[which(StartingSalary_job.df$salary!=999),]
View(PlacedStudents_job.df)

corrplot(cor(PlacedStudents_job.df),method="number")
lm.fit3_full=lm(job~.,data=PlacedStudents_job.df)
lm.fit3=lm(job~salary+work_yrs+satis,data=PlacedStudents_job.df)
lm.fit4=lm(job~age+gmat_tot+work_yrs+satis,data=PlacedStudents_job.df)
summary(lm.fit3)
summary(lm.fit3_full)
summary(lm.fit4)
anova(lm.fit3,lm.fit3_full)
vif(lm.fit3)
par(mfrow=c(3,1))
visreg(lm.fit3)

# f) Perfroming t-tests on the following hypothesis
#    T test between Salary and Gender
#    Chi-square test between Work_exp and Age
#    Dataset used:PlacedStudents.df

attach(PlacedStudents.df)
aggregate(salary,by=list(Gender=sex),FUN=mean)
t.test(salary~sex,alternative="greater")

#    Hypothesis is accepted and Male have greater salary than woman p>0.05 and it is within the confidence interval
#    For the chi-square test between age and work years these are dependent as x-squared is quite high

mytable1 <- xtabs(age~work_yrs, data=PlacedStudents.df)
chisq.test(mytable1)
detach(PlacedStudents.df)

attach(PlacedStudents_job.df)
mytable2 <- xtabs(job~frstlang, data=PlacedStudents_job.df)
mytable2

mytable3 <- xtabs(job~quarter, data=PlacedStudents_job.df)
mytable3
detach(PlacedStudents_job.df)


















