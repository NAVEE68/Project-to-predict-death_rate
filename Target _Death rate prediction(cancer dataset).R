#Cancer Target 

data=read.csv('cancer_reg.csv')

summary(data)
is.na(data)
colSums(is.na(data))

#Impute the missing values
data$PctSomeCol18_24[is.na(data$PctSomeCol18_24)]=mean(data$PctSomeCol18_24,na.rm = T)

data$PctEmployed16_Over[is.na(data$PctEmployed16_Over)]=mean(data$PctEmployed16_Over,na.rm = T)

data$PctPrivateCoverageAlone[is.na(data$PctPrivateCoverageAlone)]=mean(data$PctPrivateCoverageAlone,na.rm = T)

colSums(is.na(data))

sum(is.na(data))

#Encode the categorical variable

levels(data$Geography)
unique(data$Geography)

#Since there are 3047 levels for the Geography,we give unique names for each column but delete both of the categorical variables, as they have no impact on the dependent variable
levels(data$Geography)=1:3047
data$Geography=NULL

data$binnedInc=NULL

#Splitting the dataset
split=sample.split(data,SplitRatio = 2/3)
train=subset(data,split==T)
test=subset(data,split==F)

#Feature Scaling
train=scale(train)
test=scale(test)

class(data)
class(train)
train=data.frame(train)
test=data.frame(test)
class(test)

#Now,its time to build our model
#We'll use the Backward Elimination to build our model
regressor=lm(formula=TARGET_deathRate ~ .,
             data = train)
summary(regressor)

attach(data)
regressor=lm(formula=TARGET_deathRate~avgAnnCount+avgDeathsPerYear+incidenceRate+medIncome+popEst2015+povertyPercent+MedianAge+MedianAgeMale+MedianAgeFemale+AvgHouseholdSize+PercentMarried+PctNoHS18_24+PctHS18_24+PctSomeCol18_24+PctBachDeg18_24+PctHS25_Over+PctBachDeg25_Over+PctEmployed16_Over+PctUnemployed16_Over+PctPrivateCoverage+PctPrivateCoverageAlone+PctEmpPrivCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctWhite+PctBlack+PctAsian+PctOtherRace+PctMarriedHouseholds+BirthRate, data = train)
summary(regressor)

regressor=lm(formula=TARGET_deathRate~avgAnnCount+avgDeathsPerYear+incidenceRate+medIncome+popEst2015+povertyPercent+MedianAge+MedianAgeMale+MedianAgeFemale+AvgHouseholdSize+PercentMarried+PctNoHS18_24+PctHS18_24+PctSomeCol18_24+PctBachDeg18_24+PctHS25_Over+PctBachDeg25_Over+PctEmployed16_Over+PctUnemployed16_Over+PctPrivateCoverage+PctPrivateCoverageAlone+PctEmpPrivCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctWhite+PctBlack+PctOtherRace+PctMarriedHouseholds+BirthRate, data = train)
summary(regressor)

regressor=lm(formula=TARGET_deathRate~avgAnnCount+avgDeathsPerYear+incidenceRate+medIncome+popEst2015+povertyPercent+MedianAge+MedianAgeMale+MedianAgeFemale+PercentMarried+PctNoHS18_24+PctHS18_24+PctSomeCol18_24+PctBachDeg18_24+PctHS25_Over+PctBachDeg25_Over+PctEmployed16_Over+PctUnemployed16_Over+PctPrivateCoverage+PctPrivateCoverageAlone+PctEmpPrivCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctWhite+PctBlack+PctOtherRace+PctMarriedHouseholds+BirthRate, data = train)
summary(regressor)

regressor=lm(formula=TARGET_deathRate~avgAnnCount+avgDeathsPerYear+incidenceRate+medIncome+popEst2015+povertyPercent+MedianAgeMale+MedianAgeFemale+PercentMarried+PctNoHS18_24+PctHS18_24+PctSomeCol18_24+PctBachDeg18_24+PctHS25_Over+PctBachDeg25_Over+PctEmployed16_Over+PctUnemployed16_Over+PctPrivateCoverage+PctPrivateCoverageAlone+PctEmpPrivCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctWhite+PctBlack+PctOtherRace+PctMarriedHouseholds+BirthRate, data = train)
summary(regressor)

regressor=lm(formula=TARGET_deathRate~avgAnnCount+avgDeathsPerYear+incidenceRate+medIncome+popEst2015+povertyPercent+MedianAgeMale+MedianAgeFemale+PercentMarried+PctNoHS18_24+PctHS18_24+PctSomeCol18_24+PctBachDeg18_24+PctHS25_Over+PctBachDeg25_Over+PctEmployed16_Over+PctUnemployed16_Over+PctPrivateCoverage+PctEmpPrivCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctWhite+PctBlack+PctOtherRace+PctMarriedHouseholds+BirthRate, data = train)
summary(regressor)

regressor=lm(formula=TARGET_deathRate~avgAnnCount+avgDeathsPerYear+incidenceRate+medIncome+popEst2015+povertyPercent+MedianAgeMale+MedianAgeFemale+PercentMarried+PctNoHS18_24+PctHS18_24+PctSomeCol18_24+PctHS25_Over+PctBachDeg25_Over+PctEmployed16_Over+PctUnemployed16_Over+PctPrivateCoverage+PctEmpPrivCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctWhite+PctBlack+PctOtherRace+PctMarriedHouseholds+BirthRate, data = train)
summary(regressor)

regressor=lm(formula=TARGET_deathRate~avgAnnCount+avgDeathsPerYear+incidenceRate+popEst2015+povertyPercent+MedianAgeMale+MedianAgeFemale+PercentMarried+PctNoHS18_24+PctHS18_24+PctSomeCol18_24+PctHS25_Over+PctBachDeg25_Over+PctEmployed16_Over+PctUnemployed16_Over+PctPrivateCoverage+PctEmpPrivCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctWhite+PctBlack+PctOtherRace+PctMarriedHouseholds+BirthRate, data = train)
summary(regressor)

regressor=lm(formula=TARGET_deathRate~avgAnnCount+avgDeathsPerYear+incidenceRate+popEst2015+povertyPercent+MedianAgeMale+PercentMarried+PctNoHS18_24+PctHS18_24+PctSomeCol18_24+PctHS25_Over+PctBachDeg25_Over+PctEmployed16_Over+PctUnemployed16_Over+PctPrivateCoverage+PctEmpPrivCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctWhite+PctBlack+PctOtherRace+PctMarriedHouseholds+BirthRate, data = train)
summary(regressor)

#This is the optimal regressor
regressoroptimal=lm(formula=TARGET_deathRate~avgAnnCount+avgDeathsPerYear+incidenceRate+popEst2015+povertyPercent+MedianAgeMale+PercentMarried+PctNoHS18_24+PctHS18_24+PctHS25_Over+PctBachDeg25_Over+PctEmployed16_Over+PctUnemployed16_Over+PctPrivateCoverage+PctEmpPrivCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctWhite+PctBlack+PctOtherRace+PctMarriedHouseholds+BirthRate, data = train)
summary(regressoroptimal)

y_pred=predict(regressoroptimal,newdata = test)

library(hydroGOF)
rmsetest=rmse(test$TARGET_deathRate,y_pred)
rmsetest

