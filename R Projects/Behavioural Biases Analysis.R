#This analysis is based on a dataset on Behavioural Biases. The dataset has 500 entries and 7 columns (age, income, risk tolerance, trading frequency, confidence score and loss aversion).
#This analysis looks into the factors that affect these behavioural biases. 

#Uploading the dataset in R.
BE<-read.csv("/Users/wambui/Downloads/Behavioral_Economics_Dataset.csv", header=TRUE, skip=1)
View(BE)

#Calculating key statistics (mean, median, standard deviation, min and max.) for all the numeric variables in the dataset. 
summary(BE)

install.packages("stargazer")
library(stargazer)
stargazer(BE,out="text",type="text")

#Creating scatterplots
#Age and Income
install.packages("ggplot2")
library(ggplot2)
ggplot(BE, aes(y=Income, x=Age)) + geom_point()+ geom_smooth(method="lm",se=FALSE) + ggtitle("Age versus Income")
#Trading frequency and confidence score 
ggplot(BE, aes(y=Trading_Frequency, x=Confidence_Score)) + geom_point() + geom_smooth(method="lm")+ ggtitle("Trading frequency versus confidence score ")

#Trading frequency and loss aversion 
ggplot(BE, aes(y=Trading_Frequency, x=Loss_Aversion)) + geom_point()+geom_smooth(method="lm")+  ggtitle("Trading frequency versus loss aversion ")

#Confidence score and loss aversion 
ggplot(BE, aes(y=Loss_Aversion, x=Confidence_Score)) + geom_point()+geom_smooth(method="lm")+  ggtitle("Confidence score versus loss aversion ")

#Testing for behavioral biases using the data using regression analysis
#1.	Overconfidence bias 
model<-lm(Trading_Frequency~Confidence_Score,data=BE)
summary(model)
plot(model)

#2.	Loss aversion 
str(BE)

BE$Investment_Decision<- factor(BE$Investment_Decision,levels= c("Safe","Moderate","Risky"),labels=c(1,2,3))
str(BE)
BE$Investment_Decision

model_2<-lm(Loss_Aversion~Investment_Decision,data=BE)


summary(model_2)
plot(model_2)

#Testing for how income affects investment risk-taking.

model_3<-lm(Trading_Frequency~Income,data=BE)
summary(model_3)

# Testing for how age influences risk tolerance.
model_4<-lm(Risk_Tolerance~Age,data=BE)
summary(model_4)

# Testing for how age influences loss aversion
model_5<-lm(Loss_Aversion~Age,data=BE)
summary(model_5)
