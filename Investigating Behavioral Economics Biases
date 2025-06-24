#Upload the dataset in R.
BE<-read.csv("/Users/wambui/Downloads/Behavioral_Economics_Dataset.csv", header=TRUE, skip=1)
View(BE)

#Calculate key statistics (mean, median, standard deviation, min and max.) for all the numeric variables in the dataset. 
summary(BE)

install.packages("stargazer")
library(stargazer)
stargazer(BE,out="text",type="text")

#Create visualizations (Scatterplot for the following variables) (use ggplot2 and add a trend line; write 2 sentence to describe the relationship and why you think such relationship exists).
#Age and Income
install.packages("ggplot2")
library(ggplot2)
ggplot(BE, aes(y=Income, x=Age)) + geom_point()+ geom_smooth(method="lm",se=FALSE) + ggtitle("Age versus Income")
#b.Trading frequency and confidence score 
ggplot(BE, aes(y=Trading_Frequency, x=Confidence_Score)) + geom_point() + geom_smooth(method="lm")+ ggtitle("Trading frequency versus confidence score ")

#c.	Trading frequency and loss aversion 
ggplot(BE, aes(y=Trading_Frequency, x=Loss_Aversion)) + geom_point()+geom_smooth(method="lm")+  ggtitle("Trading frequency versus loss aversion ")

#d.	Confidence score and loss aversion 
ggplot(BE, aes(y=Loss_Aversion, x=Confidence_Score)) + geom_point()+geom_smooth(method="lm")+  ggtitle("Confidence score versus loss aversion ")

#Test for behavioral biases using the data. Provide statistical evidence (e.g., regression analysis).
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

#1.	Does income affect investment risk-taking? 

model_3<-lm(Trading_Frequency~Income,data=BE)
summary(model_3)

# 2.	Does age influence risk tolerance and loss aversion? 
model_4<-lm(Risk_Tolerance~Age,data=BE)
summary(model_4)

model_5<-lm(Loss_Aversion~Age,data=BE)
summary(model_5)
