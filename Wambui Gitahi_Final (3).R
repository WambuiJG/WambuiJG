#Load the data
LE<-read.csv("/Users/wambui/Downloads/Life Expectancy Data.csv")
View(LE)
#Omitting NA values
LE_na<-na.omit(LE)
View(LE_na)
#Run the summary of the dataset in R
summary(LE_na)
#A more beautiful way to do this
install.packages("stargazer")
library(stargazer)
stargazer(LE_na,out="text",type="text")

#Use the dplyr package in R 
install.packages("dplyr")
library(dplyr)

#group developed and developing countries. 
#df_grp_status <-df %>% group_by(Status)
#View(df_grp_status)

#Use the dplyr package in R and show the scatterplot between your dependent variable (life expectancy) and your independent variable (GDP) and show them by the groups of developed and developing countries. 
ggplot(data = LE_na, aes(x = GDP, y = Life.expectancy, color = Status)) +
  geom_point(alpha = 0.5) +  # Scatter points, semi-transparent
  geom_smooth(method = "lm", se = FALSE) +  # Regression lines for each group
  labs(title = "GDP vs Life Expectancy by Status",
       x = "GDP (in USD)",
       y = "Life Expectancy (in years)",
       color = "Country Status") +
  theme_minimal() +
  theme(legend.position = "bottom")

#Now let us look at trends in healthcare expenditure and life expectancy. Repeat step 4. 
ggplot(data = LE_na, aes(x = Total.expenditure, y = Life.expectancy, color = Status)) +
  geom_point(alpha = 0.5) +  # Scatter points, semi-transparent
  geom_smooth(method = "lm", se = FALSE) +  # Regression lines for each group
  labs(title = "Health Expenditure vs Life Expectancy by Status",
       x = "Health Expenditure (in %)",
       y = "Life Expectancy (in years)",
       color = "Country Status") +
  theme_minimal() +
  theme(legend.position = "bottom")
#Make correlation matrix of the independent variables. Do you see any multicollinearity between the variables? If yes, then drop the variables. 
colnames(LE_na)
#Choose the Life expectancy as your dependent variable and choose independent variables schooling and heath care expenditure, BMI, population, measles and polio apart from GDP.
#Make correlation matrix of the independent variables. Do you see any multicollinearity between the variables? If yes, then drop the variables. 
LED_na %>% select(c(14,22,18,11,22,10,13)) %>% modelsummary::datasummary_correlation()

#Running regression without dummy
multim<-lm(Life.expectancy~GDP+Total.expenditure+
                 Schooling+Population+BMI+Measles+Polio, data =LE_na)
summary(multim)


#Create a dummy variable for the developing countries (use developing country =1 or 0 otherwise). Run a multiple regression and report the results. 
LE_dummies <- LE_na %>%
  mutate(developing = ifelse(Status == "Developing", 1, 0))
View(LE_dummies)
Life_dummies

#Running regression with just dummy variable
multimodel<-lm(Life.expectancy~GDP+Total.expenditure+
                    Schooling+Population+BMI+developing+Measles+Polio, data = LE_dummies)
summary(multimodel)

#Run a regression again with an interaction term between developing country and schooling variable. Report the results. 
#We want to create an interaction term
multimodel1<-lm(Life.expectancy~GDP+Total.expenditure+
                    Measles+Polio+Population+BMI+developing+ 
                    Schooling*developing, 
                  data = LE_dummies)
summary(multimodel1)
#Run a regression and include an interaction term of the dummy variable with immunization (Hepatitis B). Report the results.  
multimodel2<-lm(Life.expectancy~GDP+Total.expenditure+
                  Measles+Polio+Population+BMI+developing+ 
                  Schooling+ Hepatitis.B*developing, 
                data = LE_dummies)
summary(multimodel2)

#Repeat the step with health expenditure and developing country dummy variable.
multimodel3<-lm(Life.expectancy~GDP+
                  Measles+Polio+Population+BMI+developing+ 
                  Schooling+ Total.expenditure*developing, 
                data = LE_dummies)
summary(multimodel3)

results<-stargazer(multimodel1,multimodel2,multimodel3, out="text",type="text",title="Regression Results")
getwd()

