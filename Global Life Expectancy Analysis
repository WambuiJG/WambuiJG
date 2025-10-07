#This is a Life Expectancy Dataset with 16 variables and 2,938 entries. In this project, I was investigating the factors that affect Life Expectancy in Developing and Developed countries. 

#The variables include Country factor Country name, Year of the data, Country, Country status (developed or developing), Life expectancy in age, Adult Mortality Rates of both sexes (probability of dying between 15 and 60 years per 1000 population), Number of Infant Deaths per 1000 Population, Alcohol, recorded per capita (15+) consumption (in liters of pure alcohol) Percentage,  Expenditure on health as a percentage of Gross Domestic Product per capita (%), Hepatitis B (HepB) immunization
#coverage among 1-year-olds (%),number of reported Measles cases per 1000, Body Mass Index of entire population, Number of under-five deaths per 1000 population, Polio immunization coverage among 1-year-olds (%), General government expenditure on health as a percentage of total government expenditure (%), Diphtheria tetanus toxoid and pertussis
#(DTP3) immunization coverage among 1-year-olds (%) room), HIV.AIDS Deaths per 1 000 live births (0-4 years), Gross Domestic Product per capita (in USD), Population of the country. 


#Load the data
LE<-read.csv("/Users/wambui/Downloads/Life Expectancy Data.csv")
View(LE)

#Cleaning Data
#Omitting NA values
LE_na<-na.omit(LE)
View(LE_na)

#Exploratory Data Analysis
#Summarizing the dataset. This shows the descriptive statistics. 
install.packages("stargazer")
library(stargazer)
stargazer(LE_na,out="text",type="text")

#Installing the dplyr package. This package is needed to create a scatterplot 
install.packages("dplyr")
library(dplyr)


#the scatterplot between life expectancy (dependent variable) and GDP (independent variable)  by the groups of developed and developing countries. 
#I did this to visually see if there is a correlation between the two. 
ggplot(data = LE_na, aes(x = GDP, y = Life.expectancy, color = Status)) +
  geom_point(alpha = 0.5) +  # Scatter points, semi-transparent
  geom_smooth(method = "lm", se = FALSE) +  # Regression lines for each group
  labs(title = "GDP vs Life Expectancy by Status",
       x = "GDP (in USD)",
       y = "Life Expectancy (in years)",
       color = "Country Status") +
  theme_minimal() +
  theme(legend.position = "bottom")

#Now let us look at trends in healthcare expenditure and life expectancy. 
ggplot(data = LE_na, aes(x = Total.expenditure, y = Life.expectancy, color = Status)) +
  geom_point(alpha = 0.5) +  # Scatter points, semi-transparent
  geom_smooth(method = "lm", se = FALSE) +  # Regression lines for each group
  labs(title = "Health Expenditure vs Life Expectancy by Status",
       x = "Health Expenditure (in %)",
       y = "Life Expectancy (in years)",
       color = "Country Status") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Looking at Life expectancy as dependent variable and independent variables schooling and heath care expenditure, BMI, population, measles and polio apart from GDP.

#Before doing that, I will make a correlation matrix of the independent variable to check and see if there is any multicollinearity between the variables. If there is one, I will drop those variables. 
colnames(LE_na) 
LED_na %>% select(c(14,22,18,11,22,10,13)) %>% modelsummary::datasummary_correlation()

#Running regression without dummy
multim<-lm(Life.expectancy~GDP+Total.expenditure+
             Schooling+Population+BMI+Measles+Polio, data =LE_na)
summary(multim)


#Creating a dummy variable for the developing countries (use developing country =1 or 0 otherwise). Using dummy variables because Status is a categorical variable. 
LE_dummies <- LE_na %>%
  mutate(developing = ifelse(Status == "Developing", 1, 0))
View(LE_dummies)
Life_dummies

#Running regression with just dummy variable
multimodel<-lm(Life.expectancy~GDP+Total.expenditure+
                 Schooling+Population+BMI+developing+Measles+Polio, data = LE_dummies)
summary(multimodel)

#Running a regression again with an interaction term between developing country and schooling variable. 
#I want to examine whether the relationship between life expectancy and Schooling changes depending on whether the country developing or developed.
#Creating an interaction term
multimodel1<-lm(Life.expectancy~GDP+Total.expenditure+
                  Measles+Polio+Population+BMI+developing+ 
                  Schooling*developing, 
                data = LE_dummies)
summary(multimodel1)
#Running a regression and include an interaction term of the dummy variable with immunization specifically Hepatitis B. 
#I want to examine whether the relationship between life expectancy and immunization specifically Hepatitis B changes depending on whether the country developing or developed.
multimodel2<-lm(Life.expectancy~GDP+Total.expenditure+
                  Measles+Polio+Population+BMI+developing+ 
                  Schooling+ Hepatitis.B*developing, 
                data = LE_dummies)
summary(multimodel2)

#Running a regression and including an interaction term of the dummy variable with total expenditure 
##I want to examine whether the relationship between life expectancy and total expenditure changes depending on whether the country developing or developed.
multimodel3<-lm(Life.expectancy~GDP+
                  Measles+Polio+Population+BMI+developing+ 
                  Schooling+ Total.expenditure*developing, 
                data = LE_dummies)
summary(multimodel3)

#Visualizing all the linear regression results
results<-stargazer(multimodel1,multimodel2,multimodel3, out="text",type="text",title="Regression Results")
getwd()

