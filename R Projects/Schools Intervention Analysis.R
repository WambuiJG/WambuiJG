#This analysis is based on three datasets: School Visits, Schools and Student Baseline. In this analysis, I am looking at the impact of students' performance/ attendance in school. Part of the students go through an intervention, I analyze the impact of this intervention.


#Uploading and Reading the Data
Schools_Visits<-read.csv("/Users/wambui/Downloads/Raw data-selected/school_visits_log.csv")
View(Schools_Visits)

Schools<-read.csv("/Users/wambui/Downloads/Raw data-selected/schools.csv")
View(Schools)

Student_baseline<-read.csv("/Users/wambui/Downloads/Raw data-selected/student_baseline.csv")
View(Student_baseline)

Student_follow_ups<-read.csv("/Users/wambui/Downloads/Raw data-selected/student_follow_ups.csv")
View(Student_follow_ups)

#Cleaning the Data
# The Student_follow_ups Dataset is the only one in need of cleaning because it has -99 for I don't know and N/A
#Import Tidyverse Library
library(tidyverse)

#Replace -99 and "N/A" with NA
Student_follow_ups <- Student_follow_ups %>%
  mutate(across(everything(), ~ifelse(. == -99 | . == "N/A", NA, .)))

# Check for any remaining -99 or N/A values
sum(Student_follow_ups == -99, na.rm = TRUE)
sum(Student_follow_ups == "N/A", na.rm = TRUE)

# View summary of missing values
summary(Student_follow_ups)
View(Student_follow_ups)

#Combining Data

#I combined student_baseline and student_follow_ups. This is because they both had the students ids.
combined_student_data <- merge(
  x = Student_follow_ups,
  y = Student_baseline[, c(colnames(Student_baseline)[2], "sex", "yob")],
  by.x = colnames(Student_follow_ups)[1],
  by.y = colnames(Student_baseline)[2],
  all.x = TRUE
)
View(combined_student_data)

#I combined schools and school visits because they both had school ids. 

combined_school_data<-merge(
  x= Schools,
  y= Schools_Visits,
  by.x = colnames(Schools)[1],
  by.y = colnames(Schools_Visits)[1],
  all.x = TRUE
)
View(combined_school_data)

#Summary Statistics
install.packages("skimr")
library(skimr)
skim(combined_school_data)
skim(combined_student_data)

# Summary Statistics by Groups of the Schools Data

combined_school_data_treatment <- subset(combined_school_data, treatment == 1)
combined_school_data_control <- subset(combined_school_data, treatment == 0)

View(combined_school_data_treatment)
View(combined_school_data_control)

skim(combined_school_data_treatment)
skim(combined_school_data_control)

#Summary Statistics by Groups of the Students Data
#I wanted to include the treatment column in the dataset so I included it 
combined_school_and_student_dataset <- merge(
  x = combined_student_data,
  y = combined_school_data[, c(colnames(combined_school_data)[1], "treatment")], 
  by.x = colnames(combined_student_data)[1],
  by.y = colnames(combined_school_data)[1],
  all.x = TRUE
)
View(combined_school_and_student_dataset)

combined_student_dataset_treatment <- subset(combined_school_and_student_dataset, treatment ==1 )
combined_student_dataset_control <- subset(combined_school_and_student_dataset, treatment ==0 )

View(combined_student_dataset_treatment)
View(combined_student_dataset_control)

skim(combined_student_dataset_treatment)
skim(combined_student_dataset_control)

#Investigating the impact of the intervention on the outcomes of interest after 3 and 5 years

#Looking for the Number of Dropout overs the Years in the Treatment Group 

# Initialize counters
dropout_year_3 <- 0
dropout_year_5 <- 0

# Loop through each row
for (i in 1:nrow(combined_student_dataset_treatment)) {
  year_value <- combined_student_dataset_treatment$year[i]
  dropout_value <- combined_student_dataset_treatment$dropout[i]
  
  if (!is.na(year_value) && !is.na(dropout_value)) {
    if (year_value == 3 && dropout_value == 1) {
      dropout_year_3 <- dropout_year_3 + 1
    } else if (year_value == 5 && dropout_value == 1) {
      dropout_year_5 <- dropout_year_5 + 1
    }
  }
}

# Print results
print(paste("Dropouts in Year 3:", dropout_year_3))
print(paste("Dropouts in Year 5:", dropout_year_5))

#Looking for the Number of Married overs the Years in the Treatment Group 

# Initialize counters
married_year_3 <- 0
married_year_5 <- 0

# Loop through each row
for (i in 1:nrow(combined_student_dataset_treatment)) {
  year_value <- combined_student_dataset_treatment$year[i]
  married_value <- combined_student_dataset_treatment$married[i]
  
  if (!is.na(year_value) && !is.na(married_value)) {
    if (year_value == 3 && married_value == 1) {
      married_year_3 <- married_year_3 + 1
    } else if (year_value == 5 && married_value == 1) {
      married_year_5 <- married_year_5 + 1
    }
  }
}

# Print results
print(paste("Married in Year 3:", married_year_3))
print(paste("Married in Year 5:", married_year_5))

#Looking for the Number of Pregnant overs the Years in the Treatment Group 

# Initialize counters
pregnant_year_3 <- 0
pregnant_year_5 <- 0

# Loop through each row
for (i in 1:nrow(combined_student_dataset_treatment)) {
  year_value <- combined_student_dataset_treatment$year[i]
  pregnant_value <- combined_student_dataset_treatment$pregnant[i]
  
  if (!is.na(year_value) && !is.na(pregnant_value)) {
    if (year_value == 3 && pregnant_value == 1) {
      pregnant_year_3 <- pregnant_year_3 + 1
    } else if (year_value == 5 && pregnant_value == 1) {
      pregnant_year_5 <- pregnant_year_5 + 1
    }
  }
}

# Print results
print(paste("Pregnant in Year 3:", pregnant_year_3))
print(paste("Pregnant in Year 5:", pregnant_year_5))
#Looking for the Number of Dropout overs the Years in the Control Group 

# Initialize counters
dropout_year_3 <- 0
dropout_year_5 <- 0

# Loop through each row
for (i in 1:nrow(combined_student_dataset_control)) {
  year_value <- combined_student_dataset_control$year[i]
  dropout_value <- combined_student_dataset_control$dropout[i]
  
  if (!is.na(year_value) && !is.na(dropout_value)) {
    if (year_value == 3 && dropout_value == 1) {
      dropout_year_3 <- dropout_year_3 + 1
    } else if (year_value == 5 && dropout_value == 1) {
      dropout_year_5 <- dropout_year_5 + 1
    }
  }
}

# Print results
print(paste("Dropouts in Year 3:", dropout_year_3))
print(paste("Dropouts in Year 5:", dropout_year_5))

#Looking for the Number of Married overs the Years in the Control Group 

# Initialize counters
married_year_3 <- 0
married_year_5 <- 0

# Loop through each row
for (i in 1:nrow(combined_student_dataset_control)) {
  year_value <- combined_student_dataset_control$year[i]
  married_value <- combined_student_dataset_control$married[i]
  
  if (!is.na(year_value) && !is.na(married_value)) {
    if (year_value == 3 && married_value == 1) {
      married_year_3 <- married_year_3 + 1
    } else if (year_value == 5 && married_value == 1) {
      married_year_5 <- married_year_5 + 1
    }
  }
}

# Print results
print(paste("Married in Year 3:", married_year_3))
print(paste("Married in Year 5:", married_year_5))

#Looking for the Number of Pregnant overs the Years in the Control Group 

# Initialize counters
pregnant_year_3 <- 0
pregnant_year_5 <- 0

# Loop through each row
for (i in 1:nrow(combined_student_dataset_control)) {
  year_value <- combined_student_dataset_control$year[i]
  pregnant_value <- combined_student_dataset_control$pregnant[i]
  
  if (!is.na(year_value) && !is.na(pregnant_value)) {
    if (year_value == 3 && pregnant_value == 1) {
      pregnant_year_3 <- pregnant_year_3 + 1
    } else if (year_value == 5 && pregnant_value == 1) {
      pregnant_year_5 <- pregnant_year_5 + 1
    }
  }
}

# Print results
print(paste("Pregnant in Year 3:", pregnant_year_3))
print(paste("Pregnant in Year 5:", pregnant_year_5))

#Looking for the Number of Dropout overs the Years in the Treatment Group 

# Initialize counters
dropout_year_3 <- 0
dropout_year_5 <- 0

# Loop through each row
for (i in 1:nrow(combined_student_dataset_treatment)) {
  year_value <- combined_student_dataset_treatment$year[i]
  dropout_value <- combined_student_dataset_treatment$dropout[i]
  
  if (!is.na(year_value) && !is.na(dropout_value)) {
    if (year_value == 3 && dropout_value == 1) {
      dropout_year_3 <- dropout_year_3 + 1
    } else if (year_value == 5 && dropout_value == 1) {
      dropout_year_5 <- dropout_year_5 + 1
    }
  }
}

# Print results
print(paste("Dropouts in Year 3:", dropout_year_3))
print(paste("Dropouts in Year 5:", dropout_year_5))


#Looking into The effects on school evasion for girls and boys
library(ggplot2)

# Initialize counters for boys and girls dropouts
dropout_boys <- 0
dropout_girls <- 0

#Combine datasets
combined_dataset <- merge(
  x = combined_student_data,
  y = combined_school_data, 
  by.x = colnames(combined_student_data)[1],
  by.y = colnames(combined_school_data)[1],
  all.x = TRUE
)
View(combined_dataset)
# Loop through each row in the dataset to count dropouts by gender
for (i in 1:nrow(combined_dataset)) {
  sex_value <- combined_student_data$sex[i]
  dropout_value <- combined_student_data$dropout[i]
  
  if (!is.na(sex_value) && !is.na(dropout_value)) {
    if (sex_value == 1 && dropout_value == 1) {
      dropout_boys <- dropout_boys + 1
    } else if (sex_value == 2 && dropout_value == 1) {
      dropout_girls <- dropout_girls + 1
    }
  }
}

# Create a data frame for plotting
dropout_data <- data.frame(
  Gender = c("Boys", "Girls"),
  Dropouts = c(dropout_boys, dropout_girls)
)

# Create a bar graph using ggplot2
ggplot(dropout_data, aes(x = Gender, y = Dropouts, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Dropouts by Gender", 
       x = "Gender", 
       y = "Number of Dropouts") +
  theme_minimal()


