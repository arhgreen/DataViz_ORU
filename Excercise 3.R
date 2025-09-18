###############################################################################
# Exercise 3
###############################################################################

# Installing necesary packages
install.packages("tidyverse") # ggplot2, dplyr, stringr etc
install.packages("data.table")

# Loading the libraries
library(tidyverse)
library(data.table)

## Setting working directory 
setwd("~/Documents/DataViz_ORU")

## Loading data
ds_survey <- fread("multipleChoiceResponses.csv", 
                   skip = 1,
                   na.strings = c("", "NA")) # treating "" as NA for consistency

# reviewing variable names before filtering out the needed ones
colnames(ds_survey)

# Selecting the needed variables
dss <- ds_survey[, c(1, 2, 4, 5, 6, 7, 8, 10, 12, 13, 14, 23)]

# MISSING VALUES
# Checking for missing values
sum(is.na(dss)) # 18 749 missing values from the filtered columns

# Renaming the variables
dss <- dss %>% 
 rename(duration = "Duration (in seconds)",
         gender = "What is your gender? - Selected Choice",
         age = "What is your age (# years)?",
         country = "In which country do you currently reside?",
         education = "What is the highest level of formal education that you have attained or plan to attain within the next 2 years?",
         undergrad = "Which best describes your undergraduate major? - Selected Choice",
         title = "Select the title most similar to your current role (or most recent title if retired): - Selected Choice",
         industry = "In what industry is your current employer/contract (or your most recent employer if retired)? - Selected Choice",
         experience_current_role = "How many years of experience do you have in your current role?",
         yearly_wage = "What is your current yearly compensation (approximate $USD)?",
         machine_learning_employer = "Does your current employer incorporate machine learning methods into their business?",
         analysis_too_work_business = "What is the primary tool that you use at work or school to analyze data? (include text response) - Selected Choice"
         )

# Reviewing variabkle names after filtering
colnames(dss)        

# VARIABLE TRANSFORMATION AND CLEANING

# Age
dss <- dss %>% 
  mutate(age = factor(age,
                      levels = c("18-21",
                                 "22-24",
                                 "25-29",
                                 "35-39",
                                 "40-44",
                                 "45-49",
                                 "50-54",
                                 "55-59",
                                 "60-69",
                                 "70-79",
                                 "80+"),
                      ordered = TRUE))
table(dss$age, 
      useNA = "always") # 3 776 Missing values

# Country
dss <- dss %>%
  mutate(country = str_replace_all(country, "[[:punct:]]", "")) %>%
  mutate(country = str_replace(country, "Hong Kong SAR", "Hong Kong")) %>%
  mutate(country = str_replace(country, "Iran Islamic Republic of", "Iran")) %>% 
  mutate(country = str_replace(country, "Republic of Korea", "South Korea")) %>%
  mutate(country = str_replace(country, "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>%
  mutate(country = str_replace(country, "United States of America", "United States")) %>%
  mutate(country = str_replace(country, "Viet Nam", "Vietnam")) %>% 
  mutate(country = str_replace(country, "I do not wish to disclose my location", "Undisclosed")) %>% 
  mutate(country = factor(country))

table(dss$country,useNA="always") # 0 missing values

# Education and ordering the levels appropriately 
dss <- dss %>%
  mutate(education = recode(education,
                            `No formal education past high school` = "High school",
                            `Some college/university study without earning a bachelor’s degree` = "Started bachelor",
                            `Professional degree` = "Professional",
                            `Bachelor’s degree` = "Bachelor's",
                            `Master’s degree` = "Master's",
                            `Doctoral degree` = "Doctoral",
                            `I prefer not to answer` = NA_character_)) %>% 
  mutate(education = factor(education,
                            levels = c("High school",
                                       "Started bachelor",
                                       "Professional",
                                       "Bachelor's",
                                       "Master's",
                                       "Doctoral"),
                            ordered = TRUE))

table(dss$education,useNA="always") # 766 missing values 

# Handling Missing values in education 





#Undergraduate major
dss <- dss %>%
  mutate(undergrad = recode(undergrad,
                            `A business discipline (accounting, economics, finance, etc.)` = "Business",
                            `Other` = "Other majors",
                            `Computer science (software engineering, etc.)` = "Computer science/Software engineering",
                            `Engineering (non-computer focused)` = "Other Engineering",
                            `Humanities (history, literature, philosophy, etc.)` = "Humanities",
                            `Information technology, networking, or system administration` = "Information technology",
                            `Medical or life sciences (biology, chemistry, medicine, etc.)` = "Medical or life sciences",
                            `Social sciences (anthropology, psychology, sociology, etc.)` = "Social sciences",
                            'I never declared a major' = "Undeclared",
                            `""` = NA_character_)) %>% 
  mutate(undergrad = factor(undergrad,
                            ordered = FALSE))

table(dss$undergrad,useNA="always") # 912 missing values


# Industry
dss <- dss %>%
  mutate(industry = recode(industry,
                           `I am a student` = "Student",
                           `Biotech or pharmaceuticals` = "Biotech/Pharmaceuticals",
                           `Online Service/Internet-based Services` = "Online services",
                           `Military/Security/Defense` = "Defense",
                           `Hospitality/Entertainment/Sports` = "Hospitality",
                           `Online Business/Internet-based Sales` = "Online services",
                           `Government/Public Service` = "Public sector")) %>% 
  mutate(industry = factor(industry,
                            ordered = FALSE))

table(dss$industry,useNA="always") # 2 174 missing values

# Job title
dss <- dss %>% 
  mutate(title = factor(title, 
                        ordered = FALSE))

table(dss$title,useNA="always") # 959 missing values
