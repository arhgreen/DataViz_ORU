# Excercise 2 

#######################################################################
# SET UP FROM EXCERCISE 1
#######################################################################

## Installing necessary packages 
install.packages("ggplot2")  # For Data visualisation
install.packages("tidyverse") # For data Manipulation


## Loading necessary packages
library(ggplot2)
library(tidyverse)


# Fetching the Heart disease data from the given link 
link <- "https://www.statlearning.com/s/Heart.csv"
heart_disease <- read.csv(link)

# Exploring the data
head(heart_disease)


#######################################################################
# DATA MANIPULATION & TRANSFROMATION 
#######################################################################

# Removing the first column in the dataset as it is just row numbers
heart_disease <- heart_disease[, -1]

# Converting variable names to lowercase to simplify data handling
names(heart_disease) <- tolower(names(heart_disease))

# explicitly define the variables chestpain, thal and ahd to match the specified levels 

heart_disease <- heart_disease %>% 
  mutate(chestpain = recode(chestpain, 
                            "asymptomatic" = 0, 
                            "nontypical" = 1, 
                            "nonanginal" = 2, 
                            "typical" = 3),
         thal = recode(thal, 
                       "fixed" = 1, 
                       "normal" = 2, 
                       "reversable" = 3),
         ahd = recode(ahd,
                      "No" = 1,
                      "Yes" = 0))

# renaming ahd to target for better understanding
heart_disease <- heart_disease %>% 
  rename(target = ahd)

# FACTORING VARIABLES ACCORDINGLY
#NB: slope has been coded with 1, 2, 3 to match content in the dataset 
heart_disease <- heart_disease %>% 
  mutate(sex = factor(sex,
                      levels = c(0, 1),
                      labels = c("female", "male")),
         chestpain = factor(chestpain,
                            levels = c(0, 1, 2, 3),
                            labels = c("asymptomatic", "nontypical",
                                       "nonanginal", "typical")),
         fbs = factor(fbs, 
                      levels = c(0, 1),
                      labels = c("false", "true")),
         restecg = factor(restecg,
                          levels = c(0, 1, 2),
                          labels = c("showing probable or definite left ventricular hypertrophy by Estes’ criteria",
                                     "normal",
                                     "having ST-T wave abnormality")),
         exang = factor(exang,
                        levels = c(0, 1),
                        labels = c("no", "yes")),
         slope = factor(slope,
                        levels = c(1, 2, 3),
                        labels = c("downsloping", "flat", "upsloping")),
         thal = factor(thal,
                       levels = c(1, 2, 3),
                       labels = c("fixed", "normal", "reversable")),
         ca = factor(ca,
                     levels = c(0, 1, 2, 3),
                     labels = c("0", "1", "2", "3")),
         target = factor(target,
                         levels = c(0, 1),
                         labels = c("yes", "no"))
         
  )

#######################################################################
# EXCERCISE 2
#######################################################################

# Exploring summary statistics of the continuous variables in the dataset
summary(heart_disease)

# Imputation by MAR (missing at random) for the missing values in the dataset
# I've assumed MAR since there isn't many values missing. The missing values only 
# appear in two variables namely thal and ca each accounting for 2 and 4, respectively. 

# Specifying appropriate imputation methods for each variable
spec_method <- mice::make.method(heart_disease)

# Explicitly specifying the imputation methods for the variables with missing values
spec_method["thal"] <- "polyreg" # Since it's unordinal categorical
spec_method["ca"] <- "polr" # ordinal categorical (0–3)


# Performing the imputation  
imputed_data <- mice::mice(heart_disease, 
                           method = spec_method, # specifying the methods
                           m = 6, # 6 imputed datasets
                           maxit = 1, # one iteration is enough given few missing values
                           seed = 1234) # setting seed for reproducibility
# Checking the imputed values
imputed_data$imp

# Extracting the completed dataset after imputation
heart_disease_completed <- mice::complete(imputed_data,
                                          1)

sum(is.na(heart_disease_completed)) # checking for any remaining missing values

# Exploring the structure of the completed dataset  
str(heart_disease_completed)
# Exploring summary statistics of the completed dataset
summary(heart_disease_completed)  # Missing values have been successfully imputed

###SCATTERPLOT
heart_disease_completed %>% 
  ggplot(aes(x = maxhr, y = chol)) +
  geom_point(color = "cornflowerblue", alpha = 0.6) + # scatter points
  labs(
    title = "Scatterplot of Max heart rate vs Cholesterol",
    x = "Max heart rate (bpm)",
    y = "Cholesterol (mg/dl)"
  ) +
  theme_minimal()
# From the scatterplot, there seems to be a weak positive correlation between 
# age and cholesterol levels i.e. the older people get, the higher their 
# cholesterol levels tend to get.

# CHANGES TO THE PLOT
heart_disease_completed %>% 
  ggplot(aes(x = maxhr, # x_var
             y = chol )) + # y_var 
  geom_point(color = "cornflowerblue", alpha = 0.6) + 
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # Adding regression line with confidence interval
  labs(
    title = "Max heart rate Vs Cholesterol",
    x = " Max heart rate (bpm)",
    y = "Cholesterol (mg/dl)"
  ) +
  theme_minimal()
# Adding a regression line with confidence interval to better illustrate the correlation.
# The weak positive correlation between age and cholesterol levels is further confirmed

# Setting a figure on the Correlation
cor(heart_disease_completed[, c("maxhr", 
                                "chol")])

# The correlation coefficient is ≈-0.0034, which indicates an extremely weak negative correlation

# HISTOGRAM
heart_disease_completed %>% 
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Age",
    x = "Age (years)",
    y = "Frequency"
  ) +
  theme_minimal()
# From the histogram, the age distribution of patients is right-skewed, with a higher   