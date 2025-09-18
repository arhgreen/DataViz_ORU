# Excercise 2 

#######################################################################
# SET UP FROM EXCERCISE 1
#######################################################################

## Installing necessary packages 
install.packages("ggplot2")  # For Data visualisation
install.packages("tidyverse") # For data Manipulationinstall.packages("miceadds")
install.packages("DescTools")
install.packages("confintr")
install.packages("mice")
install.packages("broom") # For tidying model outputs
install.packages("viridis") # For color palettes


## Loading necessary packages
library(ggplot2)
library(tidyverse)
library(miceadds)
library(DescTools)
library(confintr)
library(mice)
library(broom)
libary(viridis)


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
# Defining parameters 
x_var <- "maxhr" # Max Herat rate
y_var <- "chol" # Cholesterol
x_adj <- 100 # adjustment factor for x variable in MAR and MNAR mechanisms
y_adj <- 90 # adjustment factor for y variable in MNAR mechanism
mar_adj <- 0.1 # adjustment to approximate same mmissingness in MCAR
mnar_adj <- 0.15 # adjustment to approximate same mmissingness in MCAR
resp_prop <- 0.7 # proportion of non-missing data in y variable
m <- 150 # number of imputed datasets
seed <- 1234 # random number seed to allow replication

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
                           m = m, # 6 imputed datasets
                           maxit = 1, # one iteration is enough given few missing values
                           seed = seed) # setting seed for reproducibility
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
  ggplot(aes(x = maxhr, # x_var
             y = chol )) + # y_var 
  geom_point(color = "cornflowerblue", alpha = 0.6) + 
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # Adding regression line with confidence interval
  labs(
    title = "Max heart rate Vs Cholesterol",
    x = " Max heart rate (pm)",
    y = "Cholesterol (mg/dl)"
  ) +
  theme_minimal()

# the scatter plot shows a very weak negative linear relationship between max heart rate and cholesterol levels

# Setting a figure on the Correlation
cor(heart_disease_completed[, c("maxhr", 
                                "chol")])

# The correlation coefficient is ≈-0.0034 i.e. very close to zero, which indicates an extremely weak negative correlation


###############################################################################
# CREATING MISSING VALUES
#############################################################################

set.seed(seed)

heart_mcar <- heart_disease_completed
mcar <- runif(nrow(heart_disease_completed)) > resp_prop
heart_mcar[mcar,y_var] <- NA

#With MAR Mechanism
heart_mar <- heart_disease_completed
mar <- (heart_disease_completed[,x_var]/x_adj + runif(nrow(heart_disease_completed))) > 
  (resp_prop + mean(heart_disease_completed[,x_var])/x_adj + mar_adj)
heart_mar[mar,y_var] <- NA

### MNAR MECHANISM
heart_mnar <- heart_disease_completed

mnar <- (heart_disease_completed[,y_var]/y_adj - heart_disease_completed[,x_var]/x_adj + runif(nrow(heart_disease_completed))) > 
  (resp_prop + mean(heart_disease_completed[,y_var]/y_adj) - mean(heart_disease_completed[,x_var]/x_adj) + mnar_adj)

heart_mnar[mnar,y_var] <- NA


#MISSINGNESS IN y_var
miss <- c(
  sum(is.na(heart_disease_completed[,y_var]))
  ,sum(is.na(heart_mcar[,y_var]))
  ,sum(is.na(heart_mar[,y_var]))
  ,sum(is.na(heart_mnar[,y_var]))
)#
names(miss) <- c("Complete","MCAR","MAR","MNAR")
miss


#Bar plot visualising missing values
par(mfrow=c(1,1))

barplot(miss, ylim=c(0,nrow(heart_disease_completed)),col=1:4, 
        main="Number off missing values by mechanism")
legend("topleft",legend=miss, fill=1:4)


#What is the mean of y_var among observed?
mean_y_var <- c(
  mean(heart_disease_completed[,y_var])
  ,mean(heart_mcar[,y_var],na.rm=T)
  ,mean(heart_mar[,y_var],na.rm=T)
  ,mean(heart_mnar[,y_var],na.rm=T)
)#
names(mean_y_var) <- c("Complete","MCAR","MAR","MNAR")
mean_y_var

#Plot mean estimates as points (dot plot) using ggplot
ggplot(data.frame(y=mean_y_var, 
                  x=names(mean_y_var)), 
       aes(x = x,
           y = y)) +
  geom_point(size=3,
             colour = "cornflowerblue") +
  labs(x = "",
       y = "Mean of y estimates",
       title = "Mean of y estimates by MAR, MCAR and MNAR") +
  theme_minimal()

# The plot above shows a relatively stable mean cholesterol levels in the missing
# data mechanisms as compared to the complete data suggesting that the mechanisms 
# have not significantly biased the mean estimate of cholesterol in this case.
# Also, i have made changes to the plot in terms of aesthetics to make it more visually appealing
# as well as axis labels and title for better understanding of the plot.


#REGRESSION of y_var on x_var 

#####################################ORIGINAL CODE #####################################
regr_yx <- list(
  complete = summary(lm(data=heart_disease_completed, as.formula(paste0(y_var, "~", x_var))))$coeff[,1]
  ,mcar = summary(lm(data=heart_mcar, as.formula(paste0(y_var, "~", x_var))))$coeff
  ,mar = summary(lm(data=heart_mar, as.formula(paste0(y_var, "~", x_var))))$coeff
  ,mnar = summary(lm(data=heart_mnar, as.formula(paste0(y_var, "~", x_var))))$coeff
)#
regr_yx

#Show with ggplot2
ggplot(data.frame(x=heart_disease_completed[,x_var],y=heart_disease_completed[,y_var]), aes(x=x, y=y)) +
  geom_point() + 
  geom_abline(intercept = regr_yx$complete[1], slope = regr_yx$complete[2]) +
  geom_abline(intercept = regr_yx$mcar[1], slope = regr_yx$mcar[2], colour = 2) +
  geom_abline(intercept = regr_yx$mar[1], slope = regr_yx$mar[2], colour = 3) +
  geom_abline(intercept = regr_yx$mnar[1], slope = regr_yx$mnar[2], colour = 4) +
  theme_minimal()
##########################################################################################

############################REVISED CODE & VIZ #####################################

# Regression of y_var on x_var across different datasets
models <- list(complete = lm(as.formula(paste0(y_var, "~", x_var)), data = heart_disease_completed),
              mcar = lm(as.formula(paste0(y_var, "~", x_var)), data = heart_mcar),
              mar = lm(as.formula(paste0(y_var, "~", x_var)), data = heart_mar),
               mnar = lm(as.formula(paste0(y_var, "~", x_var)), data = heart_mnar)
)

# Extracting coefficients into a tidy dataframe
regr_df <- do.call(rbind, 
                   lapply(names(models),
                          function(name) {
  coef <- coef(models[[name]])
  data.frame(
    dataset = name,
    intercept = coef[1],
    slope = coef[2]
  )
}))

# ggplot visualisation using viridis color palette
ggplot(heart_disease_completed,
       aes_string(x = x_var,
                  y = y_var)) +
  geom_point(alpha = 0.8, color = "cornflowerblue") +
  geom_abline(data = regr_df,
              aes(intercept = intercept, 
                  slope = slope, 
                  color = dataset),
              size = 1.2) +
  scale_color_viridis_d(option = "A", 
                               begin = 0.1, 
                               end = 0.9) +
  labs( title = " Regression of Cholesterol on Max heart rate",
    x = "Max heart rate (pm)",
    y = "Cholesterol (mg/dl)",
  ) +
  theme_minimal() +
  theme(legend.position = "top",
    plot.title = element_text(face = "bold")
  )

##########################################################################################

# The revised code and visualization improves upon the original by:
# 1. Using a consistent color palette (viridis) for better aesthetics and accessibility.
# 2. Adding titles and axis labels for clarity.
# 3. Using a legend to identify regression lines from different datasets.
# 4. Enhancing point visibility with adjusted alpha and color.

#################CORRELATION##########################################
# Comparing the Correlation of y_var with x_var across different datasets

df <- list(complete = heart_disease_completed,
           mcar = heart_mcar,
           mar  = heart_mar,
           mnar = heart_mnar
)

# Compute correlations and store in a dataframe
cor_df <- do.call(rbind, 
                  lapply(names(df),
                         function(name) {
  data <- df[[name]]
  cor_val <- cor(data[[x_var]], data[[y_var]], use = "complete.obs")
  data.frame(dataset = name, correlation = cor_val)
}))

cor_df

# Visualising correlations using ggplot2 with viridis color palette
ggplot(cor_df, 
       aes(x = dataset, 
           y = correlation, 
           fill = dataset)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(correlation, 2)), 
            vjust = -0.5, 
            size = 5) +
  scale_fill_viridis_d(option = "D", 
                       begin = 0.1, end = 0.9, 
                       name = "Dataset") +
  labs( title = "Correlation between Cholesterol and Max heart rate",
    x = "",
    y = "Correlation coefficient"
  ) +
  ylim(-1, 1) +  # Since correlations always lie on [-1,1]
  theme_minimal() +
  theme(legend.position = "none",
    plot.title = element_text(face = "bold")
  )
##########################################################################################












