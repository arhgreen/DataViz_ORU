
# EXERCISE 2
###################################################################
#II.7 Exercise
###################################################################
#This exercise is similar to the previous, you post your code and help others.
#From the analysis that follows (below), you should try to improve the final
#graph, and then another graph at your own choice. Comment
#in the code on why you chose to alter the graph the way you did. You can
#perhaps find good ways to alter graphs in ch12 of An Introduction to R.
#As part of the code you post in blackboard, you have to decide on how to
#alter the parameters below.
#We will look at an example of missing data analysis which cover several
#plotting opportunities. However, the plots may be improved upon, that will be
#part of your exercise. We will consider the three missingness mechanisms
#presented in vanBuuren. First you should undertake the analysis, but use two
#other variables (x_var and y_var) than the default choice ("Age" and "RestBP").
#You can find the following quantitative variables in the heart dataset:
summary(heart_disease[,c("age","restbp","chol","maxhr","oldpeak","ca", "thal")])
#Notice that "Ca" has 4 missing values (and Thal has 2). This would be a
#problem for parts of the code. So if you use Ca, first run this code as to
#replace the present missing values with some other value.

#Why not assume MAR and use stochastic regression imputation (1.3.5 in vanBuuren)?
library(mice)
heart_imp <- mice(heart, m = 1, maxit = 1, seed = 1, print = FALSE)
#These are the values that was drawn to be imputed this time.
heart_imp$imp
#By changing the seed argument, you will have a different draw.
#By increasing the m argument, you may draw several imputed values at the same time.
#To replace the original dataset with the (first) imputed one, you may write:
heart <- complete(heart_imp,1)
#From here on you find all the code that you need for this exercise:
###################################################################
#The following parameters has to be decided on:
###################################################################
"Age" -> x_var # Pick one from: "Age","RestBP","Chol","MaxHR","Oldpeak", "Ca"
"RestBP" -> y_var # Pick another one from: "Age","RestBP","Chol","MaxHR","Oldpeak","Ca"
# make a basic scatterplot of x_var and y_var to investigate the relationship
par(mfrow=c(1,1))
plot(heart[,c(x_var,y_var)])
# what is the correlation between x_var and y_var?
cor(heart[,c(x_var,y_var)])
40 -> x_adj # divide x-variable by this factor in mar and mnar mechanism
20 -> y_adj # divide y-variable by this factor in mnar mechanism
-0.044 -> mar_adj # adjustment to get same proportion missing as MCAR mechanism
0.109 -> mnar_adj # adjustment to get same proportion missing as MCAR mechanism
.7 -> resp_prop # approximate proportion of non-missing data in y-variable
100 -> m # number of imputed datasets (at least 5, more improves approximation but takes
more time)
2022 -> seed # random number seed to allow replication, should just be any number
###################################################################


