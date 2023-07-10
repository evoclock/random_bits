#################################################################
#                                                               #
# Small reminder script for generating more informative         #
# diagnostic plots for a given statistical test (have not       #
# tested on Bayesian stats)                                     #
# J. Gamboa                                                     #
# j.a.r.gamboa@gmail.com | julen.a.gamboa@gmail.com             #
#################################################################

library(performance)
library(see)

dat = mtcars
str(dat)

# fit a glm to the mtcars dataset, specifically, mpg given wt and disp
mt_cars_model = glm(mpg ~ wt + disp, data=dat, family = Gamma())

# Generate diagnostic plots
check_model(mt_cars_model)

# check for outliers in the data
check_outliers(mt_cars_model)

# check whether residuals for our model are normally distributed (if applicable)
check_normality(mt_cars_model)

