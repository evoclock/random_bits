library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(pwr)
library(plyr)
library(purrr)
library(caTools)
library(e1071)
library(glmnet)

# From this:

merge_1<-merge(variations_df,visits_df,by.x="user_id",by.y="user_id")  
merge_2<-merge(merge_1,test_conv_df,by.x="user_id",by.y="user_id",all.x=TRUE)  
merge_3<-merge(merge_2,eng_df,by.x="user_id",by.y="user_id",all.x=TRUE)
merge_3$converted<-if_else(is.na(merge_3$converted),0,1)  
merge_3$clicked_on_email<-if_else(is.na(merge_3$clicked_on_email),0,1)  
merge_3$converted<-as.factor(merge_3$converted)  
merge_3$clicked_on_email<-as.factor(merge_3$clicked_on_email)
merge_3$timeofday<-  mapvalues(hour(merge_3$visit_time),from=c(0:23),  
                               to=c(rep("night",times=5), rep("morning",times=6),rep("afternoon",times=5),rep("night", times=8)))  
merge_3$timeofday<-as.factor(merge_3$timeofday)


# To this:
# Assuming variations_df, visits_df, test_conv_df, and eng_df are defined

# List of data frames to merge
dfs_to_merge = list(variations_df, visits_df, test_conv_df, eng_df)

# Define the 'by' argument for each merge
by_cols = rep("user_id", length(dfs_to_merge))

# Merge data frames using Reduce function
merged_df = Reduce(function(df1, df2) merge(df1, df2, 
                                            by = "user_id", 
                                            all.x = TRUE), dfs_to_merge)

# Fill NA values with 0 and convert to factors
merged_df$converted = as.factor(ifelse(is.na(merged_df$converted), 0, 1))
merged_df$clicked_on_email = as.factor(ifelse(is.na(merged_df$clicked_on_email), 0, 1))

# Map hour values to time of day
time_of_day = function(x) {
  c("night", "morning", "afternoon", "night")[findInterval(x, c(0, 6, 12, 18, 24))]
}
merged_df$timeofday = as.factor(time_of_day(hour(merged_df$visit_time)))

# Result: merged_df contains the final merged and processed data frame

# or a tidyverse version

# Assuming variations_df, visits_df, test_conv_df, and eng_df are defined

# Merge data frames
merged_df = list(variations_df, visits_df, test_conv_df, eng_df) %>%
  reduce(left_join, by = "user_id")

# Fill NA values with 0 and convert to factors
merged_df = merged_df %>%
  mutate(
    converted = factor(if_else(is.na(converted), 0, 1)),
    clicked_on_email = factor(if_else(is.na(clicked_on_email), 0, 1))
  )

# Map hour values to time of day
time_of_day = function(x) {
  c("night", "morning", "afternoon", "night")[findInterval(x, c(0, 6, 12, 18, 24))]
}
merged_df = merged_df %>%
  mutate(timeofday = factor(time_of_day(hour(visit_time))))

# Result: merged_df contains the final merged and processed data frame


library(dplyr)

# Assuming variations_df, visits_df, test_conv_df, and eng_df are defined

# Merge data frames
merged_df <- list(variations_df, visits_df, test_conv_df, eng_df) %>%
  reduce(function(df1, df2) left_join(df1, df2, by = "user_id"))

# Fill NA values with 0 and convert to factors
merged_df <- merged_df %>%
  mutate(
    converted = factor(if_else(is.na(converted), 0, 1)),
    clicked_on_email = factor(if_else(is.na(clicked_on_email), 0, 1))
  )

# Map hour values to time of day
time_of_day <- function(x) {
  c("night", "morning", "afternoon", "night")[findInterval(x, c(0, 6, 12, 18, 24))]
}
merged_df <- merged_df %>%
  mutate(timeofday = factor(time_of_day(hour(visit_time))))

# Result: merged_df contains the final merged and processed data frame


# Statistical testing
# To test whether the difference in proportions is statistically significant, 
# we can either carry out a difference in proportions test or a chi-squared 
# test of independence where the null hypothesis is that there is no 
# association between whether or not a user converted and the type of variation 
# they visited.

# For both tests, a p-value < 0.05 was observed indicating a statistically 
# significant difference in proportions.

# I went a step further and ran logistic regression to understand how the other 
# attributes of the users contributed to the difference in proportions. 
# Only the type of variation and income (p-values less than 0.05) appeared to 
# contribute to the difference in conversion proportions. A calculation of 
# McFadden’s R-squared tells us that only 12.94% of the variation in 
# proportions can be explained by the variation type and user attributes 
# provided within our dataset. 

# Data Preparation
mydatanew = final.filtered %>%
  select(-c(1, 3, 8))  # Drop unnecessary columns

# Randomly split data into training and test sets
split = sample.split(mydatanew$clicked_on_email, SplitRatio = 0.70) 
train = mydatanew[split == TRUE, ]
test = mydatanew[split == FALSE, ]

# #### 4. Exploratory data analysis to understand users ####
# #Income ranges from 14,000 to $1511958 - too much variation to see a trend

l = ggplot(merge_3, aes(variation,fill = converted))
l = l + geom_histogram(stat="count")
print(l)
print("Proportion that converted by variation converted")
tapply(as.numeric(merge_3$converted) - 1 ,merge_3$variation,mean)
# #slightly greater proportion of those that viewed treatment variation converted
# 
# 
# ### 5.1 A/B testing - visits, variations and test converions
# # Null Hypothesis: Assumption that there is no difference between the 
# conversion rates for control & exp
# #Alternative Hypothesis: There is a difference between the conversion rates 
# for control & exp
# control_sz<-length(which(merge_3$variation=="control"))
# exp_sz<-length(which(merge_3$variation=="treatment"))
# control_yes<-length(which(merge_3$variation=="control" & merge_3$converted=="1"))
# exp_yes<-length(which(merge_3$variation=="treatment" & merge_3$converted=="1"))
# prop.test(c (control_yes, exp_yes), c (control_sz, exp_sz))
# #p-value<0.05 - so yes there is a difference in conversion rates. 
# Slightly higher for treatment
# 
# A/B Testing
control_sz = sum(mydatanew$variation == "control")
exp_sz = sum(mydatanew$variation == "treatment")
control_yes = sum(mydatanew$variation == "control" & mydatanew$converted == 1)
exp_yes = sum(mydatanew$variation == "treatment" & mydatanew$converted == 1)

prop_test_result = prop.test(c(control_yes, exp_yes), c(control_sz, exp_sz))

# #chi-sq test for conversion and variation
# #Null hypothesis: There is no association between conversion and variation
# 
# ch_test<-chisq.test(merge_3$variation,merge_3$converted) #p-value < 0.05, 

ch_test = chisq.test(mydatanew$variation, mydatanew$converted)
# There may be an association between likelihood of conversion and variation
ch_test$stdres
# #Pearson's std residuals measure
# #  how large is the deviation from each cell to the null hypothesis ? in this 
# case, independence between row and column's treatment is negatively 
# associated with non-conversion, so it has a smaller proportion of people who 
# did not convert
# 
# 
# # Test which variables are key in determining conversion via logistic 
# regression
# ## 5.2 Further analysis - logistic regression ##
# glm_model<-glm(converted~variation+channel+age+gender+timeofday+income,
# data=merge_3,family = binomial(link="logit"))
# summary(glm_model)
# only variation treatment is statistically signficant
# positive coefficient indicates that all other factors remaining fixed, 
# if person has viewed treatment variation, they are more likely to convert
# viewing treatment rather than control increases odds by 0.0272 for conversion
# 
# anova(glm_model, test="Chisq")
# #Finding: Only adding channel and income is statistically significant in 
# reducing the residual deviance.
# How our model does against model without intercept
# 
# Logistic Regression
glm_model = glm(clicked_on_email ~ ., data = train, family = binomial)
glm_anova = anova(glm_model, test = "Chisq")
pR2_value = pR2(glm_model)
# #assess model fit
library(pscl)
pR2(glm_model) #McFadden R2 index can be used to assess the model fit.
# only 12.94$ of variation in whether or not someone will convert is likely to 
# be explained by current model
#
# #### 5. Exploratory learning for engagemnt with emails  ####
# 
# #Exploratory data analysis to understand relationship between attributes and engagement with emails
# 
# l <- ggplot(merge_3, aes(gender,fill = clicked_on_email))
# l <- l + geom_histogram(stat="count")
# print(l)
# print("Greater proportion of males clicked on email")
# tapply(as.numeric(merge_3$clicked_on_email) - 1 ,merge_3$gender,mean)
# 
# l <- ggplot(merge_3, aes(timeofday,fill = clicked_on_email))
# l <- l + geom_histogram(stat="count")
# print(l)
# print("Greatest proportion of people that visited website visited in the nighttime. Similar proportions of those that clicked on email by time of day")
# tapply(as.numeric(merge_3$clicked_on_email) - 1 ,merge_3$timeofday,mean)
# 
# 
# l <- ggplot(merge_3, aes(channel,fill = clicked_on_email))
# l <- l + geom_histogram(stat="count")
# print(l)
# print("Those that had PPC as their channel had the highest proportion of clicking on email (73%).
#       However, highest number of users were prompted to visit the website due to TV")
# tapply(as.numeric(merge_3$clicked_on_email) - 1 ,merge_3$channel,mean)
# 
# 
# #Age has 1,243 missing values
# row.has.na <- apply(merge_3, 1, function(x){any(is.na(x))})
# sum(row.has.na)
# final.filtered <- merge_3[!row.has.na,]
# 
# ggplot(final.filtered,aes(x=clicked_on_email,y=age))+geom_boxplot()
# print("On average, those that converted were on average 38 years old, while those that didn't were around 32 years of age")
# 
# dplyr::group_by(final.filtered, clicked_on_email) %>%
#   dplyr::summarise(mean=mean(age), sd=sd(age))
# 
# 
# ggplot(final.filtered,aes(x=clicked_on_email,y=income))+geom_boxplot()
# dplyr::group_by(final.filtered, clicked_on_email) %>%
#   dplyr::summarise(mean=mean(income), sd=sd(income))
# print("On average, those that converted on average earned more than those that didn't. High variation in income, with a lot of very high income values in both groups")
# 
# 
# #### 6. Machine learning model for engagement with emails ####
# library(caTools)
# library(e1071)
# library(glmnet)
# 
# ## drop columns not required
# mydatanew = final.filtered[,-c(1,3,8)]
# #randomly split data into training, test and validation
# #Splitting data
# split <- sample.split(mydatanew$clicked_on_email, SplitRatio = 0.70) 
# train <- subset(mydatanew, split == T) #ensure same proportion of clicks and non-clicks in both training and test sets to ensure balance
# test <- subset(mydatanew, split == F)
# 
# 
# ## Testing model accuracy on test set
# model_glm <- glm(clicked_on_email ~ ., data = train, family='binomial') 
# predicted_glm <- predict(model_glm, test, type='response')
# predicted_glm <- ifelse(predicted_glm > 0.5,1,0)
# misClasificError <- mean(predicted_glm != test$clicked_on_email)
# print(paste('Accuracy',1-misClasificError))
# 
# Model Accuracy Evaluation
predicted_glm = predict(glm_model, test, type = 'response')
predicted_glm = as.integer(predicted_glm > 0.5)
misClasificError = mean(predicted_glm != test$clicked_on_email)

# ROC curve and AUC
pr = prediction(predicted_glm, test$clicked_on_email)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
auc = performance(pr, measure = "auc")@y.values[[1]]

# Print Results
cat("A/B Testing Results:\n")
print(prop_test_result)
print(ch_test$stdres)

cat("\nLogistic Regression Model Summary:\n")
summary(glm_model)

cat("\nModel Accuracy:\n")
cat("Accuracy: ", 1 - misClasificError, "\n")

cat("\nArea Under the Curve (AUC):\n")
cat("AUC: ", auc, "\n")

# 
# p<-predicted_glm
# pr <- prediction(p, test$clicked_on_email)
# prf <- performance(pr, measure = "tpr", x.measure = "fpr")
# plot(prf)
# 
# auc <- performance(pr, measure = "auc")
# auc <- auc@y.values[[1]]
# auc
# 
# 
# summary(model_glm)
# print("Statistically signficant predicturs are channel, age, and gender where 
# males are more likely to click on email and those that come via Facebook, 
# followed by PPC and then TV. Also, an older user is more likely to click on an 
# email than a younger user")
# 
# ##Response to Q1: There is a statistically significant difference in those that 
# # converted based on variation; however, the difference in proportion is not 
# # very large. I would recommend re-running the test with a larger dataset to 
# # ensure reproducibility. 
# 
# ##Response to Q2: The model can predict whether or not someone will click on an 
# # email with 84% accuracy. Top predictors are gender, age, and channel. 


