########################################################
### Case: 2008 Democratic Primaries - Clinton vs. Obama
### Team 18: Yuhe (Tiffany) Jin, Yinan Chen, Tirth Pravin Gala, Chenjie (Angelina) Sun, Caryl Alexis Cohen
########################################################
source("DataAnalyticsFunctions.R")
# read data into R
election_data <- read.csv("ElectionDataAlone.csv")

# Next use the function summary to inspect the data
summary(election_data)

##############################################
# Cleaning up the data
# Write a function that replaces NAs with the mean of the non-missing data 
# in the column. This function can be called for different data sets to 
# impute the data.
impute_data <- function(vec, mn) {
  ifelse(is.na(vec), mn, vec)
}
# Find the means for all the numeric columns. 
# The function sapply automatically runs the mean function 
# (specified as second argument) on the columns 10 through 41. 
# The means are then saved in the vector named train_data_mean. 
# We use the argument na.rm=TRUE to ask the function to ignore NA entries.
data_mean <- sapply(election_data[,10:41],mean, na.rm=TRUE)

# Run this command to look at means for all the columns we found by running the sapply function
(data_mean)

# Impute the missing data. Loop through all the rows and 
# for each column call the function impute_train_data.
for(i in 10:41) {
  election_data[,i]<-impute_data(election_data[,i],data_mean[i-9])
}
# Run the summary function again. Now you see that no demographic/county columns have NA entries.
summary(election_data)

# Create two separate data sets from the data in electionData.
election_data$ElectionDate <- as.Date(election_data$ElectionDate, format="%m/%d/%Y")
election_data_train <- election_data[election_data$ElectionDate < as.Date("2/19/2008", format="%m/%d/%Y"), ]
election_data_test <- election_data[election_data$ElectionDate >= as.Date("2/19/2008", format="%m/%d/%Y"), ]

# If you want to write these data sets back out into spreadsheets, 
# use the following "write" commands in R.
# write.csv(electionDataTrain, "electionDataTrain.csv")
# write.csv(electionDataTest, "electionDataTest.csv")

##########################################################
### End of Data Cleaning up
##########################################################
#
# Create some possible variables that might be of interest.
# (things we might like to predict in a regression using the demographic information). 
# These variables directly become a part of our data set election_data_train. You can use the command names(election_data_train) to see that these are added as columns in our data set.
election_data_train$Obama_margin <- election_data_train$Obama - election_data_train$Clinton
election_data_train$Obama_margin_percent <- 100*election_data_train$Obama_margin/election_data_train$TotalVote
election_data_train$Obama_wins <- ifelse(election_data_train$Obama_margin >0, 1,0)
names(election_data_train)
###
### Based on the data, to account for the size of possible delegates on each county
### we will work with election_data_train$Obama_margin_percent to be the target out models.
###


### Question 1: Provide a visualization (very flexible format, 
### it does not need to be related to the election)
library(ggplot2)
ggplot(election_data, aes(x=HighSchool, y=UnemployRate))+
  geom_point(color = "#D16103")+
  geom_smooth(formula = y ~ x, method="lm", se=FALSE, color="#293352")+
  labs(
    x = "High school graduate or higher (percent)",
    y ="Unemployment rate in 2006",
    title = "Relationship between education and umemployment rate")


### Question 2: Prediction. No additional script beyond the cleaning up the data
### provided above. (Hint: FIPS variable bahaves as an identifier for each observation
### so you might not want to include it in a linear regression.)
library(caret)
library(randomForest) 
library(glmnet)
library(rpart)

election_data_train <- subset(election_data_train, select = -c(County, State, FIPS,TotalVote, Clinton, Obama, Obama_margin, Obama_wins))
election_data_test <- subset(election_data_test, select = -c(County, State, FIPS,TotalVote, Clinton, Obama))

###1 Models
###########
# Linear Model
linear_model <- lm(Obama_margin_percent ~ ., election_data_train)
#Random Forest Model
rf_model <- randomForest(Obama_margin_percent ~ ., election_data_train)
#CART Model
cart_model <- rpart(Obama_margin_percent ~ ., election_data_train)

###2 Metrics:RMSE
############
library(Metrics)
linear_predictions <- predict(linear_model, election_data_train)
rf_predictions <- predict(rf_model, election_data_train)
cart_predictions <- predict(cart_model, newx = election_data_train)

linear_rmse <- rmse(linear_predictions, election_data_train$Obama_margin_percent)
rf_rmse <- rmse(rf_predictions, election_data_train$Obama_margin_percent)
cart_rmse <- rmse(cart_predictions, election_data_train$Obama_margin_percent)

rmse_all <- data.frame(Model = c("Linear", "Random Forest", "CART"), RMSE = c(linear_rmse, rf_rmse, cart_rmse))
print(rmse_all)

###Random Forest Model As Final Model

###3 Cross-validation
#####################
ctrl <- trainControl(method="cv", number=5)

#Random Forest Model
rf_cv <- train(Obama_margin_percent ~ ., 
               election_data_train, 
               method = "rf", 
               trControl = ctrl)
print(rf_cv)

# Linear Model
linear_cv <- train(Obama_margin_percent ~ ., 
                   election_data_train, 
                   method = "lm", 
                   trControl = ctrl)
print(linear_cv)


#CART Model
cart_cv <- train(Obama_margin_percent ~ ., 
                 election_data_train, 
                 method = "rpart", 
                 trControl = ctrl)
print(cart_cv)

###4 Prediction
################
rf_model <- randomForest(Obama_margin_percent ~ ., election_data_train)
final_model <- predict(rf_model, election_data_test)
print(final_model)


###
### Question 3: Keep in mind that unsupervised learning 
### is used to explore the data. Feel free to consider only a subset of the 
### demographic variables. 
###

vars <- c( "AgeBelow35", "Age35to65", "Age65andAbove", 
           "White", "Black","Asian","AmericanIndian","Hawaiian","Hispanic",
           "HighSchool","Bachelors",
           "Poverty","IncomeAbove75K", "MedianIncome", 
           "MedicareRate", "UnemployRate", "SocialSecurityRate","DisabilitiesRate","PopDensity")
clustered <- election_data[ ,vars]
clustered <- na.omit(clustered)
scaled_clustered <- scale(clustered)

set.seed(123)
k_means <- kmeans(scaled_clustered, centers = 4)  
election_data$Clustered <- factor(k_means$cluster)

ggplot(election_data, aes(x = MedianIncome, y = MedicareRate, color = factor(k_means$cluster))) +
  geom_point(size=2, alpha=0.8) +
  labs(
    x = "Median household income in 2005",
    y ="Medicare program enrollment in 2005 (Rate per 100k persons)",
    title = "K-Means Clustering",
    color = "Cluster") +
  theme_minimal()

library(dplyr)
cluster_data <- election_data %>%
  group_by(k_means$cluster) %>%
  summarize(
    Avg_MedianIncome = mean(MedianIncome, na.rm = TRUE),
    Avg_MedicareRate = mean(MedicareRate, na.rm = TRUE)
  )
print(cluster_data)

###
### Question 4. First part: impact of changing hispanic demographic
###
#### 
HispanicSimple <- glm(Obama_margin_percent ~ Hispanic, data = election_data_train )
summary(HispanicSimple)

####
### Question 4. First part: impact of changing black demographic
####
#### 
BlackSimple <- glm(Obama_margin_percent ~ Black, data = election_data_train )
summary(BlackSimple)
####

####
### Question 4. Second part
####
#### Model with 1771 controls to measure the impact of 1% larger Hispanic demographic
y <- election_data_train$Obama_margin_percent
x <- model.matrix( Obama_margin_percent ~ .-Hispanic-ElectionDate, data = election_data_train )
d <- election_data_train$Hispanic

#### Model with 1771 controls to measure the impact of 1% larger Black demographic
ya <- election_data_train$Obama_margin_percent
xa <- model.matrix( Obama_margin_percent ~ .-Black-ElectionDate, data = election_data_train )
da <- election_data_train$Black
####
BlackModel <- lm(ya ~ xa + da)
summary(BlackModel)

### xCountyCharles              -8.738e+01  1.822e+01  -4.795 2.11e-06 *
### xCountyBaltimore city       -7.619e+01  1.885e+01  -4.042 6.07e-05 *
### xCountyAroostook            -6.357e+01  1.890e+01  -3.364 0.000823 *


####
#### Question 5: No additional R code. Keep in mind that you can build upon your previous 
#### analysis or develop new analysis.
####

