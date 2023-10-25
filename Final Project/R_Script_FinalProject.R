##Data Science for Business Final Project
##Team 18
library(readr)
advertising <- read_csv("advertising.csv")

#Data Preparation
colnames(advertising)[colnames(advertising) == "Male"] <- "Gender"
colnames(advertising)[colnames(advertising) == "Area Income"] <- "Area_Income"
colnames(advertising)[colnames(advertising) == "Daily Time Spent on Site"] <- "Time_Spend"

#EDA
library(tidyverse)

#Clicked on Ad by Daily Time Spent on Site
advertising$'Clicked on Ad' <- factor(advertising$'Clicked on Ad', labels = c("No", "Yes"))
boxplot(advertising$'Daily Time Spent on Site' ~ advertising$'Clicked on Ad',
        horizontal = FALSE,
        ylab = "Daily Time Spent on Site",
        xlab = "Clicked on Ad",
        main = "Clicked on Ad by Daily Time Spent on Site")

#Clicked on Ad by Age
advertising$'Clicked on Ad' <- factor(advertising$'Clicked on Ad', labels = c("No", "Yes"))
boxplot(advertising$'Age' ~ advertising$'Clicked on Ad',
        horizontal = FALSE,
        ylab = "Age",
        xlab = "Clicked on Ad",
        main = "Clicked on Ad by Age")

#Clicked on Ad by Area Income
advertising$'Clicked on Ad' <- factor(advertising$'Clicked on Ad', labels = c("No", "Yes"))
boxplot(advertising$'Area Income' ~ advertising$'Clicked on Ad',
        horizontal = FALSE,
        ylab = "Area Income",
        xlab = "Clicked on Ad",
        main = "Clicked on Ad by Area Income Level")

#Clicked on Ad by Gender
advertising$'Clicked on Ad' <- factor(advertising$'Clicked on Ad', labels = c("No", "Yes"))
advertising$'Gender' <- factor(advertising$'Gender', labels = c("Female", "Male"))

tab <- table(advertising$'Gender', advertising$'Clicked on Ad')

barplot(tab, beside = TRUE, col = c("skyblue", "salmon"),
        ylim = c(0, max(tab) + 50),  
        legend = rownames(tab),
        main = "Clicked on Ad by Gender",
        xlab = "Clicked on Ad",
        ylab = "Count")


#Daily time spent on site grouped
group <- as.factor(cut(advertising$'Daily Time Spent on Site',
                       breaks = c(30, 50, 68, 80, 92),
                       labels = c("30~50", "50~68", "68~80", "80~92"),
                       include.lowest = TRUE))

my_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")

ggplot(advertising, aes(x = `Clicked on Ad`, y = `Daily Time Spent on Site`, color = group)) +
  geom_point(shape = 16, size = 2) +
  xlab("Clicked on Ad") +
  ylab("Daily Time Spent on Site") +
  ggtitle("Clicked on Ad by Grouped Daily Time Spent on Site") +
  scale_color_manual(values = my_colors, name = "Groups")

#Data Mining
#Library
library(caret)
library(glmnet)
library(rpart)
library(class)

#Split into training and testing
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(advertising), replace=TRUE, prob=c(0.7,0.3))
ads_train  <- advertising[sample, ]
ads_test   <- advertising[!sample, ]

#Model
#Logistic RegressionC
log_model <- glm(`Clicked on Ad` ~ Time_Spend+Age+Gender+Area_Income+Timestamp, ads_train, family = "binomial")
log_model
#Decision Tree
tree_model <- rpart(`Clicked on Ad` ~ Time_Spend+Age+Gender+Area_Income+Timestamp, ads_train, method = "class")
tree_model
#CART 
cart_model <- rpart(`Clicked on Ad` ~ Time_Spend+Age+Gender+Area_Income+Timestamp, ads_train)
cart_model
#LASSO 
response_variable <- as.numeric(ads_train$`Clicked on Ad`)
model_matrix <- model.matrix(`Clicked on Ad` ~ Time_Spend+Age+Area_Income+Gender+Timestamp, ads_train)
lasso_model <- glmnet(model_matrix, y=response_variable, alpha=1)
lasso_model

#Prediction
log_prediction1 <- predict(log_model, newdata=ads_train)
log_prediction2 <- predict(log_model, newdata=ads_test)

tree_prediction1 <- predict(tree_model, newdata=ads_train)
tree_prediction2 <- predict(tree_model, newdata=ads_test)

cart_prediction1 <- predict(cart_model, newdata=ads_train)
cart_prediction2 <- predict(cart_model, newdata=ads_test)

model_matrix_train <- model.matrix(`Clicked on Ad` ~ Time_Spend+Age+Area_Income+Gender+Timestamp, ads_train)
model_matrix_test <- model.matrix(`Clicked on Ad` ~ Time_Spend+Age+Area_Income+Gender+Timestamp, ads_test)
lasso_prediction1 <- predict(lasso_model, s=lasso_model$lambda.min, newx=model_matrix_train)
lasso_prediction2 <- predict(lasso_model, s=lasso_model$lambda.min, newx=model_matrix_test)

#5-Fold Cross Validation
ctrl <- trainControl(method="cv", number=5)

#In-sample
ads_train$`Clicked on Ad` <- factor(ads_train$`Clicked on Ad`, levels = c(0, 1))
log_cv_1 <- train(`Clicked on Ad` ~ Time_Spend+Age+Area_Income+Gender+Timestamp, 
                ads_train, 
                method = "glm", 
                family = "binomial", 
                trControl = ctrl)

tree_cv_1 <- train(`Clicked on Ad` ~ Time_Spend+Age+Area_Income+Gender+Timestamp, 
                 ads_train, 
                 method = "rpart", 
                 trControl = ctrl, 
                 metric = "Accuracy")

cart_cv_1 <- train(`Clicked on Ad` ~ Time_Spend+Age+Area_Income+Gender+Timestamp, 
                 ads_train, 
                 method = "rpart", 
                 trControl = ctrl, 
                 metric = "Accuracy")

lasso_cv_1 <- train(`Clicked on Ad` ~ Time_Spend+Age+Area_Income+Gender+Timestamp,
                  ads_train, 
                  method = "glmnet", 
                  trControl = ctrl, 
                  metric = "Accuracy")

log_accuracy_1 <- mean(log_cv_1$results$Accuracy)
tree_accuracy_1 <- mean(tree_cv_1$results$Accuracy)
cart_accuracy_1 <- mean(cart_cv_1$results$Accuracy)
lasso_accuracy_1 <- mean(lasso_cv_1$results$Accuracy)

is_accuracy <- data.frame(
  Model = c("Logistic", "Decision Tree", "CART", "LASSO"),
  Accuracy = c(log_accuracy_1, tree_accuracy_1, cart_accuracy_1, lasso_accuracy_1))
is_accuracy


#Out-of-sample
ads_test$`Clicked on Ad` <- factor(ads_test$`Clicked on Ad`, levels = c(0, 1))
log_cv_2 <- train(`Clicked on Ad` ~ Time_Spend+Age+Area_Income+Gender+Timestamp, 
                ads_test, 
                method = "glm", 
                family = "binomial", 
                trControl = ctrl)

tree_cv_2 <- train(`Clicked on Ad` ~ Time_Spend+Age+Area_Income+Gender+Timestamp, 
                   ads_test, 
                   method = "rpart", 
                   trControl = ctrl, 
                   metric = "Accuracy")

cart_cv_2 <- train(`Clicked on Ad` ~ Time_Spend+Age+Area_Income+Gender+Timestamp, 
                   ads_test, 
                   method = "rpart", 
                   trControl = ctrl, 
                   metric = "Accuracy")

lasso_cv_2 <- train(`Clicked on Ad` ~ Time_Spend+Age+Area_Income+Gender+Timestamp,
                    ads_test, 
                    method = "glmnet", 
                    trControl = ctrl, 
                    metric = "Accuracy")

log_accuracy_2 <- mean(log_cv_2$results$Accuracy)
tree_accuracy_2 <- mean(tree_cv_2$results$Accuracy)
cart_accuracy_2 <- mean(cart_cv_2$results$Accuracy)
lasso_accuracy_2 <- mean(lasso_cv_2$results$Accuracy)

oos_accuracy <- data.frame(
  Model = c("Logistic", "Decision Tree", "CART", "LASSO"),
  Accuracy = c(log_accuracy_2, tree_accuracy_2, cart_accuracy_2, lasso_accuracy_2))
oos_accuracy

#Prediction with Logistic
log_model <- glm(`Clicked on Ad` ~ Time_Spend+Age+Gender+Area_Income+Timestamp, ads_train, family = "binomial")
final_log_prediction <- predict(log_model, ads_test, type = "response")
final_log_prediction

#Evaluation of Logistic
library(pROC)

actual_responses <- ads_test$`Clicked on Ad`
predicted_response <- final_log_prediction

roc_obj <- roc(actual_responses, predicted_response)
auc_value <- auc(roc_obj)
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"))


