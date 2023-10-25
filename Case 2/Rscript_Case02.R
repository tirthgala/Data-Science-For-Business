# Undercutting ALLSTATE online Case

# Load additional file to install packages
source("DataAnalyticsFunctions.R")

# Load ALLSTATE DATA FILE
ALLcost <- read.csv("ALLSTATEcost.csv")

# Data Preparation
summary(ALLcost)
ALLcost[1:6,]
drop <- c("customer_ID","shopping_pt","record_type","time","location")
DATA <- ALLcost[,!(names(ALLcost) %in% drop)]
DATA$car_value <-  factor(DATA$car_value)
DATA$day <-  factor(DATA$day)
DATA$state <-  factor(DATA$state)
duration_NA <-  ifelse( is.na(DATA$duration_previous) , 1, 0 ) 
sum(duration_NA)
sum(duration_NA)/length(duration_NA)
DATA$duration_previous[duration_NA>0] <-0 
C_NA <-  ifelse( is.na(DATA$C_previous), 1, 0 ) 
sum(C_NA)
cor(C_NA,duration_NA)
DATA$C_previous[C_NA>0] <-0 
DATA$C_previous <-  factor(DATA$C_previous)                           
risk_NA <- ifelse( is.na(DATA$risk_factor), 1, 0 )
sum(risk_NA)
DATA$risk_factor[risk_NA>0] <-0                     
DATA$risk_factor <-  factor(DATA$risk_factor)                           
DATA$homeowner <-  factor(DATA$homeowner)
DATA$married_couple <-  factor(DATA$married_couple)
summary(DATA)
### there should be no NA's in the data at this point....

# Question 1: Visualization
boxplot(cost~group_size, data=DATA, 
        xlab="Number of people covered under policy", 
        ylab="Cost of the quoted coverage options",
        main="Boxplot of Cost by Group Size",
        boxfill="lightblue")

# Question 2: A first linear Regression Model. 
result <- glm(cost ~ ., data = DATA) 
summary(result)
1 - (result$dev/result$null)
result <- glm(cost ~ ., data = DATA)
M <- model.matrix(cost~., data = DATA)
summary(M)
resultM <- glm(DATA$cost ~ M)
1 - (resultM$dev/resultM$null)
resultM5 <- glm(DATA$cost ~ M[,1:5])

result_interactions <- glm(cost ~ .+(A+B+C+D+E+F+G)^2, data = DATA) 

# Questions 3 is conceptual questions about modeling framework.
# No data analysis expected.

# Question 4 Provide quotes for new.customers
new.customers <- readRDS("NewCustomers.Rda")
##Core Task1
result_model = glm(cost ~ day + state + group_size + homeowner + 
                     car_value + car_age + risk_factor + age_oldest + age_youngest +
                     married_couple + C_previous + duration_previous + A + B + C + D + 
                     E + F + G + A:D + A:E + A:F + B:C + B:D + B:E + B:G + C:E + C:G + 
                     D:E + D:F + E:F + F:G, data = DATA)
predict (result_model, newdata= new.customers)

##Core Task2
DATA$cost_prediction = predict(result_model,newdata=DATA)
DATA$residual = DATA$cost-DATA$cost_prediction
DATA$win = ifelse(DATA$residual>0,1,0)
DATA$customer_win = ifelse(DATA$residual>0,1,0)
win_prediction=glm(win~day + state + group_size + homeowner + 
                     car_age + car_value + risk_factor + age_oldest + age_youngest + 
                     married_couple + C_previous + duration_previous + A + B + C + D + 
                     E + F + G + A:D + A:E + A:F + B:C + B:D + B:E + B:G + C:E + C:G + 
                     D:E + D:F + E:F + F:G, data = DATA, family = "binomial")
predict(win_prediction,newdata= new.customers, type="response")

value1=623.0392*0.5200724
value2=645.8769*0.3910235
value3=638.2425*0.4732155
result_values = c(value1,value2,value3)
result_values

# Question 5: No start script

