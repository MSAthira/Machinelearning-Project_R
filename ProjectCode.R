#loading the dataset from working directory
Airbnb <- read.csv("group_25_26_Boston.csv")
Airbnb_2 <- Airbnb

View(Airbnb_2)

#Exercise 2 
is.na(Airbnb_2)
colSums(is.na(Airbnb_2))
ncol(Airbnb_2)
nrow(Airbnb_2)
Airbnb_4 <- Airbnb
View(Airbnb_4)
colSums(is.na(Airbnb_4))
sum(is.na(Airbnb_4))
library("fastDummies")

Airbnb_5 <- Airbnb
Airbnb_5 <- dummy_cols(Airbnb_5, select_columns = c("host_response_time", "host_is_superhost", "host_has_profile_pic", "host_identity_verified", "room_type","instant_bookable"), remove_selected_columns = TRUE)
Airbnb_5 <- subset(Airbnb_5, select = -c(name, host_since, city))
colSums(is.na(Airbnb_5))
names(Airbnb_5)[names(Airbnb_5) == "room_type_Entire home/apt"] <- "Entire_home_apt"
names(Airbnb_5)[names(Airbnb_5) == "room_type_Private room"] <- "Private_room"
names(Airbnb_5)[names(Airbnb_5) == "host_response_time_N/A"] <- "nouse"
Airbnb_5 <- subset(Airbnb_5, select = -c(nouse))
names(Airbnb_5)[names(Airbnb_5) == "room_type_Shared room"] <- "shared_room"
names(Airbnb_5)[names(Airbnb_5) == "room_type_Hotel room"] <- "Hotel_room"
Airbnb_5 <- subset(Airbnb_5, select = -c(review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, review_scores_communication, review_scores_location, review_scores_value, reviews_per_month))
Airbnb_5$host_response_rate[is.na(Airbnb_5$host_response_rate)] <- mean(Airbnb_5$host_response_rate, na.rm = TRUE)
Airbnb_5$host_acceptance_rate[is.na(Airbnb_5$host_acceptance_rate)] <- mean(Airbnb_5$host_acceptance_rate, na.rm = TRUE)
Airbnb_5$host_response_rate <- round(Airbnb_5$host_response_rate)
Airbnb_5$host_acceptance_rate <- round(Airbnb_5$host_acceptance_rate)
Airbnb_5 <- na.omit(Airbnb_5)
nrow(Airbnb_5)
library(psych)
describe(Airbnb_5)
Airbnb_5$price <- log(Airbnb_5$price)
round(cor(Airbnb_5$price, Airbnb_5, method = "pearson"), 2)
plot(Airbnb_5$beds, Airbnb_5$price)
plot(Airbnb_5$bedrooms, Airbnb_5$price)
plot(Airbnb_5$accommodates, Airbnb_5$price)
Air_data <- subset(Airbnb_5, select = c(price, beds, accommodates, bedrooms))
library(GGally)
ggpairs(Air_data)
#highly correlated variables
cor(Airbnb_5$beds, Airbnb_5$accommodates)
cor(Airbnb_5$beds, Airbnb_5$bedrooms)
cor(Airbnb_5$bedrooms, Airbnb_5$accommodates)

###  Exercise 3: Ivestigate drivers of prices
model1 <- glm(formula = price ~ ., data = Airbnb_5)
summary(model1)
plot(model1)
model2 <- lm(formula = price ~ ., data = Airbnb_5)
summary(model2)
plot(model2)

###### Exercise 4: predicting the price
library(glmnet)
set.seed(123)
#splitting data
x_train <- sample(1:nrow(Airbnb_5), 
                  round(nrow(Airbnb_5) * 0.75))
training_data <- Airbnb_5[x_train, ]
test_data <- Airbnb_5[-x_train, ]
response_training <- Airbnb_5[x_train,]$price
response_test <- Airbnb_5[-x_train,]$price
#fitting the model from exercise3 to training data
model3 <- lm(formula = price ~ ., data = training_data)
summary(model3)
help("predict")
predict_lm <- predict(model3, newdata = test_data)
#MSE
mean((predict_lm - response_test)^2)
??MSE
install.packages("MLmetrics")
library(MLmetrics)
MSE(predict_lm, response_test)
#glm
set.seed(123)
model4 <- glm(formula = price ~ ., data = training_data)
summary(model4)
predict_glm <- predict(model4, newdata = test_data)
mean((predict_glm - response_test)^2)# calculating the erorr rate 
## ridge regression
set.seed(123)
X_train <- model.matrix(price ~ ., data = training_data)[, -1]
X_test <- model.matrix(price ~ ., data = test_data)[, -1]
cv.out <- cv.glmnet(X_train, response_training, alpha = 0)
bestlam <- cv.out$lambda.min
bestlam
ridge_bestlam <- glmnet(X_train, response_training, alpha = 0, lambda = bestlam)
coef(ridge_bestlam)
ridge.pred.bestlam <- predict(ridge_bestlam, newx = X_test)
mean((ridge.pred.bestlam - response_test)^2)
MSE(ridge.pred.bestlam, response_test)# calculating the error rate  
#lasso regression
set.seed(123)
cv.out1 <- cv.glmnet(X_train, response_training, alpha = 1)
bestlam1 <- cv.out1$lambda.min
bestlam1
lasso_bestlam <- glmnet(X_train, response_training, alpha = 1, lambda = bestlam1)
# we can have a look at the coefficients of the final model:
coef(lasso_bestlam)
lasso.pred.bestlam <- predict(lasso_bestlam, newx = X_test)
#MSE
mean((lasso.pred.bestlam - response_test)^2)# calcalutaing eroor rate 

# error rate is ridge is less we choose ridge 
#log of price is giving the answer for all ,if we de log we get the original,price value transforming the price slide 14 lect5 

