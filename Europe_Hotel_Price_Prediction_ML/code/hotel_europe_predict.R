#0.1| Load libraries
#--------------------
library(ggpubr)
library(Metrics)
library(scales)
library(ranger)
library(kableExtra)
library(rpart)
library(ranger)
library(caret)
library(tidyverse)


#0.2|Import Data
#-----------------
data <- read.csv('https://raw.githubusercontent.com/fasihatif/Data-Analysis-1-2-3/master/Data_Analysis_3/Assignment_3_DA3/data/clean/hotelbookingdata_clean.csv')

#0.3| Create backup for immediate data load
#--------------------------------------------
backup2 <- data
data <- backup2

#1.0|                             Filter data
#------------------------------------------------------------------------------#

#1.1| Filter for cities 
#-----------------------
cities_filter <- c("Berlin", "Munich", "Vienna", "Budapest", "Prague", "Warsaw")
data <- data %>% filter(f_city %in% cities_filter)

data$f_city <- droplevels(data$f_city)
data$f_country <- droplevels(data$f_country)

#1.2| Filter for date 
#----------------------
data <- data %>% filter(year == 2018)
data <- data %>% filter(month == 2)
data <- data %>% filter(accommodation_type == "Hotel")


#1.3| Filter for hotels
#-----------------------
data <- data %>% filter(accommodation_type == "Hotel")


#2.0|                               Model Construction
#------------------------------------------------------------------------------#

#2.1| Assign variables to categories for easier use in constructing models
#--------------------------------------------------------------------------
n_var <- c("distance", "distance_alter")
d_var <- c("scarce_room")
f_var <- c("f_country", "f_city")
ratings_var <- c("stars", "avg_guest_rating", "rating_count", "avg_rating_ta", "rating_count_ta")
log_var <- c("ln_rating_count", "ln_rating_count_ta", "ln_distance", "ln_distance_alter")


#2.2| Construct model equation for OLS
#--------------------------------------
ols_formula1 <- as.formula(paste("price_per_night ~ ",paste(c(n_var,d_var,f_var,ratings_var),collapse = " + ")))
ols_formula2 <- as.formula(paste("price_per_night ~ ",paste(c(n_var,d_var,f_var,ratings_var,log_var),collapse = " + ")))


#2.3| Construct model equation for LASSO
#----------------------------------------
lasso_formula <- as.formula(paste("price_per_night ~ ",paste(c(n_var,d_var,f_var,ratings_var,log_var),collapse = " + ")))


#2.4| Construct model equation for CART
#---------------------------------------
cart_formula <- as.formula(paste("price_per_night ~ ",paste(c(n_var,d_var,f_var,ratings_var),collapse = " + ")))


#2.5| Construct model equation for Random Forest
#------------------------------------------------
rf_formula <- as.formula(paste("price_per_night~ ",paste(c(n_var,d_var,f_var,ratings_var),collapse = " + ")))


#3.0|                                Data Split
#------------------------------------------------------------------------------#

set.seed(1234)
train_indices <- as.integer(createDataPartition(data$price_per_night, p = 0.75, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]


#4.0|                          OLS Linear Rregression
#------------------------------------------------------------------------------#

#4.1| Set OLS tuning parameters
#--------------------------------
train_control <- trainControl(
  method = "cv",
  number = 5)

#4.2| Prepare model 1
#---------------------
set.seed(1234)
ols_model1 <- caret::train(ols_formula1,
                            data = data_train,
                            method = "lm",
                            preProcess = c("center", "scale"),
                            trControl = train_control,
                            na.action=na.omit)
#RMSE 45.72795


#4.3| Prepare model 2
#----------------------
set.seed(1234)
ols_model2 <- caret::train(ols_formula2,
                          data = data_train,
                          method = "lm",
                          preProcess = c("center", "scale"),
                          trControl = train_control,
                          na.action=na.omit)

#RMSE 44.87983

#4.4| Predict on holdout set
#----------------------------
data_holdout_ols_prediction <- data_holdout %>%
  mutate(predicted_price = predict(ols_model2, newdata = data_holdout, na.action = na.omit))

rmse(data_holdout_ols_prediction$predicted_price, data_holdout$price_per_night) #46.15864


#5.0|                                   LASSO
#------------------------------------------------------------------------------#

#5.1| Set lasso tuning parameters
#---------------------------------
train_control <- trainControl(
  method = "cv",
  number = 5)

#5.2| Set tune_grid parameteres
#-------------------------------
tune_grid <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))

set.seed(1234)
lasso_model <- caret::train(lasso_formula,
                            data = data_train,
                            method = "glmnet",
                            preProcess = c("center", "scale"),
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            na.action=na.omit)

print(lasso_model$bestTune$lambda) # lambda 0.05 | RMSE 44.87893

lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `1`)  # the column has a name "1", to be renamed


lasso_coeffs %>% kbl(caption = "<center><strong>Lasso Model Coefficients</strong></center>", escape = FALSE) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling( position = "center")

data_holdout_lasso_prediction <- data_holdout %>%
  mutate(predicted_price = predict(lasso_model, newdata = data_holdout, na.action = na.omit))

rmse(data_holdout_lasso_prediction$predicted_price, data_holdout$price_per_night) #46.09648


#7.0|                                  CART
#------------------------------------------------------------------------------#
# Do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)


#7.2| Set tuning parameters
#---------------------------------
tune_grid <- expand.grid(
  .mtry = c(3,4,5),
  .splitrule = "variance",
  .min.node.size = c(5, 7,10))


#7.3| Train Random Forest Model
#-------------------------------

set.seed(1234)
cart_model <- train(
  cart_formula, data=data_train, method = "rpart",
  trControl = train_control,
  tuneGrid= expand.grid(cp = c(0.001,0.005,0.01,0.05)),
  na.action = na.pass)


data_holdout_cart_prediction <- predict(cart_model , newdata=data_holdout)
rmse(data_holdout_cart_prediction, data_holdout$price_per_night) #rmse 49.75809


#7.0|                               Random Forest
#------------------------------------------------------------------------------#

#7.1| Set train control parameter
#---------------------------------

# Do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)


#7.2| Set tuning parameters
#---------------------------------
tune_grid <- expand.grid(
  .mtry = c(3,4,5),
  .splitrule = "variance",
  .min.node.size = c(5, 7,10))


#7.3| Train Random Forest Model
#-------------------------------

set.seed(1234)

rf_model <- train(
  rf_formula,
  data = data_train,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tune_grid,
  importance = "impurity")
    
# mtry = 4, splitrule = variance and min.node.size = 5


#7.4| Predict model on holdout set
#----------------------------------
data_holdout_rf_prediction <- data_holdout %>%
  mutate(predicted_price = predict(rf_model, newdata = data_holdout))

rmse(data_holdout_rf_prediction$predicted_price, data_holdout$price_per_night) #60.90178




