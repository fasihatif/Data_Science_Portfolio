# Import libraries
library(caret)
library(ranger)
library(kableExtra)
library(Metrics)
library(scales)
library(rattle)
library(tidyverse)
library(dplyr)

# Import hotels-europe file
data <- read.csv('https://raw.githubusercontent.com/fasihatif/Data-Analysis-1-2-3/master/Data_Analysis_3/atif-fasih_da3_data-exercise/CH16-Q5/data/raw/hotelbookingdata.csv')


# ------------------------------DATA CLEANING----------------------------------#

backup1 <- data
data <- backup1

# distance to center entered as string in miles with one decimal
data$distance <- as.numeric(gsub("[^0-9\\.]","",data$center1distance))
data$distance_alter <- as.numeric(gsub("[^0-9\\.]","",data$center2distance))

# parsing accommodationtype column
# replace missing values to handle split
data[data$accommodationtype == "_ACCOM_TYPE@",]$accommodationtype <- "_ACCOM_TYPE@NA"
data$accommodation_type <- unlist(sapply(strsplit(as.character(data$accommodationtype), "@"), '[[', 2))
data$accommodationtype <- NULL

# Remove '/5' from the end of ratings
data$rating <- as.numeric(gsub("/5","",data$guestreviewsrating))

# look at key vars
colnames(data)[colnames(data)=="starrating"] <- "stars"
table(data$stars)
data$stars[data$stars == 0] <- NA

# drop if hotel id is missing
data <- data[!is.na(data$hotel_id), ]

# DROP PERFECT DUPLICATES
data[duplicated(data)==T,]
#these are perfect duplicates of the observation in the previous row
data <- data[!duplicated(data), ]

# Rename variables
data <- data %>% mutate(avg_rating_ta = rating2_ta)
data <- data %>% mutate(rating_count_ta = rating2_ta_reviewcount)
data <- data %>% mutate(rating_count = rating_reviewcount)

# drop vars
data$center2distance <-  NULL
data$center1distance <-  NULL
data$center1label <- NULL
data$center2label <- NULL
data$rating2_ta <- NULL
data$rating2_ta_reviewcount <- NULL
data$price_night <- NULL
data$guestreviewsrating <- NULL
data$s_city <- NULL
data$offer <- NULL
data$offer_cat <- NULL
data$neighbourhood <- NULL
data$rating_reviewcount <- NULL

count_missing_values <- function(data) {
  num_missing_values <- map_int(data, function(x) sum(is.na(x)))
  num_missing_values[num_missing_values > 0]
}

count_missing_values(data)

# Filter data
data <- data %>% filter(data$accommodation_type == 'Hotel' )
data <- data %>% filter(year == 2018)
data <- data %>% filter(month == 2)


# Convert columns to factor
data <- data %>%
  mutate(f_city = factor(city_actual),
          f_country = factor(addresscountryname))

data$city_actual <- NULL
data$addresscountryname <- NULL

data <- data[complete.cases(data), ]

data_out <- "C:/Users/abc/OneDrive/Business_Analytics/Data-Analysis-1-2-3/Data_Analysis_3/atif-fasih_da3_data-exercise/CH16-Q5/data/clean/"
write.csv(data, paste0(data_out, "hotel-europe-clean.csv"))

# -----------------------------------------------------------------------------#

ggplot(data, aes(x = rating_count )) + geom_histogram() + scale_x_continuous(trans = log_trans())
ggplot(data, aes(x = rating )) + geom_histogram() + scale_x_continuous(trans = log_trans())

# -----------------------------------------------------------------------------#

n_var <- c("stars", "distance", "distance_alter")
d_var <- c("scarce_room", "weekend", "holiday")
f_var <- c("f_city","f_country")
ratings_var <- c("rating", "rating_count", "avg_rating_ta", "rating_count_ta")

rf_formula1 <- as.formula(paste("price ~ ",paste(c(n_var,f_var),collapse = " + ")))
rf_formula2 <- as.formula(paste("price ~ ",paste(c(n_var,f_var, d_var, ratings_var),collapse = " + ")))


train_indices <- as.integer(createDataPartition(data$price, p = 0.7, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]


# Model setup same for all Random Forest models
# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)


# ----------- RF Model 1----------#

#Calculate no of variables for mtry
lm_coeff <- lm(rf_formula1, data_train)
lm_coeff$rank -1 #4 variables


# Model tuning for Model RF model 1
# set tuning
tune_grid_1 <- expand.grid(
  .mtry = c(20,22,24,26),
  .splitrule = "variance",
  .min.node.size = c(5,10,15,20)
)

# Train Model 1

set.seed(1234)

rf_model_1 <- train(
  rf_formula1,
  data = data_train,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tune_grid_1,
  importance = "impurity"
)

# mtry = 26, splitrule = variance and min.node.size = 5 RMSE: 91.92

# ----------- RF Model 2----------#

#Calculate no of variables for mtry
lm_coeff_2 <- lm(rf_formula2, data_train)
lm_coeff_2$rank -1 #5 variables


# Model tuning for Model RF model 2
# set tuning
tune_grid_2 <- expand.grid(
  .mtry = c(20,22,24,26),
  .splitrule = "variance",
  .min.node.size = c(5,10,15,20)
)

# Train Model 2

set.seed(1234)
rf_model_2 <- train(
  rf_formula2,
  data = data_train,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tune_grid_2,
  importance = "impurity"
)

# mtry = 26, splitrule = variance and min.node.size = 5 RMSE:89.74


#------------------------------------------------------------------------------#
# Show Model 1 rmse shown with all the combinations
rf_tuning_model1 <- rf_model_1$results %>%
  dplyr::select(mtry, min.node.size, RMSE) %>%
  dplyr::rename(nodes = min.node.size) %>%
  spread(key = mtry, value = RMSE)


rf_tuning_model1_table <- rf_tuning_model1 %>% kbl(caption = "<center><strong>Random Forest RMSE by tuning parameters (Model 1)</strong></center>", escape = FALSE) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling( position = "center")


rf_tuning_model2 <- rf_model_2$results %>%
  dplyr::select(mtry, min.node.size, RMSE) %>%
  dplyr::rename(nodes = min.node.size) %>%
  spread(key = mtry, value = RMSE)

rf_tuning_model2_table <- rf_tuning_model2 %>% kbl(caption = "<center><strong>Random Forest RMSE by tuning parameters (Model 2)</strong></center>", escape = FALSE) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling( position = "center")

# Predict Holdout RMSE
rf_holdout_pred <- predict(rf_model_2, data_holdout)
rf_holdout_rmse <- RMSE(data_holdout$price, rf_holdout_pred) #RMSE:89.28



##############################
# full varimp plot, top 10 only
##############################
rf_model_2_var_imp <- importance(rf_model_2$finalModel)/1000
rf_model_2_var_imp_df <-
  data.frame(varname = names(rf_model_2_var_imp),imp = rf_model_2_var_imp) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))


# have a version with top 10 vars only
rf_model_2_var_imp_plot_b <- ggplot(rf_model_2_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color= "cyan3", size=2) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color= "cyan3", size=1) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=8), axis.text.y = element_text(size=8),
        axis.title.x = element_text(size=7), axis.title.y = element_text(size=7)) +
  labs(title = "Top 10 Imp Variables")

#----------------------------------------------------------------------------
group.importance <- function(rf.obj, groups) {
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(ranger::importance(rf.obj)[g], na.rm = TRUE)
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}


varnames <- rf_model_2$finalModel$xNames
f_city_group <- grep("f_city",varnames, value = TRUE)
f_country_group <- grep("f_country",varnames, value = TRUE)


groups <- list(f_city = f_city_group,
               f_country = f_country_group,
               stars="stars",
               ratings="ratings",
               distance="distance",
               distance_alter="distance_alter",
               rating_count="rating_count",
               rating_count_ta = "rating_count_ta")

rf_model_2_var_imp_grouped <- group.importance(rf_model_2$finalModel, groups)
rf_model_2_var_imp_grouped_df <- data.frame(varname = rownames(rf_model_2_var_imp_grouped),
                                            imp = rf_model_2_var_imp_grouped[,1])  %>%
  mutate(imp_percentage = imp/sum(imp))

rf_model_2_var_imp_grouped_plot <-
  ggplot(rf_model_2_var_imp_grouped_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color="cyan3", size=2) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color="cyan3", size=1) +
  ylab("Importance (Percent)") +   xlab("Variable Name") +
  coord_flip() +
  # expand=c(0,0),
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=8), axis.text.y = element_text(size=8),
        axis.title.x = element_text(size=7), axis.title.y = element_text(size=7)) +
  labs(title = "Grouped Imp Variables")





