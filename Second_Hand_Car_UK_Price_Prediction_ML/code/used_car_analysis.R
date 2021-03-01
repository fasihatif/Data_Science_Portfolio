data <- read_csv("https://raw.githubusercontent.com/fasihatif/Data-Analysis-1-2-3/master/Data_Analysis_3/atif-fasih_da3_data-exercise/CH13_Q3/data/ford.csv")

summary(data)

# Filter for cars not older than 2015
data <-  data %>%
  filter(year > 2014)

# MISSING VALUES IN DF
# Check for Missing Values in DF
na_count <- sapply(data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

# Types of engine size
data %>%
  group_by(model) %>%
  dplyr::summarize(frequency=n()) %>%
  mutate(percent = frequency / sum(frequency)*100,
         cumulative_percent = cumsum(frequency)/sum(frequency)*100)

# Focus only on Ford Fiesta
data <-  data %>%
  filter(model ==  "Fiesta")


# Types of fuelType cars
data %>%
  group_by(fuelType) %>%
  dplyr::summarize(frequency=n()) %>%
  mutate(percent = frequency / sum(frequency)*100,
         cumulative_percent = cumsum(frequency)/sum(frequency)*100)

# Focus only on petrol cars
data <-  data %>%
  filter(fuelType ==  "Petrol")


# Types of transmission
data %>%
  group_by(transmission) %>%
  dplyr::summarize(frequency=n()) %>%
  mutate(percent = frequency / sum(frequency)*100,
         cumulative_percent = cumsum(frequency)/sum(frequency)*100)

# Focus only on Manual transmission cars
data <-  data %>%
  filter(transmission ==  "Manual")


# Types of engine size
data %>%
  group_by(engineSize) %>%
  dplyr::summarize(frequency=n()) %>%
  mutate(percent = frequency / sum(frequency)*100,
         cumulative_percent = cumsum(frequency)/sum(frequency)*100)

# Focus only on engine size of 1,1.1, and 1.2
data <-  data %>%
  filter(engineSize ==  c(1,1.1,1.2))

# Dummy variables for engineSize and covert to integer
data <- data %>%
  mutate(eng_1.0 = ifelse(engineSize == 1,1,0),
         eng_1.1 = ifelse(engineSize == 1.1,1,0),
         eng_1.2 = ifelse(engineSize == 1.2,1,0))
  
data <- data %>%
  mutate(eng_1.0 = as.integer(eng_1.0),
         eng_1.1 = as.integer(eng_1.1),
         eng_1.2 = as.integer(eng_1.2))


# Convert years into Age
data$age <- 2021-data$year


# age: quadratic, cubic
data <- data %>%
  mutate(agesq = age^2,
         agecu = age^3)

# mileage: quadratic
data <- data %>%
  mutate(mileagesq = mileage^2)


# save workfile
data_out <- "~/OneDrive/Business_Analytics/Data-Analysis-1-2-3/Data_Analysis_3/atif-fasih_da3_data-exercise/CH13_Q3/output/"
write.csv(data, paste0(data_out, "usedcars_uk.csv"), row.names = F)


###########################################################################
data_in <- "C:/Users/abc/OneDrive/Business_Analytics/Data-Analysis-1-2-3/Data_Analysis_3/atif-fasih_da3_data-exercise/CH13_Q3/output/"
data <- read.csv(paste0(data_in,"usedcars_uk.csv"), stringsAsFactors = TRUE)

ggplot(data=data, aes(x=price)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 1000, boundary=0,
                 color = "white", fill = "blue", size = 0.25, alpha = 0.8,  show.legend=F, na.rm=TRUE) +
  coord_cartesian(xlim = c(2700, 20000)) +
  labs(x = "Price (US dollars)",y = "Percent")+
  theme_bw() +
  expand_limits(x = 0.01, y = 0.01) +
  scale_y_continuous(expand = c(0.01,0.01),labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(expand = c(0.01,0.01),breaks = seq(0,20000, 2500))


###############################################################################
# REGRESSION ANALYSIS


# lowess
ggplot(data = data, aes(x=age, y=price)) +
  geom_point( color = "blue", size = 1,  shape = 16, alpha = 0.8, show.legend=F, na.rm = TRUE) + 
  geom_smooth(method="loess", se=F, colour=color[4], size=1, span=0.9) +
  labs(x = "Age (years)",y = "Price (US dollars)") +
  theme_bw() +
  expand_limits(x = 0.01, y = 0.01) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,25000), breaks = seq(0,25000, 5000)) +
  scale_x_continuous(expand = c(0.01,0.01),limits = c(0,7), breaks = seq(0,7,1))


###################################
# Linear regressions

# Model 1: Linear regression on age
model1 <- as.formula(price ~ age + agesq)
# Models 2-5: Multiple linear regressions
# note: condition - missing will be baseline for regs

model2 <- as.formula(price ~ age + agesq + mileage + mileagesq)
model3 <- as.formula(price ~ age + agesq + mileage + mileagesq + tax + mpg + eng_1.0 + eng_1.1 + eng_1.2)
model4 <- as.formula(price ~ age + agesq + agecu + mileage + mileagesq + tax + eng_1.0*age + eng_1.1*age + eng_1.2*age +
                       mpg*age + mileage*age + tax*age)

#################################################################
# Cross-validation

# set number of folds
k <- 4

set.seed(12345)
cv1 <- train(model1, data, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(12345)
cv2 <- train(model2, data, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(12345)
cv3 <- train(model3, data, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")
set.seed(12345)
cv4 <- train(model4, data, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")


# calculate average rmse
cv <- c("cv1", "cv2", "cv3", "cv4")
rmse_cv <- c()

for(i in 1:length(cv)){
  rmse_cv[i] <- sqrt((get(cv[i])$resample[[1]][1]^2 +
                        get(cv[i])$resample[[1]][2]^2 +
                        get(cv[i])$resample[[1]][3]^2 +
                        get(cv[i])$resample[[1]][4]^2)/4)
}


# summarize results
cv_mat <- data.frame(rbind(cv1$resample[4], "Average"),
                     rbind(cv1$resample[1], rmse_cv[1]),
                     rbind(cv2$resample[1], rmse_cv[2]),
                     rbind(cv3$resample[1], rmse_cv[3]),
                     rbind(cv4$resample[1], rmse_cv[4])
)

colnames(cv_mat)<-c("Resample","Model1", "Model2", "Model3", "Model4")
cv_mat

# stargazer(cv_mat, summary = F, digits=0, float=F, out=paste(output,"ch13-table-5-cvmat.tex",sep=""))
# stargazer(cv_mat, summary = F, digits=0, float=F, type="text",  out=paste(output,"ch13-table-5-cvmat.txt",sep=""))

###############################################################################
# Prediction

# Predict price with all predictors (Model3)
reg1 <- lm(model1, data=data)
# Standard errors of residuals
p1 <- predict(reg1, data)
resid_p1 <- p1-data$price
summary(resid_p1)
# predict value for newly added obs
pred1_new <- predict(reg1, newdata = new,se.fit = TRUE, interval = "prediction")
p1<- pred1_new$fit

# Predict price with all predictors (Model3)
reg3 <- lm(model3, data=data)
# Standard errors of residuals
p2 <- predict(reg3, data)
resid_p2 <- p2-data$price
summary(resid_p2)
# predict value for newly added obs
pred2_new <- predict(reg3, newdata = new,se.fit = TRUE, interval = "prediction")
p2<- pred2_new$fit
pred2_new 

#get model rmse
data$p2a <- predict(reg3, data)
rmse2 <- RMSE(data$p2a,data$price)
rmse2

# Result summary
sum1 <- cbind(t(p1), t(p2))
colnames(sum1) <- c('Model1', 'Model3')
rownames(sum1) <- c('Predicted', 'PI_low (95%)', 'PI_high (95%)')

sum1

stargazer(sum1, summary = F, digits=0, float=F, out=paste(output,"ch13-table-3-pred-new.tex",sep=""))
stargazer(sum1, summary = F, digits=0, float=F, type="text",  out=paste(output,"ch13-table-3-pred-new.txt",sep=""))
# old name: Ch13_pred_R.txt

# prediction


# summary of predictions and PI 80% version
# predict value for newly added obs
pred1_new80 <- predict(reg1, newdata = new, se.fit=TRUE, interval = "prediction", leve=0.8)
p180<- pred1_new80$fit
pred2_new80 <- predict(reg3, newdata = new,se.fit = TRUE, interval = "prediction", level=0.8)
p280<- pred2_new80$fit

# Result summary
sum2 <- cbind(t(p180), t(p280))
colnames(sum2) <- c('Model1', 'Model3')
rownames(sum2) <- c('Predicted', 'PI_low (80%)', 'PI_high (80%)')
sum2
