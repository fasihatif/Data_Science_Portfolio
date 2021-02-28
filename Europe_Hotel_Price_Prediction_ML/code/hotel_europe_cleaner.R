#0.1 |Load Libraries
#--------------------
library(ggpubr)
library(Metrics)
library(scales)
library(ranger)
library(kableExtra)
library(caret)
library(tidyverse)


#0.2 |Import Data
#-----------------
data <- read.csv('https://raw.githubusercontent.com/fasihatif/Data-Analysis-1-2-3/master/Data_Analysis_3/Assignment_3_DA3/data/raw/hotelbookingdata.csv')


#0.3 |Create backup for immediate data load
#--------------------------------------------
backup1 <- data
data <- backup1


#1.0 |                            Data Cleaning
#------------------------------------------------------------------------------#


#1.1 | distance to center entered as string in miles with one decimal
#----------------------------------------------------------------------
# distance to center entered as string in miles with one decimal
data$distance <- as.numeric(gsub("[^0-9\\.]","",data$center1distance))
data$distance_alter <- as.numeric(gsub("[^0-9\\.]","",data$center2distance))


#1.2| parsing accommodationtype column | replace missing values to handle split
#-------------------------------------------------------------------------------
data[data$accommodationtype == "_ACCOM_TYPE@",]$accommodationtype <- "_ACCOM_TYPE@NA"
data$accommodation_type <- unlist(sapply(strsplit(as.character(data$accommodationtype), "@"), '[[', 2))
data$accommodationtype <- NULL


#1.3| Remove '/5' from the end of ratings
#-----------------------------------------
data$rating <- as.numeric(gsub("/5","",data$guestreviewsrating))


#1.4| Adjust prices to be per day (rounded to nearest dollar)
#-------------------------------------------------------------
data <- data %>% mutate(price_per_night = round(ifelse(price_night == "price for 4 nights", price/4, price)))


#1.5| Rename columns
#--------------------
colnames(data)[colnames(data)=="addresscountryname"] <- "f_country"
colnames(data)[colnames(data)=="city_actual"] <- "f_city"
colnames(data)[colnames(data)=="starrating"] <- "stars"
colnames(data)[colnames(data)=="rating2_ta"] <- "avg_rating_ta"
colnames(data)[colnames(data)=="rating2_ta_reviewcount"] <- "rating_count_ta"
colnames(data)[colnames(data)=="rating"] <- "avg_guest_rating"
colnames(data)[colnames(data)=="rating_reviewcount"] <- "rating_count"


#1.6| Drop variables that arent required anymore
#------------------------------------------------
data$price <- NULL
data$price_night <- NULL
data$center1label <- NULL
data$center2label <- NULL
data$center1distance <- NULL
data$center2distance <- NULL
data$s_city <- NULL
data$offer <- NULL
data$offer_cat <- NULL
data$neighbourhood <- NULL
data$guestreviewsrating <- NULL


#1.7| Drop if hotel id is missing
#---------------------------------
# drop if hotel id is missing
data <- data[!is.na(data$hotel_id), ]


#1.8| Fix star ratings
#----------------------
#Hotels dont have 0 star ratings
table(data$stars)
data$stars[data$stars == 0] <- NA


#1.9| Drop perfect duplicates
#-----------------------------
data[duplicated(data)==T,]
#these are perfect duplicates of the observation in the previous row
data <- data[!duplicated(data), ]


#1.10| Convert columns to factor
#--------------------------------
# Convert columns to factor
data <- data %>%
  mutate(f_city = factor(f_city),
         f_country = factor(f_country))


#1.11| Take log of ratings
#--------------------------

# Check distribution
data %>%
  select(avg_guest_rating,rating_count,avg_rating_ta,rating_count_ta) %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

# Take log of ratings
data <- data %>%
  mutate(
    ln_rating_count = log(rating_count + 0.01),
    ln_rating_count_ta = log(rating_count_ta + 0.01))


#1.12| Take log of price_per_night
#----------------------------------
# Check distribution
ggplot(data, aes(x = price_per_night)) + geom_histogram()

# Remove extreme values
data <- data %>% filter(price_per_night <= 500)

# Take log of price_per_night
data <- data %>%
  mutate(
    ln_price_per_night = log(price_per_night))


#1.13| Take log of distance variables
#--------------------------------------

# Check distribution
data %>%
  select(distance, distance_alter) %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

# Take log of ratings
data <- data %>%
  mutate(
    ln_distance = log(distance + 1),
    ln_distance_alter = log(distance_alter + 1))


#1.14| Remove rows with missing values
#--------------------------------------
data <- data[complete.cases(data), ]


#2.0|                               Save cleaned data
#------------------------------------------------------------------------------#
# Write to csv file
data_out <- "C:/Users/abc/OneDrive/Business_Analytics/Data-Analysis-1-2-3/Data_Analysis_3/Assignment_3_DA3/data/clean/"
write_csv(data,paste0(data_out,"hotelbookingdata_clean.csv"))

