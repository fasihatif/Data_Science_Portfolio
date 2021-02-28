
# Import libraries
library(rmdformats)
library(stringr)  
library(fastDummies)
library(ggpubr)
library(cowplot)
library(rstatix)
library(scales)
library(Metrics)
library(caret)
library(stargazer)
library(kableExtra)
library(ranger)
library(tidyverse)
library(dplyr)


# Import data

listings <- read.csv("https://raw.githubusercontent.com/fasihatif/Data-Analysis-1-2-3/master/Data_Analysis_3/Assignment_1_DA3/data/raw/listings.csv")
data <- listings

# Filter for apartments which accommodate 2-6 persons
library(dplyr)
data <- data %>%
  filter(property_type %in% c("Entire apartment", "Entire serviced apartment", "Entire home/apt", "Private room in apartment", "Private room in serviced apartment", "Shared room in apartment", "Room in serviced apartment")) %>%
  filter(between(accommodates, 2, 6))

library(stringr)
# Drop unnecessary columns
data <- data[grep("^host", colnames(data), invert = TRUE)]
data <- data[grep("^calculated", colnames(data), invert = TRUE)]
data <- data %>% dplyr::select(-contains("maximum"))
data <- data %>% select(-c("listing_url","scrape_id","last_scraped","name","description","neighborhood_overview","picture_url",
                           "neighbourhood_group_cleansed","bathrooms","minimum_minimum_nights", "minimum_nights_avg_ntm","calendar_updated",
                           "calendar_last_scraped","number_of_reviews_ltm","number_of_reviews_l30d","license","reviews_per_month",
                           "availability_30","availability_60","availability_90","availability_365","neighbourhood","has_availability"))

# Format amenities column. Remove square brackets and convert to vector
data$amenities<-gsub("\\[","",data$amenities)
data$amenities<-gsub("\\]","",data$amenities)
data$amenities<-gsub('\\"',"",data$amenities)
data$amenities <- as.list(strsplit(data$amenities, ","))
#define levels and dummies 
levs <- levels(factor(unlist(data$amenities)))
data <- cbind(data, as.data.frame(do.call(rbind, lapply(lapply(data$amenities, factor, levs), 
                                                        table))))
data <- data %>% select(-(224:273))

# Remove all whitespaces from column names
names(data) <- trimws(names(data))

# Repace all spaces between words with underscores
names(data) <- str_replace_all(names(data), " ", "_")

backup_a <- data


#Checkpoint 1: Initial column cleaning
data <- backup_a

# rename some columns for easier aggregation
names(data)[names(data) == "Mini_fridge"] <- "Mini_frige"
names(data)[names(data) == "Shower_gel"] <- "Shower_gel_soap"
names(data)[names(data) == "Barbecue_utensils"] <- "BBQ_utensils"
names(data)[names(data) == "Freezer"] <- "Freezer_frige"
names(data)[names(data) == "Free_residential_garage_on_premises"] <- "free_garage_parking"
names(data)[names(data) == "Amazon_Prime_Video"] <- "Amazon_Prime_TV"

# ------------------------------------------------------------------------------

### Updated aggregate_columns function code ###
# Example: Combine all sound system columns into 1 column.There are several different kinds of sound systems present.We would like to
# create one generic sound category.

# Pass a vector of phrases to the for loop to make the process quicker
column_names <- c("sound", "stove","Wifi","TV","oven","frige", "soap", "BBQ", "toys", "crib", "parking", "shampoo", "heating","washer","toiletries","conditioner","dry")

for( word in column_names){
  
  # Subset columns which contains a specific word and save them to another dataframe. Also select 'id' to use for merge later
  new_df <- data %>% select(contains(word),"id")
  
  #Go row by row to see if any of the rows have at least one '1'. If it does, populate new column 'col_name' with 1
  new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
  # Save new column and id column to another dataframe. We use this new dataframe to merge with original dataframe
  new_df_merge <- new_df %>% select(id,col_name)
  
  #merge original dataframe and new_df_merge by 'id'
  data <- merge(data,new_df_merge,by = "id", all = FALSE)
  
  #remove the new column and 'id' column from the new_df dataframe
  new_df <- new_df %>% select(-c(id,col_name))
  
  # Remove the subset columns from original dataframe since they have already been aggregated into a new column and merged
  data <- data %>% select(-colnames(new_df))
  
  # Convert from character to integer
  data$col_name <- as.integer(data$col_name)
  
  # Rename new column
  names(data)[names(data) == 'col_name'] <- paste0(word,"_agg")
  
} 

# Checkpoint 2:Subset data further for cleaning
backup_b <- data

data <- backup_b

# Subset all ameneties columns and remove any which have '1' less than 5%
amenities_clean <- data %>% select(25:125, "id")
less_than_5per <- amenities_clean %>% select(where(~mean(. == 1) <= 0.005))
less_than_5per <- less_than_5per %>% select(-contains(c("id")))
amenities_clean <- amenities_clean %>% select(-colnames(less_than_5per))

# Check for count
amenities_clean_df <- as.data.frame(sapply(amenities_clean, function(x){sum(x)}))

# Merge the original and amenities dataframe
data <- data %>% select(-(25:125))
data <- merge(data,amenities_clean, by = "id", all = FALSE)

#remove dollar signs from price variable. These prices are actually Euros
data$price<-gsub("\\$","",as.character(data$price))
data$price<-as.numeric(as.character(data$price))

data <- data %>% select(-c("amenities", "Babysitter_recommendations", "Baby_bath", "Baking_sheet"))

names(data)[names(data) == "frige_agg"] <- "refrigerator"
names(data)[names(data) == "bathrooms_text"] <- "bathrooms"

# Remove text from bathrooms column
table(data$bathrooms)

data$bathrooms <- replace(data$bathrooms,data$bathrooms == '1 bath',1)
data$bathrooms <- replace(data$bathrooms,data$bathrooms == 'Half-bath',0.5)
data$bathrooms <- gsub("baths", "", data$bathrooms)
data$bathrooms <- as.numeric(data$bathrooms)

backup_c <- data

data <- backup_c

## Create dummy variables using the fastdummies library

## Create dummy variables using the fastdummies library
data$instant_bookable <- replace(data$instant_bookable,data$instant_bookable == 'TRUE', "1")
data$instant_bookable <- replace(data$instant_bookable,data$instant_bookable == 'FALSE', "0")


# data <- data %>% dummy_cols(select_columns = "instant_bookable", remove_selected_columns = TRUE)
# data <- data  %>% select(-c("instant_bookable_f"))

# create dummy vars
dummies <- names(data)[seq(23,90)]

data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))

dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))

# ------------------------------------------------------------------------------

# Convert room type to factor
table(data$room_type)

# Rename room type
data$room_type <- replace(data$room_type,data$room_type == 'Entire home/apt', "Entire_apt")
data$room_type <- replace(data$room_type,data$room_type == 'Hotel room', "Private room")
data$room_type <- replace(data$room_type,data$room_type == 'Private room', "Private_room")
data$room_type <- replace(data$room_type,data$room_type == 'Shared room', "Shared_room")

data <- data %>%
  mutate(f_room_type = factor(room_type))


# Rename neighborhoods
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'Bijlmer-Centrum', "Zuidoost")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'Bijlmer-Oost', "Zuidoost")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'Bos en Lommer', "West")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'Buitenveldert - Zuidas', "Zuid")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'Centrum-Oost', "Centre")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'Centrum-West', "Centre")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'De Aker - Nieuw Sloten', "Nieuw-West")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'De Baarsjes - Oud-West', "West")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'De Pijp - Rivierenbuurt', "Zuid")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'Gaasperdam - Driemond', "Zuidoost")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'Geuzenveld - Slotermeer', "Nieuw-West")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'IJburg - Zeeburgereiland', "Oost")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'Noord-Oost', "Noord")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'Noord-West', "Noord")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'Oostelijk Havengebied - Indische Buurt', "Oost")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'Osdorp', "Nieuw-West")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'Oud-Noord', "Noord")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'Oud-Oost', "Oost")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'Slotervaart', "Nieuw-West")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'Watergraafsmeer', "Oost")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'Westerpark', "West")
data$neighbourhood_cleansed <- replace(data$neighbourhood_cleansed,data$neighbourhood_cleansed == 'Zuid', "Zuid")


# Convert neighbourhood_cleansed to factors
data <- data %>%
  mutate(
    f_District = factor(neighbourhood_cleansed))


# Property Type
table(data$property_type)

data$property_type <- replace(data$property_type,data$property_type == 'Entire apartment', 'Entire_apartment')
data$property_type <- replace(data$property_type,data$property_type == 'Entire home/apt', 'Entire_apartment')
data$property_type <- replace(data$property_type,data$property_type == 'Entire serviced apartment', 'Entire_apartment')
data$property_type <- replace(data$property_type,data$property_type == 'Room in serviced apartment', 'Room_apartment')
data$property_type <- replace(data$property_type,data$property_type == 'Private room in apartment', 'Room_apartment')
data$property_type <- replace(data$property_type,data$property_type == 'Private room in serviced apartment', 'Room_apartment')
data$property_type <- replace(data$property_type,data$property_type == 'Shared room in apartment', 'Room_apartment')


data <- data %>%
  mutate(f_property_type = factor(property_type))


# ------------------------------------------------------------------------------

# add new numeric columns from certain columns
numericals <- c("accommodates","bathrooms", "bedrooms", "beds","minimum_nights", "number_of_reviews", "review_scores_rating")
data <- data %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))

# rename columns so they start with n_ as opposed to end with _n
nnames <- data %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(data))
colnames(data)[nnames_i] <- paste0("n_", numericals)

#-------------------------------------------------------------------------------

# keep columns if contain d_, n_,f_, p_, usd_ and some others
data <- data %>%
  select(id,price,matches("^d_.*|^n_.*|^f_.*"))


amenities_convert<- data %>%
  select(starts_with("d_"),"id") 

amenities_convert <- amenities_convert %>%mutate_if(is.integer,as.numeric)
glimpse(amenities_convert)

data <- data %>%
  select(-starts_with("d_")) 

data <- merge(data,amenities_convert, by = "id")

data <- data %>% mutate(id = as.numeric(id))


# Squares and further values to create for accommodation
data <- data %>%
  mutate(n_accommodates2=n_accommodates^2, ln_accommodates=log(n_accommodates))

data <- data %>%
  mutate(ln_number_of_reviews = log(n_number_of_reviews+1))

backup_d <- data

data <- backup_d

# EXPLORATORY DATA ANALYSIS

# Take log of price
data <- data %>%
  mutate(ln_price = log(price))


# Remove extreme values
data <- data %>%
  filter(price < 650)

# Price Distribution
price_hist <- ggplot(data, aes( x = price)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),fill = "cyan3", color = "black") +
  theme_bw() +
  scale_y_continuous(labels = label_percent()) +
  ylab("Percent") + 
  xlab("Price (Euros)")


ln_price_hist <- ggplot(data, aes(x = ln_price)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),fill = "cyan3", color = "black") +
  theme_bw() +
  scale_y_continuous(labels = label_percent()) +
  ylab("Percent") + 
  xlab("ln(Price, Euros)")

price_hist_grid <- ggarrange(
  price_hist,
  ln_price_hist,
  nrow = 1)

annotate_figure(price_hist_grid,bottom = text_grob("Note: Apartments with 2-6 accommodation capacity. Histogram without extreme values (price < 650 Euros) 
                                                                                                                                                                                                                                                                                               Data source: Inside Airbnb dataset, Amsterdam, Dec 2020", color = "black",vjust = 0.5,hjust = 1, x = 1, face = "italic", size = 10))

# Room Type
room_type_box <- ggplot(data, aes(x = f_room_type, y = price)) +
  stat_boxplot(aes(group = f_room_type), geom = "errorbar", width = 0.3,
               na.rm=T) +
  geom_boxplot(aes(group = f_room_type),
               size = 0.5, width = 0.6,  fill = c("cyan3","brown2", "green"),alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,300), breaks = seq(0,300,100)) +
  labs(x = "Room type",y = "Price (Euros)")+
  theme_bw()

prop_with_accomm_box <- ggplot(data, aes(x = factor(n_accommodates), y = price,
                        fill = f_property_type, color= f_property_type)) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
 stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  labs(x = "Accomodates (Persons)",y = "Price (Euros)") +
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 400), breaks = seq(0,400, 50)) +
  theme_bw() + theme(legend.position = c(0.26,0.88)) + theme(legend.title = element_blank())

price_prop_type <- ggarrange(
  room_type_box,
  prop_with_accomm_box,
  nrow = 1)

annotate_figure(price_prop_type,bottom = text_grob("Note: Box plots for price, graphs dont show extreme values
                                                                          Data source: Inside Airbnb dataset, Amsterdam, Dec 2020", color = "black",vjust = 0.5,hjust = 1.05, x = 1, face = "italic", size = 10))

accom_point <- ggplot(data = data, aes(x=n_accommodates, y=price)) +
  geom_point(size=1, colour= "grey", shape=16)+
 # ylim(0,800)+
# xlim(0,15)+
  labs(x="Number of people accomodated",y="Price")+
  geom_smooth(method="loess", colour= "cyan3", se=FALSE)+
  theme_bw() +
    labs(x = "Accommodation Capacity")

accom_hist <- ggplot(data, aes(x = n_accommodates)) + geom_histogram(fill = "cyan3")+ theme_bw() +
  labs(x = "Accommodation Capacity")

accom_combine <- ggarrange(
  accom_hist,
  accom_point,
  nrow = 1)

accom_combine

# Take logs of beds
data <- data %>%
  mutate(ln_beds = log(n_beds + 1))

#ggplot(data, aes(x = n_beds)) + geom_histogram() + theme_bw()

# Plot a non parametric regression plot
beds_plot <- ggplot(data = data, aes(x= ln_beds, y=price)) +
  geom_point(size=1, colour= "cyan3", shape=16)+
  labs(x="ln(Number of people accomodated)",y="ln(Price, Euros")+
  geom_smooth(method="loess", colour= "red", se=FALSE)+
  theme_bw()

# Plot a residual chart

#fit a regression model
#model_bed <- lm(price ~ ln_beds , data = data)
#get list of residuals 
#qq_res <- resid(model_bed)
#residual_chart_bed <- plot(fitted(model_bed), qq_res)
#add a horizontal line at 0 
#abline(0,0)

# We can also produce a Q-Q plot, which is useful for determining if the residuals follow a normal distribution
#qqnorm(qq_res)

# Result: Mostly falls along the 45 degree straight line. Points deviate at the end points



# Pool accommodations with 0,1,2,10 bathrooms

data <- data %>%
  mutate(f_bathroom = cut(n_bathrooms, c(0,1,2,5), labels=c(0,1,2), right = F) )


# Pool num of reviews to 3 categories: none, 1-51 and >51
data <- data %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,51,max(data$n_number_of_reviews)), labels=c(0,1,2), right = F))


# Pool and categorize the number of minimum nights: 1,2,3, 3+

data <- data %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,2,3,max(data$n_minimum_nights)), labels=c(1,2,3), right = F))

data <- data %>% select(-d_luggage_store_possible__small_fee__i_wash_your_dishes_enjoy_holiday_my_fridge_in_kitchen)


# Change Infinite values with NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)


# Number of missing values in each column
na_count <- sapply(data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

# Price, bathrooms, review_scores_rating, n_bedrooms, n_beds columns have missing values

# Since Price has only 16 missing values, we will drop the observations with missing price values
data <- data %>% 
  drop_na(price)

# Fill missing values
data <- data %>%
  mutate(
    n_bathrooms =  ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms), #assume at least 1 bath
    n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds), #assume n_beds=n_accomodates
    f_bathroom=ifelse(is.na(f_bathroom),1, f_bathroom),
    f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
    f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
    ln_beds=ifelse(is.na(ln_beds),0, ln_beds),
    n_bedrooms=ifelse(is.na(n_bedrooms),1, n_bedrooms)
  )

data <- data %>%
  mutate(
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating))


data <- data %>%
  mutate_if(is.character, factor)

# Assign columns to grouped variables for model equations
n_var <- c("n_accommodates", "ln_beds", "n_bedrooms", "n_review_scores_rating", "flag_review_scores_rating")
f_var <- c("f_room_type", "f_minimum_nights", "f_number_of_reviews", "f_bathroom", "f_District")
poly_var <- "n_accommodates2"
# Dummy variables: Extras -> collect all options and create dummies
d_amenities <-  grep("^d_.*", names(data), value = TRUE)


price_diff_by_variables2 <- function(df, factor_var, dummy_var, factor_lab, dummy_lab){
  # Looking for interactions.
  # It is a function it takes 3 arguments: 1) Your dataframe,
  # 2) the factor variable (like room_type)
  # 3)the dummy variable you are interested in (like TV)

  # Process your data frame and make a new dataframe which contains the stats
  factor_var <- as.name(factor_var)
  dummy_var <- as.name(dummy_var)

  stats <- df %>%
    group_by(!!factor_var, !!dummy_var) %>%
    dplyr::summarize(Mean = mean(price, na.rm=TRUE),
                     se = sd(price)/sqrt(n()))

  stats[,2] <- lapply(stats[,2], factor)

  ggplot(stats, aes_string(colnames(stats)[1], colnames(stats)[3], fill = colnames(stats)[2]))+
    geom_bar(stat='identity', position = position_dodge(width=0.9), alpha=0.8)+
    geom_errorbar(aes(ymin=Mean-(1.96*se),ymax=Mean+(1.96*se)),
                  position=position_dodge(width = 0.9), width = 0.25)+
    scale_color_manual(name=dummy_lab,
                       values=c("red","cyan3")) +
    scale_fill_manual(name=dummy_lab,
                      values=c("red","cyan3")) +
    ylab('Mean Price')+
    xlab(factor_lab) +
    theme_bw()+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          axis.line=element_line(),
          legend.position = "top",
          #legend.position = c(0.7, 0.9),
          legend.box = "vertical",
          legend.text = element_text(size = 5),
          legend.title = element_text(size = 5, face = "bold"),
          legend.key.size = unit(x = 0.4, units = "cm")
        )
}


###### Interactions ######

# Room Type
p1 <- price_diff_by_variables2(data, "f_room_type", "d_instant_bookable","Room Type", "Instant Bookable") # <--------------
p2 <- price_diff_by_variables2(data, "f_room_type", "d_host_greets_you","Room Type", "Host greets you") # <-------------
p3 <- price_diff_by_variables2(data, "f_room_type", "d_hot_water","Room Type", "Hot Water")
p4 <- price_diff_by_variables2(data, "f_room_type", "d_parking_agg","Room Type", "Parking") # <------------
p5 <- price_diff_by_variables2(data, "f_room_type", "d_refrigerator","Room Type", "Refrigerator")# <-------


# District
p6 <- price_diff_by_variables2(data, "f_District", "d_parking_agg","Number of Reviews", "Parking")  
p7 <- price_diff_by_variables2(data, "f_District", "d_patio_or_balcony","Number of Reviews", "Patio or Balcony")
p8 <- price_diff_by_variables2(data, "f_District", "d_kitchen","n_beds", "Kitchen") 
p9 <- price_diff_by_variables2(data, "f_District", "d_hot_water","Number of Reviews", "Hot Water") 
p10 <- price_diff_by_variables2(data, "f_District", "d_waterfront","Number of Reviews", "Waterfront") # <-------
p11 <- price_diff_by_variables2(data, "f_District", "d_building_staff","Number of Reviews", "Building Staff") # <-------

# dummies suggested by graphs
g_interactions <- plot_grid(p1, p3, p4,
                            p6, p10, p11, nrow=3, ncol=2)

# dummies suggested by graphs
X1  <- c("f_room_type*d_instant_bookable", "f_room_type*d_host_greets_you", "f_room_type*d_parking_agg", "f_room_type*d_refrigerator")
X2 <- c("f_District*d_building_staff", "f_District*d_waterfront")

g_interactions


m1 <- "= District, guests accommodated, log of number of beds, number of bedrooms, average review scores, missing score flag, guests accommodated (squared term), room type, minimum nights, number of reviews, number of bathrooms,"

m2 <- "= M1 + all amenities"

m3 <- "= M2 + amenities interactions"

model_variables <- c(m1,m2,m3)

model_names <- c("M1", "M2", "M3")

model_table <- as.data.frame(cbind(model_names, model_variables))

model_headings <- c("Model", "Predictor Variables")

colnames(model_table) <- model_headings



# Machine Learning

## Building Regression Models

model_table %>%
  kbl(caption = "<center><strong>Versions of the Airbnb Apartment Price Prediction Models</strong></center>", escape = FALSE) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling( position = "center")

## OLS Linear Regression

 model construction,  , , }
# Create models in levels models: 1-3

model1 <- as.formula(paste("price ~ ",paste(c(n_var,poly_var, f_var),collapse = " + ")))
model2 <- as.formula(paste("price ~ ",paste(c(n_var,poly_var, f_var, d_amenities),collapse = " + ")))
model3 <- as.formula(paste("price ~ ",paste(c(n_var,poly_var, f_var, d_amenities, X1, X2),collapse = " + ")))

# Create models in levels models: 1-3

train_indices <- as.integer(createDataPartition(data$price, p = 0.7, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]


# model 1 CV
set.seed(20213001)
cv_model1 <- train(model1, 
                   data = data_train, 
                   method = "lm",
                   trControl = trainControl(method = "cv", number = 5)
)


# model 2 CV
set.seed(20213001)
cv_model2 <- train(
  model2, 
  data = data_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 5)
)

# model 3 CV
set.seed(20213001)
cv_model3 <- train(
  model3, 
  data = data_train, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 5)
)

cv_mod1_pred <- predict(cv_model1, data_train)
cv_mod2_pred <- predict(cv_model2, data_train)
cv_mod3_pred <- predict(cv_model3, data_train)


# Checking coefficients
cv_model1$finalModel # coefficients


# RMSE fold results for all models
model1_rmse <- as.matrix(round(cv_model1$resample$RMSE,3))
model2_rmse <- as.matrix(round(cv_model2$resample$RMSE,3))
model3_rmse <- as.matrix(round(cv_model3$resample$RMSE,3))
mean_rmse <- c(mean(model1_rmse), mean(model2_rmse),mean(model3_rmse))

model_rmse_table <- as.data.frame(cbind(model1_rmse,model2_rmse, model3_rmse))
colnames(model_rmse_table) <- c("Model 1", "Model 2", "Model 3")
model_rmse_table <- rbind(model_rmse_table,mean_rmse)
rownames(model_rmse_table) <- c("Fold 1", "Fold 2", "Fold 3", "Fold 4", "Fold 5", "Average")


#### Comparing Fit measures

model_list <- c(model1,model2,model3)

BIC <- NULL
nvars <- NULL
r2 <- NULL

for(x in model_list){
  model_work_data <- lm(x,data = data_train)
  BIC <- c(BIC,round(BIC(model_work_data)))
  nvars <- c(nvars, model_work_data$rank -1)
  r2 <- c(r2,summary(model_work_data)$r.squared)
}
# Calculate RMSE for training set
rmse_train <- c(mean(cv_model1$resample$RMSE),mean(cv_model2$resample$RMSE), mean(cv_model3$resample$RMSE))

# Calculate RMSE for testing set
rmse_test <- c(rmse(cv_mod1_pred,data_train$price),rmse(cv_mod2_pred,data_train$price), rmse(cv_mod3_pred,data_train$price))

# Bind all the different model results together
model_results <- as.data.frame(cbind(nvars,r2,BIC,rmse_train,rmse_test))

# Convert all numeric columns to numeric data type
model_results <- model_results %>% 
  mutate_if(is.character, numeric)

# Round all numeric columns to 2 digits if applicable
model_results <- model_results %>% 
  mutate_if(is.numeric, round, digits = 2)

# Add model names to the model results table
model_names <- c("Model 1","Model 2","Model 3")
model_results <- cbind(model_names,model_results)

# Create column name list for model results table
column_names <- c("Model", "N predictors", "R-squared", "BIC", "Training RMSE","Test RMSE")
colnames(model_results) <- column_names

#### Holdout set predictions

cv_holdout_pred <- predict(cv_model3, data_holdout)
holdout_rmse <- mean(cv_model3$resample$RMSE) #0.3638667


model_results %>%
  kbl(caption = "<center><strong>Comparing Model Fit measures</strong></center>", escape = FALSE) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling( position = "center")




## Lasso

model4 <- as.formula(paste("price ~ ",paste(c(n_var,poly_var, f_var, d_amenities, X1, X2),collapse = " + ")))

# Set lasso tuning parameters
train_control <- trainControl(
  method = "cv",
  number = 5)

tune_grid <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))

# We use model 7 without the interactions so that it is easy to compare later to post lasso ols
# formula <- formula(paste0("price ~ ", paste(setdiff(vars_model_8, "price"), collapse = " + ")))

set.seed(20213001)
lasso_model <- caret::train(model4,
                            data = data_train,
                            method = "glmnet",
                            preProcess = c("center", "scale"),
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            na.action=na.exclude)

print(lasso_model$bestTune$lambda) #0.3/62.21 RMSE

lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `1`)  # the column has a name "1", to be renamed

print(lasso_coeffs)

# Evaluate model. CV error:
lasso_cv_rmse <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda) %>%
  dplyr::select(RMSE)
print(lasso_cv_rmse[1, 1]) #RMSE 60.178


# Random Forest
m1_rf <- "= District, guests accommodated, number of beds, number of bedrooms, average   review scores, missing score flag, room type,   minimum nights, number of reviews, number of bathrooms,"

m2_rf <- "= M1 + all amenities columns"


model_variables_rf <- c(m1,m2)

model_names_rf <- c("M1", "M2")

model_table_rf <- as.data.frame(cbind(model_names_rf, model_variables_rf))

model_headings_rf <- c("Model", "Predictor Variables")

colnames(model_table_rf) <- model_headings_rf

model_table_rf %>%
  kbl(caption = "<center><strong>Versions of the Airbnb Apartment Price Prediction Models for Random Forest</strong></center>", escape = FALSE) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling( position = "center")

# RANDOM FOREST


# Assign columns to grouped variables for model equations
n_var <- c("n_accommodates", "n_beds", "n_bedrooms", "n_review_scores_rating", "flag_review_scores_rating")
f_var <- c("f_room_type", "f_minimum_nights", "f_number_of_reviews", "f_bathroom")

# Dummy variables: Extras -> collect all options and create dummies
d_amenities <-  grep("^d_.*", names(data), value = TRUE)

rf_formula1 <- as.formula(paste("price ~ ",paste(c(n_var, f_var),collapse = " + ")))
rf_formula2 <- as.formula(paste("price ~ ",paste(c(n_var, f_var, d_amenities),collapse = " + ")))


# Model setup same for both models
# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)


# ----------- RF Model 1----------#

# Model tuning for Model RF model 1
# set tuning
tune_grid_1 <- expand.grid(
  .mtry = c(4, 6, 8), # Coefficients are 9 but since minimum of 4 variables are recommended, we will go with 4 and above as mtry
  .splitrule = "variance",
  .min.node.size = c(2, 4, 6, 8)
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



# ----------- RF Model 2----------#

# Model tuning for Model RF model 2
# set tuning
tune_grid_2 <- expand.grid(
  .mtry = c(7, 9, 11),
  .splitrule = "variance",
  .min.node.size = c(5, 10, 15)
)

# Train Model 1

set.seed(1234)
  rf_model_2 <- train(
    rf_formula2,
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid_2,
    importance = "impurity"
  )

# Show Model B rmse shown with all the combinations
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


rf_tuning_model1_table
rf_tuning_model2_table


results <- resamples(
  list(
    model_1  = rf_model_1,
    model_2  = rf_model_2
  )
)


# RMSE fold results for all models
model1_rf_rmse <- as.matrix(round(results$values$`model_1~RMSE`,3))
model2_rf_rmse <- as.matrix(round(results$values$`model_2~RMSE`,3))
mean_rf_rmse <- c(mean(model1_rf_rmse), mean(model2_rf_rmse))

model_rf_rmse_table <- as.data.frame(cbind(model1_rf_rmse,model2_rf_rmse))
colnames(model_rf_rmse_table) <- c("Model 1", "Model 2")
model_rf_rmse_table <- rbind(model_rf_rmse_table,mean_rf_rmse)
rownames(model_rf_rmse_table) <- c("Fold 1", "Fold 2", "Fold 3", "Fold 4", "Fold 5", "Average")

model_rf_rmse_table %>% kbl(caption = "<center><strong>RMSE fold results for all models</strong></center>", escape = FALSE) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling( position = "center")
  


### Variable Importance

#############################
# Variable Importance Plots 
#############################

# first need a function to calculate grouped varimp
group.importance <- function(rf.obj, groups) {
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(importance(rf.obj)[g], na.rm = TRUE)
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}


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


##############################
# Grouped variable Importance
##############################


varnames <- rf_model_2$finalModel$xNames
f_District_cleansed_varnames <- grep("f_District",varnames, value = TRUE)
f_room_type_varnames <- grep("f_room_type",varnames, value = TRUE)
f_bathroom_varnames <- grep("f_bathroom",varnames, value = TRUE)
f_minimum_nights <- grep("f_minimum_nights",varnames, value = TRUE)
f_minimum_nights_varnames <- grep("f_minimum_nights",varnames, value = TRUE)

groups <- list(f_District_=f_District_cleansed_varnames,
               f_room_type = f_room_type_varnames,
               f_bathroom = f_bathroom_varnames,
               f_room_type = f_room_type_varnames,
               f_bathroom = "f_bathroom",
               n_days_since = "f_minimum_nights",
               f_minimum_nights = "f_minimum_nights",
               n_beds = "n_beds")

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


ggarrange(rf_model_2_var_imp_plot_b,rf_model_2_var_imp_grouped_plot, nrow = 1)

# Partial Dependence

pdp_n_bed <- pdp::partial(rf_model_2, pred.var = "n_beds", pred.grid = distinct_(data_holdout, "n_beds"), train = data_train)
pdp_n_bed_plot <- pdp_n_bed %>%
  autoplot( ) +
  geom_point(color="cyan3", size=2) +
  geom_line(color="cyan3", size=1.5) +
  ylab("Predicted price") +
  xlab("Number of beds") +
  scale_x_continuous(limit=c(1,7), breaks=seq(1,7,1))+
  theme_bw()



pdp_n_roomtype <- pdp::partial(rf_model_2, pred.var = "f_room_type", pred.grid = distinct_(data_holdout, "f_room_type"), train = data_train)
pdp_n_roomtype_plot <- pdp_n_roomtype %>%
  autoplot( ) +
  geom_point(color="cyan3", size=4) +
  ylab("Predicted price") +
  xlab("Room type") +
  #scale_y_continuous(limits=c(60,120), breaks=seq(60,120, by=10)) +
  theme_bw()

ggarrange(pdp_n_bed_plot, pdp_n_roomtype_plot, nrow = 1)

# NOTE  we do this on the holdout set.

# ---- cheaper or more expensive flats - not used in book
data_holdout_w_prediction <- data_holdout %>%
  mutate(predicted_price = predict(rf_model_2, newdata = data_holdout))



######### create nice summary table of heterogeneity
a <- data_holdout_w_prediction %>%
  mutate(is_low_size = ifelse(n_accommodates <= 4, "small apt", "large apt")) %>%
  group_by(is_low_size) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )


b <- data_holdout_w_prediction %>%
  filter(f_District %in% c("Centre", "Nieuw-West", "Noord", "Oost", "West", "Zuid", "Zuidoost")) %>%
  group_by(f_District) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )

c <- data_holdout_w_prediction %>%
  filter(f_room_type %in% c("Entire_apt", "Private_room", "Shared_room")) %>%
  group_by(f_room_type) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )


d <- data_holdout_w_prediction %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )

# Save output
colnames(a) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(b) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(c) <- c("", "RMSE", "Mean price", "RMSE/price")
d<- cbind("All", d)
colnames(d) <- c("", "RMSE", "Mean price", "RMSE/price")

line1 <- c("Room Type", "", "", "")
line2 <- c("Apartment size", "", "", "")
line3 <- c("District", "", "", "")

result_3 <- rbind(line2, a, line1, c, line3, b, d) %>%
  transform(RMSE = as.numeric(RMSE), `Mean price` = as.numeric(`Mean price`),
            `RMSE/price` = as.numeric(`RMSE/price`))

result_3 %>% kbl(caption = "<center><strong>Performance across subsamples</strong></center>", escape = FALSE) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling( position = "center")

final_models <-
  list("OLS Model 1" = cv_model1,
       "OLS Model 2" = cv_model2,
       "OLS Model 3" = cv_model3,
  "LASSO (model w/ interactions)" = lasso_model,
  "Random forest (smaller model)" = rf_model_1,
  "Random forest" = rf_model_2)


results <- resamples(final_models) %>% summary()


# Save output --------------------------------------------------------
# Model selection is carried out on this CV RMSE

result_4 <- imap(final_models, ~{
  round(mean(results$values[[paste0(.y,"~RMSE")]]),3)
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")

result_4 %>% kbl(caption = "<center><strong>Horse Race of Models CV RSME</strong></center>", escape = FALSE) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling( position = "center")




# Link to github:https://github.com/fasihatif/Data-Analysis-1-2-3/tree/master/Data_Analysis_3/Assignment_1_DA3

