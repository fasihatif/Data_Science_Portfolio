# Cleaning London airbnb file
# v.1.2. 2021-01-04 paths changed


# IN data from web
# out: airbnb_london_cleaned.csv

#setting working directory
rm(list=ls())

# CHANGE TO YOUR WORKING DIRECTORY
# setwd("")
# setwd("C:/Users/bekes.gabor/Dropbox (MTA KRTK)/bekes_kezdi_textbook/")

#location folders
# data_in  <- "~/Data_Analysis_3/da_data_repo/airbnb/raw/"
data_out <- "~/Data_Analysis_3/Assignment_1_DA3/airbnb_london/"

library(tidyverse)

data <- listings
rm(listings)
# zero step
# not necessary
# data<-read.csv(paste0(data_in,"listings.csv"))
drops <- c("host_thumbnail_url","host_picture_url","listing_url","thumbnail_url","medium_url","picture_url","xl_picture_url","host_url","last_scraped","description", "experiences_offered", "neighborhood_overview", "notes", "transit", "access", "interaction", "house_rules", "host_about", "host_response_time", "name", "summary", "space", "host_location")
data<-data[ , !(names(data) %in% drops)]
write.csv(data,file=paste0(data_in,"airbnb_london_listing.csv"))


#####################################

# opening dataset
df<-read.csv(paste0(data_in,"airbnb_london_listing.csv"),
             sep=",",header = TRUE, stringsAsFactors = FALSE)
         
df <- data     
#drop broken lines - where id is not a character of numbers
df$junk<-grepl("[[:alpha:]]", df$id)
df<-subset(df,df$junk==FALSE)
df<-df[1:ncol(df)-1]

#display the class and type of each columns
sapply(df, class)
sapply(df, typeof)

#####################
#formatting columns

#remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
  df[[perc]]<-gsub("%","",as.character(df[[perc]]))
}

#remove dollar signs from price variables
for (pricevars in c("price", "weekly_price","monthly_price","security_deposit","cleaning_fee","extra_people")){
  df[[pricevars]]<-gsub("\\$","",as.character(df[[pricevars]]))
  df[[pricevars]]<-as.numeric(as.character(df[[pricevars]]))
}

#format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified","is_location_exact","requires_license","instant_bookable","require_guest_profile_picture","require_guest_phone_verification")){
  df[[binary]][df[[binary]]=="f"] <- 0
  df[[binary]][df[[binary]]=="t"] <- 1
}

#amenities
df$amenities<-gsub("\\{","",df$amenities)
df$amenities<-gsub("\\}","",df$amenities)
df$amenities<-gsub('\\"',"",df$amenities)
df$amenities<-as.list(strsplit(df$amenities, ","))

#define levels and dummies 
levs <- levels(factor(unlist(df$amenities)))
df<-cbind(df,as.data.frame(do.call(rbind, lapply(lapply(df$amenities, factor, levs), table))))

drops <- c("amenities","translation missing: en.hosting_amenity_49",
           "translation missing: en.hosting_amenity_50")
df<-df[ , !(names(df) %in% drops)]

# MINOR STUFF
# data changed marginally, to make it compatible with textbook, we'll drop 27 rows. 
not_in_book <- read.csv(paste0(data_in,"not_in_book.csv"), header=TRUE, row.names = 1)
df<-df %>%
  left_join(not_in_book, by="id")%>%
  filter(is.na(not_in_book))%>%
  select(-not_in_book)
  
#write csv
write.csv(df,file=paste0(data_out,"airbnb_london_cleaned.csv"))

rm(listings,data)
