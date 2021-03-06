
# Import libraries
library(tidyverse)
data <- cs_bisnode_panel

#0.2 | Filter balance sheet length and years
#-----------------------------------------------------
data <- data %>% filter(balsheet_length > 360) #assuming it as a complete year
data <- data %>% filter(year == 2013 | year == 2014)


#0.3 | Create status_alive variable to check if company existed or not
#---------------------------------------------------------------------
data  <- data %>%
  mutate(status_alive = sales > 0 & !is.na(sales) %>%
           as.numeric(.))


#0.4 | Take log of sales and sales_mil
#---------------------------------------
data <- data %>%
  mutate(sales = ifelse(sales < 0, 1, sales),
         ln_sales = ifelse(sales > 0, log(sales), 0),
         sales_mil=sales/1000000,
         sales_mil_log = ifelse(sales > 0, log(sales_mil), 0))

data <- data %>%
  group_by(comp_id) %>%
  mutate(d1_sales_mil_log = sales_mil_log - lag(sales_mil_log, 1) ) %>%
  ungroup()


# 0.5 | Replace w 0 for new firms + add dummy to capture it
#------------------------------------------------------------
data <- data %>%
  mutate(age = (year - founded_year) %>%
           ifelse(. < 0, 0, .),
         new = as.numeric(age <= 1) %>% #  (age could be 0,1 )
           ifelse(balsheet_notfullyear == 1, 1, .),
         d1_sales_mil_log = ifelse(new == 1, 0, d1_sales_mil_log),
         new = ifelse(is.na(d1_sales_mil_log), 1, new),
         d1_sales_mil_log = ifelse(is.na(d1_sales_mil_log), 0, d1_sales_mil_log))


#0.5 | Filter for status = alive and sales mil > 10 & < 0.001
#--------------------------------------------------------------
data <- data %>%
  filter(status_alive == 1) %>%
  filter(!(sales_mil > 10)) %>%
  filter(!(sales_mil < 0.001))


#0.6 | Filter company ids with only 2 rows
#------------------------------------------
data <- data %>% group_by(comp_id) %>% filter(n() == 2)


#0.7 | Calculate percentage change of sales_mil
#-----------------------------------------------
data <- data %>% group_by(comp_id)
# data <- data %>% mutate(pct_change = (sales_mil_log-lag(sales_mil_log))/lag(sales_mil_log))
data <- data %>% mutate(pct_change = (ln_sales - lag(ln_sales))/lag(ln_sales))


#0.8 | Filter for year 2014
#---------------------------
data <- data %>%
  filter(year == 2014)


#0.9 | Drop unnecessary columns
#-------------------------------
data$COGS <- NULL
data$finished_prod <- NULL
data$net_dom_sales <- NULL
data$net_exp_sales <- NULL
data$wages <- NULL
data$status_alive <- NULL
data$exit_year <- NULL
data$exit_date <- NULL
data$D <- NULL
data$balsheet_flag <- NULL
data$balsheet_length <- NULL
data$balsheet_notfullyear <- NULL

#0.10 | create age variable
#---------------------------
data <- data %>%
  mutate(age = (year - founded_year))


#0.11 | Create the Y variable
#-----------------------------
data <- data %>% mutate(comp_growth = ifelse(pct_change > 0.15,1,0))

#summary(data$pct_change)
#table(data$comp_growth)


# 1.0                            FEATURE ENGINEERING
#------------------------------------------------------------------------------#

#1.1 |  Change industry codes
#--------------------------
data <- data %>%
  mutate(ind2_cat = ind2 %>%
           ifelse(. > 56, 60, .)  %>%
           ifelse(. < 26, 20, .) %>%
           ifelse(. < 55 & . > 35, 40, .) %>%
           ifelse(. == 31, 30, .) %>%
           ifelse(is.na(.), 99, .)
  )


#1.2 | Firm characteristics
#--------------------------
data <- data %>%
  mutate(age2 = age^2,
         foreign_management = as.numeric(foreign >= 0.5),
         gender_m = factor(gender, levels = c("female", "male", "mix")),
         m_region_loc = factor(region_m, levels = c("Central", "East", "West")))


#1.3 |  Assets can't be negative. Change them to 0 and add a flag
#------------------------------------------------------------------
data <-data  %>%
  mutate(flag_asset_problem=ifelse(intang_assets<0 | curr_assets<0 | fixed_assets<0,1,0  ))
table(data$flag_asset_problem)

data <- data %>%
  mutate(intang_assets = ifelse(intang_assets < 0, 0, intang_assets),
         curr_assets = ifelse(curr_assets < 0, 0, curr_assets),
         fixed_assets = ifelse(fixed_assets < 0, 0, fixed_assets))


#1.4 | Generate total assets
#-----------------------------
data <- data %>%
  mutate(total_assets_bs = intang_assets + curr_assets + fixed_assets)
summary(data$total_assets_bs)


#1.4 | Create ratios
#---------------------
pl_names <- c("extra_exp","extra_inc",  "extra_profit_loss", "inc_bef_tax" ,"inventories",
              "material_exp", "profit_loss_year", "personnel_exp")
bs_names <- c("intang_assets", "curr_liab", "fixed_assets", "liq_assets", "curr_assets",
              "share_eq", "subscribed_cap", "tang_assets")

# divide all pl_names elements by sales and create new column for it
data <- data %>%
  mutate_at(vars(pl_names), funs("pl"=./sales))

# divide all bs_names elements by total_assets_bs and create new column for it
data <- data %>%
  mutate_at(vars(bs_names), funs("bs"=ifelse(total_assets_bs == 0, 0, ./total_assets_bs)))


#1.5 | Creating flags, and winsorizing tails
#---------------------------------------------
# Variables that represent accounting items that cannot be negative (e.g. materials)
zero <-  c("extra_exp_pl", "extra_inc_pl", "inventories_pl", "material_exp_pl", "personnel_exp_pl",
           "curr_liab_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs", "subscribed_cap_bs",
           "intang_assets_bs")

data <- data %>%
  mutate_at(vars(zero), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(zero), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(zero), funs("flag_error"= as.numeric(.< 0))) %>%
  mutate_at(vars(zero), funs(ifelse(.< 0, 0, .)))


# for vars that could be any, but are mostly between -1 and 1
any <-  c("extra_profit_loss_pl", "inc_bef_tax_pl", "profit_loss_year_pl", "share_eq_bs")

data <- data %>%
  mutate_at(vars(any), funs("flag_low"= as.numeric(.< -1))) %>%
  mutate_at(vars(any), funs(ifelse(.< -1, -1, .))) %>%
  mutate_at(vars(any), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(any), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(any), funs("flag_zero"= as.numeric(.== 0))) %>%
  mutate_at(vars(any), funs("quad"= .^2))


# dropping flags with no variation
variances<- data %>%
  select(contains("flag")) %>%
  apply(2, var, na.rm = TRUE) == 0

data <- data %>%
  select(-one_of(names(variances)[variances]))


#1.6 | Imputation of some columns
#----------------------------------
# CEO age
data <- data %>%
  mutate(ceo_age = year-birth_year,
         flag_low_ceo_age = as.numeric(ceo_age < 25 & !is.na(ceo_age)),
         flag_high_ceo_age = as.numeric(ceo_age > 75 & !is.na(ceo_age)),
         flag_miss_ceo_age = as.numeric(is.na(ceo_age)))

data <- data %>%
  mutate(ceo_age = ifelse(ceo_age < 25, 25, ceo_age) %>%
           ifelse(. > 75, 75, .) %>%
           ifelse(is.na(.), mean(., na.rm = TRUE), .),
         ceo_young = as.numeric(ceo_age < 40))

# number emp, very noisy measure
data <- data %>%
  dplyr::mutate(labor_avg_mod = ifelse(is.na(labor_avg), mean(labor_avg, na.rm = TRUE), as.numeric(labor_avg)),
                flag_miss_labor_avg = as.numeric(is.na(labor_avg)))

data$labor_avg_mod[is.na(data$labor_avg_mod)]<-mean(data$labor_avg_mod,na.rm=TRUE)


data <- data %>%
  select(-labor_avg)

# create factors
data <- data %>%
  mutate(urban_m = factor(urban_m, levels = c(1,2,3)),
         ind2_cat = factor(ind2_cat, levels = sort(unique(data$ind2_cat))))

data <- data %>%
  mutate(comp_growth_f = factor(comp_growth, levels = c(0,1)) %>%
           recode(., `0` = 'slow', `1` = "fast"))

data <- data %>%
  mutate(sales_mil_log_sq=sales_mil_log^2)

# generate variables ---------------------------------------------------

data <- data %>%
  mutate(flag_low_d1_sales_mil_log = ifelse(d1_sales_mil_log < -1.5, 1, 0),
         flag_high_d1_sales_mil_log = ifelse(d1_sales_mil_log > 1.5, 1, 0),
         d1_sales_mil_log_mod = ifelse(d1_sales_mil_log < -1.5, -1.5,
                                       ifelse(d1_sales_mil_log > 1.5, 1.5, d1_sales_mil_log)),
         d1_sales_mil_log_mod_sq = d1_sales_mil_log_mod^2
  )



#1.6 | Sales Change
#-------------------

# no more imputation, drop obs if key vars missing
data <- data %>%
  filter(!is.na(liq_assets_bs),!is.na(foreign), !is.na(ind))

# drop missing
data <- data %>%
  filter(!is.na(age),!is.na(foreign), !is.na(material_exp_pl), !is.na(m_region_loc))
Hmisc::describe(data$age)

# drop unused factor levels
data <- data %>%
  mutate_at(vars(colnames(data)[sapply(data, is.factor)]), funs(fct_drop))

data_out <- "C:/Users/abc/OneDrive/Business_Analytics/Data-Analysis-1-2-3/Data_Analysis_3/Assignment_2_DA3/data/"



write_csv(data,paste0(data_out,"bisnode_firms_clean.csv"))
write_rds(data,paste0(data_out,"bisnode_firms_clean.rds"))

na_count <- sapply(data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
