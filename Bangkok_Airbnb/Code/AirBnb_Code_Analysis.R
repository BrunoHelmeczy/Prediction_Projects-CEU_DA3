library(stargazer)
library(Hmisc)
library(readr)
library(rattle)
library(tidyverse)
library(caret)
library(ranger)
library(Hmisc)
library(knitr)
library(kableExtra)
library(xtable)
library(data.table)
library(stringr)
library(dplyr) 
library(ggthemes)

rm(list=ls())

BangkokClean <- read.csv("https://raw.githubusercontent.com/BrunoHelmeczy/Prediction_Projects-CEU_DA3/main/Bangkok_Airbnb/Data/Clean/airbnb_bangkok_cleaned.csv",
                         stringsAsFactors = T)
df <- BangkokClean

data.frame("Nr.Col"=sapply(df,class))

#### 1) Plot var distributions en-mass ####
    # Colnames selection:  df %>% select(-matches("^d_|^l_")
    # Dummies -> seperate "Race"-style plot - as for DA2-Ass2
    # rest: for loop - 
      #   continuous -> histogram
      #   Neighborhoods -> invert axes -> geom bar + geom_point(x = sum()/nrow(df))


allplots <- for(i in df %>% colnames()){
  plt <- ggplot(df, aes_string(x=i)) +
    geom_bar() + theme_tufte() + labs(title = paste0("Frequency Distribution of ",i) )
  print(plt)
#  Sys.sleep(0.1)
}



#### Modelling ####

#### 1) Prep ####
  
#### Re-set factors ####

#sapply(df %>% select(matches("^f_")),factor)
df <- df %>% mutate(
  f_property_type = factor(f_property_type),
  f_neighbourhood_cleansed= factor(f_neighbourhood_cleansed),
  f_has_1_review_monthly= factor(f_has_1_review_monthly),
  f_review_scores_rating= factor(f_review_scores_rating),
  f_number_of_reviews= factor(f_number_of_reviews),
  f_sales_365 = factor(f_sales_365),
  f_sales_90 = factor(f_sales_90),
  f_sales_60 = factor(f_sales_60),
  f_sales_30 = factor(f_sales_30),
  f_min_nights = factor(f_min_nights),
  f_beds = factor(f_beds),
  f_bedrooms = factor(f_bedrooms),
  f_bathrooms = factor(f_bathrooms),
  f_host_listing = factor(f_host_listing),
  f_host_accepts_all = factor(f_host_accepts_all)
)


df %>% select(matches(".*host.*")) %>%  colnames()

df %>% select(-matches(".*host.*|.*review.*|^d_|n_days|^usd")) %>% colnames()

#### Variable Grouping ####
  Target <- 'usd_price' # usd_price_ln -> review transforming back from ln predicted

  Prop_vars
  
  Host_vars <- df %>% select(matches(".*host.*")) %>%  colnames()
  
  Review_vars <- df %>% select(matches(".*review.*|n_days")) %>%  colnames()
  
  Amenities_vars <- df %>% select(matches("^d_")) %>%  colnames()


#### ####



#### ####


#### ####


#### ####


#### ####


