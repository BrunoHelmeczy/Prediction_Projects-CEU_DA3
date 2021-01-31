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

  Prop_vars <- df %>% select(-matches(".*host.*|.*review.*|^d_|n_days|^usd")) %>% colnames()
  Host_vars <- df %>% select(matches(".*host.*")) %>%  colnames()
  Review_vars <- df %>% select(matches(".*review.*|n_days")) %>%  colnames()
  Amenities_vars <- df %>% select(matches("^d_")) %>%  colnames()

# Interactions: Amenities + Props4Interactions
  #  Props4Interactions 
  Props4Interactions <- c(df[Prop_vars] %>% select(matches("^f_")) %>% colnames(),"n_accommodates") 

  Interactions <- paste0(paste0("( ",paste0(Props4Interactions, collapse = " + ")," )")
                         ," * ",
                         paste0("( ",paste0(Amenities_vars, collapse = " + ")," )"))
  
#### Create Predictors ####
  predictors1 <- Prop_vars
  predictors2 <- c(Prop_vars,Host_vars,Review_vars)
  predictors3 <- c(Prop_vars,Host_vars,Review_vars,Amenities_vars)
  predictorsE <- c(Prop_vars,Host_vars,Review_vars,Amenities_vars,Interactions)

#### Sample vs Holdout sets ####
set.seed(1)
  train_indices <- as.integer(createDataPartition(df$usd_price, p = 0.7, list = FALSE))
  data_train <- df[train_indices, ]
  data_holdout <- df[-train_indices, ]
  
# train control is 5 fold cross validation
  train_control <- trainControl(method = "cv",
                                number = 5,
                                verboseIter = FALSE)  

#### OLS ####

# OLS with dummies for area
# using model B
  
 set.seed(1234)
  system.time({
    ols_model <- train(
      formula(paste0("usd_price ~ ", paste0(predictors3, collapse = " + "))),
      data = data_train,
      method = "lm",
      trControl = train_control
    )
  })
  
  ols_model_coeffs <-  ols_model$finalModel$coefficients
ols_model_coeffs_df <- data.frame(
    "variable" = names(ols_model_coeffs),
    "ols_coefficient" = ols_model_coeffs
  ) %>%
    mutate(variable = gsub("`","",variable))

#### CART ####

  
  

#### LASSO ####

# using extended model w interactions

set.seed(1234)
system.time({
  lasso_model <- train(
    formula(paste0("usd_price ~ ", paste0(predictorsE, collapse = " + "))),
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneGrid =  expand.grid("alpha" = 1, "lambda" = seq(0.01, 0.25, by = 0.01)),
    trControl = train_control
  )
})

lasso_coeffs <- coef(
  lasso_model$finalModel,
  lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(lasso_coefficient = `1`)  # the column has a name "1", to be renamed

lasso_coeffs_non_null <-as.data.frame(lasso_coeffs[!lasso_coeffs$lasso_coefficient == 0,]) %>%
  arrange(desc(abs(lasso_coefficient)))

lasso_coeffs_non_null <- lasso_coeffs[!lasso_coeffs$lasso_coefficient == 0,]

regression_coeffs <- merge(lasso_coeffs_non_null, ols_model_coeffs_df,  by = "variable", all=TRUE)
regression_coeffs %>%
  write.csv(file = paste0(output, "regression_coeffs.csv"))

  
  
  
  
  
#### Random Forest ####

  # set tuning
  tune_grid <- expand.grid(
    .mtry = c(5, 7, 9),
    .splitrule = "variance",
    .min.node.size = c(5, 10)
  )
  
  
  # simpler model for model A (1)
set.seed(1234)
  system.time({
    rf_model_1 <- train(
      formula(paste0("price ~", paste0(predictors_1, collapse = " + "))),
      data = data_train,
      method = "ranger",
      trControl = train_control,
      tuneGrid = tune_grid,
      importance = "impurity"
    )
  })
  rf_model_1
  
  # set tuning for benchmark model (2)
  tune_grid <- expand.grid(
    .mtry = c(8, 10, 12),
    .splitrule = "variance",
    .min.node.size = c(5, 10, 15)
  )
  
set.seed(1234)
  system.time({
    rf_model_2 <- train(
      formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
      data = data_train,
      method = "ranger",
      trControl = train_control,
      tuneGrid = tune_grid,
      importance = "impurity"
    )
  })
  
rf_model_2
  
# Turning parameter choice 1
  result_1 <- matrix(c(
    rf_model_1$finalModel$mtry,
    rf_model_2$finalModel$mtry,
    rf_model_1$finalModel$min.node.size,
    rf_model_2$finalModel$min.node.size,
  ),
  nrow=3, ncol=2,
  dimnames = list(c("Model A", "Model B","Model B auto"),
                  c("Min vars","Min nodes"))
  )  
  
  
  
    
#### GBM ####

