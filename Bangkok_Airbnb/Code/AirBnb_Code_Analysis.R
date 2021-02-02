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
library(rpart)
library(ggplot2)

rm(list=ls())

BangkokClean <- read.csv("https://raw.githubusercontent.com/BrunoHelmeczy/Prediction_Projects-CEU_DA3/main/Bangkok_Airbnb/Data/Clean/airbnb_bangkok_cleaned.csv",
                         stringsAsFactors = T)
df <- BangkokClean

df %>% dplyr::select(usd_price) %>% mutate(g100s = round(usd_price,-2)) %>% 
  group_by(g100s) %>% summarize(count = n())

df %>% filter(usd_price >= 500) %>% nrow()

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


df %>% select(matches("^d_|^l_|^flag_")) %>% colnames()
df %>% select(matches("^f_")) %>% colnames()
df %>% select(matches("^usd_")) %>% colnames()
df %>% select(matches("^n_")) %>% select(-matches("_ln|_2")) %>% colnames()

df %>% select(-matches("^f_|^d_|^l_|^flag_|^usd_|_ln|_2|^n_")) %>% colnames()

df  %>% colnames()

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

#### Variable Grouping ####
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
  
  
PriceHist <- df %>% ggplot(aes(x = usd_price)) +
  geom_histogram( color = "red", fill = "navyblue",bins = 50) + theme_tufte() +
  labs(title = "Price frequency distribution", x = "Price",
       y = "Frequency Count") + xlim(0,300)
  
LnPriceHist <- df %>% ggplot(aes(x = usd_price_ln)) +
  geom_histogram( color = "red", fill = "navyblue",bins = 50) + theme_tufte() +
  labs(title = "Logged-Price frequency distribution", x = "Log-Transformed Price",
       y = "Frequency Count")

#### Sample vs Holdout sets ####
set.seed(1)
  train_indices <- as.integer(createDataPartition(df$usd_price_ln, p = 0.7, list = FALSE))
  data_train <- df[train_indices, ]
  data_holdout <- df[-train_indices, ]

# train control is 5 fold cross validation
  train_control <- trainControl(method = "cv",
                                number = 5,
                                verboseIter = FALSE)  

#### OLS ####

colnames(df)
  
 set.seed(1234)
  system.time({
    ols_model <- train(
      formula(paste0("usd_price_ln ~ ", paste0(predictors3, collapse = " + "))),
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

# CART -> Very large & pruned tree

set.seed(1234)
system.time({
  cart_model <- train(
    formula(paste0("usd_price_ln ~ ", paste0(predictors3, collapse = " + "))),
    data = data.frame(sapply(data_train,as.numeric)),
    method = "rpart",
    trControl = train_control,
    
    tuneGrid= expand.grid(cp = 0.0005))
})

fancyRpartPlot(cart_model$finalModel, sub = "")

# take the last model (large tree) and prunce (cut back)
#pfit <-prune(cart_model$finalModel, cp=0.005 )
#summary(pfit)


#### LASSO ####
# using extended model w interactions

set.seed(1234)
system.time({
  lasso_model <- train(
    formula(paste0("usd_price_ln ~ ", paste0(predictorsE, collapse = " + "))),
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneGrid =  expand.grid("alpha" = 1, "lambda" = seq(0.01, 0.25, by = 0.04)),
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
#regression_coeffs %>%
#  write.csv(file = paste0(output, "regression_coeffs.csv"))


  
#### Random Forest ####

  # set tuning
  tune_grid <- expand.grid(
    .mtry = c( 5, 7, 9),
    .splitrule = "variance",
    .min.node.size = c(5, 10)
  )
  
  
  # simpler model for model A (1)
set.seed(1234)
  system.time({
    rf_model_1 <- train(
      formula(paste0("usd_price_ln ~ ", paste0(predictors2, collapse = " + "))),
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
      formula(paste0("usd_price_ln", paste0(" ~ ",paste0(predictors3, collapse = " + ")))),
      data = data_train,
      method = "ranger",
      trControl = train_control,
      tuneGrid = tune_grid,
      importance = "impurity"
    )
  })
  
rf_model_2

set.seed(1234)
system.time({
  rf_model_2_lev <- train(
    formula(paste0("usd_price", paste0(" ~ ",paste0(predictors3, collapse = " + ")))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

rf_model_2_lev
  


  # Tuning parameter choice 1
  result_1 <- matrix(c(
    rf_model_1$finalModel$mtry,
    rf_model_2$finalModel$mtry,
    rf_model_2_lev$finalModel$mtry,
    rf_model_1$finalModel$min.node.size,
    rf_model_2$finalModel$min.node.size,
    rf_model_2_lev$finalModel$min.node.size
    
  ),
  nrow=3, ncol=2,
  dimnames = list(c("Model 1", "Model 2","Model 2 level"),
                  c("Min vars","Min nodes")))
  

# evaluate random forests -------------------------------------------------
  
  
#------------------ Model Comparison ####
  
  final_models <-
    list("OLS" = ols_model,
         "LASSO (model w/ interactions)" = lasso_model,
         "Random forest (smaller)" = rf_model_1,
         "Random forest" = rf_model_2)
  
  results <- resamples(final_models) %>% summary()
  
  
models <- c("ols_model","lasso_model","rf_model_1","rf_model_2")  
Predictions <-data.frame(sapply(models[1:4],function(x) {
  tl <- list()
  model <- eval(parse(text = x))
  tl[[x]] <- predict(model,newdata = data_train)
  res <- tl[[x]] - data_train$usd_price_ln
  StDev <- sd(res)
  tl[[x]] <- exp(tl[[x]]) * exp((StDev^2)/2)
  
  return(tl)
}))

Rsq <- NULL
for (i in 1:4) {
  Rsq[models[i]] <- round(mean(results[[3]][['Rsquared']][i,1:5]),3)
}

SumStatTable <- as.data.frame(cbind(Rsq,rbindlist(lapply(Predictions, function(x) {
  tl <- list()
  tl[['MAE']] <- round(MAE(x,data_train$usd_price),3)
  tl[['RMSE']] <- round(RMSE(x, data_train$usd_price),3)
  tl[['RMSE_norm']] <- round(tl[['RMSE']]/mean(data_train$usd_price),3)
  return(tl)
}))))
rownames(SumStatTable) <- models
SumStatTable


#### Final Model Re-Estimation ####
train_control <- trainControl(method = "none",verboseIter = FALSE)  

# set tuning for final model
tune_grid <- expand.grid(
  .mtry = c(12),
  .splitrule = "variance",
  .min.node.size = c(5)
)

set.seed(1234)
system.time({
  rf_model_final <- train(
    formula(paste0("usd_price_ln", paste0(" ~ ",paste0(predictors3, collapse = " + ")))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

#### Summary of RF_2_Final Model ####
data_holdout_w_prediction <- data_holdout %>%
  mutate(predicted_price_ln = predict(rf_model_final, newdata = data_holdout))

data_holdout_w_prediction <- data_holdout_w_prediction %>% mutate(res = predicted_price_ln - usd_price_ln)
StDev <- sd(data_holdout_w_prediction$res)
data_holdout_w_prediction$predicted_price <- exp(data_holdout_w_prediction$predicted_price_ln) * exp((StDev^2)/2)
Rsq <- rf_model_final$finalModel$r.squared

SumStatsFinalModelTable <- 
  cbind(Rsq,rbindlist(lapply(data_holdout_w_prediction[c("predicted_price")], function(x) {
  tl <- list()
  tl[['MAE']] <- round(MAE(x,data_holdout$usd_price),3)
  tl[['RMSE']] <- round(RMSE(x, data_holdout$usd_price),3)
  tl[['RMSE_norm']] <- round(tl[['RMSE']]/mean(data_holdout$usd_price),3)
  return(tl)
})))

SumStatsFinalModelTable


# MODEL DIAGNOSTICS -------------------------------------------------------
#
#########################################################################################
  
# Pred vs Actual Y Line + Scatter
PredvsAccPlot <- data_holdout_w_prediction %>% ggplot(aes(x = usd_price, y = usd_price)) +
  geom_point(aes(y = predicted_price)) + 
  geom_line() +
  theme_tufte() +
  labs(title = "Actual versus Predicted Prices",
       x = "Actual Prices", y = "Predicted Prices")

PredvsAccPlot

######### create nice summary table of heterogeneity
a <- data_holdout_w_prediction %>%
#  mutate(is_low_size = ifelse(n_accommodates <= 3, "small apt", "large apt")) %>%
  group_by(n_accommodates) %>%
  dplyr::summarise(
    rmse = round(RMSE(predicted_price, usd_price),2),
    mean_price = round(mean(usd_price),2),
    rmse_norm = round(RMSE(predicted_price, usd_price) / mean(usd_price),2))

b <- data_holdout_w_prediction %>%
  filter(f_neighbourhood_cleansed %in% c("Khlong Toei", "Vadhana", "Huai Khwang", "Ratchathewi", "Sathon")) %>%
  group_by(f_neighbourhood_cleansed) %>%
  dplyr::summarise( count = n(),
    rmse = round(RMSE(predicted_price, usd_price),2),
    mean_price = round(mean(usd_price),2),
    rmse_norm = round(RMSE(predicted_price, usd_price) / mean(usd_price),2)) %>% 
  arrange(desc(count)) %>% select(-count)

c <- data_holdout_w_prediction %>%
  filter(f_property_type %in% c("apartment","condominium")) %>% 
  group_by(f_property_type) %>%
  dplyr::summarise(
    rmse = round(RMSE(predicted_price, usd_price),2),
    mean_price = round(mean(usd_price),2),
    rmse_norm = round(RMSE(predicted_price, usd_price) / mean(usd_price),2))


d <- data_holdout_w_prediction %>%
  dplyr::summarise(
    rmse = round(RMSE(predicted_price, usd_price),2),
    mean_price = round(mean(usd_price),2),
    rmse_norm = round(RMSE(predicted_price, usd_price) / mean(usd_price),2)  )

# Save output
colnames(a) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(b) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(c) <- c("", "RMSE", "Mean price", "RMSE/price")
d<- cbind("All", d)
colnames(d) <- c("", "RMSE", "Mean price", "RMSE/price")

line1 <- c("Type", " ", " ", " ")
line2 <- c("Apartment size", " ", " ", " ")
line3 <- c("Neighbourhood", " ", " ", " ")

result_3 <- rbind(line2, a, line1, c, line3, b, d) %>%
  transform(RMSE = as.numeric(RMSE), `Mean price` = as.numeric(`Mean price`),
            `RMSE/price` = as.numeric(`RMSE/price`))

kable(result_3)


# FIGURES FOR FITTED VS ACTUAL OUTCOME VARIABLES #
###################################################

# Target variable
Ylev <- data_holdout_w_prediction[["predicted_price"]]
meanY <-mean(Ylev)
sdY <- sd(Ylev)

# Predicted values
predictionlev_holdout_pred <- data_holdout_w_prediction %>% select(predicted_price) %>%  #%>%
  mutate(pred_lwr = predicted_price - sdY, 
         pred_upr = predicted_price + sdY)


predictionlev_holdout <- cbind(data_holdout[,c("usd_price","n_accommodates")],
                               predictionlev_holdout_pred) %>% 
  mutate(fit = abs(predicted_price - usd_price))

predictionlev_holdout_summary <-
  predictionlev_holdout %>% 
  group_by(n_accommodates) %>%
  dplyr::summarise(
    fit = mean(predicted_price, na.rm = T),
    #    res = mean(fit, na.rm=TRUE),
    pred_lwr = mean(pred_lwr, na.rm=TRUE),
    pred_upr = mean(pred_upr, na.rm=TRUE))

F14_CI_n_accomodate <- ggplot(predictionlev_holdout_summary, aes(x=factor(n_accommodates))) +
  geom_bar(aes(y = fit ), stat="identity",  fill = "blue", alpha=0.7 ) +
  geom_errorbar(aes(ymin=pred_lwr, ymax=pred_upr, color = "Pred. interval"),width=.2) +
  #geom_errorbar(aes(ymin=conf_lwr, ymax=conf_upr, color = "Conf. interval"),width=.2) +
  scale_y_continuous(name = "Predicted price (US dollars)") +
  scale_x_discrete(name = "Accomodates (Persons)") +
  scale_color_manual(values=c("green", "green")) +
  theme_tufte() +
  theme(legend.title= element_blank(),legend.position="none")
F14_CI_n_accomodate


#########################################################################################
# Variable Importance Plots -------------------------------------------------------
#########################################################################################
# first need a function to calculate grouped varimp
group.importance <- function(rf.obj, groups) {
    var.imp <- as.matrix(sapply(groups, function(g) {
      sum(importance(rf.obj)[g], na.rm = TRUE)
    }))
    colnames(var.imp) <- "MeanDecreaseGini"
    return(var.imp)
}
  # variable importance plot
  # 1) full varimp plot, full
  # 2) varimp plot grouped
  # 3) varimp plot , top 20 Amenities

  rf_model_final_var_imp <- importance(rf_model_final$finalModel)/1000
  rf_model_final_var_imp_df <-
    data.frame(varname = names(rf_model_final_var_imp),imp = rf_model_final_var_imp) %>%
    arrange(desc(imp)) %>%
    mutate(imp_percentage = imp/sum(imp))
  
  
  ##############################
  # 1) full varimp plot, above a cutoff
  ##############################
  
  
  cutoff = 0.05
  rf_model_final_var_imp_plot <- ggplot(rf_model_final_var_imp_df[rf_model_final_var_imp_df$imp>cutoff,],
                                    aes(x=reorder(varname, imp), y=imp_percentage)) +
    geom_point(color="blue", size=1.5) +
    geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color="blue", size=1) +
    ylab("Importance (Percent)") +
    xlab("Variable Name") +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_tufte() +
    theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6),
          axis.title.x = element_text(size=6), axis.title.y = element_text(size=6))
  rf_model_final_var_imp_plot
  
  ##############################
  # 2) varimp plot grouped
  ##############################
  # grouped variable importance - keep binaries created off factors together
  
  varnames <- rf_model_2$finalModel$xNames
  f_neighbourhood_cleansed_varnames <- grep("f_neighbourhood_cleansed",varnames, value = TRUE)
  f_property_type_varnames <- grep("f_property_type",varnames, value = TRUE)
  n_accommodates_varnames <- grep("n_accommodates",varnames,value = T)
  amenities_varnames <- grep("^d_",varnames,value = T)
  n_days_since_varnames <- grep("n_days",varnames,value = T)
  host_varnames <- grep("host",varnames,value = T)
  f_sales_varnames <- grep("sales|^l_has|l_ins",varnames,value = T)
  f_beds <- grep("beds",varnames,value = T)
  f_bedrooms <- grep("bedrooms",varnames,value = T)
  f_bathrooms <- grep("bathrooms",varnames,value = T)
  n_reviews <- grep("review",varnames,value = T)
  min_nights <- grep("nights",varnames,value = T)
  
  groups <- list(
    Neighbourhood       = f_neighbourhood_cleansed_varnames,
    Property_type       = f_property_type_varnames,
    n_accommodates      = n_accommodates_varnames,
    amenities           = amenities_varnames,
    Sales_Availability  = f_sales_varnames,
    Nr_Beds             = f_beds,
    Bedrooms            = f_bedrooms,
    Host_Exp_n_Recent   = n_days_since_varnames,
    Host_information    = host_varnames,
    Nr_Bathrooms        = f_bathrooms,
    Review_Scores       = n_reviews,
    Stay_Restrictions   = min_nights)
    
  rf_model_final_var_imp_grouped <- group.importance(rf_model_final$finalModel, groups)
  rf_model_final_var_imp_grouped_df <- data.frame(varname = rownames(rf_model_final_var_imp_grouped),
                                              imp = rf_model_final_var_imp_grouped[,1])  %>%
    mutate(imp_percentage = imp/sum(imp))
  
  rf_model_final_var_imp_grouped_plot <-
    ggplot(rf_model_final_var_imp_grouped_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
    geom_point(color="blue", size=1) +
    geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color="blue", size=0.7) +
    ylab("Importance (Percent)") +   xlab("Variable Name") +
    coord_flip() +
    # expand=c(0,0),
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_tufte() +
    theme(axis.text.x = element_text(size=6), 
          axis.text.y = element_text(size=8),
          axis.title.x = element_text(size=8), axis.title.y = element_text(size=4))
  rf_model_final_var_imp_grouped_plot
  
##########################################
  # 3) full varimp plot, top 20 amenities
##########################################
  
  
  OnlyAmens <-  rf_model_final_var_imp_df[rf_model_final_var_imp_df$varname %in% 
                                        grep("^d_",rf_model_final_var_imp_df$varname, value = T),]
  
  # have a version with top 10 vars only -> only Amenities
  rf_model_final_var_imp_plot_b <- ggplot(OnlyAmens[1:20,], aes(x=reorder(varname, imp), y=imp_percentage)) +
    geom_point(color="blue", size=1) +
    geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color="blue", size=0.75) +
    labs(y = "Importance (Percent)", x = "Variable Name",
         title = "Amenities' Importance for Price prediction") +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_tufte() +
    theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6),
          axis.title.x = element_text(size=6), axis.title.y = element_text(size=4),
          title = element_text(size = 10))
  rf_model_final_var_imp_plot_b
  
