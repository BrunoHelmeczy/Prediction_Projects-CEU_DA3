getwd()
dir <- paste0(getwd(),"/da_data_repo")

#location folders
data_in <- paste0(dir,"/hotels_europe/raw/")
data_out <- paste0(dir,"/hotels_vienna/raw/")

# PACKAGES
library(caret)
library(dplyr)
library(readr)
library(data.table)
library(ggthemes)
library(ggplot2)
library(ggridges)
library(moments)
library(rattle)


# Load Data
df <- read_csv(paste0(data_in,"hotelbookingdata.csv"))

# filter Time Period & Location -> Christmas Time in Vienna
df <- df[df$year == 2017 & df$month == 12 & df$holiday == 1,]
df <- df[df$s_city == "Vienna",]

# Check for singular value columns -> Remove
ColsUniques <- rbindlist(lapply(1:length(df), function(x) {
  tl <- list()
  tl[['name']] <- colnames(df)[x]
  tl[['distinct']] <- nrow(unique(df[,x]))
  return(tl)
}))

df[,which(ColsUniques$distinct == 1)] <- NULL

# Removed columns:
ColsUniques$name[ColsUniques$distinct == 1]

# From hotels-vienna_cleaning.R -------------
  # 1) Distance to centre -> String -> Numeric 
  # 2) Accomtype -> remove: "_ACCOM_TYPE@"
  # 3) Ratings (guestreviewrating) -> remove: "/5" -> String -> Numeric
  # 4) Check Stars
  # +1) Nr. Nights -> 

df <- df %>% mutate(
  center1distance    = as.numeric(gsub("[^0-9\\.]","",center1distance)),     # 1)
  center2distance    = as.numeric(gsub("[^0-9\\.]","",center2distance)),     # 1)
  accommodationtype  = gsub("_ACCOM_TYPE@","",accommodationtype),            # 2)
  guestreviewsrating = as.numeric(trimws(gsub("/5","",guestreviewsrating))), # 3)
  Nrnights           = as.numeric(gsub("[^0-9\\]","",price_night))) %>%      # +1)
  mutate(
  pricepernight      = ifelse(Nrnights > 1, round(price/Nrnights,0) ,price))

df$price_night <- NULL

# ++1) Columns with NAs -> reviews -> zero review count == no rating
df %>% select(matches("rating")) %>% mutate(NAs = rowSums(is.na(.))) %>% 
  group_by(NAs) %>% summarize(Count = n())

# Zero Review count = NA -> Impute 0
# Review scores -> No reviews -> Mean / Median + Flagging variables
df <- df %>% mutate(
  rating2_ta_reviewcount = ifelse(is.na(rating2_ta_reviewcount),0,rating2_ta_reviewcount),
  rating_reviewcount = ifelse(is.na(rating_reviewcount), 0,rating_reviewcount ))

df <- df %>% mutate(
  rating2_ta_flag = ifelse(is.na(rating2_ta),1,0),
  guestreviewsrating_flag = ifelse(is.na(guestreviewsrating),1,0)) %>% 
  mutate(rating2_ta = ifelse(is.na(rating2_ta),median(rating2_ta,na.rm = T),rating2_ta),
         guestreviewsrating = ifelse(is.na(guestreviewsrating),
                                     median(guestreviewsrating, na.rm = T),
                                     guestreviewsrating))


# Remove Extreme Values from pricepernight - 97.6% obs < 500 -> good enough
price50s <- df %>% mutate(price50s = floor(pricepernight/50)*50) %>% 
  group_by(price50s) %>% 
  dplyr::summarize(count = n()) %>% 
  mutate(runningtotal = cumsum(count)/sum(count)) %>% 
  arrange(desc(price50s))


df <- df %>% filter(pricepernight <= 400)

# Plot Variable Distributions & Frequency tables ----
Hists <- lapply(df %>% select(-matches("id")) %>% colnames(),function(x) {
  if(is.numeric(df[,x][[1]]) ) {
    df %>% ggplot(aes_string(x = x)) + 
      geom_histogram( color = "red", fill = "blue") + theme_tufte() + 
      labs(title = paste0("Vienna Hotels ",x," Distribution")) 
  } else if (is.character(df[,x][[1]])) {
    df %>% ggplot(aes_string(x = x ) ) + 
      geom_bar(color = "red", fill = "blue") + coord_flip() + theme_tufte() + 
      labs(title = paste0("Vienna Hotels ",x," Distribution"))  
  }
})

# Summary table -----
summstats <- c("min","median","mean","max","sd","skewness")
NrCols <- c("price","pricepernight","Nrnights","starrating","center1distance","center2distance",
             "rating2_ta","rating2_ta_reviewcount","guestreviewsrating")

summtable <- list()
table <- data.frame()
for (i in summstats) {
  summtable[[i]] <- cbind(mapply( eval(parse(text = i)), df[,NrCols]))
  if (length(table) == 0) {
    table <- cbind(summtable[[i]])
  } else {table <- cbind(table, summtable[[i]])}
}
colnames(table) <- summstats
table <- round(table,1)
Vars <- rownames(table)
rownames(table) <- NULL
table <- as.data.frame(cbind(Vars,table))

# Check 4 Transformations & Dummy Feature Engineering -----
  # Log: 
    # Pricepernight (y) 
    # rating2_ta_reviewcount
    # center2distance
    # center1distance
    # rating_reviewcount
  # Exp: 
    # guestreviewsrating 
  # Dummy tables: 
    # Accommodationtype
    # Neighbourhood
  
# EDA Avg Price by Accommodation type
df %>% group_by(accommodationtype) %>% 
  dplyr::summarize(AvgPrice = mean(pricepernight),
            NrObs = n()) %>% 
  arrange(desc(AvgPrice))

df %>% group_by(neighbourhood) %>% 
  dplyr::summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  mutate(runningtotal = cumsum(count),
         percent = count/sum(count),
         Cum_percent = cumsum(count)/sum(count) )


df <- df %>% mutate(
  pricepernight_ln          = log(pricepernight),
  rating2_ta_reviewcount_ln = log(rating2_ta_reviewcount+1),
  center2distance_ln        = log(center2distance+1),
  center1distance_ln        = log(center1distance+1),
  rating_reviewcount_ln     = log(rating_reviewcount+1),
  guestreviewsrating_exp    = exp(guestreviewsrating))


Boxes_Scatters <- lapply(df %>% select(-matches("id|pricepernight|flag")) %>% 
                           select(-price) %>% colnames(),function(x) {

  if (is.character(df[,x][[1]]) | x %in% c("starrating", "rating2_ta","Nrnights")) {
    plot <- df %>% ggplot()  + 
      geom_density_ridges(aes_string(y = x, x = "pricepernight_ln", group = x),color = "red", fill = "blue") +
      theme_tufte() + 
      labs(title = paste0("Vienna Hotels log-Price Distr. by ",x))  
    
  } else if(is.numeric(df[,x][[1]]) ) {
    plot <- df %>% ggplot(aes_string(x = x,y = "pricepernight_ln")) +  theme_tufte() + 
      geom_smooth(method="loess", color="black", size = 1) + 
      labs(title = paste0("Vienna Hotels log-Price Distr. by ",x)) 
    if (length(unique(df[,x][[1]])) == 2) {
      plot <- plot + geom_point( color = "red", shape = 3) 

    } else {
      plot <- plot + geom_point( color = "blue", ) 
    }
  }  
})

#Boxes_Scatters

# Dummy Tables 4: offer_cat, accommodationtype, neighbourhood ----
  # Get vector of raw factor levels & Dataframe of dummies if in level 

# Offer Category
Levels <- levels(factor(unlist(df$offer_cat)))
Dummies1 <- as.data.frame(do.call(rbind, lapply(lapply(df$offer_cat, factor, Levels), table)))
colnames(Dummies1) <- paste0("p",Dummies1 %>% colnames())

# Accommodation Type
Levels <- levels(factor(unlist(df$accommodationtype)))
Dummies2 <- as.data.frame(do.call(rbind, lapply(lapply(df$accommodationtype, factor, Levels), table)))
Dummies2 %>% colnames()

# Neighbourhood
Levels <- levels(factor(unlist(df$neighbourhood)))
Dummies3 <- as.data.frame(do.call(rbind, lapply(lapply(df$neighbourhood, factor, Levels), table)))

# City Actual
Levels <- levels(factor(unlist(df$city_actual)))
Dummies4 <- as.data.frame(do.call(rbind, lapply(lapply(df$city_actual, factor, Levels), table)))
colnames(Dummies4) <- paste0("city_",colnames(Dummies4))
  #summary(Dummies4) -> 97% writes Vienna -> drop, no use

df <- cbind(df,Dummies1,Dummies2,Dummies3) %>% 
  select(-c(city_actual,offer_cat,`p0% no offer`))


# Var name cleaning ------
colnames(df) <- gsub("+","",
                     gsub("%","",
                          gsub("17._","",
                               gsub("-","_",
                                    gsub(" ","_",df %>% colnames())))))

colnames(df)[27] <- "p75_offer"
df$p75_offer <- NULL
  # 75%+ offers = 0.2% of observations -> Model returns NA coeffs due to singularity
  # Same for Vacation_home_Condo -> 0.2%
df$Vacation_home_Condo <- NULL

df <- df %>% rename(
  c1dist_ln = center1distance_ln,
  c2dist_ln = center2distance_ln
) %>% select(-price)


# Model Specifications -------------
predictors <- df %>% 
  select(-matches("id|pricepernight|accommodationtype|neighbourhood|rating2_ta_reviewcount$|center2distance$|center1distance$|rating_reviewcount$|guestreviewsrating$")) %>% colnames()

# Sample vs Holdout -> No holdout -> In sample prediction
  # 10-fold CV, insted of 5 though -> not soooo many observations
nrow(df)

# train control is 5 fold cross validation
train_control <- trainControl(method = "cv",
                              number = 10,
                              verboseIter = TRUE)  

formula(paste0("pricepernight_ln ~ ", paste0(predictors, collapse = " + ")))

#### OLS ####
set.seed(1234)
system.time({
  ols_model <- train(
    formula(paste0("pricepernight_ln ~ ", paste0(predictors, collapse = " + "))),
    data = df,
    method = "lm",
    trControl = train_control
  )
})

summary(ols_model)

ols_model_coeffs <-  ols_model$finalModel$coefficients
ols_model_coeffs_df <- data.frame(
  "variable" = names(ols_model_coeffs),
  "ols_coefficient" = ols_model_coeffs
) %>%
  mutate(variable = gsub("`","",variable))

#### CART ####

set.seed(1234)
system.time({
  cart_model <- train(
    formula(paste0("pricepernight_ln ~ ", paste0(predictors, collapse = " + "))),
    data = df,
    method = "rpart",
    trControl = train_control,
    
    tuneGrid= expand.grid(cp = 0.001))
})

fancyRpartPlot(cart_model$finalModel, sub = "")

summary(cart_model)


#### Random Forest ####
length(predictors)^(1/2)

# set tuning
tune_grid <- expand.grid(
  .mtry = c( 5, 6, 7 ),
  .splitrule = "variance",
  .min.node.size = c(5, 10, 15)
)

set.seed(1234)
system.time({
  rf_model_1 <- train(
    formula(paste0("pricepernight_ln ~ ", paste0(predictors, collapse = " + "))),
    data = df,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

rf_model_1


#------------------ Model Comparison ####

final_models <-
  list("OLS" = ols_model,
       "CART" = cart_model,
       "Random_forest" = rf_model_1)

results <- resamples(final_models) %>% summary()
results

models <- c("ols_model","cart_model","rf_model_1")  
Predictions <-data.frame(sapply(models,function(x) {
  tl <- list()
  model <- eval(parse(text = x))
  tl[[x]] <- predict(model,newdata = df)
  res <- tl[[x]] - df$pricepernight_ln
  StDev <- sd(res)
  tl[[x]] <- exp(tl[[x]]) * exp((StDev^2)/2)
  
  return(tl)
}))

Rsq <- NULL
for (i in 1:3) {
  Rsq[models[i]] <- round(results[[3]][['Rsquared']][i,4],3)
}

SumStatTable <- as.data.frame(cbind(Rsq,rbindlist(lapply(Predictions, function(x) {
  tl <- list()
  tl[['MAE']] <- round(MAE(x,df$pricepernight),3)
  tl[['RMSE']] <- round(RMSE(x, df$pricepernight),3)
  tl[['RMSE_norm']] <- round(tl[['RMSE']]/mean(df$pricepernight),3)
  return(tl)
}))))
rownames(SumStatTable) <- models

SumStatTable
  # Model Choice : Random Forest -> mtry = 7 , Min.NodeSize = 5


#### Final Model Re-Estimation ####
train_control <- trainControl(method = "none",verboseIter = FALSE)  

# set tuning for final model
tune_grid <- expand.grid(
  .mtry = c(7),
  .splitrule = "variance",
  .min.node.size = c(5)
)

set.seed(1234)
system.time({
  rf_model_final <- train(
    formula(paste0("pricepernight_ln", paste0(" ~ ",paste0(predictors, collapse = " + ")))),
    data = df,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

rf_model_final$finalModel
summary(rf_model_final)

#### Summary of RF_2_Final Model ####
df <- df %>%
  mutate(predicted_price_ln = predict(rf_model_final, newdata = df))

df <- df %>% mutate(res = predicted_price_ln - pricepernight_ln)
StDev <- sd(df$res)
df$predicted_price <- exp(df$predicted_price_ln) * exp((StDev^2)/2)
Rsq <- rf_model_final$finalModel$r.squared

SumStatsFinalModelTable <- 
  cbind(Rsq,rbindlist(lapply(df[c("predicted_price")], function(x) {
    tl <- list()
    tl[['MAE']] <- round(MAE(x,df$pricepernight),3)
    tl[['RMSE']] <- round(RMSE(x, df$pricepernight),3)
    tl[['RMSE_norm']] <- round(tl[['RMSE']]/mean(df$pricepernight),3)
    return(tl)
  })))

SumStatsFinalModelTable

df %>% ggplot(aes(y = pricepernight, x = predicted_price)) +
  geom_point() + geom_abline(intercept = 0, slope = 1)

df %>% ggplot(aes(y = pricepernight_ln, x = predicted_price_ln)) +
  geom_point() + geom_abline(intercept = 0, slope = 1)


#### Residual Analysis 2 Find BEST DEALS ----
  # i.e. Largest Negative residuals
    # largest as in ? Percent / Money saved / total money saved
      # Money saved / day
df$res <- round(df$pricepernight - df$predicted_price,1)  

# Characterize best deal hotels
  # Hotel id, Star rating, Pricepernight, res, Nrnights,
    # rating2_ta, exp(rating2_ta_reviewcount_ln)-1, guestreviewsrating_exp,
    # +1s: neighbourhood, distance to centre
df %>% colnames()

# Hotels ranked by Best Deals according to our model
Deals <- df %>% select(hotel_id,starrating,pricepernight, res,Nrnights,
                       accommodationtype,neighbourhood, center1distance,
                       rating2_ta,rating2_ta_reviewcount) %>%
  arrange(res)

Deals <- rbind(Deals[c(1:5),],
               Deals[Deals$hotel_id %in% c(21912, 21975, 22344, 22080, 22184),])

names <- c("Hotel_id","Stars","Avg.Price",
           "vs_Prediction","Nights","Type",
           "Where?","Miles fr Center","TA_Rating","Nr.Ratings")
colnames(Deals) <- names
Deals

# Best Deals according to Gabors:
  # IDs: 21912, 21975, 22344, 22080, 22184

df[df$hotel_id %in% c(21912, 21975, 22344, 22080, 22184),] %>% 
  select(hotel_id,starrating,pricepernight, predicted_price , res,Nrnights, 
         accommodationtype,neighbourhood, center1distance,center2distance, 
         rating2_ta,rating2_ta_reviewcount,rating_reviewcount,guestreviewsrating) %>% 
  arrange(res)

