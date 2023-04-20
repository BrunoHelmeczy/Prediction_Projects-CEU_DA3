Libs <- c(
    "stargazer" ,"Hmisc" ,"readr" ,"rattle" ,"tidyverse" ,"caret" ,
    "ranger" ,"Hmisc" ,"knitr" ,"kableExtra" ,"xtable" ,
    "stringr" ,"dplyr" ,"ggthemes" ,"rpart" ,"ggplot2",
    'data.table', 'plotly'
)

invisible(sapply(Libs, function(l) {
    if (!require(l, character.only = TRUE)) {
        install.packages(l)
        require(l, character.only = TRUE)
    } 
}))

rm(list=ls())
options(scipen = 999)

filename <- "https://raw.githubusercontent.com/BrunoHelmeczy/Prediction_Projects-CEU_DA3/main/Bangkok_Airbnb/Data/Clean/airbnb_bangkok_cleaned.csv"
df <- fread(filename)

#### 1) Plot var distributions en-mass ####
    # Colnames selection:  df %>% select(-matches("^d_|^l_")
    # Dummies -> seperate "Race"-style plot - as for DA2-Ass2
    # rest: for loop - 
      #   continuous -> histogram
      #   Neighborhoods -> invert axes -> geom bar + geom_point(x = sum()/nrow(df))

allplots <- lapply(colnames(df), function(i) {
  VarPretty <- stringr::str_to_title(gsub('_', ' ', i))
  
  df %>% 
    ggplot(aes(x = df[[i]])) + 
    geom_bar() + 
    theme_tufte() + 
    xlab(VarPretty) + 
    labs(
      title = paste0("Frequency Distribution: ", VarPretty)
    )
})

AllCols <- names(df)
FactorCols <- grep('^f_', AllCols, value = TRUE)
USDcols <- grep('^usd_', AllCols, value = TRUE)
NrCols <- grep('^n_', AllCols, value = TRUE) %>%
  grep('_ln|_2', ., invert = TRUE, value = TRUE)
FlagsBoolCols <- grep("^d_|^l_|^flag_", AllCols, value = TRUE)

# unique(c(FactorCols, USDcols, NrCols, FlagsBoolCols))
# grep("^f_|^d_|^l_|^flag_|^usd_|_ln|_2|^n_", AllCols, invert = TRUE, value = TRUE)

#### Modeling ####

# 1) Prep
df[, (FactorCols) := lapply(.SD, factor), .SDcols = FactorCols]

#### Variable Grouping ####
Prop_vars      <- grep(".*host.*|.*review.*|^d_|n_days|^usd", AllCols, value = TRUE) 
Host_vars      <- grep(".*host.*", AllCols, value = TRUE) 
Review_vars    <- grep(".*review.*|n_days", AllCols, value = TRUE) 
Amenities_vars <- grep("^d_", AllCols, value = TRUE)

# Interactions: Amenities + Props4Interactions
Props4Interactions <- c(grep('^f_', Prop_vars, value = TRUE), "n_accommodates")

Interactions <- paste0(
  paste0("( ",paste0(Props4Interactions, collapse = " + ")," )")
  ," * ",
  paste0("( ",paste0(Amenities_vars, collapse = " + ")," )")
)

predictors1 <-   Prop_vars
predictors2 <- c(predictors1, Host_vars, Review_vars)
predictors3 <- c(predictors2, Amenities_vars)
predictorsE <- c(predictors3, Interactions)

PriceHist <- plot_ly(
  data = df,
  x =~ usd_price,
  type = 'histogram'
) %>% 
  layout(
    title = 'Price Frequency Distribution',
    yaxis = list(title = list(text = 'Frequency Count')),
    xaxis = list(range = c(0, 300), title = 'Price ($)')
  ) %>% 
  plotly::config(displayModeBar = FALSE)

LnPriceHist <- plot_ly(
  data = df,
  x =~ usd_price_ln,
  type = 'histogram'
) %>% 
  layout(
    title = 'Price Frequency Distribution',
    yaxis = list(title = list(text = 'Frequency Count')),
    xaxis = list( title = 'Log Price ($)')
  ) %>% 
  plotly::config(displayModeBar = FALSE)

#### Sample vs Holdout sets ####
set.seed(1)

train_indices <- as.integer(createDataPartition(df$usd_price_ln, p = 0.7, list = FALSE))
data_train <- df[train_indices, ]
data_holdout <- df[-train_indices, ]

train_control <- trainControl(
    method = "cv",
    number = 5,
    verboseIter = FALSE
)

#### OLS ####
set.seed(1234)

system.time({
  ols_model <- train(
    formula(paste0("usd_price_ln ~ ", paste0(predictors3, collapse = " + "))),
    data = data_train,
    method = "lm",
    trControl = train_control
  )
}) # ca. 1.0s
  
ols_model_coeffs <-  ols_model$finalModel$coefficients
ols_model_coeffs_df <- data.table(
  "variable"        = names(ols_model_coeffs),
  "ols_coefficient" = round(ols_model_coeffs, 3)
) %>% 
  .[, variable := gsub('`', '', variable)]

#### CART - Very large & pruned tree ####
system.time({
  cart_model <- train(
    formula(paste0("usd_price_ln ~ ", paste0(predictors3, collapse = " + "))),
    data = data.frame(sapply(data_train,as.numeric)),
    method = "rpart",
    trControl = train_control,
    tuneGrid = expand.grid(cp = 0.0005))
}) # ca. 1.1s

# fancyRpartPlot(cart_model$finalModel, sub = "")

# take the last model (large tree) and prunce (cut back)
#pfit <-prune(cart_model$finalModel, cp=0.005 )
#summary(pfit)


#### LASSO ####
# using extended model w interactions
system.time({
  lasso_model <- train(
    formula(paste0("usd_price_ln ~ ", paste0(predictorsE, collapse = " + "))),
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneGrid =  expand.grid("alpha" = 1, "lambda" = seq(0.01, 0.25, by = 0.04)),
    trControl = train_control
  )
}) # ca. 21s

# TODO:
  # to continue from here

lasso_coeffs <- coef(
  lasso_model$finalModel,
  lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  setDT() %>% 
  setnames(old = 's1', 'lasso_coefficient') %>%
  .[order(-abs(lasso_coeffs[['lasso_coefficient']]))]

lasso_coeffs_non_null <-lasso_coeffs[lasso_coefficient != 0]

regression_coeffs <- merge(
  lasso_coeffs_non_null, 
  ols_model_coeffs_df,
  by = "variable", 
  all = TRUE
)
  
#### Random Forest ####
tune_grid <- expand.grid(
  .mtry = c( 5, 7, 9),
  .splitrule = "variance",
  .min.node.size = c(5, 10)
)
  
# simpler model for model A (1)
system.time({
  rf_model_1 <- train(
    formula(paste0("usd_price_ln ~ ", paste0(predictors2, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
}) # ca. 60s
# rf_model_1

# set tuning for benchmark model (2)
tune_grid <- expand.grid(
  .mtry = c(8, 10, 12),
  .splitrule = "variance",
  .min.node.size = c(5, 10, 15)
)
  
system.time({
  rf_model_2 <- train(
    formula(paste0("usd_price_ln", paste0(" ~ ",paste0(predictors3, collapse = " + ")))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
}) # ca. 120s
# rf_model_2

system.time({
  rf_model_2_lev <- train(
    formula(paste0("usd_price", paste0(" ~ ",paste0(predictors3, collapse = " + ")))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
}) # ca. 128s
# rf_model_2_lev


  # Tuning parameter choice 1
result_1 <- matrix(c(
    rf_model_1$finalModel$mtry,
    rf_model_2$finalModel$mtry,
    rf_model_2_lev$finalModel$mtry,
    rf_model_1$finalModel$min.node.size,
    rf_model_2$finalModel$min.node.size,
    rf_model_2_lev$finalModel$min.node.size
  ),
  nrow = 3, 
  ncol = 2,
  dimnames = list(
    c("Model 1", "Model 2","Model 2 level"),
    c("Min vars","Min nodes")
  )
)

# evaluate random forests -------------------------------------------------
  
#------------------ Model Comparison ####
final_models <- list(
  "OLS" = ols_model,
  "LASSO (with Interactions)" = lasso_model,
  "Random forest (Small)" = rf_model_1,
  "Random forest" = rf_model_2
)
  
results <- resamples(final_models) %>% 
  summary()
  
models <- c("ols_model","lasso_model","rf_model_1","rf_model_2")  

Predictions <- sapply(models[-2],function(x) {
  tl <- list()
  
  model <- eval(parse(text = x))
  
  tl[[x]] <- predict(model, newdata = data_train)

  res <- tl[[x]] - data_train$usd_price_ln
  StDev <- sd(res)
  
  tl[[x]] <- exp(tl[[x]]) * exp( (StDev ^ 2) / 2)
  
  return(tl)
}) %>% 
  data.frame() %>% 
  setDT()

Rsq <- results[[3]][['Rsquared']][, c(1:3, 5:6)] %>% 
  t() %>% 
  data.table() 

Rsq <- Rsq[, lapply(.SD, function(x) round(mean(x), 3)), .SDcols = names(Rsq)] %>% 
  setnames(new = models) %>% 
  unlist()

SumStatTable <- lapply(Predictions, function(x) {
  tl <- list()
  tl[['MAE']]       <- round(MAE(x,data_train$usd_price),3)
  tl[['RMSE']]      <- round(RMSE(x, data_train$usd_price),3)
  tl[['RMSE_norm']] <- round(tl[['RMSE']]/mean(data_train$usd_price),3)
  return(tl)
}) %>% 
  rbindlist() %>% 
  cbind(Rsq = Rsq[-2])

rownames(SumStatTable) <- models[-2]
SumStatTable

#### Final Model Re-Estimation ####
train_control_fin <- trainControl(
  method = "none",
  verboseIter = FALSE
)  

# set tuning for final model
tune_grid <- expand.grid(
  .mtry = c(12),
  .splitrule = "variance",
  .min.node.size = c(5)
)

system.time({
  rf_model_final <- train(
    formula(paste0("usd_price_ln", paste0(" ~ ",paste0(predictors3, collapse = " + ")))),
    data = data_train,
    method = "ranger",
    trControl = train_control_fin,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
}) # ca. 2s

#### Summary of RF_2_Final Model ####
data_holdout_w_prediction <- copy(data_holdout) %>% 
  .[, predicted_price_ln := predict(rf_model_final, newdata = data_holdout)] %>% 
  .[, res := predicted_price_ln - usd_price_ln]

StDev <- sd(data_holdout_w_prediction$res)

data_holdout_w_prediction[, predicted_price := exp(predicted_price_ln) * exp((StDev ^ 2) / 2)]

Rsq <- rf_model_final$finalModel$r.squared

SumStatsFinalModelTable <- lapply(data_holdout_w_prediction[,c("predicted_price")], function(x) {
      tl <- list()
      tl[['MAE']] <- round(MAE(x,data_holdout$usd_price),3)
      tl[['RMSE']] <- round(RMSE(x, data_holdout$usd_price),3)
      tl[['RMSE_norm']] <- round(tl[['RMSE']]/mean(data_holdout$usd_price),3)
      return(tl)
  }) %>% 
    rbindlist() %>% 
    cbind(Rsq)

SumStatsFinalModelTable

# MODEL DIAGNOSTICS -------------------------------------------------------

# Pred vs Actual Y Line + Scatter
PredvsAccPlot <- data_holdout_w_prediction %>% 
  plot_ly(
    type = 'scatter',
    mode = 'lines',
    showlegend = FALSE,
    x =~ usd_price,
    y =~ usd_price,
    hoverinfo = 'text',
    text =~ paste0('Actual Price: ', round(usd_price), '$')
  ) %>% 
  add_trace(
    y =~ predicted_price,
    mode = 'markers',
    text =~ paste0('Predicted Price: ', round(predicted_price), '$')
  ) %>% 
  layout(
    title = "Actual versus Predicted Prices",
    xaxis = list(title = "Actual Prices ($)"),
    yaxis = list(title = "Predicted Prices ($)"),
    hovermode = 'x'
  ) %>% 
  plotly::config(displayModeBar = FALSE)

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
d <- cbind("All", d)
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
  
