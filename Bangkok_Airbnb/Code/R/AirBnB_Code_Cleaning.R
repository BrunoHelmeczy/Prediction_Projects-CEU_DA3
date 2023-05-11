# start <- Sys.time()
utils_file <- list.files(pattern = 'utils.R', recursive = TRUE, full.names = TRUE)
source(utils_file)
options(scipen = 999)

Libs <- c(
    'stargazer', 'Hmisc', 'readr', 'rattle', 'tidyverse', 'arrow',
    'caret', 'ranger', 'Hmisc', 'knitr', 'kableExtra', 'rstudioapi',
    'xtable', 'data.table', 'stringr', 'dplyr', 'plotly', 'quantmod'
)

LoadLibraries(LibsVector = Libs)

system.time(
    Bangkok <- LoadData()
)

#### CLEANING  ####
system.time({
    VarDescribe <- DescribeVariables(Bangkok)

    Bangkok <- Bangkok %>%
        convertBooleans() %>%
        dropIDs() %>% 
        coerceCols2Prct() %>% 
        countHostVerifications() %>%
        standardizeHostNeighborhood() %>%
        dropDupeListingCols()
}) # 1.1s

#### 2.3) Host Info (ex. Logicals )  ####
Host_Cols <- getHostCols()[which(!getHostCols() %in% getHostPercCols())]

# 2.3.3) Host Neighborhood 

# Remove from Host Var list
Host_Cols <- Host_Cols[which(!Host_Cols %in% c("host_verifications","host_neighbourhood"))]

# 2.3.4) Host Listings infos -> Seems similar to CalcListings Data 
ListingCols <- getListingsCols()


# TODO: DATES a seperate Matter 

#### 2.4) Geospatial Data -> Keep Neighbourhood Cleansed only ####
GeoCols <- VarDescribe[VarGroups == "Geo", Vars]
Bangkok[, lapply(.SD, uniqueN), .SDcols = GeoCols]

Bangkok[, neighbourhood_cleansed := factor(neighbourhood_cleansed)] %>% 
    setnames(
        old = 'neighbourhood_cleansed',
        new = 'f_neighbourhood_cleansed'
    )

Bangkok[, (GeoCols[-2]) := NULL]

#### 2.5) Prop Info -> Prop/Room Type, Accommodates, Bathrooms, Bedrooms, Beds, Amenities ####
PropCols <- VarDescribe[VarGroups == 'Property', Vars]
# Bangkok[, ..PropCols]

# 2.5.1) Room Types -> Keep only 'Entire home/apt' -> Only Var.Value left -> delete Var.
Bangkok <- Bangkok[room_type == "Entire home/apt"] %>% 
    .[, room_type := NULL]

PropCols <- PropCols[!PropCols %in% "room_type"]

# 2.5.2) Property Types -> 
# Keep only Apartments & Condominiums... 
Bangkok <- CollapsePropertyTypes(Bangkok)
PropCols <- PropCols[!PropCols %in% "property_type"]

# 2.5.3) Bathroom Text -> Bathrooms figures
Bangkok[, n_bathrooms := gsub('Half-', 0.5, bathrooms_text) %>% 
            gsub('bath|s', '', .) %>% 
            as.numeric()] %>% 
  .[, bathrooms_text := NULL]

PropCols <- PropCols[!PropCols %in% "bathrooms_text"]

# 2.5.4) Beds / Bedrooms / Accommodates -> Accomms 2-6 pax
AccomCols <- VarDescribe[Vars %in% PropCols[-length(PropCols)], Vars]

Bangkok <- Bangkok[between(accommodates, 2, 6)] %>% 
  setnames(
    old = AccomCols,
    new = paste0('n_', AccomCols)
  )

# Prop Var.s left = AMENITIES -> @ End of cleaning

#### 2.6) Sales -> Price (in Thai Baht) & Min/Max Stay restrictions ####
# All numeric Except price
SalesCols <- VarDescribe[VarGroups == "Sales", Vars]

# 2.6.1)  PRICE - Clean string & convert to USD
exchange_rate <- quantmod::getQuote(paste0("THBUSD=X")) %>% 
  select(Last)

Bangkok[, price := as.numeric(gsub('[^[:digit:].]', "", price))] %>% 
  .[, usd_price := price * exchange_rate[1, 1]] %>% 
  .[, price := NULL]

SalesCols <- SalesCols[!SalesCols %in% "price"]

# 2.6.2) Stay restrictions -> Check similarity 
MinNightCols <- grep('minimum_nights', SalesCols, value = T)
CalcColumnSimilarity(Bangkok, MinNightCols)
# 97.5 - 98.6% identical -> No loss of info by throwing out
Bangkok[, (MinNightCols[-1]) := NULL]

SalesCols <- SalesCols[!SalesCols %in% grep("minimum_nights",SalesCols, value = T)]

# Maximum Night Cols -> Max Nights 88.8% identical w Others, rest 99.7% identical w eachother
# Keep 2 of the 4 -> Max Nights, Max Nights Avg.
MaxNightCols <- grep('maximum_nights', SalesCols, value = T)
CalcColumnSimilarity(Bangkok, MaxNightCols)

Bangkok[, (MaxNightCols[2:3]) := NULL] %>% 
  setnames(
    old = grep('nights', names(Bangkok), value = TRUE),
    new = paste0('n_',grep('nights', names(Bangkok), value = TRUE))
  )

#### 2.7) Availability - Exc. Logicals ####
# In terms of pricing -> How many days are sold seems more intuitive to be relevant  
AvailCols <- VarDescribe[(VarGroups == "Availability" & VarType != "logical") , Vars]
summary(Bangkok[, ..AvailCols])

lapply(AvailCols, function(x) {
  Bangkok[, (x) := abs(as.numeric(gsub("[^[:digit:]]", "", x)) - get(x)) ]
  return(NULL)
})

setnames(
  Bangkok,
  old = AvailCols,
  new = gsub('.+_([0-9]{2,})', 'n_sales_\\1', AvailCols)
)

#### 2.8) Satisfaction i.e. Reviews ####
SatCols <- VarDescribe[VarGroups == "Satisfaction", Vars]
# summary(Bangkok[, ..SatCols]) # Similar number of NAs in Review_Scores

# Check if missing for same rows
data.table(
  id  = 1:nrow(Bangkok),
  NAs = rowSums(is.na(Bangkok[, ..SatCols]))
) %>%
  .[, keyby = NAs, .N]

# Of ca. 3530 rows w NAs 3418 has all NAs -> 

# 2.8.1) Review Scores
SubReviews <- grep('review_scores', SatCols, value = TRUE)[-1]
# summary(Bangkok[, ..SubReviews])

# review score rating aggregate of 6 sub-categories
# Cats: Accuracy, Cleanliness, Check-in, Communication, Location, Value
# Seems important for guest satisfaction analysis -> TOO MUCH detail for overall pricing
# +1s: Location & Perceived-value information inside amenities, neighborhood
# DROP SUB-CATEGORIES
Bangkok[, (SubReviews) := NULL]
SatCols <- SatCols[!(SatCols %in% SubReviews)]

# 2.8.2) Review Frequency -> Grand scheme of things - 
# ONLY total Reviews & Reviews/Month of interest
Bangkok[, (SatCols[3:4]) := NULL]
SatCols <- SatCols[!SatCols %in% SatCols[3:4]]
# summary(Bangkok[, ..SatCols])

# 2.8.3) Inpute NAs for Revs/Month & Rev Score
Bangkok[, keyby = number_of_reviews, mean(review_scores_rating, na.rm = TRUE)]

# 0 Revs -> 0 Revs/month + flag var
Bangkok[, flag_reviews_per_month := as.numeric(is.na(reviews_per_month))] %>%
  FillNAs(Cols = 'reviews_per_month')

# 0 Reviews -> Mean Rev.Score + flag var
# More sensitive to outliers BUT -> No reviews means added uncertainty for guests
Bangkok[, flag_review_scores_rating := as.numeric(is.na(review_scores_rating))] %>%
  FillNAs(
    Cols = 'review_scores_rating',
    fill_value = mean(Bangkok$review_scores_rating, na.rm = T)
  )

Cols <- c('number_of_reviews' , 'review_scores_rating' , 'reviews_per_month')

setnames(
  Bangkok,
  old = Cols,
  new = paste0('n_', Cols)
)

#### 2.9) Date Vars ####
# 2 time-related infor of interest:
# 1st How long has property had visitors -> time since first review -> best proxy available
# 2nd Low long since last time it had visitors -> time since last review -> best proxy available
DateVars <- VarDescribe[VarType == "Date", Vars]

Bangkok[, `:=` (
  n_days_since_1st = as.numeric(calendar_last_scraped - first_review),
  n_days_since_last = as.numeric(calendar_last_scraped - last_review)
)]

Bangkok[, (DateVars) := NULL]

#### 2.10) Amenities ####
Bangkok <- AddAmenitiesCols(Bangkok)

# Start all varnames w l_,d_,n_,flag_,p_,usd_
torename <- grep("^l_.*|^d_.*|^n_.*|^flag_.*|^f_.*|^p_.*|^usd_.*", names(Bangkok), value = TRUE, invert = TRUE)

setnames(
  Bangkok,
  old = torename,
  new = paste0('d_', torename)
)

#### Save Files ####
StoreData(Data = Bangkok, type = 'Clean')

### DONE 'til here 

df <- Bangkok

#### 1) Group Vars ####

Vars <- colnames(df)
Vartype <- cbind(unlist(lapply(df,class)))
VarDescribe <- data.frame(Vars,Vartype)
rownames(VarDescribe) <- NULL

#### 2) Feature Engineer ####

#### 2.1) Price in USD ####
# 116 Obs.s > 350 -> delete
df <- df %>% filter(usd_price <= 350)
df[c("usd_price")] %>% 
  ggplot(aes(x = usd_price)) + 
  geom_histogram()
# Skewed as hell -> log transform
df <- df %>% mutate(usd_price_ln = log(usd_price))

#### 2.2) Numeric Vars ####
sapply(df %>% dplyr::select(matches("^n_",colnames(df))) %>% colnames(),function(x) {
  hist(df[x])
})

# To ln: 
#   n_days_since_last,
#   n_days_since_1st,
#   n_accommodates (+ ln, ln^2, ^2, ^3)
df <- df %>% mutate(
  n_days_since_last_ln = log(n_days_since_last)  ,
  n_days_since_1st_ln  = log(n_days_since_1st) ,
  n_accommodates_ln    = log(n_accommodates),
  n_accommodates_ln2   = log(n_accommodates)^2,
  n_accommodates_2     = n_accommodates^2,
  n_accommodates_2     = n_accommodates^3
)

# To Throw out: 
#   n_max_nights, 
#   n_max_nights_avg
df <- df %>% dplyr::select(-c(n_max_nights,n_max_nights_avg))


# To Group:
#   n_bathrooms (1,2,3+ -> NA = 1)
#   n_reviews_per_month -> 0,1 -> f_has_review_30d
#   n_review_scores_rating -> 80,90,100
#   n_number_of_reviews -> 0-50,51-100,100+
#   n_sales_365 -> 0,90,180,270,360
#   n_sales_90 -> 0,30,60,90
#   n_sales_60 -> Empty, non-empty
#   n_sales_30 -> Empty,Some, Full
#   n_min_nights -> 1,2,3+
#   n_beds -> 1,<= 3, 3+
#   n_bedrooms -> 1,2,3+
#   n_host_listing -> 1, 1+ (Single-,Multi- Listings)

df <- df %>% mutate(
  f_has_1_review_monthly = ifelse(df$n_reviews_per_month == 0, 0,1),
  f_review_scores_rating = cut(df$n_review_scores_rating, c(0,80,90,99,101), labels = c(0,1,2,3), right = F),
  f_number_of_reviews    = cut(df$n_number_of_reviews, c(0,1,51,max(df$n_number_of_reviews)), labels = c(0,1,2), right = F),
  f_sales_365    = cut(df$n_sales_365, c(0,90,180,270,max(df$n_sales_365)+1), labels = c(0,1,2,3), right = F),
  f_sales_90     = cut(df$n_sales_90, c(0,1,30,60,max(df$n_sales_90)+1), labels = c(0,1,2,3), right = F),
  f_sales_60     = cut(df$n_sales_60, c(0,1,max(df$n_sales_60)+1), labels = c(0,1), right = F),
  f_sales_30     = cut(df$n_sales_30, c(0,1,29,max(df$n_sales_30)+1), labels = c(0,1,2), right = F),
  f_min_nights   = cut(df$n_min_nights, c(0,2,3,max(df$n_min_nights)), labels = c(1,2,3), right = F),
  f_beds         = cut(df$n_beds, c(0,1,3,max(df$n_beds,na.rm = T)), labels = c(1,3,max(df$n_beds,na.rm = T)), right = F),
  f_bedrooms     = cut(df$n_bedrooms, c(0,1,2,max(df$n_bedrooms,na.rm = T)), labels = c(1,2,3), right = T),
  f_bathrooms    = cut(df$n_bathrooms, c(0,1,2,max(df$n_bathrooms, na.rm=T)), labels=c(1,2,3), right = T),
  f_host_listing = cut(df$n_host_listing, c(0,2,max(df$n_host_listing,na.rm = T)), labels = c(1,2), right = F))


# Change Infinite values with NaNs
for (j in 1:ncol(df) ) data.table::set(df, which(is.infinite(df[[j]])), j, NA)

# where do we have missing values now?
to_filter <- sapply(df, function(x) sum(is.na(x)))
ColswNAs <- data.frame("Rank" = to_filter[to_filter > 0]  ) %>% arrange(desc(Rank))

# 1 NA: l_host_is_superhost, n_host_listings_count,l_host_has_profile_pic,
#       l_host_identity_verified, f_number_of_reviews, f_min_nights
# Impute w Reasonable guess: 0, 1,0,0,0,1
# NA in these cases is most likely 0 -> though at least 1 night & 1 listing is implied

df <- df %>% mutate(
  l_host_is_superhost       = ifelse(is.na(l_host_is_superhost),0,l_host_is_superhost), 
  n_host_listings_count     = ifelse(is.na(n_host_listings_count),1,n_host_listings_count),
  l_host_has_profile_pic    = ifelse(is.na(l_host_has_profile_pic),0,l_host_has_profile_pic),
  l_host_identity_verified  = ifelse(is.na(l_host_identity_verified),0,l_host_identity_verified),
  f_number_of_reviews       = ifelse(is.na(f_number_of_reviews),0,f_number_of_reviews),
  f_min_nights              = ifelse(is.na(f_min_nights),1,f_min_nights))    

# Most NAs:  p_host_response_rate,f_host_neighbourhood,n_days_since_last_ln,
#  n_days_since_last, n_days_since_1st_ln, n_days_since_1st
# p_host_response_rate -> DROP

drop <- c("p_host_response_rate")
df[drop] <- NULL

# days_since -> No Value most likely implies no hosting yet -> 
#   set to max value & add flag variable -> 1 is enough we showed ealier NAs are from same rows
#   f_host_listing -> must have at least 1 if in dataset
df <- df %>% mutate(
  flag_n_days_since    = ifelse(is.na(n_days_since_last_ln),1,0),
  n_days_since_last_ln = ifelse(is.na(n_days_since_last_ln),max(n_days_since_last_ln,na.rm = T),n_days_since_last_ln),
  n_days_since_last    = ifelse(is.na(n_days_since_last),max(n_days_since_last,na.rm = T),n_days_since_last),
  n_days_since_1st_ln  = ifelse(is.na(n_days_since_1st_ln),max(n_days_since_1st_ln,na.rm = T),n_days_since_1st_ln),  
  n_days_since_1st     = ifelse(is.na(n_days_since_1st),max(n_days_since_1st,na.rm = T),n_days_since_1st),
  f_host_listing       = ifelse(is.na(f_host_listing),1,f_host_listing))

# p_host_acceptance_rate -> 100 or NOT
df <- df %>% mutate(
  f_host_accepts_all = ifelse(is.na(p_host_acceptance_rate),0,
                              ifelse(p_host_acceptance_rate == max(p_host_acceptance_rate, na.rm = T),1,0)),
  n_bedrooms = ifelse(is.na(n_bedrooms), 1, n_bedrooms),
  n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds), #assume n_beds=n_accomodates,
  n_bathrooms = ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms))  #assume at least 1 bath)  

df <- df %>% mutate(
  f_beds         = cut(df$n_beds, c(0,1,3,max(df$n_beds,na.rm = T)+1), 
                       labels = c(1,3,max(df$n_beds,na.rm = T)), right = F),
  f_bedrooms     = cut(df$n_bedrooms, c(0,1,2,max(df$n_bedrooms,na.rm = T)), labels = c(1,2,3), right = T),
  f_bathrooms    = cut(df$n_bathrooms, c(0,1.1,2.1,max(df$n_bathrooms, na.rm=T)+1), labels=c(1,2,3), right = F)) %>% 
  dplyr::select(-p_host_acceptance_rate)


#### Save Files ####
CleanDataPath <- paste0(getwd(),"/Prediction_Projects-CEU_DA3/Bangkok_Airbnb/Data/Clean/")
# CSV & RDS
write_csv(df,paste0(CleanDataPath,"airbnb_bangkok_cleaned.csv"))
saveRDS(df,paste0(CleanDataPath,"airbnb_bangkok_cleaned.rds"))