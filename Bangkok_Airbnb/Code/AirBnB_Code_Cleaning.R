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

#rm(list=ls())

BangkokRaw <- read_csv("http://data.insideairbnb.com/thailand/central-thailand/bangkok/2020-12-23/data/listings.csv.gz")
RawDataPath <- paste0(getwd(),"/Prediction_Projects-CEU_DA3/Bangkok_Airbnb/Data/Raw/")

# CSV
write_csv(BangkokRaw,paste0(RawDataPath,"airbnb_bangkok_raw.csv"))
# RDS
saveRDS(BangkokRaw,paste0(RawDataPath,"airbnb_bangkok_raw.rds"))

Bangkok <- BangkokRaw

#### CLEANING  ####

#### 1) Delete URLs & Text datas
drops <- c("host_thumbnail_url","host_picture_url",
           "listing_url","picture_url",
           "host_url","last_scraped",
           "description","neighborhood_overview",
           "host_about","host_response_time",
           "name","space","host_location")
length(drops)
Bangkok <- Bangkok[ , !(names(Bangkok) %in% drops)]

# Also DROP All NA Cols
ColNACh <- as.data.frame(colSums(is.na(Bangkok)) == nrow(Bangkok))
colnames(ColNACh) <- "AllNACols"
ColNACh$Cols <- colnames(Bangkok)
Bangkok[ColNACh$Cols[ColNACh == T]] <- NULL

# Check 4 Duplicate Rows -> None
which(duplicated(Bangkok))

#### 2) Group Vars by Topic -> Clean by Topic ####
# VarGroups: IDs, Host, Geo, Property, Sales, Availability, Satisfaction,
#             CalcListings
Vars <- colnames(Bangkok)
VarGroups <- c(rep("IDs",4),rep("Host",10),rep("Geo",4),rep("Property",7),
               rep("Sales",9),rep("Availability",5),rep("Satisfaction",13),
               "Availability",rep("CalcListings",4),"Satisfaction")
VarType <- cbind(unlist(lapply(Bangkok,class)))

VarDescribe <- data.frame(Vars, VarGroups,VarType)
rownames(VarDescribe) <- NULL


#### 2.1) Convert all Logicals -> True = 1, False = 0 ####
Logic_Cols <- VarDescribe[VarType=='logical',"Vars"]
Bangkok[Logic_Cols] %>% head()

for (binary in Logic_Cols){
  Bangkok[[binary]][Bangkok[[binary]]== F ] <- 0
  Bangkok[[binary]][Bangkok[[binary]]== T ] <- 1
}

# 2.1.1) Annotate Logical Vars -> Also Indy for having been handled 
LogColPos <- which(colnames(Bangkok) %in% Logic_Cols)
colnames(Bangkok)[LogColPos] <- paste0("l_",Logic_Cols)

#### 2.2) IDs -> No modelling purpose -> Delete ####
ID_Cols <- VarDescribe[VarGroups=="IDs","Vars"]
Bangkok[ID_Cols] <- NULL

#### 2.3) Host Info (ex. Logicals )  ####
Host_Cols <- VarDescribe[VarGroups=="Host" & VarType != 'logical',"Vars"]
Bangkok[Host_Cols] %>% head()

# 2.3.1) Response- & Acceptance Rate -> Remove % sign, coerce to Numeric & annotate w. "p_"
for (perc in c("host_response_rate","host_acceptance_rate")){
  Bangkok[[perc]]<-gsub("%","",as.character(Bangkok[[perc]]))
  Bangkok[[perc]] <- as.numeric(Bangkok[[perc]])
}
Bangkok <- Bangkok %>% rename(p_host_response_rate = host_response_rate,
                              p_host_acceptance_rate = host_acceptance_rate)

# Remove from Host Var list to handle
Host_Cols <- Host_Cols[which(!Host_Cols %in% c("host_response_rate","host_acceptance_rate"))]

Bangkok[Host_Cols]
# 2.3.2) Host Verifications -> Could be dummytables, but... 
# Nr. of Verifications seem more important, so...
#  Convert to Nr. -> Split to sub-strings, keep its length / row

# host_verifications: list of objects indicating host are not scammers
# the more the better & any single verifier is not in itself significant 
# stringsplit each row -> keep only number of splitted strings
n_host_verifications <- NULL
for (i in 1:length(Bangkok$host_verifications)) {
  n_host_verifications[i] <- length(unlist(str_split(Bangkok$host_verifications[i],", ")))
}
Bangkok$host_verifications <- n_host_verifications
Bangkok <- Bangkok %>% rename(n_host_verifications = host_verifications)


# 2.3.3) Host Neighborhood 
# where host comes from might be relevant -> should be factored 
Bangkok$host_neighbourhood <- factor(trimws(
  gsub("[[:digit:]]","",
       gsub("Lower","",
            gsub("Upper","",Bangkok$host_neighbourhood)))))


Levels <- levels(factor(unlist(Bangkok$host_neighbourhood)))
DummyTable <- as.data.frame(do.call(rbind, lapply(lapply(Bangkok$host_neighbourhood, factor, Levels), table)))
#colnames(DummyTable) <- gsub("[^[:alnum:]_]","",trimws(colnames(DummyTable)))


Dummies_w_Many_Falses <- function(DummyVardf, MaxHowManyTrue = 100) {
  
  return(lapply(1:length(DummyVardf), function(x) {
    tl <- list()
    tl[['Colname']] <- colnames(DummyVardf[c(x)])
    tl[['False']] <- table(DummyVardf[c(x)])[1]
    tl[['True']] <- table(DummyVardf[c(x)])[2]
    return(tl)
  }) %>% rbindlist() %>% filter(True < MaxHowManyTrue) %>% 
    arrange(True))
}


hostcities2keep <- Dummies_w_Many_Falses(DummyTable,10000)

# outside top3 > 3% of data / value -> 'Other'
top3hostcities <- unlist(hostcities2keep[(nrow(hostcities2keep)-2):(nrow(hostcities2keep)),"Colname"])

names(top3hostcities) <- NULL
top3hostcities[1:3]

table(Bangkok$host_neighbourhood)

Bangkok$host_neighbourhood <- ifelse(!Bangkok$host_neighbourhood %in% top3hostcities[1:3],
                                     "Other",ifelse(grepl(top3hostcities[1],Bangkok$host_neighbourhood),
                                                    top3hostcities[1],
                                                    ifelse(grepl(top3hostcities[2],Bangkok$host_neighbourhood),
                                                           top3hostcities[2],top3hostcities[3])))

Bangkok <- Bangkok %>% rename(f_host_neighbourhood = host_neighbourhood)

rm(DummyTable,hostcities2keep)

# Remove from Host Var list to handle
Host_Cols <- Host_Cols[which(!Host_Cols %in% c("host_verifications","host_neighbourhood"))]

# 2.3.4) Host Listings infos -> Seems similar to CalcListings Data 
ListingCols <- c(grep("listing", Host_Cols, value = T),
                 VarDescribe[VarGroups == unique(grep("Listing",VarGroups, value = T)),"Vars"])
summary(Bangkok[ListingCols])
# Summing occurrences where they're unequal = 0 -> Delete 1
sum(Bangkok[ListingCols][1] != Bangkok[ListingCols][2], na.rm= T)
Bangkok[ListingCols[2]] <- NULL

ListingCols[2] <- NA
ListingCols <- ListingCols[!is.na(ListingCols)]

# Compare each Listing Column -> count where values aint equal
ColSimilarity <- NULL
for (i in 1:length(ListingCols)) {
  for (j in 1:length(ListingCols)) {
    ColSimilarity[(i-1)*length(ListingCols)+j] <- sum(Bangkok[ListingCols][i] != Bangkok[ListingCols][j], na.rm= T) / nrow(Bangkok)     
    
  }
}
# They seem quite different, but I definitely do not need info on rooms
matrix(ColSimilarity,nrow = length(ListingCols), ncol = length(ListingCols))
# Figures per column identical min 50% of cases -> not the same
# Substantively -> No reason to believe listing counts affect price
# Pricing market driven, not profit / cash-flow needs-based, so inventory should not affect prices
# keep 1 Var, no need for further detail

Bangkok[ListingCols[2:5]] <- NULL
Bangkok <- Bangkok %>% rename(n_host_listings_count = host_listings_count)


# Dates a seperate Matter 

#### 2.4) Geospatial Data -> Neighborhood (Cleansed) + Long- & Latitude ####
Geo_Cols <- VarDescribe[VarGroups == "Geo","Vars"]

# Check Uniqueness... 
sapply(Bangkok[Geo_Cols],unique)    
# Neighborhood 700+ Values
# Nieghobrhood Cleansed - KEEP
# Co-ordinate data -> no purpose
# Could very indirectly indicate neighborhood information w.r.t. price
# definitely non-linear
# units so small, & not measuring relevant distance fr. somewhere

# keep only Neighboorhood Cleansed -> as factor

Bangkok <- Bangkok %>% mutate(f_neighbourhood_cleansed = factor(neighbourhood_cleansed))
Bangkok[Geo_Cols] <- NULL

#### 2.5) Prop Info -> Prop/Room Type, Accommodates, Bathrooms, Bedrooms, Beds, Amenities ####
PropCols <- VarDescribe[VarGroups=='Property',"Vars"]
Bangkok[PropCols]

# 2.5.1) Room Types -> Keep only 'Entire home/apt' -> Only Var.Value left -> delete Var.
table(Bangkok$room_type)
Bangkok <- Bangkok[Bangkok$room_type=="Entire home/apt",]
Bangkok$room_type <- NULL
PropCols <- PropCols[!PropCols %in% "room_type"]

# 2.5.2) Property Types -> 
table(Bangkok$property_type)

# Keep only Apartments & Condominiums... 
# Many weird names - Including hotel rooms, dorms, hostels, CASTLES & other
# Unique / Rare data - but NOT RELEVANT FOR APARTMENT PRICING
WierdPropTypeKeys <- c("Room","Castle","Entire cabin","chalet","dorm","hostel",
                       "place","Farm stay","Tiny house","Treehouse","Pension",
                       "cottage","Dome house","Earth house")

for (i in 1:length(WierdPropTypeKeys)) {
  Bangkok <- Bangkok[-grep(WierdPropTypeKeys[i], Bangkok$property_type),]
}

Bangkok$property_type <- gsub("home/apt","apartment",
                              gsub("serviced ","",
                                   gsub("Entire ","",Bangkok$property_type)))

# Some Exotics Left: B&B, Bungalow, Quest Suite, GuestHouse, House, Loft, Townhouse, Villa
# Not materially different to apartments
# Legitimate alternatives from guests' POVs -> KEEP & Group together @ OTHER
Bangkok$property_type[!Bangkok$property_type %in% c("apartment","condominium")] <- "Other"

Bangkok <- Bangkok %>% rename(f_property_type = property_type)
Bangkok$f_property_type <- factor(Bangkok$f_property_type)

PropCols <- PropCols[!PropCols %in% "property_type"]

# 2.5.3) Bathroom Text -> Bathrooms figures

# +1: bathrooms_text
Bangkok$n_bathrooms <- as.numeric(
  trimws(
    gsub("Half-",0.5,
         gsub("s","",
              gsub("bath","",
                   Bangkok$bathrooms_text)))))
Bangkok$bathrooms_text <- NULL
PropCols <- PropCols[!PropCols %in% "bathrooms_text"]

# 2.5.4) Beds / Bedrooms / Accommodates -> Accomms 2-6 pax
Accoms <- Bangkok[VarDescribe[Vars %in% PropCols[1:3],"Vars"]]

# Nr. Beds & Bedrooms dont seems to be well affected by Nr. Accomms...
Accoms %>% ggplot(aes(x=accommodates,y=beds)) + 
  geom_point(position = "jitter", width = 0.0, height = 0) 

Accoms %>% ggplot(aes(x=accommodates,y=bedrooms)) + 
  geom_point(position = "jitter", width = 0.0, height = 0) 

Bangkok <- Bangkok %>% filter(accommodates >= 2 & accommodates <= 6) %>% 
  rename(n_beds = beds,
         n_bedrooms = bedrooms, 
         n_accommodates = accommodates)


# Prop Var.s left = AMENITIES -> @ End of cleaning

#### 2.6) Sales -> Price (in Thai Baht) & Min/Max Stay restrictions ####
# All numeric Except price
SalesCols <- VarDescribe[VarGroups == "Sales","Vars"]

# 2.6.1)  PRICE - Clean string & convert to USD
Bangkok$price <- as.numeric(gsub('[^[:digit:].]',"",Bangkok$price))
Bangkok$usd_price <- Bangkok$price*0.033 # Thai Baht / USD = 0.033
Bangkok$price <- NULL

SalesCols <- SalesCols[!SalesCols %in% "price"]

# 2.6.2) Stay restrictions -> Check similarity 
# Count proportion of rows where values aint exactly equal
CheckVarSimilarity <- function(Keyword, ColnameVector) {
  Cols2Check <- grep(Keyword, ColnameVector, value = T)
  
  ColSimilarity <- NULL
  for (i in 1:length(Cols2Check)) {
    for (j in 1:length(Cols2Check)) {
      ColSimilarity[(i-1)*length(Cols2Check)+j] <- round(
        sum(Bangkok[Cols2Check][i] == Bangkok[Cols2Check][j], na.rm= T) /nrow(Bangkok),3)
    }
  }
  # They seem quite different, but I definitely do not need info on rooms
  return(matrix(ColSimilarity,nrow = length(Cols2Check), ncol = length(Cols2Check)))  
}

# Minimum Night Cols -> 97.5 - 98.6% identical -> No loss of info by throwing out
CheckVarSimilarity("minimum_nights",SalesCols)
Bangkok[grep("minimum_nights",SalesCols, value = T)[-1]] <- NULL
SalesCols <- SalesCols[!SalesCols %in% grep("minimum_nights",SalesCols, value = T)]

# Maximum Night Cols -> Max Nights 88.8% identical w Others, rest 99.7% identical w eachother
# Keep 2 of the 4 -> Max Nights, Max Nights Avg.
CheckVarSimilarity("maximum_nights",SalesCols)
Bangkok[SalesCols[!SalesCols %in% c("maximum_nights","maximum_nights_avg_ntm")]] <- NULL
Bangkok <- Bangkok %>% rename(n_min_nights = minimum_nights,
                              n_max_nights = maximum_nights,
                              n_max_nights_avg = maximum_nights_avg_ntm)

#### 2.7) Availability - Exc. Logicals ####
# In terms of pricing -> How many days are sold seems more intuitive to be relevant  

AvailCols <- VarDescribe[(VarGroups == "Availability" & VarType != "logical") ,"Vars"]

summary(Bangkok[AvailCols])

for (i in 1:length(AvailCols)) {
  Bangkok[AvailCols][i] <- abs(Bangkok[AvailCols][i] - as.numeric(gsub("[^[:digit:]]","",AvailCols))[i])  
}

Bangkok <- Bangkok %>% rename(n_sales_30 = availability_30,
                              n_sales_60 = availability_60,
                              n_sales_90 = availability_90,
                              n_sales_365 = availability_365)

SalesVar <- c("n_sales_30","n_sales_60","n_sales_90","n_sales_365")

# Check how identical availability columns are
CheckVarSimilarity("n_sales", SalesVar)
# Seems materially different -> KEEP ALL  
#  80.6 - 84.5% (30-90 days out) & 44-47% vs 365days out 


#### 2.8) Satisfaction i.e. Reviews ####
SatCols <- VarDescribe[VarGroups == "Satisfaction","Vars"]

summary(Bangkok[SatCols]) # Similar number of NAs in Review_Scores

# Check if missing for same rows
rowNum <- 1:nrow(Bangkok)
CountNAs <- rowSums(is.na(Bangkok[SatCols]))
data.frame(rowNum,CountNAs) %>% group_by(CountNAs) %>% 
  summarize(RowCount = n())
# Of ca. 3530 rows w NAs 3418 has all NAs -> 

# 2.8.1) Review Scores
summary(Bangkok[SatCols][8:13]) 
# review score rating aggregate of 6 sub-categories
# Cats: Accuracy, Cleanliness, Check-in, Communication, Location, Value
# Seems important for guest satisfaction analysis -> TOO MUCH detail for overall pricing
# +1s: Location & Perceived-value information inside amenities, neighborhood
# DROP SUB-CATEGORIES
Bangkok[SatCols[8:13]]  <- NULL
SatCols <- SatCols[!SatCols %in% SatCols[8:13]]

# 2.8.2) Review Frequency -> Grand scheme of things - 
# ONLY total Reviews & Reviews/Month of interest
Bangkok[SatCols[3:4]] <- NULL
SatCols <- SatCols[!SatCols %in% SatCols[3:4]]
summary(Bangkok[SatCols])

# 2.8.3) Inpute NAs for Revs/Month & Rev Score
Bangkok[SatCols] %>% group_by(number_of_reviews) %>% summarize(mean(review_scores_rating))

# 0 Revs -> 0 Revs/month + flag var
Bangkok$flag_reviews_per_month <- ifelse(is.na(Bangkok$reviews_per_month), 1,0)
Bangkok$reviews_per_month <- ifelse(is.na(Bangkok$reviews_per_month), 0, Bangkok$reviews_per_month )

# 0 Reviews -> Mean Rev.Score + flag var
# More sensitive to outliers BUT -> No reviews means added uncertainty for guests
Bangkok$flag_review_scores_rating <- ifelse(is.na(Bangkok$review_scores_rating),1,0)  
Bangkok$review_scores_rating <- ifelse(is.na(Bangkok$review_scores_rating),
                                       mean(Bangkok$review_scores_rating,na.rm = T),
                                       Bangkok$review_scores_rating )

Bangkok <- Bangkok %>% rename(n_number_of_reviews = number_of_reviews,
                              n_review_scores_rating = review_scores_rating,
                              n_reviews_per_month = reviews_per_month)

#### 2.9) Date Vars ####
# 2 time-related infor of interest:
# 1st How long has property had visitors -> time since first review -> best proxy available
# 2nd Low long since last time it had visitors -> time since last review -> best proxy available
DateVars <- VarDescribe[VarType == "Date","Vars"]

# Rest of date-related data not relevant
Bangkok <- Bangkok %>% 
  mutate(n_days_since_1st = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                         as.Date(first_review ,format="%Y-%m-%d")),
         n_days_since_last = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                          as.Date(last_review ,format="%Y-%m-%d"))) %>% 
  dplyr::select(-c(calendar_last_scraped,first_review,last_review, host_since))


colnames(Bangkok)

#### 2.10) Dummies from Amenities ####
Bangkok$amenities <- as.list(strsplit(gsub("\\[","",
                                           gsub("\\]","",
                                                gsub('\\"',"",
                                                     gsub("\\}","",
                                                          gsub("\\{","",
                                                               Bangkok$amenities))))),","))


# 2.10.1) get vector of raw factor levels & Dataframe of dummies if in level 
Levels <- levels(factor(unlist(Bangkok$amenities)))
DummyTable <- as.data.frame(do.call(rbind, lapply(lapply(Bangkok$amenities, factor, Levels), table)))
#colnames(DummyTable) <- paste0("d_",trimws(colnames(DummyTable)))

# 2.10.2) Function to reduce feature space
#   grep function -> find colnames which are substring of keyword
#   Define New Column with correct name <- ifelse(any(subsetted columns == 1),1,0)
#   Delete subsetted columns
#   Keywords thought by eyeballing: Wifi, HDTV, Dedicated WorkSpace, Aro Conditioner
test <- DummyTable
keywords <- c("wifi|ethernet","HDTV|TV","Dedicated.*Workspace",
              "Paid.*Parking|Paid.*Garage","Free.*Parking|Free.*Garage",
              "Clothing.*Storage","Refrigerator",
              "Fitness|Gym|Sauna|Hot.*tub|Pool|bath.*tub",
              "stove","Sound.*System","shampoo|conditioner|soap|shower.*gel",
              "hot.*water","Washer","air.*condition",
              "Smart.*Lock|Smoke.*Alarm|Safe|Lockbox",
              "Dryer", "Kitchen","Oven","Heater",
              "Children|Baby|crib","Microwave","garden","breakfast")




CoerceDummiesAdvanced <- function(df_w_Dummies, keywords_Vector) {
  NewColName <- NULL
  for (j in 1:length(keywords_Vector)) {
    print(paste0("Coercing columns including keyword Nr. ",j,": ", keywords_Vector[j]))
    
    #-----------------------------
    # Finding Columns
    ColMatchestest <- df_w_Dummies %>% dplyr::select(matches(keywords_Vector[j])) %>% colnames()
    NewColName[j] <- unlist(str_split(keywords_Vector[j],"\\|"))[1]
    
    #---------------------------------  
    # Coercing Columns
    print(paste0(length(ColMatchestest)," matching Columns were found!!!"))
    NewColl <- NULL
    for (i in 1:nrow(df_w_Dummies)) {
      NewColl[i] <- ifelse(any(df_w_Dummies[i,ColMatchestest] == 1),1,0)
    }
    
    df_w_Dummies[ColMatchestest] <- NULL
    df_w_Dummies <- cbind(df_w_Dummies, NewColl)
    
    colnames(df_w_Dummies)[length(colnames(df_w_Dummies))] <- paste0("d_",NewColName[j])
  }
  return(df_w_Dummies)
}

PostKeywords <- CoerceDummiesAdvanced(test,keywords)

# 2.10.3) Check remaining Duplicates
colnames(PostKeywords) <- trimws(colnames(PostKeywords))
Dups <- colnames(PostKeywords)[which(duplicated(colnames(PostKeywords)))]

# Function with same purpose but uses different subsetting - that allows duplicate columns
CoerceDummyVarColumns <- function(df_w_Dummies,Keywords_Vector, SplitStrings = T) {
  for (j in 1:length(Keywords_Vector)) {
    print(paste0("Coercing columns including keyword Nr. ",j,": ", Keywords_Vector[j]))
    
    if (SplitStrings == T) {
      # For 2+ word case -> split the string -> find all matches per splitted-string -> found in all = real match
      subwords <- trimws(unlist(str_split(Keywords_Vector[j]," ")))
      
      if (length(subwords) > 1) {
        matches <- list()
        
        # fill list w matching column names' Col Nrs. for each subword
        for (k in 1:length(subwords)) {
          matches[[k]] <- grep(tolower(subwords[k]),tolower(colnames(df_w_Dummies)))
        }
        # look for ColNumbers of shorter list in longer List -> 4 equal length doesnt matter
        if (length(matches[[1]]) >= length(matches[[2]])) {
          # Case when 1st list is longer
          ColMatchestest <- matches[[1]][matches[[1]] %in% matches[[2]]]
        } else {
          # Case when 2nd list is longer 
          ColMatchestest <- matches[[2]][matches[[2]] %in% matches[[1]]]
        }  
      } else {
        # 1 word case
        #  ColMatches <- grep(tolower(keywords[j]),tolower(colnames(DummyTable)))
        ColMatchestest <- grep(tolower(Keywords_Vector[j]),tolower(colnames(df_w_Dummies)))
      }
      
    } else {
      # 1 word case
      #  ColMatches <- grep(tolower(keywords[j]),tolower(colnames(DummyTable)))
      ColMatchestest <- grep(tolower(Keywords_Vector[j]),tolower(colnames(df_w_Dummies)))
    } 
    
    print(paste0(length(ColMatchestest)," matching Columns were found!!!"))
    
    NewColl <- NULL
    for (i in 1:nrow(df_w_Dummies)) {
      NewColl[i] <- ifelse(any(df_w_Dummies[i,ColMatchestest] == 1),1,0)
    }
    
    df_w_Dummies[ColMatchestest] <- NULL
    df_w_Dummies <- cbind(df_w_Dummies, NewColl)
    
    colnames(df_w_Dummies)[length(colnames(df_w_Dummies))] <- paste0("d_",Keywords_Vector[j])
    
  }
  return(df_w_Dummies)
}
DupsRemoved <- CoerceDummyVarColumns(PostKeywords,Dups,SplitStrings = F)
colnames(DupsRemoved) <- gsub("[^[:alnum:]_]","_",colnames(DupsRemoved))

# 2.10.4) Find & Remove Dummy Cols w VERY few Trues -> say < 1% TRUE = 1
ManyFalses <- Dummies_w_Many_Falses(DupsRemoved)
DupsRemoved[ManyFalses$Colname] <- NULL

# 2.10.5) Add back to df for final datatable
Bangkok <- cbind(Bangkok,DupsRemoved)
Bangkok$amenities <- NULL
#### Renaming ####


# Start all varnames w l_,d_,n_,flag_,p_,usd_
oldnames <- Bangkok %>% dplyr::select(-matches("^l_.*|^d_.*|^n_.*|^flag_.*|^f_.*|^p_.*|^usd_.*")) %>% colnames()
torename <- match(oldnames,colnames(Bangkok))

colnames(Bangkok)[torename] <- paste0("d_",oldnames)



#### Save Files ####
CleanDataPath <- paste0(getwd(),"/Prediction_Projects-CEU_DA3/Bangkok_Airbnb/Data/Clean/")
# CSV & RDS
write_csv(Bangkok,paste0(CleanDataPath,"airbnb_bangkok_cleaned.csv"))
saveRDS(Bangkok,paste0(CleanDataPath,"airbnb_bangkok_cleaned.rds"))


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
df[c("usd_price")] %>% ggplot(aes(x = usd_price)) + 
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
# NA in these cases is most likely 0 -> though at lest 1 night & 1 listing is implied

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
