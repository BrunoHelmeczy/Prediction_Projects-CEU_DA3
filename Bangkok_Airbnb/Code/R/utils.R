# 1) Load/Store Data
LoadLibraries <- function(LibsVector) {
    invisible(sapply(LibsVector, function(l) {
        if (!require(l, character.only = TRUE)) {
            install.packages(l)
            require(l, character.only = TRUE)
        }
    }))
}

getDataStoragePath <- function(cwd = getActiveProject(), type = 'Raw') {
    DataFolder <- grep(type, list.dirs(recursive = TRUE, full.names = FALSE), value = TRUE)
    StoragePath <- paste0(c(gsub('^/', '', cwd), DataFolder, ''), collapse = '/')

    return(StoragePath)
}

StoreData <- function(Data, filepath) {
    cat('storing raw file as feather/csv...\n')
    Type <- tolower(type)

    arrow::write_csv_arrow(Data, paste0(filepath, "airbnb_bangkok_", Type, ".csv"))
    arrow::write_feather(Data, paste0(filepath,"airbnb_bangkok_", Type, ".feather"))

    outmessage <- paste0('Data written to path: \n', WritePath, '\n')

    cat(outmessage)
}

LoadData <- function() {
    # new file: 6.3s (readr)
    # existing: 0.4s (arrow)

    # trouble reading in gzip files with anything else then readr::read_csv()
        # TODO: store data --> data/Raw/bangkok_20201223.arrow
            # before reading from net, check if arrow exists with selected city+date

    storagePath <- getDataStoragePath()

    FeatherFilePath <- list.files(path = storagePath, pattern = '.+raw.feather', full.names = TRUE)

    if( length(FeatherFilePath) > 0) {
        cat('reading existing feather file...\n')
        Raw <- arrow::read_feather(FeatherFilePath)

    } else {
        options(readr.show_progress = FALSE)

        cat('reading raw file from AirBnB website...\n')
        File <- "http://data.insideairbnb.com/thailand/central-thailand/bangkok/2020-12-23/data/listings.csv.gz"

        Raw <- read_csv(
            File, 
            show_col_types = FALSE
        ) %>% 
            setDT()

        StoreData(Raw, storagePath)
    }

    return(DropUnusedCols(Raw))
}

# 2) Cleaning Utils
DropUnusedCols <- function(Data) {
    #### 1) Delete URLs & Text datas
    drops <- c("host_thumbnail_url","host_picture_url",
        "listing_url","picture_url",
        "host_url","last_scraped",
        "description","neighborhood_overview",
        "host_about","host_response_time",
        "name","space","host_location"
    )

    Data[, (drops) := NULL]

    NAs <- colSums(is.na(Data)) == nrow(Data)
    NAcols <- names(NAs)[NAs == TRUE]

    Data[, (NAcols) := NULL]

    return(Data)
}

DescribeVariables <- function(Data) {
    VarGroups <- c(
        rep("IDs",4),
        rep("Host",10),
        rep("Geo",4),
        rep("Property",7),
        rep("Sales",9),
        rep("Availability",5),
        rep("Satisfaction",13),
        "Availability",
        rep("CalcListings",4),
        "Satisfaction"
    )
  
    VarDescribe <- data.table(
        Vars = names(Data),
        VarGroups, 
        VarType = sapply(Data, class)
    )
    
    return(VarDescribe)
}

FillNAs <- function(DF, Cols = PercCols, fill_value = 0) {
    for (i in Cols) {
        DF[is.na(get(i)), (i) := fill_value]
    }
    return(DF)
}

CalcColumnSimilarity <- function(Data, ColVector) {
    ColSimilarity <- NULL

    for (i in 1:length(ColVector)) {
        for (j in 1:length(ColVector)) {
            sames <- sum(

            Data[, get(ColVector[i])] == Data[, get(ColVector[j])], na.rm = T)

            ColSimilarity[(i - 1) * length(ColVector) + j] <- round(sames / nrow(Data), 3)
        }
    }

    out <- matrix(
        ColSimilarity,
        nrow = length(ColVector), 
        ncol = length(ColVector)
    ) %>% data.table() %>% 
        setnames(ColVector) %>% 
        .[, Var := ColVector] %>% 
        setcolorder('Var') %>% 
        .[]

    return(out)
}

CollapsePropertyTypes <- function(df) {
  WierdPropTypeKeys <- c("Room","Castle","Entire cabin","chalet","dorm","hostel",
                        "place","Farm stay","Tiny house","Treehouse","Pension",
                        "cottage","Dome house","Earth house")

  out <- Bangkok[!(property_type %in% WierdPropTypeKeys)] %>% 
    .[, property_type := gsub('serviced |Entire ', '', property_type) %>% 
          gsub('home/apt', 'apartment', .)
        ] %>% 
    .[!(property_type %in% c('apartment', 'condominium')), 
        property_type := 'Other'] %>% 
    .[, property_type := factor(property_type)] %>% 
    setnames('property_type', 'f_property_type')

  return(out)
}

CoerceDummies2023 <- function(df_w_Dummies, keywords_Vector) {
    lapply(keywords_Vector, function(kw) {
        print(paste0("Coercing columns including keyword: ", kw))

        ColMatchestest <- grep(kw, names(df_w_Dummies), ignore.case = TRUE, value = TRUE)
        NewColName <- paste0('d_', gsub('\\|.+', '', kw))

        print(paste0(length(ColMatchestest)," columns found!!!"))

        Formula <- paste0(ColMatchestest, collapse = ' + ')
        df_w_Dummies[, (NewColName) := as.numeric((eval(parse(text = Formula))) >= 1)]
        df_w_Dummies[, (ColMatchestest) := NULL]

        return(NULL)
    })

    Freqs <- sapply(df_w_Dummies, mean)
    cols_out <- names(Freqs)[Freqs > 0.01]

    return(df_w_Dummies[, ..cols_out])
}

AddAmenitiesCols <- function(df) {
    df[, amenities := gsub('\\[|\\]|\\"|\\}|\\{', '', amenities) %>%
        strsplit(',') %>% 
        lapply(trimws)] %>%
        .[, id := .I]

    # 2.10.1) get vector of raw factor levels & Dataframe of dummies if in level 
    DummyTable <- df[, keyby = id, rbindlist(list(amenities)) ] %>%
        .[, value := 1] %>%
        dcast(id ~ V1, value.var = 'value', fill = 0)

    DummyTable %>%
        setnames(
        names(DummyTable) %>% gsub('[^0-9a-zA-Z]', '_', .) %>% paste0('d_', .)
        )

  # 2.10.2) Function to reduce feature space
    keywords <- c("wifi|ethernet","HDTV|TV","Dedicated.*Workspace",
                    "Paid.*Parking|Paid.*Garage","Free.*Parking|Free.*Garage",
                    "Clothing.*Storage","Refrigerator",
                    "Fitness|Gym|Sauna|Hot.*tub|Pool|bath.*tub",
                    "stove","Sound.*System","shampoo|conditioner|soap|shower.*gel",
                    "hot.*water","Washer","air.*condition",
                    "Smart.*Lock|Smoke.*Alarm|Safe|Lockbox",
                    "Dryer", "Kitchen","Oven","Heater",
                    "Children|Baby|crib","Microwave","garden","breakfast")

    PostKeywords <- CoerceDummies2023(DummyTable, keywords)

    # 2.10.5) Add back to df for final datatable
    out <- df %>%
        merge(PostKeywords, all.x = TRUE, by.x = 'id', by.y = 'd_id') %>% 
        FillNAs(Cols = names(PostKeywords)[-1]) %>% 
        .[, amenities := NULL] %>%
        .[, id := NULL]

    return(out)
}

# Cleaning Steps Helpers

# 1) convert logicals
getBooleanCols <- function() {
    c("host_is_superhost", "host_has_profile_pic", "host_identity_verified", 
        "has_availability", "instant_bookable"
    )
}

convertBooleans <- function(dt, cols = getBooleanCols()) {
    dt[, (cols) := lapply(.SD, as.numeric), .SDcols = cols] %>% 
        setnames(
            old = cols, 
            new = paste0("l_",cols)
        )
}

# 2) delete id columns
getIDCols <- function() {
    c("id" , "scrape_id" , "host_id" , "host_name")
}

dropIDs <- function(dt, cols = getIDCols()) {
    dt[, (cols) := NULL]
}

# 3) clean host info
getHostCols <- function() {
    c(
        "host_since"
        ,"host_response_rate"
        ,"host_acceptance_rate"
        ,"l_host_is_superhost"
        ,"host_neighbourhood"
        ,"host_listings_count"
        ,"host_total_listings_count"
        ,"host_verifications"
        ,"l_host_has_profile_pic"
        ,"l_host_identity_verified"
    )
}

# 3.1) percentize host resp/acceptance rate cols
getHostPercCols <- function() {c("host_response_rate","host_acceptance_rate")}

coerceCols2Prct <- function(dt, cols = getHostPercCols()) {
    dt[, (cols) := lapply(.SD, function(x) {
        gsub('%', '', x) %>% as.numeric()
    }), .SDcols = cols] %>% 
        FillNAs(cols) %>% 
        setnames(
            old = cols,
            new = paste0('p_', cols)
        )
}

# 3.2) get nr verifications
countHostVerifications <- function(dt) {
    dt[, id := .I] %>% 
        .[, keyby = id, n_host_verifications := length(str_split(host_verifications, ', ')[[1]])] %>% 
        .[, (c('id', 'host_verifications')) := NULL]
}

# 3.3) clean host_neighbourhood
standardizeHostNeighborhood <- function(dt, top_n_cities = 3) {
    # outside top3 > 3% of data / value -> 'Other'
    dt[, host_neighbourhood := factor(trimws(gsub(
        '[0-9]|Lower|Upper', '', 
        host_neighbourhood)))]

    HostCityFreq <- dt[!is.na(host_neighbourhood), keyby = host_neighbourhood, .N] %>% 
        .[order(-N)] %>% 
        .[, total := sum(N)] %>% 
        .[, cumsum := cumsum(N)] %>% 
        .[, cumsumprct := cumsum / total]

    topNhostcities <- HostCityFreq[1:top_n_cities, host_neighbourhood]

    dt[!(host_neighbourhood %in% topNhostcities), host_neighbourhood := 'Other']
}

# 3.4) keep host_listings_count only
getListingsCols <- function() {
    c(
        "host_listings_count"
        , "host_total_listings_count"
        , "calculated_host_listings_count"
        , "calculated_host_listings_count_entire_homes"
        , "calculated_host_listings_count_private_rooms"
        , "calculated_host_listings_count_shared_rooms"
    )
}

dropDupeListingCols <- function(dt, cols = getListingsCols()) {
    # Substantively -> No reason to believe listing counts affect price
    # Pricing market driven, not profit / cash-flow needs-based, so inventory should not affect prices
    # keep 1 Var, no need for further detail
    # CalcColumnSimilarity(
    #     Data = dt,
    #     ColVector = cols[1:2]
    # )
    # CalcColumnSimilarity(
    #     Data = dt,
    #     ColVector = cols
    # )

    dt[, (cols[-1]) := NULL] %>%
        setnames(
            old = cols[1],
            new = paste0('n_', cols[1])
        )
}


# 4) drop geospatial_cols

# 5) clean property info
    # room_type == "Entire home/apt"
    # CollapsePropertyTypes()
    # clean n_bathrooms + accommodates 2-6
# sales
    # price
    # stay restrictions
    # availability
# satisfaction
    # remove subreviews
    # NA inpute nr reviews + review score
# dates
    # calc days since columns
# amenities --> DONE