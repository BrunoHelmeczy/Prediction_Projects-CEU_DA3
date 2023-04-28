# 1) Load/Store Data
LoadLibraries <- function(LibsVector) {
    invisible(sapply(LibsVector, function(l) {
        if (!require(l, character.only = TRUE)) {
            install.packages(l)
            require(l, character.only = TRUE)
        }
    }))
}

LoadData <- function() {
    options(readr.show_progress = FALSE)

    File <- "http://data.insideairbnb.com/thailand/central-thailand/bangkok/2020-12-23/data/listings.csv.gz"

    Raw <- read_csv(
        File, 
        show_col_types = FALSE  
    ) %>% 
        setDT()

    return(Raw)
}

StoreData <- function(Data, cwd = getActiveProject(), type = 'Raw') {
    Type <- tolower(type)
    DataFolder <- grep(type, list.dirs(recursive = TRUE, full.names = FALSE), value = TRUE)
    WritePath <- paste0(c(gsub('^/', '', cwd), DataFolder, ''), collapse = '/')

    Sys.sleep(1) 
    # data.table seems to be too fast on 4 threads 
    # see: https://stackoverflow.com/questions/61823407/repeated-data-table-fread-and-fwrite-causes-permission-denied-error
    fwrite(Data,  paste0(WritePath,"airbnb_bangkok_", Type, ".csv"))
    saveRDS(Data, paste0(WritePath,"airbnb_bangkok_", Type, ".rds"))

    outmessage <- paste0('Data written to path: \n', WritePath, '\n')

    cat(outmessage)
}

# 2) Cleaning
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

            Data[, get(ColVector[i])] == Data[, get(ColVector[j])], 
               na.rm = T
            )

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

# df <- Bangkok
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

# df <- Bangkok
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
