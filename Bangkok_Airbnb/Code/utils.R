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

StoreData <- function(Data, cwd = getActiveProject()) {
  RawDataFolder <- grep('Raw', list.dirs(recursive = TRUE, full.names = FALSE), value = TRUE)
  WritePath <- paste0(c(cwd, RawDataFolder, ''), collapse = '/')
  
  fwrite(Data,  paste0(WritePath,"airbnb_bangkok_raw.csv"))
  saveRDS(Data, paste0(WritePath,"airbnb_bangkok_raw.rds"))
  
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
             "name","space","host_location")
  
  Data[, (drops) := NULL]
  
  # Also DROP All NA Cols
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

FillNAs <- function(DF, Cols = PercCols) {
  for (i in Cols) {
    DF[is.na(get(i)), (i) := 0]
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

