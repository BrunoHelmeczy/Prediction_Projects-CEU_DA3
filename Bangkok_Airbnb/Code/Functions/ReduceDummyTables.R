

# Amenities
Bangkok$amenities <- as.list(strsplit(gsub("\\[","",
                                           gsub("\\]","",
                                                gsub('\\"',"",
                                                     gsub("\\}","",
                                                          gsub("\\{","",
                                                               Bangkok$amenities))))),","))

### Defining Levels & Dummies

# 1) get vector of raw factor levels & Dataframe of dummies if in level 
Levels <- levels(factor(unlist(Bangkok$amenities)))
DummyTable <- as.data.frame(do.call(rbind, lapply(lapply(Bangkok$amenities, factor, Levels), table)))
colnames(DummyTable) <- paste0("d_",trimws(colnames(DummyTable)))

# 2) Function to reduce feature space
#   grep function -> find colnames which are substring of keyword
#   Define New Column with correct name <- ifelse(any(subsetted columns == 1),1,0)
#   Delete subsetted columns
#   Keywords thought by eyeballing: Wifi, HDTV, Dedicated WorkSpace, Aro Conditioner
#   Backup plan: string distance calculation
test <- DummyTable
keywords <- c("wifi","HDTV","Dedicated Workspace","Paid Parking","Free Parking",
              "Pool","Clothing Storage","Refrigerator","Gym",
              "stove","TV","Sound System","shampoo","conditioner",
              "hot water","Washer","air condition","Smart Lock","Smoke Alarm",
              "Dryer", "Kitchen","Sauna","Oven","Safe","Heater","Garage","crib",
              "Children","Baby","Microwave","Hot tub","soap","garden","breakfast")

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
    for (i in 1:nrow(test)) {
      NewColl[i] <- ifelse(any(df_w_Dummies[i,ColMatchestest] == 1),1,0)
    }
    
    df_w_Dummies[ColMatchestest] <- NULL
    df_w_Dummies <- cbind(df_w_Dummies, NewColl)
    
    colnames(df_w_Dummies)[length(colnames(df_w_Dummies))] <- paste0(
      Keywords_Vector[j],"_re_fct")
    
  }
  return(df_w_Dummies)
}

test_postkeywds <- CoerceDummyVarColumns(test,keywords)
