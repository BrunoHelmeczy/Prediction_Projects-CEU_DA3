CoerceDummiesAdvanced <- function(df_w_Dummies, keywords_Vector) {
  NewColName <- NULL
  for (j in 1:length(keywords_Vector)) {
    print(paste0("Coercing columns including keyword Nr. ",j,": ", keywords_Vector[j]))
    
    #-----------------------------
    # Finding Columns
    ColMatchestest <- df_w_Dummies %>% select(matches(keywords_Vector[j])) %>% colnames()
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