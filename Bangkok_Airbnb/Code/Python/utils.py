import pandas as pd
import numpy as np
from pathlib import Path

def FindDateCols(Data):
    CharCols = Data.dtypes[Data.dtypes == 'object'].index.to_list()

    DateCols = Data.loc[:, CharCols].astype(str).apply(
        func = lambda x: x.str.match(r'\d{4}-\d{2}-\d{2}').any()
    )

    return DateCols[DateCols].index.to_list()

def ConvertDateCols(Data, DateCols):
    Data.loc[:, DateCols] = Data.loc[:, DateCols].agg(
        lambda x: pd.to_datetime(x, format = '%Y-%m-%d')
    )

    return Data

def LoadData():
    file_name = "http://data.insideairbnb.com/thailand/central-thailand/bangkok/2020-12-23/data/listings.csv.gz"

    raw = pd.read_csv(file_name, index_col = None)

    out = ConvertDateCols(
        Data = raw,
        DateCols = FindDateCols(raw)
    )

    return out

def StoreData(Data):
    write_path = next(Path.cwd().rglob('Bangkok_*/*/Raw'))
    write_file = write_path.joinpath('airbnb_bangkok_raw.csv')

    Data.to_csv(write_file, index = False)
    return f"Data written to path: {write_file} \n"

def DropUnusedCols(Data):
    drops = ["host_thumbnail_url","host_picture_url",
                "listing_url","picture_url",
                "host_url","last_scraped",
                "description","neighborhood_overview",
                "host_about","host_response_time",
                "name","space","host_location"]

    Data.drop(drops, inplace = True, axis = 1, errors = 'ignore')

    NAs = Data.isna().sum()
    NAcols = NAs.index[NAs / Data.shape[0] == 1]

    Data.drop(NAcols, inplace = True, axis = 1, errors = 'ignore')

def DescribeVariables(Data):
    VarGroups = [
        ['IDs'] * 4
        , ['Host'] * 10
        , ['Geo'] * 4
        , ['Property'] * 7
        , ['Sales'] * 9
        , ['Availability'] * 5
        , ['Satisfaction'] * 13
        , ['Availability']
        , ['CalcListings'] * 4
        , ['Satisfaction']
    ]


    df = pd.DataFrame(
        data = {
            'VarType': Data.dtypes,
            'VarGroups': [item for subitem in VarGroups for item in subitem]
        }
    )

    DateCols = FindDateCols(Data)
    df.loc[DateCols, 'VarType'] = 'Date'

    return df

def DropCols(Data, Cols):
    Data.drop(
        Cols, 
        axis = 1,
        inplace = True,
        errors = 'ignore'
    )

def DropRows(Data, Index):
    Data.drop(
        Index, 
        axis = 0,
        inplace = True,
        errors = 'ignore'
    )

def CalcColumnSimilarity(Data, ColumnList):
    check = Data.loc[:, ColumnList]
    vector_length = len(ColumnList)

    ColSimilarity = []
    for i in range(vector_length):
        for j in range(vector_length):
            if i == j:
                ColSimilarity.append(1)
            else:
                nr_sames = sum(check[ColumnList[i]] == check[ColumnList[j]])
                ColSimilarity.append(round(nr_sames / check.shape[0], 3))

    return pd.DataFrame(
        data    = np.array(ColSimilarity).reshape(vector_length, vector_length),
        index   = ColumnList,
        columns = ColumnList
    )

def DetectBooleans(Data):
    LogicCols = Data.astype(str).apply(
        lambda x: x.str.match(r"^t$|^f$|^nan$").all()
    )

    return LogicCols[LogicCols].index.to_list()

def convertBooleans(Data, Cols):
    Data.fillna({col: 'f' for col in Cols}, inplace = True)

    Data.replace('t', 1, inplace = True)
    Data.replace('f', 0, inplace = True)

    Data.loc[:, Cols] = Data.loc[:, Cols].astype({col: int for col in Cols})

    Data.rename(
        {col: f"l_{col}" for col in Cols},
        inplace = True
    )

def CoerceDummies(Data, keywords):
    for kw in keywords:
        ColMatchesTest = Data.columns[Data.columns.str.lower().str.contains(kw.lower(), regex = True)]
        NewColName = pd.Series(kw).str.replace(r'\|.+', '', regex = True).to_list()[0]

        Data[NewColName] = Data.loc[:, ColMatchesTest].agg(
            lambda x: int(sum(x) > 0)
            , axis = 1
        )

        DropCols(Data, ColMatchesTest)
    
    cols_out = Data.columns[Data.mean() > 0.01]

    return Data.loc[:, cols_out]

def AddAmenitiesCols(Data):
    Data['amenities'] = Data['amenities'].str.replace(r'\[|\]|\"|\{|\}', '', regex = True) \
        .str.split(',') \
        .apply(lambda X: [x.strip() for x in X]) \
        .apply(lambda X: (pd.Series(X).str.replace(r'[^0-9a-zA-Z]', '_', regex = True).to_list()))

    Data['id'] = range(Data.shape[0])
    Data['dummy'] = 1

    DummyTable = Data[['id', 'dummy', 'amenities']].explode('amenities').pivot(
        index = 'id',
        columns = 'amenities',
        values = 'dummy'   
    ).fillna(0).agg(lambda x: x.astype(int)).reset_index()

    DropCols(DummyTable, ['amenities',''])

    keywords = ["wifi|ethernet","HDTV|TV","Dedicated.*Workspace",
                    "Paid.*Parking|Paid.*Garage","Free.*Parking|Free.*Garage",
                    "Clothing.*Storage","Refrigerator",
                    "Fitness|Gym|Sauna|Hot.*tub|Pool|bath.*tub",
                    "stove","Sound.*System","shampoo|conditioner|soap|shower.*gel",
                    "hot.*water","Washer","air.*condition",
                    "Smart.*Lock|Smoke.*Alarm|Safe|Lockbox",
                    "Dryer", "Kitchen","Oven","Heater",
                    "Children|Baby|crib","Microwave","garden","breakfast"]
    
    PostKeywords = CoerceDummies(DummyTable, keywords)

    out = Data.merge(
        PostKeywords, 
        on = 'id', 
        how = 'left'
    )

    DropCols(out, ['id', 'amenities'])

    out.loc[:, PostKeywords.columns[1:]].fillna(0, inplace = True)

    return out

# Cleaning Steps Helpers

# convert logicals
# delete id columns
# clean host info
    # percentize host resp/acceptance rate cols
    # get nr verifications
    # clean host_neighbourhood
    # keep host listings cound
# drop geospatial_cols
# clean property info
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