import pandas as pd
from pathlib import Path

def LoadData():
    file_name = "http://data.insideairbnb.com/thailand/central-thailand/bangkok/2020-12-23/data/listings.csv.gz"

    raw = pd.read_csv(file_name, index_col = None)
    return raw

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

    return df

def convertBooleans(Data, Cols):
    Data.fillna({col: 'f' for col in Cols}, inplace = True)

    Data.replace('t', 1, inplace = True)
    Data.replace('f', 0, inplace = True)

    Data.loc[:, Cols] = Data.loc[:, Cols].astype({col: int for col in Cols})

    Data.rename(
        {col: f"l_{col}" for col in Cols},
        inplace = True
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
        data = np.array(ColSimilarity).reshape(vector_length, vector_length),
        index = ColumnList,
        columns = ColumnList
    )