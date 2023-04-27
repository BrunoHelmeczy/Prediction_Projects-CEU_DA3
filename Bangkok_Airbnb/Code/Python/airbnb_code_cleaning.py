import pandas as pd
import numpy as np
import forex_python.converter as fx
from Bangkok_Airbnb.Code.Python.utils import *

Bangkok = LoadData()
StoreData(Bangkok)
# Data = Bangkok

# 2) Cleaning
DropUnusedCols(Bangkok)

VarDescription = DescribeVariables(Bangkok)

# 2.1 Drop ID cols
IDcols = VarDescription[VarDescription['VarGroups'] == 'IDs'].index

DropCols(Bangkok, IDcols)
DropRows(VarDescription, IDcols)

# 2.2 logicals --> convert to ints
LogicCols = ['host_is_superhost', 'host_has_profile_pic', 'host_identity_verified', 'has_availability', 'instant_bookable']

convertBooleans(
    Data = Bangkok, 
    Cols = LogicCols
)

DropRows(VarDescription, LogicCols)

# 2.3 Host info
HostCols = VarDescription[VarDescription['VarGroups'] == 'Host'].index

# 2.3.1 Resp/Acceptance Rates
PercCols = ["host_response_rate","host_acceptance_rate"] 

Bangkok.loc[:, PercCols] = Bangkok.loc[:, PercCols].replace(r"%", '', regex = True).fillna(0)
Bangkok.rename({col: f"p_{col}" for col in PercCols}, inplace = True)

DropRows(VarDescription, PercCols)

HostCols = HostCols[np.logical_not(HostCols.isin(PercCols))]

# 2.3.2 Verifications --> Nr host verifications
Bangkok['n_host_verifications'] = Bangkok['host_verifications'].str.split(',').apply(len)

DropCols(Bangkok, 'host_verifications')

# 2.3.3 neighbourhood --> top3 + 'Other'
Bangkok['host_neighbourhood'].replace(r'[0-9]|Lower|Upper', '', regex = True, inplace = True)

top3hostcities = Bangkok['host_neighbourhood'].value_counts()[:3].index

Bangkok.loc[
    np.logical_not(Bangkok['host_neighbourhood'].isin(top3hostcities)), 
    'host_neighbourhood'] = 'Other'

HostCols = HostCols[np.logical_not(HostCols.isin(['host_verifications', 'host_neighbourhood']))]

# 2.3.4 listing cols
ListingCols = HostCols[HostCols.str.contains('listing')]
OtherListings = VarDescription[VarDescription['VarGroups'] == 'CalcListings'].index

CalcColumnSimilarity(
    Data = Bangkok, 
    ColumnList = ListingCols
)

DropCols(Bangkok, ListingCols[1])
DropRows(VarDescription, ListingCols[1])

Bangkok['host_listings_count'].fillna(0, inplace = True)
Bangkok['host_listings_count'] = Bangkok['host_listings_count'].apply(int)

AllListings = np.append(OtherListings.values, ListingCols[0])

CalcColumnSimilarity(
    Data = Bangkok,
    ColumnList = AllListings
)

DropCols(Bangkok, AllListings[:-1])
DropRows(VarDescription, AllListings[:-1])

Bangkok.rename({AllListings[-1]: f"n_{AllListings[-1]}"}, inplace = True)

# 2.4 Geospatial Data
GeoCols = VarDescription[VarDescription['VarGroups'] == 'Geo'].index
NeighbCol = 'neighbourhood_cleansed'

DropCols(Bangkok, GeoCols[GeoCols != NeighbCol])

Bangkok.rename({NeighbCol: f"f_{NeighbCol}"}, inplace = True)
# 2.5 Prop Info
## 2.5.1 Room Types
PropCols = VarDescription[VarDescription['VarGroups'] == 'Property'].index.to_list()

DropRows(
    Bangkok,
    Bangkok[Bangkok['room_type'] != "Entire home/apt"].index
)

DropCols(Bangkok, 'room_type')
PropCols.remove('room_type')

## 2.5.2 Property Types
WierdPropTypeKeys = ["Room","Castle","Entire cabin","chalet","dorm","hostel",
    "place","Farm stay","Tiny house","Treehouse","Pension",
    "cottage","Dome house","Earth house"]

DropRows(
    Bangkok,
    Bangkok[Bangkok['property_type'].isin(WierdPropTypeKeys)].index
)

Bangkok['property_type'] = Bangkok['property_type'].str.replace(
        r'serviced |Entire', '', regex = True
    ).str.replace(
        r'home/apt', 'apartment', regex = True
    ).str.strip()

Bangkok.loc[
    Bangkok[np.logical_not(Bangkok['property_type'].isin(['apartment', 'condominium']))].index, 
    'property_type'
] = 'Other'

Bangkok.rename({'property_type': 'f_property_type'}, inplace = True)
PropCols.remove('property_type')

## 2.5.3 Bathroom Text
Bangkok['n_bathrooms'] = Bangkok['bathrooms_text'].str.replace(
        r"Half-", '0.5' , regex = True
    ).str.replace(
        r"bath|s", '', regex = True
    ).str.strip().astype('float')

DropCols(Bangkok, 'bathrooms_text')
PropCols.remove('bathrooms_text')

## 2.5.4 Beds/ Bedrooms / Accommodates
DropRows(
    Bangkok,
    Bangkok[np.logical_not(Bangkok['accommodates'].between(2, 6))].index
)

# 2.6 Sales
SalesCols = VarDescription[VarDescription['VarGroups'] == 'Sales'].index.to_list()

## 2.6.1 Price --> convert to USD
exchange_rate = fx.CurrencyRates().get_rate(
    base_cur = 'THB',
    dest_cur = 'USD' 
)

Bangkok['usd_price'] = Bangkok['price'].str.replace(
    r"\$|,",'', regex = True
).astype('float') * exchange_rate

DropCols(Bangkok, 'price')
SalesCols.remove('price')

## 2.6.1 Min/MaxNight Cols

# 2.7 Availability
# 2.8 Satisfaction
# 2.9 Dates
# 2.10 Amentities

# 3) Cleaning script up
    


