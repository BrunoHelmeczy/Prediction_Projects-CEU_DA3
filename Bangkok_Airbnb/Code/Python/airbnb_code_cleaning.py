# import pandas as pd
import numpy as np
from Bangkok_Airbnb.Code.Python.utils import *

Bangkok = LoadData()
StoreData(Bangkok)
# Data = Bangkok

# 2) Cleaning
DropUnusedCols(Bangkok)

VarDescription = DescribeVariables(Bangkok)

# 2.1 Drop ID cols
IDcols = VarDescription[VarDescription['VarGroups'] == 'IDs'].index

Bangkok.drop(IDcols, inplace = True, axis = 1, errors = 'ignore')
VarDescription.drop(IDcols, axis = 0, inplace = True, errors = 'ignore')

# 2.2 logicals --> convert to ints
LogicCols = ['host_is_superhost', 'host_has_profile_pic', 'host_identity_verified', 'has_availability', 'instant_bookable']

convertBooleans(
    Data = Bangkok, 
    Cols = LogicCols
)

VarDescription.drop(LogicCols, axis = 0, inplace = True, errors = 'ignore')

# 2.3 Host info
HostCols = VarDescription[VarDescription['VarGroups'] == 'Host'].index

# 2.3.1 Resp/Acceptance Rates
PercCols = ["host_response_rate","host_acceptance_rate"] 

Bangkok.loc[:, PercCols] = Bangkok.loc[:, PercCols].replace(r"%", '', regex = True).fillna(0)
Bangkok.rename({col: f"p_{col}" for col in PercCols}, inplace = True)

VarDescription.drop(PercCols, axis = 0, inplace = True, errors = 'ignore')

HostCols = HostCols[np.logical_not(HostCols.isin(PercCols))]

# 2.3.2 Verifications --> Nr host verifications
Bangkok['n_host_verifications'] = Bangkok['host_verifications'].str.split(',').apply(len)
Bangkok.drop('host_verifications', axis = 1, inplace = True)

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

Bangkok.drop('host_total_listings_count', axis = 1, inplace = True, errors = 'ignore')
VarDescription.drop(ListingCols[1], axis = 0, inplace = True, errors = 'ignore')
Bangkok['host_listings_count'].fillna(0, inplace = True)
Bangkok['host_listings_count'] = Bangkok['host_listings_count'].apply(int)

AllListings = np.append(OtherListings.values, ListingCols[0])

CalcColumnSimilarity(
    Data = Bangkok,
    ColumnList = AllListings
)

Bangkok.drop(AllListings[:-1], axis = 1, inplace = True, errors = 'ignore')
Bangkok.rename({AllListings[-1]: f"n_{AllListings[-1]}"}, inplace = True)

# 2.4 Geospatial Data
# 2.5 Prop Info
# 2.6 Sales
# 2.7 Availability
# 2.8 Satisfaction
# 2.9 Dates
# 2.10 Amentities

# 3) Cleaning script up
    


