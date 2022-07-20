#
# Copyright: Copyright 2022, Integral Consulting Inc. All rights reserved.
#
# Title: merge_coord.py
# Purpose: Merge coordinates. Abandoned as there's not a clean merge. Elevations differ.
#
# Project Information:
#   Name: CMAT
#   Number: RD104
#
# History:
# Date	     Remarks
# 2022-05-27 Initially written. Eben Pendleton
#

# PACKAGES & SPECIAL FUNCTIONS ----
import pandas as pd

def main():
# LOAD DATA FILES & DATA HANDLING ----
    # QA: 7960 | Make sure data inputs are correct
    f1 = r"C:\Users\ependleton52\Documents\Projects\CMAT\Site Profiles\SPI_survey_profiles\merged_profiles.csv"
    df = pd.read_csv(f1)
    
    f2 = r'C:\Users\ependleton52\Documents\Projects\CMAT\Site Profiles\CBI_profiles_1995-2021_edited.csv'
    df2 = pd.read_csv(f2)
    
# ANALYSIS/FIGURES/OUTPUT ----

    ## drop any duplicates ----
    df3 =df.loc[:, ('date', 'NAVD88_ft', 'Line', 'X_ft', 'Y_ft', 'DOL_ft', 'srid', 'POINT_X', 'POINT_Y')].drop_duplicates()
    
    ## do some renaming in df3 ----
    # QA: 5689 | Ensure thse are correct
    df3['Line'] = df3['Line'].replace({'CBI-1': 'CBI-01', 'CBI-2': 'CBI-02', 'CBI-3': 'CBI-03', 'CBI-4': 'CBI-04', 'CBI-5': 'CBI-05', 'CBI-6': 'CBI-06'
    , 'CBI-7': 'CBI-07', 'CBI-8': 'CBI-08', 'CBI-9': 'CBI-09'})
    
    df2['survey_id'] = df2['survey_id'].replace({'CBI-1': 'CBI-01', 'CBI-2': 'CBI-02', 'CBI-3': 'CBI-03', 'CBI-4': 'CBI-04', 'CBI-5': 'CBI-05', 'CBI-6': 'CBI-06'
    , 'CBI-7': 'CBI-07', 'CBI-8': 'CBI-08', 'CBI-9': 'CBI-09'})

    ## merge ----
    # QA: 6789 | Ensure this is correct
    df4 = pd.merge(df2, df3, how = 'left', left_on = ['date_text', 'elevation', 'survey_id', 'dol'], right_on = ['date', 'NAVD88_ft', 'Line', 'DOL_ft'])
    
    # QA: 1235 | Assert this ensures the same shape
    assert df2.shape[0] == df4.shape[0]
    
    ## Update a few columns ----
    # QA: 2345 | Ensure these are named logically
    df4['x_ccord'] = df4['POINT_X']
    df4['y_coord'] = df4['POINT_Y']
    df4['srid_x'] = df4['srid_y']
    

    # QA: 6723 | Ensure this renames and keeps columns we don't want
    df4 = df4.rename(columns = {'srid_x': 'srid'})
    
    df4 = df4.loc[:, df2.columns]
    
    # QA: 1784 | Ensure this exports the right object
    df4.to_csv('CBI_profiles_1995-2021_edited_loca.csv', index = False)
    
if __name__ == "__main__":
    main()
    print('FIN')
    
    # QA: 3678 | test
    
    # QA: 7725 | test 2
    
    
    # QA: 6142 | comment
# QA: 5082 | comment
  # QA: 7776 | comment
# QA: 9971 | comment
# QA: 6565 | comment
# QA: 2782 | comment
# QA: 8993 | comment
# QA: 2469 | comment
# QA: 1439 | comment
# QA: 8397 | comment
# QA: 8555 | comment
# QA: 2292 | comment
# QA: 8674 | comment
# QA: 1881 | comment
# QA: 2196 | comment
# QA: 8930 | comment



