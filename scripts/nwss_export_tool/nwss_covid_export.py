from datetime import datetime as dt
from urllib.request import urlopen
import pandas as pd
import numpy as np
import json

# Pull JSON from Trend Over Time link on CDC web page below
# https://www.cdc.gov/nwss/rv/COVID19-statetrend.html
response = urlopen("https://www.cdc.gov/wcms/vizdata/NCEZID_DIDRI/NWSSStateLevel.json")
json_data = response.read()
json_data = json.loads(json_data)
df = pd.json_normalize(json_data)

# date_period is literally just duplicated data filtered by 1 year, 9 Months, 45 Days, or all data avaliable
df = df[df["date_period"] == "All Results"]
df.activity_level = df.activity_level.replace("No Data", np.nan)

# State abbreviation crosswalk
df_states = pd.read_csv("./data/states.csv")
df_states.Abbreviation = df_states.Abbreviation.str.lower()
df_states

df = pd.merge(df, df_states, on=["State"])
df.rename(columns={"Abbreviation": "geo_value", "date": "time_value"}, inplace=True)

# convert values for graphs below
df.time_value = pd.to_datetime(df.time_value)
df.state_med_conc = df.state_med_conc.astype(np.float64)
df.activity_level = df.activity_level.astype(np.float64)
df.region_value = df.region_value.astype(np.float64)
df.national_value = df.national_value.astype(np.float64)

df = df[
    [
        "time_value",
        "geo_value",
        "state_med_conc",
        "activity_level",
        "region_value",
        "national_value",
    ]
]
file_name = f'../../aux_data/nwss_covid_data/nwss_{dt.today().strftime("%Y%m%d")}.csv'
df.to_csv(file_name, index=False)

print(f"Exported {df.shape[0]} rows to {file_name}")
