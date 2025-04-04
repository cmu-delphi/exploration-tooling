# /// script
# requires-python = ">=3.12"
# dependencies = [
#     "ipykernel>=6.29.5",
#     "ipython>=9.0.2",
#     "matplotlib>=3.10.1",
#     "openpyxl>=3.1.5",
#     "pandas>=2.2.3",
# ]
# ///

"""Process VA data.
This script processes the VA data into a unified format for forecasting.

I don't know how VA_Sta3nName.csv was generated, but I'm guessing it was made
from va_data/RosenfeldJhon_Past2500DaysAdmit_Start121724_Code1_6.xlsx (where
`va_data` is a Google Drive folder shared by Roni Rosenfeld containing their
dumps from data queries). Can always ask Ruiqi Lyu for answers.

Author: Dmitry Shemetov
Date: 2025-03-25
"""
# %%
from pathlib import Path

import pandas as pd

current_dir = Path.cwd()
df_ref = pd.read_csv(current_dir / "State_City_Sta3nName.csv")

# %% This part of the code is taken directly from VA_process.ipynb
state_dir = current_dir / 'Sta3nName'
if not state_dir.exists():
    state_dir.mkdir()

ref_date = pd.to_datetime("12/17/2024", format="%m/%d/%Y")

df = pd.read_csv(current_dir / 'VA_Sta3nName.csv')
Sta3nName_array = df['Sta3nName'].unique()
print(len(Sta3nName_array))
count_cols = [
    'National_Count_Unique_Patients',
    'National_Count_Unique_Patients_wFluICDCode',
    'National_Count_Unique_Patients_wCovidICDCode',
    'National_Count_Unique_Patients_wRsvICDCode'
]

df_overall = df[['PriorDayNum', 'National_Count_Unique_Patients', 'Sta3nName']].copy()
df_overall = df_overall.dropna(subset=['PriorDayNum'])
df_overall['date'] = ref_date - pd.to_timedelta(df_overall['PriorDayNum'].astype(int), unit='D')
df_overall.drop('PriorDayNum', axis=1, inplace=True)

df_flu = df[['PriorDayNum_wFluICDCode', 'National_Count_Unique_Patients_wFluICDCode', 'Sta3nName_wFluICDCode']].copy()
df_flu = df_flu.dropna(subset=['PriorDayNum_wFluICDCode'])
df_flu['date'] = ref_date - pd.to_timedelta(df_flu['PriorDayNum_wFluICDCode'].astype(int), unit='D')
df_flu.rename(columns={'Sta3nName_wFluICDCode': 'Sta3nName'}, inplace=True)
df_flu.drop('PriorDayNum_wFluICDCode', axis=1, inplace=True)

df_covid = df[['PriorDayNum_wCovidICDCode', 'National_Count_Unique_Patients_wCovidICDCode', 'Sta3nName_wCovidICDCode']].copy()
df_covid = df_covid.dropna(subset=['PriorDayNum_wCovidICDCode'])
df_covid['date'] = ref_date - pd.to_timedelta(df_covid['PriorDayNum_wCovidICDCode'].astype(int), unit='D')
df_covid.rename(columns={'Sta3nName_wCovidICDCode': 'Sta3nName'}, inplace=True)
df_covid.drop('PriorDayNum_wCovidICDCode', axis=1, inplace=True)

df_rsv = df[['PriorDayNum_wRsvICDCode', 'National_Count_Unique_Patients_wRsvICDCode', 'Sta3nName_wRsvICDCode']].copy()
df_rsv = df_rsv.dropna(subset=['PriorDayNum_wRsvICDCode'])
df_rsv['date'] = ref_date - pd.to_timedelta(df_rsv['PriorDayNum_wRsvICDCode'].astype(int), unit='D')
df_rsv.rename(columns={'Sta3nName_wRsvICDCode': 'Sta3nName'}, inplace=True)
df_rsv.drop('PriorDayNum_wRsvICDCode', axis=1, inplace=True)

station = Sta3nName_array[0]
for station in Sta3nName_array:
    station_clean = "".join([c if c.isalnum() else "" for c in str(station)])
    filename = Path("Sta3nName") / f"VA_{station_clean}.csv"

    df_overall_station = df_overall[df_overall['Sta3nName'] == station]
    df_flu_station = df_flu[df_flu['Sta3nName'] == station]
    df_covid_station = df_covid[df_covid['Sta3nName'] == station]
    df_rsv_station = df_rsv[df_rsv['Sta3nName'] == station]

    final_df = pd.merge(df_overall_station.drop('Sta3nName', axis=1), df_flu_station.drop('Sta3nName', axis=1), on='date', how='outer', suffixes=('', '_flu'))
    final_df = pd.merge(final_df, df_covid_station.drop('Sta3nName', axis=1), on='date', how='outer', suffixes=('', '_covid'))
    final_df = pd.merge(final_df, df_rsv_station.drop('Sta3nName', axis=1), on='date', how='outer', suffixes=('', '_rsv'))
    final_df = final_df[['date'] + count_cols]
    final_df = final_df.sort_values('date').reset_index(drop=True)

    for col in count_cols:
        if col in final_df.columns:
            final_df[col] = final_df[col].fillna(-1).astype(int)

    final_df.to_csv(current_dir / filename, index=False)


# %% This part of the code is taken from plot.ipynb
measures = [
    "National_Count_Unique_Patients",
    "National_Count_Unique_Patients_wFluICDCode",
    "National_Count_Unique_Patients_wCovidICDCode",
    "National_Count_Unique_Patients_wRsvICDCode",
]

state_aggregated = {}
for state in df_ref.State.unique():
    stations = df_ref[df_ref.State == state]["Sta3nName"]

    # Dictionary to hold lists of DataFrames for each measure.
    measure_dfs = {m: [] for m in measures}

    for station in stations:
        station_clean = "".join(c for c in str(station) if c.isalnum())
        filename = current_dir / "Sta3nName" / f"VA_{station_clean}.csv"
        if not filename.exists():
            print(f"File not found: {filename}. Skipping station {station}.")
            continue
        # Read the station CSV with date parsed.
        df_station = pd.read_csv(filename, parse_dates=["date"])
        # Replace -1 with NaN in the relevant columns.
        df_station[measures] = df_station[measures].replace(-1, 0)
        # Set index to date.
        df_station = df_station.set_index("date")
        # For each measure, add its Series to the list with a station-specific column name.
        for m in measures:
            measure_dfs[m].append(
                df_station[[m]].rename(columns={m: f"{m}_{station_clean}"})
            )

    # If no station data found for this state, skip it.
    if not any(measure_dfs[m] for m in measures):
        print(f"No station data found for state {state}")
        continue

    # For each measure, concatenate all station series along the columns (outer join on date)
    # then sum row-wise with min_count=1 (so if all station values are missing, result is NaN).
    agg_data = pd.DataFrame()
    for m in measures:
        if measure_dfs[m]:
            df_concat = pd.concat(measure_dfs[m], axis=1, join="outer")
            agg_series = df_concat.sum(axis=1, min_count=1)
            agg_data[m] = agg_series
    # Reset index so that date becomes a column.
    agg_data = agg_data.reset_index()
    # Optionally, sort the DataFrame by date.
    agg_data.sort_values("date", inplace=True)
    agg_data["state"] = state.lower()

    # Save this aggregated DataFrame for the state.
    state_aggregated[state] = agg_data

# Join the aggregated data into a single dataframe
veteran_state_df = pd.concat(state_aggregated.values(), axis=0, ignore_index=True)
veteran_state_df[["state", "date"] + measures].rename(
    columns={
        "National_Count_Unique_Patients": "unique_patients",
        "National_Count_Unique_Patients_wFluICDCode": "unique_patients_flu",
        "National_Count_Unique_Patients_wCovidICDCode": "unique_patients_covid",
        "National_Count_Unique_Patients_wRsvICDCode": "unique_patients_rsv",
    }
).to_csv(current_dir / "veteran_state_df.csv", index=False)


# %%
