# this was run from within the https://github.com/reichlab/covid19-forecast-hub repo,
# specifically in the data-processed folder
# to get the rds, run
#
# full_results <- readr::read_csv("../covid19-forecast-hub/data-processed/covid19-2023season-results.csv")
# aws.s3::s3save(full_results, object = "covid19_forecast_hub_2023.rds", bucket = "forecasting-team-data")
#
using CSV
using DataFrames
using DataFramesMeta
using Dates
using RData
pwd()
res = CSV.read("COVIDhub-ensemble/2023-10-02-COVIDhub-ensemble.csv", DataFrame)
pathname = "COVIDhub-ensemble/"
filename = "2023-10-02-COVIDhub-ensemble.csv"
state_names = CSV.read("../data-locations/locations.csv", DataFrame)
lowercase(m::Missing) = m
@rtransform! state_names @passmissing :abbreviation = lowercase(:abbreviation)
@select! state_names :abbreviation :location

function format_file(pathname, filename, state_names)
    if length(filename) < 10 ||
       match(r"[0-9]{4}-[0-9]{2}-[0-9]{2}", filename[1:10]) == nothing ||
       Date(filename[1:10]) < Date(2023, 1, 1)
        return DataFrame()
    end
    println(joinpath(pathname, filename))

    res = CSV.read(joinpath(pathname, filename), DataFrame, missingstring="NA")

    if !("forecast_date" in names(res)) ||
       res[!, :forecast_date] |> minimum < Date(2023, 1, 1)
        return DataFrame()
    end
    @transform(res, :target = (:target))
    res = @chain res begin
        @rtransform :target = parse(Int64, match(r"[0-9]*", :target).match)
        @transform :forecaster = pathname
        @rsubset :type == "quantile"
    end
    res = leftjoin(res, state_names, on=:location)
    @select! res :forecaster :geo_value = :abbreviation :forecast_date :target_end_date :ahead = :target :quantile :value
    res
end
results = DataFrame[]
for (root, dirs, files) in walkdir(".")
    for file in files
        push!(results, format_file(root, file, state_names))
    end
end
full_results = vcat(results...)
CSV.write("covid19-2023season-results.csv", full_results)
full_results[!, :forecaster] |> unique
@rsubset! full_results :ahead % 7 == 0
@rtransform! full_results :forecaster = :forecaster[3:end]
"./fqfae"[3:end]
3 % 7
@rsubset full_results !ismissing(:geo_value) :forecast_date == Date(2023,11,13)
@rsubset res :forecast_date == Date(2023,11,0)
