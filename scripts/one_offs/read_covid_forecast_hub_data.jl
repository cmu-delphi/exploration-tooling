# this was run from within the https://github.com/reichlab/covid19-forecast-hub repo,
# specifically in the model-output folder
# cd("../../../covid19-forecast-hub/model-output")
# if started here
# to get the rds, run
#
# full_results <- readr::read_csv(here::here("cache/covid19-2024season-results.csv"))
# aws.s3::s3save(full_results, object = "covid19_forecast_hub_2024_full.rds", bucket = "forecasting-team-data")
#
using Base: floatrange
using CSV
using DataFrames
using DataFramesMeta
using Dates
using RData
import Base.lowercase
pwd()
res = CSV.read("CovidHub-ensemble/2024-11-23-CovidHub-ensemble.csv", DataFrame)
pathname = "CovidHub-ensemble"
filename = "2024-11-23-CovidHub-ensemble.csv"
state_names = CSV.read("../auxiliary-data/locations.csv", DataFrame)
lowercase(m::Missing) = m
@rtransform! state_names @passmissing :abbreviation = lowercase(:abbreviation)
@select! state_names :abbreviation :location
format_file(pathname, filename, state_names)
function format_file(pathname, filename, state_names)
    if length(filename) < 10 ||
       match(r"[0-9]{4}-[0-9]{2}-[0-9]{2}", filename[1:10]) == nothing ||
       Date(filename[1:10]) < Date(2023, 1, 1)
        return DataFrame()
    end
    println(joinpath(pathname, filename))
    res = CSV.read(joinpath(pathname, filename), DataFrame, missingstring="NA", types=Dict("value" => Float64))
    if "forecast_date" in names(res)
        @rename! res :reference_date = :forecast_date
    end
    if !("reference_date" in names(res)) ||
       (res[!, :reference_date] |> minimum) < Date(2023, 1, 1)
        return DataFrame()
    end
    res = @chain res begin
        # old format problem, ahead is now recorded elsewhere
        #@rtransform :target = parse(Int64, match(r"[0-9]*", :target).match)
        @transform :forecaster = pathname[3:end]
        @rsubset :output_type == "quantile"
    end
    res = leftjoin(res, state_names, on=:location)
    names(res)
    res[!, :output_type_id]
    @select res :forecaster :geo_value = :abbreviation :forecast_date = :reference_date :target_end_date :ahead = :horizon :quantile = :output_type_id :value
    # this is for converting daily forecasts into weekly, whereas this script is currently downloading weekly forecasts
    #@chain res begin
    #    @rtransform :week_ahead = div(:ahead, 7)
    #    @groupby :forecaster :geo_value :reference_date :week_ahead :quantile
    #    @combine :value = sum(:value)
    #end
end
results = DataFrame[]
for (root, dirs, files) in walkdir(".")
    for file in files
        push!(results, format_file(root, file, state_names))
    end
end
maximum(size.(results, 2))
full_results = vcat(results...)
CSV.write("../../exploration-tooling/cache/covid19-2024season-results.csv", full_results)
pwd()
