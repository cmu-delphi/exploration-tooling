# Version-faithful slicing of an epi_archive for a forecast (REFACTOR.md Exp 3).
# Replaces the per-target `grepl("latest", id)` branching with an explicit
# version_policy carried on the forecaster grid.
#
# version_policy:
#   "as_of"  - data as of the generation date (min with the archive's end); the
#              version-faithful slice used by every forecaster except the latest ones.
#   "latest" - newest available data, truncated to before latest_cutoff; used by
#              the *_latest forecasters that intentionally peek at current data.
#
# latest_cutoff defaults to generation_date. forecast_nhsn overrides it to the
# forecast date instead of the generation date -- a long-standing asymmetry vs
# its siblings (full_data, forecast_nssp), preserved here verbatim.
flu_slice_archive <- function(archive, version_policy, generation_date, latest_cutoff = generation_date) {
  if (version_policy == "latest") {
    archive %>%
      epix_as_of(archive$versions_end) %>%
      filter(time_value < as.Date(latest_cutoff))
  } else {
    archive %>%
      epix_as_of(min(as.Date(generation_date), archive$versions_end))
  }
}
