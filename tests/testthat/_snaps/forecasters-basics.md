# scaled_pop deals with no as_of

    Code
      res <- forecaster[[2]](jhu, "case_rate", extra_sources = "death_rate", ahead = 2L)

# flatline_fc deals with no as_of

    Code
      res <- forecaster[[2]](jhu, "case_rate", extra_sources = "death_rate", ahead = 2L)

# smoothed_scaled deals with no as_of

    Code
      res <- forecaster[[2]](jhu, "case_rate", extra_sources = "death_rate", ahead = 2L)
    Condition
      Error in `rename()`:
      ! Can't rename columns that don't exist.
      x Column `slide_value_case_rate` doesn't exist.

# flusion deals with no as_of

    Code
      res <- forecaster[[2]](jhu, "case_rate", extra_sources = "death_rate", ahead = 2L)
    Condition
      Warning:
      No columns were selected in `add_role()`.
      Error in `dplyr::transmute()`:
      i In argument: `across(...)`.
      i In group 1: `geo_value = ak` and `source = nhsn`.
      Caused by error in `across()`:
      ! Can't compute column `gr_21_rel_change_case_rate`.
      Caused by error in `epiprocess::growth_rate()`:
      ! `x` contains duplicate values. (If being run on a column in an `epi_df`, did you group by relevant key variables?)

# no_recent_outcome deals with no as_of

    Code
      res <- forecaster[[2]](jhu, "case_rate", extra_sources = "death_rate", ahead = 2L)

