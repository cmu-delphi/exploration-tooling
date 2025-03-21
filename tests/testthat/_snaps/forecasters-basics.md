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

# no_recent_outcome deals with no as_of

    Code
      res <- forecaster[[2]](jhu, "case_rate", extra_sources = "death_rate", ahead = 2L)

