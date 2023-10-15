# DeltaForecast

## A package, written in R, offering quick and easy prototyping tools for non-causal impact analysis.

All you need to get started is a time-series and an event for which you want to measure the impact on the time series. For instance, perhaps you wish to measure the impact of a new school curriculum on students' test scores. Given a time-series dataset of the students' test scores in a school and the date the new curriculum was implemented, `DeltaForecast` can determine whether test scores improved (with statistical significance) or nor following the new curriculum. 

NOTE: This package does not aim to identify causation - it serves as a preliminary assessment of whether or not any difference was perceived <em>following</em> an event, not necessarily <em>as a result</em> of that event. If you are interested in causation, <a href=https://github.com/google/CausalImpact>CausalImpact</a> provides this functionality if you have control time series to compare against. DeltaForecast builds on CausalImpact to use time-series forecasting in place of a synthetic control - i.e., models predict "what would have happened" in the absence of your event of interest, and compare this counterfactual against the observed/true data.

## Getting started
```r
library(DeltaForecast)

# If you have a path to your data, you can pass this in to DeltaForecast with the `path` parameter
# The following call tells DeltaForecast that the data is stored in "path/to/data.csv" (relative to the current
# working directory, each individual row in the data represents a day of data (i.e., time in this dataset is measured
# in days), the event of interest occurred 120 days (the temporal unit) after the beginning of the dataset, the
# column storing the temporal data is called "Date" in the data, and the column storing the target value to be measured
# is called "Scores".
impact <- DeltaForecast(
  path="path/to/data.csv", # Data is stored in "path/to/data.csv"
  event=120,               # Event of interest occurred 120 days (frequency) from the start of the dataset
  frequency="D",           # Each row in the dataset represents a single day
  temporal_col="Date",     # The column storing the temporal data in our dataset is called "Date"
  target_col="Scores"      # The colun storing the target data in our dataset is called "Scores"
)
```
