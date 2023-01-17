#' @import modeltime
#' @import zoo
#' @import CausallImpact

pkg.env <- new.env()

# The column storing the time
pkg.env$.temporal_col <- NULL

# The column storing the response variable
pkg.env$.target_col <- NULL

# The column storing a numeric representation of the time
pkg.env$.temporal_num_col <- NULL

# The date of the first observation (only used when temporal_col is numeric)
pkg.env$.origin <- NULL

# The numeric representation of the origin
pkg.env$.num_origin <- NULL

# Standardization parameters (to be filled in later)
pkg.env$.mean <- NULL
pkg.env$.sd <- NULL

# How far the model should look to observe an impact
pkg.env$.horizon <- NULL

# Which unit of time each observation in the dataset represents
pkg.env$.frequency <- NULL

# Possible units of time (for the frequency)
pkg.env$.frequencies <- c("Y", "M", "D", "h", "m", "s")

# Possible seasonalities the dataset exhibits (may be a vector)
pkg.env$.seasonality <- NULL

# Seasonalities supported by this package
pkg.env$.seasonalities <- c("daily", "weekly", "yearly")

# Engines supported by this package
pkg.env$.supported_engines <- c(
  'ranger', 'randomForest', 'spark', 'xgboost', 'rpart',
  'prophet', 'prophet_xgboost', 'arima', 'auto_arima',
  'arima_xgboost', 'auto_arima_xgboost', 'lm', 'glm',
  'glmnet', 'stan', 'brulee'
)

# The metric (chosen by user) values for each model
pkg.env$.accuracies <- c()



#' Load in a CSV file as a tibble from a specified path
#'
#' @param data Optional argument (must be provided if 'path' is null) specifying
#'             a pre-loaded dataframe to be processed. If specified, 'path' will
#'             be ignored. The provided tibble will be checked and the relevant
#'             columns will be filtered.
#' @param path Optional argument(must be provided if 'data' is null) specifying
#'             a path to a CSV file. Will be ignored unless 'data' is null. The
#'             provided CSV file will be read, and the data will be checked and
#'             filtered.
#' @param origin Optional argument specifying the time of the earliest
#'               observation recorded in the dataset. This parameter will only
#'               be used if the temporal column of the dataset is 'numeric'
#'               instead of 'Date'.
#'
#' @return A tibble storing the temporal and target columns from the original
#'         dataframe.
#'
#' @importFrom readr read_csv
#' @importFrom assertthat assert_that
#' @importFrom janitor make_clean_names
#' @importFrom janitor clean_names
#' @importFrom dplyr select

#' @export
#'

LoadData <- function (data, path) {
  .temporal_col <- pkg.env$.temporal_col
  .target_col <- pkg.env$.target_col
  .origin <- pkg.env$.origin

  base_msg <- "Please make sure you have specified the"
  assertthat::assert_that(!is.null(.temporal_col), msg = paste(base_msg, "temporal_col"))
  assertthat::assert_that(!is.null(.target_col), msg = paste(base_msg, "target_col"))

  if (is.null(data)) {
    assertthat::assert_that(typeof(path) == "character",
                msg = paste("'path' must be a string pointing to the location",
                            "of the CSV file storing the time-series data"))
    assertthat::assert_that(file.exists(path),
                msg = "Could not locate provided CSV file - check your 'path'")

    data <- read_csv(path,
                     name_repair = janitor::make_clean_names,
                     show_col_types = FALSE)
  } else {
    assert_that(is.data.frame(data))
    data <- janitor::clean_names(data)
  }

  assert_that(typeof(data) == "list")

  # Discard everything besides the temporal and target columns
  data <- dplyr::select(data, {{ .temporal_col }}, {{ .target_col }})

  # Ensure the target column is numeric
  n_missing_targets = sum(is.na(data[[.target_col]]))

  numeric_target = as.numeric(data[[.target_col]])
  n_missing_numerics = sum(is.na(data[[.target_col]]))

  assert_that(n_missing_numerics == n_missing_targets,
              msg = "Target column of dataset must be of class 'numeric'.")

  # Ensure the temporal column is a Date
  data <- BuildTemporal(data)
  data <- data[order(data[[.temporal_col]]), ]

  return(data)
}


#' Formats the temporal column of a given tibble
#'
#' @param data The tibble created in load.data
#' @param origin Optional parameter specifying the earliest point at which data
#'               in the tibble originates from. This parameter is used when the
#'               temporal column has a numeric type (each number is assumed to
#'               be the number of time units since the origin, and is hence
#'               added to the origin to return a column of Dates)
#'
#' @return A new dataframe with a processed temporal column (of type 'Date')
#' @importFrom assertthat assert_that
#' @export

BuildTemporal <- function(data) {
  .temporal_col <- pkg.env$.temporal_col
  .target_col <- pkg.env$.target_col
  .origin <- pkg.env$.origin

  temporal_type <- class(data[[.temporal_col]])
  if (temporal_type == "numeric") {

    if (is.null(.origin)) {
      pkg.env$.origin <- as.Date("2022-01-01")
    }

    # The column storing the dates should be the {temporal_col}_as_date
    date_colname <- paste(.temporal_col, "as_date", sep='_')
    if (date_colname == .target_col) {
      # If the date column with a _as_date matches the target column's name,
      # add another _as_date to the column name (e.g. if date and date_as_date
      # are the two columns, make the date date_as_date_as_date)
      date_colname <- paste(date_colname, "as_date", sep='_')
    }

    data[[date_colname]] = .origin + data[[.temporal_col]]

    # Reassign the temporal column to our newly created date column
    pkg.env$.temporal_col <- date_colname
    .temporal_col <- pkg.env$.temporal_col

  } else if (temporal_type == "Date") {
    # Do nothing here for now, because we have the format we want
    num_colname <- paste0(.temporal_col, "_num")
    while (num_colname %in% colnames(data)) {
      num_colname = paste0(num_colname, "_num")
    }

    pkg.env$.num_origin <- as.numeric(min(data[[.temporal_col]]))

    # data[[num_colname]] = date.to.numeric(data)

    pkg.env$.temporal_num_col <- num_colname
  } else if (temporal_type != "numeric" & temporal_type != "Date") {

    casted_date <- as.Date(data[[.temporal_col]], origin = origin)

    pre_na <- sum(is.na(data[[.temporal_col]]))
    post_na <- sum(is.na(casted_date))

    assert_that(pre_na == post_na,
                msg = "Some 'temporal_col' values were not dates")

    data[[.temporal_col]] <- casted_date
  }

  return(data)
}


#' Generate a timetk friendly string from an entered period
#'
#' @param period The number of time units (should be an integer)
#' @param frequency The unit each row of data represents (e.g. "day", "year")
#'
#' @return A string combining the period with the time units (e.g. "60 days")
#' @export

PrettifyTimes <- function(period, frequency) {
  # Ensure period is positive
  assert_that(!is.na(as.numeric(period)),
              msg = "Period must be a positive integer")

  assert_that(frequency %in% pkg.env$.frequencies,
              msg = paste("Frequency should be one of:",
                    paste(pkg.env$.frequencies, collapse = ', ')))
  map <- c(
    "D" = "day",
    "M" = "month",
    "Y" = "year",
    "h" = "hour",
    "m" = "minute",
    "s" = "second"
  )
  #
  # last_two_letters = substr(frequency, nchar(frequency) - 1, nchar(frequency))
  # last_letter = substr(last_two_letters, 2, 2)
  #
  # if (last_letter == 's') {
  #   frequency = substr(frequency, 1, nchar(frequency) - 1)
  # } else if (last_two_letters == 'ly') {
  #   frequency = substr(frequency, 1, nchar(frequency) - 2)
  # }

  # if (frequency == 'annual') {
  #   # From annually to year
  #   frequency = 'year'
  # } else if (frequency == 'dai') {
  #   # From daily to day
  #   frequency = 'day'
  # }

  frequency <- map[frequency]
  # Ensure the frequency is valid
  # time_units <- c("year", "month", "day", "hour", "minute", "second")

  # Build the time string with the form {period} {frequency}, e.g. 60 days
  time_str = paste(period, frequency)

  if (as.numeric(period) > 1) {
    time_str = paste0(time_str, 's')
  }

  return(time_str)
}


#' Engineer features in a time series dataset
#'
#' @param data The dataset to engineer
#'
#' @return A list storing two tibbles - one containing the processed dataset
#'         provided, and another containing a frame of the horizon (the period
#'         to forecast)
#'
#' @importFrom timetk future_frame
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom utils tail
#'
#' @import dplyr
#'
#' @export

GenerateFeatures <- function(data) {
  .temporal_col <- pkg.env$.temporal_col
  .temporal_num_col <- pkg.env$.temporal_num_col
  .target_col <- pkg.env$.target_col
  .horizon <- pkg.env$.horizon
  .frequency <- pkg.env$.frequency

  # Check and format horizon argument
  period = PrettifyTimes(.horizon, .frequency)

  # Arrange data in chronological order
  data <- data[order(data[[.temporal_col]]), ]

  # Apply a log transformation to target column
  data[[.target_col]] <- log1p(data[[.target_col]])

  tmp <- dplyr::group_map(data, ~ c(mean = mean(.x$count, na.rm = TRUE),
                                    sd = sd(.x$count, na.rm = TRUE))) %>%
           dplyr::bind_rows()

  # Store standardization parameters (used later)
  pkg.env$.mean <- tmp$mean
  pkg.env$.sd <- tmp$sd
  rm('tmp')

  # Standardize the target column
  data %<>% # Double ended pipe with magrittr reassigns the output to data
    dplyr::mutate_at(vars({{ .target_col }}), ~(.-mean(.))/sd(.)) %>%
    future_frame({{ .temporal_col }}, .length_out = period, .bind_data = TRUE) %>%
    tk_augment_fourier(.date_var = {{ .temporal_col }}, .periods = 56, .K = 10)

  # Ensure that there are no NA values besides the future data
  na_values = which(is.na(data[[.target_col]]))
  assert_that(length(na_values) == tail(na_values, 1) - na_values[1] + 1,
              msg = "Please remove all NA values before running DeltaForecast")

  # The data we know is the dataset we will process
  prepared_tbl <- data[!is.na(data[[.target_col]]), ]
  prepared_tbl <- drop_na(prepared_tbl)

  # The data that is missing is the dataset we will predict
  future_tbl <- data[is.na(data[[.target_col]]), ]
  # future_tbl[[.temporal_num_col]] <- date.to.numeric(future_tbl)

  return(list(prepared_tbl, future_tbl))
}


date.to.numeric <- function(data) {
  .temporal_col <- pkg.env$.temporal_col
  .temporal_num_col <- pkg.env$.temporal_num_col
  .num_origin <- pkg.env$.num_origin

  numeric_date <- as.numeric(data[[.temporal_col]])
  numeric_col <- numeric_date - .num_origin

  return(numeric_col)
}


#' Splits dataset into training and testing components
#'
#' @param prepared_tbl A processed and engineered dataset
#' @param val_pct The percent of the data to use for model validation
#'
#' @return A list containing two tibbles - the dataset used for model training,
#'         and the dataset used for model evaluation
#' @export

CrossValidate <- function(prepared_tbl, val_pct = 0.2) {
  val_pct = as.numeric(val_pct)
  assert_that(!is.na(val_pct) && val_pct > 0 && val_pct < 1,
              msg = 'val_pct must be greater than 0 and less than 1')

  train_pct = 1 - val_pct
  split_idx = round(nrow(prepared_tbl) * train_pct)

  train_tbl <- prepared_tbl[1:split_idx, ]
  valid_tbl <- prepared_tbl[(split_idx+1):nrow(prepared_tbl), ]

  return(list(train_tbl, valid_tbl))
}


#' Compile pre-processing recipe for training dataset
#'
#' @param train_data The dataset to be used for model training
#'
#' @return A recipe definition containing pre-processing steps
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr matches
#'
#' @import recipes
#' @import timetk
#'
#' @export

BuildRecipe <- function(train_data) {
  .temporal_col = pkg.env$.temporal_col
  .target_col = pkg.env$.target_col
  .origin = pkg.env$.origin

  remove_pattern = paste0("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)")

  f <- as.formula(paste0(.target_col, " ~ ."))
  recipe_spec <- recipes::recipe(f, data = train_data) %>%
    timetk::step_timeseries_signature(all_of(.temporal_col)) %>%
    recipes::step_rm(matches(remove_pattern)) %>%
    recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE) %>%
    recipes::step_normalize(date_index.num, date_year)
    # recipes::step_rm(temporal_col) %>%
    # recipes::step_mutate(
      # date_num = as.numeric({{ .temporal_col }}))
    # recipes::step_mutate(
      # date_num = date_num - min(date_num)
    # )

  recipe_baked <- juice(prep(recipe_spec))

  no_levels <- length(unique(recipe_baked$date_year)) <= 1
  is_nan <- is.na(recipe_baked$date_year[1])
  if (no_levels & is_nan) {
    recipe_spec <- recipes::step_mutate(
      recipe_spec,
      date_year = tidyr::replace_na(date_year, 0)
    )
  }

  return(recipe_spec)
}


#' Fits a model workflow to the training data based on a pre-processing recipe
#'
#' @param wflw The parsnip model to train, encompassed in a workflow
#' @param recipe_spec The recipe storing pre-processing steps
#' @param train_data The dataset to use for model training
#'
#' @return A fitted parsnip model, encompassed in a workflow
#'
#' @import modeltime
#' @importFrom modeltime prophet_fit_impl
#' @importFrom magrittr %>%
#' @importFrom workflows add_recipe
#' @importFrom parsnip fit
#'
#' @export

fit.to.recipe <- function(wflw, recipe_spec, train_data) {
  fitted <- add_recipe(wflw, recipe_spec) %>%
    fit(train_data)

  return(fitted)
}


#' Compiles a collection of trained models into a single list
#'
#' @param include A list of engines to build models around. Should only be
#'                specified if exclude is empty. If both include and exclude are
#'                empty, all supported engines will be trained
#' @param exclude A list of engines to exclude from model training. If specified,
#'                all supported engines EXCEPT those specified here will be
#'                trained. If both 'include' and 'exclude' are empty, all
#'                supported engines will be trained.
#' @param seasonalities A vector storing all seasonalities exhibited by the time
#'                      series. Possible values are: "daily", "weekly" or
#'                      "yearly"
#' @param recipe_spec A recipe definition storing list of pre-processing steps
#'                    to be applied on training dataset
#' @param train_data Dataset on which to train model collection
#'
#' @return A list of trained parsnip models, each encompassed in a workflow
#'
#' @import modeltime
#' @importFrom modeltime prophet_fit_impl
#' @importFrom assertthat assert_that
#'
#' @export

CompileModels <- function(include = c(), exclude = c(), seasonalities,
                           recipe_spec, train_data) {
  .supported_engines <- pkg.env$.supported_engines

  excess_args <- "You may only specify one of either include or exclude"

  if (length(include) > 0) {
    assert_that(length(exclude) == 0, msg = excess_args)

    # Check if all engines are valid
    first_false_engine <- TRUE
    message <- ''

    for (engine in include) {
      if (!engine %in% .supported_engines) {
        print(engine)
        print("FAILURE")
        # If this is our first bad engine, start with an intro message
        if (first_false_engine) {
          message <- paste("Failed to build models because these engines are",
                           "not supported:")
          first_false_engine = FALSE
        }

        # Print out the faulty engines
        message <- paste(message, engine, sep="\n\t")
      }
    }

    # If we did have bad engines, throw an error
    if (!first_false_engine) {
      stop(message)
    }

  } else if (length(exclude == 0)) {
    include <- .supported_engines
  } else {
    include <- setdiff(.supported_engines, exclude)
  }

  models <- list()

  model_tbl <- modeltime::modeltime_table()
  if ('ranger' %in% include) {
    wflw <- build.tree('ranger')
    fitted <- fit.to.recipe(wflw, recipe_spec, train_data)
    fitted <- modeltime_table(fitted)

    model_tbl <- combine_modeltime_tables(model_tbl, fitted)
  }

  if ('randomForest' %in% include) {
    wflw <- build.tree('randomForest')
    fitted <- fit.to.recipe(wflw, recipe_spec, train_data)
    fitted <- modeltime_table(fitted)

    model_tbl <- combine_modeltime_tables(model_tbl, fitted)
  }

  if ('spark' %in% include) {
    wflw <- build.tree('spark')
    fitted <- fit.to.recipe(wflw, recipe_spec, train_data)
    fitted <- modeltime_table(fitted)

    model_tbl <- combine_modeltime_tables(model_tbl, fitted)
  }

  if ('xgboost' %in% include) {
    wflw <- build.boost_tree('xgboost')
    fitted <- fit.to.recipe(wflw, step_rm(recipe_spec, all_of(.temporal_col)),
                            train_data)
    fitted <- modeltime_table(fitted)

    model_tbl <- combine_modeltime_tables(model_tbl, fitted)
  }

  if ('prophet' %in% include) {
    wflw <- build.sequential('prophet', seasonalities)
    fitted <- fit.to.recipe(wflw, recipe_spec, train_data)
    fitted <- modeltime_table(fitted)

    model_tbl <- combine_modeltime_tables(model_tbl, fitted)
  }

  if ('prophet_xgboost' %in% include) {
    wflw <- build.sequential.boosted('prophet_xgboost', seasonalities)
    fitted <- fit.to.recipe(wflw, recipe_spec, train_data)
    fitted <- modeltime_table(fitted)

    model_tbl <- combine_modeltime_tables(model_tbl, fitted)
  }

  if ('arima' %in% include) {
    wflw <- build.arima('arima')
    fitted <- fit.to.recipe(wflw, recipe_spec, train_data)
    fitted <- modeltime_table(fitted)

    model_tbl <- combine_modeltime_tables(model_tbl, fitted)
  }

  if ('auto_arima' %in% include) {
    wflw <- build.arima('auto_arima')
    fitted <- fit.to.recipe(wflw, recipe_spec, train_data)
    fitted <- modeltime_table(fitted)

    model_tbl <- combine_modeltime_tables(model_tbl, fitted)
  }

  if ('arima_xgboost' %in% include) {
    wflw <- build.arima.boosted('arima_xgboost')
    fitted <- fit.to.recipe(wflw, recipe_spec, train_data)
    fitted <- modeltime_table(fitted)

    model_tbl <- combine_modeltime_tables(model_tbl, fitted)
  }

  if ('auto_arima_xgboost' %in% include) {
    wflw <- build.arima.boosted('auto_arima_xgboost')
    fitted <- fit.to.recipe(wflw, recipe_spec, train_data)
    fitted <- modeltime_table(fitted)

    model_tbl <- combine_modeltime_tables(model_tbl, fitted)
  }

  if ('lm' %in% include) {
    wflw <- build.linear('lm')
    fitted <- fit.to.recipe(wflw, recipe_spec, train_data)
    fitted <- modeltime_table(fitted)

    model_tbl <- combine_modeltime_tables(model_tbl, fitted)
  }

  return(model_tbl)
}


#' Generates ensembles and final forecasts from trained models
#'
#' @param models A list of trained parsnip models, each encompassed in a
#'               workflow
#' @param prepared_tbl The full processed dataset (both training and validation
#'                     datasets)
#' @param future_tbl The empty frame storing the horizon data to forecast
#' @param valid_data The dataset on which to evaluate models
#' @param ensembles A vector storing the ensembles to be generated. Possible
#'                  options are "mean", "median", or rank"
#' @param metric The metric by which to rank models (e.g. rmse, mape, mse, etc.)
#'
#' @return A final table storing the models' forecasts on the horizon data
#'
#' @importFrom magrittr %<>%
#' @importFrom modeltime.ensemble ensemble_average
#' @importFrom modeltime.ensemble ensemble_weighted
#' @importFrom rsample vfold_cv
#' @importFrom timetk standardize_vec
#' @importFrom assertthat assert_that
#'
#' @import modeltime
#' @importFrom modeltime prophet_fit_impl
#' @import dplyr
#' @import tidyr
#'
#' @export

BuildForecasts <- function(mdl_tbl, prepared_tbl, future_tbl, valid_data,
                           ensembles = c("mean", "median", "rank"), metric = rmse) {
  # mdl_tbl <- modeltime::as_modeltime_table(models)

  calib_tbl <- modeltime::modeltime_calibrate(mdl_tbl,
                                              new_data = valid_data,
                                              quiet = FALSE)
  acc_tbl <- modeltime_accuracy(calib_tbl)

  if (length(ensembles) > 0) {
    assert_that(nrow(mdl_tbl) > 1,
                msg = "You cannot specify ensembles with only one trained model")
    # Build models to ensemble predictions
    ensemble_tbl <- modeltime::modeltime_table()

    if ("mean" %in% ensembles) {
      ensemble_fit_mean <- ensemble_average(calib_tbl, type='mean')
      ensemble_fit_mean <- modeltime::modeltime_table(ensemble_fit_mean)

      ensemble_tbl <- modeltime::combine_modeltime_tables(
        ensemble_tbl, ensemble_fit_mean)
    }

    if ("median" %in% ensembles) {
      ensemble_fit_median <- ensemble_average(calib_tbl, type='median')
      ensemble_fit_median <- modeltime::modeltime_table(ensemble_fit_median)

      ensemble_tbl <- modeltime::combine_modeltime_tables(
        ensemble_tbl, ensemble_fit_median)
    }

    if ("rank" %in% ensembles) {
      metrics <- setdiff(colnames(acc_tbl), c(".model_id", ".model_desc", ".type"))
      assert_that(metric %in% metrics, msg = paste("Metric should be one of:",
                                             paste(metrics, collapse = ', ')))

      # Rank the models from worst (low) to best (high)
      rank_tbl <- acc_tbl %>%
        dplyr::mutate(rank = dplyr::min_rank(-.data[[metric]])) %>%
        dplyr::select(.model_id, rank)

      # Get the relative ranks for models (1 for worst model)
      loadings <- rank_tbl$rank

      ensemble_fit_rank <- ensemble_weighted(calib_tbl, loadings = loadings)
      ensemble_fit_rank <- modeltime::modeltime_table(ensemble_fit_rank)

      ensemble_tbl <- modeltime::combine_modeltime_tables(
        ensemble_tbl, ensemble_fit_rank)
    }

    ensemble_calib_tbl <- modeltime::modeltime_calibrate(ensemble_tbl,
                                                         new_data = valid_data,
                                                         quiet = TRUE)

    calib_tbl <- modeltime::combine_modeltime_tables(
        calib_tbl, ensemble_calib_tbl
    )
  }

  calib_tbl <- modeltime::modeltime_calibrate(
      calib_tbl, valid_data, quiet = TRUE
  )

  refit_tbl <- modeltime_refit(
    calib_tbl, data = prepared_tbl
    # resamples = tidyr::drop_na(prepared_tbl) %>% vfold_cv(v = 5)
  )

  forecast_tbl <- modeltime::modeltime_forecast(refit_tbl,
                                                new_data = future_tbl,
                                                keep_data = FALSE)

  forecast_tbl %<>%
    mutate(across(.value:.conf_hi,
                  .fns = ~standardize_inv_vec(x = ., pkg.env$.mean, pkg.env$.sd))) %>%
    mutate(across(.value:.conf_hi, .fns = ~expm1(x = .)))

  pkg.env$.accuracies <- acc_tbl[[metric]]

  return(forecast_tbl)
}


#' Compute the impact an event had on some target value
#'
#' @param observed The observed data during the horizon
#' @param forecast The predicted data for the horizon
#'
#' @return A CausalImpact object outlining the estimated impact
#'
#' @export

ImpactAnalysis <- function(observed, forecast) {
  assert_that(nrow(observed) == nrow(forecast))
  assert_that(ncol(observed == 2))
  assert_that(ncol(forecast == 2))

  # TODO: Experiment between t-test and CausalImpact

}


#' Helper function to ensure user-provided dataset is formatted correctly
#'
#' @param data Dataset built by user
#'
#' @return TRUE if the dataset is correctly formatted - throw an error otherwise
#'
#' @importFrom assertthat assert_that
#'
#' @export

check.data <- function(data) {
  generic.error = paste("Make sure you have called 'LoadData' on a pre-built",
                        "dataset before passing it into other methods.")
  assert_that(ncol(data) == 2,
              msg = paste("Data should only have 2 columns.", generic.error))

  assert_that(class(data[[pkg.env$.temporal_col]]) == "Date",
              msg = paste("Temporal column should be a 'Date'.", generic.error))

  assert_that(class(data[[pkg.env$.target_col]]) == "Numeric",
              msg = paste("Target column should be 'numeric'.", generic.error))

  return(TRUE)
}


#' Estimate the impact on an event using time-series forecasting
#'
#' @param data Optional argument (must be provided if 'path' is null) specifying
#'             a pre-loaded dataframe to be processed. If specified, 'path' will
#'             be ignored. The provided tibble will be checked and the relevant
#'             columns will be filtered.
#' @param path Optional argument(must be provided if 'data' is null) specifying
#'             a path to a CSV file. Will be ignored unless 'data' is null. The
#'             provided CSV file will be read, and the data will be checked and
#'             filtered.
#' @param event The event of interest. Should be of type 'Date' or 'numeric'. If 'numeric', it
#'              will be added to the 'origin' to return a 'Date'.
#' @param temporal_col A string representing the ccolumn in the time-series dataset storing the
#'                     time.
#' @param target_col A string representing the column in the time-series dataset storing the
#'                   response variable (the value that you expect to change after some event)
#' @param origin Optional argument specifying the time of the earliest
#'               observation recorded in the dataset. This parameter will only
#'               be used if the temporal column of the dataset is 'numeric'
#'               instead of 'Date'.
#' @param frequency The unit of time that consecutive rows represent. Possible  values are:
#'                  D (day), W (week), M (month), h (hour), m (minute), s (second).
#' @param horizon How far into the future you wish to observe for each event. For instance, if
#'                you have a time-series with daily frequency, a horizon of 10 means that
#'                DeltaForecast will forecast 10 days after the event and compare the difference
#'                between the observed data and the forecast.
#' @param val_pct A number between 0 and 1, representing the proportion of the data to be used to
#'                evaluate the performance of each models. Can be useful for the user to see how
#'                reliable the forecasts are, and are needed to generate a rank ensemble based on
#'                the validation scores.
#' @param include_engines A vector containing names of parsnip engines to build models around and
#'                        train. If specified, 'exclude_engines' will be ignored. To specify every
#'                        available engine, pass in an empty vector for both parameters.
#' @param exclude_engines A vector containing names of parsnip engines to NOT build models around.
#'                        will only be considered if include_engines is empty. Every available
#'                        engine besides those specified here will be built and trained. If found
#'                        empty alongside 'include_engines', all available engines will be built.
#' @param ensembles A vector containing one, none or multiple of the following types of model
#'                  ensembles: 'mean' (averages all built models together), 'median' (takes the
#'                  median of all built models, and 'rank' (takes the weighted average of all models,
#'                  assigning the highest weight to the best performing model on the validation set)
#' @param seasonality A vector storing any seasonalities the dataset exhibits. Can be one, none or
#'                    multiple of: 'daily', 'weekly' or 'yearly''
#'
#' @return (for now) A tibble storing the forecast generated by each model and including the
#'                   ensembles.
#'
#' @export

DeltaForecast <- function(data = NULL, path = NULL, event, temporal_col = "date",
                          target_col = "target", origin = NULL, frequency = "D",
                          horizon = 10, val_pct = 0.2, include_engines = c(),
                          exclude_engines = c(), metric = "rmse", seasonality = c(),
                          ensembles = c("mean", "median", "rank")) {
  print("Setting up data...")
  # Set global variable values
  pkg.env$.temporal_col <- temporal_col
  pkg.env$.target_col <- target_col
  pkg.env$.origin <- origin
  pkg.env$.frequency <- frequency
  pkg.env$.frequencies <- c("s", "m", "h", "D", "W", "M", "Y")
  pkg.env$.horizon <- horizon
  pkg.env$.seasonality <- seasonality
  pkg.env$.seasonalities <- c("M", "W", "Y")
  pkg.env$supported_engines <- c(
    'ranger', 'randomForest', 'spark', 'xgboost', 'rpart',
    'prophet', 'prophet_xgboost', 'arima', 'auto_arima',
    'arima_xgboost', 'auto_arima_xgboost'
  )

  data <- LoadData(data = data, path = path)

  # If origin is not defined, set it to the earliest date in the data
  pkg.env$.origin = if (is.null(origin)) min(data[[temporal_col]])

  if (class(event) == "Date") {
    event_as_date <- event
    event <- as.numeric(event - min(data[[temporal_col]]))
  } else if (class(event) == "numeric") {
    event_as_date <- min(data[[temporal_col]]) + event
  }

  split_idx <- which(data[[pkg.env$.temporal_col]] == event_as_date)
  end_idx <- split_idx + horizon - 1

  pre.data <- data[1:(split_idx-1), ]
  post.data <- data[split_idx:end_idx, ]

  tbls <- GenerateFeatures(pre.data)
  prepared_tbl <- tbls[[1]]
  future_tbl <- tbls[[2]]

  tbls <- CrossValidate(prepared_tbl, val_pct = val_pct)
  train_tbl <- tbls[[1]]
  valid_tbl <- tbls[[2]]

  rm(tbls)

  recipe_spec <- BuildRecipe(train_tbl)

  model_tbl <- CompileModels(include = include_engines, exclude = exclude_engines,
                             seasonalities = seasonality, recipe_spec, train_tbl)

  if (length(include_engines) == 0) {
    include_engines <- setdiff(pkg.env$.supported_engines, exclude_engines)
  }

  n_models = length(ensembles) + length(include_engines)

  forecast_tbl <- BuildForecasts(model_tbl, prepared_tbl, future_tbl, valid_tbl,
                                  ensembles = ensembles, metric = metric)

  # Make sure we have the correct number of forecasts
  assert_that((n_models * horizon) == nrow(forecast_tbl))

  observed <- rbind(pre.data, post.data)

  impact_preds <- data.frame()

  pre.period <- c(1, event - 1)
  post.period <- c(event, event + horizon)

  y <- observed$count

  # Training and inference
  print("Training models and finding impact...")

  # Initializes a progress bar
  pb <- txtProgressBar(min = 0,
                       max = n_models,
                       style = 3,
                       width = 50,
                       char = "=")

  for (i in 1:n_models) {
    setTxtProgressBar(pb, i)

    start_idx = horizon * (i - 1) + 1
    end_idx = horizon * i

    cur_forecast <- forecast_tbl[start_idx:end_idx, ]
    model_name <- cur_forecast$.model_desc[1]

    y.pred <- c(pre.data$count, cur_forecast$.value)

    ci_data <- zoo(cbind(y, y.pred))

    cur_impact <- CausalImpact(ci_data, pre.period, post.period)
    x <- cur_impact$summary

    cur_row <- c(model_name, x$Actual[1], x$Actual[2], x$Pred[1], x$Pred[2],
                 x$AbsEffect[1], x$AbsEffect.lower[1], x$AbsEffect.upper[1],
                 x$AbsEffect[2], x$AbsEffect.lower[2], x$AbsEffect.upper[2],
                 x$p[1], pkg.env$.accuracies[i])

    impact_preds <- rbind(impact_preds, cur_row)
  }

  colnames(impact_preds) <- c("model", "observed_avg", "observed_total",
                              "pred_avg", "pred_total", "impact_avg",
                              "impact_avg.lower", "impact_avg.upper", "impact_total",
                              "impact_total.lower", "impact_total.upper", "p", metric)

  # counterfactual <- forecast_tbl %>% dplyr::select(.index, .value)
  # colnames(counterfactual) <- c(temporal_col, target_col)

  # print(t.test(counterfactual$count, post.data$count, paired = TRUE, alternative = "greater"))

  # counterfactual <- rbind(pre.data, counterfactual[1:horizon, ])

  # ci_data <- zoo(cbind(observed$count, counterfactual$count))
#
  # pre.period <- c(1, event-1)
  # post.period <- c(event, event + horizon)

  # impact <- CausalImpact(ci_data, pre.period, post.period)

  return(impact_preds)
}


#### For testing ####
# library(tidyverse)
# library(assertthat)
# library(workflows)
# library(recipes)
# library(parsnip)
# library(timetk)
# library(modeltime)
# library(modeltime.ensemble)
# library(zoo)
# library(CausalImpact)
#
# setwd("../Data-Science/DeltaForecast")
#
DeltaForecast(path = "../Data-Science/DeltaForecast/pos_time_series.csv", event = 77, target_col = "count",
horizon = 20, include_engines = c("lm"), ensembles = c())
#
# data = NULL
# path = "pos_time_series.csv"
# temporal_col = "date"
# event = 395
# target_col = "count"
# origin = as.Date("2020-11-01")
# frequency = "D"
# horizon = 20
# seasonality = c()
# include_engines <- c("lm", "prophet_xgboost", "prophet", "xgboost")
# exclude_engines <- c()
# ensembles <- c("mean", "rank", "median")
# val_pct = 0.2
#
# pkg.env$.temporal_col <- temporal_col
# pkg.env$.target_col <- target_col
# pkg.env$.origin <- origin
# pkg.env$.frequency <- frequency
# pkg.env$.frequencies <- c("s", "m", "h", "D", "W", "M", "Y")
# pkg.env$.horizon <- horizon
# pkg.env$.seasonality <- seasonality
# pkg.env$.seasonalities <- c("M", "W", "Y")
# pkg.env$.supported_engines <- c(
#   'ranger', 'randomForest', 'spark', 'xgboost', 'rpart',
#   'prophet', 'prophet_xgboost', 'arima', 'auto_arima',
#   'arima_xgboost', 'auto_arima_xgboost', 'lm', 'glm',
#   'glmnet', 'stan', 'brulee'
# )
#
# data <- LoadData(data = data, path = path)
#
# if (class(event) == "Date") {
#   event_as_date <- event
#   event <- as.numeric(event - min(data[[temporal_col]]))
# } else if (class(event) == "numeric") {
#   event_as_date <- min(data[[temporal_col]]) + event
# }
#
# split_idx <- which(data[[pkg.env$.temporal_col]] == event_as_date)
# end_idx <- split_idx + horizon - 1
#
# pre.data <- data[1:(split_idx-1), ]
# post.data <- data[split_idx:end_idx, ]
#
# tbls <- GenerateFeatures(pre.data)
# prepared_tbl <- tbls[[1]]
# future_tbl <- tbls[[2]]
#
# tbls <- CrossValidate(prepared_tbl, val_pct = val_pct)
# train_tbl <- tbls[[1]]
# valid_tbl <- tbls[[2]]
#
# rm(tbls)
#
# recipe_spec <- BuildRecipe(train_tbl)
#
# model_tbl <- CompileModels(include = include_engines, exclude = exclude_engines,
#                            seasonalities = seasonality, recipe_spec, train_tbl)
#
# print(model_tbl)
# forecast_tbl <- BuildForecasts(model_tbl, prepared_tbl, future_tbl, valid_tbl,
#                                  ensembles = ensembles, metric = asdf)
#
# forecast_df <- forecast_tbl[101:120, ] %>% dplyr::select(.index, .value)
# colnames(forecast_df) <- c("date", "count")
#
# observed <- rbind(pre.data, post.data)
# predicted <- rbind(pre.data, forecast_df)
#
# observed %>% ggplot(aes(date, count)) +
#   geom_line(mapping = aes(date, count), data = predicted, size=1) +
#   geom_line(colour = "#23B6B6", size=1) +
#   geom_line(mapping = aes(date, count), data = post.data, colour = "#23B6B6")
#
#
# couterfactual <- forecast_tbl %>% dplyr::select(.index, .value)
# colnames(counterfactual) <- c(temporal_col, target_col)
# counterfactual <- rbind(pre.data, counterfactual[1:horizon, ])
#
#
# ci_data <- zoo(cbind(observed$count, counterfactual$count))
#
# pre.period <- c(1, event-1)
# post.period <- c(event, event + horizon - 1)
#
# impact <- CausalImpact(ci_data, pre.period, post.period)
#
