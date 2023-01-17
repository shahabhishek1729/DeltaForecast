#' Builds a tree-based parsnip model
#'
#' @description
#' Builds and encapsulates tree-based models (Random Forests,
#' Decision Trees, etc.) inside a workflow. This model is untrained
#' and simply stores the model architecture and specifications.
#' Currently supported engines are: 'ranger', 'randomForest', 'rpart'
#' and 'spark'
#'
#' @param engine A string storing the parsnip engine the model
#'               is intended to run on
#'
#' @return A workflow object storing the un-trained model
#'
#' @import modeltime
#' @import parsnip
#' @import workflows
#' @export
#'
#' @examples
#' build.tree('ranger')
build.tree <- function(engine) {

  wflw_fit_tree <- workflow() %>%
    add_model(
      spec = rand_forest(
        mode = "regression"
      ) %>%
        set_engine(engine)
    )

  return(wflw_fit_tree)
}


#' Builds a linear parsnip model
#'
#' @description
#' Builds and encapsulates linear models (Linear Regression) inside a workflow.
#' This model is untrained and simply stores the model architecture and
#' specifications. Currently supported engines are: 'brulee', 'glm', 'glmnet',
#' 'keras', 'lm', 'spark', 'stan'
#'
#' @param engine A string storing the parsnip engine the model
#'               is intended to run on
#'
#' @return A workflow object storing the un-trained model
#'
#' @import modeltime
#' @import parsnip
#' @import workflows
#' @export
#'
#' @examples
#' build.linear('lm')
build.linear <- function(engine) {

  wflw_fit_linear <- workflow() %>%
    add_model(
      spec = linear_reg(mode = "regression") %>%
        set_engine(engine)
    )

  return(wflw_fit_linear)
}


#' Builds a boosted-tree parsnip model
#'
#' @description
#' Builds and encapsulates tree-based models (XGBoost) inside
#' a workflow. This model is untrained and simply stores the model
#' architecture and specifications. Currently supported engines
#' are: 'xgboost' and 'spark'
#'
#' @param engine A string storing the parsnip engine the model
#'               is intended to run on
#'
#' @return A workflow object storing the un-trained model
#'
#' @import modeltime
#' @import parsnip
#' @import workflows
#' @export
#'
#' @examples
#' build.boost_tree('xgboost')
build.boost_tree <- function(engine) {

  wflw_fit_boost <- workflow() %>%
    add_model(
      spec = boost_tree(
        mode = "regression"
      ) %>%
        set_engine(engine)
    )

  return(wflw_fit_boost)
}


#' Builds a sequential parsnip model
#'
#' @description
#' Builds and encapsulates sequential models (Prophet) inside a workflow. This model is untrained and
#' simply stores the model architecture and specifications.
#' Currently supported engines are: 'prophet'
#'
#' @param engine A string storing the parsnip engine the model
#'               is intended to run on
#' @param seasonalities A string or a vector of strings specifying any
#'                      daily, weekly or yearly seasonality patterns
#'                      visible in the time series dataset. If left to
#'                      the default value of the empty vector, there
#'                      will be assumed to be no seasonality
#'
#' @return A workflow object storing the un-trained model
#'
#' @importFrom modeltime prophet_fit_impl
#' @import modeltime
#' @import parsnip
#' @import workflows
#' @export
#'
#' @examples
#' build.sequential('prophet')
build.sequential <- function(engine, seasonalities = c()) {

  daily_seasons <- c("day", "days", "daily")
  weekly_seasons <- c("week", "weeks", "weekly")
  yearly_seasons <- c("year", "yearly", "annual", "annually")

  seasonality_daily <- sum(seasonalities %in% daily_seasons) > 0
  seasonality_weekly <- sum(seasonalities %in% weekly_seasons) > 0
  seasonality_yearly <- sum(seasonalities %in% yearly_seasons) > 0

  wflw_fit_sequential <- workflow() %>%
    add_model(
      spec = prophet_reg(
        seasonality_daily = seasonality_daily,
        seasonality_weekly = seasonality_weekly,
        seasonality_yearly = seasonality_yearly
      ) %>%
        set_engine(engine)
    )

  return(wflw_fit_sequential)
}


#' Builds a boosted sequential parsnip model
#'
#' @description
#' Builds and encapsulates boosted-sequential models (Prophet with
#' Boosted Errors) inside a workflow. This model is untrained and
#' simply stores the model architecture and specifications.
#' Currently supported engines are: 'prophet_xgboost'
#'
#' @param engine A string storing the parsnip engine the model
#'               is intended to run on
#' @param seasonalities A string or a vector of strings specifying any
#'                      daily, weekly or yearly seasonality patterns
#'                      visible in the time series dataset. If left to
#'                      the default value of the empty vector, there
#'                      will be assumed to be no seasonality
#'
#' @return A workflow object storing the un-trained model
#'
#' @import modeltime
#' @import parsnip
#' @import workflows
#' @export
#'
#' @examples
#' build.sequential.boosted('prophet_boost')
build.sequential.boosted <- function(engine, seasonalities = c()) {

  daily_seasons <- c("day", "days", "daily")
  weekly_seasons <- c("week", "weeks", "weekly")
  yearly_seasons <- c("year", "yearly", "annual", "annually")

  seasonality_daily <- sum(seasonalities %in% daily_seasons) > 0
  seasonality_weekly <- sum(seasonalities %in% weekly_seasons) > 0
  seasonality_yearly <- sum(seasonalities %in% yearly_seasons) > 0

  wflw_fit_sequential <- workflow() %>%
    add_model(
      spec = prophet_boost(
        seasonality_daily = seasonality_daily,
        seasonality_weekly = seasonality_weekly,
        seasonality_yearly = seasonality_yearly
      ) %>%
        set_engine(engine)
    )

  return(wflw_fit_sequential)
}

#' Builds an ARIMA-based parsnip model
#'
#' @description
#' Builds and encapsulates sequential models (ARIMA, ARIMA with
#' Boosted Errors, Auto-ARIMA, Auto-ARIMA with Boosted Errors)
#' inside a workflow. This model is untrained and simply stores
#' the model architecture and specifications. Currently supported
#' engines are: 'arima', 'auto_arima', 'arima_xgboost',
#' 'auto_arima_xgboost'
#'
#' @param engine A string storing the parsnip engine the model
#'               is intended to run on
#'
#' @return A workflow object storing the un-trained model
#'
#' @import modeltime
#' @import parsnip
#' @import workflows
#' @export
#'
#' @examples
#' build.arima('arima')
build.arima <- function(engine) {

  wflw_fit_arma <- workflow() %>%
    add_model(
      spec = arima_reg() %>%
        set_engine(engine)
    )

  return(wflw_fit_arma)
}


#' Builds an ARIMA-boosted parsnip model
#'
#' @description
#' Builds and encapsulates ARIMA-boosted models (ARIMA-XGBoost)
#' inside a workflow. This model is untrained and simply stores
#' the model architecture and specifications. Currently supported
#' engines are: 'arima_xgboost', 'auto_arima_xgboost'
#'
#' @param engine A string storing the parsnip engine the model
#'               is intended to run on
#'
#' @return A workflow object storing the un-trained model
#'
#' @import modeltime
#' @import parsnip
#' @import workflows
#' @export
#'
#' @examples
#' build.arima('arima_xgboost')
build.arima.boosted <- function(engine) {

  wflw_fit_arma <- workflow() %>%
    add_model(
      spec = arima_boost() %>%
        set_engine(engine)
    )

  return(wflw_fit_arma)
}

