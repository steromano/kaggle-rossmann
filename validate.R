setwd(Sys.getenv('ROSSMANN_HOME'))
source('load.R')
source('model.R')

# Global vars
data <- train_test_data(n_stores = 50)
lags <- c(1, 2, 3, 4, 5, 6, 12, 18)
features <- c(
  # store features (?)
  'storetype', 'promo2', 'competitiondistance', 'sales_hist_avg',
  # time series features
  'promo', 'dayofweek', paste0('sales_', lags)

)

# Set model
model <- model_rf

# Build features
train_data <- 
  data$train %>%
  group_by(store) %>%
  arrange(date) %>%
  mutate(
    logsales = log(sales),
    sales_1 = lag(logsales, 1),
    sales_2 = lag(logsales, 2),
    sales_3 = lag(logsales, 3),
    sales_4 = lag(logsales, 4),
    sales_5 = lag(logsales, 5),
    sales_6 = lag(logsales, 6),
    sales_12 = lag(logsales, 12),
    sales_18 = lag(logsales, 18)
  ) %>%
  ungroup %>%
  select_('store', 'date', 'logsales', .dots = features) %>%
  filter(complete.cases(.)) %>%
  mutate_each(
    funs(as.factor),
    storetype,
    dayofweek
  )

# Train model
fit <- model$fit(
  x = select_(train_data, .dots = features),
  y = train_data$logsales
)

# Prepare test set
add_lagged_features_store <- function(store_data, train_data) {
  n <- nrow(store_data)
  st <- store_data$store[1]
  logsales_train <- 
    train_data %>% 
    filter(store == st) %>%
    arrange(date) %$% 
    logsales
  
  for (lag in lags) {
    store_data[[paste0('sales_', lag)]] <- c(
      tail(logsales_train, lag),
      rep(NA, n - lag)
    )
  }
  store_data
}

test_data <- 
  data$test %>%
  group_by(store) %>%
  arrange(date) %>%
  do(add_lagged_features_store(., train_data)) %>%
  ungroup %>%
  mutate_each(
    funs(as.factor),
    storetype,
    dayofweek
  ) %>%
  select_(.dots = c(
    features, 'date', 'sales', 'store', 'sales_hist_avg'
  ))


# Predict the test set
preds_df <- par_predict(model, fit, test_data) %T>% preds_summary
