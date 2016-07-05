setwd(Sys.getenv('ROSSMANN_HOME'))
source('load.R')
source('model.R')

# Global vars
data <- train_test_data(n_stores = 50)
lags <- c(1, 2, 3, 4, 5, 6, 12, 18)
features <- c(
  # store features (?)
  'storetype', 'promo2', 'competitiondistance',
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
    sales_norm = sales / sales_hist_avg,
    sales_1 = lag(sales_norm, 1),
    sales_2 = lag(sales_norm, 2),
    sales_3 = lag(sales_norm, 3),
    sales_4 = lag(sales_norm, 4),
    sales_5 = lag(sales_norm, 5),
    sales_6 = lag(sales_norm, 6),
    sales_12 = lag(sales_norm, 12),
    sales_18 = lag(sales_norm, 18)
  ) %>%
  ungroup %>%
  select_('store', 'date', 'sales_norm', .dots = features) %>%
  filter(complete.cases(.)) %>%
  mutate_each(
    funs(as.factor),
    storetype,
    dayofweek
  )

# Train model
fit <- model$fit(
  x = select_(train_data, .dots = features),
  y = train_data$sales_norm
)

# Prepare test set
add_lagged_features_store <- function(store_data, train_data) {
  n <- nrow(store_data)
  st <- store_data$store[1]
  sales_norm_train <- 
    train_data %>% 
    filter(store == st) %>%
    arrange(date) %$% 
    sales_norm
  
  for (lag in lags) {
    store_data[[paste0('sales_', lag)]] <- c(
      tail(sales_norm_train, lag),
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
