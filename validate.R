setwd(Sys.getenv('ROSSMANN_HOME'))
source('load.R')
source('model.R')

data <- train_test_data(n_stores = 100)
model <- model_gbm

# Build features
train_data <- 
  data$train %>%
  group_by(store) %>%
  arrange(date) %>%
  mutate(
    sales_hist_avg = mean(sales, na.rm = TRUE),
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
  ungroup

lags <- c(1, 2, 3, 4, 5, 6, 12, 18)
features <- c(
  'storetype', 'competitiondistance', 'promo', 'dayofweek',
  'promo2', paste0('sales_', lags)
)

stores_norm_factors <- 
  train_data %>%
  select(store, sales_hist_avg) %>%
  distinct

train_data %<>%
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
add_lagged_features_store <- function(store_data, lags) {
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
  do(add_lagged_features_store(., lags)) %>%
  ungroup %>%
  mutate_each(
    funs(as.factor),
    storetype,
    dayofweek
  ) %>%
  inner_join(stores_norm_factors) %>% 
  select_(.dots = c(
    features, 'date', 'sales', 'store', 'sales_hist_avg'
  ))

predict_store <- function(fit, store_data, lags) {
  predictions <- numeric()
  for (i in 1:nrow(store_data)) {
    prediction <- model$predict(fit, store_data[i, ])
    predictions[i] <- prediction
    for (col in paste0('sales_', lags)) {
      j <- Position(is.na, store_data[[col]])
      if (!is.na(j)) {
        store_data[j, col] <- prediction
      }
    }
  }
  store_data %>%
    mutate(predicted = predictions * sales_hist_avg)
}

# Predict the test set
par_predict <- function(model, newdata, lags) {
  ncores <- parallel::detectCores()
  doParallel::registerDoParallel(cores = ncores)
  batch_assignments <- 
    data_frame(
      store = unique(newdata$store)
    ) %>%
    mutate(
      batch_n = rep(1:ncores, each = (nrow(.)/ncores), length.out = nrow(.))
    )
  
  batches <- 
    newdata %>%
    inner_join(batch_assignments) %>%
    split(.$batch_n)
  
  foreach(batch = batches, .combine = bind_rows) %dopar% {
    batch %>%
      group_by(store) %>%
      arrange(date) %>%
      do(predict_store(model, ., lags)) %>%
      ungroup
  }
}

preds_df <- par_predict(fit, test_data, lags) %T>% preds_summary