setwd(Sys.getenv('ROSSMANN_HOME'))
source('load.R')
source('model.R')

# Load data
train_clean <- read_csv('data/clean/train_clean.csv')
store_clean <- read_csv('data/clean/store_clean.csv')
test_clean <- read_csv('data/clean/test_clean.csv')

# Training
model <- model_rf
lags <- c(1, 2, 3, 4, 5, 6, 12, 18)
features <- c(
  'storetype', 'competitiondistance', 'promo', 'dayofweek',
  'promo2', paste0('sales_', lags)
)

train_data <- 
  train_clean %>%
  inner_join(store_clean) %>%
  inner_join(
    test_clean %>%
      select(store) %>%
      distinct
  ) %>%
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
  ungroup %>%
  select_('store', 'date', 'sales_norm', .dots = features) %>%
  filter(complete.cases(.)) %>%
  mutate_each(
    funs(as.factor),
    storetype,
    dayofweek
  )

fit <- model$fit(
  x = select_(train_data, .dots = features),
  y = train_data$sales_norm
)
saveRDS(fit, 'output/rf_fit.rds')

# Predicting
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
  test_clean %>%
  filter(dayofweek != 7) %>%
  inner_join(store_clean) %>%
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
    features, 'date', 'store', 'sales_hist_avg', 'id'
  ))

preds_df <- par_predict(model, fit, test_data)
saveRDS(preds_df, 'rf_predictions.rds')

preds_df %>%
  select(id, predicted) %>%
  right_join(select(test_clean, id)) %>%
  mutate(predicted = ifelse(is.na(predicted), 0, predicted)) %>%
  rename(Id = id, Sales = predicted) %>%
  write_csv('output/rf_submission.csv')



