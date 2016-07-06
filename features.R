primary_key <- c('store', 'date')
store_features <- c('storetype', 'competitiondistance', 'sales_hist_med', 'trend')
day_features <- c('promo', 'dayofweek', 'day_n', 'month', 'day', 'schoolholiday')
lags <- c(1, 2, 3, 4, 5, 6, 12, 18)

features <- c(
  store_features, day_features, paste0('lagged_', lags)
)

factor_features <- c('dayofweek', 'storetype')

build_features_day <- function(data, start = 0) {
  data %>%
    group_by(store) %>%
    arrange(date) %>%
    mutate(day_n = 1:length(date) + start) %>%
    ungroup %>%
    mutate(month = month(date), day = day(date))
}

build_features_store <- function(train_data, stores) {
  store_trends <- 
    train_data %>%
    group_by(store) %>%
    arrange(date) %>%
    mutate(day_n = 1:length(date)) %>%
    do(trend = {
      data <- select(., day_n, store, sales) %>% filter(complete.cases(.))
      coef(glmnet(
        x = as.matrix(select(data, day_n, store)),
        y = data$sales,
        alpha = 0,
        lambda = 200
      ))@x[2]
    }) %>%
    ungroup %>%
    mutate(trend = unlist(trend))
  
  store_hist_meds <-
    train_data %>%
    group_by(store) %>%
    summarise(sales_hist_med = median(sales, na.rm = TRUE))
  
  stores %>%
    inner_join(store_trends) %>%
    inner_join(store_hist_meds) %>%
    select_('store', .dots = store_features)
}

build_lagged_features_train <- function(train_data) {
  train_data %>%
    group_by(store) %>%
    arrange(date) %>%
    mutate(
      logsales = log(sales),
      lagged_1 = lag(logsales, 1),
      lagged_2 = lag(logsales, 2),
      lagged_3 = lag(logsales, 3),
      lagged_4 = lag(logsales, 4),
      lagged_5 = lag(logsales, 5),
      lagged_6 = lag(logsales, 6),
      lagged_12 = lag(logsales, 12),
      lagged_18 = lag(logsales, 18)
    ) %>%
    ungroup
}

build_lagged_features_test <- function(train_data, test_data) {
  add_lagged_features_store <- function(store_data) {
    n <- nrow(store_data)
    st <- store_data$store[1]
    logsales_train <- 
      train_data %>% 
      filter(store == st) %>%
      arrange(date) %$% 
      log(sales)
    
    for (lag in lags) {
      store_data[[paste0('lagged_', lag)]] <- c(
        tail(logsales_train, lag),
        rep(NA, n - lag)
      )
    }
    store_data
  }
  
  test_data %>%
    group_by(store) %>%
    arrange(date) %>%
    do(add_lagged_features_store(.)) %>%
    ungroup
}

# Wrappers
build_features_train <- function(train_data, stores, extra_cols = character()) {
  train_data %>%
    filter(sales > 0) %>%
    build_features_day %>%
    build_lagged_features_train %>%
    inner_join(build_features_store(., stores)) %>%
    select_('logsales', .dots = c(features, extra_cols)) %>%
    mutate_each_(funs(as.factor), factor_features)
}

build_features_test <- function(train_data, test_data, stores, extra_cols = character()) {
  start <- train_data %>% count(store) %$% max(n)
  test_data %>%
    build_features_day(start = start) %>%
    build_lagged_features_test(train_data, .) %>%
    inner_join(build_features_store(train_data, stores)) %>%
    select_(.dots = c(features, extra_cols)) %>%
    mutate_each_(funs(as.factor), factor_features)
} 
