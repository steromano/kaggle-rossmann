setwd(Sys.getenv('ROSSMANN_HOME'))
source('load.R')

data <- train_test_data(n_stores = 1100)

# time-independent store avg
store_avg <- 
  data$train %>%
  group_by(store) %>%
  summarise(avg_sales = mean(sales, na.rm = TRUE))

data$test %>%
  inner_join(store_avg) %>%
  rename(predicted = avg_sales) %>%
  preds_summary

# day of week - store median
store_dow_med <- 
  data$train %>%
  group_by(store, dayofweek) %>%
  summarise(med_sales = median(sales, na.rm = TRUE))

data$test %>%
  inner_join(store_dow_med) %>%
  rename(predicted = med_sales) %>%
  preds_summary

# day of week - store - promo geometric mean
store_dow_promo_gm <- 
  data$train %>%
  group_by(store, dayofweek, promo) %>%
  summarise(gm_sales = exp(mean(log(sales), na.rm = TRUE)))

data$test %>%
  inner_join(store_dow_promo_gm) %>%
  rename(predicted = gm_sales) %>%
  preds_summary