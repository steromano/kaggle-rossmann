setwd(Sys.getenv('ROSSMANN_HOME'))
source('load.R')

data <- train_test_data()

# time-independent prediction: store average
store_avg <- 
  data$train %>%
  group_by(store) %>%
  summarise(avg_sales = mean(sales, na.rm = TRUE))

data$test %>%
  inner_join(store_avg) %>%
  rename(predicted = avg_sales) %>%
  preds_summary

# day of week median for the store
store_dow_med <- 
  data$train %>%
  group_by(store, dayofweek) %>%
  summarise(med_sales = median(sales, na.rm = TRUE))

data$test %>%
  inner_join(store_dow_med) %>%
  rename(predicted = med_sales) %>%
  preds_summary