setwd(Sys.getenv('ROSSMANN_HOME'))
source('load.R')
source('features.R')
source('model.R')

# Load data -------------
train_clean <- read_csv('data/clean/train_clean.csv')
store_clean <- read_csv('data/clean/store_clean.csv')
test_clean <- read_csv('data/clean/test_clean.csv')

# Training --------------
model <- model_rf_xgb
model_name <- 'ensemble'

train_data <- build_features_train(
  train_clean, 
  store_clean, 
  extra_cols = 'store'
)
test_data <- build_features_test(
  train_clean, 
  test_clean, 
  store_clean, 
  extra_cols = c('store', 'date')
)

train_data %<>% filter(store %in% test_data$store)

fit <- model$fit(
  x = select_(train_data, .dots = features),
  y = train_data$logsales
)
saveRDS(fit, sprintf('output/%s_fit.rds', model_name))

# Predicting -------------
preds_df <- par_predict(model, fit, test_data)
saveRDS(preds_df, sprintf('output/%s_preds.rds', model_name))

# For stores open on Sunday, use a simple historical
# geometric mean to predict Sunday sales
sunday_preds <-
  train_clean %>%
  filter(dayofweek == 7) %>%
  group_by(store) %>%
  summarise(sunday_predicted = exp(mean(log(sales + 1), na.rm = TRUE)) - 1) %>%
  mutate(sunday_predicted = ifelse(is.na(sunday_predicted), 0, sunday_predicted))

preds_df %>%
  select(store, date, predicted) %>%
  right_join(test_clean) %>%
  inner_join(sunday_preds) %>%
  mutate(predicted = ifelse(
    dayofweek == 7,
    sunday_predicted,
    predicted
  )) %>%
  select(Id = id, Sales = predicted) %>%
  write_csv(sprintf('output/%s_submission.csv', model_name))



