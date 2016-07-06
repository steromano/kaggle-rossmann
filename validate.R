setwd(Sys.getenv('ROSSMANN_HOME'))
source('load.R')
source('model.R')
source('features.R')

# Global vars
data <- train_test_data(n_stores = 50)

# Set model
model <- model_rf

# Build features
train_data <- build_features_train(data$train, stores)
test_data <- build_features_test(data$train, data$test, stores, extra_cols = c('date', 'store'))

# Train model
fit <- model$fit(
  x = select(train_data, -logsales),
  y = train_data$logsales
)

# Predict the test set
preds_df <- par_predict(model, fit, test_data) %T>% preds_summary
