library(dplyr)
library(magrittr)
library(purrr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)
library(foreach)

rmspe <- function(predicted, actual) {
  sqrt(mean(((predicted - actual)/actual)^2))
}

cumulative <- function(f) {
  function(x, ...) {
    seq_along(x) %>%
      map_dbl(~ f(head(x, .), ...))
  }
}

seasonal_avg_impute <- function(x, frequency, order) {
  missing <- which(is.na(x))
  imputed <- x
  for (i in missing) {
    idxs <- (1:i)[(i - 1:i) %% frequency == 0]
    imputed[i] <- mean(x[tail(idxs, order + 1)], na.rm = TRUE)
    if(is.na(imputed[i])) {
      prev <- x[idxs]
      imputed[i] <- prev %>% discard(is.na) %>% last
    }
  }
  as.numeric(imputed)
}

preds_summary <- function(preds_df) {
  preds_df %>%
    filter(store %in% sample(
      unique(store), 
      min(10, length(unique(store))))
    ) %>%
    split(.$store) %>%
    map(
      ~ {
        r2 <- caret::R2(.$predicted, .$sales)
        store <- .$store[1]
        select(., date, predicted, sales) %>%
          gather(series, value, -date) %>%
          ggplot(aes(x = date, y = value, colour = series)) %>%
          + geom_line() %>%
          + ggtitle(paste("Store", store, "- R2:", round(r2, 4) * 100))
      }
    ) %>%
    map(print)
  preds_df %$% rmspe(predicted, sales) %>% cat('RMSPE', ., '\n')
  preds_df %$% caret::R2(predicted, sales) %>% cat('R^2', ., '\n')
  invisible(preds_df)
}

train_test_data <- function(n_stores = 50, 
                            train_end_date = ymd("2015-06-19")) {
  data <- 
    read_csv('data/clean/train_clean.csv') %>%
    filter(store %in% sample(unique(store), n_stores))

  list(
    train = filter(data, date <= train_end_date),
    test = filter(data, date > train_end_date),
    stores = read_csv('data/clean/store_clean.csv')
  )
}

predict_store <- function(model, fit, store_data) {
  predictions <- numeric()
  for (i in 1:nrow(store_data)) {
    if (store_data[i, ] %>% map_lgl(is.na) %>% any) {
      print(store_data[i, ])
      stop()
    }
    prediction <- model$predict(fit, store_data[i, ])
    predictions[i] <- prediction
    for (col in paste0('lagged_', lags)) {
      j <- Position(is.na, store_data[[col]])
      if (!is.na(j)) {
        store_data[j, col] <- prediction
      }
    }
  }
  store_data %>%
    mutate(predicted = exp(predictions) * 0.985)
}

par_predict <- function(model, fit, newdata) {
  ncores <- parallel::detectCores()
  doParallel::registerDoParallel(cores = ncores)
  batch_assignments <- 
    data_frame(
      store = unique(newdata$store)
    ) %>%
    mutate(
      batch_n = rep(
        1:ncores, each = (nrow(.)/ncores), 
        length.out = nrow(.)
      )
    )
  
  batches <- 
    newdata %>%
    inner_join(batch_assignments) %>%
    split(.$batch_n)
  
  foreach(batch = batches, .combine = bind_rows) %dopar% {
    batch %>%
      group_by(store) %>%
      arrange(date) %>%
      do(predict_store(model, fit, .)) %>%
      ungroup
  }
}
