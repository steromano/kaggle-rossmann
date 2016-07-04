library(dplyr)
library(magrittr)
library(purrr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)


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

par_rf <- function(ntree, ...) {
  require(randomForest)
  require(foreach)
  ncores <- parallel::detectCores()
  doParallel::registerDoParallel(cores = ncores)
  foreach(
    ntree = rep(round(ntree/ncores), ncores), 
    .combine = randomForest::combine
  ) %dopar% randomForest(ntree = ntree, ...)
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
  stores <- read_csv('data/clean/store_clean.csv')
  data <- 
    read_csv('data/clean/train_clean.csv') %>%
    filter(store %in% sample(unique(store), n_stores)) %>%
    inner_join(stores, by = 'store')
  
  list(
    train = filter(data, date <= train_end_date),
    test = filter(data, date > train_end_date)
  )
}
