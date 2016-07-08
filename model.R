setwd(Sys.getenv('ROSSMANN_HOME'))
source('load.R')

model_glm <- list(
  fit = function(x, y) {
    require(glmnet)
    valid <- complete.cases(x)
    hot1encoding <- x %$% cbind(
      model.matrix(~ storetype - 1),
      model.matrix(~ dayofweek - 1)
    )
    x %<>% select(-storetype, -dayofweek) %>% cbind(hot1encoding)
    cv.glmnet(
      x = as.matrix(x[valid, ]),
      y = y[valid],
      nfolds = ncores,
      parallel = TRUE,
      alpha = 1
    )
  },
  predict = function(obj, newdata) {
    features <- rownames(fit$glmnet.fit$beta)
    hot1encoding <- newdata %$% cbind(
      model.matrix(~ storetype - 1),
      model.matrix(~ dayofweek - 1)
    )
    newdata %<>% 
      select(-storetype, -dayofweek) %>% 
      cbind(hot1encoding) %>%
      select_(.dots = features)
    predict(obj, as.matrix(newdata), x = "lambda.1se")
  }
)

model_rf <- list(
  fit = function(x, y) {
    par_rf <- function(ntree, ...) {
      require(randomForest)
      require(foreach)
      foreach(
        ntree = rep(round(ntree/ncores), ncores), 
        .combine = randomForest::combine
      ) %dopar% randomForest(ntree = ntree, ...)
    }
    valid <- complete.cases(x)
    par_rf(
    # randomForest(
      x[valid, ],
      y[valid],
      # importance = TRUE,
      do.trace = TRUE,
      ntree = 700, 
      mtry = 4, 
      nodesize = round(0.0005 * nrow(x))
    )
  },
  predict = predict
)

model_gbm <- list(
  fit = function(x, y) {
    require(gbm)
    fit <- gbm.fit(
      x = as.data.frame(x), 
      y = y, 
      # nTrain = round(0.8 * nrow(x)),
      distribution = 'gaussian', 
      shrinkage = 0.01,
      n.tree = 3000
    )
    gbm.perf(fit)
    fit
  },
  predict = function(...) {
    predict(..., n.trees = 3000)
  }
)


model_xgb <- list(
  fit = function(x, y) {
    require(xgboost)
    in_val <- nrow(x) %>% sample(round(. / 50))
    dval <- xgb.DMatrix(
      data = data.matrix(x[in_val, ]),
      label = y[in_val],
      missing = NA
    )
    dtrain <- xgb.DMatrix(
      data = data.matrix(x[-in_val, ]),
      label = y[-in_val],
      missing = NA
    )
    watchlist <- list(val = dval, train = dtrain)
    params <- list(
      objective = "reg:linear",
      booster = "gbtree",
      eta = 0.02,
      subsample = 0.9,
      colsample_bytree = 0.7
    )
    xgb.train(
      params = params,
      data = dtrain,
      nrounds = 3000,
      verbose = 0,
      early.stop.round = 100,
      watchlist = watchlist,
      maximize = FALSE,
      feval = function(preds, dtrain) {
        predicted <- exp(as.numeric(preds))
        actual <- exp(as.numeric(getinfo(dtrain, 'label')))
        list(
          metric = "RMSPE",
          value = rmspe(predicted, actual)
        )
      }
    )
  },
  predict = function(fit, newdata) {
    predict(fit, data.matrix(newdata))
  }
)
