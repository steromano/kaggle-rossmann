setwd(Sys.getenv('ROSSMANN_HOME'))
source('load.R')

model_rf <- list(
  fit = function(x, y) {
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
    par_rf(x, y, ntree = 700, mtry = 2, nodesize = 20)
  },
  predict = predict
)

model_gbm <- list(
  fit = function(x, y) {
    require(gbm)
    fit <- gbm.fit(
      x = as.data.frame(x), 
      y = y, 
      distribution = 'gaussian', 
      shrinkage = 0.005,
      n.tree = 10000,
      nTrain = round(0.8 * nrow(x))
    )
    gbm.perf(fit)
    fit
  },
  predict = function(...) {
    predict(..., ntree = 10000)
  }
)
