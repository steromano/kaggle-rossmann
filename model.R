setwd(Sys.getenv('ROSSMANN_HOME'))
source('load.R')

model_glm <- list(
  fit = function(x, y) {
    glm(y ~ ., data = mutate(x, y = y))
  },
  predict = predict
)

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
      shrinkage = 0.03,
      n.tree = 10000
    )
    gbm.perf(fit)
    fit
  },
  predict = function(...) {
    predict(..., n.trees = 10000)
  }
)
