library(data.table)
future::plan(future::multiprocess)

## Here are a couple of data tables. The response is column 5,
## "PRES". Columns 6-36 are the predictors. You can probably ignore
## columns 1-4. In case you're curious, species 318 is Sugar Maple and
## species 123 is Table Mountain Pine, species with different life
## histories and distributions. Thanks for taking a look at these. As
## I mentioned yesterday, I think there's a relatively quick paper to
## get out of this and it would be great to work on it with you. Colin
## Quinn, one of Scott's Ph.D. students will also be helping out.

species.name.vec <- c(
  "318"="Sugar Maple",
  "123"="Table Mountain Pine")
n.folds <- 10
spp.csv.vec <- Sys.glob("data/*")
spp.csv <- spp.csv.vec[1]
cv <- list()
for(spp.csv in spp.csv.vec){
  species.id <- namedCapture::str_match_variable(
    spp.csv,
    "_",
    id="[0-9]+")
  species.name <- species.name.vec[species.id]
  spp <- fread(spp.csv)
  all.X.mat <- as.matrix(spp[, 6:36])
  all.y.vec <- spp$PRES
  set.seed(1)
  all.fold.vec <- sample(rep(1:n.folds, l=nrow(all.X.mat)))
  table(all.fold.vec, all.y.vec)
  test.fold <- 1
  OneFold <- function(test.fold){
    is.train <- all.fold.vec != test.fold
    ## i <- as.integer(seq(1, length(is.train), l=1000))
    ## is.train <- seq_along(all.fold.vec) %in% i
    train.X.mat <- all.X.mat[is.train,]
    train.y.vec <- all.y.vec[is.train]
    train.dt <- data.table(
      label=factor(train.y.vec),
      train.X.mat,
      check.names=FALSE)
    response.tab <- table(train.y.vec)
    print(response.tab)
    other.tab <- response.tab #need to assign weights to other class.
    names(other.tab) <- rev(names(response.tab))
    response.dec <- sort(response.tab, decreasing=TRUE)
    major.prob <- as.integer(names(response.dec)[1])
    large.weight.vec <- as.numeric(other.tab[paste(train.dt$label)])
    weight.list <- list(
      balanced=large.weight.vec,
      one=rep(1, nrow(train.dt)))
    weight.name <- "balanced"
    results.by.weight <- list()
    weight.name.vec <- names(weight.list)
    ##weight.name.vec <- "balanced"
    for(weight.name in weight.name.vec){
      cat(sprintf(
        "%d / %d folds weights=%s\n",
        test.fold, n.folds, weight.name))
      train.weight.vec <- weight.list[[weight.name]]
      xg.param <- list(
        objective="binary:logistic",
        eval_metric="auc"
        )
      xg.mat <- xgboost::xgb.DMatrix(
        train.X.mat, label=train.y.vec,
        weight=train.weight.vec, missing=NA)
      fit.list <- list(
        cforest=party::cforest(
          label ~ ., train.dt, weights=train.weight.vec),
        ctree=party::ctree(
          label ~ ., train.dt, weights=train.weight.vec))
      print("before pred")
      test.X.mat <- all.X.mat[!is.train,]
      test.X.dt <- data.table(test.X.mat)
      test.y.vec <- all.y.vec[!is.train]
      pred.prob.list <- list(
        baseline=rep(major.prob, nrow(test.X.mat)))
      for(model in names(fit.list)){
        fit <- fit.list[[model]]
        tree.list <- party::treeresponse(fit, test.X.dt)
        pred.prob.list[[model]] <- sapply(tree.list, "[", 2)
      }
      print("before earth")
      fit.list$earth <- earth::earth(
        label ~ ., data=train.dt, weights=train.weight.vec)
      pred.prob.list$earth <- predict(fit.list$earth, test.X.dt)
      print("before nn")
      fit.nn <- nearestNeighbors::NearestNeighborsCV(
        train.X.mat, train.y.vec, 50L, weight.vec=train.weight.vec)
      pred.prob.list$nn <- fit.nn$predict(test.X.mat)
      print("before glmnet")
      fit.list$glmnet <- glmnet::cv.glmnet(
        train.X.mat, train.y.vec, family="binomial")
      pred.prob.list$glmnet <- predict(fit.list$glmnet, test.X.mat)
      print("before xgboost")
      fit.list$xgboost <- xgboost::xgb.train(
        params=xg.param,
        data=xg.mat,
        nrounds=50)
      pred.prob.list$xgboost <- predict(fit.list$xgboost, test.X.mat)
      print("before acc")
      accuracy.dt.list <- list()
      for(model in names(pred.prob.list)){
        pred.prob <- pred.prob.list[[model]]
        pred.class <- ifelse(pred.prob < 0.5, 0, 1)
        roc.df <- WeightedROC::WeightedROC(pred.prob, test.y.vec)
        auc <- WeightedROC::WeightedAUC(roc.df)
        accuracy.dt.list[[paste(weight.name, model)]] <- data.table(
          weight.name, model, test.fold, species.name,
          auc,
          percent.correct=100*mean(test.y.vec != pred.class))
      }
      results.by.weight[[weight.name]] <- list(
        accuracy=do.call(rbind, accuracy.dt.list),
        models=fit.list,
        predictions=do.call(cbind, pred.prob.list))
    }#weight.name
    results.by.weight
  }#OneFold
  LAPPLY <- parallel::mclapply
  LAPPLY <- lapply
  LAPPLY <- future.apply::future_lapply
  cv[[species.name]] <- LAPPLY(1:n.folds, OneFold)
}

saveRDS(cv, "cv.rds")
