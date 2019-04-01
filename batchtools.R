library(batchtools)

## Here are a couple of data tables. The response is column 5,
## "PRES". Columns 6-36 are the predictors. You can probably ignore
## columns 1-4. In case you're curious, species 318 is Sugar Maple and
## species 123 is Table Mountain Pine, species with different life
## histories and distributions. Thanks for taking a look at these. As
## I mentioned yesterday, I think there's a relatively quick paper to
## get out of this and it would be great to work on it with you. Colin
## Quinn, one of Scott's Ph.D. students will also be helping out.

reg.dir <- "registry-earth-prop-zeros-new"
if(FALSE){
  unlink(reg.dir, recursive=TRUE)
}
reg <- if(file.exists(reg.dir)){
  loadRegistry(reg.dir)
}else{
  makeExperimentRegistry(reg.dir)
}

(spp.csv.vec <- normalizePath(Sys.glob("data/*")))
## ADD NEW DATA SETS TO data/ directory.
spp.csv <- spp.csv.vec[1]
spp <- fread(spp.csv)
all.X.mat <- as.matrix(spp[, 6:36])
all.y.vec <- spp$PRES
some.i <- as.integer(sapply(c(0,1), function(y)which(all.y.vec==y)[1:10]))
small.instance <- list(
    train.X.mat=all.X.mat[some.i,],
    train.y.vec=all.y.vec[some.i],
    train.weight.vec=rep(1, length(some.i)),
    test.X.mat=all.X.mat[1:2,],
    is.train="foo")
n.folds <- 10

addProblem("cv", reg=reg, fun=function(job, data, spp.csv, test.fold, n.folds, weight.name, ...){
  species.id <- namedCapture::str_match_variable(
    spp.csv,
    "_",
    id="[0-9]+")
  spp <- data.table::fread(spp.csv)
  all.X.mat <- as.matrix(spp[, 6:36])
  all.y.vec <- spp$PRES
  set.seed(1)
  all.fold.vec <- sample(rep(1:n.folds, l=nrow(all.X.mat)))
  is.train <- all.fold.vec != test.fold
  species.name.vec <- c(
    "318"="Sugar Maple",
    "123"="Table Mountain Pine")
  train.y.vec <- all.y.vec[is.train]
  response.tab <- table(train.y.vec)
  other.tab <- response.tab #need to assign weights to other class.
  names(other.tab) <- rev(names(response.tab))
  response.dec <- sort(response.tab, decreasing=TRUE)
  major.prob <- as.integer(names(response.dec)[1])
  large.weight.vec <- as.numeric(other.tab[paste(train.y.vec)])
  weight.list <- list(
    balanced=large.weight.vec,
    one=rep(1, length(large.weight.vec)))
  some.i <- as.integer(
    sapply(c(0,1), function(y)which(train.y.vec==y)[1:10]))#for testing.
  some.i <- seq_along(train.y.vec) #comment this line for testing.
  list(
    is.train=is.train,
    species.name=species.name.vec[species.id],
    train.X.mat=all.X.mat[is.train,][some.i,],
    train.y.vec=train.y.vec[some.i],
    train.weight.vec=weight.list[[weight.name]][some.i],
    test.X.mat=all.X.mat[!is.train,],
    test.y.vec=all.y.vec[!is.train])
})
makeFun <- function(expr){
  e <- substitute(expr)
  function(instance, ...){
    eval(e, instance)
  }
}
pred.fun.list <- list(
  ## ADD NEW ML ALGOS HERE
  glmnet=makeFun({
    fit <- glmnet::cv.glmnet(
      train.X.mat, factor(train.y.vec), family="binomial")
    pred.prob.vec <- predict(fit, test.X.mat, type="response")
    for(thresh in seq(0, 1, by=100)){
      ##compute whatever metric you want to optimize
      }
    list(fit=fit, pred.prob.vec=prob.prob.vec, best.thresh)
  }), xgboost=makeFun({
    xg.param <- list(
      objective="binary:logistic",
      eval_metric="auc"
    )
    xg.mat <- xgboost::xgb.DMatrix(
      train.X.mat, label=train.y.vec,
      weight=train.weight.vec, missing=NA)
    fit <- xgboost::xgb.train(
      params=xg.param,
      data=xg.mat,
      nrounds=50)
    list(fit=fit, pred.prob.vec=predict(fit, test.X.mat), is.train=is.train)
  }), earth=makeFun({
    train.df <- data.frame(
      label=factor(train.y.vec),
      train.X.mat,
      check.names=FALSE)
    fit <- earth::earth(
      label ~ ., data=train.df, weights=train.weight.vec,
      trace=3, #print progress.
      pmethod="cv", nfold=5, glm=list(family=binomial), degree=2)
    test.df <- data.frame(
      test.X.mat,
      check.names=FALSE)
    prop.zero <- colMeans(fit$dirs==0)
    ## earth model is too big to store! so we just store, for each
    ## feature, the proportion of terms which do not use it. features
    ## with prop.zero=1 are not used to make predictions.
    list(prop.zero=prop.zero, pred.prob.vec=predict(fit, test.df, type="response"))
  }), nearestNeighbors=makeFun({
    max.neighbors <- as.integer(min(50, nrow(train.X.mat)/2))
    fit <- nearestNeighbors::NearestNeighborsCV(
      train.X.mat, train.y.vec, max.neighbors, weight.vec=train.weight.vec)
    list(fit=fit$selected.neighbors, pred.prob.vec=fit$predict(test.X.mat))
  }), major.class=makeFun({
    response.tab <- table(train.y.vec)
    response.dec <- sort(response.tab, decreasing=TRUE)
    major.class <- as.integer(names(response.dec)[1])
    list(fit=response.dec, pred.prob.vec=rep(major.class, nrow(test.X.mat)))
  }))
algo.list <- list()
funs.to.launch <- names(pred.fun.list)
funs.to.launch <- "earth"
for(fun.name in funs.to.launch){
  pred.fun <- pred.fun.list[[fun.name]]
  small.result <- pred.fun(instance=small.instance)
  is.prob <- with(small.result, 0 <= pred.prob.vec & pred.prob.vec <= 1)
  if(!all(is.prob)){
    stop("some predictions not in[0,1]")
  }
  addAlgorithm(fun.name, reg=reg, fun=pred.fun)
  algo.list[[fun.name]] <- data.table()
}

addExperiments(
  list(cv=CJ(
    spp.csv=spp.csv.vec,
    n.folds=n.folds,
    weight.name=c("one", "balanced"),
    test.fold=1:n.folds)),
  algo.list,
  reg=reg)

summarizeExperiments(reg=reg)
unwrap(getJobPars(reg=reg))

job.table <- getJobTable(reg=reg)
chunks <- data.table(job.table, chunk=1)
submitJobs(chunks, reg=reg, resources=list(
  ##walltime = 24*60,#minutes
  walltime = 24*60*2,#minutes
  memory = 4000,#megabytes per cpu
  ncpus=2,
  ntasks=1,
  chunks.as.arrayjobs=TRUE))

while(1){
  print(getStatus())
  print(getErrorMessages())
  Sys.sleep(10)
}


