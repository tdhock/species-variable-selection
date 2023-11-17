### Write down what package versions work with your R code, and
### attempt to download and load those packages. The first argument is
### the version of R that you used, e.g. "3.0.2" and then the rest of
### the arguments are package versions. For
### CRAN/Bioconductor/R-Forge/etc packages, write
### e.g. RColorBrewer="1.0.5" and if RColorBrewer is not installed
### then we use install.packages to get the most recent version, and
### warn if the installed version is not the indicated version. For
### GitHub packages, write "user/repo@commit"
### e.g. "tdhock/animint@f877163cd181f390de3ef9a38bb8bdd0396d08a4" and
### we use install_github to get it, if necessary.
works_with_R <- function(Rvers,...){
  local.lib <- file.path(getwd(), "library")
  dir.create(local.lib, showWarnings=FALSE, recursive=TRUE)
  .libPaths(local.lib)
  pkg_ok_have <- function(pkg,ok,have){
    stopifnot(is.character(ok))
    if(!as.character(have) %in% ok){
      warning("works with ",pkg," version ",
              paste(ok,collapse=" or "),
              ", have ",have)
    }
  }
  pkg_ok_have("R",Rvers,getRversion())
  pkg.vers <- list(...)
  for(pkg.i in seq_along(pkg.vers)){
    vers <- pkg.vers[[pkg.i]]
    pkg <- if(is.null(names(pkg.vers))){
      ""
    }else{
      names(pkg.vers)[[pkg.i]]
    }
    if(pkg == ""){# Then it is from GitHub.
      ## suppressWarnings is quieter than quiet.
      if(!suppressWarnings(require(requireGitHub))){
        ## If requireGitHub is not available, then install it using
        ## devtools.
        if(!suppressWarnings(require(devtools))){
          install.packages("devtools")
          require(devtools)
        }
        install_github("tdhock/requireGitHub")
        require(requireGitHub)
      }
      requireGitHub(vers)
    }else{# it is from a CRAN-like repos.
      if(!suppressWarnings(require(pkg, character.only=TRUE))){
        install.packages(pkg)
      }
      pkg_ok_have(pkg, vers, packageVersion(pkg))
      library(pkg, character.only=TRUE)
    }
  }
}
works_with_R(
  "3.5.1",
  nc="2020.1.16",
  batchtools="0.9.11",
  WeightedROC="2018.10.1",
  data.table="1.0",
  glmnet="2.0.16",
  snow="0.4.3",
  earth="4.7.0",
  RJSONIO="1.3.1.4",
  "tdhock/animint2@32c5035934b3cc2490bfcd771a682e4f2c09fe65")

## Here are a couple of data tables. The response is column 5,
## "PRES". Columns 6-36 are the predictors. You can probably ignore
## columns 1-4. In case you're curious, species 318 is Sugar Maple and
## species 123 is Table Mountain Pine, species with different life
## histories and distributions. Thanks for taking a look at these. As
## I mentioned yesterday, I think there's a relatively quick paper to
## get out of this and it would be great to work on it with you. Colin
## Quinn, one of Scott's Ph.D. students will also be helping out.

reg.dir <- "registry-expired"
reg <- batchtools::loadRegistry(reg.dir)
library(data.table)
spp.csv.vec <- grep("some", normalizePath(Sys.glob("data/*")), invert=TRUE, value=TRUE)
all.y.list <- list()
all.X.list <- list()
species.name.vec <- c(
  "318"="Sugar Maple",
  "123"="Table Mountain Pine")
names(species.name.vec) <- paste0(
  "spp_env_", names(species.name.vec), ".csv")
for(spp.csv in spp.csv.vec){
  spp <- fread(spp.csv)
  species <- species.name.vec[[basename(spp.csv)]]
  all.y.list[[species]] <- spp$PRES
  all.X.list[[species]] <- as.matrix(spp[, 6:36])
}
X.sc <- scale(all.X.list[[1]])
X.center <- attr(X.sc, "scaled:center")
X.scale <- attr(X.sc, "scaled:scale")
X.my <- t((t(all.X.list[[1]])-X.center)/X.scale)
all.equal(as.numeric(X.sc), as.numeric(X.my))
all.equal(all.X.list[[1]], all.X.list[[2]])

## Registry directories come from running the machine learning algos
## in parallel on monsoon using the batchtools package. There are two
## registries below because registry-expired has most of the algos,
## and registry-earth-prop-zeros has the earth model where we only
## saved the predictions and selected variables (the other info is way
## too big for some reason).
registry <- function(reg.dir, subset.fun){
  list(reg.dir=reg.dir, subset.fun=subset.fun)
}
reg.list <- list(
  registry("registry-expired", function(dt){
    dt[algorithm != "earth"]
  }),
  registry("registry-earth-prop-zeros", identity)
)


done.list <- list()
algo.result.list <- list()
for(reg.info in reg.list){
  one.reg <- loadRegistry(reg.info$reg.dir)
  one.jobs <- getJobTable(reg=one.reg)
  some.jobs <- reg.info$subset.fun(one.jobs)[!is.na(done)]
  next.indices <-
    (length(algo.result.list)+1):(length(algo.result.list)+nrow(some.jobs))
  algo.result.list[next.indices] <- lapply(some.jobs$job.id, loadResult)
  done.list[[reg.info$reg.dir]] <- some.jobs
}
length(algo.result.list)
done <- do.call(rbind, done.list)
for(name in names(done$prob.pars[[1]])){
  done[[name]] <- sapply(done$prob.pars, "[[", name)
}
n.folds <- max(done$test.fold)
done[, species := species.name.vec[paste(basename(spp.csv))]]
done[, table(algorithm)]
done[, table(basename(spp.csv), species)]
done[, list.index := 1:.N]
xgboost1 <- done[algorithm=="xgboost" & weight.name=="one"]

auc.dt.list <- list()
roc.dt.list <- list()
glmnet.dt.list <- list()
earth.dt.list <- list()
for(test.set.i in 1:nrow(xgboost1)){
  test.set.info <- xgboost1[test.set.i]
  print(test.set.info)
  on.vec <- c("test.fold", "species")
  test.set.meta <- test.set.info[, ..on.vec]
  test.set.algos <- done[test.set.meta, on=on.vec]
  is.train <- algo.result.list[[test.set.info$list.index]]$is.train
  test.y.vec <- all.y.list[[species]][!is.train]
  test.X.mat <- all.X.list[[species]][!is.train,]
  for(algo.i in 1:nrow(test.set.algos)){
    result.i <- test.set.algos$list.index[[algo.i]]
    algo.result <- algo.result.list[[result.i]]
    algo.name <- test.set.algos$algorithm[[algo.i]]
    algo.meta <- data.table(
      test.set.meta,
      algorithm=algo.name,
      weight.name=test.set.algos$weight.name[[algo.i]])
    if(algo.name=="earth"){
      earth.dt.list[[paste(test.set.i, algo.i)]] <-
        with(algo.result, data.table(
          algo.meta,
          prop.zero,
          feature=names(prop.zero)))
    }
    if(algo.name=="glmnet"){
      weight.vec <- coef(algo.result$fit)[-1,]
      glmnet.dt.list[[paste(test.set.i, algo.i)]] <- data.table(
        algo.meta,
        feature=names(weight.vec),
        weight=weight.vec,
        norm.weight=weight.vec*X.scale)
    }
    pred.prob.vec <- if(algo.name%in%c("glmnet")){
      predict(algo.result$fit, test.X.mat, type="response")
    }else{
      algo.result$pred.prob.vec
    }
    roc.df <- WeightedROC(pred.prob.vec, test.y.vec)
    pred.class.vec <- ifelse(pred.prob.vec<0.5, 0, 1)
    test.labels.negative <- sum(test.y.vec==0)
    test.labels.positive <- sum(test.y.vec==1)
    roc.dt.list[[paste(test.set.i, algo.i)]] <- data.table(
      algo.meta, roc.df,
      test.labels=length(test.y.vec),
      test.labels.negative,
      test.labels.positive)
    auc.dt.list[[paste(test.set.i, algo.i)]] <- data.table(
      algo.meta,
      FPR=sum(pred.class.vec==1 & test.y.vec==0)/test.labels.negative,
      TPR=sum(pred.class.vec==1 & test.y.vec==1)/test.labels.positive,
      ## ADD EVAL METRIC HERE.
      accuracy.percent=mean(pred.class.vec==test.y.vec)*100,
      auc=WeightedAUC(roc.df))
  }
}
auc.dt <- do.call(rbind, auc.dt.list)
roc.dt <- do.call(rbind, roc.dt.list)
glmnet.dt <- do.call(rbind, glmnet.dt.list)
earth.dt <- do.call(rbind, earth.dt.list)
data.list <- list(
  auc=auc.dt,
  roc=roc.dt,
  glmnet=glmnet.dt,
  earth=earth.dt)
data.list$label.tab <- list()
for(spp.csv in spp.csv.vec){
  species.name <- species.name.vec[sub(".csv", "", sub(".*_", "", spp.csv))]
  spp <- fread(spp.csv)
  data.list[["label.tab"]][[species.name]] <- table(spp$PRES)
}
saveRDS(data.list,  "figure-batchtools-expired-earth.rds")
data.table::fwrite(auc.dt, "figure-batchtools-expired-earth-auc.csv")
data.table::fwrite(roc.dt, "figure-batchtools-expired-earth-roc.csv")
