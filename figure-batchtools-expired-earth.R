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
  "tdhock/animint2@dbc4f15dbd6db9bc6b70c121fcd96d5f3a7adf98")

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
saveRDS(list(
  auc=auc.dt,
  roc=roc.dt,
  glmnet=glmnet.dt,
  earth=earth.dt),
  "figure-batchtools-expired-earth.rds")
data.table::fwrite(auc.dt, "figure-batchtools-expired-earth-auc.csv")
data.table::fwrite(roc.dt, "figure-batchtools-expired-earth-roc.csv")
zero.counts <- glmnet.dt[, list(
  zeros=sum(weight==0),
  nonzeros=sum(weight!=0),
  percent.zero=mean(weight==0)*100,
  percent.nonzero=mean(weight!=0)*100,
  mean.norm.weight=mean(norm.weight),
  count=.N
  ), by=list(species, weight.name, feature)]
auc.tall <- melt(auc.dt, measure.vars=c("accuracy.percent", "auc"))

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(species ~ variable, scales="free")+
  geom_point(aes(
    value, algorithm, color=weight.name),
    shape=1,
    data=auc.tall)

(stats.dt <- auc.tall[, list(
  q25=quantile(value, 0.25),
  median=median(value),
  q75=quantile(value, 0.75),
  N=.N
), by=list(species, algorithm, weight.name, variable)])

species.name <- "Sugar Maple"

for(species.name in species.name.vec){

  species.stats <- stats.dt[
    variable=="auc" & weight.name=="one" & species==species.name]
  ord.stats <- species.stats[order(median)]
  species.tall <- auc.tall[species==species.name]
  species.tall[, Algorithm := factor(algorithm, ord.stats$algorithm)]
  min.auc <- species.tall[
    algorithm != "major.class" & variable=="auc", min(value)]
  if(0.5 < min.auc){
    species.tall[algorithm=="major.class" & variable=="auc", value := -Inf]
  }
  ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(species ~ variable, scales="free")+
    geom_point(aes(
      value, Algorithm, color=weight.name),
      shape=1,
      data=species.tall)+
    xlab("")

  ##
  options(width=100)
  some.spec <- species.tall[weight.name=="balanced" & variable=="accuracy.percent"]
  some.spec[, accuracy := value/100]
  some.spec.tall <- melt(
    some.spec,
    measure.vars=c("accuracy", "FPR", "TPR"),
    variable.name="metric",
    id.vars=c("test.fold", "algorithm"))[test.fold <10]
  some.spec.tall[, percent := value*100]
  some.spec.stats <- some.spec.tall[, .(
    mean=mean(percent),
    min=min(percent),
    max=max(percent),
    sd=sd(percent),
    folds=.N
  ), by=.(metric, algorithm)]
  some.spec.mid <- some.spec.stats[, .(mid=(max(max)+min(min))/2), by=metric][some.spec.stats, on="metric"]
  algo.levs <- some.spec.mid[metric=="accuracy"][order(mean), algorithm]
  some.spec.tall[, Algorithm := factor(algorithm, algo.levs)]
  some.spec.mid[, Algorithm := factor(algorithm, algo.levs)]
  some.spec.mid[, pos := ifelse(mean<mid, "right", "left")]

  spaces <- "  "
  gg.default <- ggplot()+
    ggtitle("Prediction accuracy/error metrics at default threshold")+
    geom_point(aes(
      percent, Algorithm),
      data=some.spec.tall)+
    xlab("percent (mean +/- sd)")+
    geom_text(aes(
      ifelse(pos=="left", min, max), Algorithm,
      ##vjust=ifelse(Algorithm==levels(Algorithm)[1], -0.5, 1.5),
      hjust=ifelse(pos=="left", 1, 0),
      label=sprintf("%s%.3f +/- %.3f%s", spaces, mean, sd, spaces)),
      data=some.spec.mid)+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(. ~ metric, labeller=label_both, scales="free")
  species.dash <- gsub(" ", "-", species.name)
  png(paste0(
    "figure-batchtools-expired-earth-metrics-default-", species.dash, ".png"),
    12, 2, units="in", res=100)
  print(gg.default)
  dev.off()

  species.roc <- roc.dt[species==species.name]
  approx.roc <- species.roc[, {
    with(approx(FPR, TPR, seq(0, 1, l=201)), data.table(
      FPR=x, TPR=y))
    }, by=list(test.fold, algorithm, weight.name)]
  ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_wrap("test.fold")+
    geom_point(aes(
      FPR, TPR, fill=algorithm),
      color="black",
      shape=21,
      data=species.tall)+
    geom_path(aes(
      FPR, TPR, color=algorithm, linetype=weight.name,
      group=paste(algorithm, weight.name)),
      data=approx.roc)+
    coord_equal()

  one.fold.id <- 1
  round.factor <- 100
  one.roc.fold <- species.roc[weight.name=="balanced" & test.fold==one.fold.id]
  one.roc.fold[, round.FPR := round(FPR*round.factor)/round.factor]
  one.roc.approx <- one.roc.fold[, .SD[1], by=.(algorithm, round.FPR)]
  one.roc.approx[, errors := FP+FN]
  one.roc.approx[, error.prop := errors/test.labels]
  one.roc.approx[, accuracy.prop := 1-error.prop]
  one.roc.dots <- species.tall[weight.name=="balanced" & test.fold==one.fold.id]
  ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    geom_point(aes(
      FPR, TPR, fill=algorithm),
      color="black",
      shape=21,
      data=one.roc.dots)+
    geom_path(aes(
      FPR, TPR, color=algorithm, linetype=weight.name,
      group=paste(algorithm, weight.name)),
      data=one.roc.approx)+
    coord_equal()


  round.factor <- 100
  get.some.roc <- function(dt)dt[weight.name=="balanced" & test.fold<10]
  some.roc.folds <- get.some.roc(species.roc)
  some.roc.folds[, round.FPR := round(FPR*round.factor)/round.factor]
  some.roc.mid <- some.roc.folds[!round.FPR %in% c(0,1), .SD[1], by=.(algorithm, test.fold, round.FPR)]
  some.roc.01 <- some.roc.folds[(FPR==0&TPR==0) | (FPR==1&TPR==1)]
  some.roc.approx <- rbind(some.roc.mid, some.roc.01)[order(test.fold, algorithm, FPR)]
  some.roc.approx[, .(count=.N), by=.(algorithm, round.FPR)][count!=max(count)]
  some.roc.approx[, errors := FP+FN]
  some.roc.approx[, error.prop := errors/test.labels]
  some.roc.approx[, accuracy := 1-error.prop]
  some.roc.dots <- get.some.roc(species.tall)
  ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    geom_point(aes(
      FPR, TPR, fill=algorithm),
      color="black",
      shape=21,
      data=some.roc.dots)+
    geom_line(aes(
      FPR, TPR, color=algorithm,
      group=paste(algorithm, test.fold)),
      data=some.roc.approx)+
    coord_equal()

  some.roc.approx[, FP.possible := test.labels.negative]
  some.roc.approx[, FN.possible := test.labels.positive]
  some.roc.approx[, FP.count := FP]
  some.roc.approx[, FN.count := FN]

  some.roc.possible <- nc::capture_melt_multiple(
    some.roc.approx,
    metric="F[PN]",
    "[.]",
    column=".*")[order(algorithm, test.fold, FPR.percent, metric)]
  some.roc.possible[, max.possible := cumsum(possible), by=.(algorithm, test.fold, FPR.percent)]
  some.roc.possible[, min.possible := max.possible-possible]
  some.roc.possible[, label := ifelse(metric=="FP", "negative", "positive")]
  some.roc.possible[, count := ifelse(metric=="FP", FP, FN)]#BUG in nc?

  some.roc.approx[, FPR.percent := round.FPR*100]
  some.roc.approx.tall <- melt(
    some.roc.approx,
    measure.vars=c("accuracy", "FPR", "TPR"),
    id.vars=c("algorithm", "test.fold", "FPR.percent", "threshold"),
    variable.name="metric")[order(algorithm, test.fold, metric, threshold)]
  inv.logistic <- function(p)-log(1/p - 1)
  some.roc.approx.tall[, real.threshold := ifelse(
    algorithm=="major.class", threshold, ifelse(
      threshold==Inf, Inf, inv.logistic(threshold)))]
  thresh.trans <- function(x)x
  thresh.trans <- function(x)1/(1+exp(-x))
  some.roc.approx.tall[, min.thresh := thresh.trans(c(
    -Inf, real.threshold[-.N])), by=.(algorithm, test.fold, metric)]
  some.roc.approx.tall[, percent := value*100]
  some.roc.approx.tall[, next.percent := c(
    percent[-1], NA), by=.(algorithm, test.fold, metric)]
  some.roc.approx.tall[, max.thresh := thresh.trans(real.threshold)]
  some.roc.approx.tall[metric=="FPR", summary(FPR.percent-percent)]
  hline.dt <- some.roc.approx.tall[, .(
    percent=range(percent)
  ), by=.(metric)]

  ggplot()+
    geom_hline(aes(
      yintercept=percent),
      color="grey",
      data=hline.dt)+
    geom_segment(aes(
      min.thresh, percent,
      color=algorithm,
      xend=max.thresh, yend=percent),
      data=some.roc.approx.tall)+
    geom_segment(aes(
      max.thresh, percent,
      color=algorithm,
      xend=max.thresh, yend=next.percent),
      data=some.roc.approx.tall[is.finite(next.percent)])+
    scale_color_manual(values=algo.colors)+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(metric ~ algorithm)

  some.roc.approx.stats <- some.roc.approx.tall[, .(
    mean=mean(percent),
    min=min(percent),
    max=max(percent),
    sd=sd(percent),
    folds=.N
  ), by=.(metric, algorithm, FPR.percent)]
  some.roc.approx.mid <- some.roc.approx.stats[, .(mid=(max(max)+min(min))/2), by=metric][some.roc.approx.stats, on="metric"]
  some.roc.approx.mid[, pos := ifelse(mean<mid, "right", "left")]

  ggplot()+
      ggtitle("Class balance errors plot")+
      theme_bw()+
      theme(panel.margin=grid::unit(0, "lines"))+
      facet_grid(Algorithm ~ FPR.percent)+
      geom_rect(aes(
        xmin=test.fold-0.5, ymin=min.possible,
        fill=label,
        xmax=test.fold+0.5, ymax=max.possible),
        color="black",
        data=some.roc.possible[FPR.percent==0])


  algo.colors <- c(
    xgboost="blue",
    glmnet="red",
    major.class="black")
  ## for top roc plot:
  top.levs <- names(algo.colors)
  some.roc.approx[, Algorithm := factor(algorithm, top.levs)]
  some.roc.dots[, Algorithm := factor(algorithm, top.levs)]
  ## for bottom metrics plot:
  bottom.levs <- rev(names(algo.colors))
  some.roc.approx.mid[, Algorithm := factor(algorithm, bottom.levs)]
  some.roc.approx.tall[, Algorithm := factor(algorithm, bottom.levs)]
  some.roc.approx.tall[, Algorithm.thresh := factor(algorithm, top.levs)]
  viz <- animint(
    title="ROC curves and error/accuracy metrics",
    area=ggplot()+
      ggtitle("Class balance errors plot")+
      theme_bw()+
      theme(panel.margin=grid::unit(0, "lines"))+
      theme_animint(width=250)+
      facet_grid(. ~ Algorithm)+
      geom_rect(aes(
        xmin=test.fold-0.5, ymin=min.possible,
        fill=label,
        color=Algorithm,
        linetype=status,
        xmax=test.fold+0.5, ymax=max.possible),
        showSelected="Algorithm",
        data=data.table(some.roc.possible[FPR.percent==0], status="correct"))+
      scale_linetype_manual(
        values=c(correct=0, error=1),
        guide=guide_legend(override.aes=list(fill="grey90")))+
      scale_fill_manual(
        values=c(positive="grey30", negative="grey70"))+
      scale_color_manual(values=algo.colors)+
      guides(color="none")+
      scale_x_continuous("Test fold", breaks=1:9)+
      ylab("Observations in test set")+
      geom_rect(aes(
        xmin=test.fold-0.5,
        ymin=ifelse(label=="negative", max.possible-count, min.possible),
        fill=label,
        color=Algorithm,
        linetype=status,
        key=paste(Algorithm, test.fold, label),
        xmax=test.fold+0.5,
        ymax=ifelse(label=="negative", max.possible, min.possible+count)),
        showSelected=c(Algorithm="FPR.percent", "Algorithm"),
        data=data.table(some.roc.possible, status="error")),
    thresh=ggplot()+
      ggtitle("Metrics as a function of threshold")+
      scale_x_continuous(
        "Threshold = smallest predicted probability which is classified as positive",
        breaks=seq(0, 1, by=0.2),
        labels=c("0", "0.2", "0.4", "0.6", "0.8", "1"))+
      geom_vline(aes(
        xintercept=default.thresh),
        color="grey",
        data=data.table(default.thresh=0.5))+
      geom_hline(aes(
        yintercept=percent),
        color="grey",
        data=hline.dt)+
      geom_segment(aes(
        max.thresh, percent,
        color=Algorithm,
        xend=max.thresh, yend=next.percent),
        size=1,
        alpha=0.2,
        showSelected="Algorithm",
        data=some.roc.approx.tall[is.finite(next.percent)])+
      geom_segment(aes(
        min.thresh, percent,
        color=Algorithm,
        xend=max.thresh, yend=percent),
        size=1,
        alpha=0.2,
        showSelected="Algorithm",
        data=some.roc.approx.tall)+
      scale_color_manual(values=algo.colors)+
      scale_fill_manual(values=algo.colors)+
      geom_point(aes(
        ifelse(
          max.thresh==Inf, min.thresh+1, ifelse(
            min.thresh==-Inf, max.thresh-1, (min.thresh+max.thresh)/2)),
        percent,
        fill=Algorithm),
        alpha=0.5,
        size=2,
        showSelected="Algorithm",
        clickSelects=c(Algorithm="FPR.percent"),
        data=some.roc.approx.tall)+
      theme_bw()+
      theme(panel.margin=grid::unit(0, "lines"))+
      theme_animint(width=400)+
      guides(color="none", fill="none")+
      facet_grid(metric ~ Algorithm.thresh),#scales="free" is buggy...
    roc=ggplot()+
      ggtitle("ROC curves, select FPR")+
      theme_bw()+
      theme(panel.margin=grid::unit(0, "lines"))+
      scale_color_manual(values=algo.colors)+
      scale_fill_manual(values=algo.colors)+
      geom_line(aes(
        FPR, TPR, color=Algorithm,
        key=paste(Algorithm, test.fold),
        group=paste(Algorithm, test.fold)),
        alpha=0.2,
        size=1,
        data=some.roc.approx)+
      geom_point(aes(
        FPR, TPR, fill=Algorithm,
        key=paste(Algorithm, test.fold)),
        color="white",
        shape=21,
        data=some.roc.dots)+
      geom_point(aes(
        FPR, TPR,
        key=paste(Algorithm, test.fold, FPR.percent),
        fill=Algorithm),
        color="black",
        clickSelects=c(Algorithm="FPR.percent"),
        alpha=0.5,
        size=4,
        data=some.roc.approx)+
      coord_equal(),
    metrics=ggplot()+
      ggtitle("Prediction accuracy/error metrics at selected threshold")+
      scale_color_manual(values=algo.colors)+
      geom_point(aes(
        percent, Algorithm, color=Algorithm,
        key=paste(Algorithm, test.fold)),
        showSelected=c(Algorithm="FPR.percent", "Algorithm"),
        data=some.roc.approx.tall)+
      xlab("percent (mean +/- sd)")+
      geom_text(aes(
        ifelse(pos=="left", min-1, max+1), Algorithm,
        key=Algorithm,
        hjust=ifelse(pos=="left", 1, 0),
        label=sprintf("%.3f +/- %.3f", mean, sd)),
        showSelected=c(Algorithm="FPR.percent", "Algorithm"),
        data=some.roc.approx.mid)+
      guides(color="none")+
      theme_bw()+
      theme(panel.margin=grid::unit(0, "lines"))+
      theme_animint(width=1200, height=300)+
      facet_grid(metric ~ .),
    duration=list(),
    first=list(
      major.class=0,
      glmnet=4,
      xgboost=24),
    selectize=list())
  ## BUG: FPR.percent for major.class has selector menu values
  ## {0,1,...,100} but should just be {0,100}.
  for(selector.name in unique(some.roc.approx$algorithm)){
    viz$duration[[selector.name]] <- 1000
    viz$selectize[[selector.name]] <- TRUE
  }
  animint2dir(viz, paste0("viz-roc-", species.dash))
  ##animint2gist(viz)


  species.earth <- earth.dt[species==species.name]
  rank.earth <- species.earth[, list(
    folds.zero=sum(prop.zero==1),
    folds.used=sum(prop.zero!=1),
    mean.nonzero=mean(prop.zero[prop.zero!=1])
  ), by=list(feature)][order(folds.zero, -mean.nonzero)]
  join.earth <- rank.earth[species.earth, on=list(feature)]
  join.earth[, Feature := factor(feature, rank.earth$feature)]
  join.earth[, Used := factor(folds.used, rev(sort(unique(folds.used))))]
  ggplot()+
    ggtitle("Variable importance in earth model")+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(Used ~ ., scales="free", space="free")+
    geom_point(aes(
      1-prop.zero, Feature),
      shape=21,
      data=join.earth)

  glmnet.species <- glmnet.dt[species==species.name]
  zero.species <- zero.counts[species==species.name]
  ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(. ~ weight.name)+
    geom_text(aes(
      Inf, feature, label=paste0("zeros=", round(percent.zero), "%")),
      hjust=1,
      data=zero.species[percent.zero!=0])+
    geom_point(aes(
      norm.weight, feature),
      shape=21,
      fill=NA,
      data=glmnet.species[weight!=0])

  feature.ranks <- zero.species[
    weight.name=="balanced"][order(abs(mean.norm.weight))]
  glmnet.species[, Feature := factor(feature, feature.ranks$feature)]
  feature.ranks.folds <- glmnet.species[feature.ranks, on=list(
    species, weight.name, feature)]
  feature.ranks.folds[, Percent.nonzero := factor(
    percent.nonzero, sort(unique(percent.nonzero), decreasing=TRUE))]
  ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(Percent.nonzero ~ ., scales="free", space="free")+
    geom_point(aes(
      norm.weight, Feature),
      shape=21,
      fill=NA,
      data=feature.ranks.folds)

  feature.ranks.tall <- melt(
    feature.ranks.folds, measure.vars=c("norm.weight", "weight"))
  ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(Percent.nonzero ~ variable, scales="free", space="free_y")+
    geom_point(aes(
      value, Feature),
      shape=21,
      fill=NA,
      data=feature.ranks.tall)

  glmnet.species[, value := ifelse(weight==0, "zero", "non-zero")]
  ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(. ~ weight.name)+
    scale_fill_manual(values=c(zero="white", "non-zero"="black"))+
    geom_point(aes(
      norm.weight, feature, fill=value),
      shape=21,
      data=glmnet.species)

  weight.colors <- c(
    balanced="black",
    one="grey")
  weight.linetypes <- c(
    balanced="solid",
    one="dashed")
  algo.colors <- c(
    earth="#66C2A5",
    glmnet="#FC8D62",
    major.class="#8DA0CB",
    xgboost="#E78AC3")
  ##dput(RColorBrewer::brewer.pal(4, "Set2"))
  rocPoint <- function(wname){
    geom_point(aes(
      FPR, TPR, fill=algorithm),
      color=weight.colors[[wname]],
      shape=21,
      showSelected="weight.name",
      data=species.tall[weight.name==wname])
  }
  label.tab <- table(all.y.list[[species.name]])
  label.dt <- data.table(
    label=names(label.tab),
    count=as.integer(label.tab),
    FPR=c(0,1),
    hjust=c(0,1),
    TPR=c(1,0))
  algo.breaks <- rev(levels(species.tall$Algorithm))
  scale_ <- function(a){
    fun <- get(paste0("scale_", a, "_manual"))
    fun(
      values=algo.colors,
      breaks=algo.breaks,
      guide=guide_legend(order=1))
  }
  viz <- animint(
    title=species.name,
    accuracy=ggplot()+
      ggtitle(paste(
        species.name))+
      theme_bw()+
      theme(panel.margin=grid::unit(0, "lines"))+
      theme_animint(height=300)+
      facet_grid(species ~ variable, scales="free")+
      scale_color_manual(values=weight.colors)+
      scale_fill_manual(
        values=algo.colors,
        breaks=algo.breaks)+
      geom_point(aes(
        value, Algorithm, color=weight.name, fill=algorithm),
        shape=21,
        size=4,
        data=species.tall)+
      xlab(""),
    roc=ggplot()+
      ggtitle(paste0(
        "ROC curves, ",
        n.folds, "-fold CV"))+
      geom_text(aes(
        FPR, TPR, hjust=hjust, label=paste0(
          count, " PRES=", label)),
        data=label.dt)+
      theme_bw()+
      theme_animint(height=300, width=300)+
      theme(panel.margin=grid::unit(0, "lines"))+
      scale_("colour")+
      scale_("fill")+
      scale_linetype_manual(
        values=weight.linetypes, guide=guide_legend(order=2))+
      geom_path(aes(
        FPR, TPR, color=algorithm, linetype=weight.name,
        group=paste(algorithm, weight.name, test.fold)),
        size=1,
        data=approx.roc)+
      rocPoint("balanced")+
      rocPoint("one")+
      coord_equal(),
    earth=ggplot()+
      ggtitle("earth model variable selection")+
      theme_bw()+
      theme(
        panel.margin=grid::unit(0, "lines"),
        panel.background=element_rect(
          color=algo.colors[["earth"]]),
        panel.border=element_rect(
          color=algo.colors[["earth"]]),
        panel.margin=grid::unit(0, "lines"))+
      theme_animint(height=450, width=300)+
      facet_grid(Used ~ ., scales="free", space="free")+
      xlab("Proportion of terms using feature")+
      geom_point(aes(
        1-prop.zero, Feature, color=weight.name),
        shape=21,
        fill=NA,
        size=4,
        data=join.earth)+
      scale_color_manual(values=weight.colors)+
      guides(color="none")
  )
  for(wname in c("one", "balanced")){
    feature.ranks <- zero.species[
      weight.name==wname][order(abs(mean.norm.weight))]
    glmnet.species[, Feature := factor(feature, feature.ranks$feature)]
    feature.ranks.folds <- glmnet.species[feature.ranks, on=list(
      species, weight.name, feature)]
    feature.ranks.folds[, show.nonzero := round(percent.nonzero/10)]
    feature.ranks.folds[, Percent.nonzero := factor(
      show.nonzero, sort(unique(show.nonzero), decreasing=TRUE))]
    feature.ranks.folds[, weight.long := paste0(
      "glmnet variable importance, weights=", weight.name)]
    viz[[wname]] <- ggplot()+
      theme_bw()+
      theme(
        panel.background=element_rect(
          color=algo.colors[["glmnet"]]),
        panel.border=element_rect(
          color=algo.colors[["glmnet"]]),
        panel.margin=grid::unit(0, "lines"))+
      theme_animint(height=450, width=300)+
      facet_grid(
        Percent.nonzero ~ weight.long,
        scales="free", space="free")+
      xlab("Normalized linear model coefficient")+
      geom_point(aes(
        norm.weight, Feature, tooltip=paste0(
          feature, " fold=", test.fold,
          "norm.weight=", norm.weight
        )),
        shape=21,
        fill=NA,
        size=4,
        color=weight.colors[[wname]],
        data=feature.ranks.folds)
  }
  ##animint2gist(viz)
  animint2dir(viz, paste0("viz-", species.dash))

}
