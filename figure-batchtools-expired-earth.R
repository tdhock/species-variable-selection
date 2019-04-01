library(batchtools)
library(WeightedROC)
library(glmnet)
library(earth)
library(animint2)

## Here are a couple of data tables. The response is column 5,
## "PRES". Columns 6-36 are the predictors. You can probably ignore
## columns 1-4. In case you're curious, species 318 is Sugar Maple and
## species 123 is Table Mountain Pine, species with different life
## histories and distributions. Thanks for taking a look at these. As
## I mentioned yesterday, I think there's a relatively quick paper to
## get out of this and it would be great to work on it with you. Colin
## Quinn, one of Scott's Ph.D. students will also be helping out.

reg.dir <- "registry-expired"
reg <- loadRegistry(reg.dir)

spp.csv.vec <- normalizePath(Sys.glob("data/*"))
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
    roc.dt.list[[paste(test.set.i, algo.i)]] <- data.table(
      algo.meta, roc.df)
    auc.dt.list[[paste(test.set.i, algo.i)]] <- data.table(
      algo.meta,
      FPR=sum(pred.class.vec==1 & test.y.vec==0)/sum(test.y.vec==0),
      TPR=sum(pred.class.vec==1 & test.y.vec==1)/sum(test.y.vec==1),
      ## ADD EVAL METRIC HERE.
      accuracy.percent=mean(pred.class.vec==test.y.vec)*100,
      auc=WeightedAUC(roc.df))
  }
}
auc.dt <- do.call(rbind, auc.dt.list)
roc.dt <- do.call(rbind, roc.dt.list)
glmnet.dt <- do.call(rbind, glmnet.dt.list)
earth.dt <- do.call(rbind, earth.dt.list)
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
  animint2gist(viz)
  ##print(viz)

}
