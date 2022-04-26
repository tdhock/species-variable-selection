library(data.table)
library(ggplot2)
roc.dt <- data.table::fread("figure-batchtools-expired-earth-roc.csv")
auc.dt <- data.table::fread("figure-batchtools-expired-earth-auc.csv")

ggplot()+
  geom_path(aes(
    FPR, TPR, color=algorithm),
    data=roc.dt)+
  geom_point(aes(
    FPR, TPR, color=algorithm),
    shape=1,
    data=auc.dt)+
  facet_grid(species + weight.name ~ test.fold, labeller=label_both)

some <- function(dt)dt[weight.name=="balanced" & species=="Sugar Maple" & test.fold == 1][, Algorithm := ifelse(algorithm=="major.class", "constant", algorithm)][]
roc.some <- some(roc.dt)
auc.some <- some(auc.dt)[order(auc)]
auc.some[, y := 0.5 + .I*0.05]
other.xgboost <- roc.some[
  auc.some[Algorithm=="glmnet", .(auc,FPR, Algorithm="xgboost")],
  on=.NATURAL]
auc.some.more <- rbind(
  roc.some[auc.some, on=.NATURAL][, names(other.xgboost), with=FALSE],
  other.xgboost)
RColorBrewer::brewer.pal(3, "Accent")
gg <- ggplot()+
  scale_color_manual(values=c(
    xgboost="#7FC97F",
    glmnet="#BEAED4",
    constant="#FDC086"))+
  ggtitle("ROC curves, D=Default prediction threshold")+
  directlabels::geom_dl(aes(
    FPR, TPR, color=Algorithm, label=sprintf(
      "%d/%d=%.2f FPR\n%d/%d=%.2f FNR\n%d/%d=%.2f errors",
      FP,test.labels.negative,FPR,
      FN,test.labels.positive,1-TPR,
      FP+FN,test.labels,(FP+FN)/test.labels)),
    method=list(cex=0.75, directlabels::polygon.method("right", offset.cm=2)),
    data=auc.some.more)+
  geom_path(aes(
    FPR, TPR, group=paste(algorithm, test.fold)),
    size=2,
    color="white",
    data=roc.some)+
  geom_path(aes(
    FPR, TPR, color=Algorithm, group=paste(algorithm, test.fold)),
    size=1,
    data=roc.some)+
  geom_point(aes(
    FPR, TPR, color=Algorithm),
    size=4,
    data=auc.some.more)+
  geom_text(aes(
    FPR, TPR, label="D"),
    data=auc.some)+
  theme(legend.position="none")+
  geom_label(aes(
    0.35, y, color=Algorithm, label=sprintf(
      "%s AUC=%.2f", Algorithm, auc)),
    size=3,
    data=auc.some)+
  coord_equal()
png("figure-batchtools-expired-earth-roc.png", width=5, height=5, units="in", res=200)
print(gg)
dev.off()

