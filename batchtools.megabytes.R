library(batchtools)
library(data.table)
reg.dir <- "../species-variable-selection-old/registry180"
reg <- loadRegistry(reg.dir)
results.glob <- file.path(reg.dir, "results", "*")
du.cmd <- paste("du -ms", results.glob)
results.dt <- fread(cmd=du.cmd, col.names=c('megabytes', 'job.rds'))
results.dt[, job.id := as.integer(sub(".*/results/", "", sub(".rds", "", job.rds)))]
join.dt <- getJobTable()[results.dt, on=list(job.id)]
join.dt[, list(
  min=min(megabytes),
  max=max(megabytes)
), by=list(algorithm)]

fit <- loadResult(119)
print(object.size(fit$fit), units="Mb")


