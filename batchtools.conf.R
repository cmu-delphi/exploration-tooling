cluster.functions <- makeClusterFunctionsInteractive()
work.dir <- "."
file.dir <- "registry"
packages <- union(packages, c("epipredict", "epiprocess", "scoringutils", "e1071"))
