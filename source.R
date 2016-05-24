#path.code <- "D:/快盘/Func/R/"
#source(paste(path.code,"garf1.1.R",sep="")) ## GARF
source(paste(path.code,"hist2.R",sep="")) #hist
source(paste(path.code,"kfoldCVsel2.0.R",sep="")) #kfoldCV
source(paste(path.code,"kfoldCV.R",sep=""))
source(paste(path.code,"plsda.R",sep="")) #PLS-DA
source(paste(path.code,"y2dummy.R",sep="")) # dummy
source(paste(path.code,"univariate.R",sep="")) #univariate
source(paste(path.code,"mvrnorm2.R",sep="")) #mvrnorm
source(paste(path.code,"mvrnorm3.R",sep=""))
source(paste(path.code,"jointdata.R",sep="")) # 二分类联合
source(paste(path.code,"interndata.R",sep="")) # 连续交互
source(paste(path.code,"inter2.R",sep="")) #一阶交互

library(pROC);library(randomForest);
library(RODBC);library(plsdepot)
library(scatterplot3d)

#source("http://bioconductor.org/biocLite.R")
#biocLite("multtest")

#####beeswarm
#source("D:/快盘/Func/R/beeswarm/R/beeswarm.R")
#source("D:/快盘/Func/R/beeswarm/R/bxplot.R")
#source("D:/快盘/Func/R/beeswarm/R/zzz.R")

