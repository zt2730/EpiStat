# install.packages("randomForest")
install.packages("randomForest")
install.packages("e1071")
install.packages("RODBC")
install.packages("plspm")
install.packages("pROC")
install.packages("scatterplot3d")
install.packages("R.oo")
install.packages("fdrtool")
install.packages("plsdepot")
#install.packages(pkgs, lib, repos =getOption("repos"), #"http://cran.csdb.cn"
#	contriburl = contrib.url(repos, type),
#	method, available = NULL, destdir = NULL,
#	installWithVers = FALSE, dependencies = NA,
#	type = getOption("pkgType"),
#	configure.args = character(0),
#	clean = FALSE)
source("http://bioconductor.org/biocLite.R")
biocLite("xcms")
#browseVignettes("xcms")
#source("http://bioconductor.org/biocLite.R")
biocLite("CAMERA")
install.packages("beenswarm")