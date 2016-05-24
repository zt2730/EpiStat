#####################################################################
## k-fold CV|loocv without variable selection
## time 2011-04-18, by tao zhang
## parameter ##
	# kfold10 代表分布在哪一层
## values ##
	#	prob
	#	auc	CV的AUC值
# 当X只有一列的时候，容易报错,as.matrix()
#####################################################################
kfoldCV <- function(X=modelX,
		Y=modelY,
		kfold=5,	# 交叉验证层数，nrow(X)表示LOOCV留一法
		model="rf",	# 建模方法，RF和SVM两种
		seed=20110101,	# RF建模的种子数
		verbose=1 #是否显示
	) {
## package
	library(pROC)
	X <- as.data.frame(X)
	set.seed(seed)
	kfold10 <- sample(rep(1:kfold,ceiling(nrow(X)/kfold))[1:nrow(X)])
	prob <- matrix(NA,nrow(X),1)
	for ( i in 1:kfold) {
		if (verbose==1){
			if (kfold==nrow(X)) cat(paste("loocv",i,".\n")) ##
			else cat(paste("fold",i,".\n")) ##
		}
		## 总建模
		if (model=="rf"){
			library(randomForest)
			set.seed(seed+1000)
			rfvim <- randomForest(as.matrix(X[kfold10 !=i,]),
				Y[kfold10 !=i],importance=T,ntree=2000)
			prob[kfold10 ==i,1] <- predict(rfvim,as.matrix(X[kfold10 ==i,]),
				type="vote")[,2]
		}
		if (model=="svm") {
			library(e1071)
			svmmodel <- svm( as.matrix(X[kfold10 !=i,]),
				Y[kfold10 !=i],probability = TRUE)
			svmpred <- predict(svmmodel,as.matrix(X[kfold10 ==i,]),probability = TRUE)
			prob[kfold10 ==i,1] <- attr(svmpred , "probabilities")[,1]
		}
	}
	auc <- auc(Y,prob)
	result <- list(prob=prob,auc=auc,seed=seed)
	return(result)
}
# kfoldCV(X=modelX,Y=modelY,kfold=5,model="rf")

