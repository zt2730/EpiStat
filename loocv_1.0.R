## 留一法的交叉验证LOOCV的程序
# 预测结果为vote，概率的方式，并求得其AUC
loocv <- function(X=X, Y=Y, seed=20101122) {
	library(randomForest)
	set.seed(seed)
		pred.loo <- matrix(0,nrow(X))
		for (loo in 1: nrow(X) )	{
			cat(".")
			xmodel=X[-loo,]                                 
			xloo=X[loo,]
			ymodel=Y[-loo]
			yloo=Y[loo]
		#model
			# svm=svm(xmodel,ymodel,probability = T)
			r.rf <- randomForest(as.matrix(xmodel),ymodel,ntree=500)
			pred.loo[loo]=as.matrix( predict(r.rf,xloo,type="vote")[,1])	
		}
		# 留一法的结果
		auc <- AUCofROC( varScore=pred.loo, varClass=Y )
	result <- list(pred=pred.loo, auc=auc)
	return(result)
}

# loo.cv <- loocv(X=X, Y=Y, seed=20101122)
