## ��һ���Ľ�����֤LOOCV�ĳ���
# Ԥ����Ϊvote�����ʵķ�ʽ���������AUC
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
		# ��һ���Ľ��
		auc <- AUCofROC( varScore=pred.loo, varClass=Y )
	result <- list(pred=pred.loo, auc=auc)
	return(result)
}

# loo.cv <- loocv(X=X, Y=Y, seed=20101122)