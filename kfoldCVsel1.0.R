## k-fold CV
## parameter ##
	# kfold10 代表分布在哪一层
## values ##
	#	auc	CV的AUC值
	#	varsel 没层筛选出的变量
kfoldCV <- function(X=modelX.x2000,Y=modelY,kfold=5,selection=0,
		topn=100,seed=20110101,ntree=500) {
	library(randomForest)
	set.seed(seed)
	kfold10 <- sample(rep(1:kfold,ceiling(nrow(X)/kfold))[1:nrow(X)])
	auc.kfold <- matrix(NA,kfold,length(topn))
	varsel <- matrix(NA,topn*2,kfold)
	prob <- matrix(NA,nrow(X),length(topn))
	for ( i in 1:kfold) {
		cat(paste("Fold",i,"...\n"))
	## 筛选和建模
		rfvim <- randomForest( X[kfold10 !=i,],Y[kfold10 !=i],importance=T,ntree=ntree)
		if (selection==1) {
			cat("-")
			vims <- importance(rfvim,type=1,scale=TRUE)
			uAUC <- uAUCofROC(X=X[kfold10 !=i,], Y=Y[kfold10 !=i])
			# flag <- sort(vims,decreasing=T,index=T)$ix[1:100]
			flag <- union(sort(uAUC,decreasing=T,index=T)$ix[1:topn],
			sort(vims,decreasing=T,index=T)$ix[1:topn])
			varsel[1:length(flag),i] <- flag
			if (length(flag)>0) {
				cat(paste("nvar=",length(flag),"---"))
				rfmodel <- randomForest( as.matrix(X[kfold10 !=i,flag]),Y[kfold10 !=i],ntree=ntree)
				rfpred <- predict(rfmodel,as.matrix(X[kfold10 ==i,flag]),type="vote")[,2]
				prob[kfold10 ==i,1] <- rfpred
			} else {
				cat("the intersect length=0.\n")
				auc.kfold[i]=NA
			}
		} 
		if (selection==0) {
			rfpred <- predict(rfvim,X[kfold10 ==i,],type="vote")[,2]
			prob[kfold10 ==i,1] <- rfpred
		}
		auc.kfold[i] <- AUCofROC(rfpred,Y[kfold10 ==i])
		cat(paste("AUC=",round(auc.kfold[i],4),"\n"))
	}
	result <- list(auc=auc.kfold,varsel=varsel,prob=prob,seed=seed)
	return(result)
}
# rfmodel <- svm( as.matrix(X[kfold10 !=i,flag]),Y[kfold10 !=i],probability = TRUE)
# rfpred <- predict(rfmodel,as.matrix(X[kfold10 ==i,flag]),probability = TRUE)
# prob[kfold10 ==i,1] <- attr(rfpred , "probabilities")[,1]

## fold5.auc <- kfoldCV(X=modelX.x2000,Y=modelY,kfold=5,selection=1,seed=20110317)