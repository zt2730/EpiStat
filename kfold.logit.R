############################################################
## 5-fold CV for Logit model
## 返回结果是5层预测结果的prob，
kfold.logit <- function(Y=b1$cancer1,X=b1,kfold=5) {
	X <- as.matrix(X)
	set.seed(123)
	kfold10 <- sample(rep(1:kfold,ceiling(nrow(X)/kfold))[1:nrow(X)])
	prob <- matrix(NA,nrow(X),1)
	for ( i in 1:kfold) {
		trX <- as.data.frame(X[kfold10 !=i,],)
		dataX <- cbind(Y[kfold10 !=i],trX)
		colnames(dataX) <- c("Y",paste("x",1:ncol(trX),sep=""))
		tr <- glm(Y~.,data=dataX,family=binomial(logit))
		teX <- as.data.frame(X[kfold10 ==i,])
		colnames(teX) <- c(paste("x",1:ncol(X),sep=""))
		prob[kfold10 ==i] <- predict(tr,teX,type="response")
	}
	return(prob)
}	
#flag=c(2,6,23,24,26)
prob1 <- kfold.logit(Y=b1$cancer1,X=b1[,c(23,24,26)],kfold=5)
auc(b1$cancer1,prob1)