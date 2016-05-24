##前进法的变量确定
topauc <- function(trainX=NULL,trainY=NULL,testX=NULL,testY=NULL,
	markers=NULL,seqs=c(2,5,10,15,20,25,30,35,40,45,50)) {
n <- length(seqs)
ikcv <- numeric(n)
ev <- numeric(n)
for (i in 1:n) {
#source("X:/Func/kfoldCV.R")
ikcv[i] <- kfoldCV(X=trainX[,markers[1:seqs[i]]],Y=trainY,kfold=5,model="rf")$auc
if (!is.null(testX)){
#library(randomForest);library(pROC)
rf1 <- randomForest(trainX[,markers[1:seqs[i]]],factor(trainY),ntree=2000)
pred1 <- predict(rf1,testX,type="vote")
#cbind(pred1,testY)
ev[i] <- auc(ifelse(testY==1,1,0),pred1[,1])
}
}
result <- list(ikcv,ev)
return(result)
}