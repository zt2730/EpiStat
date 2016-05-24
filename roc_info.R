## 给定单变量x，计算其诊断y的判别AUC的
## threshold, auc(95%CI), SE(95%CI),SP(95%CI)
roc_info <- function(Y=y,X=x) {
	library(pROC)
	rocM1 <- roc(Y,X,na.rm=TRUE,auc=TRUE,
		ci=T,thresholds="best",of="thresholds")
	#summary(rocM1)
	ci <- round(c(ci.auc(rocM1)),3)
	auc_ci <- paste(ci[2]," (",ci[1],", ",ci[3],")",sep="")
	## threshold se sp
	tt <- rocM1$ci
	thr <- round(as.numeric(rownames(tt[[1]])),3)
	sp <- round(tt[[1]],3)
	se <- round(tt[[2]],3)
	se_ci <- paste(se[2]," (",se[1],", ",se[3],")",sep="")
	sp_ci <- paste(sp[2]," (",sp[1],", ",sp[3],")",sep="")

	pp <- c(thr,auc_ci,se_ci,sp_ci)
	return(pp)
}
#auc1 <- roc_info(Y=b1$cancer1,X=b1[,flag[i]])

# todo
flag <- c(6,21:26)
roc_all <- matrix(1:28,nrow=4)
for (i in 1:length(flag)) {
	auc1 <- roc_info(Y=b1$cancer1,X=b1[,flag[i]])
	roc_all[,i] <- as.matrix(auc1)
}
pp <- t(roc_all)
rownames(pp) <- colnames(b1)[c(6,21:26)]
colnames(pp) <- c("threshold","auc(95%CI)","Sensitivity(95%CI)","Specificity(95%CI)")
write.csv(pp,file=paste(rr,"uAUC_info.csv",sep=""))