###########################################################
#数据产生
# interX <- matrix(rnorm(100*10,0,1),nrow=100)
# y <- c(rep(1,50),rep(0,50) )
# logit完全分割无法收敛
#函数定义，interX=变量(矩阵），y=类别（矩阵）
###########################################################
inter2 <- function(interX=x,interY=y){
	meanX <- apply(interX,2,mean)
	for (i in 1:nrow(interX)) {
		interX[i,] <- interX[i,] - meanX
	}
	result <- matrix(NA,ncol(interX)*(ncol(interX)-1)/2,6)
	flag=1
	for(i in 1:(ncol(interX)-1) ) {
		for (j in (i+1):ncol(interX) ) {
			result[flag,1]=i; result[flag,2]=j
			glm.logit=summary(glm(interY~interX[,i]*interX[,j],family=binomial(link=logit)))
			p=glm.logit$coefficient[4,]
			result[flag,3:6]=as.vector(round(p,4))
			flag=flag+1 # flag
		}
	}
	colnames(result)=c("i","j","beta","stderr","t","p")
	#result <- result[sort(result[,6],index=T)$ix,]
	result[,1:2] <- colnames(interX)[result[,1:2]]
	return(result)
}
# r=inter2(interX=interX,interY=y)
#函数运行
lm.inter2 <- function(interX=x,interY=y){
	meanX <- apply(interX,2,mean)
	for (i in 1:nrow(interX)) {
		interX[i,] <- interX[i,] - meanX
	}
	result <- matrix(NA,ncol(interX)*(ncol(interX)-1)/2,6)
	flag=1
	for(i in 1:(ncol(interX)-1) ) {
		for (j in (i+1):ncol(interX) ) {
			result[flag,1]=i; result[flag,2]=j
			lm.fit=summary(lm(as.numeric(interY)~interX[,i]*interX[,j]))
			p=lm.fit$coefficient[4,]
			result[flag,3:6]=as.vector(round(p,4))
			flag=flag+1 # flag
		}
	}
	colnames(result)=c("i","j","beta","stderr","t","p")
	#result <- result[sort(result[,6],index=T)$ix,]
	result[,1:2] <- colnames(interX)[result[,1:2]]
	return(result)
}
