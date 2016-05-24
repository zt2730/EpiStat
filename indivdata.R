#indivdata(nobs=c(30,30),meansd=c(1.7833,1),cor1=0.5,nd=6,nn=2,seed=123)
indivdata <- function(
	nobs=c(30,30), # T:C
	meansd=c(1.7833,1), # mean_sd
	cor1=0.5, #cor
	nd=6, # no.diff
	nn=2000,	# no.noise
	seed=123) {
source("X:/Func/mvrnorm2.R")
	## 噪声
	set.seed(312+seed)
	noise <- matrix(rnorm(nn*(nobs[1]+nobs[2]),mean=0,sd=1),ncol=nn)
## 差异变量 位置
indiv1 <- mvrnorm2(meant=meansd[1],sdt=meansd[2],nvar=nd,nsample=nobs[1],cor1=cor1,seed=20110617)
indiv2 <- mvrnorm2(meant=0,sdt=1,nvar=nd,nsample=nobs[2],cor1=cor1,seed=20110617)
indiv <- rbind(indiv1,indiv2);

	## 散落开差异基因的位置
	set.seed(321+seed)
	diff.pos <- sample(1:nn,nd)
	noise[,diff.pos] <- indiv
##### final data ####
	indiv.x <- noise;
	colnames(indiv.x) <- paste("v",1:ncol(indiv.x),sep=""); 
	colnames(indiv.x)[diff.pos] <- paste("d",diff.pos,sep="");
	indiv.y <- factor(c(rep(1,nobs[1]),rep(0,nobs[2]) ) )
	result <- list(x=indiv.x,y=indiv.y,pos=diff.pos)
	return(result)
}
