## 连续数据交互变量
interndata <- function(nobs=c(30,30),nn=100,seed=123) { # begin
## 噪声
	set.seed(seed)
	noise <- matrix(rnorm(nn*(nobs[1]+nobs[2]),mean=0,sd=1),ncol=nn)
## 差异变量 位置
#source("X:/Func/mvrnorm3.R")
## z3,z5
#indiv <- rbind(mvrnorm2(meant=1.7833,sdt=1,nvar=2,nsample=nobs[1],cor1=0.2,seed=seed+1),
#	mvrnorm2(meant=0,sdt=1,nvar=2,nsample=nobs[2],cor1=0.2,seed=seed+22))
#x4 <- indiv[,1];
#x5 <- indiv[,2]
#第一堆
set.seed(seed+133) ## 相关性
z123 <- rbind(mvrnorm3(meant=c(1,5,5),sdt=c(2,1,1),nsample=nobs[1],cor1=0.6,seed=seed+112),
	mvrnorm3(meant=c(0,0,0),sdt=c(1,1,1),nsample=nobs[2],cor1=0.6,seed=seed+113))
#z1 <- exp(z123[,1]);## 考虑将z1设定为指数正态分布
z1 <- z123[,1]
z2 <- z123[,2]; z3 <- z123[,3]
#x2 <- c(rnorm(nobs[1], mean =1,sd =1),rnorm(nobs[2], mean =0,sd =1))
## y=a+b1*x1+b2*x2+b3*x3 +r1*x1*x2 + r2*x3^2
x1 <- z1; x2 <- z2/z1; x3 <- z3/z2;

#第2堆
set.seed(seed+1231) ## 相关性
z456 <- rbind(mvrnorm3(meant=c(1,5,5),sdt=c(2,1,1),nsample=nobs[1],cor1=0.6,seed=seed+1121),
	mvrnorm3(meant=c(0,0,0),sdt=c(1,1,1),nsample=nobs[2],cor1=0.6,seed=seed+1131))
#z1 <- exp(z123[,1]);## 考虑将z1设定为指数正态分布
z4 <- z456[,1]
z5 <- z456[,2]; z6 <- z456[,3]
#x2 <- c(rnorm(nobs[1], mean =1,sd =1),rnorm(nobs[2], mean =0,sd =1))
## y=a+b1*x1+b2*x2+b3*x3 +r1*x1*x2 + r2*x3^2
x4 <- z4; x5 <- z5/z4; x6 <- z6/z5;
#####
z <- cbind(z1,z2,z3,z4,z5,z6)
x <- cbind(x1,x2,x4,x5)
y <- factor(c( rep(1,nobs[1]),rep(0,nobs[2]) ) )
## 散落开差异基因的位置
	set.seed(321+seed)
	pos <- sample(1:nn,4)
	noise[,pos] <- x
##### final data ####
	x <- noise;
	colnames(x) <- paste("v",1:ncol(x),sep=""); 
	colnames(x)[pos] <- paste("m",pos,sep="");

return(list(x=x,y=y,z=z,pos=pos))
} #end#
#intern1 <- intern(nobs=c(30,30),nn=100,seed=123)

