#############################################
## 产生联合作用的两分类差异变量
# jointdata(nobs=c(30,30),nvar=2000,nc=2,seed=123)
# 使用m表示差异交互变量
#############################################
jointdata <- function(
	nobs=c(30,30),
	nvar=2000,
	nc=2,	# cluster堆
	seed=123)
{
	no.obt <- nobs[1]
	no.obc <- nobs[2]
	no.noise <- nvar
	no.joint <- 3 	## 联合作用的变量个数
	probll <- 0.1	## 抽样概率，即某种模式下90%可能为有病
	probb1 <- c((1-probll )/3,(1-probll )/3,(1-probll )/3,probll )
	probb2 <- c(probll ,(1-probll )/3,(1-probll )/3,(1-probll )/3)
	
	## 数据产生 ##
	set.seed(seed)
	## 1 2 4 8 16 十进制位数
	bin <- 2^((no.joint-1):0) 
	# B为组合数据 二进制矩阵
	B <- matrix(0,2^no.joint,no.joint)
	# A为十进制
	A <- 0:(2^no.joint-1)
	## 十进制转化为二进制
	for (i in 1:2^no.joint) {
		flag <- A[i]
		for (j in 1:no.joint) {	
			if (bin[j] > flag) B[i,j] <- 0	
			if (bin[j] <= flag) {
				B[i,j] <- 1
				flag <- flag-bin[j]
			}
		}
	}
	
	## 规定阳性条件：两个表达即为有病
	sort(apply(B,1,sum),index=T) -> temp 
	B <- B[temp$ix,] 
	# ff <- (temp$x <=no.joint) & (temp$x >0) # 剔除全为0和1 # 包括全是1的情况 2010-08-03
	# B <- B[ff,]
	sum0 <- sum( (temp$x>=0)&(temp$x < 2) )	# 计算阴性的个数，一个1的情况

	## 在这中间抽取观测为联合分类的数据
diffx <- NULL
for (i in 1:nc) {
	# 第i堆联合变量
	set.seed(1234*i)
	x1 <- B[sample((sum0+1):nrow(B),no.obt,replace=T,prob=probb1),] # 阳性
	x2 <- B[sample(1:sum0,no.obc,replace=T,prob=probb2),]
	x12 <- rbind(x1,x2)
	diffx <- cbind(diffx,x12)
}
	## 噪声
	set.seed(312+seed)
	noise <- matrix(rnorm(no.noise*(no.obt+no.obc),mean=0,sd=1),ncol=no.noise)

	## 散落开差异基因的位置
	set.seed(321+seed)
	diff.pos <- sample(1:no.noise,no.joint*nc)
	noise[,diff.pos] <- diffx
##### final data ####
	joint.x <- noise;
	colnames(joint.x) <- paste("v",1:ncol(joint.x),sep=""); 
	colnames(joint.x)[diff.pos] <- paste("m",diff.pos,sep="");
	joint.y <- factor(c( rep(1,no.obt),rep(0,no.obc) ) )
	
	return(list(x=joint.x,y=joint.y,pos=diff.pos))
}
## END ##
