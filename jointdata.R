#############################################
## �����������õ�������������
# jointdata(nobs=c(30,30),nvar=2000,nc=2,seed=123)
# ʹ��m��ʾ���콻������
#############################################
jointdata <- function(
	nobs=c(30,30),
	nvar=2000,
	nc=2,	# cluster��
	seed=123)
{
	no.obt <- nobs[1]
	no.obc <- nobs[2]
	no.noise <- nvar
	no.joint <- 3 	## �������õı�������
	probll <- 0.1	## �������ʣ���ĳ��ģʽ��90%����Ϊ�в�
	probb1 <- c((1-probll )/3,(1-probll )/3,(1-probll )/3,probll )
	probb2 <- c(probll ,(1-probll )/3,(1-probll )/3,(1-probll )/3)
	
	## ���ݲ��� ##
	set.seed(seed)
	## 1 2 4 8 16 ʮ����λ��
	bin <- 2^((no.joint-1):0) 
	# BΪ������� �����ƾ���
	B <- matrix(0,2^no.joint,no.joint)
	# AΪʮ����
	A <- 0:(2^no.joint-1)
	## ʮ����ת��Ϊ������
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
	
	## �涨�����������������ＴΪ�в�
	sort(apply(B,1,sum),index=T) -> temp 
	B <- B[temp$ix,] 
	# ff <- (temp$x <=no.joint) & (temp$x >0) # �޳�ȫΪ0��1 # ����ȫ��1����� 2010-08-03
	# B <- B[ff,]
	sum0 <- sum( (temp$x>=0)&(temp$x < 2) )	# �������Եĸ�����һ��1�����

	## �����м��ȡ�۲�Ϊ���Ϸ��������
diffx <- NULL
for (i in 1:nc) {
	# ��i�����ϱ���
	set.seed(1234*i)
	x1 <- B[sample((sum0+1):nrow(B),no.obt,replace=T,prob=probb1),] # ����
	x2 <- B[sample(1:sum0,no.obc,replace=T,prob=probb2),]
	x12 <- rbind(x1,x2)
	diffx <- cbind(diffx,x12)
}
	## ����
	set.seed(312+seed)
	noise <- matrix(rnorm(no.noise*(no.obt+no.obc),mean=0,sd=1),ncol=no.noise)

	## ɢ�俪��������λ��
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