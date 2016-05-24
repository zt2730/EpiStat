## GALGO ##############
## galgo.garf <- function(modelX=modelX,modelY=modelY,goalFit=0.85){
library(galgo)
library(randomForest)
randomforest.R.predict <- function(chr, parent, tr, te, result) {
	d <- parent$data
	xrf <- randomForest(x=d$data[tr,chr], y=d$classes[tr],
	xtest=d$data[te,chr], ytest=d$classes[te],ntree=100)
#if all(te==tr) resubstitution was specified, which is faster
#considering that RF performs an internal cross-validation (out-of-bag)
	if (result) {
		if (all(te==tr)) sum(xrf$predicted==d$classes[te])/length(te)
		else sum(xrf$test$predicted==d$classes[te])/length(te)
	} else {
		if (all(te==tr)) xrf$predicted==d$classes[te]
		else xrf$test$predicted
	}
}
## galgo-RF
pos12.garf <- configBB.VarSel(
	data=t(trainX),
	classes=trainY,
	main="ovarian",
	train=1, #指定第一层训练集和测试集的样本比例
	test=0,
	classification.method="user",
	classification.userFitnessFunc=randomforest.R.predict,
	# classification.test.error=c(0,1),
	classification.train.error="resubstitution",
	# classification.train.Ksets=5,
	#scale=TRUE,
	 
	chromosomeSize=5,
	populationSize=20,
	niches=4,
	immigration=c(rep(0,18),0.5,1),
	crossoverPoints=round(runif(1,1,8)),#指定交叉位置
	#offspringScaleFactor=1,
	#offspringMeanFactor=0.85,
	#offspringPowerFactor=2,
	elitism=c(rep(1,7),0.5,0.5,0.5),
	#crossPoints=round(chromosomeSize/2,0),
	#mutation：默认每个染色体突变一次
	#crossover：默认0.5
	#performs 0.5n crossovers where n is the population size of the Niche
	 
	goalFitness = 0.65,
	maxGeneration=100,
	minGenerations=10,
	maxSolutions=5,
	onlySolutions=FALSE,

	saveFile="pos12_535.garf0408.Rdata",
	saveFrequency=50,
	saveVariable="garf0408"
)