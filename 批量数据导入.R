#提取文件扩展名
testsuffix2 <- function(fn) {#fn文件名
	ncf <- nchar(fn)         #文件名长度
	dotpos <- regexpr("\\.[^\\.]*$",fn) #.的位置
	
	return(substr(fn,dotpos,ncf))    #返回后缀名
}

#载入数据函数等(可以增加参数，使得使用正则表达式来筛选载入文件）
loading2 <- function(filepath,#存储路径（可以是向量，与文件名一一对应）
					 file=NULL,             #文件名向量
					 all=F) {		   #载入存储目录下所有文件
	if(all) file <- list.files(filepath);
	path0 <- paste(filepath,file,sep="")
	for (i in 1:length(path0)) {
		switch(testsuffix2(path0[i]),
			   .rda =load(path0[i]),
			   .r = source(path0[i])
			   #此处可根据其他情况扩展
		)
	}
}