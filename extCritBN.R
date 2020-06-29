#to execute this file, type:
#  source("~/Google\ Drive/prog/R/extendedCriticality/extCritBN.R")


###plot


#pdf(width = 5, height = 5, file=paste("~/prog/R/extendedCriticality/_pdf/extCritBN.pdf",sep=""));

library(entropy)

ent <- function(x,b){
	y <- rep(0,length(x))
	for (i in 1:length(x)){
		w <- rep(0,b-1)
		for (j in 1:b-1){
			w[j]=(1-x[i])/(b-1)
		}
		y[i]=entropy(c(x[i],w),unit="log2")		
	}
	y / log2(b) #normalize to [0..1]
}

cx <- function(x,b){
	4*ent(x,b)*(1-ent(x,b))
}

b <- 3
pts <- 50
heteros <- 10 #elements per heterogenous average

xs<-((0:pts)/(pts+1))+0.0000001
i<-ent(xs,b)

x <- xs
n <- length(x)
CxHomo <- cx(x,b)


XHetero <- array(0,c(n))
CxHetero <- array(0,c(n))
for (i in 1:n){
	tmp=rnorm(100, i/n, 0.2)
	for (j in 1:length(tmp)){
		if (tmp[j]<0){
			tmp[j]=0
		}
		if (tmp[j]>1){
			tmp[j]=1
		}
	}
	XHetero[i]=mean(tmp)
	CxHetero[i]=mean(cx(tmp,b))
}


plot(x,CxHomo)
points(XHetero, CxHetero, pch=2)

#dev.off();

