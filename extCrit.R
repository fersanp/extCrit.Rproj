#to execute this file, type:
#  source("~/Google\ Drive/prog/R/extendedCriticality/extCrit.R")


###plot


#pdf(width = 5, height = 5, file=paste("~/prog/R/extendedCriticality/_pdf/extCrit.pdf",sep=""));

library(entropy)

ent <- function(x){
	y <- rep(0,length(x))
	for (i in 1:length(x)){
		y[i]=entropy(c(x[i],1-x[i]),unit="log2")		
	}
	y
}

cx <- function(x){
	4*ent(x)*(1-ent(x))
}


pts <- 50
heteros <- 10 #elements per heterogenous average

xs<-((0:pts)/(pts+1))+0.0000001
i<-ent(xs)

x <- xs
n <- length(x)
CxHomo <- cx(x)


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
	CxHetero[i]=mean(cx(tmp))
}

XHetero2 <- array(0,c(n))
CxHetero2 <- array(0,c(n))
for (i in 1:n){
	tmp=i/(n+6) + 1:100/1000
	XHetero2[i]=mean(tmp)
	CxHetero2[i]=mean(cx(tmp))
}


plot(x,CxHomo)
points(XHetero, CxHetero, pch=2)
points(XHetero2, CxHetero2, pch=3)

#dev.off();

