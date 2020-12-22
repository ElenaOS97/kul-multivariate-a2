library(smacof)

dimension = 2

sim<-(confusion+t(confusion))/2
dissim<-100-sim
for (i in 1:36){
    dissim[i,i]<-0    
  } 

str(sim)



## Question 1 
#1. Use smacofSym() to conduct MDS with 2 dimensions and assuming different
#measurement levels (i.e., ratio, interval, mspline, ordinal) for the observed
#dissimilarities.

#1.1 ratio
m1 <- smacofSym(delta=dissim, ndim=dimension, type="ratio", init="torgerson")

#1.2 interval 
m2 <- smacofSym(delta=dissim, ndim=dimension, type="interval", init="torgerson")

#1.3 mspline
m3<-smacofSym(delta=dissim, ndim=dimension, type="mspline", spline.degree =4, spline.intKnots = 4, init="torgerson")

#1.4 ordinal
m4 <- smacofSym(delta=dissim, ndim=dimension, type="ordinal", init="torgerson")


dev.new(RStudioGD()) 

round(c(m1$stress,m2$stress,m3$stress,m4$stress),3)

par(mfrow=c(2,2))
plot(m1,plot.type="resplot",main="residual plot spline MDS")
plot(m1,plot.type="Shepard",main="Shepard diagram spline MDS")

plot(m3,plot.type="resplot",main="residual plot ordinal MDS")
plot(m3,plot.type="Shepard",main="Shepard diagram ordinal MDS")
plot(m4,plot.type="resplot",main="residual plot spline MDS")
plot(m4,plot.type="Shepard",main="Shepard diagram spline MDS")
#configuration ordinal MDS
plot(m3,plot.type="conf")



## Question 2
#2. Evaluate the goodness of fit of solutions with different measurement levels using stress-1, and by computing 
#stress norms with the functions randomstress() and permutation(). Discuss which solution you would select. Investigate 
#the stability of the selected solution using the Jackknife.

#stress norm
set.seed(1)
rstress<-randomstress(n=10,ndim=2,nrep=500,type="ordinal")
#distribution of stress for random data
mean(rstress)-2*sd(rstress)
#permutation test
set.seed(1)
perm.car<-permtest(m3,nrep=500)
#plot distribution stress
par(mfrow=c(1,2),pty="s")
hist(rstress,main="stress random data")
hist(perm.car$stressvec,main="stress permuted data")
#stability of solution using jackknife
jack.car<-jackmds(m3)
plot(jack.car,xlim=c(-1.2,1.2),ylim=c(-1,1))



## Question 3
#3. Construct a data set of external variables that describe the signals (e.g. length of the signal, 
#proportion of short beeps in the signal, etc.). Use an MDSbiplot to project the external variables in 
#the configuration plot of the selected solution, and interpret the results of the analysis. 



