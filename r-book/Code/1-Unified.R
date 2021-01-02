###################################################################################################
################
################
################
################
################
################
###################################################################################################

base_url <- '~/GitHub/kul-multivariate-a2/src/'
load(paste0(base_url,"dwvs.Rdata"))

library(equatiomatic)
library(candisc)
library(ggplot2)
library(ggbiplot)
library(car)
library(HDclassif)
library(dplyr)
library(lemon)
library(Rcmdr)
library(MASS)
library(class)
library(heplots) #covEllipses, cqplot
library(kableExtra)
library(car) #Manova
library(VGAM) # for vglm function
library(nnet) # for multinom function


lm.out<-lm(cbind(F_rights, F_steal, F_crime,F_religion,F_realizeself,F_dogood,
                 F_violence)~as.factor(country), data=dwvs)
candisc.out<-candisc(lm.out)
print(candisc.out)


res_t1_2 <- summary(Manova(lm.out), test="Wilks")
summary.default(Manova(lm.out), test="Wilks")

boxM(lm.out)


### Plot
plot(candisc.out,col=c("red","green","black"),pch=c(1,2,3),cex=1.2)
par(xpd=TRUE)
legend(-9.6,-6,c("Netherlands", "Nigerians", "Philippines"), pch = c(1,2,3), lty = c(1,2), col=c("red","green","black"))
lda.out1<-lda(country ~ F_rights + F_steal + F_crime + F_religion + F_realizeself +
                F_dogood + F_violence, data=dwvs)
#print(lda.out1)
pred.train1 <- predict(lda.out1,dwvs, prior=c(1,1,1)/3)
tab1 <- table(dwvs$country,pred.train1$class)
#print(tab1)
kbl(tab1)
kbl(sum(diag(tab1))/sum(tab1))

#classify test observations using LDA
pred.loocv2<-lda(country~F_rights+F_steal+F_crime+F_religion+F_realizeself+F_dogood+
                   F_violence,data=dwvs, prior=c(1,1,1)/3, CV=TRUE)
tab2<-table(dwvs$country,pred.loocv2$class)
print(tab2)

#LOOCV hit rate
kbl(sum(diag(tab2))/sum(tab2))

### Quadratic discriminant analysis
qda.out3<-qda(country ~ F_rights + F_steal + F_crime + F_religion + F_realizeself + 
                F_dogood + F_violence, data = dwvs) 

pred.train3<-predict(qda.out3,dwvs, prior=c(1,1,1)/3)

tab3<-table(dwvs$country,pred.train3$class)
kbl(tab3)
kbl(sum(diag(tab3))/sum(tab3))

#classify test observations using QDA
pred.test4 <- qda(country ~ F_rights + F_steal + F_crime + F_religion + F_realizeself + 
                    F_dogood + F_violence, data=dwvs,prior=c(1,1,1)/3,CV=TRUE)
tab4<-table(dwvs$country,pred.test4$class)
kbl(tab4)
kbl(sum(diag(tab4))/sum(tab4))
### K-nearest Neighbors
str(dwvs)
table(dwvs$country)
set.seed(9850)   # -> random number generator
gp<-runif(nrow(dwvs))
dwvs2<-dwvs[order(gp),]

hitratknn<-function(observed,predicted){
  tab<-table(observed,predicted)
  hitratknn<-sum(diag(tab))/sum(tab)
  return(hitratknn)
}

knnmax<-100
err<-matrix(rep(0,knnmax*2), nrow=knnmax)

for(j in 1:knnmax) {
  predknn.train<-knn(dwvs2[,2:8], dwvs2[,2:8], dwvs2$country, k=j)
  err[j,1]<-hitratknn(dwvs2$country,predknn.train)
}

for(j in 1:knnmax) {
  predknn.train<-knn.cv(dwvs2[,2:8], dwvs2$country, k=j)
  err [j,2]<-hitratknn(dwvs2$country,predknn.train)
}

plot('K', 'Hit rate',xlim=c(1,knnmax),ylim=c(0.8,1))
lines(c(1:knnmax),err[,1],col="red") # -> training error
lines(c(1:knnmax),err[,2],col="blue")


### High Dimensional Discriminant Analysis
w <- dwvs[,-1]
cls <- dwvs[,1]

#HDDA on the learning dataset:
hdda.out7 <- hdda(w, cls, scaling=TRUE, model="all", d="BIC",graph=TRUE,show=TRUE) 
plot(hdda.out7)
plot(hdda.out7,method="BIC")

pred.train7<-predict(hdda.out7,w,cls)

#print(tab7)
tab7<-table(dwvs$country,pred.train7$class)
#training hit rate
kbl(sum(diag(tab7))/sum(tab7))

pred.loocv8 <- hdda(w, cls,scaling=TRUE, d="BIC", LOO=TRUE)
tab8<-table(cls,pred.loocv8$class)

#print(tab8)
kbl(tab8)

#LOOCV hit rate
kbl(sum(diag(tab8))/sum(tab8))

### Error comparison for the different models

### Multinomial logistic regression model

m1<- multinom(country~F_rights+F_steal+F_crime+F_religion+F_realizeself+F_dogood +
                F_violence, family=multinomial, data=dwvs, maxit=3926, hess=TRUE)
t1_15_result <- summary (m1)
t1_15_result

train.pred<-predict(m1,newdata=dwvs)
tab<-table(dwvs$country, train.pred)
kbl(sum(diag(tab))/sum(tab))

#Error rate: 0.1428935

########compute LOOCV
nobs<- 3926
hit<-rep(0,nobs)
for (i in 1:nobs){
  train<-c(1:nobs)
  mod<- multinom(country ~ F_rights + F_steal + F_crime + F_religion + 
                   F_realizeself + F_dogood + F_violence, data=dwvs, 
                 subset=train[-i], print= FALSE,maxit=3926)
  pred<- predict(mod, newdata=dwvs[i,])
  hit[i]<-ifelse(pred==dwvs$country[i],1,0)
}


#hitrate
mean(hit)  #### LOOCV

#Error rate: 0.1444218

###################################################################################################
################
################
################
################
################
################
###################################################################################################
