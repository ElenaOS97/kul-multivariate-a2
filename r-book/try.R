library(tree)
library(randomForest)
library(gbm)
library(pdp)
library("MASS")
library(heplots)
library(candisc)
library(knitr)
#error function
err<-function(observed,predicted){
  tab<-table(observed,predicted)
  print(tab)
  
  err <- 1-sum(diag(tab))/sum(tab)
  
  #return(tab, err)
  
  
  
  newList <- list("table" = tab, "numeric" = err)
  
}

#load data
base_url <- '~/GitHub/kul-multivariate-a2/src/'
load(paste0(base_url,"spamdata.Rdata"))
spamdata$spam<-factor(ifelse(spamdata$spam==1,"1_yes","0_no"))

#create train and test set
set.seed(0829539)
trainsize<-2500
train = sample(nrow(spamdata), trainsize)
data.train<-spamdata[train,]
data.test<-spamdata[-train,]

#grow complex tree using deviance as criterion
tree.mod=tree(spam~.,data.train,control=tree.control(nobs=2500,minsize=2,mincut=1),split="deviance")
summary(tree.mod)

#plot tree
plot(tree.mod)
text(tree.mod,pretty=0,cex=1.4)

#use cross-validation to select tuning parameter for pruning the tree
set.seed(0829539)
cv.out=cv.tree(tree.mod,K=5)
par(cex=1.4)
plot(cv.out$size,cv.out$dev,type='b')

#prune the tree
prune.mod=prune.tree(tree.mod,best=12)
plot(prune.mod)
text(prune.mod,pretty=0)

#make predictions on training and test set using the unpruned tree
pred.train<-predict(tree.mod,newdata=data.train)
classif.train<-ifelse(pred.train[,2]>=pred.train[,1],1,0)
err(data.train$spam,classif.train)
pred.test<-predict(tree.mod,newdata=data.test)
classif.test<-ifelse(1*pred.test[,2]>=pred.test[,1],1,0)
err(data.test$spam,classif.test)

#make predictions on training and test set using the pruned tree
pred.train<-predict(prune.mod,newdata=data.train)
classif.train<-ifelse(pred.train[,2]>=pred.train[,1],1,0)
err(data.train$spam,classif.train)
pred.test<-predict(prune.mod,newdata=data.test)
classif.test<-ifelse(1*pred.test[,2]>=pred.test[,1],1,0)
err(data.test$spam,classif.test)

#(a)Account for different prior probabilities and equal classification costs 
# (1) linear discriminant analysis
set.seed(0829539)
ldaS.out<-lda(spam~.,data=spamdata)
print(ldaS.out)

lda.out<-lda(spam~.,prior=c(0.606,0.394),data=data.train)
print(lda.out)

pred.train<-predict(lda.out,data.train)
err(data.train$spam,pred.train$class)

pred.test<-predict(lda.out,data.test)
err(data.test$spam,pred.test$class)

# (2) bagging
set.seed(0829539)
bag.mod=randomForest(spam~.,data=data.train,classwt=c(0.606,0.394),mtry=57,ntree=5000,importance=TRUE)
bag.mod

#plot oob error
par(cex=1.2)
plot(1:3500,bag.mod$err.rate[1:3500,1],xlab="Number of iterations",ylab="OOB error",ylim=c(0.08,0.2),pch='',main="OOB error")
lines(1:3500,bag.mod$err.rate[1:3500,1],col="red")

#plot variable importance
importance(bag.mod,plot=TRUE)
varImpPlot(bag.mod,type=2,cex=1.2)

pred.train<-predict(bag.mod,newdata=data.train)
err(data.train$spam,pred.train)

pred.test<-predict(bag.mod,newdata=data.test)
err(data.test$spam,pred.test)

# (3) random forests
set.seed(0829539)
rf.mod=randomForest(spam~.,data=data.train,classwt=c(0.606,0.394),mtry=5,ntree=5000,importance=TRUE)
rf.mod

pred.train<-predict(rf.mod,newdata=data.train)
err(data.train$spam,pred.train)

pred.test<-predict(rf.mod,newdata=data.test)
err(data.test$spam,pred.test)

#variable importance plot
#two measures are reported: 
#increase in MSE computed on OOB samples when removing variable
#decrease in node impurity (or increase in node purity) resulting from splits on the variable
importance(rf.mod)
varImpPlot(rf.mod)

# (4) gradient boosting
set.seed(0829539)
data.train$spam<-ifelse(data.train$spam=="1_yes",1,0)
data.test$spam<-ifelse(data.test$spam=="1_yes",1,0)
spamdata$spam<-ifelse(spamdata$spam=="1_yes",1,0)

#use distribution="bernoulli" for binary target
#interaction.depth=4, means that we fit a tree that uses 4 splits 
#(and that includes at most a 4-th order interaction)
boost.mod=gbm(spam~.,data=data.train,distribution="bernoulli",n.trees=20000,interaction.depth=4,shrinkage=0.001,cv.folds=5)
gbm.perf(boost.mod,method="cv")
legend("topright",c("train error","CV error"),col=c("green","black"),lty=c(1,1))

#relative influence plot
par(cex=1.2)
summary(boost.mod,n.trees=19932)

pred.train<-predict(boost.mod,n.trees=19932,newdata=data.train,type="response")
classif.train<-ifelse(pred.train>=1-pred.train,1,0)
err(data.train$spam,classif.train)
pred.test<-predict(boost.mod,n.trees=19932,newdata=data.test,type="response")
classif.test<-ifelse(pred.test>=1-pred.test,1,0)
err(data.test$spam,classif.test)

#(b)Account for different prior probabilities and unequal classification costs: C(spam|email)=10*C(email|spam)
# (1) linear discriminant analysis
set.seed(0829539)
data.train$spam<-factor(ifelse(data.train$spam==1,"1_yes","0_no"))
data.test$spam<-factor(ifelse(data.test$spam==1,"1_yes","0_no"))
spamdata$spam<-factor(ifelse(spamdata$spam==1,"1_yes","0_no"))

lda.out<-lda(spam~.,prior=c(0.606,0.394),data=data.train)
print(lda.out)

pred.train<-predict(lda.out,data.train)
classif.train<-ifelse(1*pred.train$posterior[,2]>=10*pred.train$posterior[,1],1,0)
err(data.train$spam, classif.train)

pred.test<-predict(lda.out,data.test)
classif.test<-ifelse(1*pred.test$posterior[,2]>=10*pred.test$posterior[,1],1,0)
err(data.test$spam,classif.test)

# (2) bagging
set.seed(0829539)
bag.mod=randomForest(spam~.,data=data.train,mtry=57,ntree=5000,classwt=c(0.606,0.394),importance=TRUE)
bag.mod

#plot oob error
par(cex=1.2)
plot(1:3500,bag.mod$err.rate[1:3500,1],xlab="Number of iterations",ylab="OOB error",ylim=c(0.08,0.2),pch='',main="OOB error")
lines(1:3500,bag.mod$err.rate[1:3500,1],col="red")
#plot variable importance
importance(bag.mod,plot=TRUE)
varImpPlot(bag.mod,type=2,cex=1.2)

pred.train<-predict(bag.mod,type="prob",newdata=data.train)
classif.train<-ifelse(1*pred.train[,2]>=10*pred.train[,1],1,0)
err(data.train$spam,classif.train)
pred.test<-predict(bag.mod,type="prob",newdata=data.test)
classif.test<-ifelse(1*pred.test[,2]>=10*pred.test[,1],1,0)
err(data.test$spam,classif.test)

# (3) random forests
set.seed(0829539)
rf.mod=randomForest(spam~.,data=data.train,mtry=5,ntree=5000,classwt=c(0.606,0.394),importance=TRUE)
rf.mod

pred.train<-predict(rf.mod,type="prob",newdata=data.train)
classif.train<-ifelse(1*pred.train[,2]>=10*pred.train[,1],1,0)
err(data.train$spam,classif.train)
pred.test<-predict(rf.mod,type="prob",newdata=data.test)
classif.test<-ifelse(1*pred.test[,2]>=10*pred.test[,1],1,0)
err(data.test$spam,classif.test)

#variable importance plot
#two measures are reported: 
#increase in MSE computed on OOB samples when removing variable
#decrease in node impurity (or increase in node purity) resulting from splits on the variable
importance(rf.mod)
varImpPlot(rf.mod)

# (4) gradient boosting
set.seed(0829539)
data.train$spam<-ifelse(data.train$spam=="1_yes",1,0)
data.test$spam<-ifelse(data.test$spam=="1_yes",1,0)
spamdata$spam<-ifelse(spamdata$spam=="1_yes",1,0)

#use distribution="bernoulli" for binary target
#interaction.depth=4, means that we fit a tree that uses 4 splits 
#(and that includes at most a 4-th order interaction)
boost.mod=gbm(spam~.,data=data.train,distribution="bernoulli",n.trees=20000,interaction.depth=4,shrinkage=0.001,cv.folds=5)
gbm.perf(boost.mod,method="cv")
legend("topright",c("train error","CV error"),col=c("green","black"),lty=c(1,1))

#relative influence plot
par(cex=1.2)
summary(boost.mod,n.trees=19932)

pred.train<-predict(boost.mod,n.trees=19932,newdata=data.train,type="response")
classif.train<-ifelse(1*pred.train>=10*(1-pred.train),1,0)
err(data.train$spam,classif.train)
pred.test<-predict(boost.mod,n.trees=19932,newdata=data.test,type="response")
classif.test<-ifelse(1*pred.test>=10*(1-pred.test),1,0)
err(data.test$spam,classif.test)