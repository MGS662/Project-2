#Importing the train data
train<-read.csv('/Users/yashahuja/Desktop/College/Sem 2/Machine learning for IT Managers IE662/BlogFeedback/blogData_train.csv',header=FALSE,sep=",")


#preprocessing the data
features<-train[c("V51","V52","V53","V54","V56","V57","V58","V59","V281")]

#Sampling of data values 'x' and 'b', and splitting into train and test pairs
set.seed(12)
train1<-features[sample(1:nrow(train), 5000,replace=FALSE),]
train1
train.rows<-sample(nrow(train1), 4000)
train.rows
train.set<-train1[train.rows,]
train.x<-train.set[c("V51","V52","V53","V54","V56","V57","V58","V59")]
train.b<-train.set[c("V281")]
test.set<-train1[-train.rows,]
test.x<-test.set[c("V51","V52","V53","V54","V56","V57","V58","V59")]
test.b<-test.set[c("V281")]

#Converting to matrix
test.x<-as.matrix(test.x)
test.b<-as.matrix(test.b)
train.x<-as.matrix(train.x)
train.b<-as.matrix(train.b)

#Defining terms for calculating the MSE from RSS
nrow_test<-nrow(test.b-y.hat)
nrow_test
nrow_train<-nrow(train.b-y.hat)
nrow_train

##Not calling the function like how it was done using iris dataset, because of the error in dimension of 'c'. 
#Using ols function
rf=solve.ols(as.matrix(train.x), as.matrix(train.b))
rf

require(Rmosek)
solve.ols<-function(X,y, verb=1){
  p<-dim(X)[2]  # number of parameters needed
  
  #correspondence between OLS and the Quad program
  xx<-crossprod(X) # X'X=Q variable in the Quadratic program
  c<--crossprod(X,y) # X'y=c variable in the Quadratic program
  xx2<-xx
  xx2[upper.tri(xx)]<-0 #mosek needs Q to be  triangular
  idx <- which(xx2 != 0, arr.ind=TRUE) #index of the nonzero elements of Q
  
  #problem
  blogmodel<-list() #empty list that contains the Q problem
  blogmodel$sense<-"min" #problem sense
  blogmodel$c<-as.vector(c) #objective coefficients
  blogmodel$qobj<-list(i = idx[,1],
                 j = idx[,2],
                 v = xx2[idx] ) #the Q matrix is imputed by the row indexes i, the cop indexes j and the values v that define the Q matrix
  blogmodel$A<-Matrix(rep(0,p), nrow=1,byrow=TRUE,sparse=TRUE) #constraint matrix A is a null
  blogmodel$bc<-rbind(blc=0, buc= Inf) #constraint bounds
  blogmodel$bx<-rbind(blx=rep(-Inf,p), bux = rep(Inf,p)) #data value bounds 
  r<-mosek(blogmodel, opts = list(verbose = verb)) #call mosek solver
  return(r)
}

#Calculating predicted values of y (i.e.) y.hat, RSS value, and MSE for MOSEK solution for test
y.hat=test.x[,1]*0.12630415 + test.x[,2]*0.21347815 + test.x[,3]*(-0.01587096)+ test.x[,4]*(-0.14254154)  + test.x[,5]*(-1.73508394) +test.x[,6]*2.19002483 +test.x[,7]*(-1.04679858)+  test.x[,8]*(2.60912549)

RSSinmosek<-sum((test.b-y.hat)^2)
RSSinmosek
MSEinMosek.test<-RSSinmosek/nrow_test
MSEinMosek.test


#Calculating predicted values of y (i.e.) y.hat, RSS value, and MSE for MOSEK solution for train
y.hat.t=train.x[,1]*0.12630415 + train.x[,2]*0.21347815 + train.x[,3]*(-0.01587096)+ train.x[,4]*(-0.14254154)  + train.x[,5]*(-1.73508394) +train.x[,6]*2.19002483 +train.x[,7]*(-1.04679858)+  train.x[,8]*(2.60912549)
RSSinmosek.train<-sum((train.b-y.hat.t)^2)
RSSinmosek.train
MSEinMosek.train<-RSSinmosek.train/nrow_train
MSEinMosek.train

#Linear Regression on test
fit.lm<- lm(formula =V281  ~ .,data = data.frame(train1))
prediction <- predict(fit.lm, data.frame(test.x), se.fit = TRUE)
summary(fit.lm)
prediction

#Linear Regression on train
fit.lm.train<- lm(formula =V281  ~ .,data = data.frame(train1))
prediction.train <- predict(fit.lm.train, data.frame(train.x), se.fit = TRUE)
summary(fit.lm.train)
prediction.train

#MSE of model using Linear regression on test
RSS.lm<-sum((test.b - prediction$fit)^2)
RSS.lm
MSE_lm.test<-RSS.lm/nrow_test
MSE_lm.test
summary(fit.lm)

#MSE of model using Linear regression on train
summary(fit.lm)
RSS.lm<-sum((train.b - prediction.train$fit)^2)
RSS.lm
MSE_lm.train<-RSS.lm/nrow_train
MSE_lm.train