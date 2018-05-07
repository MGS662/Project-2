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

#Converting tests to matrix
test.x=as.matrix(test.x)
test.b=as.matrix(test.b)

##Not calling the function like how it was done using iris dataset, because of the error in dimension of 'c'. 
#Using ols function
rf=solve.ols(train.x, train.b)
rf


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
y.hat=test.x[,1]*0.11746 + test.x[,2]*0.22169 + test.x[,3]*(-0.01916)+ test.x[,4]*(-0.13976)  + test.x[,5]*0.25588596 +test.x[,6]*(-0.17987356) +test.x[,7]*(-0.16376755)+  test.x[,8]*0.01128952
MSEinMosek<-MSE.lm(test.b-y.hat)
MSEinMosek
RSSinmosek<-sum((test.b-y.hat)^2)
RSSinmosek

#Calculating predicted values of y (i.e.) y.hat, RSS value, and MSE for MOSEK solution for train
y.hat.t=train.x[,1]*0.11746 + train.x[,2]*0.22169 + train.x[,3]*(-0.01916)+ train.x[,4]*(-0.13976)  + train.x[,5]*0.25588596 +train.x[,6]*(-0.17987356) +train.x[,7]*(-0.16376755)+  train.x[,8]*0.01128952
RSSinmosek.train<-sum((train.b-y.hat.t)^2)
RSSinmosek.train

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
MSE.lm <- function(error)
{
  mean(error^2)
}
e <- (test.b - prediction$fit)
MSE=MSE.lm(e)
MSE
summary(fit.lm)
RSS.lm<-sum((test.b - prediction$fit)^2)
RSS.lm