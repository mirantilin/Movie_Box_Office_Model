library(glmnet)
Data=read.csv("350fp.csv")
names(Data)
X1=Data$Year
genre1=as.numeric(Data$Genre)
X2=rep(1,75)
X2[genre1>1]=0
X3=as.numeric(Data$Budget)
Y=Data$USboxOffice
X4=Data$MovieLength
X5=Data$TrailerLength
X6=Data$IMDbRating
X7=Data$Metascore
X8=Data$Metacritic
X9=Data$RottenTomatoes
X10=Data$NumberOscars
X11=Data$OscarNoms
X12=Data$OpenWeekend
newData=data.frame(Y, X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12)
newData2=data.frame(Y,X3,X4,X11,I(X3^3))
cor(newData)
lm.fit=lm(Y~.,newData)
summary(lm.fit)
lm.fit=lm(Y~X3+X4+X11+X10 +I(X3*X4)+I(X4*X11) + I(X3*X4*X11)  +I(X10*X11))
plot(lm.fit)
summary(lm.fit)
plot(X11,Y)
par(mfrow=c(2,2))
plot(lm.fit)

cov(newData)
#X=data.frame(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12)


#ridge regression
X=model.matrix(Y~.,newData)[,-1]
grid=10^seq(10,-2,length=100)
grid
set.seed(1)
nrow(X)
train=sample(1:nrow(X),nrow(X)/2)
train
test=(-train)
y.test=Y[test]
ridge.mod=glmnet(X[train,],Y[train],alpha=0,lambda=grid,thresh=1e-12)
plot(ridge.mod)
cv.out=cv.glmnet(X[train,],Y[train],alpha=0)
plot(ridge.mod,xvar="lambda",label=TRUE)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=X[test,])
mean((ridge.pred-y.test)^2)

#fit on the full model & estimated coefficients
out=glmnet(X,Y,alpha=0)

plot(out)
plot(out,xvar="lambda",label=TRUE)
out.pred=predict(out,type="coefficients",s=bestlam)
out.pred
out.predY=predict(out,s=bestlam,newx=X)
mean((out.predY-Y)^2)


#lasso
lasso.mod=glmnet(X[train,],Y[train],alpha=1,lambda=grid)
plot(lasso.mod,xvar="lambda",label=TRUE)
set.seed(1)
cv.out2=cv.glmnet(X[train,],Y[train],alpha=1)
plot(cv.out2)
bestlam2=cv.out2$lambda.min
bestlam2
lasso.pred=predict(lasso.mod,s=bestlam2,newx=X[test,])
mean((lasso.pred-y.test)^2)

#fit on the full model & estimated coefficients
out2=glmnet(X,Y,alpha=1)
plot(out2,xvar="lambda",label=TRUE)
lasso.coef=predict(out2,type="coefficients",s=bestlam2)
predict(out2,type="coefficients",s=bestlam2)
out.predY2=predict(out2,s=bestlam2,newx=X)
mean((out.predY2-Y)^2)
lasso.coef[lasso.coef!=0]


