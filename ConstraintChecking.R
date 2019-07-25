#World Space: Omega
#Signal Space: S
#Matrix p dimensions: Omega by S
#Mu: Prior vector, 1 by Omega
#Matrix X of posteriors: Omega by S


BayesUpdate<-function(mu,p){
  mup=mu%*%p
  X=matrix(0,nrow=nrow(p),ncol=ncol(p))
  for(i in 1:nrow(X)){
    for(j in 1:ncol(X)){
     X[i,j]=(mu[i]*p[i,j])/mup[1,j]
    }
  }
  return(X)
}

randomP<-function(omega,s){
  #Rows represent world, columns represent signals.
  #Each row should sum up to 1.
  Pmatrix<-matrix(0,nrow=omega,ncol=s)
  for(i in 1:omega){
    vec<-rexp(s)
    Pmatrix[i,]<-(vec/sum(vec))
  }
  return(Pmatrix)
  
}

mu=c(0.25,0.25,0.25,0.25)

i=1
n=100000
coordFrame<-data.frame(x=rep(0,n),y=rep(0,n),z=rep(0,n))
while(i <= n){
  X<-BayesUpdate(mu,randomP(4,2))
  for(j in ncol(X)){
    coordFrame[i,]<-X[(1:3),j]
    i<-i+1
  }
}

library(plotly)

plot_ly(x=coordFrame$x,y=coordFrame$y,z=coordFrame$z,type='scatter3d',marker = list(size = 3))