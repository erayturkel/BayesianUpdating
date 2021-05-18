#install.packages("plotly")
library(plotly)
numState=2

#Consumer Prior
PriorVec<-c(0.8,0.2)

x<-seq(0.00001,0.99999,0.001)
y<-seq(0.00001,0.99999,0.001)
z<-matrix(nrow = length(x),ncol=length(y))

i<-1
a<-0.8 #The weight in the consumer's optimization problem: a is the weight on Expected SE.
#In other words, a is the weight on how much the consumer values precision.

for(alpha in seq(0.00001,0.99999,0.001)){
  j<-1
  for(beta in seq(0.00001,0.99999,0.001)){
    Channel<-matrix(c(alpha,1-alpha,1-beta,beta),nrow=numState,ncol=numState)
    z[i,j]<-ExpectedCost(Channel,PriorVec,a)
    j<- j+1
  }
  i<- i+1
}

#Plotting the cost of each possible Channel with two states.
#X axis is the channel's strategy on state 1, (The prob. of reporting 1 given w=1)
#Y axis is the channel's strategy on state 2. (The prob. of reporting 2 given w=2)
library(plotly)
plot_ly(x=x,y=y,z=z,type="surface")

