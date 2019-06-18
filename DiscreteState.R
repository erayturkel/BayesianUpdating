GenPriorVec<-function(s){
  vec<-rexp(s)
  return(vec/sum(vec))
}

GenChannelMatrix<-function(s){
  #Rows represent signals, columns represent states of the world.
  #Columns: States of the world (w)
  #Rows: The probability of sending signal s in a given state of the world w : Pi(s|w)
  #Each column should sum up to 1.
  ChannelMatrix<-matrix(0,nrow=s,ncol=s)
  for(i in 1:s){
    vec<-rexp(s)
    ChannelMatrix[,i]<-(vec/sum(vec))
  }
  return(ChannelMatrix)
}


GenMultipleChannels<-function(n,s){
  #Generates n channels, in a world where the state space has s elements.
  ChanList<-array(dim=c(s,s,n))
  for(i in 1:n){
    ChanList[,,i]<-GenChannelMatrix(s)
  }
  return(ChanList)
  
}

GenMultipleConsumers<-function(n,s){
  #Generates n consumers, in a world where the state space has s elements.
  ConList<-array(dim=c(1,s,n))
  for(i in 1:n){
    ConList[,,i]<-GenPriorVec(s)
  }
  return(ConList)
  
}


ProbOfPosterior<-function(ChannelMat, PriorVec){
  #A channel's signaling strategy generates a distribution over posterior distributions.
  #Given a channel matrix with s signals, there are s possible posteriors.
  #This function calculates the probability of each posterior.
  return(ChannelMat%*%PriorVec)
}

CalcPosterior<-function(ChannelMat, PriorVec){
  #Given a channel's signal matrix and a prior vector, 
  #calculates the posterior matrix.
  #Rows represent signals, and columns represent states of the world.
  #Each row is a different posterior: The posterior realized when the signal is s.
  #So each element is mu_s(w): 
  #The posterior probability assigned to the state w when signal s is observed.
  UnWeighted<-t(t(ChannelMat)*PriorVec)
  Weights<-ProbOfPosterior(ChannelMat,PriorVec)
  for(i in 1:nrow(UnWeighted)){
    UnWeighted[i,]<-UnWeighted[i,]/Weights[i,1]
  }
  return(UnWeighted)
}

ShannonEntropy<-function(PosteriorMatrix){
  #This returns the shannon entropy vector given a posterior matrix.
  #Each row is the SE of the corresponding row in the Posterior Matrix.
  SEvec<-as.matrix(rep(0,nrow(PosteriorMatrix)))
  for(i in 1:nrow(PosteriorMatrix)){
    SEvec[i,]<- -PosteriorMatrix[i,]%*%log(PosteriorMatrix[i,])
  }
  return(SEvec)
}

KLDivergence<-function(PosteriorMatrix,PriorVec){
  #Returns the KL Divergence matrix, between the prior and the posterior distribution matrix.
  #Each row is the KL-Divergence between the prior and the corresponding
  #posterior distribution (row) of the Posterior Matrix.
  LogRatioMatrix<-log(t(PriorVec/t(PosteriorMatrix)))
  return(LogRatioMatrix%*%PriorVec)
}

ExpectedCost<-function(ChannelMat,PriorVec,alpha){
  #Returns the expected cost of watching Channel Matrix with the prior distribution PriorVec.
  #Alpha is the weight on Expected Shannon Entropy: expected degree of uncertainty in the posteriors.
  #1-Alpha is the weight on the expected KL Divergence: expected difference between prior and posteriors.
  Posterior<-CalcPosterior(ChannelMat,PriorVec)
  PostProbs<-ProbOfPosterior(ChannelMat,PriorVec)
  SE<-ShannonEntropy(Posterior)
  KLD<-KLDivergence(Posterior,PriorVec)
  return(alpha*sum(SE*PostProbs) + (1-alpha)*sum(KLD*PostProbs))
}


ChooseBestChannel<-function(ChannelList,priorvec,alpha){
  #Chooses the best channel for consumer with the given prior vector.
  MinCost<-.Machine$double.xmax
  Minimizer<-0
  NoChannel<-dim(ChannelList)[3]
  for(i in 1:NoChannel){
    cost<-ExpectedCost(ChannelList[,,i],priorvec,alpha)
    if(cost<MinCost){
      MinCost<-cost
      Minimizer<-i
    }
  }
  return(Minimizer)
  
}


PlotChannels<-function(ChannelList){
  #Plot the signaling strategies of each channel
  NoChannel<-dim(ChannelList)[3]
  par(mfcol=c(1,NoChannel))
  for(i in 1:NoChannel){
    matplot(t(ChannelList[,,i]),ylim = c(0,1),ylab = as.character(i))
  }
  title("Channels",outer="TRUE")
}

PlotConsumers<-function(ConsumerList){
  #Plot the priors for each consumer
  NoCon<-dim(ConsumerList)[3]
  par(mfcol=c(1,NoCon))
  for(i in 1:NoCon){
    matplot(t(ConsumerList[,,i]),ylim = c(0,1),ylab = as.character(i))
  }
  title("Consumers",outer = TRUE)
}


SimulateChoiceRandom<-function(Nconsumers,Nchannels,Nstates,alpha){
  #Simulate the model with random channels and consumers.
  #Choose the number of consumers and channels, and states of the world.
  #Generates random channel strategies and consumer priors.
  #Plots the generated channel strategies and consumer priors.
  #Calculates the optimal choice for each consumer.
  #Plots the consumer prior and the chosen channel's strategy side by side.
  #If there are a large number of consumers and channels,
  #the plot panel in R needs to be modified to be very large,
  #or it will throw an error saying 'figure margins too large'.
  ChannelMat<-GenMultipleChannels(Nchannels,Nstates)
  ConsumerMat<-GenMultipleConsumers(Nconsumers,Nstates)
  PlotChannels(ChannelMat)
  PlotConsumers(ConsumerMat)
  ChoiceVector<-rep(0,dim(ConsumerMat)[3])
  par(mfrow=c(dim(ConsumerMat)[3],2))
  for(i in 1:dim(ConsumerMat)[3]){
    ChoiceVector[i]<-ChooseBestChannel(ChannelMat,ConsumerMat[,,i],alpha)
    matplot(t(ConsumerMat[,,i]),ylim = c(0,1),ylab = as.character(i))
    matplot(t(ChannelMat[,,ChoiceVector[i]]),ylim = c(0,1),ylab = as.character(ChoiceVector[i]))
  }
  title("Choices",outer=TRUE)
  return(ChoiceVector)
}



GenerateChoice<-function(ConsumerMat,ChannelMat,alpha){
  #Generate Choice with pre-made consumer and channel matrices.
  #Plots channel strategies, consumer priors, and the choice.
  #Calculates the optimal choice for each consumer.
  #Plots the consumer prior and the chosen channel's strategy side by side.
  #If there are a large number of consumers and channels,
  #the plot panel in R needs to be modified to be very large,
  #or it will throw an error saying 'figure margins too large'.
  #Returns the choice vector.
  PlotChannels(ChannelMat)
  PlotConsumers(ConsumerMat)
  ChoiceVector<-rep(0,dim(ConsumerMat)[3])
  par(mfrow=c(dim(ConsumerMat)[3],2))
  for(i in 1:dim(ConsumerMat)[3]){
    ChoiceVector[i]<-ChooseBestChannel(ChannelMat,ConsumerMat[,,i],alpha)
    matplot(t(ConsumerMat[,,i]),ylim = c(0,1),ylab = as.character(i))
    matplot(t(ChannelMat[,,ChoiceVector[i]]),ylim = c(0,1),ylab = as.character(ChoiceVector[i]))
  }
  title("Choices",outer=TRUE)
  return(ChoiceVector)
}

SimulateChoiceRandom(5,2,2,0.5)