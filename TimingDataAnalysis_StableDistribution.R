##################################################################################
### last edited 07/26/2017                                                     ###
### trying to figure out how to fit timing data with stable                    ###
### distribution; define function given some # of                              ###
### items that will show how many will be over 30 min                          ###
### http://www2.stat.duke.edu/~zo2/shared/research/readings/stableprocess1.pdf ###
##################################################################################


#unsure what i will use so here i am loading everything i might need!!
  library(fitdistrplus)
  library(stabledist)
  library(MASS)
  library(fBasics)
  

#a beautiful function that will put stable parameters in a data frame!!
stable.fit=function(block, K){

  load.all.block()
  load.block(K)
  
  alpha=vector(length=nitem)
  beta=vector(length=nitem)
  gamma=vector(length=nitem)
  delta=vector(length=nitem)
  for(i in 1:nitem){
    #trunc <- block[block[,i+1] < quantile(block[,i+1], 0.95, na.rm=T),]
    Y <- block[,i+1]
    stablefit <- stableFit(Y, 1.5,1,2.5,0, type= c("q", "mle"), doplot=F)
    alpha[i] <- stablefit@fit$estimate[1]
    beta[i] <- stablefit@fit$estimate[2]
    gamma[i] <- stablefit@fit$estimate[3]
    delta[i] <- stablefit@fit$estimate[4]
  }
  sfit=data.frame(alpha,beta,gamma,delta)
  sfit["name"]<-c(colnames(block[,1:nitem+1]))
  
  return(sfit)
}

#below, find the inefficient methods I used to congregate all the data into one set
#####################################################################
#the lazy and inefficient way to rename everything
  sfitM1 <- stable.fit(M1,1)
  sfitM2 <- stable.fit(M2,2)
  sfitM3 <- stable.fit(M3,3)
  sfitM4 <- stable.fit(M4,4)
  sfitM5 <- stable.fit(M5,5)
  sfitM6 <- stable.fit(M6,6)
  sfitM7 <- stable.fit(M7,7)
  sfitM8 <- stable.fit(M8,8)

#the main data frame with all info above combined!!!
  StableFit <- rbind(sfitM1, sfitM2, sfitM3, sfitM4, sfitM5, sfitM6, sfitM7, sfitM8)
  StableFit["block"] <- c(rep(1:3, each=14), rep(4:5, each=15), rep(6:8, each=14))
  StableFit["position"] <-c(rep(1:14,3), rep(1:15,2), rep(1:14,3))
#####################################################################

  
#i think now i will try to figure out why some values in StableFit are NAs :C y tho
#####################################################################
    row=vector(mode="numeric")
    temp <- (which(is.na(StableFit[1:114,1]))) #find rows with missing values
    row <- temp
    
  #function will tell u which block # and item # is currently na, along with other NAs lol
  find.miss.block=function(){
    miss <- which(is.na(StableFit[1:114,1]))
    miss.block <- vector()
    for(i in 1:length(miss)){
      miss.block[i] <- paste0(StableFit[miss[i],]$block)
    }
    return(miss.block)
  }
  miss.block <- find.miss.block()
  print(miss.block)
  
  find.miss.name=function(){
    row <- which(is.na(StableFit[1:114,1]))
    miss.name <- vector()
    for(i in 1:length(row)){
      miss.name[i] <- paste0(StableFit[row[i],]$name)
    }
    return(miss.name)
  }
  miss.name <- find.miss.name()
  print(miss.name)
 
#####################################################################  
  
#here is jessie's attempt to manually fill in the missing parameters!!  
  #what's up with row 86?? block 6 number 14??
#####################################################################
fill.miss=function(row){
  for(i in 1:length(row)){
    Y <- unlist(M[row[i]])
    Y <- c(Y[!is.na(Y)])
    
    sf <- stableFit(Y, 1.5,0.95,2.5,1, type= c("q", "mle"), doplot=F)
    StableFit[row,1] <- sf@fit$estimate[1]
    StableFit[row,2] <- sf@fit$estimate[2]
    StableFit[row,3] <- sf@fit$estimate[3]
    StableFit[row,4] <- sf@fit$estimate[4]
  }  
}
  
  M=list()
  M<-c(M1[,1:M1nitem+1], M2[,1:M2nitem+1], M3[,1:M3nitem+1], M4[,1:M4nitem+1], 
       M5[,1:M5nitem+1], M6[,1:M6nitem+1], M7[,1:M7nitem+1], M8[,1:M8nitem+1])
  # names(M)= c(rep(1:114))
  


# shunned: Y <- Y[which(Y < quantile(Y, 0.95))]

#####################################################################

#here is an actual function!!
  sum.stable=function(names, n){
      
    #getting the vectors to store parameters 
      nitem=length(names) 
      rows=vector()
      a=vector()
      b=vector()
      c=vector()
      d=vector()
    #storing parameters of chosen items into vectors
      for(i in 1:length(names)){
        rows[i] <- which(StableFit$name== names[i])
        a[i] <- StableFit[rows[i],]$alpha
        b[i] <- StableFit[rows[i],]$beta
        c[i] <- StableFit[rows[i],]$gamma
        d[i] <- StableFit[rows[i],]$delta
      }
    
    #preparing the final display count
      sumstable <- matrix(ncol=4)
      colnames(sumstable) = c("alpha", "beta", "gamma", "delta")
    
    #sum stable alpha value
      sumstable[1] <- sum(a)/length(a)
      
    #sum stable gamma (scale) value
      C=vector()
      gamma=function(c1,c2){
        (c1^a + c2^a)^(1/a)
      }
      for(i in 2:nitem){
        C[1] <- (c[1]^a + c[2]^a)^(1/a)
        C[i] <- (C[i-1]^a + c[i+1]^a)^(1/a)
      }
      sumstable[3] <- C[nitem-1]  
      
    #sum stable beta (skew) value
      beta=function(b1,c1,b2,c2){
        (b1*((c1)^a) + b2*((c2)^a))/((c1)^a+(c2)^a)
      }
      betabase <- vector(length=nitem-1)
      for(i in 2:nitem){
        betabase[1] <- beta(b[1],c[1],b[2],c[2])
        betabase[i] <- beta(betabase[i-1], C[i-1], b[i+1], c[i+1])
      }
      sumstable[2] <- betabase[nitem-1]
      
    #sum stable delta (location) value
      sumstable[4] <- sum(d)
      
    #print table
      print(sumstable)
      
    #draw data but cut off at 95% & make all values positive
      samp <- c(rstable(n, sumstable[1], sumstable[2], sumstable[3], sumstable[4]))
      samp <- samp[which(samp > 0)]
      samp <- samp[which(samp < quantile(samp, 0.95))]
      heck <- replace(samp, samp > 1800, 1801)
      
    #plot (s)table
      hist(heck, breaks=100,
           main="Total Time",
           xlab="time (seconds)",
           xlim=range(0:max(samp)))
      abline(v=1800, col="red")
    
    #print % over 30 min
      sum(samp > 1800)/n
  }

#names for all of block 1
  names <- c(StableFit$name[1:14])
  sum.stable(names, 2000)

#compare sum.stable plot to:
  hist(M1$Total.Time..sum.of.items., breaks = 100)
  abline(v=1800, col="red")
  
  
  
  
  
  
  
#function needed below
random.sample <- function(x, i){
  success <- FALSE
  while (!success) {
    # do something
    x <- rstable(1, a[i], b[i], c[i], d[i])
    
    # check for success
    success <- x < qstable(ub, a[i], b[i], c[i], d[i])
    
  }
  return(x)
}

#now do the thing where you add percentile as a parameter!!
sum.perc.stable = function(names, n, lb, ub){
  
  #getting the vectors to store parameters 
  rows=vector()
  a=vector()
  b=vector()
  c=vector()
  d=vector()
  
  #storing parameters of chosen items into vectors
  for(i in 1:length(names)){
    rows[i] <- which(StableFit$name== names[i])
    a[i] <- StableFit[rows[i],]$alpha
    b[i] <- StableFit[rows[i],]$beta
    c[i] <- StableFit[rows[i],]$gamma
    d[i] <- StableFit[rows[i],]$delta
  }
  
  #take a sample of all items under p percentile, add it up, store it; do this n times
  time <- vector()
  t <- vector()
  
#OPTION 1  
  for(j in 1:n){
    for (i in 1:length(names)){
      t[i] <- random.sample(x, i)
      t[t < 0] <- 0
    }
    time[j] <- sum(t)
  }
  
  #OR
  
#OPTION 2
  for(j in 1:n){
    for(i in 1:length(names)){
      zero <- pstable(0, a[i], b[i], c[i], d[i])
      temp <- rstable(5000, a[i], b[i], c[i], d[i])
      temp <- temp[which(temp < quantile(temp, ub))]
      #temp <- temp[which(temp > quantile(temp, lb))]
      temp <- temp[which(temp > quantile(temp, zero))]
      
      t[i] <- sample(temp, 1)
    }
    time[j] <- sum(t)
  }
  
  hist(time, breaks=100, xlim=c(0,1250))
}

#compare with 25th percentile of actual data from M1
  actual <- M1[,23][which(M1[,23] < quantile(M1[,23],0.25))]
  hist(actual, breaks=100, xlim=c(0,1250))

  
  
  
  
  
#okay, attempt 2 at using parameters
  #draw out the rows I need and put into another data frame for easy access
  Q1 <- M1[which(M1[,23] < quantile(M1[,23], 0.25)), c(2:15,23)]
  Q2 <- M1[which(M1[,23] < quantile(M1[,23], 0.50)), c(2:15,23)]
    Q2 <- Q2[which(Q2[,15] > quantile(M1[,23], 0.25)),]
  Q3 <- M1[which(M1[,23] < quantile(M1[,23], 0.75)), c(2:15,23)]
    Q3 <- Q3[which(Q3[,15] > quantile(M1[,23], 0.50)),]
  Q4 <- M1[which(M1[,23] > quantile(M1[,23], 0.75)), c(2:15,23)]
  
  #for plotting histograms in one plot
  for(i in 1:14){
  par(mfrow = c(2,2))
  hist(Q1[,i], breaks=75, main=paste("< 25th: Item",i))
  hist(Q2[,i], breaks=75, main=paste("25th < 50th: Item",i))
  hist(Q3[,i], breaks=75, main=paste("50th < 75th: Item",i))
  hist(Q4[,i], breaks=75, main=paste("> 75th: Item",i))
}
  
  #for taking log normals of each item for 25% vs 75%
  mean <- matrix(ncol=1, nrow=56)
  sd <- matrix(ncol=1, nrow=56)  
  for(i in 1:14){  
  
    lQ1 <- log(Q1[1:14])
    lq1 <- lQ1[,i]
    lq1 <- lq1[!is.na(lq1)]
    fitQ1 <- fitdist(lq1, dist="norm")
    mean[i] <- max(fitQ1$estimate)
    sd[i] <- min(fitQ1$estimate)
    
    lQ2 <- log(Q2[1:14])
    lq2 <- lQ2[,i]
    lq2 <- lq2[!is.na(lq2)]
    fitQ2 <- fitdist(lq2, dist="norm")
    mean[i+14] <- max(fitQ2$estimate)
    sd[i+14] <- min(fitQ2$estimate)
    
    lQ3 <- log(Q3[1:14])
    lq3 <- lQ3[,i]
    lq3 <- lq3[!is.na(lq3)]
    fitQ3 <- fitdist(lq3, dist="norm")
    mean[i+14*2] <- max(fitQ3$estimate)
    sd[i+14*2] <- min(fitQ3$estimate)
  
    lQ4 <- log(Q4[1:14])
    lq4 <- lQ4[,i]
    lq4 <- lq4[!is.na(lq4)] 
    fitQ4 <- fitdist(lq4, dist="norm")
    mean[i+14*3] <- max(fitQ4$estimate)
    sd[i+14*3] <- min(fitQ4$estimate)

  }
  
#putting together a df with all info
  position <- c(rep(1,14), rep(2,14), rep(3,14), rep(4,14))
  item <- c(rep(1:14, 4))
  lQ <- data.frame(mean, sd, position, item)
  plot(x=mean, y=sd, col=position+2, pch=16)
 
  