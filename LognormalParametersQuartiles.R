# Jessie Feng
# playing around with lognormal things and quartiles
# 7 March 2018

setwd("~/Desktop")
library(stabledist)
library(fitdistrplus)

# here is a function that will give you back the lognormal mean and sd for the entire block
# as well as each quartile!
lognormalQuartiles = function(blockNumber){

  lognorm = data.frame(ncol = 10)

  blockParameters = stableParameters[stableParameters$block == blockNumber,]
  nitem = nrow(blockParameters)
  
  for(i in 1:nitem){
    samp = randomStableSample(blockNumber, i) 
    lnorm.samp = log(samp)
    fitNormal=fitdist(lnorm.samp, dist="norm")
    lognorm[i,1]=max((fitNormal$estimate))
    lognorm[i,2]=min((fitNormal$estimate))
  }
  
  for(i in 1:nitem){
    logSamp = log(randomStableSample(blockNumber, i))
    
    logSampQ1 = logSamp[logSamp < quantile(logSamp, 0.25)]
    fitQ1 = fitdist(logSampQ1, dist="norm")
    lognorm[i,3]=max((fitQ1$estimate))
    lognorm[i,4]=min((fitQ1$estimate))
    
    logSampQ2 = logSamp[logSamp > quantile(logSamp, 0.25) & logSamp < quantile(logSamp, 0.5)]
    fitQ2 = fitdist(logSampQ2, dist="norm")
    lognorm[i,5]=max((fitQ2$estimate))
    lognorm[i,6]=min((fitQ2$estimate))
    
    logSampQ3 = logSamp[logSamp > quantile(logSamp, 0.5) & logSamp < quantile(logSamp, 0.75)]
    fitQ3 = fitdist(logSampQ3, dist="norm")
    lognorm[i,7]=max((fitQ3$estimate))
    lognorm[i,8]=min((fitQ3$estimate))
    
    logSampQ4 = logSamp[logSamp > quantile(logSamp, 0.75)]
    fitQ4 = fitdist(logSampQ4, dist="norm")
    lognorm[i,9]=max((fitQ4$estimate))
    lognorm[i,10]=min((fitQ4$estimate))
  }
  colnames(lognorm) = c("overall mean", "overall sd", 
                        "Q1 mean", "Q1 sd",
                        "Q2 mean", "Q2 sd",
                        "Q3 mean", "Q3 sd",
                        "Q4 mean", "Q4 sd")
  
  return(lognorm)
   
}

# there was something wrong with block 6?
blockquartiles = lognormalQuartiles(8)

# a function that can help turn the data frame into a plottable format
lognormalQuartilesFormatted = function(blockNumber){
  
  lognorm = data.frame()
  blockParameters = stableParameters[stableParameters$block == blockNumber,]
  nitem = nrow(blockParameters)
  
  # for(i in 1:nitem){
  #   samp = randomStableSample(blockNumber, i) 
  #   lnorm.samp = log(samp)
  #   fitNormal=fitdist(lnorm.samp, dist="norm")
  #   lognorm[i,1]=max((fitNormal$estimate))
  #   lognorm[i,2]=min((fitNormal$estimate))
  # }
  
  for(i in 1:nitem){
    logSamp = log(randomStableSample(blockNumber, i))
    
    logSampQ1 = logSamp[logSamp < quantile(logSamp, 0.25)]
    fitQ1 = fitdist(logSampQ1, dist="norm")
    lognorm[1,i*3-2]=1
    lognorm[1,i*3-1]=max((fitQ1$estimate))
    lognorm[1,i*3]=min((fitQ1$estimate))
    
    logSampQ2 = logSamp[logSamp > quantile(logSamp, 0.25) & logSamp < quantile(logSamp, 0.5)]
    fitQ2 = fitdist(logSampQ2, dist="norm")
    lognorm[2,i*3-2]=2
    lognorm[2,i*3-1]=max((fitQ2$estimate))
    lognorm[2,i*3]=min((fitQ2$estimate))
    
    logSampQ3 = logSamp[logSamp > quantile(logSamp, 0.5) & logSamp < quantile(logSamp, 0.75)]
    fitQ3 = fitdist(logSampQ3, dist="norm")
    lognorm[3,i*3-2]=3
    lognorm[3,i*3-1]=max((fitQ3$estimate))
    lognorm[3,i*3]=min((fitQ3$estimate))
    
    logSampQ4 = logSamp[logSamp > quantile(logSamp, 0.75)]
    fitQ4 = fitdist(logSampQ4, dist="norm")
    lognorm[4,i*3-2]=4
    lognorm[4,i*3-1]=max((fitQ4$estimate))
    lognorm[4,i*3]=min((fitQ4$estimate))
    
    
  }
  
  return(lognorm)
  
}

block2format = lognormalQuartilesFormatted(2)

write.csv(block2format, "FormattedBlock2LognormalQuartileParameters.csv")


