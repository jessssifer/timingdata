#load one block at a time
    load.block=function(K){ 
      #setting workspace and choosing block 
      #setwd("C:/Users/jyfeng/Documents")
      dat <- read.csv(file = paste0("data_combine_M", K, ".csv"), header=T)
      
      nitem <<- which(colnames(dat) == "HELPMAT4") - 3
      
      for(j in 1:nitem+1){
        if(class(dat[,j])=="numeric"){
          dat[,j] <- dat[,j]
          
        } else{
          dat[,j] <- as.numeric(levels(dat[,j]))[dat[,j]]
        }
      } 
    } 
  
#to load all the data into different vars so u don't have to keep load.block each time
    load.all.block=function(){ 
      for(K in 1:8){
        #setting workspace and choosing block 
        setwd("C:/Users/jyfeng/Documents")
        dat <<- read.csv(file = paste0("data_combine_M", K, ".csv"), header=T)
        
        nitem <- which(colnames(dat) == "HELPMAT4") - 3
        
        for(j in 1:nitem+1){
          if(class(dat[,j])=="numeric"){
            dat[,j] <- dat[,j]
            
          } else{
            dat[,j] <- as.numeric(levels(dat[,j]))[dat[,j]]
          }
        } 
        
        assign(paste0("M",K),dat)
        assign(paste0("M",K,"nitem"),nitem)
      }
    } 

#finding difficulty for each item
    block.diff=function(K){
      
      load.block(K)
      nitem <<- which(colnames(dat) == "HELPMAT4") - 3
      
      difficulty <<- matrix(nrow=nitem, ncol=1)
      rownames(difficulty)=c(names(dat[1:nitem+1]))
      colnames(difficulty)="difficulty"
      
      for(i in 1:nitem){
        Col.num <<- which(colnames(dat) == "Block.Position")
        idx.8 <- which(dat[,i + Col.num] == 8) # row indexes which students have score 8 for item i; scores start from column Col.num + 1 
        dat[idx.8[!is.na(dat[idx.8, i+1])],i + Col.num] <<- 0  # assign score 0 to items have time with raw score 8
        dat[idx.8[is.na(dat[idx.8, i+1])],i + Col.num] <<- NA  # assign NA to items with NA time record and raw score 8
        idx.9 <- which(dat[,i+Col.num ] == 9) # do the same for 9
        dat[idx.9[!is.na(dat[idx.9, i+1])],i + Col.num] <<- 0  
        dat[idx.9[is.na(dat[idx.9, i+1])],i + Col.num] <<- NA 
        
        
        if(3 %in% dat[,(Col.num+i)]){ #scores range from 0-3, 4 categories
          score.freq <- c(sum(dat[,(Col.num+i)]==0, na.rm=T), sum(dat[,(Col.num+i)]==1, na.rm=T), sum(dat[,(Col.num+i)]==2, na.rm=T), sum(dat[,(Col.num+i)]==3, na.rm=T))/length(dat[,(Col.num+i)][!is.na(dat[,(Col.num+i)])])
          difficulty[i]<-sum(c(0,1/3,2/3,1)*score.freq)
        } else if(2 %in% dat[,(Col.num+i)]){ #scores range from 0-2, 3 categories
          score.freq <- c(sum(dat[,(Col.num+i)]==0, na.rm=T), sum(dat[,(Col.num+i)]==1, na.rm=T), sum(dat[,(Col.num+i)]==2, na.rm=T))/length(dat[,(Col.num+i)][!is.na(dat[,(Col.num+i)])])
          difficulty[i]<-sum(c(0,0.5,1)*score.freq)
        } else { #scores range from 0-1, 2 categories
          score.freq <- c(sum(dat[,(Col.num+i)]==1, na.rm=T))/length(dat[,(Col.num+i)][!is.na(dat[,(Col.num+i)])])
          difficulty[i]<-sum(c(1)*score.freq)
        }
      }
      print(difficulty)
      return(difficulty)
    }
 
#plot total time spent on each item, same block   
    plot.timespent=function(K){
      load.block(K)
      nitem <<- which(colnames(dat) == "HELPMAT4") - 3
      
      # create 95% data with only response times: trunc.data.rt 
      item.rt<<-dat[,1:nitem+1]
      
      #trying to find %correct for each item AKA P+
      diff=c(ceiling(block.diff(K)*100))
      
      #trying to find % reached
      reached <- matrix(nrow=nitem, ncol=1)
      rownames(reached)=c(names(dat[1:nitem+1]))
      colnames(reached)="reached"
      
      dat.time <<- dat[,1:nitem+2]
      
      for(i in 1:nitem){
        reached[i]=ceiling((nrow(dat.time)-sum(is.na(dat.time[,i])))/nrow(dat.time)*100)
      }
      reach=c(print(reached))
      
      #now jessie tries to do the color coding thing
      item.type.dat=read.csv("ItemTypeGr4.csv")
      col.vec=vector()
      item.names=names(item.rt)
      for(i in 1:nitem){
        row.num=which(item.type.dat$DBA.Accnum==paste0(item.names[i]))
        col.vec[i]=item.type.dat[row.num,"DBA.Item.Type"]
      }
      
      #plot the stupid things  
      boxplot(item.rt, main=paste0("Time Spent Per Item in Block ",K), ylab="time (seconds)", 
              range=1.5, outline=F, col=col.vec+2)
      axis(1, 1:nitem, col="red", line=1.4, diff, tick=F)
      mtext("percentage correct",side=1.4, line=1.5,cex=0.75)
      axis(1, 1:nitem, col="blue", line=2.9, reach, tick=F)
      mtext("percentage reached",side=1, line=2.9,cex=0.75)
      #legend("topleft", inset=0.03, legend=c("SR","SCR","ECR"), fill=c("6","5","4"), 
             #title="Item Types", horiz=T)
    }
    
#plotting item I from all blocks in one boxplot
    plot.itemfromblocks=function(I){
      
      #load item I from each block into a vector
      V1=vector(mode="numeric")
      V2=vector(mode="numeric")
      V3=vector(mode="numeric")
      V4=vector(mode="numeric")
      V5=vector(mode="numeric")
      V6=vector(mode="numeric")
      V7=vector(mode="numeric")
      V8=vector(mode="numeric")
      ItemInames=vector()
      
      ItemI <<- list(V1, V2, V3, V4, V5, V6, V7, V8)
      for(K in 1:8){
        load.block(K)
        ItemI[K] <<- c(list(dat[,I+1]))
        ItemInames[K] <- colnames(dat[I+1])
      }
      
      #trying to put all the P+ in one vector for later
      ItemIdiff <<- vector()
      for(K in 1:8){
        diff=c(ceiling(block.diff(K)*100))
        ItemIdiff[K] <<- diff[I]
      }
      
      #calculating % reached
      reached <- matrix(nrow=8, ncol=1)
      rownames(reached)=c(ItemInames)
      colnames(reached)="reached"
      
      
      
      for(K in 1:8){
        reached[K]=ceiling((length(ItemI[K])-sum(is.na(ItemI[K])))/length(ItemI[K])*100)
      }
      reach <<- c(print(reached))
      
      #trying to color code
      setwd("C:/Users/jyfeng/Documents")
      item.type.dat=read.csv("ItemTypeGr4.csv")
      col.vec <- vector()
      for(K in 1:8){
        col.vec[K] <- item.type.dat[which(item.type.dat$DBA.Accnum==paste0(ItemInames[K])),"DBA.Item.Type"]
      }
      print(col.vec)
      
      #now for the actual plot
      boxplot(ItemI, range=1.5, outline=F, ylab="time (seconds)", col=col.vec+2, xaxt='n')
      axis(1, 1:8, labels=ItemIdiff, col="red", tick=F, line=0.4)
      mtext("percentage correct",side=1, line=0.4,cex=0.75)
      axis(1, 1:8, labels=reach, col="blue", line=2.1, tick=F)
      mtext("percentage reached",side=1, line=2.3,cex=0.75)
      mtext(paste("Time Spent on Item", I, "Across All Blocks"), 3)
      legend("topleft", inset=0.03, legend=c("SR","SCR","ECR"), fill=c("6","5","4"), 
             title="Item Types", horiz=T,cex=0.6)
    }
    
#produces normal dist mean & SD that best approximates data
    iid.fit.lnorm=function(K){
      
      library(fitdistrplus)
      
      dat <- read.csv(file = paste0("data_combine_M", K, ".csv"), header=T)
      item.rt.names=c(names(dat[1:nitem+1]),"Block.Position")
      item.rt=dat[item.rt.names]
      nitem <<- which(colnames(dat) == "HELPMAT4") - 3
      
      for(j in 1:nitem){
        if(class(item.rt[,j])=="numeric"){
          
        } else{
          item.rt[,j] <- as.numeric(levels(dat[,j+1]))[dat[,j+1]]
        }
      } 
      
      printed=matrix(ncol=2,nrow=nitem)
      colnames(printed)=c("mean", "sd")
      rownames(printed)=c(colnames(dat[,1:nitem+1]))
      
      
      for(h in 1:nitem){
        log.dat=log(item.rt)
        log.dat.col=log.dat[,h]
        log.dat.col.no=log.dat.col[!is.na(log.dat.col)]
        fitNormal=fitdist(log.dat.col.no,dist="norm")
        printed[h,1]=max((fitNormal$estimate))
        printed[h,2]=min((fitNormal$estimate))
      }
      print(printed)
      return (printed)
    }
    
#jessie attempts to draw samples from the norm dist from above and find % over 30 and it works???
    iid.samp.norm=function(N,nitem,K){
      temp.para=iid.fit.lnorm(nitem,K)
      samp.vec=matrix()
      samp.sum=matrix()
      samp.temp=vector("numeric")
      
      #sample from each item and put total time (sum) in a matrix 
      for(i in 1:N){
        for(l in 1:nitem){  
          #samp.temp is a vector that holds sampled times from all items in a block
          samp.temp[l]=exp(rnorm(1,temp.para[l,1],temp.para[l,2]))
          #samp.vec holds N values of the total times of a block 
          samp.sum[i]=sum(samp.temp, na.rm=T)
        }
        samp.vec[i]=samp.sum[i]
      }
      #plot matrix and yield % over 30 min
      hist(samp.vec/60, breaks=50)
      abline(v=30, col="red")
      print(sum(samp.vec > 1800)/N)
      
    }    
    
#function for ks test without having to rewrite all the time
    kstest=function(M.1, n1, M.2, n2){
      x <- c(M.1[,n1+1], na.rm=T)
      y <- c(M.2[,n2+1], na.rm=T)
      return(ks.test(x,y))
    }
    
#this function will create and return the normal mean&sd estimates & char table for a block 
    create.block.lnormtable=function(K){
      
      #we r goin 2 load a block
      load.block(K)
      
      #we r goin 2 calculate mean and sd and store it in a data frame!!!
      temp.df.name = data.frame()
      iid.fit.lnorm(K) -> parameters
      temp.df.name = data.frame(parameters)
      
      #we r goin 2 add item position column to gary!!!
      temp.df.name["position"] <- c(1:nitem)
      
      #we r goin 2 add p+ column to gary!!!
      block.diff(K) -> pplus
      temp.df.name["difficulty"] <- c(pplus)
      
      #we r goin 2 add item type to gary!!!
      item.type.dat=read.csv("ItemTypeGr4.csv")
      col.vec=vector()
      item.names=colnames(dat[,1:nitem+1])
      for(i in 1:nitem){
        row.num=which(item.type.dat$DBA.Accnum==paste0(item.names[i]))
        col.vec[i]=item.type.dat[row.num,"DBA.Item.Type"]
      }
      temp.df.name["type"] <- col.vec
      
      return(temp.df.name)
    }
 
#find stable dist parameters for block   
    stable.fit=function(block, K){
      load.all.block()
      load.block(K)
      alpha=vector(length=nitem)
      beta=vector(length=nitem)
      gamma=vector(length=nitem)
      delta=vector(length=nitem)
      for(i in 1:nitem){
        trunc <- block[block[,i+1] < quantile(block[,i+1], 0.95, na.rm=T),]
        Y <- trunc[,i+1]
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
    
    
