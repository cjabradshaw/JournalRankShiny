jRnkBootFunc <- function(datsamp, iter=1000, kap=2, kN=5) {
  
  ## setup
  ldat <- dim(datsamp)[1]
  noVec <- 1:ldat

  # observed ranks
  rnkmat <- rnkmatjk <- matrix(data=NA, nrow=iter, ncol=ldat)
  colnames(rnkmat) <- datsamp[,1]

  h5lnObs <- ldat - rank(datsamp[,4]/log10(datsamp[,2]), ties.method="average") + 1
  IFObs <- ldat - rank(datsamp[,5], ties.method="average") + 1
  IMObs <- ldat - rank(datsamp[,6], ties.method="average") + 1
  CSObs <- ldat - rank(datsamp[,7], ties.method="average") + 1
  SNIPObs <- ldat - rank(datsamp[,8], ties.method="average") + 1
  SJRObs <- ldat - rank(datsamp[,9], ties.method="average") + 1
  datraw <- data.frame(datsamp[,1], h5lnObs, IFObs, IMObs, CSObs, SNIPObs, SJRObs)
  avgrnk <- apply(datraw[,2:7], 1, "mean")
  datobs <- data.frame(datsamp[,1], avgrnk)
  
  ## resample
  for (i in 1:iter) {
    subboot <- sample(noVec, ldat, replace=T)
    datboot <- datsamp[subboot,]
    h5lnRnk <- ldat - rank(datboot[,4]/log10(datboot[,2]), ties.method="average") + 1
    IFRnk <- ldat - rank(datboot[,5], ties.method="average") + 1
    IMRnk <- ldat - rank(datboot[,6], ties.method="average") + 1
    CSRnk <- ldat - rank(datboot[,7], ties.method="average") + 1
    SNIPRnk <- ldat - rank(datboot[,8], ties.method="average") + 1
    SJRRnk <- ldat - rank(datboot[,9], ties.method="average") + 1
    rnkboot <- data.frame(datboot[,1], h5lnRnk, IFRnk, IMRnk, CSRnk, SNIPRnk, SJRRnk)
    colnames(rnkboot) <- c("journal","h5ln","IF","IM","CS","SNIP","SJR")
    bootavgrnk <- apply(rnkboot[,2:7], 1, "mean")
    rnkbootdat <- data.frame(rnkboot, bootavgrnk)
    rnkbootsort <- rnkbootdat[order(rnkbootdat[,8],decreasing=F),]
    rnkbootsortunique <- unique(rnkbootsort)

    # store bootstrapped ranks
    lboot <- dim(rnkbootsortunique)[1]
    substor <- rep(0,lboot)
    for (b in 1:lboot) {
      substor[b] <- which(datsamp[,1] == rnkbootsortunique[b,1])
    }
    
    rnkmat[i,substor] <- rnkbootsortunique[,8]
  }
  rnkmed <- round(apply(rnkmat, 2, "median", na.rm=T), 3)
  rnklo <- round(apply(rnkmat, 2, "quantile", na.rm=T, probs=0.025), 3)
  rnkup <- round(apply(rnkmat, 2, "quantile", na.rm=T, probs=0.975), 3)
  
  # kappa clipping
  rnkKap <- rnkmat

  for (k in 1:kN) {
    bootavg <- apply(rnkKap, 2, "mean", na.rm=T)
    bootsd <- apply(rnkKap, 2, "sd", na.rm=T)
    subsetmat <- rnknew <- matrix(data=NA, nrow=iter, ncol=ldat)
    
    for (l in 1:ldat) {
        subsetsub <- which(rnkKap[,l] > (bootavg[l] - (kap*bootsd[l])) & rnkmat[,l] < (bootavg[l] + (kap*bootsd[l])))
        lsubset <- length(subsetsub)
        subsetmat[1:lsubset,l] <- subsetsub
    } 
    for (l in 1:ldat) {
        rnknewvals <- rnkmat[na.omit(subsetmat[,l]),l]
        lnew <- length(rnknewvals)
        rnknew[1:lnew,l] <- rnknewvals
    }
    rnkKap <- rnknew
  }
  
  rnkKapmed <- round(apply(rnkKap, 2, "median", na.rm=T), 3)
  rnkKaplo <- round(apply(rnkKap, 2, "quantile", na.rm=T, probs=0.025), 3)
  rnkKapup <- round(apply(rnkKap, 2, "quantile", na.rm=T, probs=0.975), 3)
  
  datout <- data.frame(datsamp[,1],round(datsamp[,5],3),round(datobs[,2], 3),rnklo,rnkmed,rnkup,rnkKaplo,rnkKapmed,rnkKapup)
  colnames(datout) <- c("journal","IF","avgObsRnk", "rnkLo", "rnkMed", "rnkUp","rnkKaplo", "rnkKapmed", "rnkKapup")
  datsort <- datout[order(datout[,8],decreasing=F),]
  
  # print final output
  return(datsort)
  
} # end jRnkbootFunc
