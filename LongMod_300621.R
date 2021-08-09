LongMod <- function(nchildren, inst, Inst, kkdata, BirthTimeStep, MaxSampleTime){
    ## Set seed ##
    .RNG.seed <- function(chain)
      return( switch(chain, "1"= 1, "2"= 2) )
    .RNG.name <- function(chain)
      return( switch(chain, "1" = "base::Super-Duper", "2" = "base::Wichmann-Hill") )
    
    
    model <- "model {
    
    # priors 
    
    # hazards
    haz.precision <- 1/(sigma*sigma)
    sigma ~ dunif(0,100)
  
    beta0 ~ dnorm(0,0.001) # baseline FOI
    beta1 ~ dnorm(0,0.001) # linear trend on time
    beta2 ~ dbeta(1,1) # try beta because it is about whether or not a worm survives and matures...?
    #beta3 ~ dgamma(0.001, 0.001)
    #beta4 ~ dbeta(0,1)
    
    shnb ~ dgamma(0.001,0.001)
    shnb2 ~ dgamma(0.001,0.001)
    
    # individual hazard variables
    tau.ind.prec <- 1/sigma.ind.haz*sigma.ind.haz
    sigma.ind.haz ~ dunif(0,100)
    
    # fixed values 
    shedding  <- 0.2 
    reproduction <- 300*21 # 300 eggs a day for three weeks
      
    #### biological process ####
    
    ## population level hazard ##
    
    Hazard[1] ~ dnorm(beta0,0.001)
    Hazard[2] ~ dnorm(Hhat[2], haz.precision) 
    
    Hhat[2] <- beta0 + beta1*2 + beta2*Hazard[1]
  
    for(t in 3:max(MaxSampleTime)){
    
      Hazard[t] ~ dnorm(Hhat[t], haz.precision) 
      Hhat[t] <- beta0 + beta1*t + beta2*Hazard[t-1]
    
      # population level treatment - will be added back in when I have a better idea of when this has occurred 
      # - beta3*treatment[t] 
      
    }
    
    ## individual level hazard ##
    
    for(i in 1:nchildren){
    
      log(h[i,BirthTimeStep[i]]) <- 0
      ind.prec[i,BirthTimeStep[i]]~ dnorm(0, tau.ind.prec) 
      
      log(h[i,BirthTimeStep[i]+1]) <- 0
      ind.prec[i,BirthTimeStep[i]+1]~ dnorm(0, tau.ind.prec) 
    
      ihaz[i,BirthTimeStep[i]] ~ dpois(h[i,BirthTimeStep[i]])
      ihaz[i,BirthTimeStep[i]+1] ~ dpois(h[i,BirthTimeStep[i]+1])
      
      for(t in (BirthTimeStep[i]+2):MaxSampleTime[i]){
        log(h[i,t]) <- Hazard[t-2]+ind.prec[i,t] # this is the individual level term 
        ind.prec[i,t]~ dnorm(0, tau.ind.prec) 
        ihaz[i,t] ~ dpois(h[i,t]) 
         #ihaz[i,t] ~ dnegbin(shnb/(h[i,t]+shnb), shnb ) 
      }
    }
    
    ## individual worm burden at each timestep ##
    
    #for(i in 1:nchildren){
      #WB[i,BirthTimeStep[i]] <- 0 
      #WB[i,BirthTimeStep[i]+1] <- ihaz[i,BirthTimeStep[i]+1]
      #WB[i,BirthTimeStep[i]+2] <- WB[i,BirthTimeStep[i]+1]+ihaz[i,BirthTimeStep[i]+2]
    
        #for(t in (BirthTimeStep[i]+3):MaxSampleTime[i]){
          #WB[i,t+1] <- (WB[i,t]+ihaz[i,t])-(WB[i,t-2])
    #*(beta4*mortality[i,t])) 
        #}  
    #}
    
    #### Observation Process ####
    
    for(i in 1:nchildren){
     
      EB[i,BirthTimeStep[i]] <- 0
      EB[i,(BirthTimeStep[i])+1] <- 0 
      log(EB[i,(BirthTimeStep[i])+3]) <- reproduction*ihaz[i,BirthTimeStep[i]]
    
      #for(t in (BirthTimeStep[i]+3):MaxSampleTime[i]){
        #log(EB[i,t+1]) <- EB[i,t] + (reproduction*WB[i,t-2])-shedding*EB[i,t]
      #}
    
      for(t in (BirthTimeStep[i])+4:MaxSampleTime[i]){
        EB[i,t+1] <- EB[i,t]+(reproduction*sum(ihaz[i,BirthTimeStep[i]]:ihaz[i,(t-1)]))-shedding*EB[i,t]
      }
    }
    
    #### likelihood ####
    
    for(i in 1:nchildren){
      for(r in 1:inst[i]){ # number of sampling instances for each child
          #kkdata[i,r] ~ dpois(EB[i,Inst[i,r]]) 
          kkdata[i,r] ~ dnegbin(shnb2/(EB[i,Inst[i,r]]+shnb2), shnb2) 
      }
    }
    
    #inits# .RNG.seed, .RNG.name
    #data# nchildren, kkdata, inst, Inst,   MaxSampleTime, BirthTimeStep
    #treatment this will be added back in to data when I have a better idea of when treatment happened. 
    #monitor# h, beta0, beta1, beta2, beta4, haz.precision, sigma, sigma.ind.haz
    # beta3, will be added back in when I know more about trt
    # mortality
  }"
  # run model #
  Results <- run.jags(model, burnin=3000, sample=10000,thin=10, check.conv = TRUE, n.chains=2, jags.refresh = 1, method = 'parallel',
                      plots = F, silent.jags = F)
  return(Results)
  
}


