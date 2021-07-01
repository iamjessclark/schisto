# doesn't take into account that a zero is not always a true not infected 
# doesn't take treatment into account
mod8 <- function(nchildren, Repeats, kkarray, BirthTimeStep, SampleTimeStep, Ttotal){
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

  # population hazard coeffcients and hazard priors out of AR timeloop
  Hazard[1] ~ dnorm(beta0,0.001)
  Hazard[2] ~ dnorm(beta0, 0.001)
  
  h[1] <- exp(Hazard[1])
  h[2] <- exp(Hazard[2])

  beta0 ~ dnorm(0,0.001) #the baseline foi (hazard)
  beta1 ~ dnorm(0,0.001) # linear trend on time 
  beta2 ~ dbeta(1,1) # try beta because it is about whether or not a worm survives and matures...?
  
  # individual hazard variables
  tau.ind.prec <- 1/sigma.ind.haz*sigma.ind.haz
  sigma.ind.haz ~ dunif(0,100)
  
  # kk counts
  #shape ~ dgamma(0.001,0.001)
  
  #### biological process ####
  
  for(t in 3:max(SampleTimeStep)){
  # population level hazard
  # slope (B0), linear trend on time (B1), AR1 term for maturation within the snail (environmental event)
  
    # hazard predictor for the rate of egg accumulation 
    
    Hazard[t] ~ dnorm(Hhat[t], haz.precision)
    Hhat[t] <- beta0 + beta1*t + beta2*Hazard[t-1]
    
    # transform to make into a positive rate 
    h[t] <- exp(Hazard[t])
  }
  
  # individual level hazard 
  
  for(i in 1:nchildren){
    for(t in BirthTimeStep[i]:SampleTimeStep[i]){
    
    # I have done this the same way mafalda did, but I am not sure why the precision is added - this is just additional variance at the individual level right?
    # so why has ind.Hazard not just been drawn stochastically? is this because the precision can't be restimated at the ind. level? 
    
      ind.Hazard[i,t] <- indHaz.hat[i,t]+ind.prec[i,t]
      ind.prec[i,t]~ dnorm(0, tau.ind.prec)
      
      # individual level hazard is driven by the FOI two timesteps ago (i.e. accounting for worm maturation in the human host)
      indHaz.hat[i,t] <- h[t-2]
     
      ihaz[i,t] <- exp(ind.Hazard[i,t]) #exponentiate to make a rate
    }
  }
  
  #### Observation Process ####
  
  # modelling egg burden/ accumulation over time for now from birth, this will change to reflect since last treatment. 
  
  for(i in 1:nchildren){
    EB[i,BirthTimeStep[i]] <- ihaz[i,BirthTimeStep[i]] # this is the first time step in the geometricish series
      
      for(t in 1:(Ttotal[i]-1)){                              
        EB[i,BirthTimeStep[i]+t] <- sum((EB[i,(BirthTimeStep[i]+t)-1]),pow(ihaz[i,(BirthTimeStep[i]+t)],t+1))
      }
  }
 
  #### likelihood ####
  
  for(i in 1:nchildren){
    for(t in BirthTimeStep[i]:SampleTimeStep[i]){
      for(r in 1:Repeats[i]){
        #kkarray[i,t,r] ~ dnegbin(shape/(EB[i,t]+shape),shape) # shape/shape+mean=probability dnegbin(prob, shape)
        kkarray[i,t,r] ~ dpois(EB[i,t]) 
      }
    }
  }
  
  #inits# .RNG.seed, .RNG.name
  #data# nchildren, Repeats, kkarray, BirthTimeStep, SampleTimeStep, Ttotal
  #monitor# h, beta0, beta1, beta2, haz.precision, sigma
}"
  # run model #
  Results <- run.jags(model, burnin=3000, sample=10000,thin=10, check.conv = TRUE, n.chains=2, jags.refresh = 1, method = 'parallel',
                      plots = F, silent.jags = F)
  return(Results)
  
}



mod9 <- function(nchildren, Repeats, kkarray, BirthTimeStep, MaxSampleTime, mortality){
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

  # population hazard coeffcients and hazard priors out of AR timeloop
  Hazard[1] ~ dnorm(beta0,0.001)
  Hazard[2] ~ dnorm(beta0, 0.001)
  
  h[1] <- exp(Hazard[1])
  h[2] <- exp(Hazard[2])

  beta0 ~ dnorm(0,0.001) # baseline FOI
  beta1 ~ dnorm(0,0.001) # linear trend on time
  beta2 ~ dbeta(1,1) # try beta because it is about whether or not a worm survives and matures...?
  #beta3 ~ dgamma(0.001, 0.001)
  beta4 ~ dbeta(0,1)
  
  # individual hazard variables
  tau.ind.prec <- 1/sigma.ind.haz*sigma.ind.haz
  sigma.ind.haz ~ dunif(0,100)
  
  # kk counts
  
  shedding  <- 0.2 
  # for now I have fixed this value from moore and sandgroun '56 but it was in hampsters, 
  # not possible to get this data in humans but is the most widely cited value in wrt humans.
  # the experiment lasted three weeks which is the same time as a timestep here. 
   
  reproduction <- 300*21 # 300 eggs a day for three weeks
    
  #### biological process ####
  
  for(t in 3:max(SampleTimeStep)){
  # population level hazard
  # slope (B0), linear trend on time (B1), AR1 term for maturation within the snail (environmental event)
  
    # hazard predictor for the population level rate of worm accumulation 
    
    Hazard[t] ~ dnorm(Hhat[t], haz.precision) # haz.precision is the error term operating at the pop lvel whether this error is needed at the pop level is not clear but if it does need to be there need to be able to explain what kind of process is generating that population level error 
    Hhat[t] <- beta0 + beta1*t + beta2*Hazard[t-1]
  
  # population level treatment - will be added back in when I have a better idea of when this has occurred 
  # - beta3*treatment[t] 
    
  }
  
  # individual level hazard the individual rate of worm accumulation 
  
  for(i in 1:nchildren){
  
    h[i,BirthTimeStep[i]] <- 0
    h[i,BirthTimeStep[i]+1] <- 0
    
    for(t in BirthTimeStep[i]+2:MaxSampleTime[i]){
    
      log(h[i,t]) <- Hazard[t-2]+ind.prec[i,t] # this is the individual level term 
      ind.prec[i,t]~ dnorm(0, tau.ind.prec) # this justifies the poisson assumption because it generates some normally distributed error quantified through tau.inc.prec
    
      ihaz[i,t] ~ dpois(h[i,t])  # in the scale of individual worms, it is therefore a count this is new worms 
    }
  }
  
  
  for(i in 1:nchildren){
    WB[i,BirthTimeStep[i]] <- 0 
      for(t in (BirthTimeStep[i]+1):MaxSampleTime[i]) 
        WB[i,t+1] <- (WB[i,t]+ihaz[i,t])-(WB[i,t-2]*(beta4*mortality[i,t])) # the mortality part of this is saying that the mortality will only impact adult worms (ie those that have been around long enough to mature)
                                                                  # we make an assumption juvenile worms can't die as they aren't affected by treatment. 
                                                                  # so it can't be total worm burden that treatment acts on, it has to be from 2 timesteps ago
                                                                  # but this is recursive, so its calculating for the next timestep, so we have WBt-1*mortality will only be the adult worms being killed by treatment
                                                                  
  }  
  
  #worms last time plus the worms acquired this time minus the worms that have died at some rate of mortality, which is small for the most part, but once in a while it will spike because of individual treatment 
  
  #### Observation Process ####
  
  for(i in 1:nchildren){
   
    EB[i,BirthTimeStep[i]] <- 0
    EB[i,(BirthTimeStep[i])+1] <- 0 
    EB[i,(BirthTimeStep[i])+2] <- reproduction*ihaz[i,t-2]
    
    #for(t in 3:Ttotal[i]){
      #EB[i,t+1] <- EB[i,t]+(reproduction*sum(ihaz[i,BirthTimeStep[i]:(t-1))])-shedding*EB[i,t]
    #}
  #}
  
    for(t in (BirthTimeStep[i])+3:MaxSampleTime[i]){
      EB[i,t+1] <- EB[i,t] + (reproduction*WB[i,t-1])-shedding*EB[i,t]
                              # WB[i,t-1] again because it has to be mature worms that are reproducing
    }
  
  #### likelihood ####
  
  for(i in 1:nchildren){
    for(r in 1:inst[i]){ # number of sampling instances for each child
        kkdata[i,r] ~ dpois(EB[i,Inst[i,r]]) 
    }
  }
  
  #inits# .RNG.seed, .RNG.name
  #data# nchildren, kkdata, inst, Inst,  mortality, MaxSampleTime, BirthTimeStep
  #treatment this will be added back in to data when I have a better idea of when treatment happened. 
  #monitor# h, beta0, beta1, beta2, beta3, beta4, haz.precision, sigma 
}"
  # run model #
  Results <- run.jags(model, burnin=3000, sample=10000,thin=10, check.conv = TRUE, n.chains=2, jags.refresh = 1, method = 'parallel',
                      plots = F, silent.jags = F)
  return(Results)
  
}




log(h[i,BirthTimeStep[i]]) <- 0
ind.prec[i,BirthTimeStep[i]]~ dnorm(0, tau.ind.prec) 

log(h[i,BirthTimeStep[i]+1]) <- 0
ind.prec[i,BirthTimeStep[i]+1]~ dnorm(0, tau.ind.prec) 

ihaz[i,BirthTimeStep[i]] ~ dpois(h[i,BirthTimeStep[i]])
ihaz[i,BirthTimeStep[i]+1] ~ dpois(h[i,BirthTimeStep[i]+1])