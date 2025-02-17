
LongModHazard <- function(nchildren
                          , kkdata
                          , Inst
                          , inst
                          , BirthTimeStep
                          , MaxSampleTime
                          , treatment
                          , MDAtimesteps
                          , MDAtime
                          , instNAs
                          , trtMatNAs
                          , prob712
                          , prob1516
){
  
  ## Set seed ##
  .RNG.seed <- function(chain)
    return( switch(chain, "1"= 1, "2"= 2) )
  .RNG.name <- function(chain)
    return( switch(chain, "1" = "base::Super-Duper", "2" = "base::Wichmann-Hill") )
  
  ihaz <- FillMatrix(matrix(NA, nrow=nchildren, ncol=max(MaxSampleTime)), BirthTimeStep, MaxSampleTime, 1)
  Hazard <- FillVector(vector(length=max(MaxSampleTime)), MaxSampleTime)
  WB <- FillMatrix(matrix(NA, nrow=nchildren, ncol=max(MaxSampleTime)), BirthTimeStep, MaxSampleTime, 1)
  EB <- FillMatrix(matrix(NA, nrow=nchildren, ncol=max(MaxSampleTime)), BirthTimeStep, MaxSampleTime, 10)
  
  model <- "model {
    
    # priors 
    
    # hazards ####
    haz.precision <- 1/(sigma*sigma)
    sigma ~ dunif(0,100)
  
    # ihaz rtnb ####
    nbsize ~ dgamma(1, 0.01)
    
    # coefficients ####
    beta0 ~ dgamma(1, 0.01) # intercept - population baseline hazard
    beta1 ~ dnorm(0,10^-6) # Hazard beta
    beta2 ~ dnorm(0,10^-6)  # population level treatment effect
    beta3 ~ dnorm(0,10^-6)  # individual level treatment (can test whether working well try making + treatmenteffect and set the prior as dnorm)
    
    # individual hazard variables ####
     tau.ind.prec <- 1/sigma.ind.haz*sigma.ind.haz
     sigma.ind.haz ~ dunif(0,100)
    
    shedding <- 0.2 #doi:10.1016/j.molbiopara.2020.111322.
  
    #size ~ dgamma(43.07566, 58.88828) # from previous LCA but for now just trying to get model to run! 
    size ~ dgamma(1, 0.01)
    
    # MDA population level treatment estimating unknown treatment timesteps ####
    # 2007 - 2012 - we now that treatment has happened in these years, and should have happened once each year
    # so allow the model to estimate when this happened
    for(t in (MDAtimesteps[3]+1):(MDAtimesteps[4]-1)){
      MDAtime[t] ~ dbern(prob712)
    }

    # 2015-2016
    for(i in (MDAtimesteps[5]+1):(MDAtimesteps[6]-1)){
      MDAtime[i] ~ dbern(prob1516)
    }

    # make sure that estimated MDA timestep lines up with individual-level treatment weighted by the covereage according to MoH data.
    for(i in 1:nchildren){
      for(t in 1:instNAs[i]){
        treatment[i,trtMatNAs[i,t]] ~ dbern(0.85*MDAtime[trtMatNAs[i,t]])
      }
    }

    #### biological process ####
    
    ## population level hazard ##
    Hazard[1] ~ dpois(beta0)
      for(t in 2:max(MaxSampleTime)){
        Hazard[t] ~ dpois(Hhat[t]) 
        log(Hhat[t]) <- beta0  + beta1*Hazard[t-1] + beta2*MDAtime[t]
      }
      
    ## individual level hazard ##
    
    for(i in 1:nchildren){
      for(t in (BirthTimeStep[i]):MaxSampleTime[i]){
        log(h[i,t]) <- Hazard[t] +ind.prec[i,t] 
        ind.prec[i,t] ~ dnorm(0, tau.ind.prec) 
        ihaz[i,t] ~ dnegbin(nbsize/(h[i,t]+nbsize), nbsize)
      }
    }
    
    ## individual worm burden at each timestep ##
    for(i in 1:nchildren){

     log(WBhat[i,BirthTimeStep[i]]) <- 0.001
     log(WBhat[i,BirthTimeStep[i]+1]) <- ihaz[i,BirthTimeStep[i]+1]+0.001
     
      for(t in (BirthTimeStep[i]+2):MaxSampleTime[i]){
          log(WBhat[i,t]) <- WBhat[i,t-1] + ihaz[i,t] + beta3*treatment[i,t-1] - (WBhat[i,t]*(0.0004807692*21)) #  1/death rate = lifespan, av lifespan= 5.7 years (fulford paper also used in NTDMC models) 5.7 years = 2080 days, 1/2080 days = 0.0004807692 * 21 for three weeks of daily death rate
             WB[i,t] ~ dpois(WBhat[i,t])
        }
    }
    
    #### Observation Process ####
  
    for(i in 1:nchildren){

      log(EBhat[i,BirthTimeStep[i]]) <- 0.0001
      log(EBhat[i,(BirthTimeStep[i]+1)]) <- 0.0001

      for(t in (BirthTimeStep[i]+2):MaxSampleTime[i]){
        log(EBhat[i,t]) <- EBhat[i,t-1] - shedding*EBhat[i,t-1] + ((300*21)*(WB[i,t-2]/2)) #+ (35.67*(((0.00001+WB[i,t-2])/2)^-0.2980)) #
        EB[i,t] ~ dpois(EBhat[i,t])
      }
    }
    
    #### likelihood ####
    
    for(i in 1:nchildren){
      for(r in 1:inst[i]){ 
          
          kkdata[i,r] ~ dnegbin(size/(((shedding*EBhat[i,Inst[i,r]])/21)+size), size) 
      }
    }
 
    #inits# .RNG.seed, .RNG.name, ihaz, Hazard
    #data# nchildren, kkdata, Inst, inst, MaxSampleTime, BirthTimeStep, treatment, MDAtime, MDAtimesteps, prob712, prob1516,instNAs, trtMatNAs
    #monitor# beta0, beta1, beta2, beta3, nbsize, size, Hazard
  }"
  
  
  
  
  # run model #
  Results <- run.jags(model, burnin=1000, sample=1000, check.conv = TRUE, n.chains=2, jags.refresh = 1, method = 'parallel',
                      plots = F, silent.jags = F)
  return(Results)
  
}



FillMatrix <- function(matrix, BirthTimeStep, MaxSampleTime, number){
  for(i in 1:nrow(matrix)){
    matrix[i, (BirthTimeStep[i]:MaxSampleTime[i])] <- number
  }
  return(matrix)
}

FillVector <- function(vector, MaxSampleTime){
  for(i in 1:max(MaxSampleTime)){
    vector[i] <- 10
  }
  return(vector)
}


