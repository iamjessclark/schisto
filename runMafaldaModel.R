
#### Requirements ####
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(runjags)){
  install.packages("runjags")
  library(runjags)
}

#### source files ####
load("schistoEnvironment.RData") # don't worry about the warning that comes up it's been checked. 
source("hazard model mafalda.R")

#### Run Model ####
start.time <- Sys.time()
runModelbugHazard <- LongModHazard(nchildren = nrow(InstCIDsbug)
                                   , kkdata = kkdatabug
                                   , Inst = Instbug
                                   , inst = instbug
                                   , BirthTimeStep = birthtimestepbug
                                   , MaxSampleTime = MaxSampleTimebug
                                   , treatment = TreatmentMatrix
                                   , MDAtimesteps = mdasbug
                                   , prob712 = prob712
                                   , prob1516 = prob1516
                                   , MDAtime = MDAtimes01bug
                                   , instNAs = instNAsbug
                                   , trtMatNAs = trtMatNAsbug
)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#### posteriors ####
# Chains ####
# done this way because my computer can't handle the two chains as one object for some reason. 
hazardchain1 <- as.data.frame(as.matrix(runModelbugHazard$mcmc[[1]][,c(which(colnames(runModelbugHazard$mcmc[[1]])=="Hazard[1]"):which(colnames(runModelbugHazard$mcmc[[1]])=="Hazard[430]"))]))
colnames(hazardchain1) <- 1:ncol(hazardchain1)
hazardchain1$chain <- 1

hazardchain2 <- as.data.frame(as.matrix(runModelbugHazard$mcmc[[1]][,c(which(colnames(runModelbugHazard$mcmc[[2]])=="Hazard[1]"):which(colnames(runModelbugHazard$mcmc[[2]])=="Hazard[430]"))]))
colnames(hazardchain2) <- 1:ncol(hazardchain2)
hazardchain2$chain <- 2

betahaz0 <- density(c(runModelbugHazard$mcmc[[1]][,"beta0"],runModelbugHazard$mcmc[[2]][,"beta0"])) # intercept
betahaz1 <- density(c(runModelbugHazard$mcmc[[1]][,"beta1"],runModelbugHazard$mcmc[[2]][,"beta1"])) # hazard coefficient
betahaz2 <- density(c(runModelbugHazard$mcmc[[1]][,"beta2"],runModelbugHazard$mcmc[[2]][,"beta2"])) # individual level trt
size <- density(c(runModelbugHazard$mcmc[[1]][,"size"],runModelbugHazard$mcmc[[2]][,"size"])) # variation in kk counts

#### plots ####
# Hazard ####
Hazard <- bind_rows(hazardchain1, hazardchain2)%>% 
  pivot_longer(-chain, names_to = "time", values_to = "hazard")

Hazard <- Hazard %>%
  group_by(time, chain) %>%
  mutate(hazard = round(hazard,2),
         qt_0025 = round(quantile(hazard, 0.025),2),
         qt_50 = round(quantile(hazard, 0.5),2),
         qt_75 = round(quantile(hazard, 0.75),2), 
         qt_975 = round(quantile(hazard, 0.975),2),
         time = as.numeric(time),
         time = (time*3)/52.1429) 
Hazard %>%
  ggplot()+
  geom_ribbon(aes(x = time, ymin = qt_0025, ymax = qt_975), fill="orange", alpha = 0.2)+
  geom_line(aes(x = time, y = qt_50))+
  theme_bw()

# Beta0 ####
range(betahaz0$x)
plot(NA, NA, xlim=c(-2,0), ylim = c(0,1), xlab="", ylab="")
lines(x = betahaz0$x, y = betahaz0$y/max(betahaz0$y))
polygon(c(rev(betahaz0$x), betahaz0$x), c(rev(betahaz0$y/max(betahaz0$y)), rep(0,length(betahaz0$y))),
        col = adjustcolor("#7570b3", alpha=.2), border = NA)
mtext("Scaled density",side=2,cex=1,line=2)
mtext("intercept",side=1,cex=1,line=3)


# betahaz1 ####
plot(NA, NA, xlim=c(0, 1), ylim = c(0,1), xlab="", ylab="")
lines(x = betahaz1$x, y = betahaz1$y/max(betahaz1$y))
polygon(c(rev(betahaz1$x), betahaz1$x), c(rev(betahaz1$y/max(betahaz1$y)), rep(0,length(betahaz1$y))),
        col = adjustcolor("turquoise", alpha=.2), border = NA)
mtext("Scaled density",side=2,cex=1,line=2)
mtext("hazard coefficient",side=1,cex=1,line=3)


# betahaz2 ####
plot(NA, NA, xlim=c(0, 1), ylim = c(0,1), xlab="", ylab="")
lines(x = betahaz2$x, y = betahaz2$y/max(betahaz2$y))
polygon(c(rev(betahaz2$x), betahaz2$x), c(rev(betahaz2$y/max(betahaz2$y)), rep(0,length(betahaz2$y))),
        col = adjustcolor("pink", alpha=.2), border = NA)
mtext("Scaled density",side=2,cex=1,line=2)
mtext("individual level treatment",side=1,cex=1,line=3)

# size ####
plot(NA, NA, xlim=c(0, 1), ylim = c(0,1), xlab="", ylab="")
lines(x = size$x, y = size$y/max(size$y))
polygon(c(rev(size$x), size$x), c(rev(size$y/max(size$y)), rep(0,length(size$y))),
        col = adjustcolor("orange", alpha=.2), border = NA)
mtext("Scaled density",side=2,cex=1,line=2)
mtext("pop level trt",side=1,cex=1,line=3)


