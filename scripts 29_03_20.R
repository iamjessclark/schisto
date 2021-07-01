#### Schistosomiasis Longitudinal Study ####
# University of Glasgow #
# PI Poppy Lamberton #
# I.P. Dr. Jessica Clark #
# March 2020 #

#### Packages ####

require(fitdistrplus)
require(reshape2)
require(tidyverse)
require(lme4)
require(performance)
require(pbkrtest)
require(RColorBrewer)
require(scales)

#### Data ####

source("longitudinal_functions2.R")

# 2004 - 2014 #

long_data <- read_csv("Clean long data 2015-07-09.csv")

long_data <- long_data_func(long_data)


long_data$Month <- factor(long_data$Month, levels = c("Baseline 2004", "July 04 1wk PT", "Aug 4wk PT", "Feb 2005", "Feb 6mth 1wk", 
                                                                          "June 2005", "July 1wk PT", "July 05 4wk PT", "May 2006", "June 1wk PT", "July 06 4wk PT", "Feb 2013", 
                                                                          "Feb 1wk PT","March 3wk PT", "May 2014",  "June 4wk PT" ,"March 2017", "Sept 2017", 
                                                                          "Oct 3wk PT", "Dec 9wk PT", "Feb 2018", "March 2018", "March 2018 3wk PT" ))
baseline_2004 <- long_data %>% filter(RecruitYr=="2004", Year=="2004") %>%
  filter(School=="Musubi" | School=="Bugoto" & AgeAtRecruit=="06"| School=="Bwondha" & AgeAtRecruit=="06")%>%
  mutate_if(is.character, as.factor) 

baseline_2004$MeanSm <- as.integer(baseline_2004$MeanSm)
baseline_2004 <- baseline_2004[-which(is.na(baseline_2004$MeanSm==T)),]
baseline_2004$prevalence <- NA

for(i in 1:nrow(baseline_2004)){
  if(baseline_2004[i,33]>0){
    baseline_2004[i,ncol(baseline_2004)] <- 1
  } else { 
    baseline_2004[i,ncol(baseline_2004)] <- 0
    }
}

baseline_2004$prevalence <- as.factor(baseline_2004$prevalence)
#### Modelling baseline prevalence and infection intensity ####

# According to jarrod's notes there are two types of models you can run here
# as this is count data you would use a NB model or a  poisson. 
# poisson is likely to be overdispersed however putting in a obs level RE can take care of that
# A NB model w/o a RE should be a similar output but I am not sure it is entirely correct 
# if the residual deviance is not so large in comparison to a small resid DF then it is not so od.
# trying poisson + RE, NB and NB + RE

bl_data <- baseline_2004
bl_data <- bl_data[-which(is.na(bl_data$MeanSm)==T),]
bl_data$MeanSm <- as.numeric(bl_data$MeanSm)

bl_int_dist_pois <- fitdist(bl_data$MeanSm, "pois", method="mle")
bl_int_dist_nb <- fitdist(bl_data$MeanSm, "nbinom")

cdfcomp(list(bl_int_dist_nb, bl_int_dist_pois), xlogscale = FALSE, ylogscale = FALSE, legendtext = c("Negative Binomial", "Poisson"))


#### Poisson model ####
bl_int_pois <- glmer(MeanSm ~ School + (1|CID), data=baseline_2004, family = poisson)
summary(bl_int_pois)

bl_int_sim <- baseline_2004 %>% select(MeanSm, School, CID) %>% mutate_if(is.character, is.factor)
sim_indiv <- bl_int_sim
sim_indiv$CID <- as.factor(paste("sim", rownames(sim_indiv), sep=""))
sim_indiv$MeanSm <- NA
bl_int_sim <- bind_rows(bl_int_sim, sim_indiv) %>% mutate_if(is.character, as.factor)

bl_int_sim$simulated <- simulate(bl_int_pois, seed=1, newdata=bl_int_sim[-1], re.form=NA, allow.new.levels=T)$sim_1

no.zeros.pois <- sum(bl_int_sim$simulated<1)

bl_sim_errorbars <- bl_int_sim %>% group_by(School) %>%
  summarise(mean_eggs=mean(MeanSm, na.rm = T), n=n(), sd_data=sd(MeanSm, na.rm=T), std_error_data=sd_data/sqrt(n), 
            mean_eggs_sim=mean(simulated, na.rm = T), n=n(), sd_sim=sd(simulated, na.rm=T), std_error_sim=sd_sim/sqrt(n), )

sim_plot_int_pois <- ggplot(data=bl_sim_errorbars) +
                        geom_point(aes(y=mean_eggs_sim, x=School), colour="#F6AE2D", size=4)+
                        geom_point(aes(y=mean_eggs, x=School), colour="red", size=4)+
                        geom_errorbar(aes(x=School, ymin=mean_eggs- std_error_data, ymax=mean_eggs+std_error_data),colour="#2F4858", width=.2)+
                        geom_errorbar(aes(x=School, ymin=mean_eggs_sim- std_error_sim, ymax=mean_eggs_sim+std_error_sim),colour="#2F4858", width=.2)+
                        theme(axis.text.x = element_text(angle=90))+
                        facet_grid(~School, scales = "free_x")+
                        ylab("Mean Eggs Per Slide")+ xlab("School")+
                        ggsave("poisson model fit.pdf")

#### Negative binomial model no RE ####
bl_int_nb <- glm.nb(MeanSm ~ School, data = baseline_2004)
summary(bl_int_nb)

bl_int_sim_nb <- baseline_2004 %>% select(MeanSm, School, CID) %>% mutate_if(is.character, is.factor)
bl_int_sim_nb$CID <- as.factor(paste("sim", rownames(bl_int_sim_nb), sep=""))

bl_int_sim_nb$simulated <- simulate(bl_int_nb, seed=1, newdata=bl_int_sim_nb[-1], re.form=NA,
                                    allow.new.levels=T)$sim_1

no.zeros.nb <- sum(bl_int_sim_nb$simulated<1)

bl_sim_nb <- bl_int_sim_nb %>% group_by(School) %>%
  summarise(mean_eggs=mean(MeanSm, na.rm = T), n=n(), sd_data=sd(MeanSm, na.rm=T), std_error_data=sd_data/sqrt(n), 
            mean_eggs_sim=mean(simulated, na.rm = T), n=n(), sd_sim=sd(simulated, na.rm=T), std_error_sim=sd_sim/sqrt(n), )

sim_plot_int_nb <- ggplot(data=bl_sim_nb) +
  geom_point(aes(y=mean_eggs_sim, x=School), colour="#F6AE2D", size=4)+
  geom_point(aes(y=mean_eggs, x=School), colour="red", size=4)+
  geom_errorbar(aes(x=School, ymin=mean_eggs- std_error_data, ymax=mean_eggs+std_error_data),colour="#2F4858", width=.2)+
  geom_errorbar(aes(x=School, ymin=mean_eggs_sim- std_error_sim, ymax=mean_eggs_sim+std_error_sim),colour="#2F4858", width=.2)+
  theme(axis.text.x = element_text(angle=90))+
  facet_grid(~School, scales = "free_x")+
  ylab("Mean Eggs Per Slide")+ xlab("School")+
  ggsave("nb model fit.pdf")

#### Negative binomial model observation level RE ####
# lme4 package did try with glmmAMDB but I can't figure out how to get predictions from it with the RE whereas lme4 does let you
bl_int_nbre <- glmer.nb(MeanSm ~ relevel(School,ref="Musubi") + (1|CID), data=baseline_2004, control=glmerControl(optimizer="bobyqa",
                                                                                           optCtrl=list(maxfun=10e5)))
summary(bl_int_nbre)

bl_int_sim_nbre <- baseline_2004 %>% select(MeanSm, School, CID) %>% mutate_if(is.character, is.factor)
bl_int_sim_nbre$CID <- as.factor(paste("sim", rownames(bl_int_sim_nbre), sep=""))
bl_int_sim_nbre <- bl_int_sim_nbre[-nrow(bl_int_sim_nbre),]

bl_int_sim_nbre$simulated <- simulate(bl_int_nbre, seed=1, newdata=bl_int_sim_nbre[-1], re.form=NA,
                                    allow.new.levels=T)$sim_1


no.zeros.nbre <- sum(bl_int_sim_nbre$simulated<1)

bl_sim_nbre <- bl_int_sim_nbre %>% group_by(School) %>%
  summarise(mean_eggs=mean(MeanSm, na.rm = T), n=n(), sd_data=sd(MeanSm, na.rm=T), std_error_data=sd_data/sqrt(n), 
            mean_eggs_sim=mean(simulated, na.rm = T), n=n(), sd_sim=sd(simulated, na.rm=T), std_error_sim=sd_sim/sqrt(n), )

colours <- c("data" = "red", "model" = "#F6AE2D")

sim_plot_int_nbre <- ggplot(data=bl_sim_nbre) +
  geom_point(aes(y=mean_eggs_sim, x=School), colour="#F6AE2D", size=4)+
  geom_point(aes(y=mean_eggs, x=School), colour="red", size=4)+
  geom_errorbar(aes(x=School, ymin=mean_eggs- std_error_data, ymax=mean_eggs+std_error_data),colour="#2F4858", width=.2)+
  geom_errorbar(aes(x=School, ymin=mean_eggs_sim- std_error_sim, ymax=mean_eggs_sim+std_error_sim),colour="#2F4858", width=.2)+
  theme(axis.text.x = element_text(angle=90))+
  facet_grid(~School, scales = "free_x")+
  labs(y="Mean Eggs Per Slide", x="School", color="legend")+scale_color_manual(values=colours)+
  ggsave("nbre model fit.pdf")

# model comes up with a conconvergence error but a different test that changes the limits and is suppoerted by ben bolker says it is fine
# see the document details for lme4 for more info on convergence 

check_convergence(bl_int_nbre) # convergence is fine 

#### Infection prevalence baseline 2004 ####

prev_2004 <- glmer(prevalence ~ School + (1|CID), data=baseline_2004, family=binomial(link='logit'))

summary(prev_2004)

baseline_2004%>% 
  count(School, prevalence) %>% 
  group_by(School) %>% 
  mutate(sum=sum(n)) %>% 
  mutate(proportion = n/sum) %>% 
  ggplot(aes(y=proportion, x=School)) +
  geom_col(position = "dodge", colour="#2F4858", fill="#F6AE2D")+
  theme(axis.text.x = element_text(angle=360))+
  ylab("Infection Prevalence")+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  ggsave("prevalence_pzq_naive_2004.pdf")

#### Baseline age/ intensity distribution ####

# can't fit a model to this data, I think the dataset is too small            

errorbars <- baseline_2004 %>% group_by(School) %>%
  summarise(mean_eggs=mean(MeanSm, na.rm = T), n=n(), sd_data=sd(MeanSm, na.rm=T), std_error_data=sd_data/sqrt(n))

int_age_2004 <- ggplot()+
  geom_count(data=baseline_2004, aes(x=School, y=MeanSm, colour=AGE),alpha=0.5)+
  geom_errorbar(data = errorbars,aes(x=School,ymin=mean_eggs- std_error_data, ymax=mean_eggs+std_error_data), width = 0.05,inherit.aes = FALSE, position=position_dodge(w=0.75))+
    labs(x = "School",
         y = "Mean Eggs Per Slide",
         size = "Number of Children")+
    scale_color_brewer(palette = "Dark2")+
    scale_size_continuous(range = c(3, 10))+
    scale_y_continuous(breaks=seq(0,3000,500), limits=c(0, 3000))+
  ggsave("age_split_baseline_int.pdf")

# no effect of age in baseline data on infection prevalence in musubi #

musubi_baseline <- baseline_2004 %>% filter(School=="Musubi")
musubi_baseline$AGE <- as.factor(musubi_baseline$AGE)

age_mus_bl <- glmer(prevalence ~ relevel(AGE, ref="12") + (1|CID), data=musubi_baseline, family=binomial(link='logit'))
summary(age_mus_bl)

#### long data pzq naive 6 year olds ####

# do the same analysis with just the six yearolds 

pzq_naive_6yo <- long_data %>% filter(AgeAtRecruit =="06" & RecruitYr !="2003" & Week == "0" & AGE == "06") %>%
  select(CID, School, Month, event, MeanSm)
pzq_naive_6yo$MeanSm <- as.integer(pzq_naive_6yo$MeanSm)
pzq_naive_6yo$prevalence <- "NA"

pzq_naive_6yo <- pzq_naive_6yo[-which(is.na(pzq_naive_6yo$MeanSm)==T),]

for(r in 1:nrow(pzq_naive_6yo)){
  if(pzq_naive_6yo[r,5]>0){
    pzq_naive_6yo[r,ncol(pzq_naive_6yo)] <- 1
  } else{
    pzq_naive_6yo[r,ncol(pzq_naive_6yo)] <- 0
  }
}

pzq_naive_6yo$prevalence <- as.factor(pzq_naive_6yo$prevalence)
pzq_naive_6yo <- pzq_naive_6yo %>% rename(`mean eggs per slide`= MeanSm)%>% rename(cid=CID)

# 2017 - 2018 #

KK.bug.BLT <- read_csv("kk_bg_17_03_13_clean.csv") # march 2017
KK.bug.6mT.2017 <- read_csv("kk_bg_17_09_25_clean.csv") # Sept 2017
KK.bug.6m3wk <- read_csv("kk_bg_17_10_25_clean.csv") # Oct 2017
KK.bug.6m9wk <- read_csv("kk_bg_17_12_04_clean.csv") # Dec 2017
KK.bug.6mT2018 <- read_csv("kk_bg_18_03_05_clean.csv") # Mar 2018
KK.bug.6m3wk2018 <- read_csv("kk_bg_18_03_27_clean.csv") # Mar 2018
KK.bwo.BLT <- read_csv("kk_bwondha_2017.09.20_clean.csv") # Sept 2017
KK.bwo.4mT <- read_csv("kk_bwondha_2018.02.21_clean.csv") # Feb 2018 6 monthsish
KK.bwo.3wk <- read_csv("kk_bwondha_2018.03.14_clean.csv") # Mar 2018
KK.mus.BLT <- read_csv("kk_musubi_2017.10.10_clean.csv") # Sep 2017
KK.mus.6mT <- read_csv("kk_musubi_2018.02.28_clean.csv") # Feb/ March 2018 
KK.mus.3wk <- read_csv("kk_musubi_2018.03.26_clean.csv") # Mar 2018



data_2017_2018 <- recent_data(KK.bug.BLT, KK.bug.6mT.2017, KK.bug.6m3wk, KK.bug.6m9wk, KK.bug.6mT2018, KK.bug.6m3wk2018, 
                              KK.bwo.BLT, KK.bwo.4mT, KK.bwo.3wk, KK.mus.BLT, KK.mus.6mT, KK.mus.3wk)

# removing all NAs from the counts
data_2017_2018 <- data_2017_2018[-which(is.na(data_2017_2018$mean.eps)==T),]

# subsetting the 17/18 data to get just the pzq naive 6yo
pzq_naive_17_18 <- data_2017_2018[grep("1706",data_2017_2018$cid),]
pzq_naive_17_18 <- subset(pzq_naive_17_18, `Weeks Since Treatment`=="Baseline")

pzq_naive_17_18 <- pzq_naive_17_18 %>% group_by(cid, School, Month,  event) %>%
  summarise(`mean eggs per slide`=mean(mean.eps))

pzq_naive_17_18$prevalence <- "NA"

for(r in 1:nrow(pzq_naive_17_18)){
  if(pzq_naive_17_18[r,5]>0){
    pzq_naive_17_18[r,ncol(pzq_naive_17_18)] <- 1
  } else{
    pzq_naive_17_18[r,ncol(pzq_naive_17_18)] <- 0
  }
}
pzq_naive_17_18$prevalence <- as.factor(pzq_naive_17_18$prevalence)
pzq_naive_17_18$`mean eggs per slide` <- as.integer(pzq_naive_17_18$`mean eggs per slide`)

#### Full pzq naive 6 yearolds ####

pzq_naive_full <- bind_rows(pzq_naive_6yo, pzq_naive_17_18) 
pzq_naive_full$Month <- factor(pzq_naive_full$Month, levels = c("Baseline 2004", "June 2005", "May 2006", "Feb 2013", 
                                                                "May 2014", "March 2017", "Sept 2017", 
                                                                "Feb 2018", "March 2018" ))


#### data with all ages for musubi in 2004 #### 
musubi04 <- musubi_baseline %>% filter(Week=="0") %>% 
  select(CID, School, Month, event, MeanSm)%>%
  rename(cid="CID", `mean eggs per slide`="MeanSm") 
musubi04$prevalence <- NA
for(i in 1:nrow(musubi04)){
  if(musubi04[i,5]>0){
    musubi04[i,ncol(musubi04)] <- 1
  } else{
    musubi04[i,ncol(musubi04)] <- 0
  }
}
musubi04$prevalence <- as.factor(musubi04$prevalence)

pzq_naive <- bind_rows(musubi04, pzq_naive_6yo)

pzq_naive_all <- bind_rows(pzq_naive, pzq_naive_17_18)
pzq_naive_all <- pzq_naive_all %>% mutate_if(is.character, as.factor)
pzq_naive_all$Month <- factor(pzq_naive_all$Month, levels = c("Baseline 2004", "June 2005", "May 2006", "Feb 2013", 
                                                                "May 2014", "March 2017", "Sept 2017"))

write.csv(pzq_naive_all, "pzq.naive.each.yr.csv")
#### Across years ####

#### Figures ####

# infection prevalence plot #
# DONT TRUST THESE TWO FIGURES!!!!!!!! #
#pzq_naive_full%>% 
  #mutate_if(is.character, as.factor)%>%
  #count(Month, School, prevalence) %>% 
  #group_by(Month) %>% 
  #mutate(sum=sum(n)) %>% 
  #mutate(proportion = n/sum) %>% 
  #ggplot(aes(y=proportion, x=Month, fill=School)) +
  #geom_col(position = "dodge", fill="#F6AE2D")+
  #theme(axis.text.x = element_text(angle=90))+
  #facet_grid(~School, scales = "free_x")+
  #ylab("Infection Prevalence")+
  #ggsave("prevalence_pzq_naive_6yo.pdf")


# Infection intensity plot #

#pzq.errorbars <- pzq_naive_full %>% group_by(School, Month) %>%
  #summarise(mean_eggs=mean(`mean eggs per slide`, na.rm = T), n=n(), sd=sd(`mean eggs per slide`, na.rm=T), std.error=sd/sqrt(n))

#pzq.errorbars%>% 
  #ggplot(aes(y=mean_eggs, x=Month, group=School)) +
  #geom_point(colour="#F6AE2D", size=4)+
  #geom_line(colour="#2F4858")+
  #geom_errorbar(aes(ymin=mean_eggs- std.error, ymax=mean_eggs+std.error),colour="#2F4858", width=.2)+
  #theme(axis.text.x = element_text(angle=90))+
  #facet_grid(~School, scales = "free_x")+
  #ylab("Mean Eggs Per Slide")+ xlab("Sampling Time")+
  #ggsave("intensity_pzq_naive_6yo.pdf")

# infection prevalence plot ALL AGES in 2004#
# trust these #

pzq_naive_all %>%
  group_by(School, Month, prevalence) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))%>%
  filter(prevalence=="1")%>%
  ggplot(aes(x=Month, y=freq))+
  geom_col(position = "dodge", fill="#F6AE2D", colour="#2F4858")+
  theme(axis.text.x = element_text(angle=90))+
  facet_grid(~School, scales = "free_x")+
  ylab("Infection Prevalence")+
  ggsave("prev_naive_all_ages.pdf")

# just bugoto for  ASTMH talk
pzq_naive_all %>%
  group_by(School, Month, prevalence) %>%
  filter(School=="Bugoto") %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))%>%
  filter(prevalence=="1")%>%
  ggplot(aes(x=Month, y=freq))+
  geom_col(position = "dodge", fill="#F6AE2D", colour="#2F4858")+
  theme(#axis.text.x = element_text(angle=90), 
        axis.title.x = element_text(colour="white", size = 16), 
        axis.title.y = element_text(colour="white", size = 16),
        axis.text.x = element_text(colour = "white", size = 14),
        axis.text.y = element_text(colour="white", size = 14), 
        axis.ticks.x = element_line(colour="white"),
        axis.ticks.y = element_line(colour="white"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA))+
  ylab("Infection Prevalence")+
  ggsave("prev_naive_bug.pdf")

# Infection intensity plot ALL AGES in 2004#

pzq.errorbars <- pzq_naive_all %>% group_by(School, Month) %>%
  summarise(mean_eggs=mean(`mean eggs per slide`, na.rm = T), n=n(), sd=sd(`mean eggs per slide`, na.rm=T), std.error=sd/sqrt(n))

pzq.errorbars%>% 
  ggplot(aes(y=mean_eggs, x=Month, group=School)) +
  geom_point(colour="#F6AE2D", size=4)+
  geom_line(colour="#2F4858")+
  geom_errorbar(aes(ymin=mean_eggs- std.error, ymax=mean_eggs+std.error),colour="#2F4858", width=.2)+
  theme(axis.text.x = element_text(angle=90))+
  facet_grid(~School, scales = "free_x")+
  ylab("Mean Eggs Per Slide")+ xlab("Sampling Time")+
  ggsave("intensity_pzq_naive_all.pdf")


#### Egg reduction ####

err_long_data <- long_data %>% filter(Month == "Baseline 2004"| Month =="Aug 4wk PT"| Month =="June 2005" |Month == "July 05 4wk PT" | Month == "May 2006" |Month == "July 06 4wk PT" | Month == "Feb 2013" |Month == "March 3wk PT" |Month =="May 2014" | Month =="June 4wk PT" )
err_long_data$Week <- factor(err_long_data$Week)
err_long_data <- err_long_data %>% filter(Week !="26")%>%
  select(CID, AGE, SEX, School, Year, Week, Month, MeanSm)%>%
  mutate_if(is.character, as.factor)
err_long_data$Week <- factor(err_long_data$Week)
err_long_data$Year <- as.factor(err_long_data$Year)
err_long_data$MeanSm <- as.integer(err_long_data$MeanSm)
err_long_data <- as.data.frame(err_long_data)
err_long_data2 <- err_long_data %>% select(CID, AGE, SEX, School, Month, MeanSm)
err_long_data2 <- as.data.frame(err_long_data2)
err_long_data2$Month <- factor(err_long_data2$Month)
#err_long_data <- err_long_data %>% pivot_wider(names_from = Week, values_from = MeanSm)
err_long_data2 <- as.data.frame(reshape(err_long_data2, idvar = c("CID", "School", "AGE", "SEX"), timevar = "Month", v.names = "MeanSm", direction = "wide")) #object has to be a dataframe not a tibble for this to work 

#### Long Data ####
# not using any children who don't have both counts
err_trt_2004 <- as.data.frame(cbind(as.character(err_long_data2$CID),as.character(err_long_data2$AGE),as.character(err_long_data2$SEX), err_long_data2$`MeanSm.Baseline 2004`, err_long_data2$`MeanSm.Aug 4wk PT`))
err_trt_2004 <- err_trt_2004 %>% mutate_if(is.character, as.factor)%>% rename(cid="V1", `age`="V2",`sex`="V3",`baseline 2004`="V4", `Aug 4wk PT`="V5")
err_trt_2004 <- err_trt_2004[-which(is.na(err_trt_2004$`baseline 2004`)==T),]
err_trt_2004 <- err_trt_2004[-which(is.na(err_trt_2004$`Aug 4wk PT`)==T),]
err_trt_2004 <- school_relabel(err_trt_2004)
err_trt_2004$`baseline 2004` <- as.numeric(err_trt_2004$`baseline 2004`)
err_trt_2004$`Aug 4wk PT` <- as.numeric(err_trt_2004$`Aug 4wk PT`)
err_trt_2004$ERR <- "NA"
err_trt_2004 <- err_func(err_trt_2004, err_trt_2004$`baseline 2004`, err_trt_2004$`Aug 4wk PT`)

for(i in 1:nrow(err_trt_2004)){
  if(err_trt_2004[i,ncol(err_trt_2004)]<(-100)){
    err_trt_2004[i,ncol(err_trt_2004)] <-(-100)
  }else(
    err_trt_2004[i,ncol(err_trt_2004)] <-  err_trt_2004[i,ncol(err_trt_2004)]
  )
}
err_trt_2004$time <- as.factor("`Baseline to Aug 4wk PT`")
err_trt_2004 <- err_trt_2004 %>% select(cid, age, sex, School, time, ERR)

err_trt_2005 <- as.data.frame(cbind(as.character(err_long_data2$CID), as.character(err_long_data2$AGE),as.character(err_long_data2$SEX), err_long_data2$`MeanSm.June 2005`, err_long_data2$`MeanSm.July 05 4wk PT`))
err_trt_2005 <- err_trt_2005 %>% mutate_if(is.character, as.factor)%>% rename(cid="V1", age="V2", sex="V3",`June 2005`="V4", `July 05 4wk PT`="V5")
err_trt_2005 <- err_trt_2005[-which(is.na(err_trt_2005$`June 2005`)==T),]
err_trt_2005 <- err_trt_2005[-which(is.na(err_trt_2005$`July 05 4wk PT`)==T),]
err_trt_2005 <- school_relabel(err_trt_2005)
err_trt_2005$`June 2005` <- as.numeric(err_trt_2005$`June 2005`)
err_trt_2005$`July 05 4wk PT` <- as.numeric(err_trt_2005$`July 05 4wk PT`)
err_trt_2005$ERR <- "NA"
err_trt_2005 <- err_func(err_trt_2005, err_trt_2005$`June 2005`, err_trt_2005$`July 05 4wk PT`)

for(i in 1:nrow(err_trt_2005)){
  if(err_trt_2005[i,ncol(err_trt_2005)]<(-100)){
    err_trt_2005[i,ncol(err_trt_2005)] <-(-100)
  }else(
    err_trt_2005[i,ncol(err_trt_2005)] <-  err_trt_2005[i,ncol(err_trt_2005)]
  )
}
err_trt_2005$time <- as.factor("`June 2005 to 4wk PT`")
err_trt_2005 <- err_trt_2005 %>% select(cid, age, sex, School, time, ERR)

err_trt_2006 <- as.data.frame(cbind(as.character(err_long_data2$CID), as.character(err_long_data2$AGE),as.character(err_long_data2$SEX), err_long_data2$`MeanSm.May 2006`, err_long_data2$`MeanSm.July 06 4wk PT`))
err_trt_2006 <- err_trt_2006 %>% mutate_if(is.character, as.factor)%>% rename(cid="V1", age="V2",sex="V3",`May 2006`="V4", `July 06 4wk PT`="V5")
err_trt_2006 <- err_trt_2006[-which(is.na(err_trt_2006$`May 2006`)==T),]
err_trt_2006 <- err_trt_2006[-which(is.na(err_trt_2006$`July 06 4wk PT`)==T),]
err_trt_2006 <- school_relabel(err_trt_2006)
err_trt_2006$`May 2006` <- as.numeric(err_trt_2006$`May 2006`)
err_trt_2006$`July 06 4wk PT` <- as.numeric(err_trt_2006$`July 06 4wk PT`)
err_trt_2006$ERR <- "NA"
err_trt_2006 <- err_func(err_trt_2006, err_trt_2006$`May 2006`, err_trt_2006$`July 06 4wk PT`)

for(i in 1:nrow(err_trt_2006)){
  if(err_trt_2006[i,ncol(err_trt_2006)]<(-100)){
    err_trt_2006[i,ncol(err_trt_2006)] <-(-100)
  }else(
    err_trt_2006[i,ncol(err_trt_2006)] <-  err_trt_2006[i,ncol(err_trt_2006)]
  )
}
err_trt_2006$time <- as.factor("`May 2006 to 4wk PT`")
err_trt_2006 <- err_trt_2006 %>% select(cid, age, sex, School, time, ERR)

err_trt_2013 <- as.data.frame(cbind(as.character(err_long_data2$CID),as.character(err_long_data2$AGE),as.character(err_long_data2$SEX), err_long_data2$`MeanSm.Feb 2013`, err_long_data2$`MeanSm.March 3wk PT`))
err_trt_2013 <- err_trt_2013 %>% mutate_if(is.character, as.factor)%>% rename(cid="V1", age="V2",sex="V3",`Feb 2013`="V4", `March 3wk PT`="V5")
err_trt_2013 <- err_trt_2013[-which(is.na(err_trt_2013$`Feb 2013`)==T),]
err_trt_2013 <- err_trt_2013[-which(is.na(err_trt_2013$`March 3wk PT`)==T),]
err_trt_2013 <- school_relabel(err_trt_2013)
err_trt_2013$`Feb 2013` <- as.numeric(err_trt_2013$`Feb 2013`)
err_trt_2013$`March 3wk PT` <- as.numeric(err_trt_2013$`March 3wk PT`)
err_trt_2013$ERR <- "NA"
err_trt_2013 <- err_func(err_trt_2013, err_trt_2013$`Feb 2013`, err_trt_2013$`March 3wk PT`)

for(i in 1:nrow(err_trt_2013)){
  if(err_trt_2013[i,ncol(err_trt_2013)]<(-100)){
    err_trt_2013[i,ncol(err_trt_2013)] <-(-100)
  }else(
    err_trt_2013[i,ncol(err_trt_2013)] <-  err_trt_2013[i,ncol(err_trt_2013)]
  )
}
err_trt_2013$time <- as.factor("`Feb 2013 to 3wk PT`")
err_trt_2013 <- err_trt_2013 %>% select(cid, age, sex, School, time, ERR)

err_trt_2014 <- as.data.frame(cbind(as.character(err_long_data2$CID),as.character(err_long_data2$AGE),as.character(err_long_data2$SEX), err_long_data2$`MeanSm.May 2014`, err_long_data2$`MeanSm.June 4wk PT`))
err_trt_2014 <- err_trt_2014 %>% mutate_if(is.character, as.factor)%>% rename(cid="V1", age="V2",sex="V3",`May 2014`="V4", `June 4wk PT`="V5")
err_trt_2014 <- err_trt_2014[-which(is.na(err_trt_2014$`May 2014`)==T),]
err_trt_2014 <- err_trt_2014[-which(is.na(err_trt_2014$`June 4wk PT`)==T),]
err_trt_2014 <- school_relabel(err_trt_2014)
err_trt_2014$`May 2014` <- as.numeric(err_trt_2014$`May 2014`)
err_trt_2014$`June 4wk PT` <- as.numeric(err_trt_2014$`June 4wk PT`)
err_trt_2014$ERR <- "NA"
err_trt_2014 <- err_func(err_trt_2014, err_trt_2014$`May 2014`, err_trt_2014$`June 4wk PT`)

for(i in 1:nrow(err_trt_2014)){
  if(err_trt_2014[i,ncol(err_trt_2014)]<(-100)){
    err_trt_2014[i,ncol(err_trt_2014)] <-(-100)
  }else(
    err_trt_2014[i,ncol(err_trt_2014)] <-  err_trt_2014[i,ncol(err_trt_2014)]
  )
}
err_trt_2014$time <- as.factor("`May 2014 to 4wk PT`")
err_trt_2014 <- err_trt_2014 %>% select(cid, age, sex, School, time, ERR)

egg_reduction <- bind_rows(err_trt_2004, err_trt_2005, err_trt_2006, err_trt_2013, err_trt_2014)%>%
  mutate_if(is.character, as.factor)

#### Recent Data ####

bugoto <- subset(data_2017_2018, School == "Bugoto")

bugoto_1st <- bugoto %>% filter(Month=="Sept 2017" | Month=="Oct 3wk PT")%>%
  select(cid, School, Month, mean.eps, SEX)
bugoto_1st$cid <- as.factor(bugoto_1st$cid)
  bugoto_1st <- bugoto_1st %>% group_by(cid, School, Month, SEX) %>%
  summarise(mean.eps=mean(mean.eps))
bugoto_1st <- as.data.frame(bugoto_1st)
bugoto_1st <- as.data.frame(reshape(bugoto_1st, idvar = c("cid", "School", "SEX"), timevar = "Month", v.names = "mean.eps", direction = "wide")) #object has to be a dataframe not a tibble for this to work 
bugoto_1st$ERR <- NA
bugoto_1st <- bugoto_1st[-which(is.na(bugoto_1st$`mean.eps.Oct 3wk PT`)==T),]
bugoto_1st <- bugoto_1st[-which(is.na(bugoto_1st$`mean.eps.Sept 2017`)==T),]
bugoto_1st <- err_func(bugoto_1st, bugoto_1st$`mean.eps.Sept 2017`, bugoto_1st$`mean.eps.Oct 3wk PT`)
bugoto_1st <- bugoto_1st[-which(is.nan(bugoto_1st$ERR)==T),]

for(i in 1:nrow(bugoto_1st)){
  if(bugoto_1st[i,ncol(bugoto_1st)]<(-100)){ 
    bugoto_1st[i,ncol(bugoto_1st)] <-(-100)
  }else(
    bugoto_1st[i,ncol(bugoto_1st)] <-  bugoto_1st[i,ncol(bugoto_1st)]
  )
}

bugoto_1st$time <- as.factor("`Sept 2017 to 3wk PT`")
bugoto_1st <- bugoto_1st %>% select(cid, School, time, ERR, SEX)

bugoto_2nd <- bugoto %>% filter(Month=="March 2018" | Month=="March 2018 3wk PT")%>%
  select(cid, School, Month, mean.eps, SEX)
bugoto_2nd$cid <- as.factor(bugoto_2nd$cid)
bugoto_2nd <- bugoto_2nd %>% group_by(cid, School, Month, SEX) %>%
  summarise(mean.eps=mean(mean.eps))
bugoto_2nd <- as.data.frame(bugoto_2nd)
bugoto_2nd <- as.data.frame(reshape(bugoto_2nd, idvar = c("cid", "School", "SEX"), timevar = "Month", v.names = "mean.eps", direction = "wide")) #object has to be a dataframe not a tibble for this to work 
bugoto_2nd$ERR <- "NA"
bugoto_2nd <- bugoto_2nd[-which(is.na(bugoto_2nd$`mean.eps.March 2018`)==T),]
bugoto_2nd <- bugoto_2nd[-which(is.na(bugoto_2nd$`mean.eps.March 2018 3wk PT`)==T),]
bugoto_2nd <- err_func(bugoto_2nd, bugoto_2nd$`mean.eps.March 2018`, bugoto_2nd$`mean.eps.March 2018 3wk PT`)
bugoto_2nd <- bugoto_2nd[-which(is.nan(bugoto_2nd$ERR)==T),]

for(i in 1:nrow(bugoto_2nd)){
  if(bugoto_2nd[i,ncol(bugoto_2nd)]<(-100)){ 
    bugoto_2nd[i,ncol(bugoto_2nd)] <-(-100)
  }else(
    bugoto_2nd[i,ncol(bugoto_2nd)] <-  bugoto_2nd[i,ncol(bugoto_2nd)]
  )
}

bugoto_2nd$time <- as.factor("`March 2018 to 3wk PT`")
bugoto_2nd <- bugoto_2nd %>% select(cid, School, time, ERR, SEX)

bwondha <- subset(data_2017_2018, School == "Bwondha")
bwondha_1st <- bwondha %>% filter(Month=="Feb 2018" | Month=="March 2018")%>%
  select(cid, School, Month, mean.eps, SEX)
bwondha_1st$cid <- as.factor(bwondha_1st$cid)
bwondha_1st <- bwondha_1st %>% group_by(cid, School, Month, SEX) %>%
  summarise(mean.eps=mean(mean.eps))
bwondha_1st <- as.data.frame(bwondha_1st)
bwondha_1st <- as.data.frame(reshape(bwondha_1st, idvar = c("cid","School", "SEX"), timevar = "Month", v.names = "mean.eps", direction = "wide")) #object has to be a dataframe not a tibble for this to work 
bwondha_1st$ERR <- NA
bwondha_1st <- bwondha_1st[-which(is.na(bwondha_1st$`mean.eps.Feb 2018`)==T),]
bwondha_1st <- bwondha_1st[-which(is.na(bwondha_1st$`mean.eps.March 2018`)==T),]
bwondha_1st <- err_func(bwondha_1st, bwondha_1st$`mean.eps.Feb 2018`, bwondha_1st$`mean.eps.March 2018`)
bwondha_1st <- bwondha_1st[-which(is.nan(bwondha_1st$ERR)==T),]

for(i in 1:nrow(bwondha_1st)){
  if(bwondha_1st[i,ncol(bwondha_1st)]<(-100)){ 
    bwondha_1st[i,ncol(bwondha_1st)] <-(-100)
  }else(
    bwondha_1st[i,ncol(bwondha_1st)] <-  bwondha_1st[i,ncol(bwondha_1st)]
  )
}

bwondha_1st$time <- as.factor("`Feb 2018 to 3wk PT`")
bwondha_1st <- bwondha_1st %>% select(cid, School, time, ERR, SEX)

musubi <- subset(data_2017_2018, School == "Musubi")
musubi_1st <- musubi %>% filter(Month=="March 2018" | Month=="March 2018 3wk PT")%>%
  select(cid, School, Month, mean.eps, SEX)
musubi_1st$cid <- as.factor(musubi_1st$cid)
musubi_1st <- musubi_1st %>% group_by(cid, School, Month, SEX) %>%
  summarise(mean.eps=mean(mean.eps))
musubi_1st <- as.data.frame(musubi_1st)
musubi_1st$SEX <- as.factor(musubi_1st$SEX)
musubi_1st$cid <- as.factor(musubi_1st$cid)
musubi_1st <- as.data.frame(reshape(musubi_1st, idvar = c("cid", "School", "SEX"), timevar = "Month", v.names = "mean.eps", direction = "wide")) #object has to be a dataframe not a tibble for this to work 
musubi_1st$ERR <- NA
musubi_1st <- musubi_1st[-which(is.na(musubi_1st$`mean.eps.March 2018`)==T),]
musubi_1st <- musubi_1st[-which(is.na(musubi_1st$`mean.eps.March 2018 3wk PT`)==T),]
musubi_1st <- err_func(musubi_1st, musubi_1st$`mean.eps.March 2018`, musubi_1st$`mean.eps.March 2018 3wk PT`)
musubi_1st <- musubi_1st[-which(is.nan(musubi_1st$ERR)==T),]

for(i in 1:nrow(musubi_1st)){
  if(musubi_1st[i,ncol(musubi_1st)]<(-100)){ 
    musubi_1st[i,ncol(musubi_1st)] <-(-100)
  }else(
    musubi_1st[i,ncol(musubi_1st)] <-  musubi_1st[i,ncol(musubi_1st)]
  )
}

musubi_1st$time <- as.factor("`March 2018 to 3wk PT`")
musubi_1st <- musubi_1st %>% select(cid, School, time, ERR, SEX)

#egg_red_no_age <- egg_reduction %>% select(-age)

err.1718 <- bind_rows(bugoto_1st, bugoto_2nd, bwondha_1st, musubi_1st)

err.1718 <- age_extraction(err.1718)
err.1718 <- err.1718 %>% rename(sex="SEX")

egg_reduction_all_data <- bind_rows(egg_reduction, err.1718)
egg_reduction_all_data$time <- as.factor(egg_reduction_all_data$time)

egg_reduction_all_data$time <- factor(egg_reduction_all_data$time, levels = c("`Baseline to Aug 4wk PT`", "`June 2005 to 4wk PT`",
                                                                              "`May 2006 to 4wk PT`" ,  "`Feb 2013 to 3wk PT`",
                                                                              "`May 2014 to 4wk PT`", "`Sept 2017 to 3wk PT`",
                                                                              "`Feb 2018 to 3wk PT`",  "`March 2018 to 3wk PT`" ))

 
write.csv(egg_reduction_all_data,"egg.reduction.data.csv")
ERR_data <-  arrange(egg_reduction_all_data, time, ERR) 

ERR_data <- ERR_data %>% group_by(time) %>% mutate(rank =rank(ERR, ties.method = 'first'))

ERR_sum <- ERR_data %>% group_by(time) %>% summarise(n=n())

ERR_data <- merge(ERR_data, ERR_sum)

ERR_data$CumPerc <- ERR_data$rank/ERR_data$n

write.csv(ERR_data, "egg.reduction.data.cumulative.percentage.csv")

colours <- c( "dodgerblue3","chocolate", "palegreen4")
# this figure only has individuals who were 
ggplot() + 
  geom_jitter(data = ERR_data, aes(x = CumPerc, y = ERR, col = factor(School), shape=School), size = 2, alpha = 0.7) + 
  scale_colour_manual(values=colours)+
  labs(x = "Cumulative percent", y = "Individual egg reduction rate") + 
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text.x = element_text(size = 14,face="bold"),
        strip.text.y = element_text(size = 14,face="bold"),
        axis.title=element_text(size=14,face="bold"))+
  facet_grid(School~time, drop=T) +
  geom_hline(yintercept = 90)+
  ggsave("eggreductionrate.pdf")
  

ERR_data <- ERR_data %>% mutate_if(is.character, as.factor)


#### Cure Rates ####

cure_rates <- err_long_data %>% select(CID, School, Month, MeanSm)

cure_rates <- data.frame(cure_rates)

cure_rates <- as.data.frame(reshape(cure_rates, idvar = c("CID", "School"), timevar = "Month", v.names = "MeanSm", direction = "wide")) #object has to be a dataframe not a tibble for this to work 

cure_rates_04 <- cure_rates %>% select(CID, `MeanSm.Baseline 2004`,`MeanSm.Aug 4wk PT` )%>%rename(cid="CID")
cure_rates_04$time <- as.factor("Baseline 2004 to 4wk PT")
cure_rates_04 <- cure_rates_04[-which(is.na(cure_rates_04$`MeanSm.Baseline 2004`)==T),]
cure_rates_04 <- cure_rates_04[-which(is.na(cure_rates_04$`MeanSm.Aug 4wk PT`)==T),]
cure_rates_04 <- school_relabel(cure_rates_04)
cure_rates_04$cure_rate <- NA
for(i in 1:nrow(cure_rates_04)){
  if(cure_rates_04[i,3]<cure_rates_04[i,2]){
    cure_rates_04[i,ncol(cure_rates_04)] <- "cured"
  } else if(cure_rates_04[i,2]==0 & cure_rates_04[i,3]==0){
    cure_rates_04[i,ncol(cure_rates_04)] <- "not infected"
  } else if(cure_rates_04[i,3]>cure_rates_04[i,2]){
    cure_rates_04[i,ncol(cure_rates_04)] <- "not cured"
  }
}

cure_rates_05 <- cure_rates %>% select(CID,`MeanSm.June 2005`,`MeanSm.July 05 4wk PT`  )%>%rename(cid="CID")
cure_rates_05$time <- as.factor("June 2005 to 4wk PT")
cure_rates_05 <- cure_rates_05[-which(is.na(cure_rates_05$`MeanSm.June 2005`)==T),]
cure_rates_05 <- cure_rates_05[-which(is.na(cure_rates_05$`MeanSm.July 05 4wk PT`)==T),]
cure_rates_05 <- school_relabel(cure_rates_05)
cure_rates_05$cure_rate <- NA
for(i in 1:nrow(cure_rates_05)){
  if(cure_rates_05[i,3]<cure_rates_05[i,2]){
    cure_rates_05[i,ncol(cure_rates_05)] <- "cured"
  } else if(cure_rates_05[i,2]==0 & cure_rates_05[i,3]==0){
    cure_rates_05[i,ncol(cure_rates_05)] <- "not infected"
  } else if(cure_rates_05[i,3]>cure_rates_05[i,2]){
    cure_rates_05[i,ncol(cure_rates_05)] <- "not cured"
  }else if(cure_rates_05[i,3]==cure_rates_05[i,2]){
    cure_rates_05[i,ncol(cure_rates_05)] <- "not cured"
  }
}


cure_rates_06 <- cure_rates %>% select(CID,`MeanSm.May 2006`, `MeanSm.July 06 4wk PT` )%>%rename(cid="CID")
cure_rates_06$time <- as.factor("May 2006 to 4wk PT")
cure_rates_06 <- cure_rates_06[-which(is.na(cure_rates_06$`MeanSm.May 2006`)==T),]
cure_rates_06 <- cure_rates_06[-which(is.na(cure_rates_06$`MeanSm.July 06 4wk PT`)==T),]
cure_rates_06 <- school_relabel(cure_rates_06)
cure_rates_06$cure_rate <- NA
for(i in 1:nrow(cure_rates_06)){
  if(cure_rates_06[i,3]<cure_rates_06[i,2]){
    cure_rates_06[i,ncol(cure_rates_06)] <- "cured"
  } else if(cure_rates_06[i,2]==0 & cure_rates_06[i,3]==0){
    cure_rates_06[i,ncol(cure_rates_06)] <- "not infected"
  } else if(cure_rates_06[i,3]>cure_rates_06[i,2]){
    cure_rates_06[i,ncol(cure_rates_06)] <- "not cured"
  }else if(cure_rates_06[i,3]==cure_rates_06[i,2]){
    cure_rates_06[i,ncol(cure_rates_06)] <- "not cured"
  }
}

cure_rates_13 <- cure_rates %>% select(CID,`MeanSm.Feb 2013`, `MeanSm.March 3wk PT` )%>% rename(cid="CID")
cure_rates_13$time <- as.factor("Feb 2013 to 3wk PT")
cure_rates_13 <- cure_rates_13[-which(is.na(cure_rates_13$`MeanSm.Feb 2013`)==T),]
cure_rates_13 <- cure_rates_13[-which(is.na(cure_rates_13$`MeanSm.March 3wk PT`)==T),]
cure_rates_13 <- school_relabel(cure_rates_13)
cure_rates_13$cure_rate <- NA
for(i in 1:nrow(cure_rates_13)){
  if(cure_rates_13[i,3]<cure_rates_13[i,2]){
    cure_rates_13[i,ncol(cure_rates_13)] <- "cured"
  } else if(cure_rates_13[i,2]==0 & cure_rates_13[i,3]==0){
    cure_rates_13[i,ncol(cure_rates_13)] <- "not infected"
  } else if(cure_rates_13[i,3]>cure_rates_13[i,2]){
    cure_rates_13[i,ncol(cure_rates_13)] <- "not cured"
  }else if(cure_rates_13[i,3]==cure_rates_13[i,2]){
    cure_rates_13[i,ncol(cure_rates_13)] <- "not cured"
  }
}

cure_rates_14 <- cure_rates %>% select(CID,`MeanSm.May 2014`, `MeanSm.June 4wk PT` )%>% rename(cid="CID")
cure_rates_14$time <- as.factor("May 2014 to 4wk PT")
cure_rates_14 <- cure_rates_14[-which(is.na(cure_rates_14$`MeanSm.May 2014`)==T),]
cure_rates_14 <- cure_rates_14[-which(is.na(cure_rates_14$`MeanSm.June 4wk PT`)==T),]
cure_rates_14 <- school_relabel(cure_rates_14)
cure_rates_14$cure_rate <- NA
for(i in 1:nrow(cure_rates_14)){
  if(cure_rates_14[i,3]<cure_rates_14[i,2]){
    cure_rates_14[i,ncol(cure_rates_14)] <- "cured"
  } else if(cure_rates_14[i,2]==0 & cure_rates_14[i,3]==0){
    cure_rates_14[i,ncol(cure_rates_14)] <- "not infected"
  } else if(cure_rates_14[i,3]>cure_rates_14[i,2]){
    cure_rates_14[i,ncol(cure_rates_14)] <- "not cured"
  }else if(cure_rates_14[i,3]==cure_rates_14[i,2]){
    cure_rates_14[i,ncol(cure_rates_14)] <- "not cured"
  }
}

cure_rates_long <- bind_rows(cure_rates_04, cure_rates_05, cure_rates_06, cure_rates_13, cure_rates_14)%>% select(cid, School, time, cure_rate)


# this is including children who were 0 at bl because if they had eggs at 3/4wks
# they had to be infected at bl. which then means that treatment didn't successfully
# clear those infections that were likely juveniles. 

# this does not include children who don't have both counts 

bugoto_cure_1st <- as.data.frame(bugoto)
bugoto_cure_1st <- bugoto_cure_1st %>% 
  filter(Month=="Sept 2017" | Month=="Oct 3wk PT") %>% 
  select(cid, School, Month, mean.eps)%>%
  mutate_if(is.character, as.factor)%>%
  group_by(cid, School, Month) %>%
  summarise(mean.eps=mean(mean.eps))%>%
  pivot_wider(names_from = Month, values_from = mean.eps)

bugoto_cure_1st <- bugoto_cure_1st[-which(is.na(bugoto_cure_1st$`Sept 2017`)==T),]
bugoto_cure_1st <- bugoto_cure_1st[-which(is.na(bugoto_cure_1st$`Oct 3wk PT`)==T),]
bugoto_cure_1st$cure_rate <- NA
for(i in 1:nrow(bugoto_cure_1st)){
  if(bugoto_cure_1st[i,3]<bugoto_cure_1st[i,4]){
    bugoto_cure_1st[i,ncol(bugoto_cure_1st)] <- "cured"
  } else if(bugoto_cure_1st[i,3]==0 & bugoto_cure_1st[i,4]==0){
    bugoto_cure_1st[i,ncol(bugoto_cure_1st)] <- "not infected"
  } else if(bugoto_cure_1st[i,3]>bugoto_cure_1st[i,4]){
    bugoto_cure_1st[i,ncol(bugoto_cure_1st)] <- "not cured"
  }
}
bugoto_cure_1st$time <- as.factor("Sept 2017 to 3wk PT")

bugoto_cure_2nd <- as.data.frame(bugoto)
bugoto_cure_2nd <- bugoto_cure_2nd %>% 
  filter(Month=="March 2018" | Month=="March 2018 3wk PT") %>% 
  select(cid, School, Month, mean.eps)%>%
  mutate_if(is.character, as.factor)%>%
  group_by(cid, School, Month) %>%
  summarise(mean.eps=mean(mean.eps))%>%
  pivot_wider(names_from = Month, values_from = mean.eps)

bugoto_cure_2nd <- bugoto_cure_2nd[-which(is.na(bugoto_cure_2nd$`March 2018`)==T),]
bugoto_cure_2nd <- bugoto_cure_2nd[-which(is.na(bugoto_cure_2nd$`March 2018 3wk PT`)==T),]

bugoto_cure_2nd$cure_rate <- NA
for(i in 1:nrow(bugoto_cure_2nd)){
  if(bugoto_cure_2nd[i,4]<bugoto_cure_2nd[i,3]){
    bugoto_cure_2nd[i,ncol(bugoto_cure_2nd)] <- "cured"
  } else if(bugoto_cure_2nd[i,3]==0 & bugoto_cure_2nd[i,4]==0){
    bugoto_cure_2nd[i,ncol(bugoto_cure_2nd)] <- "not infected"
  } else if(bugoto_cure_2nd[i,4]>bugoto_cure_2nd[i,3]){
    bugoto_cure_2nd[i,ncol(bugoto_cure_2nd)] <- "not cured"
  }
}
bugoto_cure_2nd$time <- as.factor("March 2018 to 3wk PT")

bwondha_cure <- bwondha
bwondha_cure <- bwondha_cure %>% 
  filter(Month=="Feb 2018" | Month=="March 2018") %>% 
  select(cid, School, Month, mean.eps)%>%
  mutate_if(is.character, as.factor)%>%
  group_by(cid, School, Month) %>%
  summarise(mean.eps=mean(mean.eps))%>%
  pivot_wider(names_from = Month, values_from = mean.eps)%>%
  mutate_if(is.character, as.factor)

bwondha_cure <- bwondha_cure[-which(is.na(bwondha_cure$`Feb 2018`)==T),]
bwondha_cure <- bwondha_cure[-which(is.na(bwondha_cure$`March 2018`)==T),]

bwondha_cure$cure_rate <- NA
for(i in 1:nrow(bwondha_cure)){
  if(bwondha_cure[i,4]<bwondha_cure[i,3]){
    bwondha_cure[i,ncol(bwondha_cure)] <- "cured"
  } else if(bwondha_cure[i,3]==0 & bwondha_cure[i,4]==0){
    bwondha_cure[i,ncol(bwondha_cure)] <- "not infected"
  } else if(bwondha_cure[i,4]>bwondha_cure[i,3]){
    bwondha_cure[i,ncol(bwondha_cure)] <- "not cured"
  }
}
bwondha_cure$time <- as.factor("Feb 2018 to 3wk PT")

musubi_cure <- musubi
musubi_cure <- musubi_cure %>% 
  filter(Month=="March 2018" | Month=="March 2018 3wk PT") %>% 
  select(cid, School, Month, mean.eps)%>%
  mutate_if(is.character, as.factor)%>%
  group_by(cid, School, Month) %>%
  summarise(mean.eps=mean(mean.eps))%>%
  pivot_wider(names_from = Month, values_from = mean.eps)

musubi_cure <- musubi_cure[-which(is.na(musubi_cure$`March 2018`)==T),]
musubi_cure <- musubi_cure[-which(is.na(musubi_cure$`March 2018 3wk PT`)==T),]

musubi_cure$cure_rate <- NA
for(i in 1:nrow(musubi_cure)){
  if(musubi_cure[i,4]<musubi_cure[i,3]){
    musubi_cure[i,ncol(musubi_cure)] <- "cured"
  } else if(musubi_cure[i,3]==0 & musubi_cure[i,4]==0){
    musubi_cure[i,ncol(musubi_cure)] <- "not infected"
  } else if(musubi_cure[i,4]>musubi_cure[i,3]){
    musubi_cure[i,ncol(musubi_cure)] <- "not cured"
  }
}
musubi_cure$time <- as.factor("March 2018 to 3wk PT")

cure_rates_all <- bind_rows(bugoto_cure_1st, bugoto_cure_2nd, bwondha_cure, musubi_cure)%>% 
  select(cid, School, cure_rate, time)
cure_rates_all <- bind_rows(cure_rates_long, cure_rates_all)%>%mutate_if(is.character, as.factor)
cure_rates_all <- cure_rates_all[-which(is.na(cure_rates_all$cure_rate)==T),]
cure_rates_all$time <- factor(cure_rates_all$time, levels = c("Baseline 2004 to 4wk PT", "June 2005 to 4wk PT",
                                                                              "May 2006 to 4wk PT" ,  "Feb 2013 to 3wk PT",
                                                                              "May 2014 to 4wk PT", "Sept 2017 to 3wk PT",
                                                                              "Feb 2018 to 3wk PT",  "March 2018 to 3wk PT" ))
cure_rates_all%>%
  group_by(School, time, cure_rate)%>%
  summarise(n=n())%>%
  mutate(freq=n/sum(n))%>%
  ggplot(aes(x=cure_rate, y=freq, fill=School))+
  geom_col(position = "dodge")+
  scale_fill_manual(values=colours)+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text.x = element_text(size = 14,face="bold"),
        strip.text.y = element_text(size = 14,face="bold",angle=360),
        axis.title=element_text(size=14,face="bold"))+
  facet_grid(time~School, labeller = label_wrap_gen(width=10))+
  ylab("Proportion")+
  ggsave("cure_rates.pdf")

#### analysis ####

cure_rates_m1 <- glmer(cure_rate ~ School + time + (1|cid), data=cure_rates_all, family=binomial(link=logit))
summary(cure_rates_m1)






