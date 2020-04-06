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
baseline_2004 <- baseline_2004 %>% filter(Week!="1",Week!="4",Week!="26",Week!="27")

#### Modelling baseline prevalence and infection intensity ####

# According to jarrod's notes there are two types of models you can run here
# as this is count data you would use a NB model or a  poisson. 
# poisson is likely to be overdispersed however putting in a obs level RE can take care of that
# A NB model w/o a RE should be a similar output but I am not sure it is entirely correct 
# if the residual deviance is not so large in comparison to a small resid DF then it is not so od.
# trying poisson + RE, NB and NB + RE
baseline_2004 <- bl_data[-which(is.na(baseline_2004$MeanSm)==T),]
baseline_2004$MeanSm <- as.numeric(baseline_2004$MeanSm)

bl_int_dist_pois <- fitdist(baseline_2004$MeanSm, "pois", method="mle")
bl_int_dist_nb <- fitdist(baseline_2004$MeanSm, "nbinom")

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
  theme(axis.text.x = element_text(angle=90),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14))+
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

prev_2004 <- glmer(prevalence ~ relevel(School, ref="Bwondha") + (1|CID), data=baseline_2004, family=binomial(link='logit'))

summary(prev_2004)

baseline_2004 %>%
  group_by(School, Month, prevalence) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))%>%
  filter(prevalence=="1")%>%
  ggplot(aes(x=Month, y=freq))+
  geom_col(position = "dodge", fill="#F6AE2D", colour="#2F4858")+
  theme(axis.text.x = element_text(angle=360),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14))+
  facet_grid(~School, scales = "free_x")+
  ylab("Infection Prevalence")+
  ggsave("prev_naive_all_ages.pdf")

#### Baseline age/ intensity distribution ####

# can't fit a model with obs level RE to this data, I think the dataset is too small            

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
  theme(axis.text.x = element_text(angle=360),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14))+
  ggsave("age_split_baseline_int.pdf")

# no effect of age in baseline data on infection prevalence in musubi #

musubi_baseline <- baseline_2004 %>% filter(School=="Musubi")
musubi_baseline$AGE <- as.factor(musubi_baseline$AGE)

age_mus_bl <- glm(prevalence ~ relevel(AGE, ref="12"), data=musubi_baseline, family=binomial(link='logit'))
summary(age_mus_bl)

#### long data pzq naive 6 year olds ####

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
#pzq_naive_6yo <- pzq_naive_6yo %>% rename(`mean eggs per slide`= MeanSm)%>% rename(cid=CID)

musubi_add <- musubi_baseline %>% select(CID, School, Month, event, MeanSm, prevalence)

pzq_naive_04allages <- bind_rows(musubi_add, pzq_naive_6yo)
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

#pzq_naive_full <- bind_rows(pzq_naive_6yo, pzq_naive_17_18) 
#pzq_naive_full$Month <- factor(pzq_naive_full$Month, levels = c("Baseline 2004", "June 2005", "May 2006", "Feb 2013", 
                                                                #"May 2014", "March 2017", "Sept 2017", 
                                                                #"Feb 2018", "March 2018" ))

#### data with all ages for musubi in 2004 #### 
pzq_naive_17_18 <- pzq_naive_17_18 %>% rename(CID="cid", MeanSm=`mean eggs per slide`)

pzq_naive_all <- bind_rows(pzq_naive_04allages, pzq_naive_17_18)
pzq_naive_all <- pzq_naive_all %>% mutate_if(is.character, as.factor)
pzq_naive_all$Month <- factor(pzq_naive_all$Month, levels = c("Baseline 2004", "June 2005", "May 2006", "Feb 2013", 
                                                                "May 2014", "March 2017", "Sept 2017"))
#### Across years ####

#### Figures ####

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

# change the timing labels because the same number of samples are available but the "month" labels are misleading as not all places have the same months

pzq_naive_all$sample_period <- NA

for(i in 1:nrow(pzq_naive_all)){
  if(pzq_naive_all[i,3]=="Baseline 2004"){
    pzq_naive_all[i,ncol(pzq_naive_all)] <- "2004"
  } else if(pzq_naive_all[i,3]=="June 2005"){
    pzq_naive_all[i,ncol(pzq_naive_all)] <- "2005"    
  } else if(pzq_naive_all[i,3]=="May 2006"){
    pzq_naive_all[i,ncol(pzq_naive_all)] <- "2006"
  } else if(pzq_naive_all[i,3]=="Feb 2013"){
    pzq_naive_all[i,ncol(pzq_naive_all)] <- "2013"
  } else if(pzq_naive_all[i,3]=="May 2014"){
    pzq_naive_all[i,ncol(pzq_naive_all)] <- "2014"
  } else if(pzq_naive_all[i,3]=="March 2017"){
    pzq_naive_all[i,ncol(pzq_naive_all)] <- "2017"
  } else {
    pzq_naive_all[i,ncol(pzq_naive_all)] <- "2017"
  }
}

pzq_naive_all$sample_period <- as.factor(pzq_naive_all$sample_period)

prev_bl_allyrs <- glmer(prevalence ~ School + sample_period + School*sample_period+ (1|CID), data=pzq_naive_all, family=binomial(link = "logit"))

summary(prev_bl_allyrs)

# Infection intensity plot ALL AGES in 2004#

pzq.errorbars <- pzq_naive_all %>% group_by(School, Month) %>%
  summarise(mean_eggs=mean(MeanSm, na.rm = T), n=n(), sd=sd(MeanSm, na.rm=T), std.error=sd/sqrt(n))

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
  select(CID, School, Year, Week, Month, MeanSm)%>%
  mutate_if(is.character, as.factor)
err_long_data$Week <- factor(err_long_data$Week)
err_long_data$Year <- as.factor(err_long_data$Year)
err_long_data <- as.data.frame(err_long_data)
err_long_data2 <- err_long_data %>% select(CID, Month, MeanSm)
err_long_data2 <- as.data.frame(err_long_data2)
#err_long_data <- err_long_data %>% pivot_wider(names_from = Week, values_from = MeanSm)
err_long_data2 <- as.data.frame(reshape(err_long_data2, idvar = "CID", timevar = "Month", v.names = "MeanSm", direction = "wide")) #object has to be a dataframe not a tibble for this to work 

#### Long Data ####
# not using any children who don't have both counts
err_trt_2004 <- as.data.frame(cbind(as.character(err_long_data2$CID), err_long_data2$`MeanSm.Baseline 2004`, err_long_data2$`MeanSm.Aug 4wk PT`))
err_trt_2004 <- err_trt_2004 %>% mutate_if(is.character, as.factor)%>% rename(cid="V1", `baseline 2004`="V2", `Aug 4wk PT`="V3")
err_trt_2004 <- err_trt_2004[-which(is.na(err_trt_2004$`baseline 2004`)==T),]
err_trt_2004 <- err_trt_2004[-which(is.na(err_trt_2004$`Aug 4wk PT`)==T),]
err_trt_2004 <- school_relabel(err_trt_2004)
err_trt_2004$`baseline 2004` <- as.numeric(err_trt_2004$`baseline 2004`)
err_trt_2004$`Aug 4wk PT` <- as.numeric(err_trt_2004$`Aug 4wk PT`)
err_trt_2004$percent_reduction <- "NA"
err_trt_2004$percent_reduction <- percent.change(err_trt_2004$`Aug 4wk PT`,err_trt_2004$`baseline 2004`)
err_trt_2004$time <- as.factor("`Baseline to Aug 4wk PT`")
err_trt_2004 <- err_trt_2004 %>% select(cid, School, time, percent_reduction)

err_trt_2005 <- as.data.frame(cbind(as.character(err_long_data2$CID), err_long_data2$`MeanSm.June 2005`, err_long_data2$`MeanSm.July 05 4wk PT`))
err_trt_2005 <- err_trt_2005 %>% mutate_if(is.character, as.factor)%>% rename(cid="V1", `June 2005`="V2", `July 05 4wk PT`="V3")
err_trt_2005 <- err_trt_2005[-which(is.na(err_trt_2005$`June 2005`)==T),]
err_trt_2005 <- err_trt_2005[-which(is.na(err_trt_2005$`July 05 4wk PT`)==T),]
err_trt_2005 <- school_relabel(err_trt_2005)
err_trt_2005$`June 2005` <- as.numeric(err_trt_2005$`June 2005`)
err_trt_2005$`July 05 4wk PT` <- as.numeric(err_trt_2005$`July 05 4wk PT`)
err_trt_2005$percent_reduction <- "NA"
err_trt_2005$percent_reduction <- percent.change(err_trt_2005$`July 05 4wk PT`, err_trt_2005$`June 2005`)
err_trt_2005$time <- as.factor("`June 2005 to July 4wk PT`")
err_trt_2005 <- err_trt_2005 %>% select(cid, School, time, percent_reduction)

err_trt_2006 <- as.data.frame(cbind(as.character(err_long_data2$CID), err_long_data2$`MeanSm.May 2006`, err_long_data2$`MeanSm.July 06 4wk PT`))
err_trt_2006 <- err_trt_2006 %>% mutate_if(is.character, as.factor)%>% rename(cid="V1", `May 2006`="V2", `July 06 4wk PT`="V3")
err_trt_2006 <- err_trt_2006[-which(is.na(err_trt_2006$`May 2006`)==T),]
err_trt_2006 <- err_trt_2006[-which(is.na(err_trt_2006$`July 06 4wk PT`)==T),]
err_trt_2006 <- school_relabel(err_trt_2006)
err_trt_2006$`May 2006` <- as.numeric(err_trt_2006$`May 2006`)
err_trt_2006$`July 06 4wk PT` <- as.numeric(err_trt_2006$`July 06 4wk PT`)
err_trt_2006$percent_reduction <- "NA"
err_trt_2006$percent_reduction <- percent.change(err_trt_2006$`July 06 4wk PT`, err_trt_2006$`May 2006`)
err_trt_2006$time <- as.factor("`May 2006 to July 4wk PT`")
err_trt_2006 <- err_trt_2006 %>% select(cid, School, time, percent_reduction)

err_trt_2013 <- as.data.frame(cbind(as.character(err_long_data2$CID), err_long_data2$`MeanSm.Feb 2013`, err_long_data2$`MeanSm.March 3wk PT`))
err_trt_2013 <- err_trt_2013 %>% mutate_if(is.character, as.factor)%>% rename(cid="V1", `Feb 2013`="V2", `March 3wk PT`="V3")
err_trt_2013 <- err_trt_2013[-which(is.na(err_trt_2013$`Feb 2013`)==T),]
err_trt_2013 <- err_trt_2013[-which(is.na(err_trt_2013$`March 3wk PT`)==T),]
err_trt_2013 <- school_relabel(err_trt_2013)
err_trt_2013$`Feb 2013` <- as.numeric(err_trt_2013$`Feb 2013`)
err_trt_2013$`March 3wk PT` <- as.numeric(err_trt_2013$`March 3wk PT`)
err_trt_2013$percent_reduction <- "NA"
err_trt_2013$percent_reduction <- percent.change(err_trt_2013$`March 3wk PT`, err_trt_2013$`Feb 2013`)
err_trt_2013$time <- as.factor("`Feb 2013 to March 3wk PT`")
err_trt_2013 <- err_trt_2013 %>% select(cid, School, time, percent_reduction)

err_trt_2014 <- as.data.frame(cbind(as.character(err_long_data2$CID), err_long_data2$`MeanSm.May 2014`, err_long_data2$`MeanSm.June 4wk PT`))
err_trt_2014 <- err_trt_2014 %>% mutate_if(is.character, as.factor)%>% rename(cid="V1", `May 2014`="V2", `June 4wk PT`="V3")
err_trt_2014 <- err_trt_2014[-which(is.na(err_trt_2014$`May 2014`)==T),]
err_trt_2014 <- err_trt_2014[-which(is.na(err_trt_2014$`June 4wk PT`)==T),]
err_trt_2014 <- school_relabel(err_trt_2014)
err_trt_2014$`May 2014` <- as.numeric(err_trt_2014$`May 2014`)
err_trt_2014$`June 4wk PT` <- as.numeric(err_trt_2014$`June 4wk PT`)
err_trt_2014$percent_reduction <- "NA"
err_trt_2014$percent_reduction <- percent.change(err_trt_2014$`June 4wk PT`, err_trt_2014$`May 2014`)
err_trt_2014$time <- as.factor("`May 2014 to June 4wk PT`")
err_trt_2014 <- err_trt_2014 %>% select(cid, School, time, percent_reduction)

egg_reduction <- bind_rows(err_trt_2004, err_trt_2005, err_trt_2006, err_trt_2013, err_trt_2014)%>%
  mutate_if(is.character, as.factor)






#### Recent Data ####
# only including children that were infected at the beginning of each treatment

bugoto <- subset(data_2017_2018, School == "Bugoto")
bugoto_1st <- bugoto %>% filter(Month=="Sept 2017" | Month=="Oct 3wk PT")%>%
  select(cid, School, Month, mean.eps)
bugoto_1st$cid <- as.factor(bugoto_1st$cid)
  bugoto_1st <- bugoto_1st %>% group_by(cid, School, Month) %>%
  summarise(mean.eps=mean(mean.eps))
bugoto_1st <- as.data.frame(bugoto_1st)
bugoto_1st <- as.data.frame(reshape(bugoto_1st, idvar = "cid", timevar = "Month", v.names = "mean.eps", direction = "wide")) #object has to be a dataframe not a tibble for this to work 
bugoto_1st$percent_reduction <- "NA"
bugoto_1st$percent_reduction <- percent.change(bugoto_1st$`mean.eps.Oct 3wk PT`, bugoto_1st$`mean.eps.Sept 2017`)
bugoto_1st$time <- as.factor("`Sept 2017 to Oct 3wk PT`")
bugoto_1st <- bugoto_1st[-which(is.na(bugoto_1st$`mean.eps.Oct 3wk PT`)==T),]
bugoto_1st <- bugoto_1st[-which(is.na(bugoto_1st$`mean.eps.Sept 2017`)==T),]
bugoto_1st <- bugoto_1st[-which(is.nan(bugoto_1st$percent_reduction)==T),]
bugoto_1st <- bugoto_1st[-which(is.infinite(bugoto_1st$percent_reduction)==T),]
bugoto_1st <- bugoto_1st %>% select(cid, School, time, percent_reduction)

bugoto_2nd <- bugoto %>% filter(Month=="March 2018" | Month=="March 2018 3wk PT")%>%
  select(cid, School, Month, mean.eps)
bugoto_2nd$cid <- as.factor(bugoto_2nd$cid)
bugoto_2nd <- bugoto_2nd %>% group_by(cid, School, Month) %>%
  summarise(mean.eps=mean(mean.eps))
bugoto_2nd <- as.data.frame(bugoto_2nd)
bugoto_2nd <- as.data.frame(reshape(bugoto_2nd, idvar = "cid", timevar = "Month", v.names = "mean.eps", direction = "wide")) #object has to be a dataframe not a tibble for this to work 
bugoto_2nd$percent_reduction <- "NA"
bugoto_2nd$percent_reduction <- percent.change(bugoto_2nd$`mean.eps.March 2018 3wk PT`, bugoto_2nd$`mean.eps.March 2018`)
bugoto_2nd$time <- as.factor("`March 2018 to March 3wk PT`")
bugoto_2nd <- bugoto_2nd[-which(is.na(bugoto_2nd$`mean.eps.March 2018`)==T),]
bugoto_2nd <- bugoto_2nd[-which(is.na(bugoto_2nd$`mean.eps.March 2018 3wk PT`)==T),]
bugoto_2nd <- bugoto_2nd[-which(is.nan(bugoto_2nd$percent_reduction)==T),]
bugoto_2nd <- bugoto_2nd[-which(is.infinite(bugoto_2nd$percent_reduction)==T),]
bugoto_2nd <- bugoto_2nd %>% select(cid, School, time, percent_reduction)

bwondha <- subset(data_2017_2018, School == "Bwondha")
bwondha_1st <- bwondha %>% filter(Month=="Feb 2018" | Month=="March 2018")%>%
  select(cid, School, Month, mean.eps)
bwondha_1st$cid <- as.factor(bwondha_1st$cid)
bwondha_1st <- bwondha_1st %>% group_by(cid, School, Month) %>%
  summarise(mean.eps=mean(mean.eps))
bwondha_1st <- as.data.frame(bwondha_1st)
bwondha_1st <- as.data.frame(reshape(bwondha_1st, idvar = "cid", timevar = "Month", v.names = "mean.eps", direction = "wide")) #object has to be a dataframe not a tibble for this to work 
bwondha_1st$percent_reduction <- "NA"
bwondha_1st$percent_reduction <- percent.change(bwondha_1st$`mean.eps.March 2018`, bwondha_1st$`mean.eps.Feb 2018`)
bwondha_1st$time <- as.factor("`Feb 2018 to March 3wk PT`")
bwondha_1st <- bwondha_1st[-which(is.na(bwondha_1st$`mean.eps.Feb 2018`)==T),]
bwondha_1st <- bwondha_1st[-which(is.na(bwondha_1st$`mean.eps.March 2018`)==T),]
bwondha_1st <- bwondha_1st[-which(is.nan(bwondha_1st$percent_reduction)==T),]
bwondha_1st <- bwondha_1st[-which(is.infinite(bwondha_1st$percent_reduction)==T),]
bwondha_1st <- bwondha_1st %>% select(cid, School, time, percent_reduction)

musubi <- subset(data_2017_2018, School == "Musubi")
musubi_1st <- musubi %>% filter(Month=="March 2018" | Month=="March 2018 3wk PT")%>%
  select(cid, School, Month, mean.eps)
musubi_1st$cid <- as.factor(musubi_1st$cid)
musubi_1st <- musubi_1st %>% group_by(cid, School, Month) %>%
  summarise(mean.eps=mean(mean.eps))
musubi_1st <- as.data.frame(musubi_1st)
musubi_1st <- as.data.frame(reshape(musubi_1st, idvar = "cid", timevar = "Month", v.names = "mean.eps", direction = "wide")) #object has to be a dataframe not a tibble for this to work 
musubi_1st$percent_reduction <- "NA"
musubi_1st$percent_reduction <- percent.change(musubi_1st$`mean.eps.March 2018 3wk PT`, musubi_1st$`mean.eps.March 2018`)
musubi_1st$time <- as.factor("`Feb 2018 to March 3wk PT`")
musubi_1st <- musubi_1st[-which(is.na(musubi_1st$`mean.eps.March 2018`)==T),]
musubi_1st <- musubi_1st[-which(is.na(musubi_1st$`mean.eps.March 2018 3wk PT`)==T),]
musubi_1st <- musubi_1st[-which(is.nan(musubi_1st$percent_reduction)==T),]
musubi_1st <- musubi_1st[-which(is.infinite(musubi_1st$percent_reduction)==T),]
musubi_1st <- musubi_1st %>% select(cid, School, time, percent_reduction)


egg_reduction_all_data <- bind_rows(egg_reduction, bugoto_1st, bugoto_2nd, bwondha_1st, musubi_1st)
egg_reduction_all_data$time <- as.factor(egg_reduction_all_data$time)

egg_reduction_all_data$time <- factor(egg_reduction_all_data$time, levels = c("`Baseline to Aug 4wk PT`", "`June 2005 to July 4wk PT`",
                                                            "`May 2006 to July 4wk PT`" ,  "`Feb 2013 to March 3wk PT`",
                                                            "`May 2014 to June 4wk PT`", "`Sept 2017 to Oct 3wk PT`",
                                                            "`Feb 2018 to March 3wk PT`",  "`March 2018 to March 3wk PT`" ))
egg_reduction_all_data %>%
  ggplot(aes(x=time, y=percent_reduction, group=School, colour=School))+
  geom_point( size=1)+
  scale_y_continuous( limits=c(-100, 4000), minor_breaks = seq(-100 , 4000, 0.05))























