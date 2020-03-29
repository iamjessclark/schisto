#### Schistosomiasis Longitudinal Study ####
# University of Glasgow #
# PI Poppy Lamberton #
# I.P. Dr. Jessica Clark #
# March 2020 #

#### Packages ####

require(tidyverse)
#### Data ####

source("longitudinal_functions2.R")

# 2004 - 2014 #

long_data <- read_csv("Clean long data 2015-07-09.csv")

long_data <- long_data_func(long_data)


long_data$Month <- factor(long_data$Month, levels = c("Baseline 2004", "July 04 1wk PT", "Aug 4wk PT", "Feb 2005", "Feb 6mth 1wk", 
                                                                          "June 2005", "July 1wk PT", "July 05 4wk PT", "May 2006", "June 1wk PT", "July 06 4wk PT", "Feb 2013", 
                                                                          "Feb 1wk PT","March 3wk PT", "May 2014",  "June 4wk PT" ,"March 2017", "Sept 2017", 
                                                                          "Oct 3wk PT", "Dec 9wk PT", "Feb 2018", "March 2018", "March 2018 3wk PT" ))

# long data pzq naive 6 year olds # 

pzq_naive_6yo <- long_data %>% filter(AgeAtRecruit =="06" & RecruitYr !="2003" & Week == "0" & AGE == "06") %>%
  select(CID, School, Month, event, MeanSm)

pzq_naive_6yo$prevalence <- "NA"

pzq_naive_6yo <- pzq_naive_6yo[-which(is.na(pzq_naive_6yo$MeanSm)==T),]

for(r in 1:nrow(pzq_naive_6yo)){
  if(pzq_naive_6yo[r,6]>0){
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


#### Full pzq naive 6 yearolds ####

pzq_naive_full <- bind_rows(pzq_naive_6yo, pzq_naive_17_18) 
pzq_naive_full$Month <- factor(pzq_naive_full$Month, levels = c("Baseline 2004", "June 2005", "May 2006", "Feb 2013", 
                                                                "May 2014", "March 2017", "Sept 2017", 
                                                                "Feb 2018", "March 2018" ))

#### Figures ####

# infection prevalence plot #

pzq_naive_full%>% 
  mutate_if(is.character, as.factor)%>%
  count(Month, School, prevalence) %>% 
  group_by(Month) %>% 
  mutate(sum=sum(n)) %>% 
  mutate(proportion = n/sum) %>% 
  ggplot(aes(y=proportion, x=Month, fill=School)) +
  geom_col(position = "dodge", fill="#F6AE2D")+
  theme(axis.text.x = element_text(angle=90))+
  facet_grid(~School, scales = "free_x")+
  ylab("Infection Prevalence")+
  ggsave("prevalence_pzq_naive_6yo.pdf")


# Infection intensity plot #

pzq.errorbars <- pzq_naive_full %>% group_by(School, Month) %>%
  summarise(mean_eggs=mean(`mean eggs per slide`, na.rm = T), n=n(), sd=sd(`mean eggs per slide`, na.rm=T), std.error=sd/sqrt(n))

pzq.errorbars%>% 
  ggplot(aes(y=mean_eggs, x=Month, group=School)) +
  geom_point(colour="#F6AE2D", size=4)+
  geom_line(colour="#2F4858")+
  geom_errorbar(aes(ymin=mean_eggs- std.error, ymax=mean_eggs+std.error),colour="#2F4858", width=.2)+
  theme(axis.text.x = element_text(angle=90))+
  facet_grid(~School, scales = "free_x")+
  ylab("Mean Eggs Per Slide")+ xlab("Sampling Time")+
  ggsave("intensity_pzq_naive_6yo.pdf")

# Egg reduction 

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


# not using any children who don't have both counts
err_trt_2004 <- as.data.frame(cbind(as.character(err_long_data2$CID), err_long_data2$`MeanSm.Baseline 2004`, err_long_data2$`MeanSm.Aug 4wk PT`)
err_trt_2004 <- err_trt_2004 %>% mutate_if(is.character, as.factor)%>% rename(cid="V1", `baseline 2004`="V2", `Aug 4wk PT`="V3")
err_trt_2004 <- err_trt_2004[-which(is.na(err_trt_2004$`baseline 2004`)==T),]
err_trt_2004 <- err_trt_2004[-which(is.na(err_trt_2004$`Aug 4wk PT`)==T),]

err_trt_2005 <- as.data.frame(cbind(as.character(err_long_data2$CID), err_long_data2$`MeanSm.June 2005`, err_long_data2$`MeanSm.July 05 4wk PT`))
err_trt_2005 <- err_trt_2005 %>% mutate_if(is.character, as.factor)%>% rename(cid="V1", `June 2005`="V2", `July 05 4wk PT`="V3")
err_trt_2005 <- err_trt_2005[-which(is.na(err_trt_2005$`June 2005`)==T),]
err_trt_2005 <- err_trt_2005[-which(is.na(err_trt_2005$`July 05 4wk PT`)==T),]

err_trt_2006 <- as.data.frame(cbind(as.character(err_long_data2$CID), err_long_data2$`MeanSm.May 2006`, err_long_data2$`MeanSm.July 06 4wk PT`))
err_trt_2006 <- err_trt_2006 %>% mutate_if(is.character, as.factor)%>% rename(cid="V1", `May 2006`="V2", `July 06 4wk PT`="V3")

err_trt_2013 <- as.data.frame(cbind(as.character(err_long_data2$CID), err_long_data2$`MeanSm.Feb 2013`, err_long_data2$`MeanSm.March 3wk PT`))
err_trt_2013 <- err_trt_2013 %>% mutate_if(is.character, as.factor)%>% rename(cid="V1", `Feb 2013`="V2", `March 3wk PT`="V3")

err_trt_2014 <- as.data.frame(cbind(as.character(err_long_data2$CID), err_long_data2$`MeanSm.May 2014`, err_long_data2$`MeanSm.June 4wk PT`))
err_trt_2014 <- err_trt_2014 %>% mutate_if(is.character, as.factor)%>% rename(cid="V1", `May 2013`="V2", `June 4wk PT`="V3")

















