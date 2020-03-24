#### Schistosomiasis Longitudinal Study ####
# University of Glasgow #
# PI Poppy Lamberton #
# I.P. Dr. Jessica Clark #
# March 2020 #

#### Packages ####

require(tidyverse)
require(lubridate)
#### Data ####

#### 2004-2014 ####
# michelle data
# KK and CCA data 
long.data.04to14 <- read_csv("Clean long data 2015-07-09.csv")

# need to make a unique childs ID for each combination of school and age group

long.data.04to14$cid <- NA
long.data.04to14$AgeAtRecruit <- as.factor(long.data.04to14$AgeAtRecruit)
levels(long.data.04to14$AgeAtRecruit)[levels(long.data.04to14$AgeAtRecruit)=="6"] <- "06"
levels(long.data.04to14$AgeAtRecruit)[levels(long.data.04to14$AgeAtRecruit)=="7"] <- "07"
levels(long.data.04to14$AgeAtRecruit)[levels(long.data.04to14$AgeAtRecruit)=="8"] <- "08"
levels(long.data.04to14$AgeAtRecruit)[levels(long.data.04to14$AgeAtRecruit)=="9"] <- "09"
long.data.04to14$AgeAtRecruit <- as.character(long.data.04to14$AgeAtRecruit)

long.data.04to14$AGE <- as.factor(long.data.04to14$AGE)
levels(long.data.04to14$AGE)[levels(long.data.04to14$AGE)=="6"] <- "06"
levels(long.data.04to14$AGE)[levels(long.data.04to14$AGE)=="7"] <- "07"
levels(long.data.04to14$AGE)[levels(long.data.04to14$AGE)=="8"] <- "08"
levels(long.data.04to14$AGE)[levels(long.data.04to14$AGE)=="9"] <- "09"
long.data.04to14$AGE <- as.character(long.data.04to14$AGE)

#gen.uniqueID <- long.data.04to14 %>% 
#  group_by(School, AgeAtRecruit, RecruitYr) %>%
#  summarise(count=n())
#gen.uniqueID$yor <- NA
#for(i in 1:nrow(gen.uniqueID)){
 # if(gen.uniqueID[i,3]=="2003"){
  #  gen.uniqueID[i,ncol(gen.uniqueID)] <- "03"
  #} else if(gen.uniqueID[i,3]=="2004"){
   # gen.uniqueID[i,ncol(gen.uniqueID)] <- "04"
  #} else if(gen.uniqueID[i,3]=="2005"){
   # gen.uniqueID[i,ncol(gen.uniqueID)] <- "05"
  #} else if(gen.uniqueID[i,3]=="2006"){
   # gen.uniqueID[i,ncol(gen.uniqueID)] <- "06"
  #} else if(gen.uniqueID[i,3]=="2013"){
   # gen.uniqueID[i,ncol(gen.uniqueID)] <- "13"
  #} else {
    #gen.uniqueID[i,ncol(gen.uniqueID)] <- "14"
  #}
#}

#gen.uniqueID$yor <- as.factor(gen.uniqueID$yor)

long.data.04to14$schoolID <- NA

for(r in 1:nrow(long.data.04to14)){
  if(long.data.04to14[r,1]=="Bugoto"){
    long.data.04to14[r,ncol(long.data.04to14)] <- "BUG"
  } else if(long.data.04to14[r,1]=="Bwondha"){
    long.data.04to14[r, ncol(long.data.04to14)] <- "BWO"
  } else {
    long.data.04to14[r,ncol(long.data.04to14)] <- "MUS"
  }
}

long.data.04to14$shortYOR <- NA

for(r in 1:nrow(long.data.04to14)){
  if(long.data.04to14[r,12]=="2003"){
    long.data.04to14[r,ncol(long.data.04to14)] <- "03"
  } else if(long.data.04to14[r,12]=="2004"){
    long.data.04to14[r,ncol(long.data.04to14)] <- "04"
  } else if(long.data.04to14[r,12]=="2005"){
    long.data.04to14[r,ncol(long.data.04to14)] <- "05"
  } else if(long.data.04to14[r,12]=="2006"){
    long.data.04to14[r,ncol(long.data.04to14)] <- "06"
  } else if(long.data.04to14[r,12]=="2013"){
    long.data.04to14[r,ncol(long.data.04to14)] <- "13"
  } else {
    long.data.04to14[r,ncol(long.data.04to14)] <- "14"
  }
}

long.data.04to14$shortYOR <- as.factor(long.data.04to14$shortYOR)

long.data.04to14$cid <- as.factor(paste(long.data.04to14$schoolID, long.data.04to14$shortYOR, long.data.04to14$AGE, sep=""))

unique.ID <- long.data.04to14 %>% group_split(cid)

for(i in 1:length(unique.ID)){
  df <- unique.ID[[i]]
  df <- df %>% mutate(child.no=row_number())
  df$child.no <- as.factor(df$child.no)
    levels(df$child.no)[levels(df$child.no)=="1"] <- "01"
    levels(df$child.no)[levels(df$child.no)=="2"] <- "02"
    levels(df$child.no)[levels(df$child.no)=="3"] <- "03"
    levels(df$child.no)[levels(df$child.no)=="4"] <- "04"
    levels(df$child.no)[levels(df$child.no)=="5"] <- "05"
    levels(df$child.no)[levels(df$child.no)=="6"] <- "06"
    levels(df$child.no)[levels(df$child.no)=="7"] <- "07"
    levels(df$child.no)[levels(df$child.no)=="8"] <- "08"
    levels(df$child.no)[levels(df$child.no)=="9"] <- "09"
  df$cid_complete <- as.factor(paste(df$cid, df$child.no, sep=""))
  unique.ID[[i]] <- df
}

# this is the long data that has been a list now as a data frame again
long.data <- unique.ID[[1]]
for(i in 2:length(unique.ID)){
  long.data <- rbind(long.data, unique.ID[[i]])
}

long.data$Week <- as.factor(long.data$Week)


#### Pre treatment data ####

preT04to14 <- read_csv("Clean pre data 2015-08-27.csv")

preT04to14$cid <- NA
preT04to14$AgeAtRecruit <- as.factor(preT04to14$AgeAtRecruit)
levels(preT04to14$AgeAtRecruit)[levels(preT04to14$AgeAtRecruit)=="6"] <- "06"
levels(preT04to14$AgeAtRecruit)[levels(preT04to14$AgeAtRecruit)=="7"] <- "07"
levels(preT04to14$AgeAtRecruit)[levels(preT04to14$AgeAtRecruit)=="8"] <- "08"
levels(preT04to14$AgeAtRecruit)[levels(preT04to14$AgeAtRecruit)=="9"] <- "09"
preT04to14$AgeAtRecruit <- as.character(preT04to14$AgeAtRecruit)

preT04to14$Age <- as.factor(preT04to14$Age)
levels(preT04to14$Age)[levels(preT04to14$Age)=="6"] <- "06"
levels(preT04to14$Age)[levels(preT04to14$Age)=="7"] <- "07"
levels(preT04to14$Age)[levels(preT04to14$Age)=="8"] <- "08"
levels(preT04to14$Age)[levels(preT04to14$Age)=="9"] <- "09"
preT04to14$Age <- as.character(preT04to14$Age)

preT04to14$schoolID <- NA

for(r in 1:nrow(preT04to14)){
  if(preT04to14[r,1]=="Bugoto"){
    preT04to14[r,ncol(preT04to14)] <- "BUG"
  } else if(preT04to14[r,1]=="Bwondha"){
    preT04to14[r, ncol(preT04to14)] <- "BWO"
  } else {
    preT04to14[r,ncol(preT04to14)] <- "MUS"
  }
}

preT04to14$shortYOR <- NA

for(r in 1:nrow(preT04to14)){
  if(preT04to14[r,8]=="2003"){
    preT04to14[r,ncol(preT04to14)] <- "03"
  } else if(preT04to14[r,8]=="2004"){
    preT04to14[r,ncol(preT04to14)] <- "04"
  } else if(preT04to14[r,8]=="2005"){
    preT04to14[r,ncol(preT04to14)] <- "05"
  } else if(preT04to14[r,8]=="2006"){
    preT04to14[r,ncol(preT04to14)] <- "06"
  } else if(preT04to14[r,8]=="2013"){
    preT04to14[r,ncol(preT04to14)] <- "13"
  } else {
    preT04to14[r,ncol(preT04to14)] <- "14"
  }
}

preT04to14$shortYOR <- as.factor(preT04to14$shortYOR)

preT04to14$cid <- as.factor(paste(preT04to14$schoolID, preT04to14$shortYOR, preT04to14$Age, sep=""))

unique.ID.pre <- preT04to14 %>% group_split(cid)

for(i in 1:length(unique.ID.pre)){
  df <- unique.ID.pre[[i]]
  df <- df %>% mutate(child.no=row_number())
  df$child.no <- as.factor(df$child.no)
  levels(df$child.no)[levels(df$child.no)=="1"] <- "01"
  levels(df$child.no)[levels(df$child.no)=="2"] <- "02"
  levels(df$child.no)[levels(df$child.no)=="3"] <- "03"
  levels(df$child.no)[levels(df$child.no)=="4"] <- "04"
  levels(df$child.no)[levels(df$child.no)=="5"] <- "05"
  levels(df$child.no)[levels(df$child.no)=="6"] <- "06"
  levels(df$child.no)[levels(df$child.no)=="7"] <- "07"
  levels(df$child.no)[levels(df$child.no)=="8"] <- "08"
  levels(df$child.no)[levels(df$child.no)=="9"] <- "09"
  df$cid_complete <- as.factor(paste(df$cid, df$child.no, sep=""))
  unique.ID.pre[[i]] <- df
}

# this is the long data that has been a list now as a data frame again
long.data.pre <- unique.ID.pre[[1]]
for(i in 2:length(unique.ID.pre)){
  long.data.pre <- rbind(long.data.pre, unique.ID.pre[[i]])
}


#### 2017/2018 ####
# Just KK data 

# Bugoto 
# no.trts variable is number of treatments since baseline

KK.bug.BLT <- read_csv("kk_bg_17_03_13_clean.csv") # march 2017
KK.bug.BLT$time <- "A"
KK.bug.BLT$`Weeks Since Baseline` <- "Zero"
KK.bug.BLT$`Number of Treatments` <- "One"
KK.bug.BLT$`Weeks Since Treatment` <- "Baseline"
KK.bug.BLT$Month <- "March 2017"
KK.bug.BLT$Year <- "2017"
KK.bug.BLT <- KK.bug.BLT %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))
KK.bug.BLT$School <- "Bugoto"
#KK.bug.BLT$date <- as.Date(as.character(KK.bug.BLT$date), format = "%d/%m/%Y")
#KK.bug.BLT$date.trt <- as.Date(as.character("20/10/2017"), format = "%d/%m/%Y")
#KK.bug.BLT$days.since.trt <- difftime(KK.bug.BLT$date, KK.bug.BLT$date.trt, units = c("days"))
kk.bug.bl <- KK.bug.BLT %>% select(cid,School, time,  Month, `Weeks Since Treatment`, Year, mean.eps)
kk.bug.bl$event <- "Treated"

KK.bug.6mT.2017 <- read_csv("kk_bg_17_09_25_clean.csv") # Sept 2017
KK.bug.6mT.2017$time <- "B"
KK.bug.6mT.2017$`Weeks Since Baseline` <- "6 Months"
KK.bug.6mT.2017$`Number of Treatments` <- "Two"
KK.bug.6mT.2017$`Weeks Since Treatment` <- "28 Weeks"
KK.bug.6mT.2017$Month <- "Sept 2017"
KK.bug.6mT.2017$Year <- "2017"
KK.bug.6mT.2017$School <- "Bugoto"
KK.bug.6mT.2017 <- KK.bug.6mT.2017 %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))
#KK.bug.6mT.2017$date <- as.Date(as.character(KK.bug.6mT.2017$date), format = "%d/%m/%Y")
#KK.bug.6mT.2017$date.trt <- as.Date(as.character("06/10/2017"), format = "%d/%m/%Y")
#KK.bug.6mT.2017$days.since.trt <- difftime(KK.bug.6mT.2017$date.trt,KK.bug.6mT.2017$date, units = c("days"))
kk.bug.6mPostT <- KK.bug.6mT.2017 %>% select(cid, time, School, Month, `Weeks Since Treatment`, Year, mean.eps)
kk.bug.6mPostT$event <- "Treated"

KK.bug.6m3wk <- read_csv("kk_bg_17_10_25_clean.csv") # Oct 2017
KK.bug.6m3wk$time <- "C"
KK.bug.6m3wk$`Weeks Since Baseline` <- "6 months 3 Weeks"
KK.bug.6m3wk$`Number of Treatments` <- "Two"
KK.bug.6m3wk$`Weeks Since Treatment` <- "3 Weeks"
KK.bug.6m3wk$Month <- "Oct 2017"
KK.bug.6m3wk$Year <- "2017"
KK.bug.6m3wk$School <- "Bugoto"
KK.bug.6m3wk <- KK.bug.6m3wk %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))
kk.bug.3wkpostT <- KK.bug.6m3wk %>% select(cid, time,  School, Month, `Weeks Since Treatment`, Year, mean.eps)
kk.bug.3wkpostT$event <- "Check-up"

KK.bug.6m9wk <- read_csv("kk_bg_17_12_04_clean.csv") # Dec 2017
KK.bug.6m9wk$time <- "D"
KK.bug.6m9wk$`Weeks Since Baseline` <- "6 Months 9 Weeks"
KK.bug.6m9wk$`Number of Treatments` <- "Two"
KK.bug.6m9wk$`Weeks Since Treatment` <- "9 Weeks"
KK.bug.6m9wk$Year <- "2017"
KK.bug.6m9wk$Month <- "Dec 2017"
KK.bug.6m9wk$School <- "Bugoto"
KK.bug.6m9wk <- KK.bug.6m9wk %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))
kk.bug.9wekpostT <- KK.bug.6m9wk %>% select(cid, time,  School, Month, `Weeks Since Treatment`, Year, mean.eps)
kk.bug.9wekpostT$event <- "Check-up"

KK.bug.6mT2018 <- read_csv("kk_bg_18_03_05_clean.csv") # Mar 2018
KK.bug.6mT2018$time <- "E"
KK.bug.6mT2018$`Weeks Since Baseline` <- "1 year"
KK.bug.6mT2018$`Number of Treatments` <- "Three"
KK.bug.6mT2018$`Weeks Since Treatment` <- "28 Weeks"
KK.bug.6mT2018$Year <- "2018"
KK.bug.6mT2018$Month <- "March 2018"
KK.bug.6mT2018$School <- "Bugoto"
KK.bug.6mT2018 <- KK.bug.6mT2018 %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))
kk.bug.6mpT1ypBL <- KK.bug.6mT2018 %>% select(cid, time,  School, Month, `Weeks Since Treatment`, Year, mean.eps)
kk.bug.6mpT1ypBL$event <- "Treated"

KK.bug.6m3wk2018 <- read_csv("kk_bg_18_03_27_clean.csv") # Mar 2018
KK.bug.6m3wk2018$time <- "F"
KK.bug.6m3wk2018$`Weeks Since Baseline` <- "1 year 3 Weeks"
KK.bug.6m3wk2018$`Number of Treatments` <- "Three"
KK.bug.6m3wk2018$`Weeks Since Treatment` <- "3 Weeks"
KK.bug.6m3wk2018$Year <- "2018"
KK.bug.6m3wk2018$Month <- "March wk 3"
KK.bug.6m3wk2018$School <- "Bugoto"
KK.bug.6m3wk2018$sm_b <- as.numeric(KK.bug.6m3wk2018$sm_b)
KK.bug.6m3wk2018 <- KK.bug.6m3wk2018 %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))
kk.bug.6m3wkpT1ypBL <- KK.bug.6m3wk2018 %>% select(cid, time,  School, Month, `Weeks Since Treatment`, Year, mean.eps)
kk.bug.6m3wkpT1ypBL$event <- "Check-up"
# Bwondha 

KK.bwo.BLT <- read_csv("kk_bwondha_2017.09.20_clean.csv") # Sept 2017
KK.bwo.BLT$time <- "G"
KK.bwo.BLT$`Weeks Since Baseline` <- "Zero"
KK.bwo.BLT$`Number of Treatments` <- "One"
KK.bwo.BLT$`Weeks Since Treatment` <- "Baseline"
KK.bwo.BLT$Year <- "2017"
KK.bwo.BLT$Month <- "Sept 2017"
KK.bwo.BLT$School <- "Bwondha"
KK.bwo.BLT <- KK.bwo.BLT %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))%>% rename(cid=cid_full)
kk.bwo.bl <- KK.bwo.BLT %>% select(cid, time,  School, Month, `Weeks Since Treatment`, Year, mean.eps)
kk.bwo.bl$event <- "Treated"

KK.bwo.4mT <- read_csv("kk_bwondha_2018.02.21_clean.csv") # Feb 2018 6 monthsish
KK.bwo.4mT$time <- "H"
KK.bwo.4mT$`Weeks Since Baseline` <- "16 Weeks"
KK.bwo.4mT$`Number of Treatments` <- "One"
KK.bwo.4mT$`Weeks Since Treatment` <- "16 Weeks"
KK.bwo.4mT$Year <- "2018"
KK.bwo.4mT$Month <- "Feb 2018"
KK.bwo.4mT$School <- "Bwondha"
KK.bwo.4mT <- KK.bwo.4mT %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))%>% rename(cid=cid_full)
kk.bwo.4mt <- KK.bwo.4mT %>% select(cid, time,  School, Month, `Weeks Since Treatment`, Year, mean.eps)
kk.bwo.4mt$event <- "Treated"

KK.bwo.3wk <- read_csv("kk_bwondha_2018.03.14_clean.csv") # Mar 2018
KK.bwo.3wk$time <- "I"
KK.bwo.3wk$`Weeks Since Baseline` <- "19 Weeks"
KK.bwo.3wk$`Number of Treatments` <- "Two"
KK.bwo.3wk$`Weeks Since Treatment` <- "3 Weeks"
KK.bwo.3wk$Year <- "2018"
KK.bwo.3wk$Month <- "March wk 3"
KK.bwo.3wk$School <- "Bwondha"
KK.bwo.3wk <- KK.bwo.3wk %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))%>% rename(cid=cid_full)
kk.bwo.3wkpT <- KK.bwo.3wk %>% select(cid, time,  School, Month, `Weeks Since Treatment`, Year, mean.eps)
kk.bwo.3wkpT$event <- "Check-up"

# Musubi 

KK.mus.BLT <- read_csv("kk_musubi_2017.10.10_clean.csv") # Sep 2017
KK.mus.BLT$time <- "J"
KK.mus.BLT$`Weeks Since Baseline` <- "Zero"
KK.mus.BLT$`Number of Treatments` <- "One"
KK.mus.BLT$`Weeks Since Treatment` <- "Baseline"
KK.mus.BLT$Year <- "2017"
KK.mus.BLT$Month <- "Sept 2017"
KK.mus.BLT$School <- "Musubi"
KK.mus.BLT <- KK.mus.BLT %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))%>% rename(cid=cid_full)
kk.mus.bl <- KK.mus.BLT %>% select(cid, time,  School, Month, `Weeks Since Treatment`, Year, mean.eps)
kk.mus.bl$event <- "Treated"

KK.mus.6mT <- read_csv("kk_musubi_2018.02.28_clean.csv") # Feb/ March 2018 
KK.mus.6mT$time <- "K"
KK.mus.6mT$`Weeks Since Baseline` <- "28 Weeks"
KK.mus.6mT$`Number of Treatments` <- "Two"
KK.mus.6mT$`Weeks Since Treatment` <- "28 Weeks"
KK.mus.6mT$Year <- "2018"
KK.mus.6mT$Month <- "March 2018"
KK.mus.6mT$School <- "Musubi"
KK.mus.6mT <- KK.mus.6mT %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))%>% rename(cid=cid_full)
kk.mus.6mpT <- KK.mus.6mT %>% select(cid, time,  School, Month, `Weeks Since Treatment`, Year, mean.eps)
kk.mus.6mpT$event <- "Treated"

KK.mus.3wk <- read_csv("kk_musubi_2018.03.26_clean.csv") # Mar 2018
KK.mus.3wk$time <- "L"
KK.mus.3wk$`Weeks Since Baseline` <- "31 Weeks"
KK.mus.3wk$`Number of Treatments` <- "Two"
KK.mus.3wk$`Weeks Since Treatment` <- "3 Weeks"
KK.mus.3wk$Month <- "March wk 3"
KK.mus.3wk$Year <- "2018"
KK.mus.3wk$School <- "Musubi"
KK.mus.3wk <- KK.mus.3wk %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))%>% rename(cid=cid_full)
kk.mus.3wkpT <- KK.mus.3wk %>% select(cid, time, School, Month, `Weeks Since Treatment`, Year, mean.eps)
kk.mus.3wkpT$event <- "Check-up"

recent.data <- rbind(kk.bug.bl, kk.bug.6mPostT, kk.bug.3wkpostT, kk.bug.9wekpostT, kk.bug.6mpT1ypBL, kk.bug.6m3wkpT1ypBL, kk.bwo.bl, kk.bwo.4mt, kk.bwo.3wkpT, kk.mus.bl,kk.mus.6mpT,kk.mus.3wkpT   )
recent.data$mean.eps[is.nan(recent.data$mean.eps)] <- NA
recent.data$event <- as.factor(recent.data$event)
recent.data$School <- as.factor(recent.data$School)
recent.data$time <- as.factor(recent.data$time)
recent.data$`Weeks Since Treatment` <- as.factor(recent.data$`Weeks Since Treatment`)
recent.data$Year <- as.factor(recent.data$Year)
recent.data$Month <- as.factor(recent.data$Month)
require(tidytext)


recent.data.means <- recent.data %>% group_by(School,Month, `Weeks Since Treatment`, Year, time,  event) %>%
  summarise(`Mean Eggs Per Slide`=mean(mean.eps, na.rm = T), n=n(), sd=sd(mean.eps, na.rm=T), std.error=sd/sqrt(n))


#### Plot of EPS over time ####
recent.data$Month <- factor(recent.data$Month, levels = c("March 2017", "Sept 2017", "Oct 2017", "Dec 2017", "Feb 2018", "March 2018", "March wk 3"))
levels(recent.data$Month)
theme_set(theme_classic())

plot <- ggplot(recent.data.means, aes(x = Month, y = `Mean Eggs Per Slide`, group = School, colour=School, shape=event)) + 
  geom_point(size = 3) + 
  geom_line()+
  geom_errorbar(data = recent.data.means, aes(ymin=`Mean Eggs Per Slide`- std.error, ymax=`Mean Eggs Per Slide`+std.error), width=.2)+
  xlab("Sampling Period")+
  #facet_grid(. ~ Year, switch="x", scales = "free_x") +
  theme(strip.placement="outside",
        strip.background=element_rect(colour=NA),
        panel.spacing.x=unit(0,"lines"),
        panel.border=element_rect(colour="grey50", fill=NA))

plot
