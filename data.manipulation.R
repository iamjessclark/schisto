#### Schistosomiasis Longitudinal Study ####
# University of Glasgow #
# PI Poppy Lamberton #
# I.P. Dr. Jessica Clark #
# March 2020 #

#### Packages ####

require(tidyverse)

#### Data ####

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

# Pre treatment data 

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


# 2017/2018
# Just KK data 

# Bugoto 
# no.trts variable is number of treatments since baseline

KK.bug.BLT <- read_csv("kk_bg_17_03_13_clean.csv") # march 2017
KK.bug.BLT$time <- "baseline"
KK.bug.BLT$wks.since.BL <- "zero"
KK.bug.BLT$no.trts <- "One"
KK.bug.BLT <- KK.bug.BLT %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))
#KK.bug.BLT$date <- as.Date(as.character(KK.bug.BLT$date), format = "%d/%m/%Y")
#KK.bug.BLT$date.trt <- as.Date(as.character("20/10/2017"), format = "%d/%m/%Y")
#KK.bug.BLT$days.since.trt <- difftime(KK.bug.BLT$date, KK.bug.BLT$date.trt, units = c("days"))
kk.bug.bl <- KK.bug.BLT %>% select(cid, date, time, wks.since.BL, no.trts, mean.eps)

KK.bug.6mT.2017 <- read_csv("kk_bg_17_09_25_clean.csv") # Sept 2017
KK.bug.6mT.2017$time <- "6months2017"
KK.bug.6mT.2017$wks.since.BL <- "Twenty-Eight"
KK.bug.6mT.2017$no.trts <- "Two"
KK.bug.6mT.2017 <- KK.bug.6mT.2017 %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))
#KK.bug.6mT.2017$date <- as.Date(as.character(KK.bug.6mT.2017$date), format = "%d/%m/%Y")
#KK.bug.6mT.2017$date.trt <- as.Date(as.character("06/10/2017"), format = "%d/%m/%Y")
#KK.bug.6mT.2017$days.since.trt <- difftime(KK.bug.6mT.2017$date.trt,KK.bug.6mT.2017$date, units = c("days"))
kk.bug.6mPostT <- KK.bug.6mT.2017 %>% select(cid, date, time, wks.since.BL, no.trts, mean.eps)

KK.bug.6m3wk <- read_csv("kk_bg_17_10_25_clean.csv") # Oct 2017
KK.bug.6m3wk$time <- "3weeks"
KK.bug.6m3wk$wks.since.BL <- "Thirty-Two"
KK.bug.6m3wk$no.trts <- "Two"
KK.bug.6m3wk <- KK.bug.6m3wk %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))
kk.bug.3wkpostT <- KK.bug.6m3wk %>% select(cid, date, time, wks.since.BL, no.trts, mean.eps)

KK.bug.6m9wk <- read_csv("kk_bg_17_12_04_clean.csv") # Dec 2017
KK.bug.6m9wk$time <- "9weeks"
KK.bug.6m9wk$wks.since.BL <- "Thirty-Eight"
KK.bug.6m9wk$no.trts <- "Two"
KK.bug.6m9wk <- KK.bug.6m9wk %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))
kk.bug.9wekpostT <- KK.bug.6m9wk %>% select(cid, date, time, wks.since.BL, no.trts, mean.eps)

KK.bug.6mT2018 <- read_csv("kk_bg_18_03_05_clean.csv") # Mar 2018
KK.bug.6mT2018$time <- "6months2018"
KK.bug.6mT2018$wks.since.BL <- "Fifty-one"
KK.bug.6mT2018$no.trts <- "Three"
KK.bug.6mT2018 <- KK.bug.6mT2018 %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))
kk.bug.6mpT1ypBL <- KK.bug.6mT2018 %>% select(cid, date, time, wks.since.BL, no.trts, mean.eps)

KK.bug.6m3wk2018 <- read_csv("kk_bg_18_03_27_clean.csv") # Mar 2018
KK.bug.6m3wk2018$time <- "3weeks2018"
KK.bug.6m3wk2018$wks.since.BL <- "Fifty-four"
KK.bug.6m3wk2018$no.trts <- "Three"
KK.bug.6m3wk2018$sm_b <- as.numeric(KK.bug.6m3wk2018$sm_b)
KK.bug.6m3wk2018 <- KK.bug.6m3wk2018 %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))
kk.bug.6m3wkpT1ypBL <- KK.bug.6m3wk2018 %>% select(cid, date, time, wks.since.BL, no.trts, mean.eps)

# Bwondha 

KK.bwo.BLT <- read_csv("kk_bwondha_2017.09.20_clean.csv") # Sept 2017
KK.bwo.BLT$time <- "baseline"
KK.bwo.BLT$wks.since.BL <- "zero"
KK.bwo.BLT$no.trts <- "One"
KK.bwo.BLT <- KK.bwo.BLT %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))%>% rename(cid=cid_full)
kk.bwo.bl <- KK.bwo.BLT %>% select(cid, date, time, wks.since.BL, no.trts, mean.eps)

KK.bwo.4mT <- read_csv("kk_bwondha_2018.02.21_clean.csv") # Feb 2018 6 monthsish
KK.bwo.4mT$time <- "4months"
KK.bwo.4mT$wks.since.BL <- "Twenty"
KK.bwo.4mT$no.trts <- "Two"
KK.bwo.4mT <- KK.bwo.4mT %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))%>% rename(cid=cid_full)
kk.bwo.4mpT <- KK.bwo.4mT %>% select(cid, date, time, wks.since.BL, no.trts, mean.eps)

KK.bwo.3wk <- read_csv("kk_bwondha_2018.03.14_clean.csv") # Mar 2018
KK.bwo.3wk$time <- "3weeks2018"
KK.bwo.3wk$wks.since.BL <- "Twenty-three"
KK.bwo.3wk$no.trts <- "Two"
KK.bwo.3wk <- KK.bwo.3wk %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))%>% rename(cid=cid_full)
kk.bwo.3wkpT <- KK.bwo.3wk %>% select(cid, date, time, wks.since.BL, no.trts, mean.eps)

# Musubi 

KK.mus.BLT <- read_csv("kk_musubi_2017.10.10_clean.csv") # Sep 2017
KK.mus.BLT$time <- "baseline"
KK.mus.BLT$wks.since.BL <- "Zero"
KK.mus.BLT$no.trts <- "One"
KK.mus.BLT <- KK.mus.BLT %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))%>% rename(cid=cid_full)
kk.mus.bl <- KK.mus.BLT %>% select(cid, date, time, wks.since.BL, no.trts, mean.eps)

KK.mus.6mT <- read_csv("kk_musubi_2018.02.28_clean.csv") # Feb/ March 2018 
KK.mus.6mT$time <- "6months"
KK.mus.6mT$wks.since.BL <- "Twenty-eight"
KK.mus.6mT$no.trts <- "Two"
KK.mus.6mT <- KK.mus.6mT %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))%>% rename(cid=cid_full)
kk.mus.6mpT <- KK.mus.6mT %>% select(cid, date, time, wks.since.BL, no.trts, mean.eps)

KK.mus.3wk <- read_csv("kk_musubi_2018.03.26_clean.csv") # Mar 2018
KK.mus.3wk$time <- "3weeks2018"
KK.mus.3wk$wks.since.BL <- "Thirty-one"
KK.mus.3wk$no.trts <- "Two"
KK.mus.3wk <- KK.mus.3wk %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))%>% rename(cid=cid_full)
kk.mus.3wkpT <- KK.mus.3wk %>% select(cid, date, time, wks.since.BL, no.trts, mean.eps)

recent.data <- rbind(kk.bug.bl, kk.bug.6mPostT, kk.bug.3wkpostT, kk.bug.9wekpostT, kk.bug.6mpT1ypBL, kk.bug.6m3wkpT1ypBL, kk.bwo.bl, kk.bwo.4mpT, kk.bwo.3wkpT, kk.mus.bl, kk.mus.6mpT, kk.mus.3wkpT)
recent.data$mean.eps[is.nan(recent.data$mean.eps)] <- NA



