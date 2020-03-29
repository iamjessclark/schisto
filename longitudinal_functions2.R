# functions for schisto longitudinal paper #

# function for the 04-14 data

long_data_func <- function(x){

  x$Week <- as.factor(x$Week)
  
  x$AgeAtRecruit <- as.factor(x$AgeAtRecruit)
  
  levels(x$AgeAtRecruit)[levels(x$AgeAtRecruit)=="6"] <- "06"
  levels(x$AgeAtRecruit)[levels(x$AgeAtRecruit)=="7"] <- "07"
  levels(x$AgeAtRecruit)[levels(x$AgeAtRecruit)=="8"] <- "08"
  levels(x$AgeAtRecruit)[levels(x$AgeAtRecruit)=="9"] <- "09"
  x$AgeAtRecruit <- as.character(x$AgeAtRecruit)
  
  x$AGE <- as.factor(x$AGE)
  levels(x$AGE)[levels(x$AGE)=="6"] <- "06"
  levels(x$AGE)[levels(x$AGE)=="7"] <- "07"
  levels(x$AGE)[levels(x$AGE)=="8"] <- "08"
  levels(x$AGE)[levels(x$AGE)=="9"] <- "09"
  x$AGE <- as.character(x$AGE)
  
  x$Month <- NA
  
  for(i in 1:nrow(x)){
    if(x[i,2]=="2004" & x[i,4]=="0"){
      x[i,ncol(x)] <- "Baseline 2004"
    } else if(x[i,2]=="2004" && x[i,4]=="1"){
      x[i,ncol(x)] <- "July 04 1wk PT"
    } else if(x[i,2]=="2004" && x[i,4]=="4"){
      x[i,ncol(x)] <- "Aug 4wk PT"
    } else if(x[i,2]=="2004" && x[i,4]=="26"){
      x[i,ncol(x)] <- "Feb 2005"
    } else if(x[i,2]=="2004" && x[i,4]=="27"){
      x[i,ncol(x)] <- "Feb 6mth 1wk"
    } else if(x[i,2]=="2005" && x[i,4]=="0"){
      x[i,ncol(x)] <- "June 2005"
    } else if(x[i,2]=="2005" && x[i,4]=="1"){
      x[i,ncol(x)] <- "July 1wk PT"
    } else if(x[i,2]=="2005" && x[i,4]=="4"){
      x[i,ncol(x)] <- "July 05 4wk PT"
    } else if(x[i,2]=="2006" && x[i,4]=="0"){
      x[i,ncol(x)] <- "May 2006"
    } else if(x[i,2]=="2006" && x[i,4]=="1"){
      x[i,ncol(x)] <- "June 1wk PT"
    } else if(x[i,2]=="2006" && x[i,4]=="4"){
      x[i,ncol(x)] <- "July 06 4wk PT"
    } else if(x[i,2]=="2013" && x[i,4]=="0"){
      x[i,ncol(x)] <- "Feb 2013"
    } else if(x[i,2]=="2013" && x[i,4]=="1"){
      x[i,ncol(x)] <- "Feb 1wk PT"
    } else if(x[i,2]=="2013" && x[i,4]=="4"){
      x[i,ncol(x)] <- "March 3wk PT"
    } else if(x[i,2]=="2014" && x[i,4]=="0"){
      x[i,ncol(x)] <- "May 2014"
    } else {
      x[i,ncol(x)] <- "June 4wk PT"
    }
  } 
  
  x$Month <- as.factor(x$Month)
  
  x$event <- NA
  
  for(i in 1:nrow(x)){
    if(x[i,ncol(x)-1]=="Baseline 2004"){
      x[i,ncol(x)] <- "Treated"
    } else if(x[i,ncol(x)-1]=="July 04 1wk PT"){
      x[i,ncol(x)] <- "Check-up"
    } else if(x[i,ncol(x)-1]=="Aug 4wk PT"){
      x[i,ncol(x)] <- "Check-up"
    } else if(x[i,ncol(x)-1]=="Feb 2005"){
      x[i,ncol(x)] <- "Check-up"
    } else if(x[i,ncol(x)-1]=="Feb 6mth 1wk"){
      x[i,ncol(x)] <- "Check-up"
    } else if(x[i,ncol(x)-1]=="June 2005"){
      x[i,ncol(x)] <- "Treated"
    } else if(x[i,ncol(x)-1]=="July 1wk PT"){
      x[i,ncol(x)] <- "Check-up"
    } else if(x[i,ncol(x)-1]=="July 05 4wk PT"){
      x[i,ncol(x)] <- "Check-up"
    } else if(x[i,ncol(x)-1]=="May 2006"){
      x[i,ncol(x)] <- "Treated"
    } else if(x[i,ncol(x)-1]=="June 1wk PT"){
      x[i,ncol(x)] <- "Check-up"
    } else if(x[i,ncol(x)-1]=="July 06 4wk PT"){
      x[i,ncol(x)] <- "Check-up"
    } else if(x[i,ncol(x)-1]=="Feb 2013"){
      x[i,ncol(x)] <- "Treated"
    } else if(x[i,ncol(x)-1]=="Feb 1wk PT"){
      x[i,ncol(x)] <- "Check-up"
    } else if(x[i,ncol(x)-1]=="March 3wk PT"){
      x[i,ncol(x)] <- "Check-up"
    } else if(x[i,ncol(x)-1]=="May 2014"){
      x[i,ncol(x)] <- "Treated"
    } else {
      x[i,ncol(x)] <- "Check-up"
    }
  } 
  
  x$event <- as.factor(x$event)
  
  return(x)

}

# function for the 17/18 data

recent_data <- function(KK.bug.BLT, KK.bug.6mT.2017, KK.bug.6m3wk, KK.bug.6m9wk, KK.bug.6mT2018, KK.bug.6m3wk2018, 
         KK.bwo.BLT, KK.bwo.4mT, KK.bwo.3wk, KK.mus.BLT, KK.mus.6mT, KK.mus.3wk){

  KK.bug.BLT$time <- "A"
  KK.bug.BLT$`Weeks Since Baseline` <- "Zero"
  KK.bug.BLT$`Number of Treatments` <- "One"
  KK.bug.BLT$`Weeks Since Treatment` <- "Baseline"
  KK.bug.BLT$Month <- "March 2017"
  KK.bug.BLT$Year <- "2017"
  KK.bug.BLT <- KK.bug.BLT %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))
  KK.bug.BLT$School <- "Bugoto"
  kk.bug.bl <- KK.bug.BLT %>% select(cid,School, time,  Month, `Weeks Since Treatment`, Year, mean.eps)
  kk.bug.bl$event <- "Treated"
  
  KK.bug.6mT.2017$time <- "B"
  KK.bug.6mT.2017$`Weeks Since Baseline` <- "6 Months"
  KK.bug.6mT.2017$`Number of Treatments` <- "Two"
  KK.bug.6mT.2017$`Weeks Since Treatment` <- "28 Weeks"
  KK.bug.6mT.2017$Month <- "Sept 2017"
  KK.bug.6mT.2017$Year <- "2017"
  KK.bug.6mT.2017$School <- "Bugoto"
  KK.bug.6mT.2017 <- KK.bug.6mT.2017 %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))
  kk.bug.6mPostT <- KK.bug.6mT.2017 %>% select(cid, time, School, Month, `Weeks Since Treatment`, Year, mean.eps)
  kk.bug.6mPostT$event <- "Treated"
  
  KK.bug.6m3wk$time <- "C"
  KK.bug.6m3wk$`Weeks Since Baseline` <- "6 months 3 Weeks"
  KK.bug.6m3wk$`Number of Treatments` <- "Two"
  KK.bug.6m3wk$`Weeks Since Treatment` <- "3 Weeks"
  KK.bug.6m3wk$Month <- "Oct 3wk PT"
  KK.bug.6m3wk$Year <- "2017"
  KK.bug.6m3wk$School <- "Bugoto"
  KK.bug.6m3wk <- KK.bug.6m3wk %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))
  kk.bug.3wkpostT <- KK.bug.6m3wk %>% select(cid, time,  School, Month, `Weeks Since Treatment`, Year, mean.eps)
  kk.bug.3wkpostT$event <- "Check-up"
  
  KK.bug.6m9wk$time <- "D"
  KK.bug.6m9wk$`Weeks Since Baseline` <- "6 Months 9 Weeks"
  KK.bug.6m9wk$`Number of Treatments` <- "Two"
  KK.bug.6m9wk$`Weeks Since Treatment` <- "9 Weeks"
  KK.bug.6m9wk$Year <- "2017"
  KK.bug.6m9wk$Month <- "Dec 9wk PT"
  KK.bug.6m9wk$School <- "Bugoto"
  KK.bug.6m9wk <- KK.bug.6m9wk %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))
  kk.bug.9wekpostT <- KK.bug.6m9wk %>% select(cid, time,  School, Month, `Weeks Since Treatment`, Year, mean.eps)
  kk.bug.9wekpostT$event <- "Check-up"
  
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
  
  KK.bug.6m3wk2018$time <- "F"
  KK.bug.6m3wk2018$`Weeks Since Baseline` <- "1 year 3 Weeks"
  KK.bug.6m3wk2018$`Number of Treatments` <- "Three"
  KK.bug.6m3wk2018$`Weeks Since Treatment` <- "3 Weeks"
  KK.bug.6m3wk2018$Year <- "2018"
  KK.bug.6m3wk2018$Month <- "March 2018 3wk PT"
  KK.bug.6m3wk2018$School <- "Bugoto"
  KK.bug.6m3wk2018$sm_b <- as.numeric(KK.bug.6m3wk2018$sm_b)
  KK.bug.6m3wk2018 <- KK.bug.6m3wk2018 %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))
  kk.bug.6m3wkpT1ypBL <- KK.bug.6m3wk2018 %>% select(cid, time,  School, Month, `Weeks Since Treatment`, Year, mean.eps)
  kk.bug.6m3wkpT1ypBL$event <- "Check-up"
  # Bwondha 
  
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
  
  KK.bwo.3wk$time <- "I"
  KK.bwo.3wk$`Weeks Since Baseline` <- "19 Weeks"
  KK.bwo.3wk$`Number of Treatments` <- "Two"
  KK.bwo.3wk$`Weeks Since Treatment` <- "3 Weeks"
  KK.bwo.3wk$Year <- "2018"
  KK.bwo.3wk$Month <- "March 2018 3wk PT"
  KK.bwo.3wk$School <- "Bwondha"
  KK.bwo.3wk <- KK.bwo.3wk %>% rowwise() %>% mutate(mean.eps=mean(c(sm_a, sm_b), na.rm = T))%>% rename(cid=cid_full)
  kk.bwo.3wkpT <- KK.bwo.3wk %>% select(cid, time,  School, Month, `Weeks Since Treatment`, Year, mean.eps)
  kk.bwo.3wkpT$event <- "Check-up"
  
  # Musubi 
  
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
  
  KK.mus.3wk$time <- "L"
  KK.mus.3wk$`Weeks Since Baseline` <- "31 Weeks"
  KK.mus.3wk$`Number of Treatments` <- "Two"
  KK.mus.3wk$`Weeks Since Treatment` <- "3 Weeks"
  KK.mus.3wk$Month <- "March 2018 3wk PT"
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
  
  return(recent.data)
}


#### egg reduction rate longitudinal data ####





































