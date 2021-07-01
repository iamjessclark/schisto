# functions for running longitudinal model. 
require(tidyverse)
require(lubridate)
require(hablar) # this package has the sum_ function, where it will ignore na's 
#but if there are all na's it will return NA

#### Data Manipulation ####
loadkkdata <- function(){
  kk.dummy <- read.csv("data for michelle2.csv")
  kk.dummy <- kk.dummy[-which(is.na(kk.dummy$ID_trun)==TRUE),]
  kk.dummy <- kk.dummy %>%
    filter(!RecruitYr=="2003")%>%
    select(School, unique_CID, Year, AgeAtRecruit, 
           PreSm1, PreSm2,PreSm3,PreSm4,PreSm5,PreSm6,
           X1WkSm1,X1WkSm2,X1WkSm3,X1WkSm4,X1WkSm5,X1WkSm6,
           X4WksSm1,X4WksSm2,X4WksSm3,X4WksSm4,X4WksSm5,X4WksSm6,
           X6MthsSm1,X6MthsSm2,X6MthsSm3,X6MthsSm4,X6MthsSm5,X6MthsSm6,
           X6Mth1WkSm1,X6Mth1WkSm2,X6Mth1WkSm3,X6Mth1WkSm4,X6Mth1WkSm5,X6Mth1WkSm6)%>%
    pivot_longer(cols=PreSm1:X6Mth1WkSm6, names_to = "sampletime", values_to = "epslide")
  
  #kk.dummy <- kk.dummy[!(kk.dummy$Year=="2006" & kk.dummy$Week=="26"),] # remove the two data points for week 26 I think these will be an error
  study.dates <- read.csv("study_dates_2004_2018.csv")
  study.dates$start_date <- as.Date(dmy(study.dates$start_date))
  #study.dates$start_date <- dmy(study.dates$start_date)
  
  kk.dummy$study.date <- NA
  kk.dummy$study.date <- as.Date(kk.dummy$study.date)

  for(i in 1:nrow(kk.dummy)){
      if(kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="PreSm1"|
         kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="PreSm2"|
         kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="PreSm3"|
         kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="PreSm4"|
         kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="PreSm5"|
         kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="PreSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bugoto"& study.dates$timepoint=="pre" & study.dates$year=="2004"),5])
      } else if(kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X1WkSm1"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X1WkSm2"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X1WkSm3"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X1WkSm4"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X1WkSm5"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X1WkSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bugoto"& study.dates$timepoint=="1wk" & study.dates$year=="2004"),5])
      } else if(kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X4WksSm1"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X4WksSm2"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X4WksSm3"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X4WksSm4"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X4WksSm5"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X4WksSm6" ){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bugoto"& study.dates$timepoint=="4wk" & study.dates$year=="2004"),5])
      } else if(kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6MthsSm1"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6MthsSm2"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6MthsSm3"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6MthsSm4"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6MthsSm5"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6MthsSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bugoto"& study.dates$timepoint=="6m" & study.dates$year=="2004"),5])
      } else if(kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6Mth1WkSm1"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6Mth1WkSm2"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6Mth1WkSm3"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6Mth1WkSm4"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6Mth1WkSm5"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6Mth1WkSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bugoto"& study.dates$timepoint=="6m1wk" & study.dates$year=="2004"),5])
      } else if(kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="PreSm1"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="PreSm2"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="PreSm3"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="PreSm4"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="PreSm5"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="PreSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bwondha"& study.dates$timepoint=="pre" & study.dates$year=="2004"),5])
      } else if(kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X1WkSm1"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X1WkSm2"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X1WkSm3"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X1WkSm4"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X1WkSm5"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X1WkSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bwondha"& study.dates$timepoint=="1wk" & study.dates$year=="2004"),5])
      } else if(kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X4WksSm1"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X4WksSm2"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X4WksSm3"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X4WksSm4"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X4WksSm5"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X4WksSm6" ){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bwondha"& study.dates$timepoint=="4wk" & study.dates$year=="2004"),5])
      } else if(kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6MthsSm1"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6MthsSm2"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6MthsSm3"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6MthsSm4"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6MthsSm5"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6MthsSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bwondha"& study.dates$timepoint=="6m" & study.dates$year=="2004"),5])
      } else if(kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6Mth1WkSm1"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6Mth1WkSm2"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6Mth1WkSm3"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6Mth1WkSm4"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6Mth1WkSm5"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6Mth1WkSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bwondha"& study.dates$timepoint=="6m1wk" & study.dates$year=="2004"),5])
      } else if(kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="PreSm1"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="PreSm2"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="PreSm3"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="PreSm4"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="PreSm5"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="PreSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="musubi"& study.dates$timepoint=="pre" & study.dates$year=="2004"),5])
      } else if(kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X1WkSm1"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X1WkSm2"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X1WkSm3"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X1WkSm4"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X1WkSm5"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X1WkSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="musubi"& study.dates$timepoint=="1wk" & study.dates$year=="2004"),5])
      } else if(kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X4WksSm1"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X4WksSm2"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X4WksSm3"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X4WksSm4"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X4WksSm5"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X4WksSm6" ){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="musubi"& study.dates$timepoint=="4wk" & study.dates$year=="2004"),5])
      } else if(kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6MthsSm1"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6MthsSm2"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6MthsSm3"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6MthsSm4"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6MthsSm5"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6MthsSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="musubi"& study.dates$timepoint=="6m" & study.dates$year=="2004"),5])
      } else if(kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6Mth1WkSm1"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6Mth1WkSm2"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6Mth1WkSm3"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6Mth1WkSm4"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6Mth1WkSm5"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2004" & kk.dummy[i,5]=="X6Mth1WkSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="musubi"& study.dates$timepoint=="6m1wk" & study.dates$year=="2004"),5])
      } else if(kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="PreSm1"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="PreSm2"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="PreSm3"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="PreSm4"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="PreSm5"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="PreSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bugoto"& study.dates$timepoint=="1y" & study.dates$year=="2005"),5])
      } else if(kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X1WkSm1"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X1WkSm2"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X1WkSm3"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X1WkSm4"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X1WkSm5"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X1WkSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bugoto"& study.dates$timepoint=="1y1w" & study.dates$year=="2005"),5])
      } else if(kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X4WksSm1"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X4WksSm2"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X4WksSm3"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X4WksSm4"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X4WksSm5"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X4WksSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bugoto"& study.dates$timepoint=="1y4w" & study.dates$year=="2005"),5])
      } else if(kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="PreSm1"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="PreSm2"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="PreSm3"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="PreSm4"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="PreSm5"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="PreSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bwondha"& study.dates$timepoint=="1y" & study.dates$year=="2005"),5])
      } else if(kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X1WkSm1"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X1WkSm2"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X1WkSm3"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X1WkSm4"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X1WkSm5"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X1WkSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bwondha"& study.dates$timepoint=="1y1w" & study.dates$year=="2005"),5])
      } else if(kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X4WksSm1"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X4WksSm2"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X4WksSm3"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X4WksSm4"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X4WksSm5"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X4WksSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bwondha"& study.dates$timepoint=="1y4w" & study.dates$year=="2005"),5])
      } else if(kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="PreSm1"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="PreSm2"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="PreSm3"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="PreSm4"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="PreSm5"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="PreSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="musubi"& study.dates$timepoint=="1y" & study.dates$year=="2005"),5])
      } else if(kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X1WkSm1"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X1WkSm2"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X1WkSm3"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X1WkSm4"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X1WkSm5"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X1WkSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="musubi"& study.dates$timepoint=="1y1w" & study.dates$year=="2005"),5])
      } else if(kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X4WksSm1"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X4WksSm2"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X4WksSm3"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X4WksSm4"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X4WksSm5"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2005" & kk.dummy[i,5]=="X4WksSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="musubi"& study.dates$timepoint=="1y4w" & study.dates$year=="2005"),5])
      } else if(kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="PreSm1"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="PreSm2"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="PreSm3"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="PreSm4"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="PreSm5"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="PreSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bugoto"& study.dates$timepoint=="2y" & study.dates$year=="2006"),5])
      } else if(kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X1WkSm1"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X1WkSm2"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X1WkSm3"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X1WkSm4"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X1WkSm5"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X1WkSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bugoto"& study.dates$timepoint=="2y1w" & study.dates$year=="2006"),5])
      } else if(kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X4WksSm1"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X4WksSm2"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X4WksSm3"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X4WksSm4"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X4WksSm5"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X4WksSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bugoto"& study.dates$timepoint=="2y4w" & study.dates$year=="2006"),5])
      } else if(kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="PreSm1"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="PreSm2"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="PreSm3"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="PreSm4"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="PreSm5"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="PreSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bwondha"& study.dates$timepoint=="2y" & study.dates$year=="2006"),5])
      } else if(kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X1WkSm1"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X1WkSm2"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X1WkSm3"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X1WkSm4"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X1WkSm5"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X1WkSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bwondha"& study.dates$timepoint=="2y1w" & study.dates$year=="2006"),5])
      } else if(kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X4WksSm1"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X4WksSm2"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X4WksSm3"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X4WksSm4"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X4WksSm5"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X4WksSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bwondha"& study.dates$timepoint=="2y4w" & study.dates$year=="2006"),5])
      } else if(kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="PreSm1"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="PreSm2"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="PreSm3"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="PreSm4"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="PreSm5"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="PreSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="musubi"& study.dates$timepoint=="2y" & study.dates$year=="2006"),5])
      } else if(kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X1WkSm1"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X1WkSm2"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X1WkSm3"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X1WkSm4"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X1WkSm5"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X1WkSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="musubi"& study.dates$timepoint=="2y1w" & study.dates$year=="2006"),5])
      } else if(kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X4WksSm1"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X4WksSm2"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X4WksSm3"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X4WksSm4"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X4WksSm5"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2006" & kk.dummy[i,5]=="X4WksSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="musubi"& study.dates$timepoint=="2y4w" & study.dates$year=="2006"),5])
      } else if(kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="PreSm1"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="PreSm2"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="PreSm3"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="PreSm4"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="PreSm5"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="PreSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bugoto"& study.dates$timepoint=="pre" & study.dates$year=="2013"),5])
      } else if(kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X1WkSm1"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X1WkSm2"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X1WkSm3"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X1WkSm4"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X1WkSm5"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X1WkSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bugoto"& study.dates$timepoint=="1wk" & study.dates$year=="2013"),5])
      } else if(kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X4WksSm1"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X4WksSm2"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X4WksSm3"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X4WksSm4"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X4WksSm5"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X4WksSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bugoto"& study.dates$timepoint=="3wk" & study.dates$year=="2013"),5])
      } else if(kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="PreSm1"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="PreSm2"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="PreSm3"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="PreSm4"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="PreSm5"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="PreSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bwondha"& study.dates$timepoint=="pre" & study.dates$year=="2013"),5])
      } else if(kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X1WkSm1"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X1WkSm2"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X1WkSm3"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X1WkSm4"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X1WkSm5"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X1WkSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bwondha"& study.dates$timepoint=="1wk" & study.dates$year=="2013"),5])
      } else if(kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X4WksSm1"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X4WksSm2"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X4WksSm3"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X4WksSm4"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X4WksSm5"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X4WksSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bwondha"& study.dates$timepoint=="3wk" & study.dates$year=="2013"),5])
      } else if(kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="PreSm1"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="PreSm2"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="PreSm3"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="PreSm4"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="PreSm5"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="PreSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="musubi"& study.dates$timepoint=="pre" & study.dates$year=="2013"),5])
      } else if(kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X1WkSm1"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X1WkSm2"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X1WkSm3"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X1WkSm4"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X1WkSm5"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X1WkSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="musubi"& study.dates$timepoint=="1wk" & study.dates$year=="2013"),5])
      } else if(kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X4WksSm1"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X4WksSm2"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X4WksSm3"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X4WksSm4"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X4WksSm5"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2013" & kk.dummy[i,5]=="X4WksSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="musubi"& study.dates$timepoint=="3wk" & study.dates$year=="2013"),5])
      } else if(kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="PreSm1"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="PreSm2"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="PreSm3"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="PreSm4"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="PreSm5"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="PreSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bugoto"& study.dates$timepoint=="pre" & study.dates$year=="2014"),5])
      } else if(kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="X4WksSm1"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="X4WksSm2"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="X4WksSm3"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="X4WksSm4"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="X4WksSm5"|
                kk.dummy[i,1]=="Bugoto" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="X4WksSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bugoto"& study.dates$timepoint=="3wk" & study.dates$year=="2014"),5])
      } else if(kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="PreSm1"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="PreSm2"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="PreSm3"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="PreSm4"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="PreSm5"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="PreSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bwondha"& study.dates$timepoint=="pre" & study.dates$year=="2014"),5])
      } else if(kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="X4WksSm1"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="X4WksSm2"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="X4WksSm3"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="X4WksSm4"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="X4WksSm5"|
                kk.dummy[i,1]=="Bwondha" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="X4WksSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="bwondha"& study.dates$timepoint=="3wk" & study.dates$year=="2014"),5])
      } else if(kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="PreSm1"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="PreSm2"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="PreSm3"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="PreSm4"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="PreSm5"|
                kk.dummy[i,1]=="Musubi" & kk.dummy[i,3]=="2014" & kk.dummy[i,5]=="PreSm6"){
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="musubi"& study.dates$timepoint=="pre" & study.dates$year=="2014"),5])
      } else {
        kk.dummy[i, ncol(kk.dummy)] <- (study.dates[which(study.dates$school=="musubi"& study.dates$timepoint=="3wk" & study.dates$year=="2014"),5])
      } 
    }
  
  kk.dummy$MinBirthDate<- as.Date(kk.dummy$study.date)-months(12*kk.dummy$AgeAtRecruit)
  kk.dummy$study.date <- as.Date(kk.dummy$study.date)
  
  return(kk.dummy)
}
  
#### Loading the treatment dates and working out the timesteps ####

MortTsCalc <- function(dataload){
  study_dates <- read.csv("study_dates_2004_2018.csv")
  
  mort_df <- dataload %>%
    group_by(unique_CID, study.date)%>%
    summarise(total_eggs=sum_(epslide)*24) %>% # note the use of sum_() not sum()
    mutate(treated=NA)
  
  mort_df$treated <- ifelse(!is.na(mort_df$total_eggs) & mort_df$study.date=="2004-07-06"|
                              !is.na(mort_df$total_eggs) & mort_df$study.date=="2005-02-15"|
                              !is.na(mort_df$total_eggs) & mort_df$study.date=="2005-06-02"|
                              !is.na(mort_df$total_eggs) & mort_df$study.date=="2006-05-30"|
                              !is.na(mort_df$total_eggs) & mort_df$study.date=="2013-02-15"|
                              !is.na(mort_df$total_eggs) & mort_df$study.date=="2014-05-23"|
                              
                              !is.na(mort_df$total_eggs) & mort_df$study.date=="2004-07-08"|
                              !is.na(mort_df$total_eggs) & mort_df$study.date=="2005-02-16"|
                              !is.na(mort_df$total_eggs) & mort_df$study.date=="2005-06-10"|
                              !is.na(mort_df$total_eggs) & mort_df$study.date=="2006-05-29"|
                              !is.na(mort_df$total_eggs) & mort_df$study.date=="2013-02-20"|
                              !is.na(mort_df$total_eggs) & mort_df$study.date=="2014-05-19"|
                              
                              !is.na(mort_df$total_eggs) & mort_df$study.date=="2004-07-12"|
                              !is.na(mort_df$total_eggs) & mort_df$study.date=="2005-02-21"|
                              !is.na(mort_df$total_eggs) & mort_df$study.date=="2005-05-30"|
                              !is.na(mort_df$total_eggs) & mort_df$study.date=="2006-06-02"|
                              !is.na(mort_df$total_eggs) & mort_df$study.date=="2013-02-11"|
                              !is.na(mort_df$total_eggs) & mort_df$study.date=="2014-05-28"|  
                              
                              mort_df$total_eggs>=100 & mort_df$study.date=="2004-07-14"|
                              mort_df$total_eggs>=100 & mort_df$study.date=="2004-07-19"|
                              mort_df$total_eggs>=100 & mort_df$study.date=="2004-07-20"|
                              
                              mort_df$total_eggs>0 & mort_df$study.date=="2004-08-05"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2005-02-23"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2005-06-13"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2005-07-04"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2006-06-07"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2006-06-29"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2013-02-25"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2013-03-11"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2014-06-20"|
                              
                              mort_df$total_eggs>0 & mort_df$study.date=="2004-08-09"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2005-02-24"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2005-06-20"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2005-07-11"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2006-06-06"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2006-06-26"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2013-02-28"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2013-03-14"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2014-06-16"|
                              
                              mort_df$total_eggs>0 & mort_df$study.date=="2004-08-11"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2005-03-01"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2005-06-07"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2005-06-28"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2006-06-12"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2006-07-04"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2013-03-05"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2013-03-19"|
                              mort_df$total_eggs>0 & mort_df$study.date=="2014-06-25",1, 0)
  
  mort_df$treated[which(is.na(mort_df$treated))] <- 0
  
  mort_df <- as.data.frame(mort_df)
  
  timesteps <- as.data.frame(dataload %>%
              group_by(unique_CID, study.date) %>%
              dplyr::select(study.date, study.time.step))%>%
              distinct(.)
  
  mort_df <- merge(mort_df, timesteps, by=c("unique_CID", "study.date"))
  
  return(mort_df)
}  
