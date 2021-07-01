source("longmodfuncs.R")
require(runjags)

# loading data 
dataload <- loadkkdata()

#### get birth timesteps ####
birthdates <- dataload %>%
  group_by(unique_CID) %>%
  filter(MinBirthDate==min(MinBirthDate)) %>%
  dplyr::select(unique_CID, MinBirthDate)

# wrt the birthdate of the oldest & earliest participant, 
#### get the study time step ####
# for each sampling period. 
int <- interval(as.Date(min(dataload$MinBirthDate)), dataload$study.date)
dataload$study.time.step <- round(time_length(int, "week")/3)

intbirth <- interval(as.Date(min(dataload$MinBirthDate)), dataload$MinBirthDate)
dataload$birthtimestep <- round(time_length(intbirth, "week")/3)+1 # plus 1 because R indexes on 1 and won't start on 0 for the first birthdate timestep

birthtimestep <- dataload %>%
  group_by(unique_CID)%>%
  dplyr::select(birthtimestep)%>%
  group_by(unique_CID)%>%
  summarise(birthtimestep=min(birthtimestep))

btsCID <- as_vector(birthtimestep %>% dplyr::select(unique_CID))

birthtimestep <- as_vector(birthtimestep %>% dplyr::select(birthtimestep))
names(birthtimestep) <- btsCID

#### matrix of the timesteps when each child presented each sample ####

my_order <- c("2004", "2005", "2006", "2013", "2014" )
timestepInst <- dataload %>%
                  drop_na()%>%
                  group_by(unique_CID, sampletime, Year)%>%
                  summarise(sample_instances=n())%>%
                  group_by(unique_CID)%>%
                  arrange(match(Year, my_order))%>%
                  mutate(countInst=1:n())

dataload <- dataload %>%
                  left_join(timestepInst)

Inst <- dataload %>%
  pivot_wider(id_cols = c(unique_CID, Year, sampletime), 
              names_from = countInst, 
              values_from = study.time.step)%>%
  dplyr::select(-`NA`, -sampletime, -Year)%>%
  group_by(unique_CID) %>%
  summarise_each(funs(first(.[!is.na(.)])))

Inst <- Inst[-which(is.na(Inst$`1`)==TRUE),]

InstCIDs <- Inst %>%
  dplyr::select(unique_CID)

Inst <- Inst %>% 
  dplyr::select(-unique_CID)

Inst <- as.matrix(Inst)

#### kk data in the same layout as Inst - this is your likelihood dataframe ####
kkdata <- dataload %>%
  pivot_wider(id_cols = c(unique_CID, Year, sampletime), 
              names_from = countInst, 
              values_from = epslide)%>%
  dplyr::select(-`NA`, -sampletime, -Year)%>%
  group_by(unique_CID) %>%
  summarise_each(funs(first(.[!is.na(.)]))) 

kkdata <- kkdata[-which(is.na(kkdata$`1`)==TRUE),]

kkCIDs <- kkdata %>%
  dplyr::select(unique_CID)

kkdata <- kkdata %>% dplyr::select(-unique_CID)
kkdata <- as.matrix(kkdata)

#### the vector of number of times each child was sampled. ####
inst <- timestepInst %>%
  dplyr::select(unique_CID, countInst)%>%
  group_by(unique_CID)%>%
  summarise(inst=max(countInst))

instCIDs <- unname(as_vector(inst %>%
  dplyr::select(unique_CID)))

inst <- as_vector(inst %>%
  dplyr::select(-unique_CID))

names(inst) <- instCIDs

# check the CIDs are the same (same kids, same order etc)
#identical(instCIDs, kkCIDs) 
#identical(instCIDs, InstCIDs)
#identical(InstCIDs, kkCIDs)

# take out the children that don't give any data ever 
unique(btsCID[! btsCID %in% instCIDs])
unique(btsCID[! btsCID %in% unname(as_vector(InstCIDs))])

dataload <- dataload %>%
  filter(!c(unique_CID=="BUG060609" | unique_CID=="BUG060625" | unique_CID=="BUG140722" | 
           unique_CID=="BUG140823" | unique_CID=="BWD060605" | unique_CID== "BWD060612" | 
           unique_CID=="MUS140806"))

birthtimestep <- birthtimestep[names(birthtimestep) != "BUG060609"]
birthtimestep <- birthtimestep[names(birthtimestep) != "BUG060625"]
birthtimestep <- birthtimestep[names(birthtimestep) != "BUG140722"]
birthtimestep <- birthtimestep[names(birthtimestep) != "BUG140823"]
birthtimestep <- birthtimestep[names(birthtimestep) != "BWD060605"]
birthtimestep <- birthtimestep[names(birthtimestep) != "BWD060612"]
birthtimestep <- birthtimestep[names(birthtimestep) != "MUS140806"]

remove <- c( "BUG060609", "BUG060625", "BUG140722",
             "BUG140823", "BWD060605", "BWD060612", "MUS140806")

btsCID = btsCID[!(btsCID %in% remove)]

which(btsCID!=instCIDs)
which(btsCID!=unname(as_vector(InstCIDs)))
which(instCIDs!=unname(as_vector(InstCIDs)))

#### individual level treatment dataframe ####
 # this is where WB in the individual will reduce when there is treatment

mort_df <- MortTsCalc(dataload)

mort_df$unique_CID <- factor(mort_df$unique_CID)
mort_CIDs <- unique(mort_df$unique_CID)

# check all CIDs are in the same order
which(mort_CIDs!=instCIDs)
which(mort_CIDs!=btsCID)
which(mort_CIDs!=unname(as_vector(InstCIDs)))

#mort_df <- mort_df %>%
  #pivot_wider(id_cols = unique_CID, names_from = study_dates, values_from = study.time.step)

# how many times was everyone treated and sampled 
participation <- mort_df %>%
  group_by(unique_CID, treated)%>%
  tally()%>%
  ungroup()%>%
  complete(participation, unique_CID, treated, fill = list(n=0)) 

# just treated 
inst_mort <- participation %>%
  filter(treated==1)%>%
  dplyr::select(unique_CID, n)

inst_mort <- inst_mort[order(factor(inst_mort$unique_CID, levels=unique(mort_CIDs))),]
inst_mort <- inst_mort %>%
  dplyr::select(-unique_CID)%>%
  as_vector()%>%
  unname()

# just sampled 
inst_noMort <- participation %>%
  filter(treated==0)%>%
  dplyr::select(unique_CID, n)
inst_noMort <- inst_noMort[order(factor(inst_noMort$unique_CID, levels=unique(mort_CIDs))),]

inst_noMort <- inst_noMort%>%
  dplyr::select(-unique_CID)%>%
  as_vector()%>%
  unname()

# make data frame of child x number of times treated, filled with the timestep each person was treated
order <- as.character(range(mort_df$study.time.step))

treatyes <- mort_df %>%
  filter(treated==1)%>%
  group_by(unique_CID, study.date, study.time.step)%>%
  summarise(sample_instances=n())%>%
  group_by(unique_CID)%>%
  arrange(match(study.time.step, order))%>%
  mutate(countInst=1:n())%>%
  pivot_wider(id_cols=unique_CID, 
              names_from=countInst, 
              values_from = study.time.step)
treatyes <- treatyes[order(factor(treatyes$unique_CID, levels=unique(mort_CIDs))),]

treatyes <- treatyes %>%
  ungroup()%>%
  dplyr::select(-unique_CID)

# make data frame of child x number of times NOT treated, filled with the timestep each person was NOT treated
treatno <- mort_df %>%
  filter(treated==0) %>%
  group_by(unique_CID, study.date, study.time.step)%>%
  summarise(sample_instances=n())%>%
  group_by(unique_CID)%>%
  arrange(match(study.time.step, order))%>%
  mutate(countInst=1:n())%>%
  pivot_wider(id_cols=unique_CID, 
              names_from=countInst, 
              values_from = study.time.step)

treatno$unique_CID <- factor(treatno$unique_CID)

addcids <- unique(treatyes$unique_CID[! treatyes$unique_CID %in% treatno$unique_CID])

add <- data.frame(matrix(nrow=length(addcids), ncol=ncol(treatno)))
add$X1 <- addcids
colnames(add) <- colnames(treatno)
treatno <- bind_rows(treatno, add)
rm(add)
rm(addcids)

treatno <- treatno[order(factor(treatno$unique_CID, levels=unique(mort_CIDs))),]

treatno <- treatno %>%
  ungroup()%>%
  dplyr::select(-unique_CID)

mortality <- matrix(NA,nrow=length(mort_CIDs), ncol=max(dataload$study.time.step))

# fill matrix #
# fill with 1s for treated
for(i in 1:length(mort_CIDs)){
  for(r in 1:inst_mort[i]){
    mortality[i,treatyes[[i,r]]] <- 1 
  }
}

# fill with 0's for known NOT treated. 
inst_noMort[inst_noMort==0] <- NA

for(i in 1:length(mort_CIDs)){
  ifelse(inst_noMort[i]==NA, i+1 & 
           for(r in 0:inst_noMort[i]){
                mortality[i,treatno[[i,r]]] <- 0}, 
           for(r in 0:inst_noMort[i]){
                  mortality[i,treatno[[i,r]]] <- 0
                })
}

# the last sampling timepoint for each individual 
# used to fill the WB and EB objects from when i was born, 
# to when they are last sampled. 

MaxSampleTime <- dataload %>%
  group_by(unique_CID) %>%
  summarise(maxtimestep=max(study.time.step))

MaxSampleTime <- MaxSampleTime[order(factor(MaxSampleTime$unique_CID, levels=unique(mort_CIDs))),]

MaxSampleTime <- MaxSampleTime %>%
  ungroup()%>%
  dplyr::select(-unique_CID)%>%
  as_vector()%>%
  unname()

#### Running Model ####

start.time <- Sys.time()
runModel <- LongMod(nchildren=length(mort_CIDs), inst, Inst, kkdata, BirthTimeStep = birthtimestep, MaxSampleTime, mortality)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken