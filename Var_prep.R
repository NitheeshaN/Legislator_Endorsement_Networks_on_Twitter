library(readr)
library(stringr)
library(dplyr)

ballot <- read_csv("~/OneDrive - The Pennsylvania State University/Diss Proposal/P2_Data/ballotpedia_data_2021-2022_Nakka,_Nitheesha.csv")

############################ SUBSET DATA

###subset data to capture only those with Twitter handles
ballot2 <- ballot[!with(ballot,is.na(ballot$`Campaign Twitter`)& is.na(ballot$`Personal Twitter`)),]

saveRDS(ballot2, "Twitter_ballot.rds")

###subset to only 10 states of interest: 
#5 Dem State legislatures: New Mexico, Delaware, Illinois, Massachusetts, Nevada
NM <- subset(Twitter_ballot, State == "NM")
DE <- subset(Twitter_ballot, State == "DE")
MA <- subset(Twitter_ballot, State == "MA")
IL <- subset(Twitter_ballot, State == "IL")
NV <- subset(Twitter_ballot, State == "NV")

#5 Rep State legislatures: Kansas, Alabama, Idaho, Kentucky, Louisiana
KS <- subset(Twitter_ballot, State == "KS")
AL <- subset(Twitter_ballot, State == "AL")
ID <- subset(Twitter_ballot, State == "ID")
KY <- subset(Twitter_ballot, State == "KY")
LA <- subset(Twitter_ballot, State == "LA")

Twitter_10_ballot <- rbind(NM, DE, MA, IL, NV, KS, AL, ID, KY, LA)
saveRDS(Twitter_10_ballot, "Twitter_10_ballot.rds")

###subset to remaining 40 states/ territories : 
sub40 <- subset(Twitter_ballot, !State == "NM")
sub40 <- subset(sub40, !State == "DE")
sub40 <- subset(sub40, !State == "MA")
sub40 <- subset(sub40, !State == "IL")
sub40 <- subset(sub40, !State == "NV")
sub40 <- subset(sub40, !State == "KS")
sub40 <- subset(sub40, !State == "AL")
sub40 <- subset(sub40, !State == "ID")
sub40 <- subset(sub40, !State == "KY")
sub40 <- subset(sub40, !State == "LA")

table(sub40$State)
length(unique(sub40$State))

saveRDS(sub40, "Twitter_44_ballot.rds")

samp44_race <- sub40[, c("Candidate ID","Person ID","Name","State", "Gender", "Ballotpedia URL",
                         "Campaign website", "Personal website","Campaign Facebook",
                         "Personal Facebook", "Campaign Twitter","Personal Twitter" ,
                         "Campaign Instagram", "Personal Instagram", "Campaign YouTube",          
                         "Personal YouTube","Campaign mailing address","Campaign phone", "LinkedIn")]

write.csv(samp44_race, "samp44_race.csv")

#samp44_race<- cbind(samp44_race, Twitter_44_ballot$`Party affiliation`)

############################ Extract candidates names from text
library(tidyverse)
###
test <- joined_data
cand <- Twitter_10_ballot

handles <- c(Twitter_10_ballot$`Campaign Twitter`, Twitter_10_ballot$`Personal Twitter`)
handles <- na.omit(handles)

for (k in 1:nrow(test)) {
  for (i in 1:length(handles)) {
    if (grepl(handles[[i]], test$text[[k]], fixed = TRUE) == TRUE) {
      test$cand_handle[[k]] <- handles[[i]]}
  }
  
}

#check that it can pick up multiple handles in one line of text...spoiler alert it does not
for (k in 1:nrow(test2)) {
  for (i in 1:length(handles)) {
    if (grepl(handles[[i]], test2$text[[k]], fixed = TRUE) == TRUE) {
      test2$cand_handle[[k]] <- handles[[i]]}
  }
  
}

#check loop works 
which(handles == "@AndresRomeroNM")
if (grepl(handles[[793]], test$text[[1]], fixed = TRUE) == TRUE) {
  test$cand_handle[1] <- handles[[793]]}

no_dup <- joined_data[!duplicated(joined_data), ]


############################ Prep final dataset

#### Add Legislator gender and race
#read in legis_coded from PLSC 541
#read in Twitter_10_ballot
#read in mtweets_noduplicates as "data"
data <- mtweets_noduplicates

#make sure both data frames look alike
data$Leg_name <- tolower(data$Leg_name)
  #data[str_detect(data$Candidate_name, "jrajra"), data$Candidate_race] <- "white"
  #df[str_detect(df$variable, "%"), 'unit'] <- "%"

#subset necessary data
legis <- legis_coded[, c("fullname", "race", "gender")]

#merge
data2 <- merge(data, legis, by.x = "Leg_name", by.y = "fullname", all = T)
data3 <- data2[complete.cases(data2[,c("Candidate_name")]),]

#rename some things
names(data3)[names(data3) == 'race'] <- 'Leg_race'
names(data3)[names(data3) == 'gender'] <- 'Leg_gender'

#### Add Candidate Party
cand<- Twitter_10_ballot[, c("Name", "Campaign Twitter", "Personal Twitter","Party affiliation")]
cand <- unique(cand)
names(cand)[names(cand) == 'Campaign Twitter'] <- 'Cand_handle1'
names(cand)[names(cand) == 'Personal Twitter'] <- 'Cand_handle2'

#merge
test <- data3
names(test)[names(test) == 'Candidate_name'] <- 'Cand_handle1'
data4.2 <- left_join(test, cand, by.x = "Candidate_handle1", all.x = T)
names(data4.2)[names(data4.2) == 'Party affiliation'] <- 'Cand_party1'

names(test)[names(test) == 'Cand_handle1'] <- 'Cand_handle2'
data4.2 <- left_join(test, cand, by.x = "Candidate_handle2", all.x = T)
saveRDS(data4.2, "draft_final.rds")

#data4 <- merge(data3, cand, by.x = "Candidate_name", by.y = "Campaign Twitter", all.x = T)

############################ Prep final dataset: Remove NAs
draft_final <- readRDS("~/OneDrive - The Pennsylvania State University/Diss Proposal/P2_Data/data/draft_final.rds")

#drop empty cand_handle2
draft2 = subset(draft_final, select = -c(Cand_handle2))

#drop empty candidate race and gender
draft3 <- draft2[complete.cases(draft2[,c("Candidate_race")]),]

#drop empty legis race and gender
draft4 <- draft3[complete.cases(draft3[,c("Leg_race")]),]

#change names of things 
draft4[draft4 == "AA"] <- "Asian American"
names(draft4)[names(draft4) == 'Name'] <- 'Candidate_name'

############################ Prep final dataset: Add Controls
controls <- read_excel("~/Documents/Backup_OD/PLSC 541/paper/data/final_data2.xlsx")

# change  state full names to state abbreviations
controls$state[controls$state == "New Mexico"] <- "NM"
controls$state[controls$state == "Delaware"] <- "DE"
controls$state[controls$state == "Massachusetts"] <- "MA"
controls$state[controls$state == "Illinois"] <- "IL"
controls$state[controls$state == "Nevada"] <- "NV"

controls$state[controls$state == "Kansas"] <- "KS"
controls$state[controls$state == "Alabama"] <- "AL"
controls$state[controls$state == "Idaho"] <- "ID"
controls$state[controls$state == "Kentucky"] <- "KY"
controls$state[controls$state == "Louisiana"] <- "LA"

#subset controls
controls2 <- controls[, c("state","salary_leg", "cit_ideo", "govt_ideo", "leg_seats",
                          "polarization.h", "polarization.s", "REI", "prop_white",
                           "perc_white", "hou_seats")]

# change state to candidate state
names(controls2)[names(controls2) == 'state'] <- 'Candidate_state'

# add controls to final dataset
draft5 <- draft4
draft5 <- left_join(draft5, controls2, by.x = "Candidate_state", all.x = T)

saveRDS(draft5, "final_data.rds")
# candidate summary stats
#242 unique candidates
#329 before removing Nas in line 142

############################ Prep final dataset: Add Candidate Chamber
data5 <- readRDS("~/OneDrive - The Pennsylvania State University/Diss Proposal/P2_Data/data/final_data.rds")
Twitter_10_ballot <- readRDS("~/OneDrive - The Pennsylvania State University/Diss Proposal/P2_Data/data/Twitter_10_ballot.rds")

cand<- Twitter_10_ballot[, c("Name", "District type")]
cand <- unique(cand)

#merge
names(cand)[names(cand) == 'Name'] <- 'Candidate_name'
data5 <- left_join(data5, cand, by.x = "Candidate_name", all.x = T)

names(data5)[names(data5) == 'District type'] <- 'candidate_chamber'

############################ Prep final dataset:  make all covariates look the same for leg and cand
#gender
data5$Leg_gender <- tolower(data5$Leg_gender)
data5$Candidate_gender <- tolower(data5$Candidate_gender)

#race
data5$Leg_race <- tolower(data5$Leg_race)
data5$Candidate_race <- tolower(data5$Candidate_race)

#party
data5$leg_party[data5$leg_party == "Democratic"] <- "Democratic Party"
data5$leg_party[data5$leg_party == "Republican"] <- "Republican Party"

#chamber
data5$candidate_chamber[data5$candidate_chamber == "State Legislative (Lower)"] <- "H"
data5$candidate_chamber[data5$candidate_chamber == "State Legislative (Upper)"] <- "S"

saveRDS(data5, "final_data.rds")

######################### identify non-running and non-incumbent nodes
leg <- data$leg_handle1
leg <- data.frame(leg)
leg <- unique(leg)
write.csv(leg, "leg.csv")

cand <- data$Cand_handle1
cand <- data.frame(cand)
cand <- unique(cand)
write.csv(cand, "cand.csv")
#I later combined them into one df called leg_cand and 
  #removed duplicates to create "pure_leg_cand"
