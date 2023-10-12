library(dplyr)
library(sna)

data <- readRDS("final_data.rds")


####################################
# get a unique list of all nodes
nodes <- unique(c(data$leg_handle1,data$Cand_handle1))
# how many nodes are in this dataset?
length(nodes)

# collapse node-level information
race <- data$Leg_race[match(nodes,data$leg_handle1)]
# fill in for the nodes that never send endorsements
race[is.na(race)] <- data$Candidate_race[match(nodes,data$Cand_handle1)][is.na(race)]
#populating things that would be a blank space- no endorsement = no relationship

# collapse node-level information
gender <- data$Leg_gender[match(nodes,data$leg_handle1)]
# fill in for the nodes that never send endorsements
gender[is.na(gender)] <- data$Candidate_gender[match(nodes,data$Cand_handle1)][is.na(gender)]

# collapse node-level information
party <- data$leg_party[match(nodes,data$leg_handle1)]
# fill in for the nodes that never send endorsements
party[is.na(party)] <- data$Cand_party1[match(nodes,data$Cand_handle1)][is.na(party)]

# collapse node-level information
state <- data$leg_state[match(nodes,data$leg_state)]
# fill in for the nodes that never send endorsements
state[is.na(state)] <- data$Candidate_state[match(nodes,data$Cand_handle1)][is.na(state)]

# collapse node-level information
chamber <- data$leg_chamber[match(nodes,data$leg_chamber)]
# fill in for the nodes that never send endorsements
chamber[is.na(chamber)] <- data$candidate_chamber[match(nodes,data$Cand_handle1)][is.na(chamber)]


# create a dyad-level edgelist that 
# counts number of endorsements
agg_dyad <- data.frame(data %>% count(leg_handle1,Cand_handle1))

# create an adjacency matrix object
amat <- matrix(0,length(nodes),length(nodes))
  #0s are actual no endorsements + none endorsements

# add in the endorsement counts
amat[cbind(match(agg_dyad$leg_handle1,nodes),match(agg_dyad$Cand_handle1,nodes))] <- agg_dyad$n

# diagonal na
diag(amat) <- NA #dyads can never endorse themselves


amat[,not_running] <- NA #get vector of ids of nodes not running (pure legislators) and
amat[not_incumbents,] <- NA #for pure candidates 

#pure <- read_excel("pure_leg_cand2.xlsx")
not_running <- pure$not_running
not_incumbents <- pure$not_incumbents

amat_test <- amat
amat_test[,match(not_running,nodes)] <- NA #get vector of ids of nodes not running (pure legislators) and
amat_test[match(not_incumbents,nodes),] <- NA #get vector of ids of nodes not running (pure legislators) and

amat[,j] <- NA 
amat[i,] <- NA
amat[,match(not_running,nodes)] <- NA #get vector of ids of nodes not running (pure legislators) and
amat[match(not_incumbents,nodes),] <- NA

# make covariate matrices (need to make it a numeric column to work with dist())
same_gender <- as.matrix(dist(cbind(as.numeric(as.factor(gender))), upper=T)) ==0
same_race <- as.matrix(dist(cbind(as.numeric(as.factor(race))), upper=T)) ==0 
same_party <- as.matrix(dist(cbind(as.numeric(as.factor(party))), upper=T)) ==0 
same_state <- as.matrix(dist(cbind(as.numeric(as.factor(state))), upper=T)) ==0 
same_chamber <- as.matrix(dist(cbind(as.numeric(as.factor(chamber))), upper=T)) ==0 

#state and chamber- only exists for one of the dyadic pairs (controlling for chamber of endorser) 
covariates <- list(same_gender,same_race, same_party, same_state, same_chamber)

# qap (just do 10 reps to see how long they will take)
#system.time(nl<-netlm(amat, covariates,reps=10))


# do 1000 reps, add in same party, same state, same chamber
system.time(nl<-netlm(amat, covariates,reps=1000))
#user   system  elapsed 
#1361.054   66.438 4469.275 

# user    system   elapsed 
#1456.601    62.791 10538.424 
summary(nl)

saveRDS(nl, "nl2.rds")

#plots
gplot(amat, displaylabels=F)

nodeColors<-ifelse(gender == "female","hotpink","dodgerblue")
gplot(amat,displaylabels=F,vertex.col=nodeColors,vertex.cex=1.5, xlim=c(-4, 14))

nodeColors2<-ifelse(race != "white","purple","green")
gplot(amat,displaylabels=F,vertex.col=nodeColors2,vertex.cex=1.5, xlim=c(-4, 14))
 
#to change label color: vertex.label.color = “”
