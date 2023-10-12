#install.packages("RPostgres")
library(RPostgres)
library(dplyr)
library(tidyr)

#access relational database
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "tweets",
  host = "dapr.psu.edu",
  port = 5432,
  user = "api",
  password = "d@pru53r"
)

##################### Collect Data of Candidate endorsements
#pull only handles from data
data <- readRDS("Twitter_10_ballot.rds")

handles <- c(data$`Campaign Twitter`, data$`Personal Twitter`)
handles <- handles[!is.na(handles)]

#construct queries
handle_or <- paste(" OR created_date BETWEEN '2021-01-01' AND '2022-12-31' AND text LIKE "," '%", handles, "%'", sep = "")

handle_or <- paste(handle_or, collapse = "")

query <- paste("SELECT * FROM legislator_tweets WHERE created_date BETWEEN '2021-01-01' AND '2022-12-31' AND text LIKE '%@AndresRomeroNM%'", handle_or, ";", sep  = "")

#run 
twts_endo <- dbGetQuery(con,query)

#save
saveRDS(twts_endo, "twts_endo.rds")

##################### Revise Data: pull legislator usernames
twts_endo <- readRDS("~/OneDrive - The Pennsylvania State University/Diss Proposal/P2_Data/data/twts_endo.rds")

author_id <- twts_endo %>%
  select(user_username, author_id)
author_id <- distinct(author_id)
author_ids <- c(author_id$author_id)

#get twitter author_ids2
query_or <- paste(" OR legislator_data.author_id2 LIKE ", "'", author_ids, "'", sep = "")
query_or <- paste(query_or, collapse = "")

query <- paste("SELECT * FROM legislator_data WHERE legislator_data.author_id2 LIKE '180677205'", query_or, ";", sep  = "")

user_ids2 <- dbGetQuery(con,query)

#get twitter author_ids1
query_or <- paste(" OR legislator_data.author_id1 LIKE ", "'", author_ids, "'", sep = "")
query_or <- paste(query_or, collapse = "")

query <- paste("SELECT * FROM legislator_data WHERE legislator_data.author_id1 LIKE '180677205'", query_or, ";", sep  = "")

user_ids1 <- dbGetQuery(con,query)

#combine user_ids
user_ids <- rbind(user_ids1, user_ids2)
user_ids <- distinct(user_ids)
saveRDS(user_ids, "user_ids.rds")

##################### combine user_ids with twts_endo data
# merge author_id1s
names(user_ids)
user1 <-user_ids %>%
  select("state", "chamber", "party3", "name", "handle_1", "author_id1","Date.assumed.office_bp")
user1 <- user1 %>% rename(author_id = author_id1)

joined_data1 <- merge(twts_endo, user1, by = "author_id")

# merge author_id2s
names(user_ids)
user2 <-user_ids %>%
  select("state", "chamber", "party3", "name", "handle_2", "author_id2","Date.assumed.office_bp")
user2 <- user2 %>% rename(author_id = author_id2)

joined_data2 <- merge(twts_endo, user2, by = "author_id")

# merge and create final dataset
joined_data <- bind_rows(joined_data1, joined_data2)

names(joined_data)
final_data <- joined_data %>%
  select("author_id", "state", "chamber","party3","name","text","created_date", "retweet_count",
         "like_count","reply_count","retweet_count","like_count","user_username","handle_1","handle_2" )
joined_data$state <- toupper(joined_data$state)

joined_data <- joined_data %>% rename(leg_state = state,
                                    leg_author_id = author_id,
                                    leg_chamber = chamber,
                                    leg_party = party3,
                                    leg_name = name,
                                    leg_username = user_username,
                                    leg_handle1 = handle_1,
                                    leg_handle2 = handle_2)

saveRDS(joined_data, "joined_data.rds")

names(joined_data)
table(joined_data$leg_state)

#Democratic state: New Mexico, Delaware, Illinois, Massachusetts, Nevada
#Republican state: Kansas, Alabama, Idaho, Kentucky, Louisiana 