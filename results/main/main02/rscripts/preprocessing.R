# data to evaluate RSA model "know" vs "think" (main 02)
# preprocessing

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../../helpers.R')

# load required packages for pre-processing data
library(tidyverse)
library(readr)

theme_set(theme_bw())

# read in the raw data
d = read_csv("../data/combined.csv")

#view(d)

# remove preloading rows
d = d %>% filter(trial_index != "0")

# replace participant_id by random number
length(unique(d$participant_id)) #323
d$participantID <- match(d$participant_id, unique(sort(d$participant_id)))
table(d$participantID)

# how many participants?
length(unique(d$participantID)) #323

#view(d)

# select relevant columns
d = d %>%
  select(c(condition, response, trial_type, participantID))

# unpack demographics info
dg <- d %>%
  filter(trial_type == "survey") %>%
  select(c(participantID,response))
#view(dg)

table(dg$response)
str(dg$response)

# age
dg$age = gsub("P0_Q0\":null,\"", "", dg$response) # delete everything before "age"
dg$age = gsub(",\"gender.*", "", dg$age) # delete everything after age value
# now that we can see only age values, only record numbers
dg$age = gsub("\\D", "", dg$age)
dg$age = as.numeric(dg$age)
table(dg$age)

# gender
dg$gender = case_when(grepl("female", dg$response) ~ "female",
                      grepl("male", dg$response) ~ "male",
                      grepl("non-binary", dg$response) ~ "non-binary",
                      TRUE ~ "preferNoToSay")
table(dg$gender)

# language
dg$language = case_when(grepl("language\":\"yes", dg$response) ~ "English",
                        grepl("language\":\"no", dg$response) ~ "notSpeakerOfEnglish",
                        grepl("language\":\"\"", dg$response) ~ "noResponse",
                      TRUE ~ "error") 
table(dg$language)

# American English
dg$amE = case_when(grepl("amE\":\"yes", dg$response) ~ "AmE",
                   grepl("amE\":\"no", dg$response) ~ "notAmE",
                   grepl("amE\":\"\"", dg$response) ~ "noResponse",
                    TRUE ~ "error")
table(dg$amE)

# education
dg$education = case_when(grepl("some high school", dg$response) ~ "some high school",
                         grepl("graduated high school", dg$response) ~ "graduated high school",
                         grepl("some college", dg$response) ~ "some college",
                         grepl("graduated college", dg$response) ~ "graduated college",
                         grepl("hold a higher degree", dg$response) ~ "hold a higher degree",
                         TRUE ~ "preferNoToSay")
table(dg$education)

# comments
dg$comments = gsub(".*comments", "", dg$response)
table(dg$comments)
# comments show that text in jackson/ai condition erroneously mentions Charley, not Jackson!!!
                      
# remove response column from demographics data
dg = dg %>%
  select(-c(response))
summary(dg)

#view(d)
# remove demographics and instructions from data
d = d %>%
  filter(trial_type == "multiple-slider")

# add demographics data back to data
d = left_join(d, dg, by = "participantID")

#view(d)
# create useful columns from condition column
table(d$condition)

# content
d$content = case_when(grepl("content\":\"charley", d$condition) ~ "Charley speaks Spanish",
                      grepl("content\":\"jackson", d$condition) ~ "Jackson ran ten miles",
                      TRUE ~ "error")
table(d$content)

# qud
d$qud = case_when(grepl("qud\":\"ai", d$condition) ~ "ai",
                      grepl("qud\":\"nai", d$condition) ~ "nai",
                  TRUE ~ "error")
table(d$qud)

# # prior
# d$prior = case_when(grepl("Mexico", d$stimulus) ~ "higher",
#                   grepl("Korea", d$stimulus) ~ "lower",
#                   grepl("marathon", d$stimulus) ~ "higher",
#                   grepl("obese", d$stimulus) ~ "lower",
#                   TRUE ~ "error")
# table(d$prior)

# utterance
d$utterance = case_when(grepl("utterance\":\"Cole knows that", d$condition) ~ "know-pos",
                    grepl("utterance\":\"Cole thinks that", d$condition) ~ "think-pos",
                    grepl("utterance\":\"Cole doesn't know that", d$condition) ~ "know-neg",
                    grepl("utterance\":\"Cole doesn't think that", d$condition) ~ "think-neg",
                    #grepl("utterance\":\"Charley", d$condition) ~ "simple-pos",
                    #grepl("utterance\":\"Jackson", d$condition) ~ "simple-pos",
                    #grepl("\"Charley doesn't", d$condition) ~ "simple-neg",
                    #grepl("\"Jackson didn't", d$condition) ~ "simple-neg",
                    TRUE ~ "error")
table(d$utterance)

# predicate
d$predicate = case_when(grepl("utterance\":\"Cole knows that", d$condition) ~ "know",
                        grepl("utterance\":\"Cole thinks that", d$condition) ~ "think",
                        grepl("utterance\":\"Cole doesn't know that", d$condition) ~ "know",
                        grepl("utterance\":\"Cole doesn't think that", d$condition) ~ "think",
                        #grepl("\"Charley speaks", d$condition) ~ "simple",
                        #grepl("\"Jackson ran", d$condition) ~ "simple",
                        #grepl("\"Charley doesn't", d$condition) ~ "simple",
                        #grepl("\"Jackson didn't", d$condition) ~ "simple",
                        TRUE ~ "error")
table(d$predicate)

# valence
d$valence = case_when(grepl("knows that", d$condition) ~ "pos",
                        grepl("thinks that", d$condition) ~ "pos",
                        grepl("doesn't know that", d$condition) ~ "neg",
                        grepl("doesn't think that", d$condition) ~ "neg",
                        #grepl("\"Charley speaks", d$condition) ~ "pos",
                        #grepl("\"Jackson ran", d$condition) ~ "pos",
                        #grepl("\"Charley doesn't", d$condition) ~ "neg",
                        #grepl("\"Jackson didn't", d$condition) ~ "neg",
                        TRUE ~ "error")
table(d$valence)


# remove condition column now that everything has been extracted from it
d = d %>%
  select(-c(condition))

# get the response for the first and second slider
#view(d)

# now code the responses, based on condition
table(d$response)

# first slider
d$responseFIRST = gsub("\\{\"stimFIRST\":","",d$response) # delete stimFIRST
d$responseFIRST = gsub(",\".*","",d$responseFIRST) #delete ," and everything after it
table(d$responseFIRST)

# second slider
d$responseSECOND = gsub(".*D\":","",d$response) # delete D": (in SECOND:") and everything before it
d$responseSECOND = gsub("\\}","",d$responseSECOND) #delete }
table(d$responseSECOND)

# new response coding:
# responseCC
# responseMC
# if qud = ai, responseFIRST is responseCC, if qud = nai, responseSECOND is responseCC
d$responseCC = case_when(d$qud == "ai" ~ d$responseFIRST,
                         d$qud == "nai" ~ d$responseSECOND,
                         TRUE ~ "555")
table(d$responseCC)

d$responseMC = case_when(d$qud == "ai" ~ d$responseSECOND,
                         d$qud == "nai" ~ d$responseFIRST,
                         TRUE ~ "555")
table(d$responseMC)

# remove columns not needed
d = d %>%
  select(-c(response,responseFIRST,responseSECOND,trial_type))
#view(d)

# make responses numeric and between 0 and 1
d$responseCC <- as.numeric(d$responseCC)
d$responseCC <- d$responseCC/100
table(d$responseCC)

d$responseMC <- as.numeric(d$responseMC)
d$responseMC <- d$responseMC/100
table(d$responseMC)
  
# participant info
table(d$age) #20-74
length(which(is.na(d$age))) # 0 missing values
mean(d$age) #41.7

d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())

# gender        count
# 1 female          180
# 2 male            134
# 3 non-binary        5
# 4 preferNoToSay     4

# how many participants were recruited to the jackson/ai condition (where text was wrong)?
table(d$content,d$qud) #72
#                         ai nai
# Charley speaks Spanish 83  86
# Jackson ran ten miles  72  82
# 72 in erroneous condition (jackson/ai)

### exclude non-English speakers and non-American English speakers
# exclude non-English speakers
length(which(is.na(d$language))) #no missing responses
table(d$language) 

d <- d %>%
  filter(language == "English") %>%  droplevels()
length(unique(d$participantID)) #1 participant excluded, 322 remain

# exclude non-American English speakers
length(which(is.na(d$amE))) #0 (everybody responded)
table(d$amE) 

d <- d %>%
  filter(amE != "notAmE") %>%  droplevels()
length(unique(d$participantID)) #315 so 7 participants excluded

# age and gender of remaining participants
table(d$age) #20-73
length(which(is.na(d$age))) # 0 missing values
mean(d$age,na.rm=TRUE) #41.9

d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())

# gender        count
# 1 female          176
# 2 male            130
# 3 non-binary        5
# 4 preferNoToSay     4

# how many participants were recruited to the jackson/ai condition (where text was wrong)?
table(d$content,d$qud) #72 WAS THE VALUE BEFORE EXCLUSION
#                        ai nai
# Charley speaks Spanish 82  84
# Jackson ran ten miles  70  79
# this means of the 8 participants excluded, 2 were excluded in the erroneous condition

write_csv(d, file="../data/cd.csv")
# run separate experiment (main03) with just jackson/ai condition (4 utterances x 1 content x 1 qud) 
# 4 x 20 participants = 80 participants
# and bind these data to the newly collected data
