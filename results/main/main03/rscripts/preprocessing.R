# data to evaluate RSA model "know" vs "think" (main 03)
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

# remove preloading rows
d = d %>% filter(trial_index != "0")

# replace participant_id by random number
length(unique(d$participant_id)) #82
# participants in previous run were given numbers between 1 and 323, so give these higher ones
d$participantID <- match(d$participant_id, unique(sort(d$participant_id)))+323
table(d$participantID) #324+405

# how many participants?
length(unique(d$participantID)) #82

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
table(d$age) #21-71
length(which(is.na(d$age))) # 0 missing values
mean(d$age) #42.2

d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())

# gender        count
# <chr>         <int>
#   1 female           52
# 2 male             28
# 3 preferNoToSay     2

### exclude non-English speakers and non-American English speakers
# exclude non-English speakers
length(which(is.na(d$language))) #no missing responses
table(d$language) 

d <- d %>%
  filter(language == "English") %>%  droplevels()
length(unique(d$participantID)) #0 participant excluded

# exclude non-American English speakers
length(which(is.na(d$amE))) #0 (everybody responded)
table(d$amE) 

d <- d %>%
  filter(amE != "notAmE") %>%  droplevels()
length(unique(d$participantID)) #82 so 0 excluded

# age and gender of remaining participants
table(d$age) #21-71
length(which(is.na(d$age))) # 0 missing values
mean(d$age,na.rm=TRUE) #42.2

d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())

# gender        count
# <chr>         <int>
#   1 female           52
# 2 male             28
# 3 preferNoToSay     2


# now import cleaned up data from main02 and merge with these data
d2 = read_csv("../../main02/data/cd.csv")
table(d2$qud,d2$content)
nrow(d2) #315 participants' data

# remove from d2 the jackson/ai items (which had the wrong text)
d2 = d2 %>%
  filter(!(qud == "ai" & content == "Jackson ran ten miles"))
table(d2$qud,d2$content)
nrow(d2) #245 participants' data (70 participants' data eliminated)

# combine newly collected data with d2
d = rbind(d, d2)
nrow(d) #327 participants (= 245 + 82)
table(d$qud,d$content)

# save the data
write_csv(d, file="../data/cd.csv")

length(unique(d$participantID)) #327

# how many data points per condition (qud, utterance)?
names(d)

tmp = d %>%
  group_by(content, qud, utterance) %>% 
  tally
tmp
nrow(tmp) # 16 (4 utterances x 2 quds x 2 items) 
min(tmp$n) #13
mean(tmp$n) #20.4
max(tmp$n) #28

tmp = d %>%
  group_by(qud, utterance) %>% 
  tally
tmp
nrow(tmp) # 8 (4 utterances x 2 quds)
min(tmp$n) #32
mean(tmp$n) #40.9
max(tmp$n) #55

