# data to evaluate RSA model "know" vs "think" (pilot 01)
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

view(d)

# remove instruction rows
d = d %>% filter(trial_index != "0")

# replace participant_id by random number
length(unique(d$participant_id)) #5
d$participantID <- match(d$participant_id, unique(sort(d$participant_id)))

# how many participants?
length(unique(d$participantID)) #10

# select relevant columns
d = d %>%
  select(c(stimulus, response, trial_index, time_elapsed, participantID))

# unpack demographics info
dg <- d %>%
  filter(trial_index == "2") %>%
  select(c(participantID,response))
#view(dg)

table(dg$response)
str(dg$response)

# age
dg$age = gsub("P0_Q0\":null,\"", "", dg$response) # delete everything before "age"
dg$age = gsub(",\"gender.*", "", dg$age) # delete everything after "age"
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
                      TRUE ~ "notSpeakerOfEnglish")
table(dg$language)

# American English
dg$amE = case_when(grepl("amE\":\"yes", dg$response) ~ "AmE",
                        TRUE ~ "notAmE")
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

# remove demographics from data
d = d %>%
  filter(trial_index != "2")

# add demographics data back to data
d = left_join(d, dg, by = "participantID")

# make response a numeric column, with values between 0 and 1
#view(d)
summary(d$response)
d$response <- as.numeric(d$response)
d$response <- d$response/100
table(d$response)

# create useful columns from stimulus column

# content
d$content = case_when(grepl("Charley", d$stimulus) ~ "Charley speaks Spanish",
                      grepl("Jackson", d$stimulus) ~ "Jackson ran ten miles",
                      TRUE ~ "error")
table(d$content)

# qud
d$qud = case_when(grepl("list", d$stimulus) ~ "ai",
                      grepl("knows", d$stimulus) ~ "nai",
                  TRUE ~ "error")
table(d$qud)

# prior
d$prior = case_when(grepl("Mexico", d$stimulus) ~ "higher",
                  grepl("Korea", d$stimulus) ~ "lower",
                  grepl("marathon", d$stimulus) ~ "higher",
                  grepl("obese", d$stimulus) ~ "lower",
                  TRUE ~ "error")
table(d$prior)

# utterance
d$utterance = case_when(grepl("knows that", d$stimulus) ~ "know-pos",
                    grepl("thinks that", d$stimulus) ~ "think-pos",
                    grepl("doesn't know that", d$stimulus) ~ "know-neg",
                    grepl("doesn't think that", d$stimulus) ~ "think-neg",
                    grepl("\"Charley speaks", d$stimulus) ~ "simple-pos",
                    grepl("\"Jackson ran", d$stimulus) ~ "simple-pos",
                    grepl("\"Charley doesn't", d$stimulus) ~ "simple-neg",
                    grepl("\"Jackson didn't", d$stimulus) ~ "simple-neg",
                    TRUE ~ "error")
table(d$utterance)

# predicate
d$predicate = case_when(grepl("knows that", d$stimulus) ~ "know",
                        grepl("thinks that", d$stimulus) ~ "think",
                        grepl("doesn't know that", d$stimulus) ~ "know",
                        grepl("doesn't think that", d$stimulus) ~ "think",
                        grepl("\"Charley speaks", d$stimulus) ~ "simple",
                        grepl("\"Jackson ran", d$stimulus) ~ "simple",
                        grepl("\"Charley doesn't", d$stimulus) ~ "simple",
                        grepl("\"Jackson didn't", d$stimulus) ~ "simple",
                        TRUE ~ "error")
table(d$predicate)

# valence
d$valence = case_when(grepl("knows that", d$stimulus) ~ "pos",
                        grepl("thinks that", d$stimulus) ~ "pos",
                        grepl("doesn't know that", d$stimulus) ~ "neg",
                        grepl("doesn't think that", d$stimulus) ~ "neg",
                        grepl("\"Charley speaks", d$stimulus) ~ "pos",
                        grepl("\"Jackson ran", d$stimulus) ~ "pos",
                        grepl("\"Charley doesn't", d$stimulus) ~ "neg",
                        grepl("\"Jackson didn't", d$stimulus) ~ "neg",
                        TRUE ~ "error")
table(d$valence)


# remove stimulus column now that everything has been extracted from it
d = d %>%
  select(-c(stimulus))
  
# participant info
table(d$age) #19-80
length(which(is.na(d$age))) # 0 missing values
# exclude outliers (0, 3330) before calculating mean
mean(d[10 < d$age & d$age < 100,]$age,na.rm=TRUE) #40.8

d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())

### exclude non-English speakers and non-American English speakers
# exclude non-English speakers
length(which(is.na(d$language))) #no missing responses
table(d$language) 

d <- d %>%
  filter(language != "notSpeakerOfEnglish") %>%  droplevels()
length(unique(d$participantID)) #0 participants excluded

# exclude non-American English speakers
length(which(is.na(d$amE))) #0 (everybody responded)
table(d$amE) 

d <- d %>%
  filter(amE != "notAmE") %>%  droplevels()
length(unique(d$participantID)) #5 so 0 participants excluded

# age and gender of remaining participants
table(d$age) #27-66
length(which(is.na(d$age))) # 0 missing values
# exclude outliers (0, 3330) before calculating mean
mean(d[10 < d$age & d$age < 100,]$age,na.rm=TRUE) #46.4

d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())

write_csv(d, file="../data/cd.csv")

# how many data points per condition (prior, qud, utterance)?
names(d)

tmp = d %>%
  group_by(content,prior, qud, utterance) %>% 
  tally
tmp
nrow(tmp) # 4 (if everything had been chosen, should a multiple of 22 conditions)
min(tmp$n) #1
mean(tmp$n) #6.2
max(tmp$n) #23
#view(tmp)

