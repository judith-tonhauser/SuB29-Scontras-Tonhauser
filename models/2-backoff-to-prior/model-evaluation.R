# model 2-backoff-to-prior
# R script for model evaluation

# load required libraries
library(jsonlite)
library(tidyverse)
library(rwebppl)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
theme_set(theme_bw())

# read in the  model
model <- read_file("model.wppl")
model

# evaluate the model
eval_webppl <- function(command) {
  webppl(paste(model,command,sep="\n"))
}

# define the utterances
utterances = c("pos-know-pos-dance", 
               "pos-think-pos-dance",
               "neg-know-pos-dance", 
               "neg-think-pos-dance",
               "pos-bare-pos-dance",
               "neg-bare-pos-dance")
utterances

# define the QUDs
quds = c("ccQUD", "mcQUD")
quds

# define ccPriors
ccPriors = c("higher", "lower")
ccPriors

# pragmatic listener ----

# input to PL: utterance, qud, CCprior

#### call PL ----

PL = data.frame(utterance = character(), qud = character(), ccPrior = character(),
                state.CC = numeric(),
                prob = numeric())
PL

for (u in utterances) {
  print(u)
  for (q in quds) {
    print(q)
    for (ccP in ccPriors) {
      print(ccP)
      PL_tmp = eval_webppl(paste("responseGenerator('",u,"','",q,"','",ccP,"')",sep=""))
      for (i in 1:nrow(PL_tmp)) {
        PL = PL %>% 
          add_row(utterance = u, qud = q, ccPrior = ccP, 
                  state.CC = PL_tmp$support[i], 
                  prob = PL_tmp$prob[i])
      }
    }
  }
}

PL
#view(PL)
names(PL)

# make a state column
PL$state = PL$state.CC
PL = PL %>%
  select(-c(state.CC))

table(PL$qud) 
# ccQUD mcQUD 
#  24    24

table(PL$utterance)  
#neg-bare-pos-dance  neg-know-pos-dance 
#8                   8 
#neg-think-pos-dance  pos-bare-pos-dance 
#8                   8 
#pos-know-pos-dance pos-think-pos-dance 
#8                   8

# save PL
write_csv(PL, file="data/PL.csv")

# plots ----

# Fig 8 ----
# strength of inference by utterance (higher, default QUDs

# read PL
PL = read_csv("data/PL.csv")
nrow(PL) #48

# calculate mean inference strength across utterances
means.predicted = PL %>%
  filter(state == "1") %>%
  filter(ccPrior == "higher") %>%
  filter(!(grepl("know",utterance) & qud == "ccQUD")) %>%
  filter(!(grepl("think",utterance) & qud == "ccQUD")) %>%
  filter(!(grepl("bare",utterance) & qud == "mcQUD")) %>%
  group_by(utterance) %>%
  summarize(Mean = mean(prob)) %>%
  droplevels() %>%
  mutate(data = "model from Section 4") %>%
  mutate(utterance = recode(utterance, "pos-bare-pos-dance" = "simple-pos",
                            "neg-bare-pos-dance" = "simple-neg",
                            "pos-know-pos-dance" = "know-pos",
                            "neg-know-pos-dance" = "know-neg",
                            "pos-think-pos-dance" = "think-pos",
                            "neg-think-pos-dance" = "think-neg"))
means.predicted

# load means.predicted from original model
means.predicted.original.model <- read_csv("../1-original-model/data/means.predicted.original.model.csv")

# add column to identify the data
means.predicted.original.model = means.predicted.original.model %>%
  mutate(data = "model from Section 3")
means.predicted.original.model

# load data from Exp 1 to compare to
d_exp1 <- read_csv("../../results/main/main01/data/cd.csv")
nrow(d_exp1) #873

# calculate mean inference in exp 1 for higher prior and default QUDs
means.exp1 = d_exp1 %>%
  filter(prior == "higher") %>%
  filter(!(grepl("know",utterance) & qud == "ccQUD")) %>%
  filter(!(grepl("think",utterance) & qud == "ccQUD")) %>%
  filter(!(grepl("simple",utterance) & qud == "mcQUD")) %>%
  group_by(utterance) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(data = "human")
means.exp1

# add fake YMin and YMax to the model predictions, for binding with human data
means.predicted = means.predicted %>%
  mutate(YMin = 0) %>%
  mutate(YMax = 0)

means.predicted.original.model = means.predicted.original.model %>%
  mutate(YMin = 0) %>%
  mutate(YMax = 0)

means.exp1
means.predicted

tmp = rbind(means.predicted,means.predicted.original.model,means.exp1)
tmp

# relevel the utterances
tmp$utterance = factor(tmp$utterance, levels = means.exp1$utterance[order(means.exp1$Mean)], ordered = TRUE)
levels(tmp$utterance)

tmp$data = factor(tmp$data, levels = c("model from Section 3", "model from Section 4","human"))
levels(tmp$data)

ggplot(tmp, aes(x=utterance, y=Mean, color=data, group = interaction(utterance, data))) +
  geom_point(size = 2) + 
  #geom_line(aes(group = interaction(data))) +
  #geom_point(data=means.predicted, aes(x=utterance, y=mean.prob, group=1), color = "blue", size = 2) +
  #geom_line(data=means.predicted,aes(x=utterance, y=mean.prob),color = "blue") +
  theme(legend.position="top") +
  scale_color_manual(values = c("red","blue","black")) +
  geom_errorbar(data=tmp[tmp$data == "human",], aes(ymin=YMin,ymax=YMax),width=.05,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  #facet_grid(content ~ .) +
  scale_x_discrete(labels=c("simple-pos"="C", "simple-neg"="not C", "know-pos" = "Cole knows \n that C", "know-neg" = "Cole doesn't \n know that C",
                            "think-pos" = "Cole thinks \n that C", "think-neg" = "Cole doesn't \n think that C")) +
  #theme_classic() +
  ylab("Predicted probability from model \n in Section 3 (red) and Section 4 (blue) \n  Mean inference rating (black)") +
  xlab("\nUtterance")
ggsave("graphs/Section4-by-utterance-comparison.pdf",height=3,width=6)
