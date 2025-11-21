# 0-original-model
# R script for model evaluation for SuB29 paper

# load required libraries
library(jsonlite)
library(tidyverse)
library(rwebppl)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
theme_set(theme_bw())

source('../helpers.R')

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
                state = numeric(), prob = numeric())
PL

for (u in utterances) {
  print(u)
  for (q in quds) {
    print(q)
    for (ccP in ccPriors) {
      print(ccP)
  PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",q,"','",ccP,"')",sep=""))
  for (i in 1:nrow(PL_tmp)) {
    PL = PL %>% 
      add_row(utterance = u, qud = q, ccPrior = ccP, 
              state = PL_tmp$support[i],
              prob = PL_tmp$prob[i])
  }
    }
  }
}
PL
#view(PL)
nrow(PL) #36

write_csv(PL, file="data/PL.csv")

# Fig 7 in SuB paper ----

# read PL
PL = read_csv("data/PL.csv")
nrow(PL) #36

##### P1 in panel (a): Negated know- vs. think-utterances  ----

# filter bare utterances and positive utterances
PL2 = PL %>% 
  filter(state == "1") %>%
  filter(!(grepl("bare",utterance))) %>%
  filter(!(grepl("pos-think",utterance))) %>%
  filter(!(grepl("pos-know",utterance))) %>%
  droplevels()
PL2

# calculate model predictions by utterance, aggregating over priors
PL_agg.utt = PL2 %>%
  # summarize probabilities over cgs and ccPrior
  #group_by(utterance,ccPrior,qud) %>%
  #summarize(sum.prob = sum(prob)) %>% 
  # calculate by-utterance mean probability
  group_by(utterance) %>%
  summarize(prob = mean(prob)) %>% 
  mutate(utterance = recode(utterance, "pos-bare-pos-dance" = "simple-pos",
                            "pos-know-pos-dance" = "know-pos",
                            "neg-know-pos-dance" = "know-neg",
                            "pos-think-pos-dance" = "think-pos",
                            "neg-think-pos-dance" = "think-neg")) 
PL_agg.utt

# load data from exp1 and exp2 for pooling
d_exp1 <- read_csv("../../results/main/main01/data/cd.csv")
nrow(d_exp1) #873
d_exp2 <- read_csv("../../results/main/main03/data/cd.csv")
nrow(d_exp2) #327

# select relevant utterances and QUDs: negated know and think
d_exp1 = d_exp1 %>%
  filter(predicate == "know" | predicate == "think") %>%
  filter(valence == "neg") %>%
  select(c(response,utterance))
#view(d_exp1)

d_exp2 = d_exp2 %>%
  filter(predicate == "know" | predicate == "think") %>%
  filter(valence == "neg") %>%
  select(c(responseCC,utterance)) %>%
  rename(response = responseCC)
#view(d_exp2)

# bind data and calculate mean by-utterance inference 
d = rbind(d_exp1,d_exp2)

means.utt = d %>%
  group_by(utterance) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) 
means.utt

# relevel the utterances
means.utt$utterance = factor(means.utt$utterance, levels = means.utt$utterance[order(means.utt$Mean)], ordered = FALSE)
levels(means.utt$utterance)

PL_agg.utt$utterance = factor(PL_agg.utt$utterance, levels = means.utt$utterance[order(means.utt$Mean)], ordered = FALSE)
levels(PL_agg.utt$utterance)

# plot 
ggplot(means.utt, aes(x=utterance, y=Mean)) +
  geom_point(data=PL_agg.utt, shape=21, aes(x=utterance, y=prob), size = 3, color = "red", fill="red") +
  geom_point(shape=21, size = 3, color = "black", fill="black") + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.05,color="black") +
  theme(legend.position="top") +
  guides(shape=guide_legend("prior probability of target content"),fill=guide_legend("QUD", override.aes=list(shape=21))) +
  theme(axis.text.y = element_text(size=10)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  scale_x_discrete(labels=c("simple-pos"="C", "simple-neg"="not C", "know-pos" = "Cole knows \n that C", "know-neg" = "Cole doesn't \n know that C",
                            "think-pos" = "Cole thinks \n that C", "think-neg" = "Cole doesn't \n think that C")) +
  ylab("Predicted probability (red) \n Mean inference rating (black)") +
  xlab("Utterance") +
  theme(axis.title.x=element_blank()) 
ggsave("graphs/P1-with-human.pdf",height=2.4,width=2.3)

#####  P2 in panel (b): negated know- and think-utterances by prior probability ----

# filter bare utterances and positive utterances
PL2 = PL %>% 
  filter(state == "1") %>%
  filter(!(grepl("bare",utterance))) %>%
  filter(!(grepl("pos-know",utterance))) %>%
  filter(!(grepl("pos-think",utterance))) %>%
  droplevels()
PL2
nrow(PL2) #8

# calculate model predictions by utterance and prior
PL_agg.prior = PL2 %>%
  # summarize probabilities over cgs and ccPrior
  #group_by(ccPrior) %>%
  #summarize(sum.prob = sum(prob)) %>% 
  # calculate by-utterance and prior mean probability
  group_by(ccPrior) %>%
  summarize(prob = mean(prob)) %>% 
  #mutate(utterance = recode(utterance, "pos-bare-pos-dance" = "simple-pos",
  #                          "pos-know-pos-dance" = "know-pos",
  #                          "neg-know-pos-dance" = "know-neg",
  #                          "pos-think-pos-dance" = "think-pos",
  #                          "neg-think-pos-dance" = "think-neg")) %>%
  rename(prior = ccPrior)
PL_agg.prior

# load data from exp1 to compare to
d_exp1 <- read_csv("../../results/main/main01/data/cd.csv")
nrow(d_exp1) #873

# select relevant utterances: negated know and think
d_exp1 = d_exp1 %>%
  filter(predicate == "know" | predicate == "think") %>%
  filter(valence == "neg")
table(d_exp1$utterance)
table(d_exp1$qud)

# calculate mean inference ratings in exp1 with QUD BEL?
# exp1
means_exp1 = d_exp1 %>%
  group_by(prior) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) 
means_exp1

# relevel prior in the relevant two datasets
means_exp1$prior <- factor(means_exp1$prior, levels = c("lower","higher"))
PL_agg.utt.prior$prior <- factor(PL_agg.utt.prior$prior, levels = c("lower","higher"))

# plot 
ggplot(means_exp1, aes(x=prior, y=Mean)) +
  geom_point(data=PL_agg.prior, aes(x=prior, y=prob), color = "red", fill="red", size = 3) +
  geom_point(size = 3, color="black",fill="black") + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.05,color="black") +
  theme(legend.position="none") +
  scale_shape_manual(values=c(25,24)) +
  #guides(shape=guide_legend("prior probability \n of target content"), nrow=2) +
  theme(axis.text.y = element_text(size=10)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  scale_x_discrete(labels=c("simple-pos"="C", "simple-neg"="not C", "know-pos" = "Cole knows \n that C", "know-neg" = "Cole doesn't \n know that C",
                            "think-pos" = "Cole thinks \n that C", "think-neg" = "Cole doesn't \n think that C")) +
  ylab("Predicted probability (red) \n Mean inference rating (black)") +
  xlab("Prior probability") +
  theme(axis.title.x=element_blank()) 
ggsave("graphs/P2-with-human.pdf",height=2.4,width=2.3)

#####  P3 in panel (c): negated know- and think-utterances by QUD ----

# filter bare utterances and positive utterances
PL2 = PL %>% 
  filter(state == "1") %>%
  filter(!(grepl("bare",utterance))) %>%
  filter(!(grepl("pos-know",utterance))) %>%
  filter(!(grepl("pos-think",utterance))) %>%
  droplevels()
PL2

# calculate model predictions by qud, with higher prior
PL_agg.utt.qud = PL2 %>%
  # summarize probabilities over cgs and quds
  #group_by(utterance,qud,ccPrior) %>%
  #summarize(sum.prob = sum(prob)) %>% 
  # calculate by-qud mean probability
  group_by(qud)  %>%
  summarize(prob = mean(prob)) #%>% 
# mutate(utterance = recode(utterance, "pos-bare-pos-dance" = "simple-pos",
#                           "pos-know-pos-dance" = "know-pos",
#                           "neg-know-pos-dance" = "know-neg",
#                           "pos-think-pos-dance" = "think-pos",
#                           "neg-think-pos-dance" = "think-neg"))

PL_agg.utt.qud
#table(PL_agg.utt.qud$utterance)

# load data from exp2 to compare to
d_exp2 <- read_csv("../../results/main/main03/data/cd.csv")
nrow(d_exp2) #327

# select relevant utterances: negated know and think, QUD BEL?
d_exp2 = d_exp2 %>%
  filter(predicate == "know" | predicate == "think") %>%
  filter(valence == "neg") 
table(d_exp2$utterance)
table(d_exp2$qud)


# calculate mean inference ratings in exp2 by QUD
means_exp2 = d_exp2 %>%
  group_by(qud) %>%
  summarize(Mean = mean(responseCC), CILow = ci.low(responseCC), CIHigh = ci.high(responseCC)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) 
means_exp2

# relevel QUDs 
means_exp2$qud <- factor(means_exp2$qud, levels = c("ai","nai"), labels = c("C?","BEL?"))
PL_agg.utt.qud$qud <- factor(PL_agg.utt.qud$qud, levels = c("ccQUD","mcQUD"), labels = c("C?","BEL?"))

# plot
ggplot(means_exp2, aes(x=qud, y=Mean)) +
  geom_point(data=PL_agg.utt.qud, shape=21, aes(x=qud, y=prob), size = 3, color = "red", fill = "red") +
  geom_point(size = 3, color="black",fill="black") + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.05,color="black") +
  theme(legend.position="none") +
  #scale_fill_manual(values=c("#E69F00","#56B4E9")) +
  #guides(fill=guide_legend("QUD")) +
  theme(axis.text.y = element_text(size=10)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,.2,.4,.6,.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  scale_x_discrete(labels=c("BEL?"="BEL?", "CC?"="C?")) +
  #scale_x_discrete(labels=c("BEL?"="Does Cole \n believe C?", "C?"="Does Charley \n speak Spanish?")) +
  ylab("Predicted probability (red) \n Mean inference rating (black)") +
  xlab("Utterance") +
  theme(axis.title.x=element_blank()) 
ggsave("graphs/P3-with-human.pdf",height=2.4,width=2.3)

# data for Fig 8 (and code for original Fig 8) ----
# strength of inference by utterance (higher, default QUDs)

# read PL
PL = read_csv("data/PL.csv")
nrow(PL) #36

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
  mutate(data = "model") %>%
  mutate(utterance = recode(utterance, "pos-bare-pos-dance" = "simple-pos",
                            "neg-bare-pos-dance" = "simple-neg",
                            "pos-know-pos-dance" = "know-pos",
                            "neg-know-pos-dance" = "know-neg",
                            "pos-think-pos-dance" = "think-pos",
                            "neg-think-pos-dance" = "think-neg"))
means.predicted

# add "simple-neg" with value 0 to model predictions
means.predicted <- as.data.frame(means.predicted)
tmp.data = data.frame(utterance = "simple-neg", Mean = 0, data = "model")
means.predicted = rbind(means.predicted, tmp.data)
means.predicted

# save means.predicted to import into Fig 8 in 2-backoff-to-prior-model
write_csv(means.predicted, file ="data/means.predicted.original.model.csv")

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

means.exp1
means.predicted

tmp = rbind(means.predicted,means.exp1)
tmp

# relevel the utterances
tmp$utterance = factor(tmp$utterance, levels = means.exp1$utterance[order(means.exp1$Mean)], ordered = TRUE)
levels(tmp$utterance)

tmp$data = factor(tmp$data, levels = c("model","human"))
levels(tmp$data)

ggplot(tmp, aes(x=utterance, y=Mean, color=data, group = interaction(utterance, data))) +
  geom_point(size = 3) + 
  #geom_line(aes(group = interaction(data))) +
  #geom_point(data=means.predicted, aes(x=utterance, y=mean.prob, group=1), color = "blue", size = 2) +
  #geom_line(data=means.predicted,aes(x=utterance, y=mean.prob),color = "blue") +
  theme(legend.position="top") +
  scale_color_manual(values = c("red","black")) +
  geom_errorbar(data=tmp[tmp$data == "human",], aes(ymin=YMin,ymax=YMax),width=.2,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  #facet_grid(content ~ .) +
  scale_x_discrete(labels=c("simple-pos"="C", "simple-neg"="not C", "know-pos" = "Cole knows \n that C", "know-neg" = "Cole doesn't \n know that C",
                            "think-pos" = "Cole thinks \n that C", "think-neg" = "Cole doesn't \n think that C")) +
  #theme_classic() +
  ylab("Predicted probability (red) \n Mean inference rating (black)") +
  xlab("\nUtterance")
ggsave("graphs/Section4-by-utterance-comparison-original-model.pdf",height=3,width=6)

# figure for Tübingen 2025 talk ----
# strength of inference by utterance and prior (default QUDs)

# read PL
PL = read_csv("data/PL.csv")
nrow(PL) #36
names(PL)

# calculate mean inference strength across utterances
means.predicted = PL %>%
  filter(state == "1") %>%
  filter(!(grepl("know",utterance) & qud == "ccQUD")) %>%
  filter(!(grepl("think",utterance) & qud == "ccQUD")) %>%
  filter(!(grepl("bare",utterance) & qud == "mcQUD")) %>%
  group_by(utterance,ccPrior) %>%
  summarize(Mean = mean(prob)) %>%
  droplevels() %>%
  mutate(data = "model") %>%
  mutate(utterance = recode(utterance, "pos-bare-pos-dance" = "simple-pos",
                            "neg-bare-pos-dance" = "simple-neg",
                            "pos-know-pos-dance" = "know-pos",
                            "neg-know-pos-dance" = "know-neg",
                            "pos-think-pos-dance" = "think-pos",
                            "neg-think-pos-dance" = "think-neg"))
means.predicted

# add "simple-neg" with value 0 to model predictions
means.predicted <- as.data.frame(means.predicted)
tmp.data = data.frame(utterance = "simple-neg", Mean = 0, ccPrior = "higher", data = "model")
tmp.data2 = data.frame(utterance = "simple-neg", Mean = 0, ccPrior = "lower", data = "model")
means.predicted = rbind(means.predicted, tmp.data, tmp.data2)
means.predicted

# change ccPrior to prior
means.predicted = means.predicted %>%
  rename("prior" = "ccPrior")
means.predicted

# load data from Exp 1 to compare to
d_exp1 <- read_csv("../../results/main/main01/data/cd.csv")
nrow(d_exp1) #873
names(d_exp1)

# calculate mean inference in exp 1 by utterance and prior, for default QUDs
means.exp1 = d_exp1 %>%
  filter(!(grepl("know",utterance) & qud == "ccQUD")) %>%
  filter(!(grepl("think",utterance) & qud == "ccQUD")) %>%
  filter(!(grepl("simple",utterance) & qud == "mcQUD")) %>%
  group_by(utterance, prior) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(data = "human")
means.exp1

means.exp1.noPrior = d_exp1 %>%
  filter(!(grepl("know",utterance) & qud == "ccQUD")) %>%
  filter(!(grepl("think",utterance) & qud == "ccQUD")) %>%
  filter(!(grepl("simple",utterance) & qud == "mcQUD")) %>%
  group_by(utterance) %>%
  summarize(Mean = mean(response))
means.exp1.noPrior

# add fake YMin and YMax to the model predictions, for binding with human data
means.predicted = means.predicted %>%
  mutate(YMin = 0) %>%
  mutate(YMax = 0)

means.exp1
means.predicted

tmp = rbind(means.predicted,means.exp1)
tmp

# relevel the utterances
tmp$utterance = factor(tmp$utterance, levels = means.exp1.noPrior$utterance[order(means.exp1.noPrior$Mean)], ordered = TRUE)
levels(tmp$utterance)

# relevel the data
tmp$data = factor(tmp$data, levels = c("model","human"))
levels(tmp$data)

# relevel the prior
tmp$prior <- factor(tmp$prior, levels = c("lower","higher"))

ggplot(tmp, aes(x=utterance, y=Mean, shape = prior, color=data, fill=data, group = interaction(utterance, data))) +
  geom_point(size = 3) + 
  scale_shape_manual(values=c(25,24)) +
  theme(legend.position="top") +
  scale_color_manual(values = c("red","black")) +
  scale_fill_manual(values = c("red","black")) +
  geom_errorbar(data=tmp[tmp$data == "human",], aes(ymin=YMin,ymax=YMax),width=.05,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  scale_x_discrete(labels=c("simple-pos"="C", "simple-neg"="not C", "know-pos" = "Cole knows \n that C", "know-neg" = "Cole doesn't \n know that C",
                            "think-pos" = "Cole thinks \n that C", "think-neg" = "Cole doesn't \n think that C")) +
  ylab("Predicted probability (red) \n Mean inference rating (black)") +
  xlab("\nUtterance")
#ggsave("../../../../../../Downloads/results-by-utt-and-prior.pdf",height=3,width=6)




