# data to evaluate RSA model "know" vs "think" (main01)
# graphs

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)
library(ggrepel)
library(dichromat)
library(forcats)
library(ggrepel)

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

theme_set(theme_bw())

# load helper functions
source('../../../helpers.R')

# load cleaned data
d = read_csv("../data/cd.csv")
nrow(d) #873

names(d)

# Fig 2 ----

#### mean rating by utterance type, prior and QUD ----

# calculate by-utterance mean to sort utterances
means.utt = d %>%
  group_by(utterance) %>%
  summarize(Mean = mean(response))
means.utt

# calculate mean by utterance, prior and qud
means.utt.prior.qud = d %>%
  group_by(utterance,prior,qud) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) 
means.utt.prior.qud

means.utt.prior.qud$utterance = factor(means.utt.prior.qud$utterance, levels = means.utt$utterance[order(means.utt$Mean)], ordered = TRUE)
levels(means.utt.prior.qud$utterance)

# relevel prior
means.utt.prior.qud$prior <- factor(means.utt.prior.qud$prior, levels = c("lower","higher"))

# relevel QUD
means.utt.prior.qud$qud <- factor(means.utt.prior.qud$qud, levels = c("nai","ai"), labels = c("BEL?", "C?"))

ggplot(means.utt.prior.qud, aes(x=utterance, y=Mean)) +
  geom_point(aes(shape=prior,fill=qud), size = 3) + 
  scale_fill_manual(values=c("#E69F00","#56B4E9")) +
  scale_shape_manual(values=c(25,24)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0,color="black") +
  theme(legend.position="top") +
  guides(shape=guide_legend("prior probability of content C"),fill=guide_legend("QUD",override.aes=list(shape=21))) +
  theme(axis.text.y = element_text(size=10)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  scale_x_discrete(labels=c("simple-pos"="C", "simple-neg"="not C", "know-pos" = "Cole knows \n that C", "know-neg" = "Cole doesn't \n know that C",
                            "think-pos" = "Cole thinks \n that C", "think-neg" = "Cole doesn't \n think that C")) +
  ylab("Mean inference rating") +
  xlab("Utterance") +
  theme(axis.title.x=element_blank()) 
ggsave("../graphs/mean-rating-by-utt-prior-qud.pdf",height=2.7,width=6)

# Fig 3 ----

##### P1 in panel (a): negated know- vs. think-utterances, collapsing across priors and QUDs ----

# select relevant utterances and QUDs: negated know and think
h1 = d %>%
  filter(predicate == "know" | predicate == "think") %>%
  filter(valence == "neg") 
table(h1$utterance)
table(h1$qud)

# calculate mean inference in exp1 to order utterances
means.utt = d %>%
  group_by(utterance) %>%
  summarize(Mean = mean(response))
means.utt

# calculate mean inference ratings in the two experiments with QUD BEL?

# exp1
means_exp1 = h1 %>%
  group_by(utterance) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) 
means_exp1

# relevel the utterances
means_exp1$utterance = factor(means_exp1$utterance, levels = means.utt$utterance[order(means.utt$Mean)], ordered = TRUE)
levels(means_exp1$utterance)

# plot
ggplot(means_exp1, aes(x=utterance, y=Mean)) +
  #geom_point(data=PL_agg.utt, shape=21, aes(x=utterance, y=prob), size = 3, color = "cornflowerblue", fill="cornflowerblue") +
  #geom_point(data=means_exp2, aes(x=utterance, y=Mean), color="#E69F00", size = 3, position=position_nudge(x=.2)) +
  #geom_errorbar(data=means_exp2, aes(ymin=YMin,ymax=YMax), width=.05,color="#E69F00",position=position_nudge(x=.2)) +
  geom_point(shape=21, size = 3, color = "black", fill="black") + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.05,color="black") +
  theme(legend.position="top") +
  guides(shape=guide_legend("prior probability of target content"),fill=guide_legend("QUD", override.aes=list(shape=21))) +
  theme(axis.text.y = element_text(size=10)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  scale_x_discrete(labels=c("simple-pos"="C", "simple-neg"="not C", "know-pos" = "Cole knows \n that C", "know-neg" = "Cole doesn't \n know that C",
                            "think-pos" = "Cole thinks \n that C", "think-neg" = "Cole doesn't \n think that C")) +
  ylab("Mean inference rating") +
  xlab("Utterance") +
  theme(axis.title.x=element_blank()) 
ggsave("../graphs/H1.pdf",height=2.3,width=2.3)

#####  P2 in panel (b): negated know- and think-utterances by prior probability, collapsing across utterances and QUDs ----

# select relevant utterances and QUDs: negated know and think
h2 = d %>%
  filter(predicate == "know" | predicate == "think") %>%
  filter(valence == "neg")

# calculate mean inference in exp1 to order utterances
means.utt = d %>%
  group_by(utterance) %>%
  summarize(Mean = mean(response))
means.utt

# calculate mean inference ratings in exp1 with QUD BEL?

# exp1
means_exp1 = h2 %>%
  group_by(prior) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) 
means_exp1

# relevel prior in the relevant two datasets
means_exp1$prior <- factor(means_exp1$prior, levels = c("lower","higher"))

# plot 
ggplot(means_exp1, aes(x=prior, y=Mean)) +
  geom_point(shape=21, size = 3, color = "black", fill="black") + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.05,color="black") +
  theme(legend.position="none") +
  scale_shape_manual(values=c(25,24)) +
  #guides(shape=guide_legend("prior probability \n of target content"), nrow=2) +
  theme(axis.text.y = element_text(size=10)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  scale_x_discrete(labels=c("simple-pos"="C", "simple-neg"="not C", "know-pos" = "Cole knows \n that C", "know-neg" = "Cole doesn't \n know that C",
                            "think-pos" = "Cole thinks \n that C", "think-neg" = "Cole doesn't \n think that C")) +
  ylab("Mean inference rating") +
  xlab("Utterance") +
  theme(axis.title.x=element_blank()) 
ggsave("../graphs/H2.pdf",height=2.3,width=2.3)

#####  P3 in panel (c): negated know- and think-utterances by QUD, collapsing over priors ----

# select relevant utterances and QUDs: negated know and think
h3 = d %>%
  filter(predicate == "know" | predicate == "think") %>%
  filter(valence == "neg") 

# calculate mean inference in exp1 to order utterances
means.utt = d %>%
  group_by(utterance) %>%
  summarize(Mean = mean(response))
means.utt

# calculate mean inference ratings in exp2 by QUD
means_exp1 = h3 %>%
  group_by(qud) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) 
means_exp1

# relevel QUDs 
means_exp1$qud <- factor(means_exp1$qud, levels = c("ai","nai"), labels = c("C?", "BEL?"))

# plot
ggplot(means_exp1, aes(x=qud, y=Mean)) +
  #geom_point(data=PL_agg.utt.qud, shape=21, aes(x=qud, y=prob), size = 3, color = "red", fill = "red") +
  geom_point(size = 3, color="black",fill="black") + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.05,color="black") +
  theme(legend.position="none") +
  #scale_fill_manual(values=c("#E69F00","#56B4E9")) +
  #guides(fill=guide_legend("QUD")) +
  theme(axis.text.y = element_text(size=10)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,.2,.4,.6,.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  scale_x_discrete(labels=c("BEL?"="BEL?", "CC?"="C?")) +
  #scale_x_discrete(labels=c("BEL?"="Does Cole \n believe C?", "C?"="Does Charley \n speak Spanish?")) +
  ylab("Mean inference rating") +
  xlab("Utterance") +
  theme(axis.title.x=element_blank()) 
ggsave("../graphs/H3.pdf",height=2.3,width=2.3)

