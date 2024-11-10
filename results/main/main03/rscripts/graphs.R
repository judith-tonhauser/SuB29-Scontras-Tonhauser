# data to evaluate RSA model "know" vs "think" (main03)
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
nrow(d) #327

names(d)

length(unique(d$participantID)) #327

# Fig 5 ----

#### mean rating by utterance type and QUD ----

means.utt = d %>%
  group_by(utterance) %>%
  summarize(Mean = mean(responseCC), CILow = ci.low(responseCC), CIHigh = ci.high(responseCC)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh))
means.utt

means.uttQUD = d %>%
  group_by(utterance,qud) %>%
  summarize(Mean = mean(responseCC), CILow = ci.low(responseCC), CIHigh = ci.high(responseCC)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(qud = recode(qud, "ai" = "C?", "nai" = "BEL?"))
means.uttQUD

means.uttQUD$utterance = factor(means.uttQUD$utterance, levels = means.utt$utterance[order(means.utt$Mean)], ordered = TRUE)
levels(means.uttQUD$utterance)

# relevel QUD
means.uttQUD$qud <- factor(means.uttQUD$qud, levels = c("BEL?","C?"), labels = c("BEL?", "C?"))
levels(means.uttQUD$qud)
means.uttQUD

ggplot(means.uttQUD, aes(x=utterance, y=Mean)) +
  geom_point(aes(fill=qud), shape=21, size = 3) +  
  scale_fill_manual(values=c("#E69F00","#56B4E9")) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0,color="black") +
  theme(legend.position="top") +
  guides(fill=guide_legend("QUD")) +
  theme(axis.text.y = element_text(size=10)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  scale_x_discrete(labels=c("simple-pos"="C", "simple-neg"="not C", "know-pos" = "Cole knows \n that C", "know-neg" = "Cole doesn't \n know that C",
                            "think-pos" = "Cole thinks \n that C", "think-neg" = "Cole doesn't \n think that C")) +
  ylab("Mean inference rating") +
  xlab("Utterance") +
  theme(axis.title.x=element_blank()) 
ggsave("../graphs/mean-rating-by-utterance-type-and-QUD.pdf",height=2.7,width=4)

# Fig 6 ----

##### H1 in panel (a): Negated know- vs. think-utterances ----

# select relevant utterances and QUDs: negated know and think, collapsing across QUDs
h1 = d %>%
  filter(predicate == "know" | predicate == "think") %>%
  filter(valence == "neg") 
table(h1$utterance)
table(h1$qud)

# calculate mean inference to order utterances
means.utt = d %>%
  group_by(utterance) %>%
  summarize(Mean = mean(responseCC))
means.utt

# calculate mean inference ratings in the two experiments with QUD BEL?

# exp2
means_exp2 = h1 %>%
  group_by(utterance) %>%
  summarize(Mean = mean(responseCC), CILow = ci.low(responseCC), CIHigh = ci.high(responseCC)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) 
means_exp2

# relevel the utterances
means_exp2$utterance = factor(means_exp2$utterance, levels = means.utt$utterance[order(means.utt$Mean)], ordered = TRUE)
levels(means_exp2$utterance)

# plot projection (QUD BEL) for neg-know and neg-think (Exp 1)
ggplot(means_exp2, aes(x=utterance, y=Mean)) +
  #geom_point(data=PL_agg.utt, shape=21, aes(x=utterance, y=prob), size = 3, color = "cornflowerblue", fill="cornflowerblue") +
  #geom_point(data=means_exp2, aes(x=utterance, y=Mean), color="#E69F00", size = 3, position=position_nudge(x=.2)) +
  #geom_errorbar(data=means_exp2, aes(ymin=YMin,ymax=YMax), width=.05,color="#E69F00",position=position_nudge(x=.2)) +
  geom_point(shape=21, size = 3, color = "black", fill="black") + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.05,color="black") +
  theme(legend.position="top") +
  guides(shape=guide_legend("prior probability of target content"),fill=guide_legend("QUD", override.aes=list(shape=21))) +
  theme(axis.text.y = element_text(size=10)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  scale_x_discrete(labels=c("simple-pos"="c", "simple-neg"="not c", "know-pos" = "Cole knows \n that c", "know-neg" = "Cole doesn't \n know that c",
                            "think-pos" = "Cole thinks \n that c", "think-neg" = "Cole doesn't \n think that c")) +
  ylab("Mean inference rating") +
  xlab("Utterance") +
  theme(axis.title.x=element_blank()) 
ggsave("../graphs/H1.pdf",height=2.3,width=2.3)

#####  H3 in panel (b): negated know- and think-utterances by QUD, collapsing across utterances ----

# select relevant utterances and QUDs: negated know and think
h3 = d %>%
  filter(predicate == "know" | predicate == "think") %>%
  filter(valence == "neg") 

# calculate mean inference in exp1 to order utterances
means.utt = d %>%
  group_by(utterance) %>%
  summarize(Mean = mean(responseCC))
means.utt

# calculate mean inference ratings in exp2 by QUD
means_exp2 = h3 %>%
  group_by(qud) %>%
  summarize(Mean = mean(responseCC), CILow = ci.low(responseCC), CIHigh = ci.high(responseCC)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) 
means_exp2

# relevel QUDs 
means_exp2$qud <- factor(means_exp2$qud, levels = c("ai","nai"), labels = c("C?", "BEL?"))

# plot
ggplot(means_exp2, aes(x=qud, y=Mean)) +
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

#### Hypothesis3 for negated know and think separately ----

# select relevant utterances and QUDs: negated know and think, QUD BEL?
h3 = d %>%
  filter(predicate == "know" | predicate == "think") %>%
  filter(valence == "neg") 

# calculate mean inference in exp1 to order utterances
means.utt = d %>%
  group_by(utterance) %>%
  summarize(Mean = mean(responseCC))
means.utt

# calculate mean inference ratings in exp2 by QUD
means_exp2 = h3 %>%
  group_by(qud,utterance) %>%
  summarize(Mean = mean(responseCC), CILow = ci.low(responseCC), CIHigh = ci.high(responseCC)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) 
means_exp2

# relevel QUDs 
means_exp2$qud <- factor(means_exp2$qud, levels = c("nai","ai"), labels = c("BEL?", "C?"))

# plot
ggplot(means_exp2[means_exp2$utterance == "know-neg",], aes(x=qud, y=Mean)) +
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
ggsave("../graphs/H3-neg-know.pdf",height=2.7,width=2.3)

ggplot(means_exp2[means_exp2$utterance == "think-neg",], aes(x=qud, y=Mean)) +
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
ggsave("../graphs/H3-neg-think.pdf",height=2.3,width=2.3)

