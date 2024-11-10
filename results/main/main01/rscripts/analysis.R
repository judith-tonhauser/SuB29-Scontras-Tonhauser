# data to evaluate RSA model for "know" (main 01)
# analysis.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)
library(lme4)
library(lmerTest)

# load cleaned data
d = read_csv("../data/cd.csv")
nrow(d) #873

# analyses reported in SuB abstract and paper ----
names(d)
table(d$utterance)
table(d$qud)
table(d$prior)

# set reference levels
d = d %>%
  mutate(utterance = fct_relevel(utterance, "think-neg"), 
         qud = fct_relevel(qud, "ai"),
         prior = fct_relevel(prior, "lower"))

# H1
m = lmer(response ~ utterance + (1|prior) + (1|content), data=d[d$utterance == "know-neg" | d$utterance == "think-neg",])
summary(m)
#utteranceknow-neg   0.34483    0.02829 376.99996  12.188   <2e-16 ***

# H2
m = lmer(response ~ prior + (1|utterance) + (1|content), data=d[d$utterance == "know-neg" | d$utterance == "think-neg",])
summary(m)
#priorhigher   0.15469    0.02833 377.00067   5.460 8.63e-08 ***

m = lmer(response ~ prior + (1|content) + (1|qud), data=d[d$utterance == "know-neg",])
summary(m)
# priorhigher   0.12619    0.04214 197.14032   2.995   0.0031 **

m = lmer(response ~ prior + (1|content) + (1|qud), data=d[d$utterance == "think-neg",])
summary(m)
# priorhigher   0.19355    0.03689 179.00000   5.247 4.35e-07 ***

# H3
m = lmer(response ~ qud + (1|prior) + (1|utterance) + (1|content), data=d[d$utterance == "know-neg" | d$utterance == "think-neg",])
summary(m)
# qudnai      9.162e-03  2.853e-02 3.763e+02   0.321    0.748

