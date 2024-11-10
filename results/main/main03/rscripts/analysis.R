# data to evaluate RSA model for "know" (main 03)
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
nrow(d) #327

# analyses reported in SuB abstract and paper ----
names(d)
table(d$utterance)
table(d$qud)

# set reference levels
d = d %>%
  mutate(utterance = fct_relevel(utterance, "think-neg"), 
         qud = fct_relevel(qud, "ai"))

# P1: neg-know projects more than neg-think
m = lmer(responseCC ~ utterance + (1|qud) + (1|content), data=d[d$valence == "neg",])
summary(m)
# utteranceknow-neg   0.33536    0.03829 144.23884   8.758  4.9e-15 ***

# P3: QUD BEL? projects more than QUD C?
m = lmer(responseCC ~ qud + (1|utterance) + (1|content), data=d[d$valence == "neg",])
summary(m)
# qudnai        0.13760    0.03828 144.04628   3.594 0.000446 ***

# P3 just for know
m = lmer(responseCC ~ qud + (1|content), data=d[d$predicate == "know" & d$valence == "neg",])
summary(m)
# qudnai       0.09196    0.05829 73.55435   1.578   0.1189 
# direction of effect is good, but not significant

# P3 just for think
m = lmer(responseCC ~ qud + (1|content), data=d[d$predicate == "think" & d$valence == "neg",])
summary(m)
# 0.17135    0.05051 69.36470   3.392  0.00115 **

