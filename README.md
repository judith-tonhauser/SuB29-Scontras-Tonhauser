This repo contains the models, experiments, data and R code for generating the figures and analyses of the experiments reported on in Scontras & Tonhauser 2025 "Projection without presupposition: A model for know", Sinn und Bedeutung 29.

### /experiments: This folder contains the experiments that were designed to investigate the model predictions

Main 01: Experiment that manipulated both at-issueness and the prior (run based on pilot 01), reported as Exp 1 in the paper

Main 02: Experiment that manipulated only at-issueness for think- and know-utterances (run based on pilot 02 which still included simple clauses too), pilot 03 (excluded simple clauses)), there was a problem with the jackson/ai condition (wrong name in description, namely "charley" instead of "jackson"), reported as Exp 2 in the paper together with Main 03

Main 03: Experiment that manipulated only at-issueness (like main 02) but that fixed the problem with the jackson/ai condition (only condition that was run)

### /models: webppl models

Each folder contains the model (.wppl), a folder "data" for the data generated from the model, a folder "graphs" for figures created based on the model data, and an R script (model-evaluation.R) to evaluate the predictions of the model.

1-original-model: The model reported on in section 3.

1a-original-model-more-alternatives: The model mentioned in footnote 8.

1b-original-model-with-maxQUD: The model mentioned in footnote 9.

2-backoff-to-prior: The model reported on in section 4.

### /results: Analyses of the experiment data

The analysis of Exp 1 is in Main01; the joint analysis of Main02/Main03 is in Main03.