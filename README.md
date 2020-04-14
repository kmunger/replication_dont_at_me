# replication_dont_at_me

This repository contains replication materials for the article (Munger, 2020). <i>Don't @ Me: Experimentally Reducing Partisan Incivility on Twitter</i>.

All of the data provided has been calculated from word counts collected from subjects' Twitter accounts. In order to protect subjects' privacy, the raw text is not avaliable; even a single tweet can be enough to uniquely identify a user.

To replicate the figures in both the body of the text and the appendix, first run analyze_coded_tweets_total.R. This produces Figure 9 as well as three datasets used in the rest of the analysis: subjects_70th.RData, subjects_90th.RData, and subjects_civil.RData.

The following list of R files labels the figures or tables they produce:

model_ols_pooled.R : Figures 4 and 15

model_ols_semipooled.R : Figures 5 and 11 and 15

model_ols.R : Figures 6 and 11

model_ols.R : Figures 7 and 17

model_interaction_effects.R : Figure 8

measure_validation.R : Figure 10

negative_binomial_model.R : Figures 12 and 13

model_ols_loquacity.R : Figure 14

ideology_graph.R : Figure 16
