###### this is the file for convenient stage analysis

df.agg1$subj_idx <- as.factor(df.agg1$subj_idx)
bf1 <- BayesFactor::generalTestBF(
  log_rt ~ valence * matchness * seq * subj_idx - subj_idx:valence:matchness:seq,
  data = data.frame(df.agg1),
  whichRandom = "subj_idx",
  neverExclude = "subj_idx",
  whichModels = "top"
)

summary(bf1)



df.agg2$subj_idx <- as.factor(df.agg2$subj_idx)
bf2 <- BayesFactor::generalTestBF(
  log_rt ~ valence * matchness * seq * subj_idx - subj_idx:valence:matchness:seq,
  data = data.frame(df.agg2),
  whichRandom = "subj_idx",
  neverExclude = "subj_idx",
  whichModels = "top"
)

summary(bf2)

