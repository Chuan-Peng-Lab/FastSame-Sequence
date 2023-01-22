options(scipen = 999) # force R to output in decimal instead of scientifc notion
options(digits = 5) # limit the number of reporting
# rm(list = setdiff(ls(), lsf.str()))  # remove all data but keep functions
rm(list = ls())

##### import packages and functions
library(here)
library(tidyverse)

here::here()

source(here::here("2_Analysis", "1_Prepro_code", "functions.R"))

##### set the path
recent_file <- here::here("1_Data", "2_Postpro_data")

##### load the dataset
dat_exp1 <- readr::read_csv(paste(recent_file, "Exp1_postpro.csv", sep = "/"))

dat_exp2 <- readr::read_csv(paste(recent_file, "Exp2_postpro.csv", sep = "/"))

##### Rule 1: wrong trials numbers because of procedure errors
# The wrong subject in exp1
excldSub1_r1 <- dat_exp1 %>%
  dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0)) %>% # no response as wrong
  dplyr::group_by(subj_idx, matchness, valence, condition) %>%
  dplyr::summarise(N = length(ACC)) %>% # count the trial # for each condition of each subject
  dplyr::ungroup() %>%
  dplyr::filter(N != 40) %>% # filter the rows that trial Number is not 75
  dplyr::distinct(subj_idx) %>% # find the unique subject ID
  dplyr::pull(subj_idx) # pull the subj ID as vector

# The wrong subject in exp2
excldSub2_r1 <- dat_exp2 %>%
  dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0)) %>% # no response as wrong
  dplyr::group_by(subj_idx, matchness, valence, condition) %>%
  dplyr::summarise(N = length(ACC)) %>% # count the trial # for each condition of each subject
  dplyr::ungroup() %>%
  dplyr::filter(N != 40) %>% # filter the rows that trial Number is not 75
  dplyr::distinct(subj_idx) %>% # find the unique subject ID
  dplyr::pull(subj_idx) # pull the subj ID as vector

### Rule 2:  overall accuracy < 0.5
excldSub1_r2 <- dat_exp1 %>%
  dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0)) %>% # no response as wrong
  dplyr::group_by(subj_idx) %>%
  dplyr::summarise(
    N = length(ACC),
    countN = sum(ACC),
    ACC = sum(ACC) / length(ACC)
  ) %>% # count the trial # for each condition of each subject
  dplyr::ungroup() %>%
  dplyr::filter(ACC < .5) %>% # filter the subjects with over all ACC < 0.5
  dplyr::distinct(subj_idx) %>% # find the unique subject ID
  dplyr::pull(subj_idx) # pull the subj ID as vector

excldSub2_r2 <- dat_exp2 %>%
  dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0)) %>% # no response as wrong
  dplyr::group_by(subj_idx) %>%
  dplyr::summarise(
    N = length(ACC),
    countN = sum(ACC),
    ACC = sum(ACC) / length(ACC)
  ) %>% # count the trial # for each condition of each subject
  dplyr::ungroup() %>%
  dplyr::filter(ACC < .5) %>% # filter the subjects with over all ACC < 0.5
  dplyr::distinct(subj_idx) %>% # find the unique subject ID
  dplyr::pull(subj_idx) # pull the subj ID as vector

### Rule 3:  one condition with zero ACC
excldSub1_r3 <- dat_exp1 %>%
  dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0)) %>% # no response as wrong
  dplyr::group_by(subj_idx, matchness, valence, condition) %>%
  dplyr::summarise(
    N = length(ACC),
    countN = sum(ACC),
    ACC = sum(ACC) / length(ACC)
  ) %>% # count the trial # for each condition of each subject
  dplyr::ungroup() %>%
  dplyr::filter(ACC == 0) %>% # filter the subjects with over all ACC < 0.5
  dplyr::distinct(subj_idx) %>% # find the unique subject ID
  dplyr::pull(subj_idx) # pull the subj ID as vector

excldSub2_r3 <- dat_exp2 %>%
  dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0)) %>% # no response as wrong
  dplyr::group_by(subj_idx, matchness, valence, condition) %>%
  dplyr::summarise(
    N = length(ACC),
    countN = sum(ACC),
    ACC = sum(ACC) / length(ACC)
  ) %>% # count the trial # for each condition of each subject
  dplyr::ungroup() %>%
  dplyr::filter(ACC == 0) %>% # filter the subjects with over all ACC < 0.5
  dplyr::distinct(subj_idx) %>% # find the unique subject ID
  dplyr::pull(subj_idx) # pull the subj ID as vector


# all participants excluded(no participants were excluded in two expriments)
excldSub1 <- c(excldSub1_r1, excldSub1_r2, excldSub1_r3)
excldSub2 <- c(excldSub2_r1, excldSub2_r2, excldSub2_r3)
# select valid data for further analysis
dat_exp1.V <- dat_exp1 %>%
  dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0)) %>% # no response as wrong
  dplyr::filter(!subj_idx %in% excldSub1) # exclude the invalid subjects

dat_exp2.V <- dat_exp2 %>%
  dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0)) %>% # no response as wrong
  dplyr::filter(!subj_idx %in% excldSub2) # exclude the invalid subjects


# exclude the trial'rt with null
dat_exp1.V <- dat_exp1.V %>%
  dplyr::filter(rt != "null")

dat_exp2.V <- dat_exp2.V %>%
  dplyr::filter(rt != "null")

dat_exp1.V$rt <- as.numeric(dat_exp1.V$rt)
dat_exp2.V$rt <- as.numeric(dat_exp2.V$rt)
# excluded correct trials with < 200ms RT
ratio.excld.trials.1 <- nrow(dat_exp1.V[dat_exp1.V$rt <= 200 & dat_exp1.V$ACC == 1, ]) / nrow(dat_exp1.V) # ratio of invalid trials
ratio.excld.trials.2 <- nrow(dat_exp2.V[dat_exp2.V$rt <= 200 & dat_exp2.V$ACC == 1, ]) / nrow(dat_exp2.V)

dat_exp1.V <- dat_exp1.V %>% dplyr::filter(!(rt <= 200 & ACC == 1)) # filter invalid trials
dat_exp2.V <- dat_exp2.V %>% dplyr::filter(!(rt <= 200 & ACC == 1)) # filter invalid trials


######### write the valid data to csv
outpath_v <- here::here("1_Data", "2_Postpro_data")
# readr::write_csv(dat_exp1.V, paste(outpath_v, "Exp1_valid.csv", sep = "/"))
# readr::write_csv(dat_exp2.V, paste(outpath_v, "Exp2_valid.csv", sep = "/"))

####################################
###############   ACC    ###########
####################################
df.exp1.V.acc <- dat_exp1.V %>%
  dplyr::group_by(subj_idx, matchness, valence, condition) %>%
  dplyr::summarise(
    N = length(ACC),
    countN = sum(ACC),
    ACC = sum(ACC) / length(ACC)
  )


df.exp2.V.acc <- dat_exp2.V %>%
  dplyr::group_by(subj_idx, matchness, valence, condition) %>%
  dplyr::summarise(
    N = length(ACC),
    countN = sum(ACC),
    ACC = sum(ACC) / length(ACC)
  )

######### write the ACC data to csv
# readr::write_csv(df.exp1.V.acc, paste(outpath_v, "Exp1_ACC.csv", sep = "/"))
# readr::write_csv(df.exp2.V.acc, paste(outpath_v, "Exp2_ACC.csv", sep = "/"))


####################################
############   d prime   ###########
####################################
# calculate the number of hit,CR,miss or FA a
exp1.V.sdt <- dat_exp1.V %>%
  dplyr::mutate(sdt = case_when(
    matchness == "match" & ACC == 1 ~ "hit",
    matchness == "mismatch" & ACC == 1 ~ "CR",
    matchness == "match" & ACC == 0 ~ "miss",
    matchness == "mismatch" & ACC == 0 ~ "FA"
  ))



exp2.V.sdt <- dat_exp2.V %>%
  dplyr::mutate(sdt = case_when(
    matchness == "match" & ACC == 1 ~ "hit",
    matchness == "mismatch" & ACC == 1 ~ "CR",
    matchness == "match" & ACC == 0 ~ "miss",
    matchness == "mismatch" & ACC == 0 ~ "FA"
  ))
# calculate the number of each for each condition
exp1.V.sdt <- exp1.V.sdt %>%
  dplyr::group_by(subj_idx, valence, condition, sdt) %>%
  dplyr::summarise(N = length(sdt)) %>%
  dplyr::filter(!is.na(sdt)) # no NAs


exp2.V.sdt <- exp2.V.sdt %>%
  dplyr::group_by(subj_idx, valence, condition, sdt) %>%
  dplyr::summarise(N = length(sdt)) %>%
  dplyr::filter(!is.na(sdt)) # no NAs
# long format to wide
exp1.V.sdt_w <- tidyr::pivot_wider(exp1.V.sdt, names_from = sdt, values_from = "N")
exp1.V.sdt_w <- exp1.V.sdt_w %>%
  dplyr::mutate(
    miss = ifelse(is.na(miss), 0, miss), # if not miss trial, to 0
    FA = ifelse(is.na(FA), 0, FA), # if not FA trial, to 0
    hitR = hit / (hit + miss), # calculate the hit rate
    faR = FA / (FA + CR)
  ) # calculate the FA rate


exp2.V.sdt_w <- tidyr::pivot_wider(exp2.V.sdt, names_from = sdt, values_from = "N")
exp2.V.sdt_w <- exp2.V.sdt_w %>%
  dplyr::mutate(
    miss = ifelse(is.na(miss), 0, miss), # if not miss trial, to 0
    FA = ifelse(is.na(FA), 0, FA), # if not FA trial, to 0
    hitR = hit / (hit + miss), # calculate the hit rate
    faR = FA / (FA + CR)
  ) # calculate the FA rate

# standardized way to deal with the extreme values
for (i in 1:nrow(exp1.V.sdt_w)) {
  if (exp1.V.sdt_w$hitR[i] == 1) {
    exp1.V.sdt_w$hitR[i] <- 1 - 1 / (2 * (exp1.V.sdt_w$hit[i] + exp1.V.sdt_w$miss[i]))
  }
}

for (i in 1:nrow(exp1.V.sdt_w)) {
  if (exp1.V.sdt_w$faR[i] == 0) {
    exp1.V.sdt_w$faR[i] <- 1 / (2 * (exp1.V.sdt_w$FA[i] + exp1.V.sdt_w$CR[i]))
  }
}


for (i in 1:nrow(exp2.V.sdt_w)) {
  if (exp2.V.sdt_w$hitR[i] == 1) {
    exp2.V.sdt_w$hitR[i] <- 1 - 1 / (2 * (exp2.V.sdt_w$hit[i] + exp2.V.sdt_w$miss[i]))
  }
}

for (i in 1:nrow(exp2.V.sdt_w)) {
  if (exp2.V.sdt_w$faR[i] == 0) {
    exp2.V.sdt_w$faR[i] <- 1 / (2 * (exp2.V.sdt_w$FA[i] + exp2.V.sdt_w$CR[i]))
  }
}

# calculate the d prime for each condition
exp1.V.sdt_w <- exp1.V.sdt_w %>%
  mutate(dprime = qnorm(hitR) - qnorm(faR))


exp2.V.sdt_w <- exp2.V.sdt_w %>%
  mutate(dprime = qnorm(hitR) - qnorm(faR))

######### write the dprime data to csv
# readr::write_csv(exp1.V.sdt_w, paste(outpath_v, "Exp1_dprime.csv", sep = "/"))
# readr::write_csv(exp2.V.sdt_w, paste(outpath_v, "Exp2_dprime.csv", sep = "/"))

####################################
############      RT     ###########
####################################

df.exp1_RT <- dat_exp1.V %>%
  filter(ACC == 1)

df.exp2_RT <- dat_exp2.V %>%
  filter(ACC == 1)
######### write the unaggregated data to csv
# readr::write_csv(df.exp1_RT, paste(outpath_v, "Exp1_RT.csv", sep = "/"))
# readr::write_csv(df.exp2_RT, paste(outpath_v, "Exp2_RT.csv", sep = "/"))

######## Aggregated data
df.exp1_RT.subj <- df.exp1_RT %>%
  dplyr::group_by(
    subj_idx, valence, matchness, condition
  ) %>%
  dplyr::summarise(
    rt_m = mean(rt, na.rm = T),
    sd = sd(rt),
    var = var(rt)
  ) %>%
  dplyr::ungroup()


df.exp2_RT.subj <- df.exp2_RT %>%
  dplyr::group_by(
    subj_idx, valence, matchness, condition
  ) %>%
  dplyr::summarise(
    rt_m = mean(rt, na.rm = T),
    sd = sd(rt),
    var = var(rt)
  ) %>%
  dplyr::ungroup()

######### write the aggregated data to csv
# readr::write_csv(df.exp1_RT.subj, paste(outpath_v, "Exp1_RT_Agg.csv", sep = "/"))
# readr::write_csv(df.exp2_RT.subj, paste(outpath_v, "Exp2_RT_Agg.csv", sep = "/"))
