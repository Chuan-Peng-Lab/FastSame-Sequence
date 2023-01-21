##### import packages
library(here)
library(tidyverse) 

##### set the path 
here::here()
recent_file <- here::here("1_Data", "2_Postpro_data")

##### load the dataset
dat_exp1 <- readr::read_csv(paste(recent_file, "Exp1_postpro.csv", sep = "/"))

dat_exp2 <- readr::read_csv(paste(recent_file, "Exp2_postpro.csv", sep = "/"))

##### Rule 1: wrong trials numbers because of procedure errors
# The wrong subject in exp1
excldSub1_r1 <- dat_exp1 %>%
  dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0))  %>%  # no response as wrong
  dplyr::group_by(subj_idx, matchness, valence,condition) %>%
  dplyr::summarise(N = length(ACC)) %>%  # count the trial # for each condition of each subject
  dplyr::ungroup() %>%
  dplyr::filter(N != 40) %>%             # filter the rows that trial Number is not 75
  dplyr::distinct(subj_idx) %>%           # find the unique subject ID
  dplyr::pull(subj_idx)                   # pull the subj ID as vector

# The wrong subject in exp2
excldSub2_r1 <- dat_exp2 %>%
  dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0))  %>%  # no response as wrong
  dplyr::group_by(subj_idx, matchness, valence,condition) %>%
  dplyr::summarise(N = length(ACC)) %>%  # count the trial # for each condition of each subject
  dplyr::ungroup() %>%
  dplyr::filter(N != 40) %>%             # filter the rows that trial Number is not 75
  dplyr::distinct(subj_idx) %>%           # find the unique subject ID
  dplyr::pull(subj_idx)                   # pull the subj ID as vector

### Rule 2:  overall accuracy < 0.5
excldSub1_r2 <- dat_exp1 %>%
  dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0))  %>%  # no response as wrong
  dplyr::group_by(subj_idx) %>%
  dplyr::summarise(N = length(ACC),
                   countN = sum(ACC),
                   ACC = sum(ACC)/length(ACC)) %>%  # count the trial # for each condition of each subject
  dplyr::ungroup() %>%
  dplyr::filter(ACC < .5) %>%             # filter the subjects with over all ACC < 0.5
  dplyr::distinct(subj_idx) %>%             # find the unique subject ID
  dplyr::pull(subj_idx)                     # pull the subj ID as vector

excldSub2_r2 <- dat_exp2 %>%
  dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0))  %>%  # no response as wrong
  dplyr::group_by(subj_idx) %>%
  dplyr::summarise(N = length(ACC),
                   countN = sum(ACC),
                   ACC = sum(ACC)/length(ACC)) %>%  # count the trial # for each condition of each subject
  dplyr::ungroup() %>%
  dplyr::filter(ACC < .5) %>%             # filter the subjects with over all ACC < 0.5
  dplyr::distinct(subj_idx) %>%             # find the unique subject ID
  dplyr::pull(subj_idx)                     # pull the subj ID as vector

### Rule 3:  one condition with zero ACC
excldSub1_r3 <- dat_exp1 %>%
  dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0))  %>%  # no response as wrong
  dplyr::group_by(subj_idx, matchness, valence, condition) %>%
  dplyr::summarise(N = length(ACC),
                   countN = sum(ACC),
                   ACC = sum(ACC)/length(ACC)) %>%  # count the trial # for each condition of each subject
  dplyr::ungroup() %>%
  dplyr::filter(ACC == 0) %>%             # filter the subjects with over all ACC < 0.5
  dplyr::distinct(subj_idx) %>%             # find the unique subject ID
  dplyr::pull(subj_idx)                     # pull the subj ID as vector

excldSub2_r3 <- dat_exp2 %>%
  dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0))  %>%  # no response as wrong
  dplyr::group_by(subj_idx, matchness, valence, condition) %>%
  dplyr::summarise(N = length(ACC),
                   countN = sum(ACC),
                   ACC = sum(ACC)/length(ACC)) %>%  # count the trial # for each condition of each subject
  dplyr::ungroup() %>%
  dplyr::filter(ACC == 0) %>%             # filter the subjects with over all ACC < 0.5
  dplyr::distinct(subj_idx) %>%             # find the unique subject ID
  dplyr::pull(subj_idx)                     # pull the subj ID as vector


# all participants excluded(no participants were excluded in two expriments)
excldSub1   <- c(excldSub1_r1, excldSub1_r2,excldSub1_r3) 
excldSub2   <- c(excldSub2_r1, excldSub2_r2,excldSub2_r3) 
# select valid data for further analysis
dat_exp1.V <- dat_exp1 %>%
  dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0))  %>%  # no response as wrong
  dplyr::filter(!subj_idx %in% excldSub1)   # exclude the invalid subjects

dat_exp2.V <- dat_exp2 %>%
  dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0))  %>%  # no response as wrong
  dplyr::filter(!subj_idx %in% excldSub2)   # exclude the invalid subjects


# exclude the trial'rt with null
dat_exp1.V <- dat_exp1.V %>% 
  dplyr::filter(rt != "null")

dat_exp2.V <- dat_exp2.V %>% 
  dplyr::filter(rt != "null")

dat_exp1.V$rt <- as.numeric(dat_exp1.V$rt)
dat_exp2.V$rt <- as.numeric(dat_exp2.V$rt)
# excluded correct trials with < 200ms RT
ratio.excld.trials.1 <- nrow(dat_exp1.V[dat_exp1.V$rt <= 200 & dat_exp1.V$ACC == 1,])/nrow(dat_exp1.V)  # ratio of invalid trials
ratio.excld.trials.2 <- nrow(dat_exp2.V[dat_exp2.V$rt <= 200 & dat_exp2.V$ACC == 1,])/nrow(dat_exp2.V)

dat_exp1.V <- dat_exp1.V %>% dplyr::filter(!(rt <= 200 & ACC==1)) # filter invalid trials
dat_exp2.V <- dat_exp2.V %>% dplyr::filter(!(rt <= 200 & ACC==1)) # filter invalid trials

