#########################################
### import packages###
library(tidyverse)
library(gghalves)
library(here)
#You can try to register the fonts first using extrafont.
#https://stackoverflow.com/questions/46662296/ggplot-r-error-in-grid-call-and-font-family#
library(extrafont)
font_import()
loadfonts(device = "pdf")
library(patchwork)
set.seed(123)
###########################################
###########################################
### import dataset
df.agg1 <- read_csv(here("1_Data", "2_Postpro_data", "Exp1_RT_Agg.csv"))
df.agg2 <- read_csv(here("1_Data", "2_Postpro_data", "Exp2_RT_Agg.csv"))
df.agg1_d <- read_csv(here("1_Data", "2_Postpro_data", "Exp1_dprime.csv"))
df.agg2_d <- read_csv(here("1_Data", "2_Postpro_data", "Exp2_dprime.csv"))
#########################################
df.agg1 <- df.agg1 %>%
  dplyr::mutate(
    Sequence = dplyr::case_when(
      condition == "word_first" ~ "Word first",
      condition == "image_first" ~ "Image first",
      condition == "simultaneous" ~ "Simultaneous"
    ), 
    Seq_posi = dplyr::case_when(
      valence == "circle" ~ 1,
      valence == "triangle" ~ 3,
      valence == "square" ~ 5
    ), 
    Seq_nudge = dplyr::case_when(
      valence == "circle" & matchness == "match" ~ 0.8,
      valence == "circle" & matchness == "mismatch" ~ 0.5,
      valence == "triangle" & matchness == "match" ~ 2.8,
      valence == "triangle" & matchness == "mismatch" ~ 2.5,
      valence == "square" & matchness == "match" ~ 4.8,
      valence == "square" & matchness == "mismatch" ~ 4.5,
    ), 
    Con_j = jitter(Seq_nudge, amount = 0.09)
  )


df_avg1 <- df.agg1 %>%
  dplyr::group_by(Sequence, matchness, valence) %>%
  dplyr::summarise(
    n = n(),
    meanrt = mean(rt_m),
    sd = sd(rt_m),
    se = sd / sqrt(n)
  )  %>% 
  dplyr::mutate(
    Seq_posi = dplyr::case_when(
      valence == "circle" ~ 1,
      valence == "triangle" ~ 3,
      valence == "square" ~ 5
    )) %>% 
  dplyr::ungroup()



df.agg1$valence <- factor(df.agg1$valence, levels = c("circle", "square", "triangle"))
df_avg1$valence <- factor(df_avg1$valence, levels = c("circle", "square", "triangle"))
df.agg1$Sequence <- factor(df.agg1$Sequence, levels = c("Word first", "Image first", "Simultaneous"))
df_avg1$Sequence <- factor(df_avg1$Sequence, levels = c("Word first", "Image first", "Simultaneous"))


ggplot(data = df.agg1, aes(y = rt_m)) + 
  geom_boxplot(data = df.agg1 %>% dplyr::filter(Seq_posi == 1), aes(x = Seq_nudge, y = rt_m, fill = matchness), outlier.shape = NA, width = 0.1) +
  geom_boxplot(data = df.agg1 %>% dplyr::filter(Seq_posi == 3), aes(x = Seq_nudge, y = rt_m, fill = matchness), outlier.shape = NA, width = 0.1) +
  geom_boxplot(data = df.agg1 %>% dplyr::filter(Seq_posi == 5), aes(x = Seq_nudge, y = rt_m, fill = matchness), outlier.shape = NA, width = 0.1) + 
  geom_point(data = df.agg1 %>% dplyr::filter(Seq_posi == 1), aes(x = Con_j, y = rt_m, color = matchness), alpha = 0.3) + 
  geom_point(data = df.agg1 %>% dplyr::filter(Seq_posi == 3), aes(x = Con_j, y = rt_m, color = matchness), alpha = 0.3) + 
  geom_point(data = df.agg1 %>% dplyr::filter(Seq_posi == 5), aes(x = Con_j, y = rt_m, color = matchness), alpha = 0.3) + 
  geom_line(data = df.agg1 %>% filter(matchness == "match"), aes(x = Con_j, group = subj_idx), color = "lightgrey", alpha = 0.3) + 
  geom_line(data = df.agg1 %>% filter(matchness == "mismatch"), aes(x = Con_j, group = subj_idx), color = "lightgrey", alpha = 0.3) + 
  gghalves::geom_half_violin(data = df.agg1 %>% dplyr::filter(Seq_posi == 1), aes(x = Seq_posi, y = rt_m, fill = matchness), side = "r", position = position_nudge(x = .2),
                             trim = FALSE, alpha = 0.8, color = NA) +
  gghalves::geom_half_violin(data = df.agg1 %>% dplyr::filter(Seq_posi == 3), aes(x = Seq_posi, y = rt_m, fill = matchness), side = "r", position = position_nudge(x = .2),
                             trim = FALSE, alpha = 0.8, color = NA) + 
  gghalves::geom_half_violin(data = df.agg1 %>% dplyr::filter(Seq_posi == 5), aes(x = Seq_posi, y = rt_m, fill = matchness), side = "r", position = position_nudge(x = .2),
                             trim = FALSE, alpha = 0.8, color = NA) + 
  geom_pointrange(
    data = df_avg1, aes(
      x = Seq_posi, y = meanrt,
      ymin = meanrt - se,
      ymax = meanrt + se,
      color = matchness, fill = matchness
    ), fatten = 2.5,
    linewidth = 1,
    alpha = 1, position = position_nudge(x = 0.1), show.legend = FALSE
  ) + 
  facet_wrap(~Sequence) + 
  theme_bw()







