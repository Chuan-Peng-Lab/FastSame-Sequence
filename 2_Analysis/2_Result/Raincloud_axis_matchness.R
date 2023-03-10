#########################################
### import packages###
library(tidyverse)
library(gghalves)
library(here)
# You can try to register the fonts first using extrafont.
# https://stackoverflow.com/questions/46662296/ggplot-r-error-in-grid-call-and-font-family#
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
      matchness == "match" ~ 1,
      matchness == "mismatch" ~ 5
    ),
    Seq_nudge = dplyr::case_when(
      valence == "circle" & matchness == "match" ~ 1.3,
      valence == "circle" & matchness == "mismatch" ~ 4.9,
      valence == "triangle" & matchness == "match" ~ 1.5,
      valence == "triangle" & matchness == "mismatch" ~ 4.7,
      valence == "square" & matchness == "match" ~ 1.7,
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
  ) %>%
  dplyr::mutate(
    Seq_posi = dplyr::case_when(
      matchness == "match" ~ 1,
      matchness == "mismatch" ~ 5
    ),
    Seq_nudge = dplyr::case_when(
      valence == "circle" & matchness == "match" ~ 0.6,
      valence == "circle" & matchness == "mismatch" ~ 5.4,
      valence == "triangle" & matchness == "match" ~ 0.8,
      valence == "triangle" & matchness == "mismatch" ~ 5.2,
      valence == "square" & matchness == "match" ~ 1,
      valence == "square" & matchness == "mismatch" ~ 5.0,
    )
  ) %>%
  dplyr::ungroup()



df.agg1$valence <- factor(df.agg1$valence, levels = c("circle", "triangle", "square"))
df_avg1$valence <- factor(df_avg1$valence, levels = c("circle", "triangle", "square"))
df.agg1$Sequence <- factor(df.agg1$Sequence, levels = c("Word first", "Image first", "Simultaneous"))
df_avg1$Sequence <- factor(df_avg1$Sequence, levels = c("Word first", "Image first", "Simultaneous"))


ggplot(data = df.agg1, aes(y = rt_m)) +
  geom_boxplot(data = df.agg1 %>% dplyr::filter(Seq_posi == 1), aes(x = Seq_nudge, y = rt_m, fill = valence), outlier.shape = NA, width = 0.1) +
  geom_boxplot(data = df.agg1 %>% dplyr::filter(Seq_posi == 5), aes(x = Seq_nudge, y = rt_m, fill = valence), outlier.shape = NA, width = 0.1) +
  geom_point(data = df.agg1 %>% dplyr::filter(Seq_posi == 1), aes(x = Con_j, y = rt_m, color = valence), alpha = 0.3) +
  geom_point(data = df.agg1 %>% dplyr::filter(Seq_posi == 5), aes(x = Con_j, y = rt_m, color = valence), alpha = 0.3) +
  geom_line(data = df.agg1 %>% filter(valence == "circle"), aes(x = Con_j, group = subj_idx), color = "lightgrey", alpha = 0.3) +
  geom_line(data = df.agg1 %>% filter(valence == "triangle"), aes(x = Con_j, group = subj_idx), color = "lightgrey", alpha = 0.3) +
  geom_line(data = df.agg1 %>% filter(valence == "square"), aes(x = Con_j, group = subj_idx), color = "lightgrey", alpha = 0.3) +
  gghalves::geom_half_violin(
    data = df.agg1 %>% dplyr::filter(Seq_posi == 1), aes(x = Seq_posi, y = rt_m, fill = valence), side = "l", position = position_nudge(x = -.4),
    trim = FALSE, alpha = 0.8, color = NA
  ) +
  gghalves::geom_half_violin(
    data = df.agg1 %>% dplyr::filter(Seq_posi == 5), aes(x = Seq_posi, y = rt_m, fill = valence), side = "r", position = position_nudge(x = .6),
    trim = FALSE, alpha = 0.8, color = NA
  ) +
  geom_pointrange(
    data = df_avg1 %>% dplyr::filter(Seq_posi == 1), aes(
      x = Seq_nudge, y = meanrt,
      ymin = meanrt - se,
      ymax = meanrt + se,
      color = valence, fill = valence
    ), fatten = 2.5,
    linewidth = 1,
    alpha = 1, position = position_nudge(x = 0.1), show.legend = FALSE
  ) +
  geom_pointrange(
    data = df_avg1 %>% dplyr::filter(Seq_posi == 5), aes(
      x = Seq_nudge, y = meanrt,
      ymin = meanrt - se,
      ymax = meanrt + se,
      color = valence, fill = valence
    ), fatten = 2.5,
    linewidth = 1,
    alpha = 1, position = position_nudge(x = 0.1), show.legend = FALSE
  ) +
  facet_wrap(~Sequence) +
  theme_bw() +
  scale_x_continuous(
    name = "Matchness",
    breaks = c(1, 5),
    labels = c("Match", "Mismatch")
  ) +
  scale_y_continuous(
    name = "Reaction times(ms)"
  ) +
  guides(
    fill = guide_legend(
      keywidth = 0.6,
      keyheight = 0.6,
      default.unit = "cm",
      byrow = T
    )
  ) +
  theme(
    aspect.ratio = 4/3, 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(family = "Times New Roman"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16, face = "bold"),
    # legend.position='top',
    plot.title = element_text(lineheight = .8, face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(size = 16, color = "black", face = "bold"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(margin = margin(10, 0, 0, 0)), # increase the sapce betwen title and x axis
    axis.title.y = element_text(margin = margin(0, 10, 0, 0)), # increase the space between title and y axis
    axis.line.x = element_line(color = "black", linewidth = 1), # increase the size of font
    axis.line.y = element_line(color = "black", linewidth = 1),
    axis.ticks = element_line(linewidth = 1),
    strip.text.x = element_text(size = 16, face = "bold"),
    panel.spacing.x = unit(1, "cm"),
  ) + # increase the size of font
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF"), labels = c("Circle", "Triangle", "Square")) +
  scale_color_manual(values = c("#F8766D", "#00BA38", "#619CFF"), labels = c("Circle", "Triangle", "Square"))
ggsave("matchness_axis1.pdf", width = 15, height = 7, dpi = 300)




