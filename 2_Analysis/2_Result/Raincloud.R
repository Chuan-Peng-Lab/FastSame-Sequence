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
###########################################
### import dataset
df.agg1 <- read_csv(here("1_Data", "2_Postpro_data", "Exp1_RT_Agg.csv"))
df.agg2 <- read_csv(here("1_Data", "2_Postpro_data", "Exp2_RT_Agg.csv"))
df.agg1_d <- read_csv(here("1_Data", "2_Postpro_data", "Exp1_dprime.csv"))
df.agg2_d <- read_csv(here("1_Data", "2_Postpro_data", "Exp2_dprime.csv"))
#########################################
### data preprocessing for the RT
#########################################
####### preprocessing for the repeated data, the points, grey lines in the plot
df.agg1 <- df.agg1 %>%
  dplyr::mutate(
    Seq_Num = dplyr::case_when(
      condition == "image_first" ~ 1,
      condition == "simultaneous" ~ 3,
      condition == "word_first" ~ 5
    ),
    Con_j = jitter(Seq_Num, amount = 0.09) # this code is use for jitter from x axis
  )

df.agg2 <- df.agg2 %>%
  dplyr::mutate(
    Seq_Num = dplyr::case_when(
      condition == "image_first" ~ 1,
      condition == "simultaneous" ~ 3,
      condition == "word_first" ~ 5
    ),
    Con_j = jitter(Seq_Num, amount = 0.09)
  )

df_avg1 <- df.agg1 %>%
  dplyr::group_by(condition, matchness, valence) %>%
  dplyr::summarise(
    n = n(),
    meanrt = mean(rt_m),
    sd = sd(rt_m),
    se = sd / sqrt(n)
  ) %>%
  dplyr::mutate(
    Seq_Num = dplyr::case_when( # change the factor to numeric for plotting
      condition == "image_first" ~ 1,
      condition == "simultaneous" ~ 3,
      condition == "word_first" ~ 5
    )
  ) %>%
  dplyr::ungroup()

df.agg1$valence <- factor(df.agg1$valence, levels = c("circle", "square", "triangle"))
df_avg1$valence <- factor(df_avg1$valence, levels = c("circle", "square", "triangle"))
###############
df_avg2 <- df.agg2 %>%
  dplyr::group_by(condition, matchness, valence) %>%
  dplyr::summarise(
    n = n(),
    meanrt = mean(rt_m),
    sd = sd(rt_m),
    se = sd / sqrt(n)
  ) %>%
  mutate(
    Seq_Num = case_when( # change the factor to numeric for plotting
      condition == "image_first" ~ 1,
      condition == "simultaneous" ~ 3,
      condition == "word_first" ~ 5
    )
  ) %>%
  dplyr::ungroup()

df.agg2$valence <- factor(df.agg2$valence, levels = c("Good", "Neutral", "Bad"))
df_avg2$valence <- factor(df_avg2$valence, levels = c("Good", "Neutral", "Bad"))
##################
plot_rt1 <- ggplot(data = df.agg1, aes(y = rt_m)) +
  geom_point(data = df.agg1 %>% filter(Seq_Num == 1), aes(x = Con_j, color = valence), size = 1.5, alpha = .5, position = position_nudge(-.3), show.legend = FALSE) +
  geom_point(data = df.agg1 %>% filter(Seq_Num == 3), aes(x = Con_j, color = valence), size = 1.5, alpha = .5, position = position_nudge(-.3), show.legend = FALSE) +
  geom_point(data = df.agg1 %>% filter(Seq_Num == 5), aes(x = Con_j, color = valence), size = 1.5, alpha = .5, position = position_nudge(-.3), show.legend = FALSE) +
  geom_line(data = df.agg1 %>% filter(valence == "circle"), aes(x = Con_j, group = subj_idx), color = "lightgrey", alpha = .3, position = position_nudge(-.3)) +
  geom_line(data = df.agg1 %>% filter(valence == "square"), aes(x = Con_j, group = subj_idx), color = "lightgrey", alpha = .3, position = position_nudge(-.3)) +
  geom_line(data = df.agg1 %>% filter(valence == "triangle"), aes(x = Con_j, group = subj_idx), color = "lightgrey", alpha = .3, position = position_nudge(-.3)) +
  geom_pointrange(
    data = df_avg1, aes(
      x = Seq_Num, y = meanrt,
      ymin = meanrt - se,
      ymax = meanrt + se,
      color = valence, fill = valence
    ), fatten = 2.5,
    linewidth = 1,
    alpha = 1, position = position_nudge(x = 0.19), show.legend = FALSE
  ) +
  gghalves::geom_half_violin(
    data = df.agg1 %>% filter(Seq_Num == 1), aes(x = as.numeric(Seq_Num), fill = valence), side = "r", position = position_nudge(x = 0.22, y = 0),
    trim = FALSE, alpha = 0.8, color = NA
  ) +
  gghalves::geom_half_violin(
    data = df.agg1 %>% filter(Seq_Num == 3), aes(x = as.numeric(Seq_Num), fill = valence), side = "r", position = position_nudge(x = 0.22, y = 0),
    trim = FALSE, alpha = 0.8, color = NA
  ) +
  gghalves::geom_half_violin(
    data = df.agg1 %>% filter(Seq_Num == 5), aes(x = as.numeric(Seq_Num), fill = valence), side = "r", position = position_nudge(x = 0.22, y = 0),
    trim = FALSE, alpha = 0.8, color = NA
  ) +
  geom_half_boxplot(
    data = df.agg1 %>% filter(Seq_Num == 1), aes(x = as.numeric(Seq_Num), fill = valence), side = "l", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE,
    width = .3, show.legend = FALSE
  ) +
  geom_half_boxplot(
    data = df.agg1 %>% filter(Seq_Num == 3), aes(x = as.numeric(Seq_Num), fill = valence), side = "l", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE,
    width = .3, show.legend = FALSE
  ) +
  geom_half_boxplot(
    data = df.agg1 %>% filter(Seq_Num == 5), aes(x = as.numeric(Seq_Num), fill = valence), side = "l", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE,
    width = .3, show.legend = FALSE
  ) +
  facet_wrap(~matchness, labeller = as_labeller(c(`match` = "Match", `mismatch` = "Mismatch"))) +
  scale_x_continuous(
    name = "Stimulus presentation",
    breaks = c(1, 3, 5),
    labels = c("Image first", "Simultaneous", "Word first")
  ) +
  scale_y_continuous(
    name = "Reaction times (ms)"
  ) +
  guides(
    fill = guide_legend(
      keywidth = 0.6,
      keyheight = 0.6,
      default.unit = "cm",
      byrow = T
    )
  ) +
  theme_bw() +
  theme(
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
    panel.spacing.x = unit(2, "cm")
  ) + # increase the size of font
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF"), labels = c("Circle", "Square", "Triangle")) +
  scale_color_manual(values = c("#F8766D", "#00BA38", "#619CFF"), labels = c("Circle", "Square", "Triangle"))
ggsave(plot = plot_rt1, "raincloud1_rt.png", width = 15, height = 10, dpi = 300)
###########################################
###########################################
plot_rt2 <- ggplot(data = df.agg2, aes(y = rt_m)) +
  geom_point(data = df.agg2 %>% filter(Seq_Num == 1), aes(x = Con_j, color = valence), size = 1.5, alpha = .5, position = position_nudge(-.3), show.legend = FALSE) +
  geom_point(data = df.agg2 %>% filter(Seq_Num == 3), aes(x = Con_j, color = valence), size = 1.5, alpha = .5, position = position_nudge(-.3), show.legend = FALSE) +
  geom_point(data = df.agg2 %>% filter(Seq_Num == 5), aes(x = Con_j, color = valence), size = 1.5, alpha = .5, position = position_nudge(-.3), show.legend = FALSE) +
  geom_line(data = df.agg2 %>% filter(valence == "Good"), aes(x = Con_j, group = subj_idx), color = "lightgrey", alpha = .3, position = position_nudge(-.3)) +
  geom_line(data = df.agg2 %>% filter(valence == "Bad"), aes(x = Con_j, group = subj_idx), color = "lightgrey", alpha = .3, position = position_nudge(-.3)) +
  geom_line(data = df.agg2 %>% filter(valence == "Neutral"), aes(x = Con_j, group = subj_idx), color = "lightgrey", alpha = .3, position = position_nudge(-.3)) +
  geom_pointrange(
    data = df_avg2, aes(
      x = Seq_Num, y = meanrt,
      ymin = meanrt - se,
      ymax = meanrt + se,
      color = valence, fill = valence
    ), fatten = 2.5,
    linewidth = 1,
    alpha = 1, position = position_nudge(x = 0.19), show.legend = FALSE
  ) +
  gghalves::geom_half_violin(
    data = df.agg2 %>% filter(Seq_Num == 1), aes(x = as.numeric(Seq_Num), fill = valence), side = "r", position = position_nudge(x = 0.22, y = 0),
    trim = FALSE, alpha = 0.8, color = NA
  ) +
  gghalves::geom_half_violin(
    data = df.agg2 %>% filter(Seq_Num == 3), aes(x = as.numeric(Seq_Num), fill = valence), side = "r", position = position_nudge(x = 0.22, y = 0),
    trim = FALSE, alpha = 0.8, color = NA
  ) +
  gghalves::geom_half_violin(
    data = df.agg2 %>% filter(Seq_Num == 5), aes(x = as.numeric(Seq_Num), fill = valence), side = "r", position = position_nudge(x = 0.22, y = 0),
    trim = FALSE, alpha = 0.8, color = NA
  ) +
  geom_half_boxplot(
    data = df.agg2 %>% filter(Seq_Num == 1), aes(x = as.numeric(Seq_Num), fill = valence), side = "l", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE,
    width = .3, show.legend = FALSE
  ) +
  geom_half_boxplot(
    data = df.agg2 %>% filter(Seq_Num == 3), aes(x = as.numeric(Seq_Num), fill = valence), side = "l", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE,
    width = .3, show.legend = FALSE
  ) +
  geom_half_boxplot(
    data = df.agg2 %>% filter(Seq_Num == 5), aes(x = as.numeric(Seq_Num), fill = valence), side = "l", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE,
    width = .3, show.legend = FALSE
  ) +
  facet_wrap(~matchness, labeller = as_labeller(c(`match` = "Match", `mismatch` = "Mismatch"))) +
  scale_x_continuous(
    name = "Stimulus presentation",
    breaks = c(1, 3, 5),
    labels = c("Image first", "Simultaneous", "Word first")
  ) +
  scale_y_continuous(
    name = "Reaction times (ms)"
  ) +
  guides(
    fill = guide_legend(
      keywidth = 0.6,
      keyheight = 0.6,
      default.unit = "cm",
      byrow = T
    )
  ) +
  theme_bw() +
  theme(
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
    axis.ticks = element_line(size = 1),
    strip.text.x = element_text(size = 16, face = "bold"),
    panel.spacing.x = unit(2, "cm")
  ) + # increase the size of font
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF")) +
  scale_color_manual(values = c("#F8766D", "#00BA38", "#619CFF"))
ggsave(plot = plot_rt2, "raincloud2_rt.png", width = 15, height = 10, dpi = 300)

#########################################
### data preprocessing for the dprime
#########################################

df.agg1_d <- df.agg1_d %>%
  dplyr::mutate(
    Seq_Num = dplyr::case_when(
      condition == "image_first" ~ 1,
      condition == "simultaneous" ~ 3,
      condition == "word_first" ~ 5
    ),
    Con_j = jitter(Seq_Num, amount = 0.09)
  )



df.agg2_d <- df.agg2_d %>%
  dplyr::mutate(
    Seq_Num = dplyr::case_when(
      condition == "image_first" ~ 1,
      condition == "simultaneous" ~ 3,
      condition == "word_first" ~ 5
    ),
    Con_j = jitter(Seq_Num, amount = 0.09)
  )

df_avg1.d <- df.agg1_d %>%
  dplyr::group_by(condition, valence) %>%
  dplyr::summarise(
    n = n(),
    mean_d = mean(dprime),
    sd = sd(dprime),
    se = sd / sqrt(n)
  ) %>%
  dplyr::mutate(
    Seq_Num = dplyr::case_when( # change the factor to numeric for plotting
      condition == "image_first" ~ 1,
      condition == "simultaneous" ~ 3,
      condition == "word_first" ~ 5
    )
  ) %>%
  dplyr::ungroup()

df_avg2.d <- df.agg2_d %>%
  dplyr::group_by(condition, valence) %>%
  dplyr::summarise(
    n = n(),
    mean_d = mean(dprime),
    sd = sd(dprime),
    se = sd / sqrt(n)
  ) %>%
  dplyr::mutate(
    Seq_Num = dplyr::case_when( # change the factor to numeric for plotting
      condition == "image_first" ~ 1,
      condition == "simultaneous" ~ 3,
      condition == "word_first" ~ 5
    )
  ) %>%
  dplyr::ungroup()

df.agg1_d$valence <- factor(df.agg1_d$valence, levels = c("circle", "square", "triangle"))
df_avg1.d$valence <- factor(df_avg1.d$valence, levels = c("circle", "square", "triangle"))
df.agg2_d$valence <- factor(df.agg2_d$valence, levels = c("Good", "Neutral", "Bad"))
df_avg2.d$valence <- factor(df_avg2.d$valence, levels = c("Good", "Neutral", "Bad"))
d_yaxis <- expression(paste(bolditalic("d"), bold(" prime")))


##################################
plot_d1 <- ggplot(data = df.agg1_d, aes(y = dprime)) +
  geom_point(data = df.agg1_d %>% filter(Seq_Num == 1), aes(x = Con_j, color = valence), size = 1.5, alpha = .5, position = position_nudge(-.3), show.legend = FALSE) +
  geom_point(data = df.agg1_d %>% filter(Seq_Num == 3), aes(x = Con_j, color = valence), size = 1.5, alpha = .5, position = position_nudge(-.3), show.legend = FALSE) +
  geom_point(data = df.agg1_d %>% filter(Seq_Num == 5), aes(x = Con_j, color = valence), size = 1.5, alpha = .5, position = position_nudge(-.3), show.legend = FALSE) +
  geom_line(data = df.agg1_d %>% filter(valence == "circle"), aes(x = Con_j, group = subj_idx), color = "lightgrey", alpha = .3, position = position_nudge(-.3)) +
  geom_line(data = df.agg1_d %>% filter(valence == "square"), aes(x = Con_j, group = subj_idx), color = "lightgrey", alpha = .3, position = position_nudge(-.3)) +
  geom_line(data = df.agg1_d %>% filter(valence == "triangle"), aes(x = Con_j, group = subj_idx), color = "lightgrey", alpha = .3, position = position_nudge(-.3)) +
  geom_pointrange(
    data = df_avg1.d, aes(
      x = Seq_Num, y = mean_d,
      ymin = mean_d - se,
      ymax = mean_d + se,
      color = valence, fill = valence
    ), fatten = 2.5,
    linewidth = 1,
    alpha = 1, position = position_nudge(x = 0.19), show.legend = FALSE
  ) +
  gghalves::geom_half_violin(
    data = df.agg1_d %>% filter(Seq_Num == 1), aes(x = as.numeric(Seq_Num), fill = valence), side = "r", position = position_nudge(x = 0.22, y = 0),
    trim = FALSE, alpha = 0.8, color = NA
  ) +
  gghalves::geom_half_violin(
    data = df.agg1_d %>% filter(Seq_Num == 3), aes(x = as.numeric(Seq_Num), fill = valence), side = "r", position = position_nudge(x = 0.22, y = 0),
    trim = FALSE, alpha = 0.8, color = NA
  ) +
  gghalves::geom_half_violin(
    data = df.agg1_d %>% filter(Seq_Num == 5), aes(x = as.numeric(Seq_Num), fill = valence), side = "r", position = position_nudge(x = 0.22, y = 0),
    trim = FALSE, alpha = 0.8, color = NA
  ) +
  geom_half_boxplot(
    data = df.agg1_d %>% filter(Seq_Num == 1), aes(x = as.numeric(Seq_Num), fill = valence), side = "l", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE,
    width = .3, show.legend = FALSE
  ) +
  geom_half_boxplot(
    data = df.agg1_d %>% filter(Seq_Num == 3), aes(x = as.numeric(Seq_Num), fill = valence), side = "l", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE,
    width = .3, show.legend = FALSE
  ) +
  geom_half_boxplot(
    data = df.agg1_d %>% filter(Seq_Num == 5), aes(x = as.numeric(Seq_Num), fill = valence), side = "l", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE,
    width = .3, show.legend = FALSE
  ) +
  scale_x_continuous(
    name = "Stimulus presentation",
    breaks = c(1, 3, 5),
    labels = c("Image first", "Simultaneous", "Word first")
  ) +
  scale_y_continuous(
    name = d_yaxis
  ) +
  guides(
    fill = guide_legend(
      keywidth = 0.6,
      keyheight = 0.6,
      default.unit = "cm",
      byrow = T
    )
  ) +
  theme_bw() +
  theme(
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
    panel.spacing.x = unit(2, "cm")
  ) + # increase the size of font
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF"), labels = c("Circle", "Square", "Triangle")) +
  scale_color_manual(values = c("#F8766D", "#00BA38", "#619CFF"), labels = c("Circle", "Square", "Triangle"))
ggsave(plot = plot_d1, "raincloud1_d.png", width = 15, height = 10, dpi = 300)





plot_d2 <- ggplot(data = df.agg2_d, aes(y = dprime)) +
  geom_point(data = df.agg2_d %>% filter(Seq_Num == 1), aes(x = Con_j, color = valence), size = 1.5, alpha = .5, position = position_nudge(-.3), show.legend = FALSE) +
  geom_point(data = df.agg2_d %>% filter(Seq_Num == 3), aes(x = Con_j, color = valence), size = 1.5, alpha = .5, position = position_nudge(-.3), show.legend = FALSE) +
  geom_point(data = df.agg2_d %>% filter(Seq_Num == 5), aes(x = Con_j, color = valence), size = 1.5, alpha = .5, position = position_nudge(-.3), show.legend = FALSE) +
  geom_line(data = df.agg2_d %>% filter(valence == "Good"), aes(x = Con_j, group = subj_idx), color = "lightgrey", alpha = .3, position = position_nudge(-.3)) +
  geom_line(data = df.agg2_d %>% filter(valence == "Neutral"), aes(x = Con_j, group = subj_idx), color = "lightgrey", alpha = .3, position = position_nudge(-.3)) +
  geom_line(data = df.agg2_d %>% filter(valence == "Bad"), aes(x = Con_j, group = subj_idx), color = "lightgrey", alpha = .3, position = position_nudge(-.3)) +
  geom_pointrange(
    data = df_avg2.d, aes(
      x = Seq_Num, y = mean_d,
      ymin = mean_d - se,
      ymax = mean_d + se,
      color = valence, fill = valence
    ), fatten = 2.5,
    linewidth = 1,
    alpha = 1, position = position_nudge(x = 0.19), show.legend = FALSE
  ) +
  gghalves::geom_half_violin(
    data = df.agg2_d %>% filter(Seq_Num == 1), aes(x = as.numeric(Seq_Num), fill = valence), side = "r", position = position_nudge(x = 0.22, y = 0),
    trim = FALSE, alpha = 0.8, color = NA
  ) +
  gghalves::geom_half_violin(
    data = df.agg2_d %>% filter(Seq_Num == 3), aes(x = as.numeric(Seq_Num), fill = valence), side = "r", position = position_nudge(x = 0.22, y = 0),
    trim = FALSE, alpha = 0.8, color = NA
  ) +
  gghalves::geom_half_violin(
    data = df.agg2_d %>% filter(Seq_Num == 5), aes(x = as.numeric(Seq_Num), fill = valence), side = "r", position = position_nudge(x = 0.22, y = 0),
    trim = FALSE, alpha = 0.8, color = NA
  ) +
  geom_half_boxplot(
    data = df.agg2_d %>% filter(Seq_Num == 1), aes(x = as.numeric(Seq_Num), fill = valence), side = "l", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE,
    width = .3, show.legend = FALSE
  ) +
  geom_half_boxplot(
    data = df.agg2_d %>% filter(Seq_Num == 3), aes(x = as.numeric(Seq_Num), fill = valence), side = "l", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE,
    width = .3, show.legend = FALSE
  ) +
  geom_half_boxplot(
    data = df.agg2_d %>% filter(Seq_Num == 5), aes(x = as.numeric(Seq_Num), fill = valence), side = "l", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE,
    width = .3, show.legend = FALSE
  ) +
  scale_x_continuous(
    name = "Stimulus presentation",
    breaks = c(1, 3, 5),
    labels = c("Image first", "Simultaneous", "Word first")
  ) +
  scale_y_continuous(
    name = d_yaxis
  ) +
  guides(
    fill = guide_legend(
      keywidth = 0.6,
      keyheight = 0.6,
      default.unit = "cm",
      byrow = T
    )
  ) +
  theme_bw() +
  theme(
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
    panel.spacing.x = unit(2, "cm")
  ) + # increase the size of font
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF"), labels = c("Good", "Neutral", "Bad")) +
  scale_color_manual(values = c("#F8766D", "#00BA38", "#619CFF"), labels = c("Good", "Neutral", "Bad"))

############### save the plots as pdf
p1 <- plot_rt1 / plot_d1 + plot_layout(guides = 'collect')
p2 <- plot_rt2 / plot_d2 + plot_layout(guides = 'collect')
ggsave(plot = p1, "Exp1.pdf", width = 15, height = 10)
ggsave(plot = p2, "Exp2.pdf", width = 15, height = 10)
