
rm(list = ls())
setwd("O:/_other/projects/nfl/")

# Parameters. ----
filename_base <- "pfref_game_results_tm_to"
dir_import <- "data/created/"

library("stringr")
filepath_import <-
	str_c(dir_import, filename_base, "_cleaned.csv")
d <-
	read.csv(filepath_import, stringsAsFactors = FALSE) %>% as_tibble()
export <- FALSE

# Analyze. ----

library("ggplot2")
library("dplyr")
theme_set(theme_minimal())


d %>%
  # filter(result != "T") %>%
  ggplot(aes(x = to)) +
  geom_histogram() +
  labs(title = "Distribution of # of Turnovers Per Team Per Game",
       y = "",
       x = "")


d %>%
  filter(result != "T") %>%
  group_by(result) %>%
  ggplot(aes(x = to)) +
  geom_histogram(aes(fill = result)) +
  facet_wrap(~ result, nrow = 2) +
  labs(
    title = "Distribution of # of Turnovers Per Team Per Game",
    subtitle = "By Result",
    caption = "Note that the distribution is skewed higher for more turnovers.",
    y = "",
    x = ""
  )

library("scales")
d %>%
  ggplot() +
  geom_histogram(
    data = filter(d, result == "W"),
    aes(x = to, y = ..density..),
    alpha = 0.5,
    fill = hue_pal()(1)[[1]]
  ) +
  geom_line(
    data = filter(d, result == "W"),
    aes(x = to, y = ..density..),
    alpha = 0.5,
    fill = hue_pal()(1)[[1]],
    stat = "density",
    size = 1
  ) +
  geom_histogram(
    data = filter(d, result == "L"),
    aes(x = to, y = ..density..),
    alpha = 0.5,
    fill = hue_pal()(2)[[2]]
  ) +
  geom_line(
    data = filter(d, result == "L"),
    aes(x = to, y = ..density..),
    alpha = 0.5,
    fill = hue_pal()(2)[[2]],
    stat = "density",
    size = 1
  ) +
  labs(
    title = "Distribution of # of Turnovers Per Team Per Game",
    subtitle = "By Result",
    caption = "Note that the distribution is skewed higher for more turnovers.",
    y = "",
    x = ""
  )

library("ggpmisc")
d %>%
  ggplot(aes(x = to, y = pts)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..eq.label.., sep = "*plain(\",\")~")),
    parse = TRUE
  ) +
  stat_fit_glance(
    method = "lm",
    method.args = list(formula = y ~ x),
    geom = "text",
    aes(
      # label = paste(aes(signif(..p.value.., digits = 4), sep = ""))
      label = ifelse(
        ..p.value.. < 0.001,
        "p<0.001**",
        ifelse(..p.value.. >= 0.001 &
                 ..p.value.. < 0.05, "p<0.05*", "p>0.05")
      )
    ),
    label.x.npc = "right", 
    label.y.npc = 0.35,
    size = 4
  ) +
  labs(
    title = "Linear Regression Model for Points Scored As a Function of Turnovers",
    subtitle = "",
    caption = "",
    y = "",
    x = ""
  )
summary(lm(pts ~ to, data = d))

