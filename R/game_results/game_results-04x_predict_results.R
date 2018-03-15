


rm(list = ls())
setwd("O:/_other/projects/nfl/")

# Parameters. ----
filename <- "pfref_game_results"
dir_import <- "data/nfl/created/"
library("stringr")
filepath_import <- str_c(dir_import, filename, "_prepared.csv")
d <- read.csv(filepath_import, stringsAsFactors = FALSE)

# Do. ----

require("dplyr")
check_na_or_tie <- function(x, y) {
  ifelse((is.na(x) | is.na(y) | (x == y)), TRUE, FALSE)
}


calculate_pick <-
  function(x_1, x_2, x_match, value, value_threshhold) {
    ifelse(
      (
        (x_1 == x_match & value <= value_threshhold) |
          (x_1 != x_match & value > value_threshhold)
      ),
      x_1,
      x_2
    )
  }

# _1 is based on home field and the pts_ value.
# _2 is based on the winning percentage prior to the game.
# _3 is based on end of year winning percentage.
pts_spread <- 3
pts_straight <- 0
picks <-
  d %>%
  mutate(
    spread_1 =
      ifelse(
        check_na_or_tie(wp_lastn_1, wp_lastn_2),
        ifelse(line_home <= -pts_spread, tm_home, tm_away),
        calculate_pick(tm_1, tm_2, tm_home, line_home, -pts_spread)
      )
  ) %>%
  mutate(
    straight_1 =
      ifelse(
        check_na_or_tie(wp_lastn_1, wp_lastn_2),
        ifelse(line_home <= -pts_straight, tm_home, tm_away),
        calculate_pick(tm_1, tm_2, tm_home, line_home, -pts_straight)
      )
  ) %>%
  mutate(
    spread_2 =
      ifelse(
        check_na_or_tie(wp_lastn_1, wp_lastn_2),
        ifelse(line_home <= -pts_spread, tm_home, tm_away),
        ifelse(wp_lastn_1 > wp_lastn_2, tm_1, tm_2)
      )
  ) %>%
  mutate(
    straight_2 =
      ifelse(
        check_na_or_tie(wp_lastn_1, wp_lastn_2),
        ifelse(line_home <= -pts_straight, tm_home, tm_away),
        ifelse(wp_lastn_1 > wp_lastn_2, tm_1, tm_2)
      )
  ) %>%
  mutate(
    spread_3 =
      ifelse(
        (wp_eoy_1 == wp_eoy_2),
        ifelse(tm_1 == tm_home, tm_1, tm_2),
        ifelse(tm_1 == tm_home,
               ifelse(wp_eoy_1 >= wp_eoy_2, tm_1,
                      ifelse(line_home > -pts_spread, tm_2, tm_1)
               ),
               ifelse(wp_eoy_1 < wp_eoy_2,
                      ifelse(line_home < -pts_spread, tm_1, tm_2),
                      tm_1
               )
        )
      )
  ) %>%
  mutate(
    straight_3 =
      ifelse(
        (wp_eoy_1 == wp_eoy_2),
        ifelse(tm_1 == tm_home, tm_1, tm_2),
        ifelse(wp_eoy_1 > wp_eoy_2, tm_1, tm_2)
      )
  )

pick_results <-
  picks %>%
  mutate(
    spread_1 =
      ifelse(spread_1 == tm_winner_spread, 1, 0),
    straight_1 =
      ifelse(straight_1 == tm_winner_straight, 1, 0),
    spread_2 =
      ifelse(spread_2 == tm_winner_spread, 1, 0),
    straight_2 =
      ifelse(straight_2 == tm_winner_straight, 1, 0),
    spread_3 =
      ifelse(spread_3 == tm_winner_spread, 1, 0),
    straight_3 =
      ifelse(straight_3 == tm_winner_straight, 1, 0)
  )

# write.csv(pick_results, "data/debug.csv", row.names = FALSE)

# This function is always used in the analysis of bet metrics.
calculate_pct <- function(x, value = 1, remove_na = TRUE, digits_round = 1) {
  round(100 * sum(x == value, na.rm = remove_na) / sum(!is.na(x)), digits_round)
}

pick_results_summary <-
  pick_results %>%
  group_by(season) %>%
  summarise(
    spread_1_pct = calculate_pct(spread_1),
    straight_1_pct = calculate_pct(straight_1),
    spread_2_pct = calculate_pct(spread_2),
    straight_2_pct = calculate_pct(straight_2),
    spread_3_pct = calculate_pct(spread_3),
    straight_3_pct = calculate_pct(straight_3)
  )
pick_results_summary

require("stringr")
cols_gather_idx <-
  names(pick_results_summary) %>%
  str_which("pct$")
cols_gather_idx

require("tidyr")
require("Hmisc")
pick_results_summary_tidy <-
  pick_results_summary %>%
  gather(key, value, cols_gather_idx, factor_key = TRUE) %>%
  mutate(key_2 = key) %>%
  separate(key_2, Cs(type, i, calc), sep = "_")

# Using this to maintain ggplot colors with scale_color_manual().
require("scales")
ggplot_cp <- hue_pal()(3)

# require("ggpmisc")
require("ggplot2")
theme_set(theme_minimal())
pick_results_summary_tidy %>%
  group_by(key) %>%
  mutate(avg = mean(value)) %>%
  ggplot(aes(x = season, y = value, group = i, color = i)) +
  geom_line() +
  geom_smooth(method = lm, se = FALSE, linetype = "dashed") +
  # stat_poly_eq(formula = y ~ x,
  #              aes(label = paste(..eq.label..)),
  #              parse = TRUE)
  geom_line(aes(x = season, y = avg, group = i, color = i), size = 2) +
  scale_color_manual(
    values = ggplot_cp,
    labels = c("Home Field Advantage", "Win % To Date", "EOY Win %")
  ) +
  facet_grid( ~ type) +
  labs(color = NULL)

