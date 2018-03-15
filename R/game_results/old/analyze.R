
file_name <- "pfref_game_results"
file_path_import <- paste0("data/", file_name, "_processed.csv")
processed <- read.csv(file_path_import, stringsAsFactors = FALSE)

# Visualizing how lines/totals how changed over time. --------------------------

require("ggplot2")
require("dplyr")
processed %>%
  group_by(season) %>%
  summarise(line_home_avg = round(mean(line_home), 2)) %>%
  ggplot(aes(x = season, y = line_home_avg)) +
  geom_line()

processed %>%
  filter(!is.na(total)) %>%
  group_by(season) %>%
  summarise(total_avg = round(mean(total), 2)) %>%
  ggplot(aes(x = season, y = total_avg)) +
  geom_line()

# Evalutating favorites/underdogs over time. -----------------------------------

processed %>%
  mutate(score_diff = score_home - score_away) %>%
  ggplot() +
  geom_histogram(aes(x = line_home), fill = "red") +
  # geom_density(aes(x = line_home), color = "red") +
  geom_histogram(aes(x = score_diff), fill = "blue")

bet_q_season <-
  d_processed %>%
  group_by(season) %>%
  summarise(
    q_25 = quantile(line_home, 0.25, na.rm = TRUE),
    q_50 = quantile(line_home, 0.5, na.rm = TRUE),
    q_75 = quantile(line_home, 0.75, na.rm = TRUE)
  )
bet_q_season

bet_q_overall <-
  d_processed %>%
  summarise(
    q_25 = quantile(line_home, 0.25, na.rm = TRUE),
    q_50 = quantile(line_home, 0.5, na.rm = TRUE),
    q_75 = quantile(line_home, 0.75, na.rm = TRUE),
    q_10 = quantile(line_home, 0.1, na.rm = TRUE),
    q_90 = quantile(line_home, 0.9, na.rm = TRUE)
  )
bet_q_overall

bet_metrics <-
  d_processed %>%
  mutate(
    team_winner_diff =
      ifelse(
        team_winner_straight != team_winner_spread, 1, 0),
    auw =
      ifelse(
        line_home < 0,
        ifelse(team_winner_spread == team_away,
               1, 0),
        as.numeric(NULL)),
    huw =
      ifelse(
        line_home > 0,
        ifelse(team_winner_spread == team_home,
               1, 0),
        as.numeric(NULL)),
    afw =
      ifelse(
        line_home > 0,
        ifelse(team_winner_spread == team_away,
               1, 0),
        as.numeric(NULL)),
    hfw =
      ifelse(
        line_home < 0,
        ifelse(team_winner_spread == team_home,
               1, 0),
        as.numeric(NULL)),
    no_fav = ifelse(line_home == 0, 1, 0),
    push = ifelse((score_home + line_home) == score_away, 1, 0),

  )

calculate_pct <- function(x, value = 1, remove_na = TRUE, digits_round = 1) {
  round(100 * sum(x == value, na.rm = remove_na) / sum(!is.na(x)), digits_round)
}

bet_metrics_summary <-
  d_bet_metrics %>%
  group_by(season) %>%
  summarise(
    g = n(),
    spread_straight_diff_count = sum(team_winner_diff),
    spread_straight_diff_pct = calculate_pct(team_winner_diff),
    auw_count = sum(auw == 1, na.rm = TRUE),
    huw_count = sum(huw == 1, na.rm = TRUE),
    afw_count = sum(afw == 1, na.rm = TRUE),
    hfw_count = sum(hfw == 1, na.rm = TRUE),
    no_fav_count = sum(no_fav),
    push_count = sum(push),
    auw_pct = calculate_pct(auw),
    huw_pct = calculate_pct(huw),
    afw_pct = calculate_pct(afw),
    hfw_pct = calculate_pct(hfw),
    no_fav_pct = calculate_pct(no_fav),
    push_pct = calculate_pct(push)
  )

bet_metrics_check <-
  d_bet_metrics_summary %>%
  mutate(
    counts_sum = auw_count + huw_count + afw_count + hfw_count
  ) %>%
  mutate(
    counts_diff = g - counts_sum,
    counts_diff_2 = g - counts_sum - no_fav_count - push_count
  ) %>%
  select(season, g, counts_sum, counts_diff, no_fav_count, push_count, counts_diff_2)

require("Hmisc")
rm(list = Cs(d_bet_metrics_check))


# Remove some of the values because they portray the same information.
require("stringr")
cols_hxw_pct <-
  names(d_bet_metrics_summary) %>%
  str_subset("w_pct$") %>%
  str_subset("^h")

cols_gather_idx <-
  names(d_bet_metrics_summary) %>%
  str_which("count$|pct$")
cols_gather_idx

require("tidyr")
bet_metrics_summary %>%
  gather(key, value, cols_gather_idx, factor_key = TRUE) %>%
  filter(key %in% cols_hxw_pct) %>%
  ggplot(aes(x = season)) +
  geom_line(aes(y = value, group = key, color = key)) +
  geom_smooth(aes(y = value, group = key, color = key), method = lm, se = TRUE, size = 2) +
  theme(legend.title = element_blank())

# Check bet metrics in "binned" ranges. ----------------------------------------
require("magrittr")
bet_metrics %<>%
  mutate(
    line_50 = ifelse(line_home <= d_bet_q_overall$q_50, 1, 0),
    line_25 = ifelse(line_home <= d_bet_q_overall$q_25, 1, 0),
    line_75 = ifelse(line_home >= d_bet_q_overall$q_75, 1, 0),
    line_10 = ifelse(line_home <= d_bet_q_overall$q_10, 1, 0),
    line_90 = ifelse(line_home >= d_bet_q_overall$q_90, 1, 0)
  )


calculate_pct_2 <-
  function(x_1 = NULL,
           x_2 = NULL,
           value_1 = 1,
           value_2 = 1,
           remove_na = TRUE,
           digits_round = 1) {
    if (is.null(x_2)) {
      calc <- round(sum(x_1 == value_1, na.rm = remove_na) / sum(!is.na(x_1)),
            digits_round)
    } else {
      calc <- round(100 * sum((x_1 == value_1 &
                        x_2 == value_2), na.rm = remove_na) / sum(!(is.na(x_1) &
                                                                     !is.na(x_2))),
            digits_round)
    }
    calc
  }

bet_metrics_summary_2 <-
  d_bet_metrics %>%
  group_by(season) %>%
  summarise(
    g = n(),
    g_a50_count = sum(line_50 == 0),
    g_b50_count = sum(line_50 == 1),
    hfw_a50_count = sum(hfw == 1 & line_50 == 0, na.rm = TRUE),
    hfw_b50_count = sum(hfw == 1 & line_50 == 1, na.rm = TRUE),
    hfw_25_count = sum(hfw == 1 & line_25 == 1, na.rm = TRUE),
    hfw_b25_count = sum(hfw == 0 & line_25 == 1, na.rm = TRUE),
    huw_75_count = sum(huw == 1 & line_75 == 1, na.rm = TRUE),
    hfw_b75_count = sum(huw == 0 & line_75 == 1, na.rm = TRUE),
    hfw_25_pct = calculate_pct(hfw, line_25),
    hfw_b25_pct = calculate_pct(hfw, line_25, 0, 1),
    huw_75_pct = calculate_pct(huw, line_75),
    huw_b75_pct = calculate_pct(huw, line_75, 0, 1),
  )
bet_metrics_summary_2
glimpse(d_bet_metrics_summary_2)
cols_gather_idx_2 <-
  names(d_bet_metrics_summary_2) %>%
  str_which("5_pct$")
cols_gather_idx_2

bet_metrics_summary_2 %>%
  gather(key, value, cols_gather_idx_2, factor_key = TRUE) %>%
  ggplot(aes(x = season)) +
  geom_line(aes(y = value, group = key, color = key)) +
  geom_smooth(aes(y = value, group = key, color = key), method = lm, se = TRUE, size = 2) +
  theme(legend.title = element_blank())


# Predicting by record to date. ------------------------------------------------

rbind_prep_1 <-
  d_processed %>%
  mutate(
    line_1 = line_home,
    line_2 = -line_home,
    team_1 = team_home,
    team_2 = team_away,
    score_1 = score_home,
    score_2 = score_away,
    pd_1 = score_1 - score_2,
    pd_2 = score_2 - score_1
  )

# Need to figure out why I can't reverse pd_1 and pd_2 too.
rbind_prep_2 <-
  d_processed %>%
  mutate(
    line_1 = -line_home,
    line_2 = line_home,
    team_1 = team_away,
    team_2 = team_home,
    score_1 = score_away,
    score_2 = score_home,
    # pd_1 = score_2 - score_1,
    # pd_2 = score_1 - score_2
    pd_1 = score_1 - score_2,
    pd_2 = score_2 - score_1
  )

rbind_prep <-
  d_rbind_prep_1 %>%
  bind_rows(d_rbind_prep_2) %>%
  arrange(season, week, team_home, team_away)

rm(list = Cs(d_rbind_prep_1, d_rbind_prep_2))

determine_winner <- function(x_1, x_2, x_match) {
  ifelse(x_1 == x_match, 1,
         ifelse(x_2 == x_match, 0, as.integer(NA)))
}

rbind_prep_calc <-
  d_rbind_prep %>%
  mutate(
    g = 1,
    w = determine_winner(team_1, team_2, team_winner_straight),
    wats = determine_winner(team_1, team_2, team_winner_spread)
  ) %>%
  group_by(season, team_1) %>%
  mutate(
    gtd = cumsum(g),
    wtd = cumsum(!is.na(w) & w == 1),
    ltd = cumsum(!is.na(w) & w == 0),
    ttd = cumsum(is.na(w)),
    pftd = cummean(score_1),
    patd = cummean(score_2),
  ) %>%
  # mutate(
  #   pdtd_1 = cummean(pd_1),
  #   pdtd_2 = cummean(pd_2)
  # ) %>%
  mutate(
    wptd = wtd / gtd,
    pdtd = (pftd - patd)
  ) %>%
  ungroup()

eoy_records <-
  d_rbind_prep_calc %>%
  group_by(season, team_1) %>%
  summarise(
    # g_eoy = max(gtd),
    # w_eoy = max(wtd),
    # l_eoy = max(ltd),
    # t_eoy = max(ttd),
    wp_eoy = round(100 * max(wtd) / max(gtd), 1)
  ) %>%
  ungroup()
eoy_records


require("stringr")
cols_lag1 <-
  names(d_rbind_prep_calc) %>%
  str_subset("td$")
cols_lag1

rbind_prep_calc %<>%
  group_by(season, team_1) %>%
  mutate_at(vars(cols_lag1), funs(lag1 = lag)) %>%
  ungroup()

require("zoo")
# The partial argument is really only necessary for provided a value for the very
# first n values in the entire data set (otherwise NA would be returned).
# Can't use pdtd for pd_lastn.
calculate_wavg <- function(x, w = seq(n), n = length(x), remove_na = TRUE) {
  weighted.mean(x, w, na.rm = remove_na)
}

rbind_prep_calc_2 <-
  d_rbind_prep_calc %>%
  arrange(season, week, team_1) %>%
  group_by(team_1) %>%
  mutate(
    # w_lastn = RcppRoll::roll_sum(w, n = 10, align = "right", fill = NA),
    w_lastn = rollapplyr(w, width = 10, FUN = sum, na.rm = TRUE, partial = TRUE),
    wp_lastn = rollapplyr(w, width = 10, FUN = mean, na.rm = TRUE, partial = TRUE),
    wp_wlastn = rollapplyr(w, width = 10, FUN = calculate_wavg, by.column = FALSE, partial = TRUE),
    # pd_1_lastn = rollapplyr(pd_1, width = 10, FUN = mean, na.rm = TRUE, partial = TRUE),
    # pd_2_lastn = rollapplyr(pd_2, width = 10, FUN = mean, na.rm = TRUE, partial = TRUE),
    pd_lastn = rollapplyr(score_1 - score_2, width = 10, FUN = mean, na.rm = TRUE, partial = TRUE),
    pd_wlastn = rollapplyr(score_1 - score_2, width = 10, FUN = calculate_wavg, partial = TRUE)
  ) %>%
  ungroup()

rbind_prep_calc_2 %>%
  filter(season == 2016) %>%
  filter(team_1 == "DAL") %>%
  View()

cols_lastn_lag1 <-
  names(d_rbind_prep_calc_2) %>%
  str_subset("lastn$")
cols_lastn_lag1

rbind_prep_calc_2 %<>%
  group_by(season, team_1) %>%
  mutate_at(vars(cols_lastn_lag1), funs(lag1 = lag)) %>%
  ungroup()

cols_int <-
  names(d_rbind_prep_calc_2) %>%
  str_subset("^.td$|^.td_lag1$|^._lastn$|^._lastn_lag1$")
cols_int

rbind_prep_calc_2 %<>%
  mutate_at(vars(cols_int), funs(as.integer))

cols_remove <-
  names(d_rbind_prep_calc_2) %>%
  str_subset("td$|^.td_lag1$")
cols_remove

rbind_prep_calc_3 <-
  d_rbind_prep_calc_2 %>%
  select(-one_of(cols_remove))

rbind_prep_calc_3 %<>% filter(season >= 2000)

# rm(list = Cs(d_rbind_prep_calc))

cols_join <-
  names(d_rbind_prep_calc_3) %>%
  str_subset("season|week|team|score|^pd|line|lag1$|lastn$")
cols_join

rbind_join <-
  d_rbind_prep_calc_3 %>%
  select(one_of(cols_join)) %>%
  inner_join(
    select(d_rbind_prep_calc_3, one_of(cols_join)),
    by = c(
      "season",
      "week",
      "team_home",
      "team_away",
      "score_home",
      "score_away",
      "line_home",
      "team_winner_spread",
      "team_winner_straight",
      "line_1" = "line_2",
      "line_2" = "line_1",
      "team_1" = "team_2",
      "team_2" = "team_1",
      "score_1" = "score_2",
      "score_2" = "score_1",
      "pd_1" = "pd_2",
      "pd_2" = "pd_1"
    )
  )

rbind_join_2 <-
  d_rbind_join %>%
  inner_join(d_eoy_records, by = c("season", "team_1")) %>%
  inner_join(d_eoy_records, by = c("season", "team_2" = "team_1"))

rm(d_rbind_join)

# str_which(names(d_rbind_join_2), "\\.x$")
names(d_rbind_join_2) %<>%
  str_replace_all("\\.x$", "_1") %>%
  str_replace_all("\\.y$", "_2")

# write.csv(d_rbind_join, "data/debug.csv", row.names = FALSE)

check_na_or_tie <- function(x, y) {
  ifelse((is.na(x) | is.na(y) | (x == y)), TRUE, FALSE)
}

calculate_threshhold <-
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

# _1 is based on home field and the points_break_ value.
# _2 is based on the winning percentage prior to the game.
# _3 is based on end of year winning percentage.
points_break_spread <- 3
points_break_straight <- 0
picks <-
  d_rbind_join_2 %>%
  mutate(
    spread_1 =
      ifelse(
        check_na_or_tie(wptd_lag1_1, wptd_lag1_2),
        ifelse(line_home <= -points_break_spread, team_home, team_away),
        calculate_threshhold(team_1, team_2, team_home, line_home, -points_break_spread)
      )
  ) %>%
  mutate(
    straight_1 =
    ifelse(
      check_na_or_tie(wptd_lag1_1, wptd_lag1_2),
      ifelse(line_home <= -points_break_straight, team_home, team_away),
      calculate_threshhold(team_1, team_2, team_home, line_home, -points_break_straight)
    )
  ) %>%
  mutate(
    spread_2 =
      ifelse(
        check_na_or_tie(wptd_lag1_1, wptd_lag1_2),
        ifelse(line_home <= -points_break_spread, team_home, team_away),
        ifelse(wptd_lag1_1 > wptd_lag1_2, team_1, team_2)
      )
  ) %>%
  mutate(
    straight_2 =
      ifelse(
        check_na_or_tie(wptd_lag1_1, wptd_lag1_2),
        ifelse(line_home <= -points_break_straight, team_home, team_away),
        ifelse(wptd_lag1_1 > wptd_lag1_2, team_1, team_2)
      )
  ) %>%
  mutate(
    spread_3 =
      ifelse(
        (wp_eoy_1 == wp_eoy_2),
        ifelse(team_1 == team_home, team_1, team_2),
        ifelse(team_1 == team_home,
               ifelse(wp_eoy_1 >= wp_eoy_2, team_1,
                      ifelse(line_home > -points_break_spread, team_2, team_1)
               ),
               ifelse(wp_eoy_1 < wp_eoy_2,
                      ifelse(line_home < -points_break_spread, team_1, team_2),
                      team_1
               )
        )
      )
  ) %>%
  mutate(
    straight_3 =
      ifelse(
        (wp_eoy_1 == wp_eoy_2),
        ifelse(team_1 == team_home, team_1, team_2),
        ifelse(wp_eoy_1 > wp_eoy_2, team_1, team_2)
      )
  )

pick_results <-
  d_picks %>%
  mutate(
    spread_1 =
      ifelse(spread_1 == team_winner_spread, 1, 0),
    straight_1 =
      ifelse(straight_1 == team_winner_straight, 1, 0),
    spread_2 =
      ifelse(spread_2 == team_winner_spread, 1, 0),
    straight_2 =
      ifelse(straight_2 == team_winner_straight, 1, 0),
    spread_3 =
      ifelse(spread_3 == team_winner_spread, 1, 0),
    straight_3 =
      ifelse(straight_3 == team_winner_straight, 1, 0)
  )

# write.csv(d_pick_results, "data/debug.csv", row.names = FALSE)

pick_results_summary <-
  d_pick_results %>%
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

cols_tidy_idx <-
  names(d_pick_results_summary) %>%
  str_which("pct$")
cols_tidy_idx

pick_results_summary_tidy <-
  d_pick_results_summary %>%
  gather(key, value, cols_tidy_idx, factor_key = TRUE) %>%
  mutate(key_2 = key) %>%
  separate(key_2, Cs(type, i, calc), sep = "_")

# Using this to maintain ggplot colors with scale_color_manual().
require("scales")
ggplot_cp <- hue_pal()(3)

require("ggpmisc")
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

# More ----

round_to <- function(x, n = 1) {
  if(n >= 1) {
    round(x * n) / n
  } else {
    round(x / n) * n
  }
}

cols_join <- Cs(wp_wlastn_lag1_1, wp_wlastn_lag1_2)
cols_join

cols_join_alt <- Cs(pd_wlastn_lag1_1, pd_wlastn_lag1_2)

cols_round <- c(cols_join, cols_join_alt)
cols_round_not <- Cs(home_1, pd_1, line_1)

grid_prep_calc <-
  d_rbind_join_2 %>%
  # mutate(debug = paste(season, week, team_1, team_2, sep = "_")) %>%
  mutate(home_1 = ifelse(team_1 == team_home, 1, 0)) %>%
  select(one_of(cols_round, cols_round_not))

resolution <- 0.05
grid_prep_calc %<>%
  mutate_at(vars(cols_round), funs(round_to), resolution)

grid_prep_nums <-
  expand.grid(seq(from = 0, to = 1, by = resolution),
              seq(from = 0, to = 1, by = resolution))

names(d_grid_prep_nums) <- cols_join

grid <-
  d_grid_prep_nums %>%
  left_join(d_grid_prep_calc, by = cols_join)

cols_group <- cols_join

grid_summary_1 <-
  d_grid %>%
  group_by_at(cols_group) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1)

cols_avg <- names(d_grid_prep_calc) %>% str_subset("^[^w].")
cols_avg

grid_summary_2 <-
  d_grid %>%
  group_by_at(cols_group) %>%
  summarise_at(vars(cols_avg), funs(avg = mean(.)), na.rm = TRUE) %>%
  # filter(!is.na(line_home_avg)) %>%
  ungroup()

grid_summary <-
  d_grid_summary_1 %>%
  inner_join(d_grid_summary_2, by = cols_group)

rm(list = Cs(d_grid_summary_1, d_grid_summary_2))

grid_summary_calc <-
  d_grid_summary %>%
  mutate(join_diff = wp_wlastn_lag1_1 - wp_wlastn_lag1_2) %>%
  # arrange(join_diff)
  arrange(line_1_avg, join_diff)

grid_summary_calc %<>% distinct(n, line_1_avg, .keep_all = TRUE)

grid_summary_calc
y_1 <- "line_1_avg"
fmla_1a <-
  paste(y_1, paste(cols_join, collapse = " + "), sep = " ~ ")
fmla_1b <-
  paste(y_1, paste(paste0(cols_join_alt, "_avg"), collapse = " + "), sep = " ~ ")
fmla_1c <-
  paste(y_1, paste(
    paste(cols_join, collapse = " + "),
    paste(paste0(cols_join_alt, "_avg"), collapse = " + "),
    sep = " + "
  ), sep = " ~ ")
fmla_1d <- paste(y_1, "join_diff", sep = " ~ ")
fmla_1a_2 <- paste0(fmla_1a, " + home_1_avg")

lm_1a <- lm(fmla_1a, data = d_grid_summary_calc)
lm_1a_2 <- lm(fmla_1a_2, data = d_grid_summary_calc)
lm_1b <- lm(fmla_1b, data = d_grid_summary_calc)
lm_1c <- lm(fmla_1c, data = d_grid_summary_calc)
lm_1d <- lm(fmla_1d, data = d_grid_summary_calc)
summary(lm_1a)
mean(abs(lm_1a$residuals))
summary(lm_1a_2)
glm_1a <- glm(fmla_1a, data = d_grid_summary_calc)
glm_1a
mean(abs(glm_1a$residuals))
str(glm_1a)
lm_1b
mean(abs(lm_1b$residuals))
summary(lm_1c)
mean(abs(lm_1c$residuals))
summary(lm_1d)
mean(abs(lm_1d$residuals))

ggplot(data = d_grid_summary_calc, aes(x = wp_wlastn_lag1_1 - wp_wlastn_lag1_2, y = line_1_avg)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +
  geom_smooth(method = "glm", color = "blue")


ggplot(data = d_grid_summary_calc, aes(x = pd_wlastn_lag1_1_avg - pd_wlastn_lag1_2_avg, y = line_1_avg)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +
  geom_smooth(method = "glm", color = "blue")

glm_1a_all <- glm(line_1 ~ wp_wlastn_lag1_1 + wp_wlastn_lag1_2, data = d_rbind_join_2)
summary(glm_1a_all)
ggplot(data = distinct(d_rbind_join_2, season, week, team_home, team_away, .keep_all = TRUE),
       aes(x = pd_wlastn_lag1_1 - pd_wlastn_lag1_2, y = line_1)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +
  geom_smooth(method = "glm", color = "blue")

y_2 <- "pd_1_avg"
fmla_2a <-str_replace(fmla_1a, y_1, y_2)
fmla_2b <-str_replace(fmla_1b, y_1, y_2)
fmla_2c <-str_replace(fmla_1c, y_1, y_2)
fmla_2d <-str_replace(fmla_1d, y_1, y_2)
lm_2a <- lm(fmla_2a, data = d_grid_summary_calc)
lm_2b <- lm(fmla_2b, data = d_grid_summary_calc)
lm_2c <- lm(fmla_2c, data = d_grid_summary_calc)
lm_2d <- lm(fmla_2d, data = d_grid_summary_calc)
summary(lm_2a)
mean(abs(lm_2a$residuals))
summary(lm_2b)
mean(abs(lm_2b$residuals))
summary(lm_2c)
mean(abs(lm_2c$residuals))
summary(lm_2d)
mean(abs(lm_2d$residuals))
histogram(lm_2$residuals)

# require("glmnet")
# glma_1a_cv <- cv.glmnet(fmla_1a, data = d_grid_summary_calc)

require("boot")
set.seed(42)
glm_1a_cv_k10 <- cv.glm(data = d_grid_summary_calc, glmfit = glm_1a, K = 10)
round(1 - sqrt(lm_1a_cv_k10$delta[1]), 2)


set.seed(42)
glm_1a_cv_loocv <- cv.glm(data = d_grid_summary_calc, glmfit = lm_1a)
glm_1a_cv_loocv
round(1 - sqrt(glm_1a_cv_loocv$delta[1]), 2)
?cv.glm



