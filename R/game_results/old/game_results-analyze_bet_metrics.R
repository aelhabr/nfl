
# Import game results data. ----------------------------------------------------

file_name <- "pfref_game_results"
file_path_import <- paste0("data/", file_name, "_cleaned.csv")
game_results <- read.csv(file_path_import, stringsAsFactors = FALSE)

# Visualize how lines/totals how changed over time. --------------------------

library("ggplot2")
library("dplyr")
game_results %>%
  group_by(season) %>%
  summarise(line_home_avg = round(mean(line_home), 2)) %>%
  ggplot(aes(x = season, y = line_home_avg)) +
  geom_line() +
  scale_y_reverse() +
  labs(
    title = "Average Home Line By Season",
    y = "",
    x = ""
  ) +
  theme_minimal()

game_results %>%
  filter(!is.na(total)) %>%
  group_by(season) %>%
  summarise(total_avg = round(mean(total), 2)) %>%
  ggplot(aes(x = season, y = total_avg)) +
  geom_line() +
  labs(
    title = "Average Total By Season",
    y = "",
    x = ""
  ) +
  theme_minimal()

# Evalutate favorites/underdogs over time. -----------------------------------

game_results %>%
  mutate(pts_diff = pts_home - pts_away) %>%
  ggplot() +
  geom_histogram(aes(x = line_home), fill = "red", alpha = 0.5) +
  geom_histogram(aes(x = pts_diff), fill = "blue", alpha = 0.5) +
  # scale_fill_manual(values = Hmisc::Cs(red, blue), labels = Hmisc::Cs(line_home, pts_diff)) +
  labs(
    title = "Distribution of (Pre-Game) Home Lines and (Post-Game) Point Differential",
    y = "",
    x = ""
  ) +
  theme_minimal()

bet_q_season <-
  game_results %>%
  group_by(season) %>%
  summarise(
    q_25 = quantile(line_home, 0.25, na.rm = TRUE),
    q_50 = quantile(line_home, 0.5, na.rm = TRUE),
    q_75 = quantile(line_home, 0.75, na.rm = TRUE)
  )
bet_q_season

bet_q_overall <-
  game_results %>%
  summarise(
    q_25 = quantile(line_home, 0.25, na.rm = TRUE),
    q_50 = quantile(line_home, 0.5, na.rm = TRUE),
    q_75 = quantile(line_home, 0.75, na.rm = TRUE),
    q_10 = quantile(line_home, 0.1, na.rm = TRUE),
    q_90 = quantile(line_home, 0.9, na.rm = TRUE)
  )
bet_q_overall

bet_metrics <-
  game_results %>%
  mutate(
    tm_winner_diff =
      ifelse(
        tm_winner_straight != tm_winner_spread, 1, 0),
    auw =
      ifelse(
        line_home < 0,
        ifelse(tm_winner_spread == tm_away,
               1, 0),
        as.numeric(NULL)),
    huw =
      ifelse(
        line_home > 0,
        ifelse(tm_winner_spread == tm_home,
               1, 0),
        as.numeric(NULL)),
    afw =
      ifelse(
        line_home > 0,
        ifelse(tm_winner_spread == tm_away,
               1, 0),
        as.numeric(NULL)),
    hfw =
      ifelse(
        line_home < 0,
        ifelse(tm_winner_spread == tm_home,
               1, 0),
        as.numeric(NULL)),
    no_fav = ifelse(line_home == 0, 1, 0),
    push = ifelse((pts_home + line_home) == pts_away, 1, 0)

  )

calculate_pct <- function(x, na_rm = TRUE, digits_round = 2) {
  round((sum(x, na.rm = na_rm) / sum(!is.na(x))), digits_round)
}

bet_metrics_summary <-
  bet_metrics %>%
  group_by(season) %>%
  summarise(
    g = n(),
    spread_straight_diff_count = sum(tm_winner_diff),
    spread_straight_diff_pct = calculate_pct(tm_winner_diff),
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
  bet_metrics_summary %>%
  mutate(
    counts_sum = auw_count + huw_count + afw_count + hfw_count
  ) %>%
  mutate(
    counts_diff = g - counts_sum,
    counts_diff_2 = g - counts_sum - no_fav_count - push_count
  ) %>%
  select(season, g, counts_sum, counts_diff, no_fav_count, push_count, counts_diff_2)

rm(list = Hmisc::Cs(bet_metrics_check))


# Remove some of the values because they portray the same information.
library("stringr")
cols_hxw_pct <-
  names(bet_metrics_summary) %>%
  str_subset("w_pct$") %>%
  str_subset("^h")

cols_tidy_idx <-
  names(bet_metrics_summary) %>%
  str_which("count$|pct$")
cols_tidy_idx

library("tidyr")
bet_metrics_summary %>%
  gather(key, value, cols_tidy_idx, factor_key = TRUE) %>%
  filter(key %in% cols_hxw_pct) %>%
  ggplot(aes(x = season)) +
  geom_line(aes(y = value, group = key, color = key)) +
  geom_smooth(aes(y = value, group = key, color = key), method = lm, se = TRUE, size = 2) +
  # coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Home Underdog/Favorite Win % ATS",
    y = "",
    x = ""
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Check bet metrics in "binned" ranges. ----------------------------------------
library("magrittr")
bet_metrics %<>%
  mutate(
    line_50 = ifelse(line_home <= bet_q_overall$q_50, 1, 0),
    line_25 = ifelse(line_home <= bet_q_overall$q_25, 1, 0),
    line_75 = ifelse(line_home >= bet_q_overall$q_75, 1, 0),
    line_10 = ifelse(line_home <= bet_q_overall$q_10, 1, 0),
    line_90 = ifelse(line_home >= bet_q_overall$q_90, 1, 0)
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
  bet_metrics %>%
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
    huw_b75_pct = calculate_pct(huw, line_75, 0, 1)
  )
bet_metrics_summary_2
glimpse(bet_metrics_summary_2)

cols_tidy_idx_2 <-
  names(bet_metrics_summary_2) %>%
  str_which("_[0-9]5_pct$")
cols_tidy_idx_2

bet_metrics_summary_2 %>%
  gather(key, value, cols_tidy_idx_2, factor_key = TRUE) %>%
  ggplot(aes(x = season)) +
  geom_line(aes(y = value, group = key, color = key)) +
  geom_smooth(aes(y = value, group = key, color = key), method = lm, se = TRUE, size = 2) +
  theme(legend.title = element_blank())

# Import my picks data and join with other data.. ------------------------------

library("readxl")
file_path_import_picks <- "data/db_nfl.xlsm"
excel_sheets(file_path_import_picks)
ws_game_picks <- "nfl_game_picks"

game_picks <-
  file_path_import_picks %>% 
  read_excel(sheet = ws_game_picks)
game_picks

game_picks %<>% filter(person != "k")

game_picks_2 <-
  game_picks %>%
  separate(game_results_name, Hmisc::Cs(season, week, tms), sep = "_") %>% 
  separate(tms, Hmisc::Cs(tm_away, tm_home), sep = "@")

game_picks_2 %<>%
  mutate_at(vars(Hmisc::Cs(season, week)), funs(as.numeric)) %>% 
  mutate_at(vars(Hmisc::Cs(person)), funs(as.factor))
# ws_game_results <- "nfl_game_results"
# game_results_xlsx <-
#   file_path_import_picks %>% 
#   read_excel(sheet = ws_game_results)
# game_results_xlsx

game_picks_yrs <- game_picks_2 %>% pull(season) %>% unique() %>% sort()
game_picks_yrs

bet_metrics_filtered <-
  filter(bet_metrics, season %in% game_picks_yrs)

# Note that these data sets have some redundant columns that will
# not be included in the join. These are the columns for line values
# and the spread/straight winner.

check_names <- function(d_1, d_2, direction = 1) {
  if(direction == -1) {
    temp <- d_1
    d_1 <- d_2
    d_2 <- temp
  }
  names(d_1)[(names(d_1) %in% names(d_2))]
}

check_names(game_results_2, game_picks_2)
# check_names(game_results_2, game_picks_2, -1)

# Recalculate the line change and difference values instead of dropping them.
# Don't join on team names because they differ for some teams between the two data sets.
# Instead, join on final scores.
cols_drop_picks <- Hmisc::Cs(pts_home, pts_away, tm_winner_spread, tm_winner_straight)
cols_join <- Hmisc::Cs(season, week, tm_home, tm_away)

game_picks_join <-
  game_picks_2 %>%
  select(-one_of(cols_drop_picks)) %>% 
  inner_join(bet_metrics_filtered, by = cols_join)
game_picks_join

# Debugging...
game_picks_2 %>%
  select(-one_of(cols_drop_picks)) %>% 
  anti_join(game_results, by = cols_join) %>% 
  filter(season < 2017) %>% 
  distinct(game_results_id, .keep_all = TRUE) %>% 
  arrange(game_results_id)

game_picks_join %<>%
  mutate(line_home_close = line_home) %>% 
  select(-line_home) %>% 
  mutate(
    line_home_change = abs(line_home_close - line_home_open),
    line_home_pick_diff =
      abs(line_home_pick - 
            ifelse(is.na(line_home_open), line_home_close, line_home_open)
      )
  )

# Debugging...
game_picks_join %>% 
  group_by(season, person) %>% 
  filter(person == "t") %>% 
  summarise(n = n()) %>% 
  ungroup

# Visualize my picks. -------------------------------------------------------
game_picks_calc_week <-
  game_picks_join %>%
  group_by(season, week, person) %>% 
  summarise(
    pick_correct_count_spread = sum(pick_result_spread, na.rm = TRUE),
    pick_correct_count_straight = sum(pick_result_straight, na.rm = TRUE),
    pick_correct_pct_spread = calculate_pct(pick_result_spread),
    pick_correct_pct_straight = calculate_pct(pick_result_straight)
  )

game_picks_calc_week %>% 
  filter(week <= 17) %>% 
  ggplot() +
  geom_line(
    aes(x = week, y = pick_correct_pct_spread, 
        group = person, color = person)
  ) +
  facet_wrap( ~ season) +
  labs(
    title = "Regular Season Pick Correct % ATS by Week"
  ) +
  theme_minimal()

game_picks_calc_season <-
  game_picks_join %>%
  filter(week <= 17) %>% 
  group_by(season, person) %>% 
  summarise(
    pick_correct_count_spread = sum(pick_result_spread, na.rm = TRUE),
    pick_correct_count_straight = sum(pick_result_straight, na.rm = TRUE),
    pick_correct_pct_spread = calculate_pct(pick_result_spread),
    pick_correct_pct_straight = calculate_pct(pick_result_straight)
  )

game_picks_calc_season %>% 
  ggplot() +
  geom_col(
    aes(x = season, y = pick_correct_pct_spread,
        group = person, fill = person
    ),
    position = "dodge"
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "Regular Season Pick Correct % ATS by Season"
  ) +
  theme_minimal()

cols_tidy_idx <=
  names(game_picks_join) %>% 
  str_which("uw$|fw$")

game_picks_join_tidy <-
  game_picks_join %>% 
  gather(bet_type, value, cols_tidy_idx, factor_key = TRUE)

game_picks_calc_season_2 <-
  game_picks_calc_season %>% 
  group_by(season, bet_type) %>% 
  mutate(
    pick_correct_count_spread = sum(pick_result_spread, na.rm = TRUE),
    pick_correct_pct_spread = calculate_pct(pick_result_spread)
  )
