

# Calculated constants. ----





# Functions. ----





# Viz constants. ----


# Prep main data for analysis. ----

#'
#'
#'
# Analyze head-to-head line picks. ----

#'
#'
#'


#'
#'
#'

#'
#'
#'
line_picks_tidy <-
  game_picks_join %>%
  mutate(line_home = ifelse(is.na(line_home_open), line_home_close, line_home_open)) %>%
  select(-starts_with("line_home_[co]")) %>%
  select(one_of(cols_display_line_picks)) %>%
  tidyr::spread(person, line_home_pick, fill = 0, sep = "_")
line_picks_tidy

line_picks_calc %>%
  group_by(season, wk, tm_away, tm_home, line_home)

line_picks_calc <-
  line_picks_tidy %>%
  mutate_at(vars(person_a, person_t), funs(diff = line_home - .)) %>%
  mutate(winner_1g = determine_h2h_winner(person_a, person_t))
line_picks_calc

line_picks_summary_bywk <-
  line_picks_calc %>%
  group_by_at(c(cols_date, "winner_1g")) %>%
  summarise(winner_1g_cnt = n())
line_picks_summary_bywk %>% tail()

line_picks_bywk_spread <-
  line_picks_summary_bywk %>%
  group_by(season, winner_1g) %>%
  tidyr::spread(winner_1g, winner_1g_cnt) %>%
  arrange(season, wk)
line_picks_bywk_spread %>% tail()

line_picks_bywk_output <-
  line_picks_bywk_spread %>%
  tail(wks_display)
line_picks_bywk_output

viz_line_picks_bywk_output <-
  line_picks_bywk_spread %>%
  tidyr::gather(winner, winner_cnt,-season,-wk) %>%
  filter(season == season_curr) %>%
  filter(wk < wk_curr) %>%
  ggplot() +
  geom_col(aes(x = wk, y = winner_cnt, fill = winner), position = "dodge") +
  scale_fill_manual(values = colors_2persons_1default) +
  scale_x_continuous(breaks = x_wk_vals) +
  scale_y_continuous(breaks = y_wk_vals) +
  labs(title = title_line_picks) +
  labs(subtitle = paste0("Games Count", ", ", "By Week, for ", season_curr)) +
  theme_custom
viz_line_picks_bywk_output

#'
#'
#'
# Both filter statements are intended to nullify the most recent week.
line_picks_bywk_spread_calc <-
  line_picks_bywk_spread %>%
  mutate(winner_1wk = determine_h2h_winner(a, t)) %>%
  filter(!is.na(winner_1wk)) %>%
  filter(season != season_curr | (season ==season_curr & wk < wk_curr))
line_picks_bywk_spread_calc %>% tail()

line_picks_summary_byseason <-
  line_picks_bywk_spread_calc %>%
  group_by(season, winner_1wk) %>%
  summarise(winner_1wk_cnt = n())
line_picks_summary_byseason %>% tail()

line_picks_byseason_spread <-
  line_picks_summary_byseason %>%
  spread(winner_1wk, winner_1wk_cnt)
line_picks_byseason_spread %>% tail()

line_picks_byseason_output <-
  line_picks_byseason_spread %>%
  tail(seasons_display)
line_picks_byseason_output

#'
#'
#'

viz_line_picks_byseason_output <-
  line_picks_byseason_spread %>%
  gather(winner, winner_cnt,-season) %>%
  ggplot() +
  geom_col(aes(x = season, y = winner_cnt, fill = winner), position = "dodge") +
  scale_fill_manual(values = colors_2persons_1default) +
  scale_y_continuous(breaks = y_wk_vals) +
  labs(title = title_line_picks) +
  labs(subtitle = paste0(
    "Week Count",
    ", ",
    "By Season, for ",
    season_display_min,
    " - ",
    season_curr
  )) +
  theme_custom
viz_line_picks_byseason_output

#'
#'
#'
# Analyze results picks. ----
cols_precalc_results_picks <-
  c("pick_result_spread", "pick_result_straight")
cols_display_results_picks <-
  c(cols_game_id,
    "person",
    "line_home_close",
    "tm_pick_spread",
    "tm_winner_spread")

results_picks_calc <-
  game_picks_join %>%
  select(-one_of(cols_precalc_results_picks)) %>%
  mutate(
    pick_result_spread = determine_if_correct(tm_pick_spread, tm_winner_spread),
    pick_result_straight = determine_if_correct(tm_pick_straight, tm_winner_straight)
  ) %>%
  filter(!is.na(pick_result_spread))
results_picks_calc
#'
#'
#'
results_picks_calc %>%
  filter(season == season_curr, wk == wk_prev) %>%
  mutate(matchup = paste0(tm_away, "@", tm_home)) %>%
  ggplot() +
  geom_col(aes(x = matchup, y = confidence_spread, fill = person), position = "dodge") +
  scale_fill_manual(values = colors_2persons_1default) +
  coord_flip() +
  theme_custom

results_picks_calc_recent <-
  results_picks_calc %>%
  filter(season == season_curr, wk == wk_prev) %>%
  mutate(matchup = paste0(tm_away, "@", tm_home)) %>%
  mutate(pts_h2a_diff = pts_home - pts_away) %>%
  mutate(w_home = ifelse(pts_h2a_diff > 0, "yes", "no")) %>%
  mutate(matchup = forcats::fct_reorder(as.factor(matchup), line_home_close))

arrow_padding <- 1.5
arrow_offset <- 5
results_picks_calc_recent <-
  results_picks_calc_recent %>%
  mutate(arrow_direction = ifelse(tm_pick_spread == tm_home, -1, 1)) %>%
  mutate(pick_arrow_1 = line_home_close + arrow_padding * arrow_direction,
         pick_arrow_2 = line_home_close + (arrow_padding + arrow_offset) * arrow_direction)

# viz_colors_contrast2 <- c("#00ba38", "#f8766d")
colors_contrast2 <- list(high = "#00ba38", low = "#f8766d")
point_size <- 10
viz_results_picks_recent <-
  results_picks_calc_recent %>%
  ggplot() +
  geom_hline(aes(yintercept = 0), color = "black") +
  geom_segment(
    aes(
      x = matchup,
      y = pick_arrow_1,
      xend = matchup,
      yend = pick_arrow_2,
      color = person
    ),
    size = 2,
    arrow = arrow(length = unit(0.1,"cm"))
  ) +
  # scale_color_manual(values = colors_2persons_1default) +
  geom_point(
    aes(x = matchup, y = line_home_close),
    shape = 21,
    color = "black",
    size = point_size
  ) +
  geom_text(aes(x = matchup, y = line_home_close, label = line_home_close)) +
  geom_point(aes(x = matchup, y = pts_h2a_diff, color = w_home), size = point_size) +
  # scale_color_manual(
  #   name = "Home Win",
  #   labels = c("Yes", "No"),
  #   values = c("yes" = colors_contrast2$high,
  #              "no" =  colors_contrast2$low)
  # ) +
  geom_text(aes(x = matchup, y = pts_h2a_diff, label = pts_h2a_diff)) +
  geom_segment(aes(
    x = matchup,
    y = line_home_close,
    xend = matchup,
    yend = pts_h2a_diff
  ),
  linetype = "dotted") +
  coord_flip()
viz_results_picks_recent
#'
#'
#'
results_picks_summary <-
  results_picks_calc %>%
  group_by_at(c(cols_date, "person", "pick_result_spread")) %>%
  summarise(pick_result_spread_cnt = n())
results_picks_summary %>% tail()

results_picks_bywk <-
  results_picks_summary %>%
  # spread(pick_result_spread, pick_result_spread_cnt, sep = "_") %>%
  spread(pick_result_spread, pick_result_spread_cnt) %>%
  mutate(correct_pct = correct / (correct + incorrect)) %>%
  ungroup() %>%
  group_by(season, person) %>%
  mutate(correct_pct_td = cumsum(correct) / (cumsum(correct) + cumsum(incorrect)))
results_picks_bywk %>% tail()

results_picks_bywk_output <-
  results_picks_bywk %>%
  tail(wks_display * length(persons_display))
results_picks_bywk_output

#'
#'
#'

viz_results_picks_bywk_output <-
  results_picks_bywk %>%
  filter(season == season_curr) %>%
  ggplot() +
  geom_col(aes(x = wk, y = correct_pct, fill = person),
           position = "dodge",
           alpha = alpha_secondary) +
  geom_line(aes(x = wk, y = correct_pct_td, color = person), size = 2) +
  geom_hline(
    aes(yintercept = 0.5),
    color = "red",
    size = 1,
    linetype = "dashed"
  ) +
  scale_fill_manual(values = colors_2persons_1default) +
  scale_color_manual(values = colors_2persons_1default) +
  scale_x_continuous(breaks = x_wk_vals) +
  labs(fill = "correct_pct", color = "correct_pct_td") +
  labs(title = title_results_picks) +
  labs(subtitle = paste0("Correct %", ", ", "By Week, For ", season_curr)) +
  theme_custom
viz_results_picks_bywk_output

viz_results_picks_bywk_byseason_output <-
  results_picks_bywk %>%
  ggplot() +
  geom_col(aes(x = wk, y = correct_pct, fill = person),
           position = "dodge",
           alpha = alpha_secondary) +
  geom_line(aes(x = wk, y = correct_pct_td, color = person), size = 2) +
  geom_hline(aes(yintercept = 0.5), color = "black", size = 1) +
  scale_fill_manual(values = colors_2persons_1default) +
  scale_color_manual(values = colors_2persons_1default) +
  scale_x_continuous(breaks = x_wk_vals) +
  facet_wrap(~ season) +
  labs(fill = "correct_pct", color = "correct_pct_td") +
  labs(title = title_results_picks) +
  labs(subtitle = paste0(
    "Correct %",
    ", ",
    "By Week, For ",
    season_display_min,
    " - ",
    season_curr
  )) +
  theme_custom
viz_results_picks_bywk_byseason_output
#'
#'
#'
results_picks_byseason <-
  results_picks_bywk %>%
  group_by(season, person) %>%
  # mutate(correct_pct = correct / (correct + incorrect))
  summarise_at(vars(correct, incorrect), funs(sum), na.rm = TRUE) %>%
  mutate(correct_pct = correct / (correct + incorrect))
results_picks_byseason %>% tail()

results_picks_byseason_output <-
  results_picks_bywk %>%
  tail(seasons_display * length(persons_display))
results_picks_byseason_output
#'
#'
#'

viz_results_picks_byseason_output <-
  results_picks_byseason %>%
  ggplot() +
  geom_col(aes(x = season, y = correct_pct, fill = person), position = "dodge") +
  geom_hline(aes(yintercept = 0.5), color = "black", size = 1) +
  scale_fill_manual(values = colors_2persons_1default) +
  scale_color_manual(values = colors_2persons_1default) +
  scale_x_continuous(breaks = x_season_vals) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  # labs(fill = "correct_pct") +
  labs(title = title_results_picks) +
  labs(subtitle = paste0(
    "Correct %",
    ", ",
    "By Season, for ",
    season_display_min,
    " - ",
    season_curr
  )) +
  theme_custom

#'
#' # Current Season
#'
#' ## By Week
#'
#+ results = "md", fig.show = "asis"
line_picks_bywk_output
viz_line_picks_bywk_output

#'
#'
#'
#+ results = "md", fig.show = "asis"
results_picks_bywk_output
viz_results_picks_bywk_output

#'
#' # All Seasons
#'
#' ## By Season
#'
#+ results = "md", fig.show = "asis"
line_picks_byseason_output
viz_line_picks_byseason_output

#'
#'
#'
#+ results = "md", fig.show = "asis"
results_picks_byseason_output
viz_results_picks_byseason_output

#'
#'
#'
#+ include = FALSE
viz_results_picks_bywk_byseason_output

#'
#'
#'
