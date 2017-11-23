
# Import win total picks. ------------------------------------------------------

filepath_import_picks <- "data/db_nfl.xlsm"
excel_sheets(filepath_import_picks)
ws_wt_picks <- "nfl_win_total_picks"

library("readxl")
library("dplyr")
wt_picks <-
  filepath_import_picks %>% 
  read_excel(sheet = ws_wt_picks)
wt_picks

library("magrittr")
wt_picks %<>% filter(person != "k")

library("tidyr")
wt_picks_2 <-
  wt_picks %>%
  separate(win_total_results_name, Hmisc::Cs(season, tm), sep = "_")
wt_picks_2 %<>% mutate_at(vars(season), funs(as.numeric))

# Import game picks. -----------------------------------------------------------

ws_game_picks <- "nfl_game_picks"

# Change the n_max paramter because the first non-blank tm_pick_straight is not
# seen in the firs 1000 rows.
game_picks <-
  filepath_import_picks %>% 
  read_excel(sheet = ws_game_picks, guess_max = 2000)
game_picks

game_picks %<>% filter(person != "k")

game_picks_2 <-
  game_picks %>%
  separate(game_results_name, Hmisc::Cs(season, week, tms), sep = "_") %>% 
  separate(tms, Hmisc::Cs(tm_away, tm_home), sep = "@")

game_picks_2 %<>% mutate_at(vars(Hmisc::Cs(season, week)), funs(as.numeric))
# game_picks_2 %<>% mutate_at(vars(Hmisc::Cs(person)), funs(as.factor))

game_picks_summary_straight <-
  game_picks_2 %>% 
  filter(season >= 2015) %>% 
  filter(week <= 17) %>% 
  group_by(season, person, tm_pick_straight) %>%
  summarise(w_pick_game_straight = n()) %>% 
  ungroup() %>% 
  rename(tm = tm_pick_straight)

# Debugging...
game_picks_summary_straight
game_picks_summary_straight %>% 
  filter(person == "t") %>% 
  filter(season == 2016) %>% 
  arrange(season, desc(w_pick_game_straight))

game_picks_summary_spread <-
  game_picks_2 %>% 
  filter(week <= 17) %>% 
  group_by(season, person, tm_pick_spread) %>%
  summarise(w_pick_game_spread = n()) %>% 
  ungroup() %>% 
  rename(tm = tm_pick_spread)

# Debugging...
game_picks_summary_spread
game_picks_summary_spread %>%
  filter(person == "t") %>%
  filter(season == 2016) %>%
  arrange(season, desc(w_pick_game_spread))

# This doesn't work because it doesn't have a singular tm column.
# Using the summary data sets instead.
# game_picks_summary_calc <-
#   game_picks_2 %>% 
#   group_by(season, person, tm_home) %>% 
#   summarise(
#     pick_straight_no_na = sum(!is.na(tm_pick_straight)),
#     pick_straight = sum(tm_home == tm_pick_straight, na.rm = TRUE),
#     pick_spread = sum(tm_home == tm_pick_spread, na.rm = TRUE)
#   ) %>% 
#   ungroup()
# 
# # Debugging...
# game_picks_summary_calc %>% 
#   filter(person == "t") %>%
#   filter(season == 2016) %>% 
#   arrange(season, desc(pick_straight))

eoy_records <- read.csv("data/nfl_eoy_records.csv")
eoy_records %<>% mutate_at(vars(tm), funs(as.character))
cols_join <- Hmisc::Cs(season, person, tm)
wt_picks_join <-
  wt_picks_2 %>% 
  left_join(game_picks_summary_spread, by = cols_join) %>% 
  left_join(game_picks_summary_straight, by = cols_join) %>% 
  left_join(eoy_records, by = Hmisc::Cs(season, tm))

wt_picks_join 
wt_picks_join %>% filter(!is.na(w_pick_game_straight))

wt_picks_join_calc <-
  wt_picks_join %>%
  mutate(
    w_pick_diff_spread = ifelse(is.na(w_pick_game_spread), NA, w_pick - w_pick_game_spread),
    w_pick_diff_straight = ifelse(is.na(w_pick_game_straight), NA, w_pick - w_pick_game_straight)
  )

wt_picks_join_calc %>% 
  select(-id, -win_total_results_id) %>% 
  filter(person == "t") %>% 
  arrange(w_pick_diff_straight)

wt_picks_join_calc %>% 
  select(-id, -win_total_results_id) %>% 
  filter(season != 2017) %>% 
  filter(person == "t") %>% 
  arrange(desc(w_pick_diff_straight))


