

rm(list = ls())
setwd("O:/_other/projects/nfl/")

# Parameters. ----
filename_base <- "pfref_game_results_tm_to"
dir_import <- "data/scraped/"

library("stringr")
filepath_import <-
	str_c(dir_import, filename_base, "_scraped.csv")
d_0 <-
	read.csv(filepath_import, stringsAsFactors = FALSE) %>% as_tibble()
export <- FALSE

# Clean. ----

library("dplyr")
names(d_0)
d_1 <-
	d_0 %>%
	select(year, week, tm, opp, X, result, to) %>%
	rename(season = year) %>%
	mutate(
		tm_home = if_else(X == "", tm, opp),
		tm_away = if_else(X == "", opp, tm)
	) %>%
  select(-opp) %>% 
  mutate_at(vars(season, week, to), funs(as.numeric)) %>% 
  arrange(season, week, tm, tm_home, tm_away)

d_1
d_1 %>% filter(tm == "DAL")

library("tidyr")
d_2 <-
	d_1 %>%
	separate(result, c("result", "pts_1", "pts_2"))

d_3 <-
	d_2 %>%
	mutate(
		pts_home = if_else(X == "", pts_1, pts_2),
		pts_away = if_else(X == "", pts_2, pts_1)
	) %>%
  rename(pts = pts_1) %>% 
	select(-X, -pts_2) %>% 
  mutate_at(vars(pts, pts_home, pts_away), funs(as.numeric)) %>% 
  mutate(result = factor(result))

output <- d_3

if (export == TRUE) {
	dir_export <- str_c("data/created/")
	filepath_export <-
		str_c(dir_export, filename_base, "_cleaned.csv")
	write.csv(output, filepath_export, row.names = FALSE)
}
