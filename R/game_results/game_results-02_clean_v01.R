

rm(list = ls())
setwd("O:/_other/projects/nfl/")

# Parameters. ----
filename_base <- "pfref_game_results"
dir_import <- "data/scraped/"

library("stringr")
filepath_import <-
	str_c(dir_import, filename_base, "_scraped.csv")
game_results_0 <-
	read.csv(filepath_import, stringsAsFactors = FALSE)
export <- FALSE

# Clean. ----

names(game_results_0)
library("dplyr")
game_results_0 %>% pull(tm) %>% unique() %>% sort()
game_results_0 %>% pull(vs_line) %>% unique() %>% sort()
game_results_0 %>% pull(ou_result) %>% unique() %>% sort()

# Note that ifelse() is used if performing an operation on the
# same column where the result is stored, but if_else() is used otherwise.
# This is due to the vectorized nature of if_else().
# Using transmute() or not renaming the variables beforehand
# could be possible alternatives.
game_results_1 <-
	game_results_0 %>%
	select(-rk, -date, -time, -ltime, -g, -day, -ot, -vs_line) %>%
	rename(season = year,
				 total = over_under,
				 total_result = ou_result) %>%
	mutate(
		tm_home = if_else(X == "", tm, opp),
		tm_away = if_else(X == "", opp, tm),
		line_home = if_else(X == "", spread, -spread),
		total = ifelse(total == 0, as.numeric(NULL), total),
		total_result = ifelse(total == 0, as.character(NULL), total_result)
	) %>%
	select(-tm, -opp, -spread)

# The scraped data has some duplicate records.
game_results_1 <-
	game_results_1 %>%
	distinct(season, week, tm_home, tm_away, .keep_all = TRUE)

library("tidyr")
game_results_2 <-
	game_results_1 %>%
	separate(result, Hmisc::Cs(result, score_1, score_2))

game_results_2 <-
	game_results_2 %>%
	mutate(
		pts_home = if_else(X == "", score_1, score_2),
		pts_away = if_else(X == "", score_2, score_1)
	) %>%
	select(-score_1, -score_2)

cols_numeric <-
	Hmisc::Cs(season, week, pts_home, pts_away, line_home, total)

game_results_2 %<>%
	mutate_at(vars(cols_numeric), funs(as.numeric))

game_results_3 <-
	game_results_2 %>%
	mutate(
		tm_winner_straight =
			if_else(
				pts_home > pts_away,
				tm_home,
				if_else(pts_home < pts_away,
								tm_away,
								"na")
			),
		tm_winner_spread =
			if_else(
				(pts_home + line_home) > pts_away,
				tm_home,
				if_else((pts_home + line_home) < pts_away,
								tm_away,
								"na")
			)
	)


# library("readxl")
# # This is Tony's self-created spreadsheet.
# db_filepath <- "data/nfl/db_nfl.xlsm"
# excel_sheets(db_filepath)
# tms_db <- read_excel(db_filepath, sheet = "nfl_teams")
# tms_db <- tms_db %>% pull(tm) %>% unique %>% sort()
# tms_raw <- game_results_3 %>% pull(tm_home) %>% unique() %>% sort()
# setdiff(tms_raw, tms_db)
# setdiff(tms_db, tms_raw)
cols_tms_idx <- str_which(names(game_results_3), c("tm"))

combos_yr_tm <-
	game_results_3 %>%
	distinct(season, tm_home) %>%
	group_by(tm_home) %>%
	mutate(rn = row_number(season),
				 yrs_count = row_number(desc(season))) %>%
	ungroup() %>%
	filter(rn == 1) %>%
	select(-rn) %>%
	rename(yr_first = season,
				 tm = tm_home) %>%
	mutate(yr_last = yr_first + yrs_count)
combos_yr_tm
combos_yr_tm %>% filter(yrs_count != max(yrs_count))
glimpse(combos_yr_tm)

game_results_4 <- game_results_3
# Note the following team changes after 1978
# (per https://en.wikipedia.org/wiki/National_Football_League_franchise_moves_and_mergers):
# Baltimore Colts: to Indianapolis in 1984
# St. Louis Cardinals: to Phoenix in 1988 (renamed Arizona Cardinals in 1994)
# Los Angeles Rams: to St. Louis in 1995
# Los Angeles Raiders: back to Oakland in 1995
# Houston Oilers: temporarily to Memphis in 1997 as the Tennessee Oilers and permanently to Nashville in 1998 (renamed Tennessee Titans in 1999)
# St. Louis Rams: back to Los Angeles in 2016
# San Diego Chargers: back to Los Angeles in 2017
# Oakland Raiders: to Las Vegas in either 2019 or 2020

# Only correcting names for which I use two characters.
# Not attempting to fix city names.
game_results_4[cols_tms_idx] <-
	lapply(game_results_4[cols_tms_idx], function(d) {
		d %>%
			str_replace_all("GNB", "GB") %>%
			str_replace_all("KAN", "KC") %>%
			str_replace_all("NOR", "NO") %>%
			str_replace_all("NWE", "NE") %>%
			# str_replace_all("PHO", "ARI") %>%
			# str_replace_all("RAI", "OAK") %>%
			# str_replace_all("RAM", "LAR") %>%
			str_replace_all("SDG", "SD") %>%
			str_replace_all("SFO", "SF") %>%
			# str_replace_all("STL", "LAR") %>%
			str_replace_all("TAM", "TB")
	})

tms_clean <-
	game_results_4 %>% pull(tm_home) %>% unique() %>% sort()
tms_clean
# setdiff(tms_db, tms_clean)
# setdiff(tms_clean, tms_db)

game_results_4 <-
	game_results_4 %>%
	select(-X, -result)

if (export == TRUE) {
	dir_export <- str_c("data/created/")
	filepath_export <-
		str_c(dir_export, filename_base, "_cleaned.csv")
	write.csv(game_results_4, filepath_export, row.names = FALSE)
}
