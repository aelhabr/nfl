
# STILL NEED TO TEST FOR REAL.
rm(list = ls())
setwd("O:/_other/projects/nfl/")

# Packages. ----
library("dplyr")
library("stringr")
library("readr")
library("tidyr")

# Parameters. ----
filename_import_base <- "game_results"
filename_import_suffix <- "_scraped"
filename_import_ext <- ".csv"
dir_import <- "data/"


filepath_import <-
  str_c(dir_import,
        filename_import_base,
        filename_import_suffix,
        filename_import_ext)

export <- TRUE

if (export == TRUE) {
  filename_export_base <- filename_import_base
  filename_export_suffix <- "_cleaned"
  filename_export_ext <- filename_import_ext
  dir_export <- dir_import
  filepath_export <-
    str_c(dir_export,
          filename_export_base,
          filename_export_suffix,
          filename_export_ext)
}

# Clean. ----
d <- read_csv(filepath_import)
names(d)
library("dplyr")
d %>% pull(tm) %>% unique() %>% sort()
d %>% pull(vs_line) %>% unique() %>% sort()
d %>% pull(ou_result) %>% unique() %>% sort()

# Note that ifelse() is used if performing an operation on the
# same column where the result is stored, but if_else() is used otherwise.
# This is due to the vectorized nature of if_else().
# Using transmute() or not renaming the variables beforehand
# could be possible alternatives.
d_1 <-
	d %>%
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
d_1 <-
	d_1 %>%
	distinct(season, week, tm_home, tm_away, .keep_all = TRUE)


d_2 <-
	d_1 %>%
	separate(result, c("result", "pts_1", "pts_2"))

d_2 <-
	d_2 %>%
	mutate(
		pts_home = if_else(X == "", pts_1, pts_2),
		pts_away = if_else(X == "", pts_2, pts_1)
	) %>%
	select(-pts_1, -pts_2)

cols_numeric <- c("season", "week", "pts_home", "pts_away", "line_home", "total")

d_2 <-
  d_2 %>%
	mutate_at(vars(cols_numeric), funs(as.numeric))

# Move this to prepare_model file.
d_3 <-
	d_2 %>%
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
# tms_raw <- d_3 %>% pull(tm_home) %>% unique() %>% sort()
# setdiff(tms_raw, tms_db)
# setdiff(tms_db, tms_raw)
cols_tms_idx <- str_which(names(d_3), c("tm"))

combos_yr_tm <-
	d_3 %>%
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

d_4 <- d_3
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
d_4[cols_tms_idx] <-
	lapply(d_4[cols_tms_idx], function(d) {
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
	d_4 %>% pull(tm_home) %>% unique() %>% sort()
tms_clean
# setdiff(tms_db, tms_clean)
# setdiff(tms_clean, tms_db)

d_4 <-
	d_4 %>%
	select(-X, -result)

if (export == TRUE) {
	dir_export <- str_c("data/created/")
	filepath_export <-
		str_c(dir_export, filename_base, "_cleaned.csv")
	write.csv(d_4, filepath_export, row.names = FALSE)
}
