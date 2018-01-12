#'
#'
#'
#+ include = FALSE
rm(list = ls())
setwd("O:/_other/projects/nba/")

#'
#'
#'
# Packages. ----
library("dplyr")
library("stringr")
# library("readr")
# library("tidyr")
# library("printr")


#'
#'
#'
# Parameters. ----
export <- TRUE
filename_import_base <- "bigfour_public"
filename_import_suffix <- ""
filename_import_ext <- ".rda"
dir_import <- "data/downloaded/"
filepath_import <-
  str_c(dir_import,
        filename_import_base,
        filename_import_suffix,
        filename_import_ext)

filepath_db <- "data/db_nba.xlsm"
ws_tms <- "nba_tms"

if (export == TRUE) {
  filename_export_base <- "bigfour"
  filename_export_suffix <- "-cleaned"
  filename_export_ext <- ".csv"
  dir_export <- "data/"
  filepath_export <-
    str_c(
      dir_export,
      filename_export_base,
      filename_export_suffix,
      filename_export_ext
    )
}
#'
#'
#'
filepath_import
load(filepath_import)
names(bigfour_public)

readxl::excel_sheets(filepath_db)
tms <-
  filepath_db %>%
  readxl::read_excel(sheet = ws_tms) %>%
  select(tm, tm_name_full)
#'
#'
#'
bigfour_nba <-
  bigfour_public %>%
  filter(sport == "nba") %>%
  mutate(date = lubridate::ymd(format(gameDate, "%Y-%m%d"))) %>%
  mutate(season = if_else(lubridate::month(date) >= 10, lubridate::year(date), lubridate::year(date) - 1)) %>%
  select(date, season, everything()) %>%
  select(-gameDate, -sport) %>%
  rename(tm_away_name_full = visitor_team, tm_home_name_full = home_team, pts_away = visitor_score, pts_home = home_score) %>%
  arrange(date, season, tm_home_name_full, tm_away_name_full)
bigfour_nba

lines <-
  bigfour_nba %>%
  left_join(tms, by = c("tm_away_name_full" = "tm_name_full")) %>%
  rename(tm_away = tm) %>%
  left_join(tms, by = c("tm_home_name_full" = "tm_name_full")) %>%
  rename(tm_home = tm) %>%
  select(-ends_with("name_full")) %>%
  select(date, season, tm_home, tm_away, pts_home, pts_away, p_home)
lines

# Check that there are no missing team names.
lines %>% filter(is.na(tm_home))

# Check the most lopsided games in terms of lines.
lines %>%
  mutate(rank_fav = row_number(desc(p_home))) %>%
  arrange(desc(rank_fav)) %>%
  top_n(10, rank_fav)

lines %>%
  mutate(rank_fav = row_number(desc(p_home))) %>%
  arrange(desc(rank_fav)) %>%
  top_n(-10, rank_fav)

#'
#'
#'
if(export == TRUE) {
  readr::write_csv(lines, filepath_export)
}


