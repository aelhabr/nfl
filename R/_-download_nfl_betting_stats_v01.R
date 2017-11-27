

#'
#'
#'
#+ include = FALSE
rm(list = ls())

#'
#'
#'
# Packages. ----
library("dplyr")
library("stringr")
library("readxl")
# library("lubridate")

#'
#'
#'
# Functions. ----
source("R/functions_predictiontracker.R")

#'
#'
#'
# Parameters. ----
dir_import <- "O:/_other/projects/nfl/"
filename_import <- "data/db_nfl.xlsm"
filepath_import <-
  str_c(dir_import, filename_import)

ws_tms <- "nfl_tms"
timestamp_runtime <- Sys.time()
timestamp_runtime_char <-
  format(timestamp_runtime, "%Y-%m-%d_%H-%M-%S")
timestamp_runtime_ymd <-
  as.Date(format(timestamp_runtime, "%Y-%m-%d"))
#'
#'
#'
tms <-
  filepath_import %>%
  read_excel(ws_tms) %>%
  select(tm, tm_name_predictiontracker)

#'
#'
#'

lines_raw <-
  get_lines_predictiontracker(
    suffix_download_backup = str_c("_", timestamp_runtime_char),
    keep_download = FALSE
  )

lines_joined <-
  lines_raw %>%
  # select(home, road, line, lineopen) %>%
  inner_join(tms, by = c("home" = "tm_name_predictiontracker")) %>%
  rename(tm_home = tm) %>%
  inner_join(tms, by = c("road" = "tm_name_predictiontracker")) %>%
  rename(tm_away = tm) %>%
  rename(line_open = lineopen, line_current = line) %>%
  select(-home, -road) %>%
  mutate(timestamp_download = timestamp_runtime,
         timestamp_download_ymd = timestamp_runtime_ymd)
lines_joined

lines_truncated <-
  lines_joined %>%
  select(
    tm_home,
    tm_away,
    line_open,
    line_current,
    timestamp_download_ymd,
    timestamp_download
  )
lines_truncated

lines_truncated %>%
  save_as_xlsx(
    timestamp = timestamp_runtime_ymd,
    colname_timestamp = "timestamp_download_ymd",
    ws_name = "lines",
    filename_save = "betting_metrics",
    dir_save = "data/",
    overwrite_data = FALSE
  )

#'
#'
#'

totals_raw <-
  get_totals_predictiontracker(
    suffix_download_backup = str_c("_", timestamp_runtime_char),
    keep_download = FALSE
  )

totals_joined <-
  totals_raw %>%
  # select(home, visitor, total_open, total_current) %>%
  inner_join(tms, by = c("home" = "tm_name_predictiontracker")) %>%
  rename(tm_home = tm) %>%
  inner_join(tms, by = c("visitor" = "tm_name_predictiontracker")) %>%
  rename(tm_away = tm) %>%
  select(-home, -visitor) %>%
  mutate(timestamp_runtime = timestamp_runtime,
         timestamp_runtime_ymd = timestamp_runtime_char)
totals_joined

# totals_truncated <-
#   totals_joined %>%
#   select(
#     tm_home,
#     tm_away,
#     total_open,
#     total_current,
#     timestamp_runtime_ymd,
#     timestamp_runtime
#   )
# totals_truncated

totals_joined %>%
  save_as_xlsx(
    timestamp = timestamp_runtime_ymd,
    colname_timestamp = "timestamp_runtime_ymd",
    ws_name = "totals",
    filename_save = "betting_metrics",
    dir_save = "data/",
    overwrite_data = FALSE
  )
