

rm(list = ls())
setwd("O:/_other/projects/nba/")

# Packages. ----
library("dplyr")
library("stringr")
library("rvest")
library("lubridate")

# Parameters. ----
yyyy_start <- 2017
yyyy_end <- 2017
games_per_season <- 1230
rows_per_page <- 100
num_pages <-
  ceiling(2 * games_per_season * (yyyy_end - yyyy_start + 1) / rows_per_page)
url_base <-
  str_c(
    "https://www.basketball-reference.com/play-index/tgl_finder.cgi?request=1&match=game&lg_id=NBA&is_playoffs=N&team_seed_cmp=eq&opp_seed_cmp=eq&year_min=",
    yyyy_start,
    "&year_max=",
    yyyy_end,
    "&is_range=N&game_num_type=team&c1stat=fg&c1comp=gt&c2stat=diff_fg&c2comp=gt&c3stat=off_rtg&c3comp=gt&c4stat=diff_off_rtg&c4comp=gt&order_by=date_game&order_by_asc=Y&offset="
  )
export <- TRUE
dir_scrape <- "data/"

if (export == TRUE) {
  filename_export_base <- "game_results"
  filename_export_suffix <- "_scraped"
  filename_export_ext <- ".csv"
  dir_export <- dir_scrape
  filepath_export <-
    str_c(dir_export,
          filename_export_base,
          filename_export_suffix,
          filename_export_ext)
  filepath_export_ts <-
    str_c(
      dir_export,
      filename_export_base,
      filename_export_suffix,
      "_",
      format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
      filename_export_ext
    )
}

# Loop. ----

i <- 1
while (i <= num_pages) {
  url <- str_c(url_base, (i - 1) * rows_per_page)
  filename_html <- str_c("temp")
  filepath_html <- str_c(dir_scrape, filename_html, ".html")
  download.file(url, destfile = filepath_html)
  
  raw <-
    filepath_html %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table(header = TRUE) %>%
    as.data.frame() %>%
    tbl_df()
  raw
  
  if (nrow(raw) == 0)
    break
  
  # Column names are mistakenly captured in the first row.
  # Save these names to add on at the end.
  if (i == 1)
    header <- raw %>% slice(1) %>% as.matrix() %>% c()
  
  filtered <- raw %>% filter(Var.1 != "Rk")
  
  if (i == 1) {
    all <- filtered
  } else {
    all <- bind_rows(all, filtered)
  }
  
  i <- i + 1
}

output <- all

head(output)
tail(output)
header
output <- output %>% filter(!is.na(Var.1))
names(output)
names(output) <- str_to_lower(header)
names(output) <-
  names(output) %>%
  str_replace_all("\\s", "_") %>%
  str_replace_all("\\.|#", "") %>%
  str_replace_all("/", "p") %>%
  str_replace_all("\\%", "pct")
names(output)


if (export == TRUE) {
  write_csv(output, filepath_export)
  write_csv(output, filepath_export_ts)
}
