

rm(list = ls())
setwd("O:/_other/projects/nfl/")

# Packages. ----
library("dplyr")
library("stringr")
library("rvest")
library("lubridate")

# Parameters. ----
# yyyy_start <- 1978
yyyy_start <- 2000
yyyy_end <- 2016
games_per_season <- 256 + 11
rows_per_page <- 100
num_pages <-
  ceiling(games_per_season * (yyyy_end - yyyy_start + 1) / rows_per_page)
url_base <-
  str_c(
    "https://www.pro-football-reference.com/play-index/tgl_finder.cgi?request=1&match=game&year_min=",
    yyyy_start,
    "&year_max=",
    yyyy_end,
    "&game_type=E&game_num_min=0&game_num_max=99&week_num_min=0&week_num_max=99&temperature_gtlt=lt&c1stat=vegas_line&c1comp=gt&c5val=1.0&order_by=game_date&order_by_asc=Y&offset="
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
      format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
      filename_export_ext
    )
}


# Preallocate memory for data to be scraped..
# The number of columns is known through prior debugging.
m <- matrix(NA, nrow = num_pages * rows_per_page, ncol = 17)

# Main for loop. ----

for (i in 1:num_pages) {
  # Debugging...
  # for(i in 1:2) {
  # i <- 1
  url <- str_c(url_base, (i - 1) * rows_per_page)
  filename_html <- str_c("temp")
  filepath_html <- str_c(dir_scrape, filename_html, ".html")
  download.file(url, destfile = filepath_html)
  
  # Convert to data.frame first on purpose (even though converstion to a tibble
  # could be done directly. This avoid a "Error: Column 1 must be named" error.
  # The column names get converted to Var.1, Var.2, etc.
  raw <-
    filepath_html %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table(header = TRUE) %>%
    as.data.frame() %>%
    tbl_df()
  
  if (nrow(raw) == 0)
    break
  
  # Column names are mistakenly captured in the first row.
  # Save these names to add on at the end.
  if (i == 1)
    header <- raw %>% slice(1) %>% as.matrix() %>% c()
  
  # Filter out extra rows that are embedded in the html tables.
  filtered <- raw %>% filter(Var.1 != "Rk")
  
  
  if (i == 1) {
    all <- filtered
  } else {
    all <- bind_rows(all, filtered)
  }
  
  i <- i + 1
}

# Finalize. ----

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

# Save with a backup with a timestamp in order to prevent accidentally overwriting anything.

if (export == TRUE) {
  write_csv(output, filepath_export)
  write_csv(output, filepath_export_ts)
}
