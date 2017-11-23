
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
  ceiling(2 * games_per_season * (yyyy_end - yyyy_start) / rows_per_page)
url_base <-
  "https://www.pro-football-reference.com/play-index/tgl_finder.cgi?request=1&match=game&year_min=2000&year_max=2016&game_type=R&game_num_min=0&game_num_max=99&week_num_min=0&week_num_max=99&temperature_gtlt=lt&c1stat=turnovers&c1comp=gt&c1val=0&c5val=1.0&order_by=game_date&order_by_asc=Y&offset="
export <- TRUE


# Main loop. ----
i <- 1
# Acually, needed one more iteration.
while(i <= num_pages) {
  # Debugging...
  # for(i in 1:2) {
  # i <- 1
  url <- str_c(url_base, (i - 1) * rows_per_page)

  # Don't necessarily need to download each html file with a new name.
  # Could use the same name.
  filename_html <- str_c("temp_", (i - 1) * rows_per_page)

  # Not sure working directory needs to be specified hear in order for download.file() to work.
  filepath_html <-
    str_c(getwd(), "/", dir_scrape, filename_html, ".html")
  # filepath_html <- str_c("~/data/nfl/downloaded/", filename_html, ".html")
  try(download.file(url, destfile = filepath_html))

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

  raw
  if (nrow(raw) == 0)
    break

  if(i == 1) header <- raw %>% slice(1) %>% as.matrix() %>% c()

  # Filter out extra rows that are embedded in the html tables.
  filtered <- raw %>% filter(Var.1 != "Rk")
  if(i == 1) {
  	all <- filtered
  } else {
  	all <- bind_rows(all, filtered)
  }
  i <- i + 1
}

tail(all)
# Finalize. ----
output <- all
head(output)
tail(output)

output <- output %>% filter(!is.na(Var.1))
names(output)
header
names(output) <- str_to_lower(header)
names(output) <-
  names(output) %>%
  str_replace_all("\\s", "_") %>%
	str_replace_all("/", "p") %>%
  str_replace_all("\\.|#", "")
names(output)

# Export. ----

# Save with a timestamp in order to prevent accidentally overwriting anything.
# Go back and rename this file manually without the timestamp when
# it is determined that it is good.

if (export == TRUE) {
	filename_export <- "pfref_game_results_tm_to"
	dir_export <- dir_scrape
	filepath_export <-
		str_c(
			dir_export,
			filename_export,
			"_scraped_",
			format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
			".csv"
		)
  write.csv(output, file = filepath_export, row.names = FALSE)
}
